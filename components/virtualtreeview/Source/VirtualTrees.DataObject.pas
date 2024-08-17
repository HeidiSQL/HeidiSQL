unit VirtualTrees.DataObject;

interface

uses
  WinApi.ActiveX,
  WinApi.Windows,
  Vcl.Controls,
  VirtualTrees.Types;

type
  IDataObject = WinApi.ActiveX.IDataObject;

  // IDataObject.SetData support
  TInternalStgMedium = packed record
    Format : TClipFormat;
    Medium : TStgMedium;
  end;

  TInternalStgMediumArray = array of TInternalStgMedium;

  // This data object is used in two different places. One is for clipboard operations and the other while dragging.
  TVTDataObject = class(TInterfacedObject, IDataObject)
  private
    FOwner                  : TCustomControl;          // The tree which provides clipboard or drag data.
    FForClipboard           : Boolean;                 // Determines which data to render with GetData.
    FFormatEtcArray         : TFormatEtcArray;
    FInternalStgMediumArray : TInternalStgMediumArray; // The available formats in the DataObject
    FAdviseHolder           : IDataAdviseHolder;       // Reference to an OLE supplied implementation for advising.
  protected
    function CanonicalIUnknown(const TestUnknown : IUnknown) : IUnknown;
    function EqualFormatEtc(FormatEtc1, FormatEtc2 : TFormatEtc) : Boolean;
    function FindFormatEtc(TestFormatEtc : TFormatEtc; const FormatEtcArray : TFormatEtcArray) : Integer;
    function FindInternalStgMedium(Format : TClipFormat) : PStgMedium;
    function HGlobalClone(HGlobal : THandle) : THandle;
    function RenderInternalOLEData(const FormatEtcIn : TFormatEtc; var Medium : TStgMedium; var OLEResult : HResult) : Boolean;
    function StgMediumIncRef(const InStgMedium : TStgMedium; var OutStgMedium : TStgMedium; CopyInMedium : Boolean; const DataObject : IDataObject) : HResult;

    property ForClipboard : Boolean read FForClipboard;
    property FormatEtcArray : TFormatEtcArray read FFormatEtcArray write FFormatEtcArray;
    property InternalStgMediumArray : TInternalStgMediumArray read FInternalStgMediumArray write FInternalStgMediumArray;
    property Owner : TCustomControl read FOwner;
  public
    constructor Create(AOwner : TCustomControl; ForClipboard : Boolean); virtual;
    destructor Destroy; override;

    function DAdvise(const FormatEtc : TFormatEtc; advf : Integer; const advSink : IAdviseSink; out dwConnection : Integer) : HResult; virtual; stdcall;
    function DUnadvise(dwConnection : Integer) : HResult; virtual; stdcall;
    function EnumDAdvise(out enumAdvise : IEnumStatData) : HResult; virtual; stdcall;
    function EnumFormatEtc(Direction : Integer; out EnumFormatEtc : IEnumFormatEtc) : HResult; virtual; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc : TFormatEtc; out FormatEtcOut : TFormatEtc) : HResult; virtual; stdcall;
    function GetData(const FormatEtcIn : TFormatEtc; out Medium : TStgMedium) : HResult; virtual; stdcall;
    function GetDataHere(const FormatEtc : TFormatEtc; out Medium : TStgMedium) : HResult; virtual; stdcall;
    function QueryGetData(const FormatEtc : TFormatEtc) : HResult; virtual; stdcall;
    function SetData(const FormatEtc : TFormatEtc; var Medium : TStgMedium; DoRelease : BOOL) : HResult; virtual; stdcall;
  end;

implementation

uses
  VirtualTrees.ClipBoard,
  VirtualTrees.DragnDrop,
  VirtualTrees.BaseTree;

type
  TVTCracker = class(TBaseVirtualTree);

  //----------------- TVTDataObject --------------------------------------------------------------------------------------

constructor TVTDataObject.Create(AOwner : TCustomControl; ForClipboard : Boolean);
begin
  inherited Create;

  FOwner := AOwner;
  FForClipboard := ForClipboard;
  TVTCracker(FOwner).GetNativeClipboardFormats(FFormatEtcArray);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTDataObject.Destroy;
var
  I         : Integer;
  StgMedium : PStgMedium;
begin
  // Cancel a pending clipboard operation if this data object was created for the clipboard and
  // is freed because something else is placed there.
  if FForClipboard and not (tsClipboardFlushing in TBaseVirtualTree(FOwner).TreeStates) then
    TBaseVirtualTree(FOwner).CancelCutOrCopy;

  // Release any internal clipboard formats
  for I := 0 to High(FormatEtcArray) do
  begin
    StgMedium := FindInternalStgMedium(FormatEtcArray[I].cfFormat);
    if Assigned(StgMedium) then
      ReleaseStgMedium(StgMedium^);
  end;

  FormatEtcArray := nil;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.CanonicalIUnknown(const TestUnknown : IUnknown) : IUnknown;
// Uses COM object identity: An explicit call to the IUnknown::QueryInterface method, requesting the IUnknown
// interface, will always return the same pointer.
begin
  if Assigned(TestUnknown) then
  begin
    if TestUnknown.QueryInterface(IUnknown, Result) = 0 then
      Result._Release // Don't actually need it just need the pointer value
    else
      Result := TestUnknown;
  end
  else
    Result := TestUnknown;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EqualFormatEtc(FormatEtc1, FormatEtc2 : TFormatEtc) : Boolean;
begin
  Result := (FormatEtc1.cfFormat = FormatEtc2.cfFormat) and (FormatEtc1.ptd = FormatEtc2.ptd) and (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and
    (FormatEtc1.lindex = FormatEtc2.lindex) and (FormatEtc1.tymed and FormatEtc2.tymed <> 0);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.FindFormatEtc(TestFormatEtc : TFormatEtc; const FormatEtcArray : TFormatEtcArray) : Integer;
var
  I : Integer;
begin
  Result := - 1;
  for I := 0 to High(FormatEtcArray) do
  begin
    if EqualFormatEtc(TestFormatEtc, FormatEtcArray[I]) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.FindInternalStgMedium(Format : TClipFormat) : PStgMedium;
var
  I : Integer;
begin
  Result := nil;
  for I := 0 to High(InternalStgMediumArray) do
  begin
    if Format = InternalStgMediumArray[I].Format then
    begin
      Result := @InternalStgMediumArray[I].Medium;
      Break;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.HGlobalClone(HGlobal : THandle) : THandle;
// Returns a global memory block that is a copy of the passed memory block.
var
  Size          : Cardinal;
  Data, NewData : PByte;
begin
  Size := GlobalSize(HGlobal);
  Result := GlobalAlloc(GPTR, Size);
  Data := GlobalLock(HGlobal);
  try
    NewData := GlobalLock(Result);
    try
      Move(Data^, NewData^, Size);
    finally
      GlobalUnLock(Result);
    end;
  finally
    GlobalUnLock(HGlobal);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.RenderInternalOLEData(const FormatEtcIn : TFormatEtc; var Medium : TStgMedium; var OLEResult : HResult) : Boolean;
// Tries to render one of the formats which have been stored via the SetData method.
// Since this data is already there it is just copied or its reference count is increased (depending on storage medium).
var
  InternalMedium : PStgMedium;
begin
  Result := True;
  InternalMedium := FindInternalStgMedium(FormatEtcIn.cfFormat);
  if Assigned(InternalMedium) then
    OLEResult := StgMediumIncRef(InternalMedium^, Medium, False, Self as IDataObject)
  else
    Result := False;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.StgMediumIncRef(const InStgMedium : TStgMedium; var OutStgMedium : TStgMedium; CopyInMedium : Boolean; const DataObject : IDataObject) : HResult;
// InStgMedium is the data that is requested, OutStgMedium is the data that we are to return either a copy of or
// increase the IDataObject's reference and send ourselves back as the data (unkForRelease). The InStgMedium is usually
// the result of a call to find a particular FormatEtc that has been stored locally through a call to SetData.
// If CopyInMedium is not true we already have a local copy of the data when the SetData function was called (during
// that call the CopyInMedium must be true). Then as the caller asks for the data through GetData we do not have to make
// copy of the data for the caller only to have them destroy it then need us to copy it again if necessary.
// This way we increase the reference count to ourselves and pass the STGMEDIUM structure initially stored in SetData.
// This way when the caller frees the structure it sees the unkForRelease is not nil and calls Release on the object
// instead of destroying the actual data.
var
  Len : Integer;
begin
  Result := S_OK;

  // Simply copy all fields to start with.
  OutStgMedium := InStgMedium;
  // The data handled here always results from a call of SetData we got. This ensures only one storage format
  // is indicated and hence the case statement below is safe (IDataObject.GetData can optionally use several
  // storage formats).
  case InStgMedium.tymed of
    TYMED_HGLOBAL :
      begin
        if CopyInMedium then
        begin
          // Generate a unique copy of the data passed
          OutStgMedium.HGlobal := HGlobalClone(InStgMedium.HGlobal);
          if OutStgMedium.HGlobal = 0 then
            Result := E_OUTOFMEMORY;
        end
        else
          // Don't generate a copy just use ourselves and the copy previously saved.
          OutStgMedium.unkForRelease := Pointer(DataObject); // Does not increase RefCount.
      end;
    TYMED_FILE :
      begin
        Len := lstrLenW(InStgMedium.lpszFileName) + 1; // Don't forget the terminating null character.
        OutStgMedium.lpszFileName := CoTaskMemAlloc(2 * Len);
        Move(InStgMedium.lpszFileName^, OutStgMedium.lpszFileName^, 2 * Len);
      end;
    TYMED_ISTREAM :
      IUnknown(OutStgMedium.stm)._AddRef;
    TYMED_ISTORAGE :
      IUnknown(OutStgMedium.stg)._AddRef;
    TYMED_GDI :
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.unkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED;                             // Don't know how to copy GDI objects right now.
    TYMED_MFPICT :
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.unkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED;                             // Don't know how to copy MetaFile objects right now.
    TYMED_ENHMF :
      if not CopyInMedium then
        // Don't generate a copy just use ourselves and the previously saved data.
        OutStgMedium.unkForRelease := Pointer(DataObject) // Does not increase RefCount.
      else
        Result := DV_E_TYMED;                             // Don't know how to copy enhanced metafiles objects right now.
  else
    Result := DV_E_TYMED;
  end;

  if (Result = S_OK) and Assigned(OutStgMedium.unkForRelease) then
    IUnknown(OutStgMedium.unkForRelease)._AddRef;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.DAdvise(const FormatEtc : TFormatEtc; advf : Integer; const advSink : IAdviseSink; out dwConnection : Integer) : HResult;
// Advise sink management is greatly simplified by the IDataAdviseHolder interface.
// We use this interface and forward all concerning calls to it.
begin
  Result := S_OK;
  if FAdviseHolder = nil then
    Result := CreateDataAdviseHolder(FAdviseHolder);
  if Result = S_OK then
    Result := FAdviseHolder.Advise(Self as IDataObject, FormatEtc, advf, advSink, dwConnection);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.DUnadvise(dwConnection : Integer) : HResult;
begin
  if FAdviseHolder = nil then
    Result := E_NOTIMPL
  else
    Result := FAdviseHolder.Unadvise(dwConnection);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EnumDAdvise(out enumAdvise : IEnumStatData) : HResult;
begin
  if FAdviseHolder = nil then
    Result := OLE_E_ADVISENOTSUPPORTED
  else
    Result := FAdviseHolder.enumAdvise(enumAdvise);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.EnumFormatEtc(Direction : Integer; out EnumFormatEtc : IEnumFormatEtc) : HResult;
var
  NewList : TEnumFormatEtc;
begin
  Result := E_FAIL;
  if Direction = DATADIR_GET then
  begin
    NewList := TEnumFormatEtc.Create(FormatEtcArray);
    EnumFormatEtc := NewList as IEnumFormatEtc;
    Result := S_OK;
  end
  else
    EnumFormatEtc := nil;
  if EnumFormatEtc = nil then
    Result := OLE_S_USEREG;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.GetCanonicalFormatEtc(const FormatEtc : TFormatEtc; out FormatEtcOut : TFormatEtc) : HResult;
begin
  Result := DATA_S_SAMEFORMATETC;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.GetData(const FormatEtcIn : TFormatEtc; out Medium : TStgMedium) : HResult;
// Data is requested by clipboard or drop target. This method dispatchs the call
// depending on the data being requested.
var
  I    : Integer;
  Data : PVTReference;
begin
  // The tree reference format is always supported and returned from here.
  if FormatEtcIn.cfFormat = CF_VTREFERENCE then
  begin
    // Note: this format is not used while flushing the clipboard to avoid a dangling reference
    //       when the owner tree is destroyed before the clipboard data is replaced with something else.
    if tsClipboardFlushing in TBaseVirtualTree(FOwner).TreeStates then
      Result := E_FAIL
    else
    begin
      Medium.HGlobal := GlobalAlloc(GHND or GMEM_SHARE, SizeOf(TVTReference));
      Data := GlobalLock(Medium.HGlobal);
      Data.Process := GetCurrentProcessID;
      Data.Tree := TBaseVirtualTree(FOwner);
      GlobalUnLock(Medium.HGlobal);
      Medium.tymed := TYMED_HGLOBAL;
      Medium.unkForRelease := nil;
      Result := S_OK;
    end;
  end
  else
  begin
    try
      // See if we accept this type and if not get the correct return value.
      Result := QueryGetData(FormatEtcIn);
      if Result = S_OK then
      begin
        for I := 0 to High(FormatEtcArray) do
        begin
          if EqualFormatEtc(FormatEtcIn, FormatEtcArray[I]) then
          begin
            if not RenderInternalOLEData(FormatEtcIn, Medium, Result) then
              Result := TVTCracker(FOwner).RenderOLEData(FormatEtcIn, Medium, FForClipboard);
            Break;
          end;
        end;
      end;
    except
      ZeroMemory(@Medium, SizeOf(Medium));
      Result := E_FAIL;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.GetDataHere(const FormatEtc : TFormatEtc; out Medium : TStgMedium) : HResult;
begin
  Result := E_NOTIMPL;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.QueryGetData(const FormatEtc : TFormatEtc) : HResult;
var
  I : Integer;
begin
  Result := DV_E_CLIPFORMAT;
  for I := 0 to High(FFormatEtcArray) do
  begin
    if FormatEtc.cfFormat = FFormatEtcArray[I].cfFormat then
    begin
      if (FormatEtc.tymed and FFormatEtcArray[I].tymed) <> 0 then
      begin
        if FormatEtc.dwAspect = FFormatEtcArray[I].dwAspect then
        begin
          if FormatEtc.lindex = FFormatEtcArray[I].lindex then
          begin
            Result := S_OK;
            Break;
          end
          else
            Result := DV_E_LINDEX;
        end
        else
          Result := DV_E_DVASPECT;
      end
      else
        Result := DV_E_TYMED;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDataObject.SetData(const FormatEtc : TFormatEtc; var Medium : TStgMedium; DoRelease : BOOL) : HResult;
// Allows dynamic adding to the IDataObject during its existance. Most noteably it is used to implement
// IDropSourceHelper and allows to set a special format for optimized moves during a shell transfer.
var
  Index          : Integer;
  LocalStgMedium : PStgMedium;
begin
  // See if we already have a format of that type available.
  Index := FindFormatEtc(FormatEtc, FormatEtcArray);
  if Index > - 1 then
  begin
    // Just use the TFormatEct in the array after releasing the data.
    LocalStgMedium := FindInternalStgMedium(FormatEtcArray[Index].cfFormat);
    if Assigned(LocalStgMedium) then
    begin
      ReleaseStgMedium(LocalStgMedium^);
      ZeroMemory(LocalStgMedium, SizeOf(LocalStgMedium^));
    end;
  end
  else
  begin
    // It is a new format so create a new TFormatCollectionItem, copy the
    // FormatEtc parameter into the new object and and put it in the list.
    SetLength(FFormatEtcArray, Length(FormatEtcArray) + 1);
    FormatEtcArray[High(FormatEtcArray)] := FormatEtc;

    // Create a new InternalStgMedium and initialize it and associate it with the format.
    SetLength(FInternalStgMediumArray, Length(InternalStgMediumArray) + 1);
    InternalStgMediumArray[High(InternalStgMediumArray)].Format := FormatEtc.cfFormat;
    LocalStgMedium := @InternalStgMediumArray[High(InternalStgMediumArray)].Medium;
    ZeroMemory(LocalStgMedium, SizeOf(LocalStgMedium^));
  end;

  if DoRelease then
  begin
    // We are simply being given the data and we take control of it.
    LocalStgMedium^ := Medium;
    Result := S_OK;
  end
  else
  begin
    // We need to reference count or copy the data and keep our own references to it.
    Result := StgMediumIncRef(Medium, LocalStgMedium^, True, Self as IDataObject);

    // Can get a circular reference if the client calls GetData then calls SetData with the same StgMedium.
    // Because the unkForRelease for the IDataObject can be marshalled it is necessary to get pointers that
    // can be correctly compared. See the IDragSourceHelper article by Raymond Chen at MSDN.
    if Assigned(LocalStgMedium.unkForRelease) then
    begin
      if CanonicalIUnknown(Self) = CanonicalIUnknown(IUnknown(LocalStgMedium.unkForRelease)) then
        IUnknown(LocalStgMedium.unkForRelease) := nil; // release the interface
    end;
  end;

  // Tell all registered advice sinks about the data change.
  if Assigned(FAdviseHolder) then
    FAdviseHolder.SendOnDataChange(Self as IDataObject, 0, 0);
end;

end.
