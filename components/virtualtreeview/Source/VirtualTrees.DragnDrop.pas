unit VirtualTrees.DragnDrop;

interface

uses
  WinApi.Windows,
  WinApi.ActiveX,
  WinApi.ShlObj,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    FFormatEtcArray : TFormatEtcArray;
    FCurrentIndex   : Integer;
  public
    constructor Create(const AFormatEtcArray : TFormatEtcArray);

    function Clone(out Enum : IEnumFormatEtc) : HResult; stdcall;
    function Next(celt : Integer; out elt; pceltFetched : PLongint) : HResult; stdcall;
    function Reset : HResult; stdcall;
    function Skip(celt : Integer) : HResult; stdcall;
  end;

  // TVTDragManager is a class to manage drag and drop in a Virtual Treeview.
  TVTDragManager = class(TInterfacedObject, IVTDragManager, IDropSource, IDropTarget)
  private
    FOwner,                                // The tree which is responsible for drag management.
    FDragSource       : TBaseVirtualTree;  // Reference to the source tree if the source was a VT, might be different than the owner tree.
    FIsDropTarget     : Boolean;           // True if the owner is currently the drop target.
    FDataObject       : IDataObject;       // A reference to the data object passed in by DragEnter (only used when the owner tree is the current drop target).
    FDropTargetHelper : IDropTargetHelper; // Win2k > Drag image support
    FFullDragging     : BOOL;              // True, if full dragging is currently enabled in the system.

    function GetDataObject : IDataObject; stdcall;
    function GetDragSource : TBaseVirtualTree; stdcall;
    function GetIsDropTarget : Boolean; stdcall;
  public
    constructor Create(AOwner : TBaseVirtualTree); virtual;
    destructor Destroy; override;

    function DragEnter(const DataObject : IDataObject; KeyState : Integer; Pt : TPoint; var Effect : Longint) : HResult; stdcall;
    function DragLeave : HResult; stdcall;
    function DragOver(KeyState : Integer; Pt : TPoint; var Effect : Longint) : HResult; stdcall;
    function Drop(const DataObject : IDataObject; KeyState : Integer; Pt : TPoint; var Effect : Integer) : HResult; stdcall;
    procedure ForceDragLeave; stdcall;
    function GiveFeedback(Effect : Integer) : HResult; stdcall;
    function QueryContinueDrag(EscapePressed : BOOL; KeyState : Integer) : HResult; stdcall;
  end;

var
  StandardOLEFormat : TFormatEtc = (
    // Format must later be set.
    cfFormat : 0;
    // No specific target device to render on.
    ptd : nil;
    // Normal content to render.
    dwAspect : DVASPECT_CONTENT;
    // No specific page of multipage data (we don't use multipage data by default).
    lindex : - 1;
    // Acceptable storage formats are IStream and global memory. The first is preferred.
    tymed : TYMED_ISTREAM or TYMED_HGLOBAL;
  );

implementation

uses
  VirtualTrees.DataObject;

type
  TBaseVirtualTreeCracker = class(TBaseVirtualTree);

  TVTDragManagerHelper = class helper for TVTDragManager
    function TreeView : TBaseVirtualTreeCracker;
  end;


  //----------------- TEnumFormatEtc -------------------------------------------------------------------------------------

constructor TEnumFormatEtc.Create(const AFormatEtcArray : TFormatEtcArray);
var
  I : Integer;
begin
  inherited Create;
  // Make a local copy of the format data.
  SetLength(FFormatEtcArray, Length(AFormatEtcArray));
  for I := 0 to High(AFormatEtcArray) do
    FFormatEtcArray[I] := AFormatEtcArray[I];
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Clone(out Enum : IEnumFormatEtc) : HResult;
var
  AClone : TEnumFormatEtc;
begin
  Result := S_OK;
  try
    AClone := TEnumFormatEtc.Create(FFormatEtcArray);
    AClone.FCurrentIndex := FCurrentIndex;
    Enum := AClone as IEnumFormatEtc;
  except
    Result := E_FAIL;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Next(celt : Integer; out elt; pceltFetched : PLongint) : HResult;
var
  CopyCount : Integer;
begin
  Result := S_FALSE;
  CopyCount := Length(FFormatEtcArray) - FCurrentIndex;
  if celt < CopyCount then
    CopyCount := celt;
  if CopyCount > 0 then
  begin
    Move(FFormatEtcArray[FCurrentIndex], elt, CopyCount * SizeOf(TFormatEtc));
    Inc(FCurrentIndex, CopyCount);
    Result := S_OK;
  end;
  if Assigned(pceltFetched) then
    pceltFetched^ := CopyCount;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Reset : HResult;
begin
  FCurrentIndex := 0;
  Result := S_OK;
end;

//----------------------------------------------------------------------------------------------------------------------

function TEnumFormatEtc.Skip(celt : Integer) : HResult;
begin
  if FCurrentIndex + celt < High(FFormatEtcArray) then
  begin
    Inc(FCurrentIndex, celt);
    Result := S_OK;
  end
  else
    Result := S_FALSE;
end;


//----------------------------------------------------------------------------------------------------------------------

// OLE drag and drop support classes
// This is quite heavy stuff (compared with the VCL implementation) but is much better suited to fit the needs
// of DD'ing various kinds of virtual data and works also between applications.


//----------------- TVTDragManager -------------------------------------------------------------------------------------

constructor TVTDragManager.Create(AOwner : TBaseVirtualTree);
begin
  inherited Create;
  FOwner := AOwner;

  // Create an instance  of the drop target helper interface. This will fail but not harm on systems which do
  // not support this interface (everything below Windows 2000);
  CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IID_IDropTargetHelper, FDropTargetHelper);
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTDragManager.Destroy;
begin
  // Set the owner's reference to us to nil otherwise it will access an invalid pointer
  // after our desctruction is complete.
  TreeView.ClearDragManager;
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDataObject : IDataObject;
begin
  // When the owner tree starts a drag operation then it gets a data object here to pass it to the OLE subsystem.
  // In this case there is no local reference to a data object and one is created (but not stored).
  // If there is a local reference then the owner tree is currently the drop target and the stored interface is
  // that of the drag initiator.
  if Assigned(FDataObject) then
    Result := FDataObject
  else
  begin
    Result := TreeView.DoCreateDataObject;
    if (Result = nil) and not Assigned(TreeView.OnCreateDataObject) then
      // Do not create a TVTDataObject if the event handler explicitely decided not to supply one, issue #736.
      Result := TVTDataObject.Create(FOwner, False) as IDataObject;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetDragSource : TBaseVirtualTree;
begin
  Result := FDragSource;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GetIsDropTarget : Boolean;
begin
  Result := FIsDropTarget;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragEnter(const DataObject : IDataObject; KeyState : Integer; Pt : TPoint; var Effect : Integer) : HResult;
begin
  FDataObject := DataObject;
  FIsDropTarget := True;

  SystemParametersInfo(SPI_GETDRAGFULLWINDOWS, 0, @FFullDragging, 0);
  // If full dragging of window contents is disabled in the system then our tree windows will be locked
  // and cannot be updated during a drag operation. With the following call painting is again enabled.
  if not FFullDragging then
    LockWindowUpdate(0);
  if Assigned(FDropTargetHelper) and FFullDragging then
  begin
    if toAutoScroll in TreeView.TreeOptions.AutoOptions then
      FDropTargetHelper.DragEnter(FOwner.Handle, DataObject, Pt, Effect)
    else
      FDropTargetHelper.DragEnter(0, DataObject, Pt, Effect); // Do not pass handle, otherwise the IDropTargetHelper will perform autoscroll. Issue #486
  end;
  FDragSource := TreeView.GetTreeFromDataObject(DataObject);
  Result := TreeView.DragEnter(KeyState, Pt, Effect);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragLeave : HResult;
begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragLeave;

  TreeView.DragLeave;
  FIsDropTarget := False;
  FDragSource := nil;
  FDataObject := nil;
  Result := NOERROR;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.DragOver(KeyState : Integer; Pt : TPoint; var Effect : Integer) : HResult;
begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragOver(Pt, Effect);

  Result := TreeView.DragOver(FDragSource, KeyState, dsDragMove, Pt, Effect);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.Drop(const DataObject : IDataObject; KeyState : Integer; Pt : TPoint; var Effect : Integer) : HResult;
begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.Drop(DataObject, Pt, Effect);

  Result := TreeView.DragDrop(DataObject, KeyState, Pt, Effect);
  FIsDropTarget := False;
  FDataObject := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTDragManager.ForceDragLeave;
// Some drop targets, e.g. Internet Explorer leave a drag image on screen instead removing it when they receive
// a drop action. This method calls the drop target helper's DragLeave method to ensure it removes the drag image from
// screen. Unfortunately, sometimes not even this does help (e.g. when dragging text from VT to a text field in IE).
begin
  if Assigned(FDropTargetHelper) and FFullDragging then
    FDropTargetHelper.DragLeave;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.GiveFeedback(Effect : Integer) : HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTDragManager.QueryContinueDrag(EscapePressed : BOOL; KeyState : Integer) : HResult;
var
  RButton, LButton : Boolean;
begin
  LButton := (KeyState and MK_LBUTTON) <> 0;
  RButton := (KeyState and MK_RBUTTON) <> 0;

  // Drag'n drop canceled by pressing both mouse buttons or Esc?
  if (LButton and RButton) or EscapePressed then
    Result := DRAGDROP_S_CANCEL
  else
    // Drag'n drop finished?
    if not (LButton or RButton) then
      Result := DRAGDROP_S_DROP
    else
      Result := S_OK;
end;

{ TVTDragManagerHelper }

function TVTDragManagerHelper.TreeView : TBaseVirtualTreeCracker;
begin
  Result := TBaseVirtualTreeCracker(FOwner);
end;

end.
