// ----------------------------------------------------------------------------------
// Windows 7 Delphi interfaces
//
// Serhiy Perevoznyk
//
// THIS CODE AND INFORMATION ARE PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
// EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
// ----------------------------------------------------------------------------------

{$WARN SYMBOL_PLATFORM OFF}

unit JumpList;

interface

uses
  Windows, SysUtils, Classes, ShellApi, ShlObj, ActiveX,
  Generics.Collections, Generics.Defaults,
  IOUtils, PropSys, PropKey, SHLwAPI, ObjectArray;

type

  ICustomDestinationList = interface(IUnknown)
    [SID_ICustomDestinationList]
    function SetAppID(pszAppID: LPCWSTR): HRESULT; stdcall;
    function BeginList(var pcMaxSlots: UINT; const riid: TIID;
      out ppv): HRESULT; stdcall;
    function AppendCategory(pszCategory: LPCWSTR;
      const poa: IObjectArray): HRESULT; stdcall;
    function AppendKnownCategory(category: Integer): HRESULT; stdcall;
    function AddUserTasks(const poa: IObjectArray): HRESULT; stdcall;
    function CommitList: HRESULT; stdcall;
    function GetRemovedDestinations(const riid: TIID;
      out ppv): HRESULT; stdcall;
    function DeleteList(pszAppID: LPCWSTR): HRESULT; stdcall;
    function AbortList: HRESULT; stdcall;
  end;

  TJumpItem = class abstract
  private
    FCustomCategory: string;
  public
    property CustomCategory: string read FCustomCategory write FCustomCategory;
  end;

  TJumpSeparator = class (TJumpItem)
  end;

  TJumpPath = class(TJumpItem)
  private
    FPath: string;
  public
    property Path: string read FPath write FPath;
  end;

  TJumpTask = class(TJumpItem)
  private
    FArguments: string;
    FApplicationPath: string;
    FDescription: string;
    FIconResourceIndex: integer;
    FIconResourcePath: string;
    FTitle: string;
    FWorkingDirectory: string;
  public
    property ApplicationPath : string read FApplicationPath write FApplicationPath;
    property Arguments: string read FArguments write FArguments;
    property Description: string read FDescription write FDescription;
    property IconResourceIndex : integer read FIconResourceIndex write FIconResourceIndex;
    property IconResourcePath : string read FIconResourcePath write FIconResourcePath;
    property Title: string read FTitle write FTitle;
    property WorkingDirectory : string read FWorkingDirectory write FWorkingDirectory;
  end;

  TJumpList = class sealed
  private
    class var FFullName: string;
  private
    FShowFrequentCategory: boolean;
    FShowRecentCategory: boolean;
    FJumpItems: TList<TJumpItem>;
    FApplicationId: string;
  private
    class function CreateItemFromJumpPath(JumpPath: TJumpPath): IShellItem;
    class function GetShellItemForPath(Path: string): IShellItem;
    class function CreateLinkFromJumpTask(JumpTask: TJumpTask; AllowSeparators: boolean): IShellLink;
    class function CreateSeparator(JumpSeparator : TJumpSeparator) : IShellLink;
    class function InitPropVariantFromString(const Value: string): TPropVariant;
    class function InitPropVariantFromBoolean(const Value: boolean) : TPropVariant;
    class function AddCategory(Items : TList<TJumpItem>) : IObjectArray;
    procedure ApplyList;
    procedure AddToRecentCategoryXP(ItemPath: string);
    procedure SetApplicationId(const Value: string);
    class procedure CheckResult(ACode : HRESULT);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddToRecentCategory(ItemPath: string); overload;
    procedure AddToRecentCategory(JumpPath: TJumpPath); overload;
    procedure AddToRecentCategory(JumpTask: TJumpTask); overload;
    function AddJumpPath : TJumpPath;
    function AddJumpTask : TJumpTask;
    function AddJumpSeparator : TJumpSeparator;
    function AddJumpItem<T : TJumpItem, constructor> : T;
    procedure Apply;
    procedure DeleteList;
    property ShowFrequentCategory : boolean read FShowFrequentCategory write FShowFrequentCategory;
    property ShowRecentCategory : boolean read FShowRecentCategory write FShowRecentCategory;
    property JumpItems : TList<TJumpItem> read FJumpItems;
    property ApplicationId : string read FApplicationId write SetApplicationId;
  end;

implementation

{ TJumpList }

function PropVariantClear(ppval : PPropVariant) : HRESULT; stdcall; external 'ole32.dll';

procedure TJumpList.AddToRecentCategory(ItemPath: string);
var
  ShellItem : IShellItem;
  pv : SHARDAPPIDINFO;
begin
  if FileExists(ItemPath) then
  begin
    if (CheckWin32Version(6, 1)) then
    begin
      if FApplicationId = '' then
         begin
           ItemPath := TPath.GetFullPath(ItemPath);
           SHAddToRecentDocs(SHARD_PATHW, LPWStr(ItemPath));
         end
          else
            begin
              ShellItem := GetShellItemForPath(ItemPath);
              pv.psi := ShellItem;
              pv.pszAppID := PChar(FApplicationId);
              SHAddToRecentDocs(SHARD_APPIDINFO, @pv );
            end;
    end
     else
       AddToRecentCategoryXP(ItemPath);
  end;
end;

procedure TJumpList.AddToRecentCategory(JumpPath: TJumpPath);
var
  ShellItem : IShellItem;
  pv : SHARDAPPIDINFO;
begin
  if Assigned(JumpPath) then
  begin
    if (CheckWin32Version(6, 1)) then
    begin
      if FApplicationId = '' then
        AddToRecentCategory(JumpPath.Path)
          else
            begin
              ShellItem := GetShellItemForPath(JumpPath.Path);
              pv.psi := ShellItem;
              pv.pszAppID := PChar(FApplicationId);
              SHAddToRecentDocs(SHARD_APPIDINFO, @pv );
            end;
    end
     else
       AddToRecentCategoryXP(JumpPath.Path);
  end;
end;

procedure TJumpList.AddToRecentCategory(JumpTask: TJumpTask);
var
  ShellLink: IShellLink;
  pv : SHARDAPPIDINFOLINK;
begin
  if (CheckWin32Version(6, 1)) then
  begin
    ShellLink := CreateLinkFromJumpTask(JumpTask, false);
    if (ShellLink <> nil) then
     begin
       if FApplicationId = '' then
         SHAddToRecentDocs(SHARD_LINK, Pointer(ShellLink))
           else
             begin
               pv.psl := ShellLink;
               pv.pszAppID := PChar(FApplicationId);
               SHAddToRecentDocs(SHARD_APPIDINFOLINK, @pv);
             end;
     end;
  end
    else
      AddToRecentCategoryXP(JumpTask.ApplicationPath);
end;

procedure TJumpList.AddToRecentCategoryXP(ItemPath: string);
begin
  ItemPath := TPath.GetFullPath(ItemPath);
  SHAddToRecentDocs(SHARD_PATHW, LPWStr(ItemPath));
end;

class function TJumpList.AddCategory(Items: TList<TJumpItem>) : IObjectArray;
var
 poa  : IObjectCollection;
 Item : TJumpItem;
 Link : IShellLink;
 Path : IShellItem;
 pUnk : IUnknown;
begin
  Result := nil;

  CoCreateInstance(CLSID_EnumerableObjectCollection, nil, CLSCTX_INPROC_SERVER, IID_IObjectCollection, pUnk);

  if pUnk <> nil then
   pUnk.QueryInterface(IObjectCollection, poa);

  if poa <> nil then
   begin
     for Item in Items do
      begin
        if (Item is TJumpTask) then
         begin
          Link := CreateLinkFromJumpTask(TJumpTask(Item), true);
          if Link <> nil then
            CheckResult(poa.AddObject(Link));
         end
            else
              if (Item is TJumpPath) then
               begin
                 Path := CreateItemFromJumpPath(TJumpPath(Item));
                 if Path <> nil then
                   CheckResult(poa.AddObject(Path));
               end
                 else
                  if (Item is TJumpSeparator) then
                   begin
                     Link := CreateSeparator(TJumpSeparator(Item));
                     if Link <> nil then
                       CheckResult(poa.AddObject(Link));
                   end;

      end;

      poa.QueryInterface(IObjectArray, Result);
   end;
end;

function TJumpList.AddJumpItem<T>: T;
begin
  Result := T.Create;
  FJumpItems.Add(Result);
end;

function TJumpList.AddJumpPath: TJumpPath;
begin
  Result := AddJumpItem<TJumpPath>;
end;

function TJumpList.AddJumpSeparator: TJumpSeparator;
begin
  Result := AddJumpItem<TJumpSeparator>;
end;

function TJumpList.AddJumpTask: TJumpTask;
begin
  Result := AddJumpItem<TJumpTask>;
end;


procedure TJumpList.Apply;
begin
  if (CheckWin32Version(6, 1)) then
    begin
      ApplyList();
    end;
end;

procedure TJumpList.ApplyList;
var
  cdl : JumpList.ICustomDestinationList;
  cMinSlots : UINT;
  poaRemoved : IObjectArray;
  Categories : TStringList;
  cnt : integer;
  Items : TList<TJumpItem>;
  Item : TJumpItem;
  pUnk : IUnknown;
  oav : IObjectArray;
  number : UINT;
begin
  CoCreateInstance(CLSID_DestinationList, nil, CLSCTX_INPROC_SERVER, IID_ICustomDestinationList, pUnk);
  if pUnk <> nil then
    pUnk.QueryInterface(JumpList.ICustomDestinationList, cdl);

  if (cdl <> nil) then
    begin
      if FApplicationId <> '' then
         CheckResult(cdl.SetAppID(PChar(FApplicationId)));

      CheckResult(cdl.BeginList(cMinSlots, IID_IObjectArray, poaRemoved));

      if ShowFrequentCategory then
        CheckResult(cdl.AppendKnownCategory(KDC_FREQUENT));

       if ShowRecentCategory then
         CheckResult(cdl.AppendKnownCategory(KDC_RECENT));

      Categories := TStringList.Create;
      Items := TList<TJumpItem>.Create;
      try
      Categories.Duplicates := dupIgnore;
      Categories.Sorted := true;

      for cnt := 0 to FJumpItems.Count - 1 do
        begin
          Categories.Add(TJumpItem(FJumpItems[cnt]).CustomCategory);
        end;

      for cnt := 0 to Categories.Count - 1 do
        begin
          Items.Clear;
          for Item in FJumpItems do
            begin
              if Item.CustomCategory = Categories[cnt] then
                Items.Add(Item);
            end;
           oav := AddCategory(Items);
           if oav <> nil then
             begin
               oav.GetCount(number);
               if (number > 0) then
                 begin
                  if Categories[cnt] = '' then
                     CheckResult(cdl.AddUserTasks(oav))
                      else
                        begin
                          CheckResult(cdl.AppendCategory(PWideChar(Categories[cnt]), oav));
                        end;
                  end;
             end;
             oav := nil;
        end;

      CheckResult(cdl.CommitList);

      finally
        Categories.Free;
        Items.Free;
      end;
    end;

   cdl := nil;
end;

class procedure TJumpList.CheckResult(ACode: HRESULT);
var
  S : string;
begin
 if (not SUCCEEDED(ACode)) then
   begin
      S := SysErrorMessage(Cardinal(ACode));
      if S = '' then
        FmtStr(S, 'Error %.8x', [ACode]);
      raise Exception.Create(S);
   end;
end;

procedure TJumpList.Clear;
var
  cnt : integer;
begin
  for cnt := 0 to FJumpItems.Count - 1 do
    FJumpItems[cnt].Free;
  FJumpItems.Clear;
end;

constructor TJumpList.Create;
var
  appId : LPCWSTR;
begin
  FFullName := GetModuleName(0);
  FJumpItems := TList<TJumpItem>.Create;
  FShowFrequentCategory := false;
  FShowRecentCategory := false;
  if CheckWin32Version(6,1) then
   begin
      appId := nil;
      GetCurrentProcessExplicitAppUserModelID(appId);
      if appId <> nil then
        begin
          FApplicationId := WideCharToString(appId);
          CoTaskMemFree(appId);
        end;
   end;
end;

class function TJumpList.CreateItemFromJumpPath(JumpPath: TJumpPath) : IShellItem;
begin
  try
    Result := GetShellItemForPath(TPath.GetFullPath(JumpPath.Path));
  except
    Result := nil;
  end;
end;

class function TJumpList.CreateLinkFromJumpTask(JumpTask: TJumpTask;
  AllowSeparators: boolean): IShellLink;
var
  pszFile: string;
  pszIconPath: string;
  Store: IPropertyStore;
  pv: TPropVariant;
  Title: TPropertyKey;
  pUnk : IUnknown;
begin
  Result := nil;

  if ((JumpTask.Title = '') and (not AllowSeparators or
        (JumpTask.CustomCategory <> ''))) then
  begin
    Exit;
  end;

  CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IID_IShellLink, pUnk);

  if pUnk = nil then
    Exit;

  pUnk.QueryInterface(IShellLink, Result);

  if Result = nil then
    Exit;

  try

    if (JumpTask.ApplicationPath <> '') then
      pszFile := JumpTask.ApplicationPath
    else
      pszFile := FFullName;

    CheckResult(Result.SetPath(PChar(pszFile)));

    if (JumpTask.WorkingDirectory <> '') then
    begin
      Result.SetWorkingDirectory(PChar(JumpTask.WorkingDirectory));
    end;

    CheckResult(Result.SetArguments(PChar(JumpTask.Arguments)));

    if (JumpTask.IconResourceIndex <> -1) then
    begin
      pszIconPath := FFullName;
      if (JumpTask.IconResourcePath <> '') then
      begin
        if (Length(JumpTask.IconResourcePath) >= 260) then
        begin
          exit;
        end;
        pszIconPath := JumpTask.IconResourcePath;
      end;
      Result.SetIconLocation(PChar(pszIconPath), JumpTask.IconResourceIndex);
    end;

    if (JumpTask.Description <> '') then
    begin
      Result.SetDescription(PChar(JumpTask.Description));
    end;

   // Result.QueryInterface(IPropertyStore, Store);
   Store := Result as IPropertyStore;

    if (JumpTask.Title <> '') then
    begin
      pv := InitPropVariantFromString(JumpTask.Title);
      Title := PKey_Title;
    end
    else
    begin
      pv := InitPropVariantFromBoolean(true);
      Title := PKEY_AppUserModel_IsDestListSeparator;
    end;

    CheckResult(Store.SetValue(Title, pv));

    CheckResult(Store.Commit());

    PropVariantClear(@pv);

  except
    Result := nil;
    raise;
  end;

end;

class function TJumpList.CreateSeparator(
  JumpSeparator: TJumpSeparator): IShellLink;
var
  Store: IPropertyStore;
  pv: TPropVariant;
  Title: TPropertyKey;
  pUnk : IUnknown;
begin
  Result := nil;
  if CheckWin32Version(6,1) then
   begin
      CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER, IUnknown, pUnk);

      if pUnk <> nil then
        pUnk.QueryInterface(IShellLink, Result);

      if Result = nil then
        Exit;

      Result.QueryInterface(IPropertyStore, Store);
      pv := InitPropVariantFromBoolean(true);
      Title := PKEY_AppUserModel_IsDestListSeparator;
      Store.SetValue(Title, pv);
      Store.Commit();
      PropVariantClear(@pv);
   end;
end;

procedure TJumpList.DeleteList;
var
  pUnk : IUnknown;
  cdl : ICustomDestinationList;
begin
  if CheckWin32Version(6,1) then
   begin
      CoCreateInstance(CLSID_DestinationList, nil, CLSCTX_INPROC_SERVER, IUnknown, pUnk);
      if pUnk <> nil then
        pUnk.QueryInterface(JumpList.ICustomDestinationList, cdl);
      if cdl <> nil then
        CheckResult(cdl.DeleteList(nil));
   end;
end;

destructor TJumpList.Destroy;
begin
  Clear;
  FJumpItems.Free;
  inherited;
end;

class function TJumpList.GetShellItemForPath(Path: string): IShellItem;
var
  hres: HResult;
  pUnk : IUnknown;
  ext : string;
begin
  if Path <> '' then
    begin
      ext := ExtractFileExt(Path);
      if (ext <> '') then
       begin
          hres := SHCreateItemFromParsingName(PChar(Path), nil, IShellItem, pUnk);
          if hres = S_OK then
           pUnk.QueryInterface(IShellItem, Result)
             else
                Result := nil;
       end
        else
          Result := nil;
    end
      else
        Result := nil;
end;


class function TJumpList.InitPropVariantFromBoolean(const Value: boolean)
    : TPropVariant;
begin
  Result.vt := VT_BOOL;
  Result.boolVal := Value;
end;

class function TJumpList.InitPropVariantFromString(const Value: string)
    : TPropVariant;
begin
  Result.vt := VT_LPWSTR;
  ShStrDupW(PChar(Value), Result.pwszVal);
end;

procedure TJumpList.SetApplicationId(const Value: string);
var
  appId : LPCWSTR;
begin
  if FApplicationId <> Value then
    begin
      FApplicationId := Value;
      if (CheckWin32Version(6, 1)) then
        begin
          appId := nil;
          GetCurrentProcessExplicitAppUserModelID(appId);
          if ((appId = nil) and (FApplicationId <> '')) then
           begin
             SetCurrentProcessExplicitAppUserModelID(PChar(FApplicationId));
           end;

          if appId <> nil then
            CoTaskMemFree(appId);
        end;
    end;
end;

end.
