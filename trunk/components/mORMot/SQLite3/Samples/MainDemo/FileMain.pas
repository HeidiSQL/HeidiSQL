/// SynFile main Window
unit FileMain;

interface

{$define DEBUGINTERNALSERVER}

{.$define EXTRACTALLRESOURCES}
// must be set globally for the whole application

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$ifdef USETMSPACK}
  AdvToolBar, AdvPreviewMenu, AdvShapeButton, AdvOfficePager,
{$endif}
  ImgList, ShellApi,
  SynCommons, SynGdiPlus, SQLite3Commons, SQLite3HttpClient,
  SQLite3ToolBar, SQLite3UI, SQLite3UILogin, SQLite3i18n,
{$ifdef DEBUGINTERNALSERVER}
  FileServer,
{$endif}
  FileTables, FileClient, FileEdit;

type
  /// SynFile main Window
  TMainForm = class(TSynForm)
    ImageList32: TImageList;
    ImageList16: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
{$ifdef DEBUGINTERNALSERVER}
    Server: TFileServer;
{$endif}
  protected
    /// event called when the User click on a ribbon button
    procedure ActionClick(Sender: TObject; const RecordClass: TSQLRecordClass;
      ActionValue: integer);
	  /// display some help
    procedure HelpClick(Sender: TObject);
  	/// a double click on the list will edit the item
    procedure ListDblClick(Sender: TObject);
	  /// will be used to refresh the UI using a Stateless approach
    procedure WMRefreshTimer(var Msg: TWMTimer); message WM_TIMER;
  	/// used to edit a record
    function Edit(aRec: TSQLFile; const aTitle: string; aReadOnly: boolean): boolean;
  public
    /// the associated database client
    Client: TFileClient;
  	/// the associated Ribbon which will handle all User Interface
    Ribbon: TFileRibbon;
	  /// release all used memory
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;


implementation

{$R *.dfm}

{$R FileMain.res}

procedure TMainForm.ActionClick(Sender: TObject;
  const RecordClass: TSQLRecordClass; ActionValue: integer);
var Action: TFileAction absolute ActionValue;
    Tab: TSQLRibbonTab;
    ActionCaption, ActionHint: string;
    Rec: TSQLFile;
    FN: TFileName;
    isMemo: boolean;
    i, n: integer;
function Open: Boolean;
begin
  result := false;
  with TOpenDialog.Create(self) do
  try
    Title := ActionHint;
    if isMemo then begin
      DefaultExt := 'txt';
      Filter := '*.txt';
    end;
    Options := [ofHideReadOnly,ofPathMustExist,ofFileMustExist,ofEnableSizing];
    if not Execute or not FileExists(FileName) then
      exit;
    FN := FileName;
    result := true;
  finally
    Free;
  end;
end;
function Save(const aFileName: TFileName): Boolean;
begin
  result := false;
  with TSaveDialog.Create(self) do
  try
    Title := ActionHint;
    Options := [ofOverwritePrompt,ofHideReadOnly,ofEnableSizing];
    if isMemo then
      DefaultExt := 'txt' else
      DefaultExt:= Copy(ExtractFileExt(aFileName),2,10);
    if DefaultExt<>'' then
      Filter := '*.'+DefaultExt;
    FileName := aFileName;
    if not Execute then
      exit;
    FN := FileName;
    result := true;
  finally
    Free;
  end;
end;
begin
  if not Visible or (Client=nil) or
    Ribbon.RefreshClickHandled(Sender,RecordClass,ActionValue,Tab) then
    exit;
  ActionCaption := Client.Model.ActionName(Action);
  ActionHint := Tab.Lister.ActionHint(Action);
  isMemo := (RecordClass=TSQLMemo) or (RecordClass=TSQLSafeMemo);
  Rec := nil;
  case Action of
    faRefresh: exit;
  end;
  case Action of
    faCreate, faCopy, faImport:
    if RecordClass.InheritsFrom(TSQLFile) then
    try
      Rec := TSQLFile(RecordClass.Create);
      case Action of
      faCopy:
        if Tab.Retrieve(Client,Tab.List.Row,false) then
          CopyObject(Tab.CurrentRecord,Rec) else
          exit;
      faImport:
        if isMemo and Open then
        with TSQLSafeMemo(Rec) do begin
          fName := S2U(ExtractFileName(FN));
          fData := S2U(AnyTextFileToString(FN));
          if (RecordClass=TSQLSafeMemo) and not Cypher(CaptionName,fData,true) then
            exit;
        end else
          exit;
      faCreate:
        if ((RecordClass=TSQLData) or (RecordClass=TSQLSafeData)) then
        if Open then begin
          Rec.fName := S2U(ExtractFileName(FN));
          if RecordClass=TSQLData then
          with TSQLData(Rec) do begin
            Data := StringFromFile(FN);
            if TSynPicture.IsPicture(FN)<>nil then
              EditForm.LoadPicture(FN,RawByteString(fPicture));
          end else
          with TSQLSafeData(Rec) do begin
            fData := StringFromFile(FN);
            if not Cypher(CaptionName,fData,true) then
              exit;
          end;
        end else
          exit;
      end;
      if Edit(Rec,ActionHint,false) then
      try
        Rec.fCreated := Rec.fModified;
        Ribbon.GotoRecord(RecordClass,Client.Add(Rec,true));
      finally
        Client.UnLock(Tab.CurrentRecord);
      end;
    finally
      Rec.Free;
    end;
    faEdit:
    if RecordClass.InheritsFrom(TSQLFile) and Tab.Retrieve(Client,Tab.List.Row,true) then
      try
        if Edit(TSQLFile(Tab.CurrentRecord),ActionHint,false) then
          if Client.Update(Tab.CurrentRecord) then
            Ribbon.GotoRecord(Tab.CurrentRecord);
      finally
        Client.UnLock(Tab.CurrentRecord);
      end;
    faExtract, faExport:
    if RecordClass.InheritsFrom(TSQLFile) then
     if Tab.Retrieve(Client,Tab.List.Row) then
      with TSQLData(Tab.CurrentRecord) do
        if (RecordClass=TSQLMemo) or (RecordClass=TSQLData) or
           Cypher(CaptionName,fData,false) then
          if Save(U2S(TSQLFile(Tab.CurrentRecord).fName)) then
            if FileFromString(fData,FN) then begin
              Client.AddAuditTrail(feRecordExported,Tab.CurrentRecord);
              ShellExecute(Handle,nil,pointer(FN),nil,nil,SW_SHOWNORMAL);
            end;
     faDelete:
     with Tab.TableToGrid do begin
       n := MarkedTotalCount;
       if n=0 then
         if (Tab.List.Row<1) or (YesNo(ActionHint,
           U2S(Client.OneFieldValue(RecordClass,'Name',Tab.CurrentID)),false)=ID_NO) or
            not Client.Delete(RecordClass,Tab.CurrentID) then
           Beep else begin
           n := Table.IDColumnHiddenValue(Tab.List.Row+1);
           Refresh;
           Ribbon.GotoRecord(RecordClass,n);
         end else
       if YesNo(ActionHint,Format(sDeleteN,[n]),false)=ID_NO then
         exit else
       if Client.TransactionBegin(RecordClass) then
       try
         for i := Table.RowCount downto 1 do
           if Marked[i] then
             if not Client.Delete(RecordClass,Table.IDColumnHiddenValue(i)) then begin
               Client.RollBack;
               Beep;
               break;
             end;
         SetMark(actUnmarkAll);
         Tab.List.Row := 0;
       finally
         Client.Commit;
         Refresh;
       end;
    end;
    faSign:
    if RecordClass.InheritsFrom(TSQLFile) and Tab.Retrieve(Client,Tab.List.Row,true) then
      try
        with TSQLData(Tab.CurrentRecord) do
        if SetAndSignContent('User',Data) then
          if Client.Update(Tab.CurrentRecord) then
            Ribbon.GotoRecord(Tab.CurrentRecord);
      finally
        Client.UnLock(Tab.CurrentRecord);
      end;
    faPrintPreview:
      Tab.Report.ShowPreviewForm;
  end;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(Ribbon);
  FreeAndNil(Client);
{$ifdef DEBUGINTERNALSERVER}
  FreeAndNil(Server);
{$endif}
  inherited;
end;

procedure TMainForm.WMRefreshTimer(var Msg: TWMTimer);
begin
  Ribbon.WMRefreshTimer(Msg);
end;

resourcestring
  sHelpN = '%s Version %s';
  sWrongPassword = 'Wrong password';

procedure TMainForm.HelpClick(Sender: TObject);
begin
  ShowMessage(format(sHelpN,[Caption,ExeVersion.Version.Detailed])+
    '\n\nSynopse mORMot '+SYNOPSE_FRAMEWORK_VERSION+
    ' - http://synopse.info');
end;

function TMainForm.Edit(aRec: TSQLFile; const aTitle: string; aReadOnly: boolean): boolean;
begin
  EditForm.Caption := ' '+aTitle;
  EditForm.ReadOnly := aReadOnly;
  result := EditForm.SetRec(aRec);
  if result then
    result := EditForm.ShowModal=mrOk else
    ShowMessage(sWrongPassword,true);
end;

procedure TMainForm.ListDblClick(Sender: TObject);
var P: TSQLRibbonTab;
begin
  P := Ribbon.GetActivePage;
  if P<>nil then
    if P.Table=TSQLAuditTrail then begin
      if P.Retrieve(Client,P.List.Row) then
        with RecordRef(TSQLAuditTrail(P.CurrentRecord).AssociatedRecord) do
          Ribbon.GotoRecord(Table(Client.Model),ID);
    end else
    ActionClick(Sender,P.Table,ord(faEdit));
end;

procedure TMainForm.FormCreate(Sender: TObject);
var P: integer;
begin
{$ifdef DEBUGINTERNALSERVER}
  Server := TFileServer.Create;
{$endif}
  LoadImageListFromEmbeddedZip(ImageList32,'buttons.bmp');
  ImageListStretch(ImageList32,ImageList16);
  Client := TFileClient.Create('localhost');
  Ribbon := TFileRibbon.Create(self, nil, nil, ImageList32, ImageList16,
    Client, ALL_ACCESS_RIGHTS, nil, Client.OnSetAction, sFileActionsToolbar,
    sFileActionsHints, nil, ActionClick, integer(faRefresh), 1, false,
    length(FileTabs), @FileTabs[0], sizeof(FileTabs[0]),
    sFileTabsGroup, ',BannerData,BannerSafe');
  Ribbon.ToolBar.Caption.Caption := Caption;
  Ribbon.ToolBar.HelpButton.OnClick := HelpClick;
  for P := 0 to high(Ribbon.Page) do
    with Ribbon.Page[P] do
      if Lister<>nil then
        Lister.Grid.OnDblClick := ListDblClick;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
{$ifdef EXTRACTALLRESOURCES}
  ExtractAllResources(
    // first, all enumerations to be translated
    [TypeInfo(TFileEvent),TypeInfo(TFileAction),TypeInfo(TPreviewAction)],
    // then some class instances (including the TSQLModel will handle all TSQLRecord)
    [Client.Model],
    // some custom classes or captions
    [],[]);
  Close;
{$else}
  //i18nLanguageToRegistry(lngFrench);
{$endif}
  Ribbon.ToolBar.ActivePageIndex := 1;
end;

end.
