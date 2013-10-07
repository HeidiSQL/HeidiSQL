unit SynDBExplorerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Grids, ExtCtrls, StdCtrls, Consts,
  SynCommons, SQLite3Commons, SynSQLite3, SynZip,
  SQLite3i18n, SQLite3UI, SQLite3UIEdit, SQLite3UILogin, SQLite3ToolBar,
  SynTaskDialog,  SynDB, SynDBOracle, SynOleDB, SynDBSQLite3, SynDBODBC,
  SynDBExplorerClasses, SynDBExplorerFrame, ComCtrls;

type
  TDbExplorerMain = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    MainCaption: string;
    Page: TSynPager;
    PageNew: TSynPage;
    procedure PageChange(Sender: TObject);
    procedure PageDblClick(Sender: TObject);
  public
    ConnectionName: string;
    Tables: TStringList;
    Props: TSQLDBConnectionProperties;
    function CreateFrame: TDBExplorerFrame;
  end;

var
  DbExplorerMain: TDbExplorerMain;

  
resourcestring
  sSelectAConnection = 'Select a connection';
  sNew = 'New connection';
  sNewOne = 'New';
  sConnectionHints = 'Display name|Database type|Server name '+
    '(for "Generic OLEDB", use ADO-like connection string, and ignore other fields; '+
    'for SQLite3 or Jet, specify the full file name)|'+
    'Database name (unneeded for Oracle/SQLite3/Jet/ODBC)|User login|'+
    'User password (set ? for prompt)';
  sSelectOrCreateAConnection = 'Select a connection to be used, or\n'+
    'click on "New connection" to create one.';
  sPleaseWaitN = 'Connecting to %s...';
  sUpdateConnection = 'Update connection settings';
  sPassPromptN = 'Please enter password for %s@%s:';


implementation

{$R Vista.res}
{$R *.dfm}

procedure TDbExplorerMain.FormDestroy(Sender: TObject);
begin
  Tables.Free;
  Props.Free;
end;

function Crypt(const s: RawUTF8): RawUTF8;
var i: integer;
begin // just not to be written in plain ascii in .config file
  SetLength(result,length(s));
  for i := 0 to length(s)-1 do
    PByteArray(result)[i] := PByteArray(s)[i] xor (i+137);
end;

procedure TDbExplorerMain.FormCreate(Sender: TObject);
var Conns: TSQLRestServerStaticInMemory;
function TryConnect(C: TSQLConnection; LoadTableNames: boolean): boolean;
const CONN_CLASSES: array[TExpConnectionType] of TSQLDBConnectionPropertiesClass =
  (TSQLDBOracleConnectionProperties,TOleDBOracleConnectionProperties,
   TOleDBMSOracleConnectionProperties,TOleDBMSSQLConnectionProperties,
   TOleDBConnectionProperties,TSQLDBSQLite3ConnectionProperties,
   TOleDBJetConnectionProperties,TODBCConnectionProperties);
var i: integer;
    Pass: RawUTF8;
begin
  result := false;
  try
    Pass := Crypt(C.Password);
    if Pass='?' then 
      Pass := StringToUTF8(InputBox(Caption,format(sPassPromptN,
        [UTF8ToString(C.UserName),UTF8ToString(C.Server)]),'',true));
    if C.Connection=ctGenericOLEDB then begin
      Props := TOleDBConnectionProperties.Create('','','','');
      with TOleDBConnectionProperties(Props) do begin
        ConnectionString := UTF8ToWideString(C.Server);
        if LoadTableNames then begin
          ConnectionStringDialogExecute(Handle);
          C.Server := WideStringToUTF8(ConnectionString);
        end;
      end;
    end else 
      Props := CONN_CLASSES[C.Connection].Create(C.Server,C.Database,C.UserName,Pass);
    ConnectionName := U2S(C.Ident);
    with CreateTempForm(format(sPleaseWaitN,[ConnectionName]),nil,True) do
    try
      MainCaption := MainCaption+' - '+ConnectionName;
      if LoadTableNames then begin // retrieve all needed info from DB
        Props.GetTableNames(C.fTableNames);     // retrieve and set table names        
        C.ForeignKeys := CompressString(Props.ForeignKeysData); // set foreign keys
        if Conns<>nil then
          Conns.Modified := true;
      end else begin
        Props.ThreadSafeConnection.Connect;
        Props.ForeignKeysData := UncompressString(C.ForeignKeys); 
      end;
      for i := 0 to High(C.TableNames) do
        Tables.Add(U2S(C.TableNames[i]));
      result := true;
    finally
      Screen.Cursor := crDefault;
      Free;
    end;
  except
    on E: Exception do begin
      ShowMessage(E.Message,true);
      FreeAndNil(Props);
    end;
  end;
end;
var Btns: TCommonButtons;
    Task: TTaskDialog;
    C: TSQLConnection;
    CmdLine: TExpConnectionType;
    FN, msg: string;
    i, res: Integer;
begin
  DefaultFont.Name := 'Tahoma';
  DefaultFont.Size := 9;
  Tables := TStringList.Create;
  C := nil;
  with TSQLConnection.RecordProps do begin
    AddFilterOrValidate('Ident',TSynFilterTrim.Create);
    AddFilterOrValidate('Ident',TSynValidateText.Create);
    AddFilterOrValidate('Server',TSynValidateText.Create);
  end;
  MainCaption := Caption;
  if (ParamCount=1) and FileExists(paramstr(1)) then begin
    FN := paramstr(1);
    if IsJetFile(FN) then
      CmdLine := ctJet_mdbOLEDB else
    if IsSQLite3File(FN) then
      CmdLine := ctSqlite3 else begin
      ShowMessage(FN+'?',True);
      exit;
    end;
    C := TSQLConnection.Create;
    try
      C.Connection := CmdLine;
      C.Ident := S2U(FN);
      C.Server := C.Ident;
      TryConnect(C,True);
    finally
      C.Free;
    end;
  end else begin
    Conns := TSQLRestServerStaticInMemory.Create(
      TSQLConnection,nil,ChangeFileExt(paramstr(0),'.config'),false);
    try
      Conns.ExpandedJSON := true; // for better human reading and modification
      Task.Title := MainCaption;
      Task.Inst := sSelectAConnection;
      Task.Content := sSelectOrCreateAConnection;
      if Conns.Count=0 then
        Btns := [cbCancel] else begin
        for i := 0 to Conns.Count-1 do
          Task.Selection := Task.Selection+U2S(TSQLConnection(Conns[i]).Ident)+#10;
        Btns := [cbOk,cbCancel];
        Task.Query := U2S(TSQLConnection(Conns[0]).Ident);
        Task.Verify := sUpdateConnection;
      end;
      Task.VerifyChecked := false;
      Task.Buttons := sNew;
      res := Task.Execute(Btns,0,[],tiQuestion);
      case res of
      mrOk:
        if Task.VerifyChecked then begin
          C := TSQLConnection(Conns[Task.SelectionRes]);
          msg := Task.Verify;
        end else
          TryConnect(TSQLConnection(Conns[Task.SelectionRes]),false);
      100: begin
        C := TSQLConnection.Create;
        msg := sNew;
      end;
      end;
      if C<>nil then
        with TRecordEditForm.Create(self) do
        try
          C.Password := Crypt(C.Password);
          SetRecord(nil,C,nil,nil,sConnectionHints,0,msg);
          if ShowModal=mrOk then begin
            C.Password := Crypt(C.Password);
            if TryConnect(C,true) and (res=100) then
              Conns.AddOne(C) else
              if res=100 then
                FreeAndNil(C);
          end;
        finally
          Free;
        end;
    finally
      Conns.Free;
    end;
  end;
  Page := TSynPager.Create(self);
  Page.ControlStyle := Page.ControlStyle+[csClickEvents]; // enable OnDblClick
  Page.Parent := self;
  Page.Align := alClient;
  PageNew := TSynPage.Create(self);
  PageNew.Caption := sNewOne;
  PageNew.PageControl := Page;
  Page.OnChange := PageChange;
  Page.OnDblClick := PageDblClick;
end;

procedure TDbExplorerMain.FormShow(Sender: TObject);
begin
  if Props=nil then begin
    Close;
    exit;
  end;
  Caption := MainCaption;
  SetStyle(self);
  CreateFrame;
end;

procedure TDbExplorerMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift=[] then
  case Key of
    VK_F9:
      with Page.ActivePage do
        if TObject(Tag).InheritsFrom(TDBExplorerFrame) then
          TDBExplorerFrame(Tag).BtnExecClick(nil);
  end;
end;

function TDbExplorerMain.CreateFrame: TDBExplorerFrame;
var P: TSynPage;
begin
  P := TSynPage.Create(self);
  P.PageControl := Page;
  P.PageIndex := Page.PageCount-2;
  result := TDBExplorerFrame.Create(P);
  result.Parent := P;
  result.Align := alClient;
  result.Tables := Tables;
  result.Props := Props;
  result.EditTableChange(nil);
  result.Name := 'P'+IntToStr(GetTickCount);
  P.Tag := PtrInt(result);
  Page.ActivePage := P;
  result.EditTable.SetFocus;
  SetStyle(P);
end;

procedure TDbExplorerMain.PageChange(Sender: TObject);
begin
  if Page.ActivePage=PageNew then
    CreateFrame;
end;

procedure TDbExplorerMain.PageDblClick(Sender: TObject);
var n, i: Integer;
begin
  i := Page.ActivePageIndex;
  n := Page.PageCount-2;
  if n>0 then begin
    Page.ActivePage.Free;
    if i=n then
      Page.ActivePageIndex := n-1;
  end;
end;

end.
