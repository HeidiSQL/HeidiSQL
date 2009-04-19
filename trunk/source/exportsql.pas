unit exportsql;


// -------------------------------------
// Export Tables
// -------------------------------------


interface

uses
  Threading,
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  CheckLst,
  Buttons,
  comctrls,
  ToolWin,
  DB,
  SynEdit,
  SynMemo,
  ZDataSet,
  PngSpeedButton, StdActns, WideStrings, TntCheckLst, TntStdCtrls, Menus;

type
  TExportSQLForm = class(TForm)
    btnExport: TButton;
    btnCancel: TButton;
    dialogSave: TSaveDialog;
    barProgress: TProgressBar;
    lblProgress: TLabel;
    pageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lblSelectDbTables: TLabel;
    checkListTables: TTNTCheckListBox;
    comboSelectDatabase: TTNTComboBox;
    toolbarSelectTools: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    groupOutput: TGroupBox;
    btnFileBrowse: TPngSpeedButton;
    editFileName: TEdit;
    radioOtherDatabase: TRadioButton;
    radioFile: TRadioButton;
    comboOtherDatabase: TTNTComboBox;
    radioOtherHost: TRadioButton;
    comboOtherHost: TComboBox;
    comboOtherHostDatabase: TTNTComboBox;
    groupExampleSql: TGroupBox;
    SynMemoExampleSQL: TSynMemo;
    groupOptions: TGroupBox;
    lblTargetCompat: TLabel;
    cbxStructure: TCheckBox;
    cbxDatabase: TCheckBox;
    comboDatabase: TComboBox;
    cbxTables: TCheckBox;
    comboTables: TComboBox;
    cbxData: TCheckBox;
    comboData: TComboBox;
    comboTargetCompat: TComboBox;
    radioDirectory: TRadioButton;
    editDirectory: TEdit;
    btnDirectoryBrowse: TPngSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure comboTargetCompatChange(Sender: TObject);
    procedure comboOtherHostSelect(Sender: TObject);
    procedure comboDataChange(Sender: TObject);
    procedure comboTablesChange(Sender: TObject);
    procedure comboDatabaseChange(Sender: TObject);
    procedure cbxTablesClick(Sender: TObject);
    procedure cbxDatabaseClick(Sender: TObject);
    procedure btnDirectoryBrowseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure comboSelectDatabaseChange(Sender: TObject);
    procedure CheckListToggle(Sender: TObject);
    procedure btnFileBrowseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure radioOtherDatabaseClick(Sender: TObject);
    procedure radioFileOrDirClick(Sender: TObject);
    procedure fillcombo_anotherdb(Sender: TObject);
    procedure generateExampleSQL;
    procedure validateRadioControls(Sender: TObject);
    procedure validateControls(Sender: TObject);
    procedure cbxStructureClick(Sender: TObject);
    procedure radioOtherHostClick(Sender: TObject);
    procedure cbxDataClick(Sender: TObject);
    procedure checkListTablesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SaveSettings;
  private
    { Private declarations }
    SelectedTables: TWideStringList;
    DoOverwriteAll: Boolean;
    function InitFileStream(TableName: String; OldStream: TFileStream = nil): TFileStream;
  public
    { Public declarations }
  end;

{$I const.inc}

implementation

uses
  Main,
  Helpers,
  Synchronization,
  Communication;

{$R *.DFM}

const
  // Order of items in combo box: comboDatabase
  DB_DROP_CREATE   = 0;
  DB_CREATE        = 1;
  DB_CREATE_IGNORE = 2;
  // Order of items in combo box: comboTables
  TAB_DROP_CREATE   = 0;
  TAB_CREATE        = 1;
  TAB_CREATE_IGNORE = 2;
  // Order of items in combo box: comboData
  DATA_TRUNCATE_INSERT = 0;
  DATA_INSERT          = 1;
  DATA_INSERT_IGNORE   = 2;
  DATA_REPLACE_INTO    = 3;
  // Order of radiobutton group "Output"
  OUTPUT_FILE       = 1;
  OUTPUT_DB         = 2;
  OUTPUT_HOST       = 3;
  OUTPUT_DIR        = 4;

  // Default output compatibility
  SQL_VERSION_DEFAULT = SQL_VERSION_ANSI;

  // Pattern for use in creating destination files
  TABLENAME_PATTERN = '<table>';

var
  appHandles: array of THandle;
  cancelDialog: TForm = nil;
  remote_version: integer;
  remote_max_allowed_packet : Int64;
  target_versions : TStringList;


procedure TExportSQLForm.FormCreate(Sender: TObject);
var
  menu: TMenu;
  ds: TDataset;
begin
  menu := nil;
  if Owner is TMenuItem then
    menu := (Owner as TMenuItem).GetParentMenu;
  if menu = Mainform.popupTreeView then begin
    SelectedTables := TWideStringlist.Create;
    // If a table is selected, use that for preselection. If only a db was selected, use all tables inside it.
    if Mainform.SelectedTable <> '' then
      SelectedTables.Add(Mainform.SelectedTable)
    else if Mainform.ActiveDatabase <> '' then begin
      ds := Mainform.FetchDbTableList(Mainform.ActiveDatabase);
      while not ds.Eof do begin
        SelectedTables.Add(ds.FieldByName(DBO_NAME).AsWideString);
        ds.Next;
      end;
    end;
  end else
    SelectedTables := GetVTCaptions( Mainform.ListTables, True );

  // Assign images from main imagelist to speedbuttons
  btnFileBrowse.PngImage := Mainform.PngImageListMain.PngImages[10].PngImage;
  btnDirectoryBrowse.PngImage := Mainform.PngImageListMain.PngImages[51].PngImage;
  SetWindowSizeGrip( Self.Handle, True );
  InheritFont(Font);
end;


procedure TExportSQLForm.FormShow(Sender: TObject);
var
  i, OutputTo : Integer;
  list: TWindowDataArray;
begin
  barProgress.Position := 0;
  lblProgress.Caption := '';
  PageControl1.ActivePageIndex := 0;
  SynMemoExampleSQL.Highlighter := Mainform.SynSQLSyn1;
  SynMemoExampleSQL.Font := Mainform.SynMemoQuery.Font;

  // read dbs and Tables from treeview
  comboSelectDatabase.Items.Clear;
  Caption := Mainform.SessionName + ' - Export Tables...';
  comboSelectDatabase.Items.Assign(Mainform.Databases);
  comboSelectDatabase.ItemIndex := comboSelectDatabase.Items.IndexOf(Mainform.ActiveDatabase);
  // Select first database if at least one is available.
  if (comboSelectDatabase.ItemIndex = -1) and (comboSelectDatabase.Items.Count>0) then
    comboSelectDatabase.ItemIndex := 0;
  comboSelectDatabaseChange(self);

  // Initialize and fill list with target versions
  target_versions := TStringList.Create;
  with target_versions do
  begin
    Add( IntToStr( SQL_VERSION_ANSI ) + '=ANSI SQL' );
    Add( IntToStr( Mainform.mysql_version ) + '=Same as source (' + ConvertServerVersion(Mainform.mysql_version) + ')');
    Add( '50100=HeidiSQL w/ MySQL Server 5.1' );
    Add( '50000=HeidiSQL w/ MySQL Server 5.0' );
    Add( '40100=HeidiSQL w/ MySQL Server 4.1' );
    Add( '40000=HeidiSQL w/ MySQL Server 4.0' );
    Add( '32300=HeidiSQL w/ MySQL Server 3.23' );
    Add( '50100_mysqldump=mysqldump+mysqlcli 5.1' );
    Add( '50000_mysqldump=mysqldump+mysqlcli 5.0' );
    Add( '40100_mysqldump=mysqldump+mysqlcli 4.1' );
    Add( '40000_mysqldump=mysqldump+mysqlcli 4.0' );
    Add( '32300_mysqldump=mysqldump+mysqlcli 3.23' );
  end;

  // Add all target versions to combobox and set default option
  comboTargetCompat.Items.Clear;
  for i := 0 to target_versions.Count - 1 do
  begin
    comboTargetCompat.Items.Add( target_versions.ValueFromIndex[i] );
    if( target_versions.Names[i] = IntToStr( SQL_VERSION_DEFAULT ) ) then
    begin
      comboTargetCompat.ItemIndex := i;
    end;
  end;

  // Read options
  // WithUseDB, UseBackticks, CompleteInserts: deprecated (hardcoded true now)
  cbxStructure.Checked := GetRegValue(REGNAME_EXP_STRUCTURE, cbxStructure.Checked);
  cbxDatabase.Checked := GetRegValue(REGNAME_EXP_CREATEDB, cbxDatabase.Checked);
  cbxTables.Checked := GetRegValue(REGNAME_EXP_CREATETABLE, cbxTables.Checked);
  cbxData.Checked := GetRegValue(REGNAME_EXP_DATA, cbxData.Checked);
  comboDatabase.ItemIndex := GetRegValue(REGNAME_EXP_DBHOW, comboDatabase.ItemIndex);
  comboTables.ItemIndex := GetRegValue(REGNAME_EXP_TABLESHOW, comboTables.ItemIndex);
  comboData.ItemIndex := GetRegValue(REGNAME_EXP_DATAHOW, comboData.ItemIndex);
  comboTargetCompat.ItemIndex := GetRegValue(REGNAME_EXP_COMPAT, comboTargetCompat.ItemIndex);
  editFileName.Text := GetRegValue(REGNAME_EXP_OUTFILE, '');
  editDirectory.Text := GetRegValue(REGNAME_EXP_OUTDIR, '');
  OutputTo := GetRegValue(REGNAME_EXP_TARGET, -1);
  if OutputTo > -1 then
  begin

    {***
      @note ansgarbecker, 2007-02-24
        If OutputTo is now OUTPUT_HOST and there are no other windows
        to export to, reset OutputTo to OUTPUT_FILE to avoid the error-popup
        "You need at least 2 windows...", which should not be fired in
        FormShow rather than only when the user has really clicked that
        radiobutton.
        As a benefit, the OUTPUT_FILE won't be saved to registry here
        so the OUTPUT_HOST is still enabled in registry and will be used
        the next time if we have more than 1 window
      @see bug #1666054
    }
    // Check if all the heidisql windows are still alive.
    CheckForCrashedWindows;
    // Fetch list of heidisql windows.
    list := GetWindowList;
    if (Length(list) < 2) and (OutputTo = OUTPUT_HOST) then
      OutputTo := OUTPUT_FILE;

    case OutputTo of
      OUTPUT_FILE : radioFile.Checked := true;
      OUTPUT_DIR  : radioDirectory.Checked := true;
      OUTPUT_DB   : radioOtherDatabase.Checked := true;
      OUTPUT_HOST : radioOtherHost.Checked := true;
    end;
  end;
  Width := GetRegValue(REGNAME_EXP_WINWIDTH, Width);
  Height := GetRegValue(REGNAME_EXP_WINHEIGHT, Height);

  if EditFileName.Text = '' then
    EditFileName.Text := DirnameUserAppData + goodfilename(Mainform.ActiveDatabase + '.sql');

  // Tell the user how to use the table pattern  
  EditFileName.Hint := 'Usage for generating one file per table: c:\foo\bar_'+TABLENAME_PATTERN+'.sql';

  validateControls(Sender);
  generateExampleSQL;
  DoOverwriteAll := False;
end;


procedure TExportSQLForm.comboDatabaseChange(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.comboDataChange(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.comboOtherHostSelect(Sender: TObject);
var
  data: TDataSet;
  j: integer;
  versions : TWideStringList;
begin
  // Get both databases and version right when the radio
  // is clicked, so we can switch to the 'file' radio
  // immediately when something goes wrong.
  try
    data := RemoteExecQuery(
      appHandles[comboOtherHost.ItemIndex],
      'SHOW DATABASES',
      'Fetching remote list of databases...'
    );
    comboOtherHostDatabase.Clear;
    for j:=0 to data.RecordCount - 1 do begin
      comboOtherHostDatabase.Items.Add(data.FieldByName('Database').AsWideString);
      data.Next;
    end;
    data.Free;

    data := RemoteExecQuery(
      appHandles[comboOtherHost.ItemIndex],
      'SELECT VERSION()',
      'Probing for remote version...'
    );
    versions := explode('.', data.Fields[0].AsWideString);
    remote_version := MakeInt(versions[0]) * 10000 + MakeInt(versions[1]) * 100 + MakeInt(versions[2]);
    data.Free;

    // Fetch the max_allowed_packet variable to be sure not to
    // overload the server when using "Extended Insert"
    data := RemoteExecQuery(
      appHandles[comboOtherHost.ItemIndex],
      'SHOW VARIABLES LIKE ' + esc('max_allowed_packet'),
      'Checking for maximum allowed SQL-packet size on server '+comboOtherHost.Text+'...'
    );
    remote_max_allowed_packet := MakeInt( data.FieldByName('Value').AsString );
    data.Free;
  except
    on E: Exception do begin
      ShowMessage(E.Message);
      radioFile.Checked := true;
      E.Free;
    end;
  end;

  // Select remote database with the same name as the source db if available
  if comboOtherHostDatabase.Items.IndexOf( comboSelectDatabase.Text ) > -1 then
    comboOtherHostDatabase.ItemIndex := comboOtherHostDatabase.Items.IndexOf( comboSelectDatabase.Text )
  // Otherwise, select first database if available
  else if comboOtherHostDatabase.Items.Count > 0 then
    comboOtherHostDatabase.ItemIndex := 0;

end;

procedure TExportSQLForm.comboSelectDatabaseChange(Sender: TObject);
var
  i : Integer;
  CheckThisItem: Boolean;
  ds: TDataset;
begin
  // read tables from db
  checkListTables.Items.Clear;
  ds := Mainform.FetchDbTableList(comboSelectDatabase.Text);
  while not ds.Eof do begin
    if GetDBObjectType(ds.Fields) = NODETYPE_TABLE then
      checkListTables.Items.Add(ds.FieldByName(DBO_NAME).AsWideString);
    ds.Next;
  end;

  // select all/some:
  for i:=0 to checkListTables.Items.Count-1 do
  begin
    if Mainform.ActiveDatabase = comboSelectDatabase.Text then
      CheckThisItem := SelectedTables.IndexOf( checkListTables.Items[i] ) > -1
    else
      CheckThisItem := true;
    checkListTables.checked[i] := CheckThisItem;
  end;

  // write items for "Another Databases":
  fillcombo_anotherdb(self);
end;


procedure TExportSQLForm.comboTablesChange(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.comboTargetCompatChange(Sender: TObject);
begin
  generateExampleSQL;
end;

procedure TExportSQLForm.CheckListToggle(Sender: TObject);
begin
  // check all or none
  ToggleCheckListBox(checkListTables, ((Sender as TControl).Tag = 1));
end;



procedure TExportSQLForm.btnFileBrowseClick(Sender: TObject);
begin
  dialogSave.Filename := comboSelectDatabase.Text;
  if dialogSave.Execute then
    if dialogSave.Filename <> '' then
      EditFileName.Text := dialogSave.Filename;
end;


{**
  Parse destination filename for variables like %table% and create the file
  If an existing filestream is passed, check if it should be reused, depending on the "Part"
  @return TFileStream|Nil
}
function TExportSQLForm.InitFileStream(TableName: String; OldStream: TFileStream = nil): TFileStream;
var
  UnparsedFileName, ParsedFileName, FileName, FilePath : String;
  dlgResult: Integer;
begin
  Result := nil;

  // File or directory ?
  if radioFile.Checked then
    UnparsedFileName := EditFileName.Text
  else if radioDirectory.Checked then begin
    UnparsedFileName := EditDirectory.Text;
    // Ensure directory ends with a slash.
    // ExtractFilePath() expects a slash at the very end, otherwise the last segment
    // will be identified as filename and therefore stripped from the return value
    if UnparsedFileName[Length(UnparsedFileName)] <> '\' then
      UnparsedFileName := UnparsedFileName + '\';
    UnparsedFileName := ExtractFilePath(UnparsedFileName);
    UnparsedFileName := UnparsedFileName + TABLENAME_PATTERN + '.sql';
  end else begin
    Screen.Cursor := crDefault;
    Raise Exception.Create('Internal error: InitFileStream called in wrong context.');
  end;

  // Parse filename
  FilePath := ExtractFilePath(UnparsedFileName);
  FileName := ExtractFileName(UnparsedFileName);
  FileName := StringReplace(FileName, TABLENAME_PATTERN, TableName, [rfIgnoreCase]);
  ParsedFileName := FilePath + GoodFileName(FileName);

  // Reuse the old stream if its filename has not changed
  if (OldStream <> nil) and (OldStream.FileName = ParsedFileName) then
  begin
    Result := OldStream;
    Exit;
  end;

  // Filename has changed, so close the old file.
  if OldStream <> nil then
    OldStream.Free;

  // Warn about overwriting target file
  if FileExists(ParsedFileName) and (not DoOverwriteAll) then begin
    dlgResult := MessageDlg('Overwrite file "'+ParsedFileName+'" ?', mtConfirmation, [mbYes, mbYesToAll, mbCancel], 0 );
    if dlgResult = mrCancel then
      Exit
    else
      DoOverwriteAll := dlgResult = mrYesToAll;
  end;

  // Create the file
  try
    Result := openfs(ParsedFileName);
  except
    MessageDlg('File "'+ParsedFileName+'" could not be opened!' +  CRLF + 'Maybe in use by another application?', mterror, [mbOK], 0);
    Result.Free;
  end;
end;


procedure TExportSQLForm.btnExportClick(Sender: TObject);
var
  f                         : TFileStream;
  i,j,k,m                   : Integer;
  spos, epos                : Integer;
  exportdb,exporttables     : boolean;
  exportdata                : boolean;
  dropquery,createquery,insertquery,
  columnnames               : WideString;
  keylist                   : Array of TMyKey;
  keystr                    : WideString;
  sourceDb, destDb          : WideString;
  which                     : Integer;
  tofile,todb,tohost        : boolean;
  samehost                  : boolean;
  sameuuid                  : TGuid;
  tcount,tablecounter       : Integer;
  win2export                : THandle;
  StrProgress               : String;
  value                     : WideString;
  Escaped,fullvalue         : PChar;
  extended_insert           : Boolean;
  max_allowed_packet        : Int64;
  thesevalues               : WideString;
  valuescount               : Integer;
  donext                    : Boolean;
  PBuffer                   : PChar;
  sql, current_characterset : WideString;
  loopnumber                : Integer;
  target_version            : Integer;
  target_cliwa              : Boolean;
  ansi                      : Boolean;
  RecordCount_all, RecordCount_one, RecordNo_all,
  offset, limit             : Int64;
  sql_select                : WideString;
  query                     : TDataSet;
  OldActualDatabase         : WideString;

function sourceMask(sql: WideString): WideString;
begin
  // Same as mask(sql).
  Result := maskSql(Mainform.mysql_version, sql);
end;

function destMask(sql: WideString): WideString;
begin
  Result := maskSql(target_version, sql);
end;

function makeConditionalStmt(sql: WideString; version: integer; tofile: boolean): WideString;
begin
  // End statement with semicolon, unless destination is a live server.
  // Afterwards, wrap in conditional comment.
  Result := sql;
  if tofile then Result := Result + ';';
  Result := '/*!' + IntToStr(version) + ' ' + Result + '*/';
end;

begin
  // Check for valid directory
  if radioDirectory.Checked then begin
    if not DirectoryExists(EditDirectory.Text) then begin
      MessageDlg('The selected directory "'+EditDirectory.Text+'" does not exist.', mtError, [mbOk], 0);
      EditDirectory.SetFocus;
      Exit;
    end;
  end;

  // to where?
  tofile := radioFile.Checked or radioDirectory.Checked;
  todb := radioOtherDatabase.Checked;
  tohost := radioOtherHost.Checked;

  // export!
  pageControl1.ActivePageIndex := 0;
  Screen.Cursor := crHourGlass;

  // Initialize default-variables
  target_version := SQL_VERSION_DEFAULT;
  target_cliwa := false;
  win2export := 0;
  max_allowed_packet := 1024*1024;

  // export what?
  exportdb      := cbxDatabase.Enabled and cbxDatabase.Checked;
  exporttables  := cbxTables.Enabled and cbxTables.Checked;
  exportdata    := cbxData.Checked;

  {***
    @note ansgarbecker
    For "export to file" set max_allowed_packet to the default-value
      in mysql-server to be safe on most servers.
    For "export to another db" set max_allowed_packet to the value set on current host
    For "export to other host" set max_allowed_packet to the value set on remote host
    @see http://dev.mysql.com/doc/refman/5.0/en/packet-too-large.html
  }

  // Export to .sql-file on disk
  if tofile then begin
    // Extract name part of selected target version
    target_version := MakeInt(target_versions.Names[comboTargetCompat.ItemIndex]);
    target_cliwa := Pos('mysqldump', target_versions.Names[comboTargetCompat.ItemIndex]) > 0;
    max_allowed_packet := MakeInt( Mainform.GetVar( 'SHOW VARIABLES LIKE ' + esc('max_allowed_packet'), 1 ) );
    f := InitFileStream('header');
    if f = nil then
    begin
      Screen.Cursor := crDefault;
      Abort;
    end;
    wfs(f, '# ' + APPNAME + ' Dump ');
    wfs(f, '#');
    sourceDb := comboSelectDatabase.Text;
    destDb := comboSelectDatabase.Text;
  end;

  // Export to other database in the same window
  if todb then begin
    target_version := Mainform.mysql_version;
    max_allowed_packet := MakeInt( Mainform.GetVar( 'SHOW VARIABLES LIKE ' + esc('max_allowed_packet'), 1 ) );
    sourceDb := comboSelectDatabase.Text;
    destDb := comboOtherDatabase.Text;
  end;

  // Export to other window/host
  if tohost then begin
    target_version := remote_version;
    max_allowed_packet := remote_max_allowed_packet;
    win2export := appHandles[comboOtherHost.ItemIndex];
    sourceDb := comboSelectDatabase.Text;
    if exportdb then begin
      // Use original DB-name from source-server
      destDb := comboSelectDatabase.Text;
    end else begin
      // Use existing DB-name on target-server
      destDb := comboOtherHostDatabase.Items[comboOtherHostDatabase.ItemIndex];
    end;
  end;

  // MySQL has supported extended insert since 3.23.
  extended_insert := not (target_version = SQL_VERSION_ANSI);

  try
    // Be sure to read everything from the correct database
    Mainform.TemporaryDatabase := comboSelectDatabase.Text;

    {***
      Ouput useful header information only when exporting to file
    }
    if tofile then
    begin
      wfs(f, '# --------------------------------------------------------');
      wfs(f, WideFormat('# %-30s%s', ['Host:', Mainform.MysqlConn.Connection.HostName]));
      wfs(f, WideFormat('# %-30s%s', ['Database:', sourceDb]));
      wfs(f, WideFormat('# %-30s%s', ['Server version:', Mainform.GetVar('SELECT VERSION()')]));
      wfs(f, WideFormat('# %-30s%s', ['Server OS:', Mainform.GetVar('SHOW VARIABLES LIKE ' + esc('version_compile_os'), 1)]));
      wfs(f, WideFormat('# %-30s%s', ['Target compatibility:', comboTargetCompat.Text]));
      if extended_insert then
      begin
        wfs(f, WideFormat('# %-30s%d', ['Target max_allowed_packet:', max_allowed_packet]));
      end;
      wfs(f, WideFormat('# %-30s%s', [APPNAME + ' version:', appversion]));
      wfs(f, WideFormat('# %-30s%s', ['Date/time:', DateTimeToStr(Now)]));
      wfs(f, '# --------------------------------------------------------');
      wfs(f);
    end;

    {***
      Set characterset to current one
    }
    if Mainform.mysql_version > 40100 then
      current_characterset := Mainform.GetVar( 'SHOW VARIABLES LIKE ' + esc('character_set_connection'), 1 )
    else if Mainform.mysql_version > 40000 then
      // todo: test this, add charolation --> charset conversion table from 4.0 to 4.1+
      current_characterset := Mainform.GetVar( 'SHOW VARIABLES LIKE ' + esc('character_set'), 1 )
    else
      // todo: test this
      current_characterset := 'binary';

    {***
      Check if source and destination session is connected to the same server.
    }
    samehost := todb;
    if tohost then begin
      i := CreateGuid(sameuuid);
      if i <> 0 then raise Exception.Create('Could not create a GUID.');
      sql := esc(appName + '_' + GuidToString(sameuuid));
      try
        i := StrToInt(Mainform.GetVar('SELECT GET_LOCK(' + sql + ', 0)'));
        if i <> 1 then raise Exception.Create('Could not create a server lock.');
        query := RemoteExecQuery(
          win2export,
          'SELECT GET_LOCK(' + sql + ', 0)',
          'Checking for same server...'
        );
        i := query.Fields[0].AsInteger;
        query.Free;
        if i <> 1 then samehost := true
        else begin
        query := RemoteExecQuery(
          win2export,
          'SELECT RELEASE_LOCK(' + sql + ')',
          'Releasing remote lock...'
        );
        end;
      finally
        Mainform.ExecuteNonQuery('SELECT RELEASE_LOCK(' + sql + ')');
      end;
    end;

    {***
     Avoid destructive actions (DROP before CREATE) on same host and database.
    }
    if tohost and samehost then begin
      if exportdb and (comboDatabase.ItemIndex = DB_DROP_CREATE) then raise Exception.Create('Aborted: selected action "database recreate" on same source/destination host would drop source database.');
      if exporttables and (comboTables.ItemIndex = TAB_DROP_CREATE) then begin
        if sourceDb = destDb then raise Exception.Create('Aborted: selected action "tables recreate" on same source/destination host and database would drop source tables.');
      end;
    end;

    {***
      Some actions which are only needed if we're not in OtherDatabase-mode:
      Set character set, create and use database.
    }
    if tofile or tohost then
    begin
      // Switch to correct SQL_MODE so MySQL doesn't reject ANSI SQL
      if target_version = SQL_VERSION_ANSI then
      begin
        sql := makeConditionalStmt('SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE=''ANSI,NO_BACKSLASH_ESCAPES''', 40101, tofile);
        sql := fixSQL( sql, target_version, target_cliwa );
        if tofile then
          wfs(f, sql)
        else if tohost then
          RemoteExecNonQuery(win2export, sql );
      end;

      {***
        FOREIGN KEY import compatibility (head)
        Based on mysqldump output file
      }
      sql := makeConditionalStmt('SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0', 40014, tofile);
      sql := fixSQL( sql, target_version, target_cliwa );
      if tofile then
        wfs(f, sql)
      else if tohost then
        RemoteExecNonQuery(win2export, sql );

      if exportdb then
      begin
        {***
          DROP statement for database
        }
        if tofile then
        begin
          wfs(f);
          wfs(f);
          wfs(f, '#');
          wfs(f, '# Database structure for database ''' + sourceDb + '''');
          wfs(f, '#');
          wfs(f);
        end;
        if comboDatabase.ItemIndex = DB_DROP_CREATE then
        begin
          sql := 'DROP DATABASE IF EXISTS ' + destMask(destDb);
          if tofile then
            wfs(f, sql + ';')
          else if tohost then
            RemoteExecNonQuery(win2export, sql );
        end;

        {***
          CREATE statement for database plus database-switching
        }
        if Mainform.mysql_version < 50002 then
        begin
          sql := 'CREATE DATABASE ';
          if comboDatabase.ItemIndex = DB_CREATE_IGNORE then
          begin
            sql := sql + '/*!32312 IF NOT EXISTS*/ ';
          end;
          sql := sql + destMask(destDb);
        end
        else
        begin
          sql := Mainform.GetVar( 'SHOW CREATE DATABASE ' + sourceMask(sourceDb), 1 );
          sql := fixNewlines(sql);
          if target_version = SQL_VERSION_ANSI then
            sql := StringReplace(sql, '`', '"', [rfReplaceAll]);
          if comboDatabase.ItemIndex = DB_CREATE_IGNORE then
          begin
            Insert('/*!32312 IF NOT EXISTS*/ ', sql, Pos('DATABASE', sql) + 9);
          end;
        end;
        sql := fixSQL( sql, target_version, target_cliwa );
        if tofile then
          wfs(f, sql + ';')
        else if tohost then
          RemoteExecNonQuery(win2export, sql);
        if exporttables then
        begin
          if tofile then
          begin
            sql := 'USE ' + destMask(destDb);
            wfs(f);
            wfs(f, sql + ';');
          end;
        end;
      end;
    end;

    // How many tables?
    tcount := 0;
    for i:=0 to checkListTables.Items.Count-1 do if checkListTables.checked[i] then inc(tcount);
    barProgress.Max := 0;
    if exporttables then
      barProgress.Max := tcount;
    if exportdata then
      barProgress.Max := barProgress.Max + tcount;

    checkListTables.ItemIndex := -1;
    tablecounter := 0;

    for i:=0 to checkListTables.Items.Count-1 do if checkListTables.checked[i] then
    begin
      inc(tablecounter);
      if checkListTables.ItemIndex > -1 then
        checkListTables.Checked[checkListTables.ItemIndex] := false;
      checkListTables.ItemIndex := i;
      StrProgress := 'Table ' + inttostr(tablecounter) + '/' + inttostr(tcount) + ': ' + checkListTables.Items[i];
      lblProgress.caption := StrProgress;

      if tofile then
        f := InitFileStream(checkListTables.Items[i], f);

      if exporttables then
      begin
        if tofile then
        begin
          if f = nil then
          begin
            Screen.Cursor := crDefault;
            Abort;
          end;
        end;

        dropquery := '';
        if comboTables.ItemIndex = TAB_DROP_CREATE then begin
          if tofile then
            dropquery := 'DROP TABLE IF EXISTS ' + destMask(checkListTables.Items[i])
          else
            dropquery := 'DROP TABLE IF EXISTS ' + destMask(destDb) + '.' + destMask(checkListTables.Items[i]);
        end;

        createquery := '';
        if tofile then begin
          createquery := '#' + crlf;
          createquery := createquery + '# Table structure for table ''' + checkListTables.Items[i] + '''' + crlf;
          createquery := createquery + '#' + crlf + crlf;
        end;

        {***
          Let the server generate the CREATE TABLE statement if the version allows that
        }
        if Mainform.mysql_version >= 32320 then
        begin
          Query := Mainform.GetResults('SHOW CREATE TABLE ' + sourceMask(checkListTables.Items[i]));
          sql := Query.Fields[1].AsWideString;
          Query.Close;
          FreeAndNil(Query);
          sql := fixNewlines(sql);
          sql := fixSQL( sql, target_version, target_cliwa );
        end
        {***
          Generate CREATE TABLE statement by hand on old servers
        }
        else if Mainform.mysql_version < 32320 then begin
          Query := Mainform.GetResults( 'SHOW COLUMNS FROM ' + sourceMask(checkListTables.Items[i]));
          if tofile then
            sql := 'CREATE TABLE ' + destMask(checkListTables.Items[i]) + ' (' + crlf
          else
            sql := sql + 'CREATE TABLE ' + destMask(destDb) + '.' + sourceMask(checkListTables.Items[i]) + ' (' + crlf;
          for j := 1 to Query.Fieldcount do
          begin
            sql := sql + '  ' + destMask(Query.Fields[0].AsWideString) + ' ' + Query.Fields[1].AsWideString;
            if Query.Fields[2].AsString <> 'YES' then
              sql := sql + ' NOT NULL';
            if Query.Fields[4].AsWideString <> '' then
              sql := sql + ' DEFAULT ''' + Query.Fields[4].AsWideString + '''';
            if Query.Fields[5].AsWideString <> '' then
              sql := sql + ' ' + Query.Fields[5].AsWideString;
            if j < Query.Fieldcount then
              sql := sql + ',' + crlf;
          end;
          Query.Close;
          FreeAndNil(Query);

          // Keys:
          Query := Mainform.GetResults( 'SHOW KEYS FROM ' + sourceMask(checkListTables.Items[i]));
          setLength(keylist, 0);
          keystr := '';
          if Query.RecordCount > 0 then
            keystr := ',';

          for j := 1 to Query.RecordCount do
          begin
            which := -1;

            for k:=0 to length(keylist)-1 do
            begin
              if keylist[k].Name = Query.Fields[2].AsString then // keyname exists!
                which := k;
            end;
            if which = -1 then
            begin
              setlength(keylist, length(keylist)+1);
              which := high(keylist);
              keylist[which].Columns := TWideStringList.Create;
              with keylist[which] do // set properties for new key
              begin
                Name := Query.Fields[2].AsString;
                if Query.Fields[2].AsString = 'PRIMARY' then
                  _type := 'PRIMARY'
                else if Query.FieldCount >= 10 then if Query.Fields[9].AsString = 'FULLTEXT' then
                  _type := 'FULLTEXT'
                else if Query.Fields[1].AsString = '1' then
                  _type := ''
                else if Query.Fields[1].AsString = '0' then
                  _type := 'UNIQUE';
              end;
            end;
            keylist[which].Columns.add(destMask(Query.Fields[4].AsWideString)); // add column(s)
            Query.Next;
          end;
          Query.Close;
          FreeAndNil(Query);
          for k:=0 to high(keylist) do
          begin
            if k > 0 then
              keystr := keystr + ',';
            if keylist[k].Name = 'PRIMARY' then
              keystr := keystr + crlf + '  PRIMARY KEY ('
            else
              keystr := keystr + crlf + '  ' + keylist[k]._type + ' KEY ' + destMask(keylist[k].Name) + ' (';
            keystr := keystr + implodestr(',', keylist[k].Columns) + ')';
          end;
          sql := sql + keystr + crlf + ')';
        end; // mysql_version < 32320

        if not tofile then Insert(destMask(destDb) + '.', sql, Pos('TABLE', sql) + 6);
        
        if comboTables.ItemIndex = TAB_CREATE_IGNORE then
        begin
          Insert('/*!32312 IF NOT EXISTS*/ ', sql, Pos('TABLE', sql) + 6);
        end;

        createquery := createquery + sql;

        // Output CREATE TABLE to file
        if tofile then begin
          createquery := createquery + ';' + crlf;
          if dropquery <> '' then dropquery := dropquery + ';' + crlf;
          wfs(f);
          wfs(f);
          if dropquery <> '' then wfs(f, dropquery);
          wfs(f, createquery);
        end

        // Run CREATE TABLE on another Database
        else if todb then begin
          if comboTables.ItemIndex = TAB_DROP_CREATE then
            Mainform.ExecUpdateQuery( dropquery );
          Mainform.ExecUpdateQuery( createquery );
        end

        // Run CREATE TABLE on another host
        else if tohost then begin
          if comboTables.ItemIndex = TAB_DROP_CREATE then
            RemoteExecNonQuery(win2export, dropquery);
          RemoteExecNonQuery(win2export, createquery);
        end;

        barProgress.StepIt;
      end;

      {***
        Export data
      }
      if exportdata then
      begin
        // Set to mysql-readable char:
        DecimalSeparator := '.';
        columnnames := ' (';
        Query := Mainform.GetResults( 'SHOW FIELDS FROM ' + sourceMask(checkListTables.Items[i]));
        for k:=1 to Query.RecordCount do
        begin
          if k>1 then
            columnnames := columnnames + ', ';
          columnnames := columnnames + destMask(Query.Fields[0].AsWideString);
          Query.Next;
        end;
        columnnames := columnnames+')';
        Query.Close;
        FreeAndNil(Query);

        if tofile then
        begin
          wfs(f);
          wfs(f);
          wfs(f, '#');
          wfs(f, '# Dumping data for table ''' + checkListTables.Items[i] + '''');
          wfs(f, '#');
          wfs(f);
        end;

        if comboData.ItemIndex = DATA_TRUNCATE_INSERT then
        begin
          if tofile then
          begin
            wfs(f, 'TRUNCATE TABLE ' + destMask(checkListTables.Items[i]) + ';');
          end
          else if todb then
          begin
            Mainform.ExecUpdateQuery('TRUNCATE TABLE ' + sourceMask(destDb) + '.' + checkListTables.Items[i]);
          end
          else if tohost then
          begin
            RemoteExecNonQuery(win2export, 'TRUNCATE TABLE ' + destMask(destDb) + '.' + checkListTables.Items[i]);
          end;
        end;

        // Set rows per step limit and detect total row count
        // Be sure to do this step before the table is locked!
        limit := 5000;
        RecordCount_all := MakeInt(Mainform.GetNamedVar('SHOW TABLE STATUS LIKE '+esc(checkListTables.Items[i]), 'Rows'));

        if RecordCount_all = 0 then begin
          if tofile then
          begin
            wfs(f, '# No data found.');
            wfs(f);
          end;
        end
        else
        begin
          if tofile then
          begin
            wfs(f, 'LOCK TABLES '+ destMask(checkListTables.Items[i]) +' WRITE;' );
            wfs(f, fixSQL( '/*!40000 ALTER TABLE '+ destMask(checkListTables.Items[i]) +' DISABLE KEYS;*/', target_version, target_cliwa) );
          end
          else if todb then
          begin
            Mainform.ExecUpdateQuery( 'LOCK TABLES ' + sourceMask(destDb) + '.' + sourceMask(checkListTables.Items[i])+ ' WRITE, ' + sourceMask(sourceDb) + '.' + sourceMask(checkListTables.Items[i])+ ' WRITE');
            if target_version > 40000 then
              Mainform.ExecUpdateQuery( 'ALTER TABLE ' + sourceMask(destDb) + '.' + sourceMask(checkListTables.Items[i])+ ' DISABLE KEYS' );
          end
          else if tohost then
          begin
            RemoteExecNonQuery(win2export, 'LOCK TABLES ' + destMask(destDb) + '.' + destMask(checkListTables.Items[i]) + ' WRITE');
            if target_version > 40000 then
              RemoteExecNonQuery(win2export, 'ALTER TABLE ' + destMask(destDb) + '.' + destMask(checkListTables.Items[i]) + ' DISABLE KEYS');
          end;
        end;

        offset := 0;
        loopnumber := 0;
        RecordNo_all := 0;

        // Loop as long as (offset+limit) have not reached (recordcount)
        while true do
        begin
          inc( loopnumber );
          debug('loopnumber: '+formatnumber(loopnumber));

          // Check if end of data has been reached
          if ( (offset) >= RecordCount_all) or ( (limit = -1) and (loopnumber > 1) ) then
          begin
            break;
          end;

          sql_select := 'SELECT * FROM ' + sourceMask(comboSelectDatabase.Text) + '.' + sourceMask(checkListTables.Items[i]);
          if limit > -1 then
          begin
            sql_select := sql_select + ' LIMIT ' + IntToStr( offset ) + ', ' + IntToStr( limit );
            offset := offset + limit;
          end;

          // Execute SELECT
          Query := Mainform.GetResults( sql_select );

          insertquery := '';
          valuescount := 0;
          j := 0;
          donext := true;
          RecordCount_one := Query.RecordCount;
          while not Query.Eof do
          begin
            inc(j);
            inc(RecordNo_all);
            lblProgress.caption := StrProgress + ' (Record ' + FormatNumber(RecordNo_all) + ')';
            if j mod 100 = 0 then
              lblProgress.Repaint;
            if insertquery = '' then
            begin
              case comboData.ItemIndex of
                DATA_TRUNCATE_INSERT: insertquery := 'INSERT INTO ';
                DATA_INSERT: insertquery := 'INSERT INTO ';
                DATA_INSERT_IGNORE: insertquery := 'INSERT IGNORE INTO ';
                DATA_REPLACE_INTO: insertquery := 'REPLACE INTO ';
              end;
              if tofile then
                insertquery := insertquery + destMask(checkListTables.Items[i])
              else
                insertquery := insertquery + destMask(destDb) + '.' + destMask(checkListTables.Items[i]);
              insertquery := insertquery + columnnames;
              insertquery := insertquery + ' VALUES' + crlf + #9;
            end;
            thesevalues := '(';

            for k := 0 to Query.fieldcount-1 do
            begin
              if Query.Fields[k].IsNull then
                value := 'NULL'
              else
              case Query.Fields[k].DataType of
                ftInteger, ftSmallint, ftWord:
                  value := Query.Fields[k].AsWideString;
                ftBoolean:
                  value := esc( Bool2Str( Query.Fields[k].AsBoolean ) );
                ftBlob:
                  if Query.Fields[k].AsString <> '' then
                    value := '0x' + BinToWideHex(Query.Fields[k].AsString)
                  else
                    value := esc('');
                else
                  value := esc( Query.Fields[k].AsWideString, False, target_version );
              end;
              thesevalues := thesevalues + value;
              if k < Query.Fieldcount-1 then
                thesevalues := thesevalues + ',';
            end;
            thesevalues := thesevalues + ')';
            if extended_insert then
            begin
              if (valuescount > 1)
                and (length(insertquery)+length(thesevalues)+2 >= max_allowed_packet)
                then
              begin
                // Rewind one record and throw thesevalues away
                donext := false;
                dec(j);
                delete( insertquery, length(insertquery)-3, 4 );
              end
              else if j = RecordCount_one then
              begin
                insertquery := insertquery + thesevalues;
              end
              else
              begin
                inc(valuescount);
                insertquery := insertquery + thesevalues + ',' + crlf + #9;
                Query.Next;
                continue;
              end;
            end
            else
            begin
              insertquery := insertquery + thesevalues;
            end;
            if tofile then begin
              insertquery := insertquery + ';';
              wfs(f, insertquery)
            end else if todb then
              Mainform.ExecUpdateQuery(insertquery)
            else if tohost then
              RemoteExecNonQuery(win2export, insertquery);
            if donext then
              Query.Next;
            donext := true;
            insertquery := '';
          end;
          Query.Close;
          FreeAndNil(Query);
        end;
        // Set back to local setting:
        setLocales;

        if RecordCount_all > 0 then begin
          if tofile then
          begin
            wfs(f, fixSQL( '/*!40000 ALTER TABLE '+destMask(checkListTables.Items[i])+' ENABLE KEYS;*/', target_version, target_cliwa) );
            wfs(f, 'UNLOCK TABLES;' );
          end
          else if todb then
          begin
            if target_version > 40000 then
              Mainform.ExecUpdateQuery( 'ALTER TABLE ' + sourceMask(destDb) + '.' + sourceMask(checkListTables.Items[i]) + ' ENABLE KEYS' );
            Mainform.ExecUpdateQuery( 'UNLOCK TABLES' );
          end
          else if tohost then
          begin
            if target_version > 40000 then
              RemoteExecNonQuery(win2export, 'ALTER TABLE ' + destMask(destDb) + '.' + destMask(checkListTables.Items[i]) + ' ENABLE KEYS');
            RemoteExecNonQuery(win2export, 'UNLOCK TABLES');
          end;
        end;
        barProgress.StepIt;
      end;
    end;

    if tofile then
    begin
      f := InitFileStream('footer', f);
      if f = nil then
      begin
        Screen.Cursor := crDefault;
        Abort;
      end;
    end;

    // Restore old value for SQL_MODE
    if (tofile or tohost) and (target_version = SQL_VERSION_ANSI) then
    begin
      sql := makeConditionalStmt('SET SQL_MODE=@OLD_SQL_MODE', 40101, tofile);
      sql := fixSql(sql, target_version, target_cliwa);
      if tofile then
        wfs(f, sql)
      else if tohost then
        RemoteExecNonQuery(win2export, sql );
    end;

    {***
      FOREIGN KEY import compatibility (foot)
      Based on mysqldump output file
    }
    sql := makeConditionalStmt('SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS', 40014, tofile);
    sql := fixSQL( sql, target_version, target_cliwa );
    if tofile then
      wfs(f, sql)
    else if tohost then
      RemoteExecNonQuery(win2export, sql );

  FINALLY
    Mainform.TemporaryDatabase := '';
    if tofile then
      f.Free;
    Screen.Cursor := crDefault;
  END;

  SaveSettings;
  close;
end;

procedure TExportSQLForm.radioOtherDatabaseClick(Sender: TObject);
begin
  if comboSelectDatabase.Items.Count <= 1 then begin
    MessageDLG('There must be more than one database to enable this option.', mtError, [mbOK], 0);
    radioFile.OnClick(self);
    abort;
  end;
  validateRadioControls(Sender);
  validateControls(Sender);
  generateExampleSql;
end;

procedure TExportSQLForm.radioFileOrDirClick(Sender: TObject);
begin
  validateRadioControls(Sender);
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.fillcombo_anotherdb(Sender: TObject);
var
  lastdb: WideString;
begin
  comboOtherDatabase.Items := comboSelectDatabase.Items;
  comboOtherDatabase.Items.delete(comboSelectDatabase.ItemIndex);
  if comboOtherDatabase.ItemIndex = -1 then begin
    lastdb := Utf8Decode(GetRegValue(REGNAME_EXP_DESTDB, ''));
    comboOtherDatabase.ItemIndex := comboOtherDatabase.Items.IndexOf(lastdb);
  end;
  if (comboOtherDatabase.ItemIndex = -1) and (comboOtherDatabase.Items.Count > 0) then
    comboOtherDatabase.ItemIndex := 0;
end;

procedure TExportSQLForm.generateExampleSQL;
const
  STR_DROP_DB              = 'DROP DATABASE IF EXISTS <db>;' + CRLF;
  STR_CREATE_DB            = 'CREATE DATABASE <db>;' + CRLF;
  STR_CREATE_DB_IGNORE     = 'CREATE DATABASE IF NOT EXISTS <db>;' + CRLF;
  STR_DROP_TABLE           = 'DROP TABLE IF EXISTS <table>;' + CRLF;
  STR_CREATE_TABLE         = 'CREATE TABLE <table> <definition>;' + CRLF;
  STR_CREATE_TABLE_IGNORE  = 'CREATE TABLE IF NOT EXISTS <table> <definition>;' + CRLF;
  STR_TRUNCATE_TABLE       = 'TRUNCATE TABLE <table>;' + CRLF;
  STR_INSERT               = 'INSERT INTO <table> (<columns>) VALUES (<values>)';
  STR_INSERT_IGNORE        = 'INSERT IGNORE INTO <table> (<columns>) VALUES (<values>)';
  STR_REPLACE_INTO         = 'REPLACE INTO <table> (<columns>) VALUES (<values>)';
  STR_END_INSERT_REG       = ';' + CRLF + '(...)' + CRLF;
  STR_END_INSERT_EXT       = ', (<values>)...;' + CRLF;
var
  s: string;
procedure add(str: string); overload;
begin
  s := s + str;
end;
procedure add(str1: string; str2: string); overload;
begin
  s := s + str1 + str2;
end;
begin
  s := '';
  if cbxStructure.Enabled and cbxStructure.Checked then begin
    if cbxDatabase.Enabled and cbxDatabase.Checked then begin
      case comboDatabase.ItemIndex of
        DB_DROP_CREATE:        add(STR_DROP_DB, STR_CREATE_DB);
        DB_CREATE:             add(STR_CREATE_DB);
        DB_CREATE_IGNORE:      add(STR_CREATE_DB_IGNORE);
      end;
      add( CRLF );
    end;
    if cbxTables.Enabled and cbxTables.Checked then begin
      case comboTables.ItemIndex of
        TAB_DROP_CREATE:       add(STR_DROP_TABLE, STR_CREATE_TABLE);
        TAB_CREATE:            add(STR_CREATE_TABLE);
        TAB_CREATE_IGNORE:     add(STR_CREATE_TABLE_IGNORE);
      end;
      add( CRLF );
    end;
  end;
  if cbxData.Enabled and cbxData.Checked then begin
    case comboData.ItemIndex of
      DATA_TRUNCATE_INSERT:  add(STR_TRUNCATE_TABLE, STR_INSERT);
      DATA_INSERT:           add(STR_INSERT);
      DATA_INSERT_IGNORE:    add(STR_INSERT_IGNORE);
      DATA_REPLACE_INTO:     add(STR_REPLACE_INTO);
    end;
    if comboTargetCompat.ItemIndex > 0 then add(STR_END_INSERT_EXT)
    else add(STR_END_INSERT_REG);
  end;
  s := TrimRight(s);
  SynMemoExampleSql.Text := s;
end;

procedure TExportSQLForm.validateRadioControls(Sender: TObject);
const
  EnabledColor = clWindow;
  DisabledColor = clBtnFace;
var
  ControlToFocus : TWinControl;
begin
  // Disable all controls ...
  EditFileName.Enabled := False;
  EditFileName.Color := DisabledColor;
  btnFileBrowse.Enabled := False;
  EditDirectory.Enabled := False;
  EditDirectory.Color := DisabledColor;
  btnDirectoryBrowse.Enabled := False;
  comboOtherDatabase.Enabled := False;
  comboOtherDatabase.Color := DisabledColor;
  comboOtherHost.Enabled := False;
  comboOtherHost.Color := DisabledColor;
  comboOtherHostDatabase.Enabled := False;
  comboOtherHostDatabase.Color := DisabledColor;

  // Silence compiler warning
  ControlToFocus := EditFileName;

  // ... and re-enable the selected controlset
  if radioFile.Checked then begin
    EditFileName.Enabled := True;
    EditFileName.Color := EnabledColor;
    btnFileBrowse.Enabled := True;
  end else if radioDirectory.Checked then begin
    EditDirectory.Enabled := True;
    EditDirectory.Color := EnabledColor;
    btnDirectoryBrowse.Enabled := True;
    ControlToFocus := EditDirectory;
  end else if radioOtherDatabase.Checked then begin
    comboOtherDatabase.Enabled := True;
    comboOtherDatabase.Color := EnabledColor;
    ControlToFocus := comboOtherDatabase;
  end else if radioOtherHost.Checked then begin
    comboOtherHost.Enabled := True;
    comboOtherHost.Color := EnabledColor;
    comboOtherHostDatabase.Color := EnabledColor;
    ControlToFocus := comboOtherHost;
  end;
  if ControlToFocus.CanFocus then
    ControlToFocus.SetFocus;

  // Disable target selection if exporting to known session.
  comboTargetCompat.Enabled := radioFile.Checked or radioDirectory.Checked;
end;

procedure TExportSQLForm.validateControls(Sender: TObject);
begin
  cbxDatabase.Enabled := cbxStructure.Checked and (radioFile.Checked or radioDirectory.Checked or radioOtherHost.Checked);
  comboDatabase.Enabled := cbxDatabase.Enabled and cbxDatabase.Checked;

  cbxTables.Enabled := cbxStructure.Checked;
  comboTables.Enabled := cbxTables.Enabled and cbxTables.Checked;

  comboData.Enabled := cbxData.Checked;

  // Should possible be in validateRadioControls() but is dependent on properties decided above.
  comboOtherHostDatabase.Enabled := not (cbxDatabase.Enabled and cbxDatabase.Checked);

  // Prevent choosing export of db struct + data but no table struct.
  if cbxData.Checked then begin
    if Sender = cbxTables then cbxDatabase.Checked := cbxDatabase.Checked and cbxTables.Checked
    else cbxTables.Checked := cbxTables.Checked or cbxDatabase.Checked;
  end;

  btnExport.Enabled := cbxData.Checked or
  (cbxStructure.Checked and (cbxDatabase.Checked or cbxTables.Checked));

  if cbxStructure.Checked and cbxDatabase.Checked and (comboDatabase.ItemIndex = DB_DROP_CREATE) then begin
    // 'drop tables', 'truncate data', 'insert ignore' and 'replace into' is useless.
    comboTables.ItemIndex := TAB_CREATE;
    comboTables.Enabled := false;
    comboData.ItemIndex := DATA_INSERT;
    comboData.Enabled := false;
  end;

  if cbxStructure.Checked and cbxTables.Checked and (comboTables.ItemIndex = TAB_DROP_CREATE) then begin
    // 'truncate data', 'insert ignore' and 'replace into' is useless.
    comboData.ItemIndex := DATA_INSERT;
    comboData.Enabled := false;
  end;
end;

procedure TExportSQLForm.cbxStructureClick(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.cbxDataClick(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.cbxTablesClick(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.checkListTablesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
begin
  // if exist tables and is more than one, 'cause
  // if exists only one, this action is not needed
  // and CTRL Key is pressed
  if ((checkListTables.Count > 1) and (ssCtrl in Shift)) then
  begin
    case (Key) of
      VK_UP: // UP Key
      begin
        // find the selected, starting from the second table
        for i := 1 to (checkListTables.Count - 1) do
        begin
          if (checkListTables.Selected[i]) then
          begin
            // move the selected to up
            checkListTables.Items.Move(i, (i - 1));
            // select it again
            checkListTables.Selected[i] := True;
            // stop the find
            Break;
          end;
        end;
      end;
      VK_DOWN: // DOWN Key
      begin
        // find the selected, starting from the first table, but
        // ignore the last
        for i := 0 to (checkListTables.Count - 2) do
        begin
          if (checkListTables.Selected[i]) then
          begin
            // move the selected to down
            checkListTables.Items.Move(i, (i + 1));
            // select it again
            checkListTables.Selected[i] := True;
            // stop the find
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TExportSQLForm.cbxDatabaseClick(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;


procedure TExportSQLForm.radioOtherHostClick(Sender: TObject);
var
  list: TWindowDataArray;
  i, k: integer;
begin
  // Check if all the heidisql windows are still alive.
  CheckForCrashedWindows;

  // Fetch list of heidisql windows.
  list := GetWindowList;

  // Fill list of hosts.
  comboOtherHost.Items.Clear;
  SetLength(appHandles, High(list));
  k := 0;
  for i := 0 to High(list) do with list[i] do begin
    // Do not include current window.
    if appHandle <> MainForm.Handle then begin
      // Do not include non-connected windows.
      if connected then begin
        if namePostfix <> 0 then name := name + Format(' (%d)', [namePostFix]);
        comboOtherHost.Items.Add(name);
        appHandles[k] := appHandle;
        k := k + 1;
      end;
    end;
  end;

  // Abort if no other windows.
  if comboOtherHost.Items.Count = 0 then begin
    MessageDLG('You need at least two open connection-windows to enable this option.', mtError, [mbOK], 0);
    radioFile.Checked := true;
    abort;
  end;

  // Select first host and call change event
  comboOtherHost.ItemIndex := 0;
  comboOtherHost.OnSelect(comboOtherHost);

  // De-select database structure to enable database dropdown box.
  cbxDatabase.Checked := false;

  validateRadioControls(Sender);
  validateControls(Sender);
  generateExampleSql;
end;



{***
  Save settings in registry, should be called just before closing
  the form, but not when Cancel was pressed.
}
procedure TExportSQLForm.SaveSettings;
var
  OutputTo : Byte;
begin
  OpenRegistry;
  with MainReg do begin
    // WithUseDB, UseBackticks, CompleteInserts, WithDropTable: deprecated (currently not automagically removed)
    WriteBool(REGNAME_EXP_STRUCTURE,     cbxStructure.Checked);
    WriteBool(REGNAME_EXP_CREATEDB,      cbxDatabase.Checked);
    WriteBool(REGNAME_EXP_CREATETABLE,   cbxTables.Checked);
    WriteBool(REGNAME_EXP_DATA,          cbxData.Checked);
    WriteInteger(REGNAME_EXP_DBHOW,      comboDatabase.ItemIndex);
    WriteInteger(REGNAME_EXP_TABLESHOW,  comboTables.ItemIndex);
    WriteInteger(REGNAME_EXP_DATAHOW,    comboData.ItemIndex);
    WriteInteger(REGNAME_EXP_COMPAT,     comboTargetCompat.ItemIndex);
    WriteString(REGNAME_EXP_OUTFILE,     EditFileName.Text);
    WriteString(REGNAME_EXP_OUTDIR,      EditDirectory.Text);
    OutputTo := OUTPUT_FILE;
    if radioDirectory.checked then
      OutputTo := OUTPUT_DIR
    else if radioOtherDatabase.checked then
      OutputTo := OUTPUT_DB
    else if radioOtherHost.checked then
      OutputTo := OUTPUT_HOST;
    WriteInteger(REGNAME_EXP_TARGET,     OutputTo );
    WriteString(REGNAME_EXP_DESTDB,      Utf8Encode(comboOtherDatabase.Text));
    WriteInteger(REGNAME_EXP_WINWIDTH,   Width );
    WriteInteger(REGNAME_EXP_WINHEIGHT,  Height );
  end;
end;


{**
  Browse for a directory
}
procedure TExportSQLForm.btnDirectoryBrowseClick(Sender: TObject);
var
  Browse: TBrowseForFolder;
begin
  // Avoid using platform specific SelectDirectory()
  Browse := TBrowseForFolder.Create(Self);
  Browse.Folder := EditDirectory.Text;
  Browse.DialogCaption := 'Select output directory';
  // Enable "Create new folder" button
  Browse.BrowseOptions := Browse.BrowseOptions - [bifNoNewFolderButton] + [bifNewDialogStyle];
  if Browse.Execute then
    EditDirectory.Text := Browse.Folder;
end;


end.




