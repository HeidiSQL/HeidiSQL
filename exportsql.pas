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
  Registry,
  ToolWin,
  DB,
  SynEdit,
  SynMemo,
  ZDataSet;

type
  TExportSQLForm = class(TForm)
    btnExport: TButton;
    btnCancel: TButton;
    groupOutput: TGroupBox;
    dialogSave: TSaveDialog;
    btnFileBrowse: TBitBtn;
    editFileName: TEdit;
    radioOtherDatabase: TRadioButton;
    radioFile: TRadioButton;
    comboOtherDatabase: TComboBox;
    barProgress: TProgressBar;
    radioOtherHost: TRadioButton;
    comboOtherHost: TComboBox;
    lblProgress: TLabel;
    pageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lblSelectDbTables: TLabel;
    checkListTables: TCheckListBox;
    comboSelectDatabase: TComboBox;
    toolbarSelectTools: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    cbxStructure: TCheckBox;
    cbxData: TCheckBox;
    cbxExtendedInsert: TCheckBox;
    lblTargetCompat: TLabel;
    comboTargetCompat: TComboBox;
    cbxDatabase: TCheckBox;
    cbxTables: TCheckBox;
    comboTables: TComboBox;
    groupExampleSql: TGroupBox;
    comboDatabase: TComboBox;
    comboData: TComboBox;
    SynMemoExampleSQL: TSynMemo;
    comboOtherHostDatabase: TComboBox;
    procedure comboOtherHostSelect(Sender: TObject);
    procedure cbxExtendedInsertClick(Sender: TObject);
    procedure comboDataChange(Sender: TObject);
    procedure comboTablesChange(Sender: TObject);
    procedure comboDatabaseChange(Sender: TObject);
    procedure cbxTablesClick(Sender: TObject);
    procedure cbxDatabaseClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure comboSelectDatabaseChange(Sender: TObject);
    procedure CheckListToggle(Sender: TObject);
    procedure btnFileBrowseClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure radioOtherDatabaseClick(Sender: TObject);
    procedure radioFileClick(Sender: TObject);
    procedure fillcombo_anotherdb(Sender: TObject);
    procedure generateExampleSQL;
    procedure validateRadioControls(Sender: TObject);
    procedure validateControls(Sender: TObject);
    procedure cbxStructureClick(Sender: TObject);
    procedure radioOtherHostClick(Sender: TObject);
    procedure cbxDataClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure checkListTablesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function ExportTablesWindow (AOwner : TComponent; Flags : String = '') : Boolean;

{$I const.inc}

implementation

uses
  Main,
  Childwin,
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

var
  appHandles: array of THandle;
  cancelDialog: TForm = nil;
  remote_version: integer;
  remote_max_allowed_packet : Int64;

function ExportTablesWindow (AOwner : TComponent; Flags : String = '') : Boolean;
var
  f : TExportSQLForm;
begin
  f := TExportSQLForm.Create(AOwner);
  // todo: pass params if needed
  Result := (f.ShowModal = mrOK);
  FreeAndNil (f);
end;

procedure TExportSQLForm.btnCancelClick(Sender: TObject);
begin
  close;
end;

procedure TExportSQLForm.FormShow(Sender: TObject);
var
  tn : TTreeNode;
  i, OutputTo : Integer;
  dbtree_db : String;
begin
  barProgress.Position := 0;
  lblProgress.Caption := '';
  PageControl1.ActivePageIndex := 0;
  SynMemoExampleSQL.Highlighter := TMDIChild(MainForm.ActiveMDIChild).SynSQLSyn1;
  SynMemoExampleSQL.Font := TMDIChild(MainForm.ActiveMDIChild).SynMemoQuery.Font;
  
  // read dbs and Tables from treeview
  comboSelectDatabase.Items.Clear;
  with TMDIChild(Mainform.ActiveMDIChild) do
  begin
    self.Caption := MysqlConn.Description + ' - Export Tables...';
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Level = 1 then
        comboSelectDatabase.Items.Add(tn.Text);
    end;

    if DBRightClickSelectedItem <> nil then
    begin
      case DBRightClickSelectedItem.Level of
        1 : dbtree_db := DBRightClickSelectedItem.Text;
        2 : dbtree_db := DBRightClickSelectedItem.Parent.Text;
        3 : dbtree_db := DBRightClickSelectedItem.Parent.Parent.Text;
      end;
    end;

    for i:=0 to comboSelectDatabase.Items.Count-1 do
    begin
      if ((dbtree_db = '') and (comboSelectDatabase.Items[i] = ActualDatabase))
        or ((dbtree_db <> '') and (comboSelectDatabase.Items[i] = dbtree_db)) then
      begin
        comboSelectDatabase.ItemIndex := i;
        break;
      end;
    end;

    if comboSelectDatabase.ItemIndex = -1 then
      comboSelectDatabase.ItemIndex := 0;

  end;
  comboSelectDatabaseChange(self);

  // Read options
  with TRegistry.Create do
    if OpenKey(REGPATH, true) then begin
    // WithUseDB, UseBackticks, CompleteInserts: deprecated (hardcoded true now)
    if Valueexists('ExportStructure') then cbxStructure.Checked := ReadBool('ExportStructure');
    if Valueexists('WithCreateDatabase') then cbxDatabase.Checked := ReadBool('WithCreateDatabase');
    if Valueexists('WithCreateTable') then cbxTables.Checked := ReadBool('WithCreateTable');
    if Valueexists('ExportData') then cbxData.Checked := ReadBool('ExportData');
    if Valueexists('CreateDatabaseHow') then comboDatabase.ItemIndex := ReadInteger('CreateDatabaseHow');
    if Valueexists('CreateTablesHow') then comboTables.ItemIndex := ReadInteger('CreateTablesHow')
    else if Valueexists('WithDropTable') and ReadBool('WithDropTable') then comboTables.ItemIndex := TAB_DROP_CREATE;
    if Valueexists('CreateDataHow') then comboData.ItemIndex := ReadInteger('CreateDataHow');
    if Valueexists('ExtendedInsert') then cbxExtendedInsert.Checked := ReadBool('ExtendedInsert');
    if Valueexists('Compatibility') then comboTargetCompat.ItemIndex := ReadInteger('Compatibility');
    if Valueexists('exportfilename') then editFileName.Text := ReadString('exportfilename');
    if Valueexists('ExportSQL_OutputTo') then
    begin
      OutputTo := ReadInteger('ExportSQL_OutputTo');
      case OutputTo of
        OUTPUT_FILE : radioFile.Checked := true;
        OUTPUT_DB   : radioOtherDatabase.Checked := true;
        OUTPUT_HOST : radioOtherHost.Checked := true;
      end;
    end;
    if ValueExists('ExportSQL_WindowWidth') then Width := ReadInteger('ExportSQL_WindowWidth');
    if ValueExists('ExportSQL_WindowHeight') then Height := ReadInteger('ExportSQL_WindowHeight');
  end;

  if EditFileName.Text = '' then
    EditFileName.Text := ExtractFilePath(paramstr(0)) + 'export.sql';

  validateControls(Sender);
  generateExampleSQL;
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
  versions : TStringList;
begin
  // Get both databases and version right when the radio
  // is clicked, so we can switch to the 'file' radio
  // immediately when something goes wrong.
  try
    data := RemoteExecQuerySimple(
      appHandles[comboOtherHost.ItemIndex],
      'SHOW DATABASES',
      'Fetching remote list of databases...'
    );
    comboOtherHostDatabase.Clear;
    for j:=0 to data.RecordCount - 1 do begin
      comboOtherHostDatabase.Items.Add(data.FieldByName('Database').AsString);
      data.Next;
    end;
    data.Free;

    data := RemoteExecQuerySimple(
      appHandles[comboOtherHost.ItemIndex],
      'SELECT VERSION()',
      'Probing for remote version...'
    );
    versions := explode('.', data.Fields[0].AsString);
    remote_version := MakeInt(versions[0]) * 10000 + MakeInt(versions[1]) * 100 + MakeInt(versions[2]);
    data.Free;

    // Fetch the max_allowed_packet variable to be sure not to
    // overload the server when the user has checked "Extended Insert"
    data := RemoteExecQuerySimple(
      appHandles[comboOtherHost.ItemIndex],
      'SHOW VARIABLES LIKE "max_allowed_packet"',
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
end;

procedure TExportSQLForm.comboSelectDatabaseChange(Sender: TObject);
var
  tn, child : TTreeNode;
  i,j : Integer;
  dbtree_table : String;
begin
  // read tables from db
  checkListTables.Items.Clear;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Text = comboSelectDatabase.Text then
      begin
        child := tn.getFirstChild;
        for j:=0 to tn.Count-1 do
        begin
           // Sometimes a column-name of the last table gets into the table-list.
           // Seems like a bug in getNextChild
           if child.Level = 2 then
            checkListTables.Items.Add(child.Text);
          child := tn.getNextChild(child);
        end;
      end;
    end;

    // select all/some:
    for i:=0 to checkListTables.Items.Count-1 do
    begin
      if DBRightClickSelectedItem <> nil then
      begin
        case DBRightClickSelectedItem.Level of
          2 : dbtree_table := DBRightClickSelectedItem.Text;
          3 : dbtree_table := DBRightClickSelectedItem.Parent.Text;
        end;
        case DBRightClickSelectedItem.Level of
          1 : checkListTables.checked[i] := true;
          2,3 : checkListTables.checked[i] := dbtree_table = checkListTables.Items[i];
        end;
      end
      else if ActualDatabase = comboSelectDatabase.Text then for j:=0 to ListTables.Items.Count-1 do
      begin
        if checkListTables.Items[i] = ListTables.Items[j].Caption then
        begin
          checkListTables.checked[i] := ListTables.Items[j].Selected;
          break;
        end;
      end
      else
        checkListTables.checked[i] := true;
    end;
    DBRightClickSelectedItem := nil;
  end;

  // write items for "Another Databases":
  fillcombo_anotherdb(self);
end;


procedure TExportSQLForm.comboTablesChange(Sender: TObject);
begin
  validateControls(Sender);
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


procedure TExportSQLForm.btnExportClick(Sender: TObject);
var
  f                         : TFileStream;
  i,j,k,m                   : Integer;
  exportdb,exporttables     : boolean;
  exportdata                : boolean;
  dropquery,createquery,insertquery,
  columnnames               : String;
  keylist                   : Array of TMyKey;
  keystr,DB2export          : String;
  which                     : Integer;
  tofile,todb,tohost        : boolean;
  tcount,tablecounter       : Integer;
  win2export                : THandle;
  StrProgress               : String;
  value                     : String;
  Escaped,fullvalue         : PChar;
  max_allowed_packet        : Int64;
  thesevalues               : String;
  valuescount, limit        : Integer;
  donext                    : Boolean;
  PBuffer                   : PChar;
  sql, current_characterset : String;
  target_version, loopnumber: Integer;
  ansi                      : Boolean;
  RecordCount_all, RecordCount_one, RecordNo_all,
  offset                    : Int64;
  sql_select                : String;
  cwin                      : TMDIChild;
  query                     : TZReadOnlyQuery;
begin
  // export!
  pageControl1.ActivePageIndex := 0;
  Screen.Cursor := crHourGlass;

  // Initialize default-variables
  target_version := SQL_VERSION_ANSI;
  max_allowed_packet := 1024*1024;

  // export what?
  exportdb      := cbxDatabase.Enabled and cbxDatabase.Checked;
  exporttables  := cbxTables.Enabled and cbxTables.Checked;
  exportdata    := cbxData.Checked;

  // to where?
  tofile := radioFile.Checked;
  todb := radioOtherDatabase.Checked;
  tohost := radioOtherHost.Checked;

  // for easy use of methods in childwin
  cwin := TMDIChild(Mainform.ActiveMDIChild);

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
    case comboTargetCompat.ItemIndex of
      0: target_version := SQL_VERSION_ANSI;
      1: target_version := 32300;
      2: target_version := 40000;
      3: target_version := 51000;
    end;
    try
      f := TFileStream.Create(EditFileName.Text, fmCreate);
    except
      messagedlg('File "'+EditFileName.Text+'" could not be opened!' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
      f.free;
      Screen.Cursor := crDefault;
      abort;
    end;
    wfs(f, '# ' + APPNAME + ' Dump ');
    wfs(f, '#');
    DB2export := comboSelectDatabase.Text;
  end;

  // Export to other database in the same window
  if todb then begin
    target_version := cwin.mysql_version;
    // Only query max_allowed_packet if we really need that value later
    if cbxExtendedInsert.Checked then
    begin
      max_allowed_packet := MakeInt( cwin.GetVar( 'SHOW VARIABLES LIKE ''max_allowed_packet''', 1 ) );
    end;
    DB2export := comboOtherDatabase.Text;
  end;

  // Export to other window/host
  if tohost then begin
    target_version := remote_version;
    max_allowed_packet := remote_max_allowed_packet;
    win2export := appHandles[comboOtherHost.ItemIndex];
    if cbxDatabase.Checked then
    begin
      // Use original DB-name from source-server
      DB2export := comboSelectDatabase.Text;
    end
    else
      // Use existing DB-name on target-server
      DB2export := comboOtherHostDatabase.Items[comboOtherHostDatabase.ItemIndex];
  end;

  try
    // Create helper-dataset
    query := TZReadOnlyQuery.Create(self);
    query.Connection := cwin.MysqlConn.Connection;
    // Be sure to read everything from the correct database
    cwin.ExecUseQuery( comboSelectDatabase.Text );

    {***
      Ouput useful header information only when exporting to file
    }
    if tofile then
    begin
      wfs(f, '# --------------------------------------------------------');
      wfs(f, '# Host:                 ' + query.Connection.HostName );
      wfs(f, '# Database:             ' + DB2export );
      wfs(f, '# Server version:       ' + cwin.GetVar( 'SELECT VERSION()' ) );
      wfs(f, '# Server OS:            ' + cwin.GetVar( 'SHOW VARIABLES LIKE "version_compile_os"', 1 ) );
      wfs(f, '# Target-Compatibility: ' + comboTargetCompat.Text );
      wfs(f, '# Extended INSERTs:     ' + Bool2Str( cbxExtendedInsert.Checked ) );
      if cbxExtendedInsert.Checked then
      begin
        wfs(f, '# max_allowed_packet:   ' + inttostr(max_allowed_packet) );
      end;
      wfs(f, '# ' + APPNAME + ' version:     ' + appversion );
      wfs(f, '# --------------------------------------------------------');
      wfs(f);
    end;

    {***
      Some actions which are only needed if we're not in OtherDatabase-mode:
      Set character set, create and use database.
    }
    if tofile or tohost then
    begin
      {***
        Set characterset to current one
      }
      current_characterset := cwin.GetVar( 'SHOW VARIABLES LIKE "character_set_connection"', 1 );
      if current_characterset <> '' then
      begin
        sql := '/*!40100 SET CHARACTER SET ' + current_characterset + ';*/';
        sql := fixSQL( sql, target_version );
        if tofile then
          wfs(f, sql)
        else if tohost then
          RemoteExecNonQuery(win2export, sql );
      end;
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
          wfs(f, '# Database structure for database ''' + DB2export + '''');
          wfs(f, '#');
          wfs(f);
        end;
        if comboDatabase.ItemIndex = DB_DROP_CREATE then
        begin
          sql := 'DROP DATABASE IF EXISTS ' + maskSql(target_version, DB2export) + ';';
          if tofile then
            wfs(f, sql)
          else if tohost then
            RemoteExecNonQuery(win2export, sql );
        end;

        {***
          CREATE statement for database plus database-switching
        }
        if cwin.mysql_version < 50002 then
        begin
          sql := 'CREATE DATABASE ';
          if comboDatabase.ItemIndex = DB_CREATE_IGNORE then
          begin
            sql := sql + '/*!32312 IF NOT EXISTS*/ ';
          end;
          sql := sql + maskSql(target_version, DB2export) + ';';
        end
        else
        begin
          sql := cwin.GetVar( 'SHOW CREATE DATABASE ' + mainform.mask(DB2export), 1 );
          sql := fixNewlines(sql) + ';';
          if target_version = SQL_VERSION_ANSI then
            sql := StringReplace(sql, '`', '"', [rfReplaceAll]);
          if comboDatabase.ItemIndex = DB_CREATE_IGNORE then
          begin
            Insert('/*!32312 IF NOT EXISTS*/ ', sql, Pos('DATABASE', sql) + 9);
          end;
        end;
        sql := fixSQL( sql, target_version);
        if tofile then
          wfs(f, sql )
        else if tohost then
          RemoteExecNonQuery(win2export, sql );
        if exporttables then
        begin
          sql := 'USE ' + maskSql(target_version, DB2export) + ';';
          if tofile then
          begin
            wfs(f);
            wfs(f, sql);
          end
          else if tohost then
            RemoteExecNonQuery(win2export, sql);
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

      if exporttables then
      begin
        dropquery := '';
        if comboTables.ItemIndex = TAB_DROP_CREATE then begin
          if tofile then
            dropquery := 'DROP TABLE IF EXISTS ' + maskSql(target_version, checkListTables.Items[i])
          else
            dropquery := 'DROP TABLE IF EXISTS ' + maskSql(target_version, DB2Export) + '.' + maskSql(target_version, checkListTables.Items[i]);
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
        if cwin.mysql_version >= 32320 then
        begin
          cwin.GetResults('SHOW CREATE TABLE ' + mainform.mask(checkListTables.Items[i]), Query );
          sql := Query.Fields[1].AsString;
          sql := fixNewlines(sql);
          if target_version = SQL_VERSION_ANSI then
          begin
            sql := StringReplace(sql, '`', '"', [rfReplaceAll]);
            j := max(pos('TYPE=', sql), pos('ENGINE=', sql));
            // Delphi's Pos() lacks a start-at parameter.  Admittedly very ugly hack to achieve said effect.
            k := 0;
            while k <= j do k := k + 1 + Pos(' ', Copy(sql, k + 1, Length(sql)));
            Delete(sql, j, k - j);
          end;
          {***
            @note ansgarbecker
              The ENGINE and TYPE options specify the storage engine for the table.
              ENGINE was added in MySQL 4.0.18 (for 4.0) and 4.1.2 (for 4.1).
              It is the preferred option name as of those versions, and TYPE has
              become deprecated. TYPE is supported throughout the 4.x series, but
              likely will be removed in the future.
            @see http://dev.mysql.com/doc/refman/4.1/en/create-table.html
          }
          if target_version < 40000 then
          begin
            sql := stringreplace(sql, 'ENGINE=', 'TYPE=', [rfReplaceAll]);
          end
          else if target_version >= 51000 then
          begin
            sql := stringreplace(sql, 'TYPE=', 'ENGINE=', [rfReplaceAll]);
          end;
        end
        {***
          Generate CREATE TABLE statement by hand on old servers
        }
        else if cwin.mysql_version < 32320 then begin
          cwin.GetResults( 'SHOW COLUMNS FROM ' + mainform.mask(checkListTables.Items[i]), Query );
          if tofile then
            sql := 'CREATE TABLE IF NOT EXISTS ' + maskSql(target_version, checkListTables.Items[i]) + ' (' + crlf
          else
            sql := sql + 'CREATE TABLE IF NOT EXISTS ' + maskSql(target_version, DB2Export) + '.' + cwin.mask(checkListTables.Items[i]) + ' (' + crlf;
          for j := 1 to Query.Fieldcount do
          begin
            sql := sql + '  ' + maskSql(target_version, Query.Fields[0].AsString) + ' ' + Query.Fields[1].AsString;
            if Query.Fields[2].AsString <> 'YES' then
              sql := sql + ' NOT NULL';
            if Query.Fields[4].AsString <> '' then
              sql := sql + ' DEFAULT ''' + Query.Fields[4].AsString + '''';
            if Query.Fields[5].AsString <> '' then
              sql := sql + ' ' + Query.Fields[5].AsString;
            if j < Query.Fieldcount then
              sql := sql + ',' + crlf;
          end;

          // Keys:
          cwin.GetResults( 'SHOW KEYS FROM ' + cwin.mask(checkListTables.Items[i]), Query );
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
              keylist[which].Columns := TStringList.Create;
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
            keylist[which].Columns.add(maskSql(target_version, Query.Fields[4].AsString)); // add column(s)
            Query.Next;
          end;
          for k:=0 to high(keylist) do
          begin
            if k > 0 then
              keystr := keystr + ',';
            if keylist[k].Name = 'PRIMARY' then
              keystr := keystr + crlf + '  PRIMARY KEY ('
            else
              keystr := keystr + crlf + '  ' + keylist[k]._type + ' KEY ' + maskSql(target_version, keylist[k].Name) + ' (';
            keystr := keystr + implodestr(',', keylist[k].Columns) + ')';
          end;
          sql := sql + keystr + crlf + ')';
        end; // mysql_version < 32320

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
          cwin.ExecUseQuery( DB2export );
          if comboTables.ItemIndex = TAB_DROP_CREATE then
            cwin.ExecUpdateQuery( dropquery );
          cwin.ExecUpdateQuery( createquery );
          cwin.ExecUseQuery( comboSelectDatabase.Text );
        end

        // Run CREATE TABLE on another host
        else if tohost then begin
          RemoteExecUseNonQuery(win2export, cwin.mysql_version, DB2Export);
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
        cwin.GetResults( 'SHOW FIELDS FROM ' + mainform.mask(checkListTables.Items[i]), Query );
        for k:=1 to Query.RecordCount do
        begin
          if k>1 then
            columnnames := columnnames + ', ';
          columnnames := columnnames + maskSql(target_version, Query.Fields[0].AsString);
          Query.Next;
        end;
        columnnames := columnnames+')';

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
            wfs(f, 'TRUNCATE TABLE ' + maskSql(target_version, checkListTables.Items[i]) + ';');
          end
          else if todb then
          begin
            cwin.ExecUpdateQuery('TRUNCATE TABLE ' + cwin.mask(DB2Export) + '.' + checkListTables.Items[i]);
          end
          else if tohost then
          begin
            RemoteExecNonQuery(win2export, 'TRUNCATE TABLE ' + maskSql(target_version, DB2Export) + '.' + checkListTables.Items[i]);
          end;
        end;

        {***
          Detect average row size and limit the number of rows fetched at
          once if more than ~ 5 MB of data
          Be sure to do this step before the table is locked!
        }
        RecordCount_all := MakeInt( cwin.GetVar( 'SELECT COUNT(*) FROM ' + cwin.mask(checkListTables.Items[i]) ) );
        limit := cwin.GetCalculatedLimit( checkListTables.Items[i] );

        if tofile then
        begin
          wfs(f, fixSQL( '/*!40000 ALTER TABLE '+ maskSql(target_version, checkListTables.Items[i]) +' DISABLE KEYS;*/', target_version) );
          wfs(f, 'LOCK TABLES '+ maskSql(target_version, checkListTables.Items[i]) +' WRITE;' );
        end
        else if todb then
        begin
          cwin.ExecUseQuery(DB2Export);
          if target_version > 40000 then
            cwin.ExecUpdateQuery( 'ALTER TABLE ' + cwin.mask(checkListTables.Items[i])+' DISABLE KEYS' );
          {***
            @note ansgarbecker, 2007-02-18:
            Normally we would have to apply a WRITE-LOCK to the target-table.
            Unfortunately the server invokes a "Table xyz was not locked"-error
            on the source-table if we do that:
            cwin.ExecQuery( 'LOCK TABLES ' + cwin.mask(DB2Export) + '.' +  cwin.mask(checkListTables.Items[i])+' WRITE' );
            Even when applying a WRITE- or READ-LOCK also to the source-table,
            the INSERTs seem to be not executed.
            So the best solution for now seems to be to not LOCK the target-table,
            running the risk that the table is edited by other concurrent users
          }
          cwin.ExecUseQuery(comboSelectDatabase.Text);
        end
        else if tohost then
        begin
          if target_version > 40000 then
            RemoteExecNonQuery(win2export, 'ALTER TABLE ' + maskSql(target_version, DB2Export) + '.' + maskSql(target_version, checkListTables.Items[i]) + ' DISABLE KEYS');
          RemoteExecNonQuery(win2export, 'LOCK TABLES ' + maskSql(target_version, DB2Export) + '.' + maskSql(target_version, checkListTables.Items[i]) + ' WRITE');
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

          sql_select := 'SELECT * FROM ' + cwin.mask(comboSelectDatabase.Text) + '.' + cwin.mask(checkListTables.Items[i]);
          if limit > -1 then
          begin
            sql_select := sql_select + ' LIMIT ' + IntToStr( offset ) + ', ' + IntToStr( limit );
            offset := offset + limit;
          end;

          // Execute SELECT
          cwin.GetResults( sql_select, Query );

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
                insertquery := insertquery + maskSql(target_version, checkListTables.Items[i])
              else
                insertquery := insertquery + maskSql(target_version, DB2Export) + '.' + maskSql(target_version, checkListTables.Items[i]);
              insertquery := insertquery + columnnames;
              insertquery := insertquery + ' VALUES ';
            end;
            thesevalues := '(';

            for k := 0 to Query.fieldcount-1 do
            begin
              if Query.Fields[k].IsNull then
                value := 'NULL'
              else
              case Query.Fields[k].DataType of
                ftInteger, ftSmallint, ftWord:
                  value := Query.Fields[k].AsString;
                ftBoolean:
                  value := esc( Bool2Str( Query.Fields[k].AsBoolean ) );
                else
                  value := escapeAuto( Query.Fields[k].AsString );
              end;
              thesevalues := thesevalues + value;
              if k < Query.Fieldcount-1 then
                thesevalues := thesevalues + ',';
            end;
            thesevalues := thesevalues + ')';
            if cbxExtendedInsert.Checked then
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
            if tofile then
              wfs(f, insertquery + ';')
            else if todb then
              cwin.ExecUpdateQuery(insertquery)
            else if tohost then
              RemoteExecNonQuery(win2export, insertquery);
            if donext then
              Query.Next;
            donext := true;
            insertquery := '';
          end;
          Query.Close;
        end;
        // Set back to local setting:
        setLocales;

        if tofile then
        begin
          wfs(f, 'UNLOCK TABLES;' );
          wfs(f, fixSQL( '/*!40000 ALTER TABLE '+maskSql(target_version, checkListTables.Items[i])+' ENABLE KEYS;*/', target_version) );
        end
        else if todb then
        begin
          if target_version > 40000 then
            cwin.ExecUpdateQuery( 'ALTER TABLE ' + maskSql(target_version, DB2Export) + '.' + maskSql(target_version, checkListTables.Items[i]) + ' ENABLE KEYS' );
        end
        else if tohost then
        begin
          RemoteExecNonQuery(win2export, 'UNLOCK TABLES');
          if target_version > 40000 then
            RemoteExecNonQuery(win2export, 'ALTER TABLE ' + maskSql(target_version, DB2Export) + '.' + maskSql(target_version, checkListTables.Items[i]) + ' ENABLE KEYS');
        end;
        barProgress.StepIt;
      end;
    end;
    if cwin.ActualDatabase <> '' then
    begin
      cwin.ExecUseQuery( cwin.ActualDatabase );
    end;
  FINALLY
    if tofile then
      f.Free;
    Screen.Cursor := crDefault;
  END;

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

procedure TExportSQLForm.radioFileClick(Sender: TObject);
begin
  validateRadioControls(Sender);
  validateControls(Sender);
  generateExampleSQL;
end;

procedure TExportSQLForm.fillcombo_anotherdb(Sender: TObject);
begin
  comboOtherDatabase.Items := comboSelectDatabase.Items;
  comboOtherDatabase.Items.delete(comboSelectDatabase.ItemIndex);
  if comboOtherDatabase.ItemIndex = -1 then
    comboOtherDatabase.ItemIndex := 0;
end;

procedure TExportSQLForm.generateExampleSQL;
const
  STR_DROP_DB              = 'DROP DATABASE <db>;' + CRLF;
  STR_CREATE_DB            = 'CREATE DATABASE <db>;' + CRLF;
  STR_CREATE_DB_IGNORE     = 'CREATE DATABASE IF NOT EXISTS <db>;' + CRLF;
  STR_DROP_TABLE           = 'DROP TABLE <table>;' + CRLF;
  STR_CREATE_TABLE         = 'CREATE TABLE <table> <definition>;' + CRLF;
  STR_CREATE_TABLE_IGNORE  = 'CREATE TABLE IF NOT EXISTS <table> <definition>;' + CRLF;
  STR_TRUNCATE_TABLE       = 'TRUNCATE TABLE <table>;' + CRLF;
  STR_INSERT               = 'INSERT INTO <table> (<columns>) <values>';
  STR_INSERT_IGNORE        = 'INSERT IGNORE INTO <table> (<columns>) <values>';
  STR_REPLACE_INTO         = 'REPLACE INTO <table> (<columns>) <values>';
  STR_END_INSERT_REG       = ';' + CRLF + '(...)' + CRLF;
  STR_END_INSERT_EXT       = ', <values...>;' + CRLF;
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
    if cbxExtendedInsert.Checked then add(STR_END_INSERT_EXT)
    else add(STR_END_INSERT_REG);
  end;
  s := TrimRight(s);
  SynMemoExampleSql.Text := s;
end;

procedure TExportSQLForm.validateRadioControls(Sender: TObject);
begin
  if radioFile.Checked then begin
    EditFileName.Enabled := true;
    EditFileName.Color := clWindow;
    btnFileBrowse.Enabled := true;
    EditFileName.SetFocus;
  end else begin
    EditFileName.Enabled := false;
    EditFileName.Color := clBtnFace;
    btnFileBrowse.Enabled := false;
  end;

  if radioOtherDatabase.Checked then begin
    comboOtherDatabase.Enabled := true;
    comboOtherDatabase.Color := clWindow;
    comboOtherDatabase.SetFocus;
  end else begin
    comboOtherDatabase.Enabled := false;
    comboOtherDatabase.Color := clBtnFace;
  end;

  if radioOtherHost.Checked then begin
    comboOtherHost.Enabled := true;
    comboOtherHost.Color := clWindow;
    comboOtherHostDatabase.Enabled := not cbxDatabase.Checked;
    comboOtherHostDatabase.Color := clWindow;
    comboOtherHost.SetFocus;
  end else begin
    comboOtherHost.Enabled := false;
    comboOtherHost.Color := clBtnFace;
    comboOtherHostDatabase.Enabled := false;
    comboOtherHostDatabase.Color := clBtnFace;
  end;

  // Disable target selection if exporting to known session.
  comboTargetCompat.Enabled := radioFile.Checked;
end;

procedure TExportSQLForm.validateControls(Sender: TObject);
begin
  cbxDatabase.Enabled := cbxStructure.Checked and ( radioFile.Checked or radioOtherHost.Checked );
  comboDatabase.Enabled := cbxStructure.Checked and ( radioFile.Checked or radioOtherHost.Checked ) and cbxDatabase.Checked;
  comboOtherHostDatabase.Enabled := not cbxDatabase.Checked;

  cbxTables.Enabled := cbxStructure.Checked;
  comboTables.Enabled := cbxStructure.Checked and cbxTables.Checked;

  comboData.Enabled := cbxData.Checked;
  cbxExtendedInsert.Enabled := cbxData.Checked;

  // Prevent choosing export of db struct + data but no table struct.
  if cbxData.Checked then begin
    if Sender = cbxTables then cbxDatabase.Checked := cbxDatabase.Checked And cbxTables.Checked
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

procedure TExportSQLForm.cbxExtendedInsertClick(Sender: TObject);
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

  // Select first host and first database.
  comboOtherHost.ItemIndex := 0;
  comboOtherHost.OnSelect(comboOtherHost);
  comboOtherHostDatabase.ItemIndex := 0;

  validateRadioControls(Sender);
  validateControls(Sender);
  generateExampleSql;
end;


procedure TExportSQLForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  OutputTo : Byte;
begin
  with TRegistry.Create do
  begin
    OpenKey(REGPATH, true);
    // WithUseDB, UseBackticks, CompleteInserts, WithDropTable: deprecated (currently not automagically removed)
    WriteBool('ExportStructure',      cbxStructure.Checked);
    WriteBool('WithCreateDatabase',   cbxDatabase.Checked);
    WriteBool('WithCreateTable',      cbxTables.Checked);
    WriteBool('ExportData',           cbxData.Checked);
    WriteInteger('CreateDatabaseHow', comboDatabase.ItemIndex);
    WriteInteger('CreateTablesHow',   comboTables.ItemIndex);
    WriteInteger('CreateDataHow',     comboData.ItemIndex);
    WriteBool('ExtendedInsert',       cbxExtendedInsert.Checked);
    WriteInteger('Compatibility',     comboTargetCompat.ItemIndex);
    WriteString('exportfilename',     EditFileName.Text);
    OutputTo := OUTPUT_FILE;
    if radioOtherDatabase.checked then
      OutputTo := OUTPUT_DB
    else if radioOtherHost.checked then
      OutputTo := OUTPUT_HOST;
    WriteInteger('ExportSQL_OutputTo',     OutputTo );
    WriteInteger('ExportSQL_WindowWidth',  Width );
    WriteInteger('ExportSQL_WindowHeight', Height );
    CloseKey();
  end;
end;

end.


