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
  SynMemo;

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
    procedure GetRemoteDatabasesCompleted(res: TNotifyStructure);
    { Private declarations }
  public
    { Public declarations }
  end;

  function ExportTablesWindow (AOwner : TComponent; Flags : String = '') : Boolean;


const
	CRLF = #13#10;

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
    self.Caption := ZQuery3.Connection.HostName + ' - Export Tables...';
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
    if OpenKey(regpath, true) then begin
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

procedure TExportSQLForm.GetRemoteDatabasesCompleted(res: TNotifyStructure);
var
  j: integer;
  list: TStringList;
  error: Exception;
begin
  error := res.GetException;
  list := TStringList(res.GetResult);
  // Hide the cancel dialog if it's still showing.
  if cancelDialog.Visible then cancelDialog.Close;
  if list <> nil then begin
    // Fetching list was successful.
    comboOtherHostDatabase.Clear;
    for j:=0 to list.Count - 1 do begin
      comboOtherHostDatabase.Items.Add(list[j]);
    end;
    list.Free;
  end else begin
    // Error occurred while fetching remote list,
    // just switch back to the 'output to file' choice.
    radioFile.Checked := true;
    error.Free;
  end;
  res.Free;
end;

procedure TExportSQLForm.comboOtherHostSelect(Sender: TObject);
var
  requestId: Cardinal;
begin
  cancelDialog := CreateMessageDialog('Fetching remote list of databases...', mtCustom, [mbCancel]);
  requestId := RemoteGetDatabases(Self.GetRemoteDatabasesCompleted, INFINITE_TIMEOUT, appHandles[comboOtherHost.ItemIndex]);
  // The callback method shouldn't be activated before messages has been processed,
  // so we can safely touch the cancelDialog here.
  cancelDialog.ShowModal;
  // We just cancel in any case.
  // If the query was completed before the cancel dialog closed,
  // the notification code won't accept the cancel, so it's OK. 
  NotifyInterrupted(requestId, Exception.Create('User cancelled.'));
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
  i,j,k,fieldcount,m        : Integer;
  exportdb,exporttables     : boolean;
  exportdata                : boolean;
  dropquery,createquery,insertquery,
  feldnamen                 : String;
  keylist                   : Array of TMyKey;
  keystr,DB2export          : String;
  which                     : Integer;
  tofile,todb,tohost        : boolean;
  tcount,tablecounter       : Integer;
  win2export                : THandle;
  StrProgress               : String;
  value                     : String;
  Escaped,fullvalue         : PChar;
  max_allowed_packet        : Integer;
  thesevalues               : String;
  valuescount, recordcount  : Integer;
  donext                    : Boolean;
  PBuffer                   : PChar;
  sql, current_characterset : String;
  target_version            : Integer;
begin
  // export!
  pageControl1.ActivePageIndex := 0;
  Screen.Cursor := crHourGlass;

  // export what?
  exportdb      := cbxDatabase.Checked;
  exporttables  := cbxTables.Checked;
  exportdata    := cbxData.Checked;

  // to where?
  tofile := radioFile.Checked;
  todb := radioOtherDatabase.Checked;
  tohost := radioOtherHost.Checked;

  // open output file if needed.
  if tofile then begin
    case comboTargetCompat.ItemIndex of
      0: target_version := 32300;
      1: target_version := 51000;
    end;
    try
      f := TFileStream.Create(EditFileName.Text, fmCreate);
    except
      messagedlg('File "'+EditFileName.Text+'" could not be opened!' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
      f.free;
      Screen.Cursor := crDefault;
      abort;
    end;
    wfs(f, '# ' + main.appname + ' Dump ');
    wfs(f, '#');
  end;

  // which db is destination?
  if todb then begin
    target_version := TMDIChild(Mainform.ActiveMDIChild).mysql_version;
    DB2export := comboOtherDatabase.Text;
  end;

  if tohost then begin
    // TODO: Disable the combo and fetch remote version.
    case comboTargetCompat.ItemIndex of
      0: target_version := 32300;
      1: target_version := 51000;
    end;
    win2export := appHandles[comboOtherHost.ItemIndex];
    DB2export := comboOtherHostDatabase.Items[comboOtherHostDatabase.ItemIndex];
  end;

  TRY
    with TMDIChild(Mainform.ActiveMDIChild) do
    begin
      ExecUseQuery( comboSelectDatabase.Text );
      max_allowed_packet := 1024*1024;
      if cbxExtendedInsert.Checked then
      begin
        max_allowed_packet := StrToIntDef( GetVar( 'SHOW VARIABLES LIKE ''max_allowed_packet''', 1 ), 1024*1024 );
      end;
      if tofile then
      begin
        wfs(f, '# --------------------------------------------------------');
        wfs(f, '# Host:                 ' + ZQuery3.Connection.HostName );
        wfs(f, '# Database:             ' + comboSelectDatabase.Text );
        wfs(f, '# Server version:       ' + GetVar( 'SELECT VERSION()' ) );
        wfs(f, '# Server OS:            ' + GetVar( 'SHOW VARIABLES LIKE "version_compile_os"', 1 ) );
        if cbxExtendedInsert.Checked then
        begin
          wfs(f, '# max_allowed_packet:   ' + inttostr(max_allowed_packet) );
        end;
        wfs(f, '# ' + appname + ' version:     ' + appversion );
        wfs(f, '# --------------------------------------------------------');
        wfs(f);
        current_characterset := GetVar( 'SHOW VARIABLES LIKE "character_set_connection"', 1 );
        if current_characterset <> '' then
        begin
          sql := '/*!40100 SET CHARACTER SET ' + current_characterset + ';*/';
          wfs(f, sql);
        end;
        if cbxDatabase.Checked then
        begin
          wfs(f);
          wfs(f);
          wfs(f, '#');
          wfs(f, '# Database structure for database ''' + comboSelectDatabase.Text + '''');
          wfs(f, '#');
          wfs(f);
          if comboDatabase.ItemIndex = DB_DROP_CREATE then
          begin
            sql := 'DROP DATABASE IF EXISTS ' + mainform.mask(comboSelectDatabase.Text) + ';';
            wfs(f, sql);
          end;
          if mysql_version < 50002 then
          begin
            sql := 'CREATE DATABASE ';
            if comboDatabase.ItemIndex = DB_CREATE_IGNORE then
            begin
              sql := sql + '/*!32312 IF NOT EXISTS*/ ';
            end;
            sql := sql + mainform.mask(comboSelectDatabase.Text) + ';';
          end
          else
          begin
            sql := GetVar( 'SHOW CREATE DATABASE ' + mainform.mask(comboSelectDatabase.Text), 1 );
            sql := fixNewlines(sql) + ';';
            if comboDatabase.ItemIndex = DB_CREATE_IGNORE then
            begin
              Insert('/*!32312 IF NOT EXISTS*/ ', sql, Pos('DATABASE', sql) + 9);
            end;
          end;
          wfs(f, sql );
          if exporttables then
          begin
            wfs(f);
            sql := 'USE ' + mainform.mask(comboSelectDatabase.Text) + ';';
            wfs(f, sql );
          end;
        end;
      end;

      // How many tables?
      tcount := 0;
      for i:=0 to checkListTables.Items.Count-1 do if checkListTables.checked[i] then inc(tcount);
      barProgress.Max := 0;
      if exporttables then barProgress.Max := tcount;
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
              dropquery := 'DROP TABLE IF EXISTS ' + mask(checkListTables.Items[i])
            else
              dropquery := 'DROP TABLE IF EXISTS ' + mask(DB2Export) + '.' + mask(checkListTables.Items[i]);
          end;

          createquery := '';
          if tofile then begin
            createquery := '#' + crlf;
            createquery := createquery + '# Table structure for table ''' + checkListTables.Items[i] + '''' + crlf;
            createquery := createquery + '#' + crlf + crlf;
          end;
          
          if mysql_version < 32320 then begin
            GetResults( 'SHOW COLUMNS FROM ' + mainform.mask(checkListTables.Items[i]), ZQuery3 );
            fieldcount := ZQuery3.FieldCount;
          end else begin
            GetResults('SHOW CREATE TABLE ' + mainform.mask(checkListTables.Items[i]), ZQuery3 );
            fieldcount := -1;
          end;
          if mysql_version >= 32320 then
          begin
            sql := ZQuery3.Fields[1].AsString;
            sql := fixNewlines(sql);
            if comboTargetCompat.ItemIndex = 1 then begin
              sql := stringreplace(sql, 'TYPE=', 'ENGINE=', [rfReplaceAll]);
            end;
          end;
          if mysql_version < 32320 then begin
            if tofile then
              sql := 'CREATE TABLE IF NOT EXISTS ' + mask(checkListTables.Items[i]) + ' (' + crlf
            else
              sql := sql + 'CREATE TABLE IF NOT EXISTS ' + mask(DB2Export) + '.' + mask(checkListTables.Items[i]) + ' (' + crlf;
            for j := 1 to fieldcount do
            begin
              sql := sql + '  ' + mask(ZQuery3.Fields[0].AsString) + ' ' + ZQuery3.Fields[1].AsString;
              if ZQuery3.Fields[2].AsString <> 'YES' then
                sql := sql + ' NOT NULL';
              if ZQuery3.Fields[4].AsString <> '' then
                sql := sql + ' DEFAULT ''' + ZQuery3.Fields[4].AsString + '''';
              if ZQuery3.Fields[5].AsString <> '' then
                sql := sql + ' ' + ZQuery3.Fields[5].AsString;
              if j < fieldcount then
                sql := sql + ',' + crlf;
            end;

            // Keys:
            GetResults( 'SHOW KEYS FROM ' + mask(checkListTables.Items[i]), ZQuery3 );
            setLength(keylist, 0);
            keystr := '';
            if ZQuery3.RecordCount > 0 then
              keystr := ',';

            for j := 1 to ZQuery3.RecordCount do
            begin
              which := -1;

              for k:=0 to length(keylist)-1 do
              begin
                if keylist[k].Name = ZQuery3.Fields[2].AsString then // keyname exists!
                  which := k;
              end;
              if which = -1 then
              begin
                setlength(keylist, length(keylist)+1);
                which := high(keylist);
                keylist[which].Columns := TStringList.Create;
                with keylist[which] do // set properties for new key
                begin
                  Name := ZQuery3.Fields[2].AsString;
                  if ZQuery3.Fields[2].AsString = 'PRIMARY' then
                    _type := 'PRIMARY'
                  else if ZQuery3.FieldCount >= 10 then if ZQuery3.Fields[9].AsString = 'FULLTEXT' then
                    _type := 'FULLTEXT'
                  else if ZQuery3.Fields[1].AsString = '1' then
                    _type := ''
                  else if ZQuery3.Fields[1].AsString = '0' then
                    _type := 'UNIQUE';
                end;
              end;
              keylist[which].Columns.add(mask(ZQuery3.Fields[4].AsString)); // add column(s)
              ZQuery3.Next;
            end;
            for k:=0 to high(keylist) do
            begin
              if k > 0 then
                keystr := keystr + ',';
              if keylist[k].Name = 'PRIMARY' then
                keystr := keystr + crlf + '  PRIMARY KEY ('
              else
                keystr := keystr + crlf + '  ' + keylist[k]._type + ' KEY ' + mask(keylist[k].Name) + ' (';
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
            ExecUseQuery( DB2Export );
            if comboTables.ItemIndex = TAB_DROP_CREATE then
              ExecQuery( dropquery );
            ExecQuery( createquery );
            ExecUseQuery( comboSelectDatabase.Text );
          end

          // Run CREATE TABLE on another host
          else if tohost then begin
            RemoteExecUseQuery(win2export, mysql_version, DB2Export);
            if comboTables.ItemIndex = TAB_DROP_CREATE then
              RemoteExecQuery(win2export, dropquery);
            RemoteExecQuery(win2export, createquery);
          end;

          barProgress.StepIt;
        end;

        // export data
        if exportdata then
        begin
          // Set to mysql-readable char:
          DecimalSeparator := '.';
          feldnamen := ' (';
          GetResults( 'SHOW FIELDS FROM ' + mainform.mask(checkListTables.Items[i]), ZQuery3 );
          for k:=1 to ZQuery3.RecordCount do
          begin
            if k>1 then
              feldnamen := feldnamen + ', ';
            feldnamen := feldnamen + mask(ZQuery3.Fields[0].AsString);
            ZQuery3.Next;
          end;
          feldnamen := feldnamen+')';

          GetResults( 'SELECT * FROM ' + mainform.mask(checkListTables.Items[i]), ZQuery3 );
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
              wfs(f, 'TRUNCATE TABLE ' + mask(checkListTables.Items[i]) + ';');
            end
            else if todb then
            begin
              ExecQuery('TRUNCATE TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]);
            end
            else if tohost then
            begin
              RemoteExecQuery(win2export, 'TRUNCATE TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]);
            end;
          end;

          if ZQuery3.RecordCount > 0 then
          begin
            if tofile then
            begin
              wfs(f, '/*!40000 ALTER TABLE '+ mask(checkListTables.Items[i]) +' DISABLE KEYS;*/' );
              wfs(f, 'LOCK TABLES '+ mask(checkListTables.Items[i]) +' WRITE;' );
            end
            else if todb then
            begin
              if target_version > 40000 then
                ExecQuery( 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' DISABLE KEYS' );
              ExecQuery( 'LOCK TABLES ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' WRITE' );
            end
            else if tohost then
            begin
              if target_version > 40000 then
                RemoteExecQuery(win2export, 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' DISABLE KEYS');
              RemoteExecQuery(win2export, 'LOCK TABLES ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' WRITE');
            end;
          end;

          insertquery := '';
          valuescount := 0;
          j := 0;
          donext := true;
          recordcount := ZQuery3.RecordCount;
          while not ZQuery3.Eof do
          begin
            inc(j);
            lblProgress.caption := StrProgress + ' (Record ' + inttostr(j) + ')';
            if ZQuery3.RecNo mod 100 = 0 then
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
                insertquery := insertquery + mask(checkListTables.Items[i])
              else
                insertquery := insertquery + mask(DB2Export) + '.' + mask(checkListTables.Items[i]);
              insertquery := insertquery + feldnamen;
              insertquery := insertquery + ' VALUES ';
            end;
            thesevalues := '(';

            for k := 0 to ZQuery3.fieldcount-1 do
            begin
              if ZQuery3.Fields[k].IsNull then
                value := 'NULL'
              else
              case ZQuery3.Fields[k].DataType of
                ftInteger, ftSmallint, ftWord:
                  value := ZQuery3.Fields[k].AsString;
                else
                  value := escapeAuto( ZQuery3.Fields[k].AsString );
              end;
              thesevalues := thesevalues + value;
              if k < ZQuery3.Fieldcount-1 then
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
              else if j = RecordCount then
              begin
                insertquery := insertquery + thesevalues;
              end
              else
              begin
                inc(valuescount);
                insertquery := insertquery + thesevalues + ',' + crlf + #9;
                ZQuery3.Next;
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
              ExecQuery(insertquery)
            else if tohost then
              RemoteExecQuery(win2export, insertquery);
            if donext then
              ZQuery3.Next;
            donext := true;
            insertquery := '';
          end;
          // Set back to local setting:
          setLocales;

          if ZQuery3.RecordCount > 0 then
          begin
            if tofile then
            begin
              wfs(f, 'UNLOCK TABLES;' );
              wfs(f, '/*!40000 ALTER TABLE '+mask(checkListTables.Items[i])+' ENABLE KEYS;*/' );
            end
            else if todb then
            begin
              ExecQuery( 'UNLOCK TABLES' );
              if target_version > 40000 then
                ExecQuery( 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' ENABLE KEYS' );
            end
            else if tohost then
            begin
              RemoteExecQuery(win2export, 'UNLOCK TABLES');
              if target_version > 40000 then
                RemoteExecQuery(win2export, 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' ENABLE KEYS');
            end;
          end;
          ZQuery3.Close;
          barProgress.StepIt;
        end;
      end;
      if ActualDatabase <> '' then
      begin
        ExecUseQuery( ActualDatabase );
      end;
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
  STR_DROP_DB              = 'DROP DATABASE <db>;'#13#10;
  STR_CREATE_DB            = 'CREATE DATABASE <db>;'#13#10;
  STR_CREATE_DB_IGNORE     = 'CREATE DATABASE IF NOT EXISTS <db>;'#13#10;
  STR_DROP_TABLE           = 'DROP TABLE <table>;'#13#10;
  STR_CREATE_TABLE         = 'CREATE TABLE <table> <definition>;'#13#10;
  STR_CREATE_TABLE_IGNORE  = 'CREATE TABLE IF NOT EXISTS <table> <definition>;'#13#10;
  STR_TRUNCATE_TABLE       = 'TRUNCATE TABLE <table>;'#13#10;
  STR_INSERT               = 'INSERT INTO <table> (<columns>) <values>';
  STR_INSERT_IGNORE        = 'INSERT IGNORE INTO <table> (<columns>) <values>';
  STR_REPLACE_INTO         = 'REPLACE INTO <table> (<columns>) <values>';
  STR_END_INSERT_REG       = ';'#13#10'(...)'#13#10;
  STR_END_INSERT_EXT       = ', <values...>;'#13#10;
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
      add(#13#10);
    end;
    if cbxTables.Enabled and cbxTables.Checked then begin
      case comboTables.ItemIndex of
        TAB_DROP_CREATE:       add(STR_DROP_TABLE, STR_CREATE_TABLE);
        TAB_CREATE:            add(STR_CREATE_TABLE);
        TAB_CREATE_IGNORE:     add(STR_CREATE_TABLE_IGNORE);
      end;
      add(#13#10);
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
    comboOtherHostDatabase.Enabled := true;
    comboOtherHostDatabase.Color := clWindow;
    comboOtherHost.SetFocus;
  end else begin
    comboOtherHost.Enabled := false;
    comboOtherHost.Color := clBtnFace;
    comboOtherHostDatabase.Enabled := false;
    comboOtherHostDatabase.Color := clBtnFace;
  end;
end;

procedure TExportSQLForm.validateControls(Sender: TObject);
begin
  cbxDatabase.Enabled := cbxStructure.Checked and radioFile.Checked;
  comboDatabase.Enabled := cbxStructure.Checked and radioFile.Checked and cbxDatabase.Checked;

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
    radioFile.OnClick(radioFile);
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
    OpenKey(regpath, true);
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

