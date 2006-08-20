unit exportsql;


// -------------------------------------
// HeidiSQL
// Export Tables
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, Buttons, comctrls, Registry, ToolWin, DB,
  SynEdit, SynMemo;

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
    procedure validateControls(Sender: TObject);
    procedure cbxStructureClick(Sender: TObject);
    procedure radioOtherHostClick(Sender: TObject);
    procedure cbxDataClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExportSQLForm: TExportSQLForm;

const
	CRLF = #13#10;

implementation

uses Main, Childwin, helpers;

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


procedure TExportSQLForm.btnCancelClick(Sender: TObject);
begin
  close;
end;

procedure TExportSQLForm.FormShow(Sender: TObject);
var
  tn : TTreeNode;
  i,j, OutputTo, idxHost : Integer;
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
    self.Caption := ZConn.HostName + ' - Export Tables...';
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

  // Another Host / DB
  comboOtherHost.Items.Clear;
  idxHost := -1;
  for i:=0 to MainForm.MDIChildCount-1 do
  begin
    if MainForm.MDIChildren[i] <> MainForm.ActiveMDIChild then
      with TMDIChild(MainForm.MDIChildren[i]) do
      begin
        for j:=0 to tnodehost.Count-1 do
        begin
          self.comboOtherHost.Items.Add(ZConn.HostName + ':' + tnodehost.Item[j].text);
          if (ActualDatabase = tnodehost.Item[j].text) and (idxHost=-1) then
            idxHost := self.comboOtherHost.Items.Count-1;
        end;
      end;
  end;

  // select the ActualDatabase from the first window
  // or - if no ActualDatabase set - set at least the first item in the combo
  if comboOtherHost.Items.Count > 0 then
  begin
    if idxHost = -1 then
      comboOtherHost.ItemIndex := 0
    else
      comboOtherHost.ItemIndex := idxHost;
  end;

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
      if (comboOtherHost.Items.Count = 0) and (OutputTo = OUTPUT_HOST) then
        OutputTo := OUTPUT_FILE;
      case OutputTo of
        OUTPUT_FILE : radioFile.checked := true;
        OUTPUT_DB   : radioOtherDatabase.checked := true;
        OUTPUT_HOST : radioOtherHost.checked := true;
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
  HostDb                    : TStringList;
  win2export                : TMDIChild;
  StrProgress               : String;
  value                     : String;
  Escaped,fullvalue         : PChar;
  max_allowed_packet        : Integer;
  thesevalues               : String;
  valuescount, recordcount  : Integer;
  donext                    : Boolean;
  PBuffer                   : PChar;
  sql                       : String;
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
  if todb then DB2export := comboOtherDatabase.Text;

  if tohost then begin
    HostDb := explode(':', comboOtherHost.Items[comboOtherHost.ItemIndex]);
    DB2export := HostDb[1];
    for m:=0 to mainform.MDIChildCount-1 do begin
      if (TMDIChild(mainform.MDIChildren[m]) <> TMDIChild(mainform.ActiveMDIChild)) and
        (TMDIChild(mainform.MDIChildren[m]).ZConn.HostName = HostDb[0]) then
        win2export := TMDIChild(mainform.MDIChildren[m]);
    end;
  end;

  TRY
    with TMDIChild(Mainform.ActiveMDIChild) do
    begin
      ExecUseQuery( comboSelectDatabase.Text );
      if cbxExtendedInsert.Checked then
      begin
        max_allowed_packet := StrToIntDef( GetVar( 'SHOW VARIABLES LIKE ''max_allowed_packet''', 1 ), 1024*1024 );
      end;
      if tofile then
      begin
        wfs(f, '# --------------------------------------------------------');
        wfs(f, '# Host:                 ' + ZConn.HostName );
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
        sql := '/*!40100 SET CHARACTER SET ' + GetVar( 'SHOW VARIABLES LIKE "character_set_connection"', 1 ) + ';*/';
        wfs(f, sql);
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
          if mysql_version < 32320 then begin
            GetResults( 'SHOW FIELDS FROM ' + mainform.mask(checkListTables.Items[i]), ZQuery3 );
            fieldcount := ZQuery3.FieldCount;
          end else begin
            GetResults('SHOW CREATE TABLE ' + mainform.mask(checkListTables.Items[i]), ZQuery3 );
          end;
          createquery := '';
          dropquery := '';
          if tofile then begin
            createquery := '#' + crlf;
            createquery := createquery + '# Table structure for table ''' + checkListTables.Items[i] + '''' + crlf;
            createquery := createquery + '#' + crlf + crlf;
          end;

          if comboTables.ItemIndex = TAB_DROP_CREATE then begin
            if tofile then
              createquery := createquery + 'DROP TABLE IF EXISTS ' + mask(checkListTables.Items[i]) + ';' + crlf
            else
              dropquery := createquery + 'DROP TABLE IF EXISTS ' + mask(DB2Export) + '.' + mask(checkListTables.Items[i]) + '' + crlf;
          end;

          if mysql_version >= 32320 then
          begin
            sql := ZQuery3.Fields[1].AsString;
            sql := fixNewlines(sql) + ';';
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
            sql := sql + keystr + crlf + ');';
          end; // mysql_version < 32320

          if comboTables.ItemIndex = TAB_CREATE_IGNORE then
          begin
            Insert('/*!32312 IF NOT EXISTS*/ ', sql, Pos('TABLE', sql) + 6);
          end;

          createquery := createquery + sql;

          // Output CREATE TABLE to file
          if tofile then begin
            createquery := createquery + crlf;
            wfs(f);
            wfs(f);
            wfs(f, createquery);
          end

          // Run CREATE TABLE on another Database
          else if todb then begin
            if mysql_version >= 32320 then
            begin
              ExecUseQuery( DB2Export );
            end;
            if comboTables.ItemIndex = TAB_DROP_CREATE then
              ExecQuery( dropquery );
            ExecQuery( createquery );
            if mysql_version >= 32320 then
            begin
              ExecUseQuery( comboSelectDatabase.Text );
            end;
          end

          // Run CREATE TABLE on another host
          else if tohost then begin
            if mysql_version >= 32320 then
            begin
              win2export.ExecUseQuery( DB2Export );
            end;
            if comboTables.ItemIndex = TAB_DROP_CREATE then
              win2export.ExecQuery(dropquery);
            win2export.ExecQuery(createquery);
          end;

          barProgress.StepIt;
        end;

        // export data
        if exportdata then
        begin
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
            else if tohost then with win2export do
            begin
              ExecQuery('TRUNCATE TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]);
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
              if mysql_version > 40000 then
                ExecQuery( 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' DISABLE KEYS' );
              ExecQuery( 'LOCK TABLES ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' WRITE' );
            end
            else if tohost then with win2export do
            begin
              if mysql_version > 40000 then
                ExecQuery( 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' DISABLE KEYS' );
              ExecQuery( 'LOCK TABLES ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' WRITE' );
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
              Application.ProcessMessages;
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
                ftInteger, ftSmallint, ftWord, ftFloat:
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
              win2export.ExecQuery(insertquery);
            if donext then
              ZQuery3.Next;
            donext := true;
            insertquery := '';
          end;
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
              if mysql_version > 40000 then
                ExecQuery( 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' ENABLE KEYS' );
            end
            else if tohost then with win2export do
            begin
              ExecQuery( 'UNLOCK TABLES' );
              if mysql_version > 40000 then
                ExecQuery( 'ALTER TABLE ' + mask(DB2Export) + '.' + checkListTables.Items[i]+' ENABLE KEYS' );
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
  radioFile.Checked := false;
  radioOtherHost.Checked := false;
  radioOtherDatabase.Checked := true;
  EditFileName.Enabled := false;
  EditFileName.Color := clBtnFace;
  btnFileBrowse.Enabled := false;
  comboOtherDatabase.Enabled := true;
  comboOtherDatabase.Color := clWindow;
  comboOtherHost.Enabled := false;
  comboOtherHost.Color := clBtnFace;
  comboOtherDatabase.SetFocus;
end;

procedure TExportSQLForm.radioFileClick(Sender: TObject);
begin
  radioFile.Checked := true;
  radioOtherDatabase.Checked := false;
  radioOtherHost.Checked := false;
  EditFileName.Enabled := true;
  EditFileName.Color := clWindow;
  btnFileBrowse.Enabled := true;
  comboOtherDatabase.Enabled := false;
  comboOtherDatabase.Color := clBtnFace;
  comboOtherHost.Enabled := false;
  comboOtherHost.Color := clBtnFace;
  editFileName.SetFocus;
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
  if cbxStructure.Checked then begin
    if cbxDatabase.Checked then begin
      case comboDatabase.ItemIndex of
        DB_DROP_CREATE:        add(STR_DROP_DB, STR_CREATE_DB);
        DB_CREATE:             add(STR_CREATE_DB);
        DB_CREATE_IGNORE:      add(STR_CREATE_DB_IGNORE);
      end;
      add(#13#10);
    end;
    if cbxTables.Checked then begin
      case comboTables.ItemIndex of
        TAB_DROP_CREATE:       add(STR_DROP_TABLE, STR_CREATE_TABLE);
        TAB_CREATE:            add(STR_CREATE_TABLE);
        TAB_CREATE_IGNORE:     add(STR_CREATE_TABLE_IGNORE);
      end;
      add(#13#10);
    end;
  end;
  if cbxData.Checked then begin
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

procedure TExportSQLForm.validateControls(Sender: TObject);
begin
  cbxDatabase.Enabled := cbxStructure.Checked;
  comboDatabase.Enabled := cbxStructure.Checked and cbxDatabase.Checked;

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

procedure TExportSQLForm.cbxDatabaseClick(Sender: TObject);
begin
  validateControls(Sender);
  generateExampleSQL;
end;


procedure TExportSQLForm.radioOtherHostClick(Sender: TObject);
begin
  if comboOtherHost.Items.Count = 0 then begin
    MessageDLG('You need at least two open connection-windows to enable this option.', mtError, [mbOK], 0);
    radioFile.OnClick(self);
    abort;
  end;
  radioFile.Checked := false;
  radioOtherDatabase.Checked := false;
  radioOtherHost.Checked := true;
  EditFileName.Enabled := false;
  EditFileName.Color := clBtnFace;
  btnFileBrowse.Enabled := false;
  comboOtherDatabase.Enabled := false;
  comboOtherDatabase.Color := clBtnFace;
  comboOtherHost.Enabled := true;
  comboOtherHost.Color := clWindow;
  comboOtherHost.SetFocus;
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
    if radioFile.Checked then
      OutputTo := OUTPUT_FILE
    else if radioOtherDatabase.checked then
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

