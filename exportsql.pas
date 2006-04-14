unit exportsql;


// -------------------------------------
// HeidiSQL
// Export Tables
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CheckLst, Buttons, comctrls, Registry, ToolWin;

type
  TExportSQLForm = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    TablesCheckListBox: TCheckListBox;
    Label1: TLabel;
    DBComboBox: TComboBox;
    GroupBox1: TGroupBox;
    SaveDialog1: TSaveDialog;
    BitBtn1: TBitBtn;
    EditFileName: TEdit;
    RadioButtonDB: TRadioButton;
    RadioButtonFile: TRadioButton;
    ComboBoxODB: TComboBox;
    ProgressBar1: TProgressBar;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RadioButtonHost: TRadioButton;
    ComboBoxHost: TComboBox;
    Label2: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    GroupBox3: TGroupBox;
    CheckBoxWithUseDB: TCheckBox;
    CheckBoxWithDropTable: TCheckBox;
    CheckBoxCompleteInserts: TCheckBox;
    CheckBoxUseBackticks: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBComboBoxChange(Sender: TObject);
    procedure CheckListToggle(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioButtonDBClick(Sender: TObject);
    procedure RadioButtonFileClick(Sender: TObject);
    procedure fillcombo_anotherdb(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioButtonHostClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
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

procedure TExportSQLForm.Button2Click(Sender: TObject);
begin
  close;
end;

procedure TExportSQLForm.FormShow(Sender: TObject);
var
  tn : TTreeNode;
  i,j : Integer;
begin
  ProgressBar1.Position := 0;
  Label2.Caption := '';
  // read dbs and Tables from treeview
  DBComboBox.Items.Clear;
  with TMDIChild(Mainform.ActiveMDIChild) do begin
    self.Caption := ZConn.HostName + ' - Export Tables...';
    for i:=0 to DBTree.Items.Count-1 do begin
      tn := DBTree.Items[i];
      if tn.Level = 1 then
        DBComboBox.Items.Add(tn.Text);
    end;

    for i:=0 to DBComboBox.Items.Count-1 do
      if DBComboBox.Items[i] = ActualDatabase then
        DBComboBox.ItemIndex := i;

    if DBComboBox.ItemIndex = -1 then
      DBComboBox.ItemIndex := 0;

  end;
  DBComboBoxChange(self);
  // save filename
  with TRegistry.Create do
    if OpenKey(regpath, true) then begin
      if Valueexists('exportfilename') then EditFileName.Text := ReadString('exportfilename');
      if Valueexists('CompleteInserts') then CheckBoxCompleteInserts.Checked := ReadBool('CompleteInserts');
      if Valueexists('WithDropTable') then CheckBoxWithDropTable.Checked := ReadBool('WithDropTable');
      if Valueexists('WithUseDB') then CheckBoxWithUseDB.Checked := ReadBool('WithUseDB');
      if Valueexists('ExportStructure') then CheckBox1.Checked := ReadBool('ExportStructure');
      if Valueexists('ExportData') then CheckBox2.Checked := ReadBool('ExportData');
      if Valueexists('UseBackticks') then CheckBoxUseBackticks.Checked := ReadBool('UseBackticks');
    end;
  CheckBox1Click(self);
  CheckBox2Click(self);
  if EditFileName.Text = '' then
    EditFileName.Text := ExtractFilePath(paramstr(0)) + 'export.sql';

  // Another Host / DB
  ComboBoxHost.Items.Clear;
  for i:=0 to MainForm.MDIChildCount-1 do begin
    if MainForm.MDIChildren[i] <> MainForm.ActiveMDIChild then
      with TMDIChild(MainForm.MDIChildren[i]) do begin
        for j:=0 to tnodehost.Count-1 do
          self.ComboBoxHost.Items.Add(ZConn.HostName + ':' + tnodehost.Item[j].text);
      end;
  end;
  if ComboBoxHost.Items.Count > 0 then
    ComboBoxHost.ItemIndex := 0;
  RadioButtonFile.OnClick(self);
end;



procedure TExportSQLForm.DBComboBoxChange(Sender: TObject);
var
  tn, child : TTreeNode;
  i,j : Integer;
begin
  // read tables from db
  TablesCheckListBox.Items.Clear;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    for i:=0 to DBTree.Items.Count-1 do
    begin
      tn := DBTree.Items[i];
      if tn.Text = DBComboBox.Text then
      begin
        child := tn.getFirstChild;
        for j:=0 to tn.Count-1 do
        begin
          TablesCheckListBox.Items.Add(child.Text);
          child := tn.getNextChild(child);
        end;
      end;
    end;
  end;
  // select all:
  for i:=0 to TablesCheckListBox.Items.Count-1 do
    TablesCheckListBox.checked[i] := true;

  // write items for "Another Databases":
  fillcombo_anotherdb(self);
end;


procedure TExportSQLForm.CheckListToggle(Sender: TObject);
begin
  // check all or none
  ToggleCheckListBox(TablesCheckListBox, ((Sender as TControl).Tag = 1));
end;



procedure TExportSQLForm.BitBtn1Click(Sender: TObject);
begin
  SaveDialog1.Filename := DBComboBox.Text;
  if SaveDialog1.Execute then
    if SaveDialog1.Filename <> '' then
      EditFileName.Text := SaveDialog1.Filename;
end;


procedure TExportSQLForm.Button1Click(Sender: TObject);

function mask (str: String): String;
begin
  if CheckBoxUseBackticks.Checked then
    result := '`'+str+'`'
  else
    result := str;
end;

var
  f                         : TFileStream;

  i,j,k,fieldcount,m        : Integer;
  exportdata,exportstruc    : boolean;
  dropquery,createquery,insertquery,
  feldnamen                 : String;
  keylist                   : Array of TMyKey;
  keystr,DB2export          : String;
  which                     : Integer;
  tofile                    : boolean;
  tcount,tablecounter       : Integer;
  HostDb                    : TStringList;
  win2export                : TMDIChild;
  StrProgress               : String;
  value                     : String;
  Escaped,fullvalue         : PChar;
begin
  // export!
  with TRegistry.Create do
  begin
    OpenKey(regpath, true);
    WriteString('exportfilename',   EditFileName.Text);
    WriteBool('CompleteInserts',    CheckBoxCompleteInserts.Checked);
    WriteBool('WithDropTable',      CheckBoxWithDropTable.Checked);
    WriteBool('WithUseDB',          CheckBoxWithUseDB.Checked);
    WriteBool('ExportStructure',    CheckBox1.Checked);
    WriteBool('ExportData',         CheckBox2.Checked);
    WriteBool('UseBackticks',       CheckBoxUseBackticks.Checked);
    CloseKey();
  end;

  Screen.Cursor := crHourGlass;

  // export what?
  exportstruc := CheckBox1.Checked;
  exportdata := CheckBox2.Checked;

  tofile := RadioButtonFile.Checked;
  if tofile then begin
    try
      f := TFileStream.Create(EditFileName.Text, fmCreate);
    except
      messagedlg('File "'+EditFileName.Text+'" could not be opened!' +  crlf + 'Maybe in use by another application?', mterror, [mbOK], 0);
      f.free;
      Screen.Cursor := crDefault;
      abort;
    end;
    wfs(f, '# ' + main.appname + ' Dump ' + appversion);
    wfs(f, '#');
  end
  else if RadioButtonDB.Checked then
    DB2export := ComboBoxODB.Text
  else if RadioButtonHost.Checked then begin
    HostDb := explode(':', ComboBoxHost.Items[ComboBoxHost.ItemIndex]);
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
      ExecQuery( 'USE ' + DBComBoBox.Text );
      if tofile then
      begin
        wfs(f, '# Host: ' + ZConn.HostName + '   Database: ' + DBComBoBox.Text);
        wfs(f, '# --------------------------------------------------------');
        wfs(f, '# Server version: ' + GetVar( 'SELECT VERSION()' ) + ' ' + GetVar( 'SHOW VARIABLES LIKE "version_compile_os"' ) );
        if CheckBoxWithUseDB.Checked then
        begin
          wfs(f);
          wfs(f, 'USE ' + DBComBoBox.Text + ';');
        end;
      end;

      // How many tables?
      tcount := 0;
      for i:=0 to TablesCheckListBox.Items.Count-1 do if TablesCheckListBox.checked[i] then
        inc(tcount);
      ProgressBar1.Max := 0;
      if exportstruc then
        ProgressBar1.Max := tcount;
      if exportdata then
        ProgressBar1.Max := ProgressBar1.Max + tcount;

      TablesCheckListBox.ItemIndex := -1;
      tablecounter := 0;

      for i:=0 to TablesCheckListBox.Items.Count-1 do if TablesCheckListBox.checked[i] then
      begin
        inc(tablecounter);
        if TablesCheckListBox.ItemIndex > -1 then
          TablesCheckListBox.Checked[TablesCheckListBox.ItemIndex] := false;
        TablesCheckListBox.ItemIndex := i;
        StrProgress := 'Table ' + inttostr(tablecounter) + '/' + inttostr(tcount) + ': ' + TablesCheckListBox.Items[i];
        Label2.caption := StrProgress;

        if exportstruc then begin
          if mysql_version < 32320 then begin
            GetResults( 'SHOW FIELDS FROM ' + mainform.mask(TablesCheckListBox.Items[i]), ZQuery3 );
            fieldcount := ZQuery3.FieldCount;
          end else begin
            GetResults('SHOW CREATE TABLE ' + mainform.mask(TablesCheckListBox.Items[i]), ZQuery3 );
          end;
          createquery := '';
          if tofile then begin
            createquery := '#' + crlf;
            createquery := createquery + '# Table structure for table ''' + TablesCheckListBox.Items[i] + '''' + crlf;
            createquery := createquery + '#' + crlf + crlf;
          end;

          if CheckBoxWithDropTable.checked and CheckBoxWithDropTable.Enabled then begin
            if tofile then
              createquery := createquery + 'DROP TABLE IF EXISTS ' + mask(TablesCheckListBox.Items[i]) + ';' + crlf
            else
              dropquery := createquery + 'DROP TABLE IF EXISTS ' + mask(DB2Export) + '.' + mask(TablesCheckListBox.Items[i]) + '' + crlf;
          end;

          if mysql_version < 32320 then begin
            if tofile then
              createquery := createquery + 'CREATE TABLE IF NOT EXISTS ' + mask(TablesCheckListBox.Items[i]) + ' (' + crlf
            else
              createquery := createquery + 'CREATE TABLE IF NOT EXISTS ' + mask(DB2Export) + '.' + mask(TablesCheckListBox.Items[i]) + ' (' + crlf;
          end else begin
            if CheckBoxuseBackticks.checked then
              createquery := createquery + ZQuery3.Fields[1].AsString
            else
              createquery := createquery + stringreplace(ZQuery3.Fields[1].AsString, '`', '', [rfReplaceAll]);
          end;

          if mysql_version < 32320 then begin
            for j := 1 to fieldcount do
            begin
              createquery := createquery + '  ' + mask(ZQuery3.Fields[0].AsString) + ' ' + ZQuery3.Fields[1].AsString;
              if ZQuery3.Fields[2].AsString <> 'YES' then
                createquery := createquery + ' NOT NULL';
              if ZQuery3.Fields[4].AsString <> '' then
                createquery := createquery + ' DEFAULT ''' + ZQuery3.Fields[4].AsString + '''';
              if ZQuery3.Fields[5].AsString <> '' then
                createquery := createquery + ' ' + ZQuery3.Fields[5].AsString;
              if j < fieldcount then
                createquery := createquery + ',' + crlf;
            end;

            // Keys:
            GetResults( 'SHOW KEYS FROM ' + TablesCheckListBox.Items[i], ZQuery3 );
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
                keystr := keystr + crlf + '  ' + keylist[k]._type + ' KEY ' + keylist[k].Name + ' (';
              keystr := keystr + implodestr(',', keylist[k].Columns) + ')';
            end;
            createquery := createquery + keystr + crlf + ')';
          end; // mysql_version < 32320

          feldnamen := '';
          if CheckBoxCompleteInserts.Checked then
          begin
            feldnamen := ' (';
            GetResults( 'SHOW FIELDS FROM ' + mainform.mask(TablesCheckListBox.Items[i]), ZQuery3 );
            for k:=1 to ZQuery3.RecordCount do
            begin
              if k>1 then
                feldnamen := feldnamen + ', ';
              feldnamen := feldnamen + mask(ZQuery3.Fields[0].AsString);
              ZQuery3.Next;
            end;
            feldnamen := feldnamen+')';
          end;

          // File:
          if tofile then begin
            createquery := createquery + ';' + crlf;
            wfs(f);
            wfs(f);
            wfs(f, createquery);
          end

          // Another Database
          else if RadioButtonDB.Checked then begin
            if mysql_version >= 32320 then
              ExecQuery( 'USE ' + DB2Export );
            if CheckBoxWithDropTable.checked and CheckBoxWithDropTable.Enabled then
              ExecQuery( dropquery );
            ExecQuery( createquery );
            if mysql_version >= 32320 then
              ExecQuery( 'USE ' + DBComboBox.Text );
          end

          // Another Host / DB:
          else if RadioButtonHost.Checked then begin
            if mysql_version >= 32320 then
              win2export.ExecQuery('USE ' + DB2Export);
            if CheckBoxWithDropTable.checked and CheckBoxWithDropTable.Enabled then
              win2export.ExecQuery(dropquery);
            win2export.ExecQuery(createquery);
          end;

          ProgressBar1.StepIt;
        end;

        // export data:
        if exportdata then
        begin
          GetResults( 'SELECT * FROM ' + mainform.mask(TablesCheckListBox.Items[i]), ZQuery3 );
          if tofile then
          begin
            wfs(f);
            wfs(f);
            wfs(f, '#');
            wfs(f, '# Dumping data for table ''' + TablesCheckListBox.Items[i] + '''');
            wfs(f, '#');
            wfs(f);
          end;
          for j:=1 to ZQuery3.RecordCount do
          begin
            Label2.caption := StrProgress + ' (Record ' + inttostr(j) + ')';
            Application.ProcessMessages;
            if tofile then
              insertquery := 'INSERT INTO ' + mask(TablesCheckListBox.Items[i])
            else
              insertquery := 'INSERT INTO ' + mask(DB2Export) + '.' + mask(TablesCheckListBox.Items[i]);
            insertquery := insertquery + feldnamen;
            insertquery := insertquery + ' VALUES(';
            for k := 0 to ZQuery3.fieldcount-1 do
            begin
              if ZQuery3.Fields[k].AsString <> '' then begin
                QuotedStr(ZQuery3.Fields[k].AsString);
              end
              else
                value := 'NULL';
              insertquery := insertquery + value;
              if k < ZQuery3.Fieldcount-1 then
                insertquery := insertquery + ', ';
            end;
            insertquery := insertquery + ')';
            if tofile then
              wfs(f, insertquery + ';')
            else if RadioButtonDB.Checked then
              ExecQuery(insertquery)
            else if RadioButtonHost.Checked then
              win2export.ExecQuery(insertquery);
            ZQuery3.Next;
          end;
          ProgressBar1.StepIt;
        end;
      end;
      ExecQuery( 'USE ' + ActualDatabase );
    end;
  FINALLY
    if tofile then f.Free;
    Screen.Cursor := crDefault;
  END;

  close;
end;

procedure TExportSQLForm.RadioButtonDBClick(Sender: TObject);
begin
  if DBComboBox.Items.Count <= 1 then begin
    MessageDLG('There must be more than one database to enable this option.', mtError, [mbOK], 0);
    RadioButtonFile.OnClick(self);
    abort;
  end;
  RadioButtonFile.Checked := false;
  RadioButtonHost.Checked := false;
  RadioButtonDB.Checked := true;
  EditFileName.Enabled := false;
  EditFileName.Color := clBtnFace;
  BitBtn1.Enabled := false;
  ComboBoxODB.Enabled := true;
  ComboBoxODB.Color := clWindow;
  ComboBoxHost.Enabled := false;
  ComboBoxHost.Color := clBtnFace;
  CheckBoxWithUseDB.Enabled := false;
  ComboBoxODB.SetFocus;
end;

procedure TExportSQLForm.RadioButtonFileClick(Sender: TObject);
begin
  RadioButtonFile.Checked := true;
  RadioButtonDB.Checked := false;
  RadioButtonHost.Checked := false;
  EditFileName.Enabled := true;
  EditFileName.Color := clWindow;
  BitBtn1.Enabled := true;
  ComboBoxODB.Enabled := false;
  ComboBoxODB.Color := clBtnFace;
  ComboBoxHost.Enabled := false;
  ComboBoxHost.Color := clBtnFace;
  CheckBoxWithUseDB.Enabled := true;
  EditFileName.SetFocus;
end;


procedure TExportSQLForm.fillcombo_anotherdb(Sender: TObject);
begin
  ComboBoxODB.Items := DBComboBox.Items;
  ComboBoxODB.Items.delete(DBComboBox.ItemIndex);
  if ComboBoxODB.ItemIndex = -1 then
    ComboBoxODB.ItemIndex := 0;
end;

procedure TExportSQLForm.CheckBox1Click(Sender: TObject);
begin
  CheckBoxWithDropTable.Enabled := CheckBox1.checked;
  if CheckBox1.Checked or CheckBox2.Checked then
    Button1.Enabled := true
  else
    Button1.Enabled := false;
end;

procedure TExportSQLForm.RadioButtonHostClick(Sender: TObject);
begin
  if ComboBoxHost.Items.Count = 0 then begin
    MessageDLG('You need at least two open connection-windows to enable this option.', mtError, [mbOK], 0);
    RadioButtonFile.OnClick(self);
    abort;
  end;
  RadioButtonFile.Checked := false;
  RadioButtonDB.Checked := false;
  RadioButtonHost.Checked := true;
  EditFileName.Enabled := false;
  EditFileName.Color := clBtnFace;
  BitBtn1.Enabled := false;
  ComboBoxODB.Enabled := false;
  ComboBoxODB.Color := clBtnFace;
  ComboBoxHost.Enabled := true;
  ComboBoxHost.Color := clWindow;
  CheckBoxWithUseDB.Enabled := false;
  ComboBoxHost.SetFocus;
end;


procedure TExportSQLForm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox1.Checked or CheckBox2.Checked then
    Button1.Enabled := true
  else
    Button1.Enabled := false;
end;



end.
