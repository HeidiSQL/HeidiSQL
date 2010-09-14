unit loaddata;


// -------------------------------------
// Load Textfile into table
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, CheckLst,
  SynRegExpr, Buttons, ExtCtrls, ToolWin, ExtDlgs, Math,
  mysql_connection, mysql_structures;

type
  Tloaddataform = class(TForm)
    btnImport: TButton;
    btnCancel: TButton;
    lblDatabase: TLabel;
    comboDatabase: TComboBox;
    lblTable: TLabel;
    comboTable: TComboBox;
    lblColumns: TLabel;
    chklistColumns: TCheckListBox;
    ToolBarColMove: TToolBar;
    btnColUp: TToolButton;
    btnColDown: TToolButton;
    grpFilename: TGroupBox;
    editFilename: TButtonedEdit;
    grpChars: TGroupBox;
    lblFieldTerminater: TLabel;
    lblFieldEncloser: TLabel;
    lblFieldEscaper: TLabel;
    editFieldEscaper: TEdit;
    editFieldEncloser: TEdit;
    editFieldTerminator: TEdit;
    chkFieldsEnclosedOptionally: TCheckBox;
    grpOptions: TGroupBox;
    lblIgnoreLinesCount: TLabel;
    updownIgnoreLines: TUpDown;
    editIgnoreLines: TEdit;
    editLineTerminator: TEdit;
    lblLineTerminator: TLabel;
    lblIgnoreLines: TLabel;
    lblFilename: TLabel;
    comboEncoding: TComboBox;
    lblEncoding: TLabel;
    grpDuplicates: TRadioGroup;
    grpParseMethod: TRadioGroup;
    grpDestination: TGroupBox;
    chkLowPriority: TCheckBox;
    chkLocalNumbers: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure editFilenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure comboDatabaseChange(Sender: TObject);
    procedure comboTableChange(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure ServerParse(Sender: TObject);
    procedure ClientParse(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure btnColMoveClick(Sender: TObject);
    procedure grpParseMethodClick(Sender: TObject);
    procedure comboEncodingSelect(Sender: TObject);
  private
    { Private declarations }
    Encoding: TEncoding;
    Term, Encl, Escp, LineTerm: String;
    RowCount, ColumnCount: Integer;
    SelectedCharsetIndex: Integer;
    Columns: TTableColumnList;
  public
    { Public declarations }
  end;


implementation

uses Main, helpers;

{$R *.DFM}



procedure Tloaddataform.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  SetWindowSizeGrip(Handle, True);
  // Restore settings
  Width := GetRegValue(REGNAME_CSV_WINDOWWIDTH, Width);
  Height := GetRegValue(REGNAME_CSV_WINDOWHEIGHT, Height);
  editFilename.Text := GetRegValue(REGNAME_CSV_FILENAME, '');
  editFieldTerminator.Text := GetRegValue(REGNAME_CSV_SEPARATOR, DEFAULT_CSV_SEPARATOR);
  editFieldEncloser.Text := GetRegValue(REGNAME_CSV_ENCLOSER, DEFAULT_CSV_ENCLOSER);
  editLineTerminator.Text := GetRegValue(REGNAME_CSV_TERMINATOR, DEFAULT_CSV_TERMINATOR);
  chkFieldsEnclosedOptionally.Checked :=  GetRegValue(REGNAME_CSV_ENCLOPTION, chkFieldsEnclosedOptionally.Checked);
  editFieldEscaper.Text := GetRegValue(REGNAME_CSV_ESCAPER, editFieldEscaper.Text);
  updownIgnoreLines.Position := GetRegValue(REGNAME_CSV_IGNORELINES, updownIgnoreLines.Position);
  chkLowPriority.Checked := GetRegValue(REGNAME_CSV_LOWPRIO, chkLowPriority.Checked);
  chkLocalNumbers.Checked := GetRegValue(REGNAME_CSV_LOCALNUMBERS, chkLocalNumbers.Checked);
  grpDuplicates.ItemIndex := GetRegValue(REGNAME_CSV_DUPLICATES, grpDuplicates.ItemIndex);
  grpParseMethod.ItemIndex := GetRegValue(REGNAME_CSV_PARSEMETHOD, grpParseMethod.ItemIndex);
end;


procedure Tloaddataform.FormDestroy(Sender: TObject);
begin
  // Save settings
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_CSV_WINDOWWIDTH, Width);
  MainReg.WriteInteger(REGNAME_CSV_WINDOWHEIGHT, Height);
  MainReg.WriteString(REGNAME_CSV_FILENAME, editFilename.Text);
  MainReg.WriteString(REGNAME_CSV_SEPARATOR, editFieldTerminator.Text);
  MainReg.WriteString(REGNAME_CSV_ENCLOSER, editFieldEncloser.Text);
  MainReg.WriteString(REGNAME_CSV_TERMINATOR, editLineTerminator.Text);
  MainReg.WriteBool(REGNAME_CSV_ENCLOPTION, chkFieldsEnclosedOptionally.Checked);
  MainReg.WriteString(REGNAME_CSV_ESCAPER, editFieldEscaper.Text);
  MainReg.WriteInteger(REGNAME_CSV_IGNORELINES, updownIgnoreLines.Position);
  MainReg.WriteBool(REGNAME_CSV_LOWPRIO, chkLowPriority.Checked);
  MainReg.WriteBool(REGNAME_CSV_LOCALNUMBERS, chkLocalNumbers.Checked);
  MainReg.WriteInteger(REGNAME_CSV_DUPLICATES, grpDuplicates.ItemIndex);
  MainReg.WriteInteger(REGNAME_CSV_PARSEMETHOD, grpParseMethod.ItemIndex);
end;


procedure Tloaddataform.FormShow(Sender: TObject);
begin
  // read dbs and Tables from treeview
  comboDatabase.Items.Clear;
  comboDatabase.Items.Assign(Mainform.AllDatabases);
  comboDatabase.ItemIndex := comboDatabase.Items.IndexOf( Mainform.ActiveDatabase );
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;
  comboDatabaseChange(Sender);
  editFilename.SetFocus;
end;


procedure Tloaddataform.grpParseMethodClick(Sender: TObject);
var
  ServerWillParse: Boolean;
  Charset, DefCharset, dbcreate: String;
  v: Integer;
  CharsetTable: TMySQLQuery;
  rx: TRegExpr;
begin
  ServerWillParse := grpParseMethod.ItemIndex = 0;
  comboEncoding.Enabled := ServerWillParse;
  editFieldEscaper.Enabled := ServerWillParse;
  chkFieldsEnclosedOptionally.Enabled := ServerWillParse;
  comboEncoding.Clear;
  if comboEncoding.Enabled then begin
    // Populate charset combo
    v := Mainform.Connection.ServerVersionInt;
    if ((v >= 50038) and (v < 50100)) or (v >= 50117) then begin
      Charset := MainForm.GetCharsetByEncoding(Encoding);
      // Detect db charset
      DefCharset := 'Let server/database decide';
      dbcreate := Mainform.Connection.GetVar('SHOW CREATE DATABASE '+Mainform.mask(comboDatabase.Text), 1);
      rx := TRegExpr.Create;
      rx.ModifierG := True;
      rx.Expression := 'CHARACTER SET (\w+)';
      if rx.Exec(dbcreate) then
        DefCharset := DefCharset + ' ('+rx.Match[1]+')';
      comboEncoding.Items.Add(DefCharset);
      CharsetTable := Mainform.Connection.CharsetTable;
      CharsetTable.First;
      while not CharsetTable.Eof do begin
        comboEncoding.Items.Add(CharsetTable.Col(1) + ' ('+CharsetTable.Col(0)+')');
        if (SelectedCharsetIndex = -1) and (Charset = CharsetTable.Col(0)) then
          SelectedCharsetIndex := comboEncoding.Items.Count-1;
        CharsetTable.Next;
      end;
      if SelectedCharsetIndex = -1 then
        SelectedCharsetIndex := 0;
      comboEncoding.ItemIndex := SelectedCharsetIndex;
    end else begin
      comboEncoding.Items.Add('Unsupported by this server');
      comboEncoding.ItemIndex := 0;
    end;
  end else begin
    comboEncoding.Items.Add(Mainform.GetEncodingName(Encoding));
    comboEncoding.ItemIndex := 0;
  end;
end;


procedure Tloaddataform.comboDatabaseChange(Sender: TObject);
var
  count, i: Integer;
  DBObjects: TDBObjectList;
  seldb, seltable: String;
begin
  // read tables from db
  comboTable.Items.Clear;
  seldb := Mainform.ActiveDatabase;
  seltable := Mainform.SelectedTable.Name;
  DBObjects := Mainform.Connection.GetDBObjects(comboDatabase.Text);
  for i:=0 to DBObjects.Count-1 do begin
    if DBObjects[i].NodeType in [lntTable, lntView] then
      comboTable.Items.Add(DBObjects[i].Name);
    count := comboTable.Items.Count-1;
    if (comboDatabase.Text = seldb) and (comboTable.Items[count] = seltable) then
      comboTable.ItemIndex := count;
  end;
  if comboTable.ItemIndex = -1 then
    comboTable.ItemIndex := 0;

  grpParseMethod.OnClick(Sender);
  comboTableChange(Sender);
end;


procedure Tloaddataform.comboEncodingSelect(Sender: TObject);
begin
  SelectedCharsetIndex := comboEncoding.ItemIndex;
end;

procedure Tloaddataform.comboTableChange(Sender: TObject);
var
  Algorithm, CheckOption, SelectCode: String;
  Col: TTableColumn;
  DBObjects: TDBObjectList;
  Obj: TDBObject;
begin
  // fill columns:
  chklistColumns.Items.Clear;
  if (comboDatabase.Text <> '') and (comboTable.Text <> '') then begin
    if not Assigned(Columns) then
      Columns := TTableColumnList.Create;
    DBObjects := Mainform.Connection.GetDBObjects(comboDatabase.Text);
    for Obj in DBObjects do begin
      if (Obj.Database=comboDatabase.Text) and (Obj.Name=comboTable.Text) then begin
        case Obj.NodeType of
          lntTable: ParseTableStructure(Obj.CreateCode, Columns, nil, nil);
          lntView: ParseViewStructure(Obj.CreateCode, Obj.Name, Columns, Algorithm, CheckOption, SelectCode);
        end;
      end;
    end;
    for Col in Columns do
      chklistColumns.Items.Add(Col.Name);
  end;

  // select all:
  ToggleCheckListBox( chklistColumns, True );

  // Ensure valid state of Import-Button
  editFilenameChange(Sender);
end;


procedure Tloaddataform.btnImportClick(Sender: TObject);
var
  StartTickCount: Cardinal;
  i: Integer;
  Warnings: TMySQLQuery;
begin
  Screen.Cursor := crHourglass;
  StartTickCount := GetTickCount;

  ColumnCount := 0;
  for i:=0 to chkListColumns.Items.Count-1 do begin
    if chkListColumns.Checked[i] then
      Inc(ColumnCount);
  end;

  Term := MainForm.Connection.UnescapeString(editFieldTerminator.Text);
  Encl := MainForm.Connection.UnescapeString(editFieldEncloser.Text);
  LineTerm := MainForm.Connection.UnescapeString(editLineTerminator.Text);
  Escp := MainForm.Connection.UnescapeString(editFieldEscaper.Text);

  try
    case grpParseMethod.ItemIndex of
      0: ServerParse(Sender);
      1: ClientParse(Sender);
    end;
    MainForm.LogSQL(FormatNumber(RowCount)+' rows imported in '+FormatNumber((GetTickcount-StartTickCount)/1000, 3)+' seconds.');
    // SHOW WARNINGS is implemented as of MySQL 4.1.0
    if MainForm.Connection.ServerVersionInt >= 40100 then begin
      Warnings := MainForm.Connection.GetResults('SHOW WARNINGS');
      while not Warnings.Eof do begin
        MainForm.LogSQL(Warnings.Col(0)+' ('+Warnings.Col(1)+'): '+Warnings.Col(2), lcError);
        Warnings.Next;
      end;
      if Warnings.RecordCount > 0 then begin
        MessageDlg('Your file was imported but the server returned '+FormatNumber(Warnings.RecordCount)+' warnings and/or notes. See the log panel for details.', mtError, [mbOK], 0);
        ModalResult := mrNone;
      end;
    end;
    // Hint user if zero rows were detected in file
    if (ModalResult <> mrNone) and (RowCount = 0) then begin
      MessageDlg('No rows were imported. This can have several causes:'+CRLF+
        ' - File is empty'+CRLF+
        ' - Wrong file encoding was selected or detected'+CRLF+
        ' - Field and/or line terminator do not fit to the file contents'
        , mtError, [mbOK], 0);
      ModalResult := mrNone;
    end;

  except
    on E:EDatabaseError do begin
      Screen.Cursor := crDefault;
      ModalResult := mrNone;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


procedure Tloaddataform.ServerParse(Sender: TObject);
var
  SQL, SetColVars: String;
  i: Integer;
begin
  SQL := 'LOAD DATA ';
  if chkLowPriority.Checked then
    SQL := SQL + 'LOW_PRIORITY ';
  SQL := SQL + 'LOCAL INFILE ' + esc(editFilename.Text) + ' ';
  case grpDuplicates.ItemIndex of
    1: SQL := SQL + 'IGNORE ';
    2: SQL := SQL + 'REPLACE ';
  end;
  SQL := SQL + 'INTO TABLE ' + Mainform.Mask(comboDatabase.Text) + '.' +  Mainform.Mask(comboTable.Text) + ' ';

  if comboEncoding.ItemIndex > 0 then begin
    Mainform.Connection.CharsetTable.RecNo := comboEncoding.ItemIndex-1;
    SQL := SQL + 'CHARACTER SET '+Mainform.Connection.CharsetTable.Col(0)+' ';
  end;

  // Fields:
  if (Term <> '') or (Encl <> '') or (Escp <> '') then
    SQL := SQL + 'FIELDS ';
  if editFieldTerminator.Text <> '' then
    SQL := SQL + 'TERMINATED BY ' + esc(Term) + ' ';
  if Encl <> '' then begin
    if chkFieldsEnclosedOptionally.Checked then
      SQL := SQL + 'OPTIONALLY ';
    SQL := SQL + 'ENCLOSED BY ' + esc(Encl) + ' ';
  end;
  if Escp <> '' then
    SQL := SQL + 'ESCAPED BY ' + esc(Escp) + ' ';

  // Lines:
  if LineTerm <> '' then
    SQL := SQL + 'LINES TERMINATED BY ' + esc(LineTerm) + ' ';
  if updownIgnoreLines.Position > 0 then
    SQL := SQL + 'IGNORE ' + inttostr(updownIgnoreLines.Position) + ' LINES ';

  // Column listing
  SQL := SQL + '(';
  SetColVars := '';
  for i:=0 to chklistColumns.Items.Count-1 do begin
    if chklistColumns.Checked[i] then begin
      if chkLocalNumbers.Checked and (Columns[i].DataType.Category in [dtcInteger, dtcReal]) then begin
        SQL := SQL + '@ColVar' + IntToStr(i) + ', ';
        SetColVars := SetColVars + Mainform.Mask(chklistColumns.Items[i]) +
          ' = REPLACE(REPLACE(@ColVar' + IntToStr(i) + ', '+esc(ThousandSeparator)+', ''''), '+esc(DecimalSeparator)+', ''.''), ';
      end else
        SQL := SQL + Mainform.Mask(chklistColumns.Items[i]) + ', ';
    end;
  end;
  SetLength(SQL, Length(SQL)-2);
  SQL := SQL + ')';
  if SetColVars <> '' then begin
    SetLength(SetColVars, Length(SetColVars)-2);
    SQL := SQL + ' SET ' + SetColVars;
  end;


  Mainform.Connection.Query(SQL);
  RowCount := Max(MainForm.Connection.RowsAffected, 0);
end;


procedure Tloaddataform.ClientParse(Sender: TObject);
var
  P, ContentLen, ProgressCharsPerStep, ProgressChars: Integer;
  IgnoreLines, ValueCount, PacketSize: Integer;
  EnclLen, TermLen, LineTermLen: Integer;
  Contents: String;
  EnclTest, TermTest, LineTermTest: String;
  Value, SQL: String;
  IsEncl, IsTerm, IsLineTerm: Boolean;
  InEncl: Boolean;
  OutStream: TMemoryStream;
const
  ProgressBarSteps=100;

  procedure NextChar;
  begin
    Inc(P);
    Inc(ProgressChars);
    if ProgressChars >= ProgressCharsPerStep then begin
      Mainform.ProgressBarStatus.StepIt;
      Mainform.ShowStatusMsg('Importing textfile, row '+FormatNumber(RowCount-IgnoreLines)+', '+IntToStr(Mainform.ProgressBarStatus.Position)+'%');
      ProgressChars := 0;
    end;
  end;

  function TestLeftChars(var Portion: String; CompareTo: String; Len: Integer): Boolean;
  var i: Integer;
  begin
    if Len > 0 then begin
      for i:=1 to Len-1 do
        Portion[i] := Portion[i+1];
      Portion[Len] := Contents[P];
      Result := Portion = CompareTo;
    end else
      Result := False;
  end;

  procedure AddValue;
  var
    i: Integer;
    LowPrio: String;
  begin
    Inc(ValueCount);
    if ValueCount <= ColumnCount then begin
      if Copy(Value, 1, EnclLen) = Encl then begin
        Delete(Value, 1, EnclLen);
        Delete(Value, Length(Value)-EnclLen+1, EnclLen);
      end;
      if SQL = '' then begin
        LowPrio := '';
        if chkLowPriority.Checked then
          LowPrio := 'LOW_PRIORITY ';
        case grpDuplicates.ItemIndex of
          0: SQL := 'INSERT '+LowPrio;
          1: SQL := 'INSERT '+LowPrio+'IGNORE ';
          2: SQL := 'REPLACE '+LowPrio;
        end;
        SQL := SQL + 'INTO '+MainForm.mask(comboDatabase.Text)+'.'+MainForm.mask(comboTable.Text)+' (';
        for i:=0 to chkListColumns.Items.Count-1 do begin
          if chkListColumns.Checked[i] then
            SQL := SQL + MainForm.mask(chkListColumns.Items[i]) + ', ';
        end;
        SetLength(SQL, Length(SQL)-2);
        SQL := SQL + ') VALUES (';
      end;
      if Value <> 'NULL' then begin
        if chkLocalNumbers.Checked and (Columns[ValueCount-1].DataType.Category in [dtcInteger, dtcReal]) then
          Value := UnformatNumber(Value)
        else
          Value := esc(Value);
      end;
      SQL := SQL + Value + ', ';
    end;
    Value := '';
  end;

  procedure AddRow;
  var
    SA: AnsiString;
    ChunkSize: Int64;
    i: Integer;
  begin
    if SQL = '' then
      Exit;
    Inc(RowCount);
    for i:=ValueCount to ColumnCount do begin
      Value := 'NULL';
      AddValue;
    end;
    ValueCount := 0;
    if RowCount > IgnoreLines then begin
      Delete(SQL, Length(SQL)-1, 2);
      StreamWrite(OutStream, SQL + ')');
      SQL := '';
      if (OutStream.Size < PacketSize) and (P < ContentLen) then
        SQL := SQL + ', ('
      else begin
        OutStream.Position := 0;
        ChunkSize := OutStream.Size;
        SetLength(SA, ChunkSize div SizeOf(AnsiChar));
        OutStream.Read(PAnsiChar(SA)^, ChunkSize);
        OutStream.Size := 0;
        Mainform.Connection.Query(UTF8ToString(SA));
        SQL := '';
      end;
    end else
      SQL := '';
  end;

begin
  EnableProgressBar(ProgressBarSteps);

  TermLen := Length(Term);
  EnclLen := Length(Encl);
  LineTermLen := Length(LineTerm);

  SetLength(TermTest, TermLen);
  SetLength(EnclTest, EnclLen);
  SetLength(LineTermTest, LineTermLen);

  InEncl := False;

  SQL := '';
  Value := '';
  OutStream := TMemoryStream.Create;

  MainForm.ShowStatusMsg('Reading textfile ('+FormatByteNumber(_GetFileSize(editFilename.Text))+') ...');
  Contents := ReadTextfile(editFilename.Text, Encoding);
  ContentLen := Length(Contents);
  MainForm.ShowStatusMsg;

  P := 0;
  ProgressCharsPerStep := ContentLen div ProgressBarSteps;
  ProgressChars := 0;
  RowCount := 0;
  IgnoreLines := UpDownIgnoreLines.Position;
  ValueCount := 0;
  PacketSize := SIZE_MB div 2;
  NextChar;

  // TODO: read chunks!
  while P <= ContentLen do begin
    // Check characters left-side from current position
    IsEncl := TestLeftChars(EnclTest, Encl, EnclLen);
    IsTerm := TestLeftChars(TermTest, Term, TermLen);
    IsLineTerm := TestLeftChars(LineTermTest, LineTerm, LineTermLen) and (ValueCount >= ColumnCount-1);

    Value := Value + Contents[P];

    if IsEncl then
      InEncl := not InEncl;

    if not InEncl then begin
      if IsTerm then begin
        SetLength(Value, Length(Value)-TermLen);
        AddValue;
      end else if IsLineTerm then begin
        SetLength(Value, Length(Value)-LineTermLen);
        AddValue;
      end;
    end;

    if IsLineTerm and (not InEncl) then
      AddRow;

    NextChar;
  end;
  // Will check if SQL is empty and not run any query in that case:
  AddRow;

  Contents := '';
  FreeAndNil(OutStream);
  RowCount := Max(RowCount-IgnoreLines, 0);
  Mainform.ProgressBarStatus.Hide;
end;


procedure Tloaddataform.btnOpenFileClick(Sender: TObject);
var
  Dialog: TOpenTextFileDialog;
  TestStream: TFileStream;
begin
  Dialog := TOpenTextFileDialog.Create(Self);
  Dialog.Filter := 'MySQL CSV files (*.csv)|*.csv|Text files (*.txt)|*.txt|All files (*.*)|*.*';
  Dialog.DefaultExt := 'csv';
  Dialog.Encodings.Assign(Mainform.FileEncodings);
  Dialog.EncodingIndex := 0;
  if Dialog.Execute then begin
    editfilename.Text := Dialog.FileName;
    Encoding := Mainform.GetEncodingByName(Dialog.Encodings[Dialog.EncodingIndex]);
    if Encoding = nil then begin
      TestStream := TFileStream.Create(Dialog.Filename, fmOpenRead or fmShareDenyNone);
      Encoding := DetectEncoding(TestStream);
      TestStream.Free;
    end;
    SelectedCharsetIndex := -1;
    grpParseMethod.OnClick(Sender);
  end;
  Dialog.Free;
end;


procedure Tloaddataform.btnColMoveClick(Sender: TObject);
var
  CheckedSelected, CheckedTarget: Boolean;
  TargetIndex: Integer;
begin
  // Move column name and its checkstate up or down
  if Sender = btnColUp then
    TargetIndex := chklistColumns.ItemIndex-1
  else
    TargetIndex := chklistColumns.ItemIndex+1;
  if (TargetIndex > -1) and (TargetIndex < chklistColumns.Count) then begin
    CheckedSelected := chklistColumns.Checked[chklistColumns.ItemIndex];
    CheckedTarget := chklistColumns.Checked[TargetIndex];
    chklistColumns.Items.Exchange(chklistColumns.ItemIndex, TargetIndex);
    chklistColumns.Checked[chklistColumns.ItemIndex] := CheckedTarget;
    chklistColumns.Checked[TargetIndex] := CheckedSelected;
    chklistColumns.ItemIndex := TargetIndex;
  end;
end;



{** Make "OK"-button only clickable if
 - filename is not empty
 - table is selected
 - columnnames could be fetched normally
 - filename exists
}
procedure Tloaddataform.editFilenameChange(Sender: TObject);
begin
  btnImport.Enabled := (editFilename.Text <> '')
    and (chklistColumns.Items.Count > 0)
    and (FileExists(editFilename.Text));
end;


end.
