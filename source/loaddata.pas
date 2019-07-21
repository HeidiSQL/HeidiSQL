unit loaddata;


// -------------------------------------
// Load Textfile into table
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, CheckLst,
  SynRegExpr, Buttons, ExtCtrls, ToolWin, ExtDlgs, Math, extra_controls,
  dbconnection, dbstructures, gnugettext;

type
  Tloaddataform = class(TExtForm)
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
    chkTruncateTable: TCheckBox;
    btnCheckAll: TToolButton;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chklistColumnsClick(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FFileEncoding: TEncoding;
    Term, Encl, Escp, LineTerm: String;
    RowCount, ColumnCount: Integer;
    Columns: TTableColumnList;
    FConnection: TDBConnection;
  public
    { Public declarations }
  end;


implementation

uses Main, apphelpers;

{$R *.DFM}

const
  ProgressBarSteps=100;


procedure Tloaddataform.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  // Restore settings
  Width := AppSettings.ReadInt(asCSVImportWindowWidth);
  Height := AppSettings.ReadInt(asCSVImportWindowHeight);
  editFilename.Text := AppSettings.ReadString(asCSVImportFilename);
  editFieldTerminator.Text := AppSettings.ReadString(asCSVImportSeparator);
  editFieldEncloser.Text := AppSettings.ReadString(asCSVImportEncloser);
  editLineTerminator.Text := AppSettings.ReadString(asCSVImportTerminator);
  chkFieldsEnclosedOptionally.Checked :=  AppSettings.ReadBool(asCSVImportFieldsEnclosedOptionally);
  editFieldEscaper.Text := AppSettings.ReadString(asCSVImportFieldEscaper);
  updownIgnoreLines.Position := AppSettings.ReadInt(asCSVImportIgnoreLines);
  chkLowPriority.Checked := AppSettings.ReadBool(asCSVImportLowPriority);
  chkLocalNumbers.Checked := AppSettings.ReadBool(asCSVImportLocalNumbers);
  // Uncheck critical "Truncate table" checkbox, to avoid accidental data removal
  chkTruncateTable.Checked := False;
  grpDuplicates.ItemIndex := AppSettings.ReadInt(asCSVImportDuplicateHandling);
  grpParseMethod.ItemIndex := AppSettings.ReadInt(asCSVImportParseMethod);
end;


procedure Tloaddataform.FormDestroy(Sender: TObject);
begin
  // Save settings
  AppSettings.WriteInt(asCSVImportWindowWidth, Width);
  AppSettings.WriteInt(asCSVImportWindowHeight, Height);
  AppSettings.WriteString(asCSVImportFilename, editFilename.Text);
  AppSettings.WriteString(asCSVImportSeparator, editFieldTerminator.Text);
  AppSettings.WriteString(asCSVImportEncloser, editFieldEncloser.Text);
  AppSettings.WriteString(asCSVImportTerminator, editLineTerminator.Text);
  AppSettings.WriteBool(asCSVImportFieldsEnclosedOptionally, chkFieldsEnclosedOptionally.Checked);
  AppSettings.WriteString(asCSVImportFieldEscaper, editFieldEscaper.Text);
  AppSettings.WriteInt(asCSVImportIgnoreLines, updownIgnoreLines.Position);
  AppSettings.WriteBool(asCSVImportLowPriority, chkLowPriority.Checked);
  AppSettings.WriteBool(asCSVImportLocalNumbers, chkLocalNumbers.Checked);
  AppSettings.WriteInt(asCSVImportDuplicateHandling, grpDuplicates.ItemIndex);
  AppSettings.WriteInt(asCSVImportParseMethod, grpParseMethod.ItemIndex);
end;


procedure Tloaddataform.FormResize(Sender: TObject);
var
  HalfWidth, RightBoxX: Integer;
begin
  // Rethink width of side-by-side group boxes
  HalfWidth := (ClientWidth - 3 * grpFilename.Left) div 2;
  RightBoxX := HalfWidth + 2 * grpFilename.Left;
  grpOptions.Width := HalfWidth;
  grpDuplicates.Width := HalfWidth;
  grpParseMethod.Width := HalfWidth;
  grpChars.Width := HalfWidth;
  grpDestination.Width := HalfWidth;
  // Move right boxes to the right position
  grpChars.Left := RightBoxX;
  grpDestination.Left := RightBoxX;
end;


procedure Tloaddataform.FormShow(Sender: TObject);
begin
  FConnection := MainForm.ActiveConnection;

  // Disable features supported in MySQL only, if active connection is not MySQL
  if not FConnection.Parameters.IsMySQL then begin
    grpParseMethod.ItemIndex := 1;
    grpDuplicates.ItemIndex := 0;
  end;
  grpParseMethod.Controls[0].Enabled := FConnection.Parameters.IsMySQL;
  grpDuplicates.Controls[1].Enabled := FConnection.Parameters.IsMySQL;
  grpDuplicates.Controls[2].Enabled := FConnection.Parameters.IsMySQL;
  chkLowPriority.Enabled := FConnection.Parameters.IsMySQL;

  // Read databases and tables from active connection
  comboDatabase.Items.Clear;
  comboDatabase.Items.Assign(FConnection.AllDatabases);
  comboDatabase.ItemIndex := comboDatabase.Items.IndexOf(Mainform.ActiveDatabase);
  if comboDatabase.ItemIndex = -1 then
    comboDatabase.ItemIndex := 0;
  comboDatabaseChange(Sender);

  editFilename.SetFocus;
end;


procedure Tloaddataform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure Tloaddataform.grpParseMethodClick(Sender: TObject);
var
  ServerWillParse: Boolean;
  FileCharset, Item: String;
  v, i: Integer;
  CharsetTable: TDBQuery;
begin
  ServerWillParse := grpParseMethod.ItemIndex = 0;
  comboEncoding.Enabled := ServerWillParse;
  editFieldEscaper.Enabled := ServerWillParse;
  chkFieldsEnclosedOptionally.Enabled := ServerWillParse;
  comboEncoding.Clear;
  if comboEncoding.Enabled then begin
    // Populate charset combo
    v := FConnection.ServerVersionInt;
    if ((v >= 50038) and (v < 50100)) or (v >= 50117) then begin
      FileCharset := MainForm.GetCharsetByEncoding(FFileEncoding);
      if FileCharset.IsEmpty then
        FileCharset := 'utf8';
      CharsetTable := FConnection.CharsetTable;
      CharsetTable.First;
      while not CharsetTable.Eof do begin
        if IsNotEmpty(CharsetTable.Col(0)) then
          Item := CharsetTable.Col(0);
        if IsNotEmpty(CharsetTable.Col(1)) then
          Item := Item + ': ' + CharsetTable.Col(1);
        comboEncoding.Items.Add(Item);
        CharsetTable.Next;
      end;

      // Preselect file encoding, or utf8 as a fallback
      for i:=0 to comboEncoding.Items.Count-1 do begin
        if ExecRegExpr('^'+QuoteRegExprMetaChars(FileCharset)+'\b', comboEncoding.Items[i]) then begin
          comboEncoding.ItemIndex :=  i;
          Break;
        end;
      end;

    end else begin
      comboEncoding.Items.Add(_(SUnsupported));
      comboEncoding.ItemIndex := 0;
    end;
  end else begin
    comboEncoding.Items.Add(Mainform.GetEncodingName(FFileEncoding));
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
  seltable := Mainform.ActiveDbObj.Name;
  DBObjects := FConnection.GetDBObjects(comboDatabase.Text);
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


procedure Tloaddataform.comboTableChange(Sender: TObject);
var
  DummyStr: String;
  Col: TTableColumn;
  DBObjects: TDBObjectList;
  Obj: TDBObject;
begin
  // fill columns:
  chklistColumns.Items.Clear;
  if (comboDatabase.Text <> '') and (comboTable.Text <> '') then begin
    if not Assigned(Columns) then
      Columns := TTableColumnList.Create;
    DBObjects := FConnection.GetDBObjects(comboDatabase.Text);
    for Obj in DBObjects do begin
      if (Obj.Database=comboDatabase.Text) and (Obj.Name=comboTable.Text) then begin
        case Obj.NodeType of
          lntTable: Obj.Connection.ParseTableStructure(Obj.CreateCode, Columns, nil, nil);
          lntView: Obj.Connection.ParseViewStructure(Obj.CreateCode, Obj, Columns, DummyStr, DummyStr, DummyStr, DummyStr, DummyStr);
        end;
      end;
    end;
    for Col in Columns do
      chklistColumns.Items.Add(Col.Name);
  end;

  // select all:
  chklistColumns.CheckAll(cbChecked);
  chklistColumns.OnClick(Sender);

  // Ensure valid state of Import-Button
  editFilenameChange(Sender);
end;


procedure Tloaddataform.btnImportClick(Sender: TObject);
var
  StartTickCount: Cardinal;
  i: Integer;
  Warnings: TDBQuery;
begin
  Screen.Cursor := crHourglass;
  StartTickCount := GetTickCount;
  MainForm.EnableProgress(ProgressBarSteps);

  // Truncate table before importing
  if chkTruncateTable.Checked then try
    FConnection.Query('TRUNCATE TABLE ' + FConnection.QuotedDbAndTableName(comboDatabase.Text, comboTable.Text));
  except
    try
      FConnection.Query('DELETE FROM ' + FConnection.QuotedDbAndTableName(comboDatabase.Text, comboTable.Text));
    except
      on E:EDbError do
        ErrorDialog(_('Cannot truncate table'), E.Message);
    end;
  end;

  ColumnCount := 0;
  for i:=0 to chkListColumns.Items.Count-1 do begin
    if chkListColumns.Checked[i] then
      Inc(ColumnCount);
  end;

  Term := FConnection.UnescapeString(editFieldTerminator.Text);
  Encl := FConnection.UnescapeString(editFieldEncloser.Text);
  LineTerm := FConnection.UnescapeString(editLineTerminator.Text);
  Escp := FConnection.UnescapeString(editFieldEscaper.Text);

  try
    case grpParseMethod.ItemIndex of
      0: ServerParse(Sender);
      1: ClientParse(Sender);
    end;
    MainForm.LogSQL(FormatNumber(RowCount)+' rows imported in '+FormatNumber((GetTickcount-StartTickCount)/1000, 3)+' seconds.');
    // SHOW WARNINGS is implemented as of MySQL 4.1.0
    if FConnection.Parameters.IsMySQL and (FConnection.ServerVersionInt >= 40100) then begin
      Warnings := FConnection.GetResults('SHOW WARNINGS');
      while not Warnings.Eof do begin
        MainForm.LogSQL(Warnings.Col(0)+' ('+Warnings.Col(1)+'): '+Warnings.Col(2), lcError);
        Warnings.Next;
      end;
      if Warnings.RecordCount > 0 then begin
        ErrorDialog(f_('Your file was imported but the server returned %s warnings and/or notes. See the log panel for details.', [FormatNumber(Warnings.RecordCount)]));
        ModalResult := mrNone;
      end;
    end;
    // Hint user if zero rows were detected in file
    if (ModalResult <> mrNone) and (RowCount = 0) then begin
      ErrorDialog(_('No rows were imported'),
        _('This can have several causes:')+CRLF+
        _(' - File is empty')+CRLF+
        _(' - Wrong file encoding was selected or detected')+CRLF+
        _(' - Field and/or line terminator do not fit to the file contents')
        );
      ModalResult := mrNone;
    end;

  except
    on E:EDbError do begin
      Screen.Cursor := crDefault;
      ModalResult := mrNone;
      MainForm.SetProgressState(pbsError);
      ErrorDialog(E.Message);
    end;
  end;

  Mainform.ShowStatusMsg;
  MainForm.DisableProgress;
  Screen.Cursor := crDefault;
end;


procedure Tloaddataform.ServerParse(Sender: TObject);
var
  SQL, SetColVars, SelectedCharset: String;
  i: Integer;
begin
  SQL := 'LOAD DATA ';
  if chkLowPriority.Checked and chkLowPriority.Enabled then
    SQL := SQL + 'LOW_PRIORITY ';
  SQL := SQL + 'LOCAL INFILE ' + esc(editFilename.Text) + ' ';
  case grpDuplicates.ItemIndex of
    1: SQL := SQL + 'IGNORE ';
    2: SQL := SQL + 'REPLACE ';
  end;
  SQL := SQL + 'INTO TABLE ' + FConnection.QuotedDbAndTableName(comboDatabase.Text, comboTable.Text) + ' ';

  SelectedCharset := RegExprGetMatch('^(\w+)\b', comboEncoding.Text, 1);
  if not SelectedCharset.IsEmpty then begin
    SQL := SQL + 'CHARACTER SET '+SelectedCharset+' ';
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
        SetColVars := SetColVars + FConnection.QuoteIdent(chklistColumns.Items[i]) +
          ' = REPLACE(REPLACE(@ColVar' + IntToStr(i) + ', '+esc(FormatSettings.ThousandSeparator)+', ''''), '+esc(FormatSettings.DecimalSeparator)+', ''.''), ';
      end else
        SQL := SQL + FConnection.QuoteIdent(chklistColumns.Items[i]) + ', ';
    end;
  end;
  SetLength(SQL, Length(SQL)-2);
  SQL := SQL + ')';
  if SetColVars <> '' then begin
    SetLength(SetColVars, Length(SetColVars)-2);
    SQL := SQL + ' SET ' + SetColVars;
  end;


  FConnection.Query(SQL);
  RowCount := Max(FConnection.RowsAffected, 0);
end;


procedure Tloaddataform.ClientParse(Sender: TObject);
var
  P, ContentLen, ProgressCharsPerStep, ProgressChars: Integer;
  IgnoreLines, ValueCount, PacketSize: Integer;
  EnclLen, TermLen, LineTermLen: Integer;
  Contents: String;
  EnclTest, TermTest, LineTermTest: String;
  Value, SQL: String;
  IsEncl, IsTerm, IsLineTerm, IsEof: Boolean;
  InEncl: Boolean;
  OutStream: TMemoryStream;

  procedure NextChar;
  begin
    Inc(P);
    Inc(ProgressChars);
    if ProgressChars >= ProgressCharsPerStep then begin
      Mainform.ProgressStep;
      Mainform.ShowStatusMsg(f_('Importing textfile, row %s, %d%%', [FormatNumber(RowCount-IgnoreLines), Mainform.ProgressBarStatus.Position]));
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
        if chkLowPriority.Checked and chkLowPriority.Enabled then
          LowPrio := 'LOW_PRIORITY ';
        case grpDuplicates.ItemIndex of
          0: SQL := 'INSERT '+LowPrio;
          1: SQL := 'INSERT '+LowPrio+'IGNORE ';
          2: SQL := 'REPLACE '+LowPrio;
        end;
        SQL := SQL + 'INTO '+FConnection.QuotedDbAndTableName(comboDatabase.Text, comboTable.Text)+' (';
        for i:=0 to chkListColumns.Items.Count-1 do begin
          if chkListColumns.Checked[i] then
            SQL := SQL + FConnection.QuoteIdent(chkListColumns.Items[i]) + ', ';
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
        FConnection.Query(UTF8ToString(SA), False, lcScript);
        SQL := '';
      end;
    end else
      SQL := '';
  end;

begin
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

  MainForm.ShowStatusMsg(f_('Reading textfile (%s) ...', [FormatByteNumber(_GetFileSize(editFilename.Text))]));
  Contents := ReadTextfile(editFilename.Text, FFileEncoding);
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
    IsEof := P = ContentLen;

    Value := Value + Contents[P];

    if IsEncl then
      InEncl := not InEncl;

    if IsEof or (not InEncl) then begin
      if IsLineTerm then begin
        SetLength(Value, Length(Value)-LineTermLen);
        AddValue;
      end else if IsEof then begin
        AddValue;
      end else if IsTerm then begin
        SetLength(Value, Length(Value)-TermLen);
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
end;


procedure Tloaddataform.btnOpenFileClick(Sender: TObject);
var
  Dialog: TOpenTextFileDialog;
  TestStream: TFileStream;
begin
  AppSettings.ResetPath;
  Dialog := TOpenTextFileDialog.Create(Self);
  Dialog.Filter := _('CSV files')+' (*.csv)|*.csv|'+_('Text files')+' (*.txt)|*.txt|'+_('All files')+' (*.*)|*.*';
  Dialog.DefaultExt := 'csv';
  Dialog.Encodings.Assign(Mainform.FileEncodings);
  Dialog.EncodingIndex := AppSettings.ReadInt(asFileDialogEncoding, Self.Name);
  if Dialog.Execute then begin
    editfilename.Text := Dialog.FileName;
    FFileEncoding := Mainform.GetEncodingByName(Dialog.Encodings[Dialog.EncodingIndex]);
    if FFileEncoding = nil then begin
      TestStream := TFileStream.Create(Dialog.Filename, fmOpenRead or fmShareDenyNone);
      FFileEncoding := DetectEncoding(TestStream);
      TestStream.Free;
    end;
    grpParseMethod.OnClick(Sender);
    AppSettings.WriteInt(asFileDialogEncoding, Dialog.EncodingIndex, Self.Name);
  end;
  Dialog.Free;
end;


procedure Tloaddataform.chklistColumnsClick(Sender: TObject);
var
  i, CheckedNum: Integer;
begin
  btnColDown.Enabled := (chklistColumns.ItemIndex > -1)
    and (chklistColumns.ItemIndex < chklistColumns.Count-1);
  btnColUp.Enabled := (chklistColumns.ItemIndex > -1)
    and (chklistColumns.ItemIndex > 0);
  // Toggle icon when none is selected
  CheckedNum := 0;
  for i:=0 to chklistColumns.Items.Count-1 do begin
    if chklistColumns.Checked[i] then
      Inc(CheckedNum);
  end;
  if CheckedNum < chklistColumns.Items.Count then
    btnCheckAll.ImageIndex := 127
  else
    btnCheckAll.ImageIndex := 128;
end;


procedure Tloaddataform.btnCheckAllClick(Sender: TObject);
var
  i, CheckedNum: Integer;
begin
  CheckedNum := 0;
  for i:=0 to chklistColumns.Items.Count-1 do begin
    if chklistColumns.Checked[i] then
      Inc(CheckedNum);
  end;
  if CheckedNum < chklistColumns.Items.Count then
    chklistColumns.CheckAll(cbChecked)
  else
    chklistColumns.CheckAll(cbUnchecked);
  chklistColumns.OnClick(Sender);
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
  chklistColumns.OnClick(Sender);
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
