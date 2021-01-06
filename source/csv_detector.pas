unit csv_detector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, SynEdit, SynMemo, extra_controls, apphelpers,
  loaddata, dbconnection, Vcl.ExtCtrls, gnugettext, dbstructures, System.Math, SynRegExpr;

type
  TfrmCsvDetector = class(TExtForm)
    btnScan: TButton;
    SynMemoCreateTable: TSynMemo;
    btnCancel: TButton;
    btnSave: TButton;
    TimerStartScan: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnScanClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FLoadDataFrm: Tloaddataform;
    FConnection: TDBConnection;
    function DetectColumnAttributes(Rows: TGridRows; IgnoreLines: Integer): TTableColumnList;
    function ComposeCreateStatement(Columns: TTableColumnList): String;
  public
  end;


var
  frmCsvDetector: TfrmCsvDetector;

implementation

{$R *.dfm}

uses main;



procedure TfrmCsvDetector.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  FLoadDataFrm := Tloaddataform(Owner);
  FConnection := MainForm.ActiveConnection;
end;

procedure TfrmCsvDetector.FormShow(Sender: TObject);
begin
  SynMemoCreateTable.Highlighter := MainForm.SynSQLSynUsed;
  MainForm.SetupSynEditors;
  TimerStartScan.Enabled := True;
end;

procedure TfrmCsvDetector.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TfrmCsvDetector.btnScanClick(Sender: TObject);
var
  Stream: TFileStream;
  Encoding: TEncoding;
  GridRows: TGridRows;
  GridRow: TGridRow;
  GridValue: TGridValue;
  Term, Encl, Escp, LineTerm: String;
  RowNum, IgnoreLines: Integer;
  P, ContentLen, ProgressCharsPerStep, ProgressChars: Integer;
  EnclLen, TermLen, LineTermLen: Integer;
  Contents: String;
  EnclTest, TermTest, LineTermTest: String;
  Value: String;
  IsEncl, IsTerm, IsLineTerm, IsEof: Boolean;
  InEncl: Boolean;
  Columns: TTableColumnList;
const
  TestChunkSize = 20*SIZE_MB;

  procedure NextChar;
  begin
    Inc(P);
    Inc(ProgressChars);
    if ProgressChars >= ProgressCharsPerStep then begin
      Mainform.ProgressStep;
      Mainform.ShowStatusMsg(f_('Parsing textfile, row %s, %d%%', [FormatNumber(GridRows.Count-IgnoreLines), Mainform.ProgressBarStatus.Position]));
      //Mainform.LogSQL(f_('Analyzing textfile, row %s, %d%%', [FormatNumber(Rows.Count-IgnoreLines), Mainform.ProgressBarStatus.Position]));
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
  begin
    if Copy(Value, 1, EnclLen) = Encl then begin
      Delete(Value, 1, EnclLen);
      Delete(Value, Length(Value)-EnclLen+1, EnclLen);
    end;
    GridValue := TGridValue.Create;
    GridValue.OldText := Value;
    GridValue.OldIsNull := Value = 'NULL';
    if GridRow = nil then
      GridRow := TGridRow.Create(True);
    GridRow.Add(GridValue);
    Value := '';
  end;

  procedure AddRow;
  begin
    Inc(RowNum);
    GridRows.Add(GridRow);
    GridRow := TGridRow.Create(True);
  end;

begin
  // Scan user selected file for column types
  TimerStartScan.Enabled := False;
  Screen.Cursor := crHourGlass;
  btnScan.ImageIndex := 150;
  btnScan.Enabled := False;
  // Parse contents to a TGridRows instance
  GridRows := TGridRows.Create(True);
  GridRow := nil;
  Term := FConnection.UnescapeString(FLoadDataFrm.editFieldTerminator.Text);
  Encl := FConnection.UnescapeString(FLoadDataFrm.editFieldEncloser.Text);
  LineTerm := FConnection.UnescapeString(FLoadDataFrm.editLineTerminator.Text);
  Escp := FConnection.UnescapeString(FLoadDataFrm.editFieldEscaper.Text);
  RowNum := 0;

  TermLen := Length(Term);
  EnclLen := Length(Encl);
  LineTermLen := Length(LineTerm);

  SetLength(TermTest, TermLen);
  SetLength(EnclTest, EnclLen);
  SetLength(LineTermTest, LineTermLen);

  InEncl := False;

  MainForm.ShowStatusMsg(f_('Reading textfile (%s) ...', [FormatByteNumber(TestChunkSize)]));
  Encoding := FLoadDataFrm.FileEncoding;
  OpenTextfile(FLoadDataFrm.editFilename.Text, Stream, Encoding);
  Contents := ReadTextfileChunk(Stream, Encoding, TestChunkSize);
  Stream.Free;
  ContentLen := Length(Contents);
  MainForm.ShowStatusMsg;

  P := 0;
  ProgressCharsPerStep := ContentLen div FLoadDataFrm.ProgressBarSteps;
  ProgressChars := 0;
  MainForm.EnableProgress(FLoadDataFrm.ProgressBarSteps);
  IgnoreLines := FLoadDataFrm.updownIgnoreLines.Position;
  NextChar;

  while P <= ContentLen do begin
    // Check characters left-side from current position
    IsEncl := TestLeftChars(EnclTest, Encl, EnclLen);
    IsTerm := TestLeftChars(TermTest, Term, TermLen);
    IsLineTerm := TestLeftChars(LineTermTest, LineTerm, LineTermLen);
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

  Contents := '';

  // Find matching column types for values
  Columns := DetectColumnAttributes(GridRows, IgnoreLines);
  SynMemoCreateTable.Text := ComposeCreateStatement(Columns);

  GridRows.Free;

  MainForm.ShowStatusMsg;
  MainForm.DisableProgress;
  btnScan.ImageIndex := -1;
  btnScan.Enabled := True;
  Screen.Cursor := crDefault;
end;


function TfrmCsvDetector.DetectColumnAttributes(Rows: TGridRows; IgnoreLines: Integer): TTableColumnList;
var
  Row: TGridRow;
  Value: TGridValue;
  Col: TTableColumn;
  i, j, k: Integer;
  UnknownTypeYet, IsInteger, IsFloat, IsDate, IsDatetime, IsText: Boolean;
  ValueSize, TypeSize: Int64;
  FloatValue: Extended;
  LoopType: TDBDatatype;
const
  FloatChars = ['0'..'9', '.'];
begin
  MainForm.ShowStatusMsg(f_('Analyzing %s rows...', [FormatNumber(Rows.Count)]));
  MainForm.EnableProgress(Rows.Count);
  Result := TTableColumnList.Create;

  for Row in Rows do begin
    for Value in Row do begin
      Col := TTableColumn.Create(FConnection);
      if IgnoreLines > 0 then
        Col.Name := Value.OldText
      else
        Col.Name := 'col_'+Row.IndexOf(Value).ToString;
      Col.DataType := FConnection.Datatypes[0]; // UNKNOWN by default
      Col.AllowNull := False; // Make True as soon as we encounter NULL or empty strings in the values
      Col.Unsigned := False; // No detection for unsigned types
      Col.LengthSet := '';
      Result.Add(Col);
    end;
    Break;
  end;

  for i:=IgnoreLines to Rows.Count-1 do begin
    MainForm.ProgressStep;
    for j:=0 to Rows[i].Count-1 do begin
      Value := Rows[i][j];
      Col := Result[j];

      // Detect data type of current value

      IsInteger := IntToStr(StrToInt64Def(Value.OldText, -1)) = Value.OldText;

      FloatValue := StrToFloatDef(Value.OldText, -1, MainForm.FormatSettings);
      IsFloat := Value.OldText.Contains('.');
      if IsFloat then begin
        for k:=1 to Length(Value.OldText) do begin
          IsFloat := IsFloat and CharInSet(Value.OldText[k], FloatChars);
          if not IsFloat then
            Break;
        end;
      end;

      { Using StrToDateTimeDef allows values like '2020-12-08 foo'
      IsDate := (not IsInteger) and (not IsFloat)
        and (StrToDateDef(Value.OldText, MaxDateTime) <> MaxDateTime);
      IsDatetime := (not IsInteger) and (not IsFloat)
        and (StrToDateTimeDef(Value.OldText, MaxDateTime) <> MaxDateTime);}
      IsDate := (not IsInteger) and (not IsFloat)
        and ExecRegExpr('^\d{4}-\d{2}-\d{2}$', Value.OldText);
      IsDatetime := (not IsInteger) and (not IsFloat)
        and ExecRegExpr('^\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2}$', Value.OldText);

      IsText := (not IsInteger) and (not IsFloat) and (not IsDate) and (not IsDatetime);

      ValueSize := IfThen(IsInteger, StrToInt64Def(Value.OldText, -1), Length(Value.OldText));

      //MainForm.LogSQL(Format('Value:"%s" IsInteger:%d IsFloat:%d IsDate:%d IsDateTime:%d IsText:%d',
      //  [Value.OldText, IsInteger.ToInteger, IsFloat.ToInteger, IsDate.ToInteger, IsDatetime.ToInteger, IsText.ToInteger]), lcDebug);

      // Now, find a fitting data type for this column

      for k:=Low(FConnection.Datatypes) to High(FConnection.Datatypes) do begin

        LoopType := FConnection.Datatypes[k];
        UnknownTypeYet := Col.DataType.Index = dtUnknown;
        if (ValueSize = 0) or (CompareText(Value.OldText, 'null')=0) then
          Col.AllowNull := True;

        // Integer types
        if (LoopType.Category = dtcInteger) and IsInteger and (UnknownTypeYet or (Col.DataType.Category = dtcInteger)) then begin
          if (ValueSize > Col.DataType.MaxSize) and (ValueSize <= LoopType.MaxSize)
            then begin
            Col.DataType := LoopType;
          end;
        end;

        // Float types
        if (LoopType.Category = dtcReal) and IsFloat and (UnknownTypeYet or (Col.DataType.Category = dtcReal)) then begin
          if (ValueSize > Col.DataType.MaxSize) and (ValueSize <= LoopType.MaxSize)
            then begin
            Col.DataType := LoopType;
            if LoopType.RequiresLength then
              Col.LengthSet := LoopType.DefLengthSet;
          end;
        end;

        // Datetime type
        if IsDatetime and (UnknownTypeYet or (Col.DataType.Index in [dtDate, dtDatetime])) and (LoopType.Index = dtDatetime) then begin
          Col.DataType := LoopType;
        end;
        // Date type
        if IsDate and (UnknownTypeYet or (Col.DataType.Index = dtDate)) and (LoopType.Index = dtDate) then begin
          Col.DataType := LoopType;
        end;

        // Text types - fall back here if nothing else matches
        if (LoopType.Category = dtcText) and IsText then begin
          if ((not Col.LengthSet.IsEmpty) and (ValueSize > StrToInt64Def(Col.LengthSet, 0)))
            or ((ValueSize > Col.DataType.MaxSize) and (ValueSize <= LoopType.MaxSize))
            or (Col.DataType.Category <> LoopType.Category)
            then begin
            if Col.DataType.Index <> LoopType.Index then begin
              MainForm.LogSQL('Preferring '+LoopType.Name+' type over '+Col.DataType.Name+' for '+col.Name+' due to value "'+Value.OldText+'"', lcDebug);
            end;

            Col.DataType := LoopType;
            if Col.DataType.RequiresLength then begin
              TypeSize := Max(ValueSize, 1);
              TypeSize := System.Math.Ceil(TypeSize / 10) * 10;
              Col.LengthSet := Min(TypeSize, LoopType.MaxSize).ToString;
            end else
              Col.LengthSet := '';
          end;
        end;

      end;
    end;
    //break;
  end;

end;


function TfrmCsvDetector.ComposeCreateStatement(Columns: TTableColumnList): String;
var
  Col: TTableColumn;
  TableName: String;
begin
  // Compose CREATE TABLE
  TableName := ExtractBaseFileName(FLoadDataFrm.editFilename.Text);
  TableName := ValidFilename(TableName);
  Result := 'CREATE TABLE '+FConnection.QuoteIdent(FLoadDataFrm.comboDatabase.Text)+'.'+FConnection.QuoteIdent(TableName)+' (' + sLineBreak;
  for Col in Columns do begin
    Result := Result + #9 + Col.SQLCode;
    if Col <> Columns.Last then
      Result := Result + ',';
    Result := Result + sLineBreak;
  end;
  Result := Result + ')' + sLineBreak;
end;


procedure TfrmCsvDetector.btnSaveClick(Sender: TObject);
var
  rx: TRegExpr;
  TableName, Quote: String;
begin
  // Run code
  try
    Screen.Cursor := crHourGlass;
    FConnection.Query(SynMemoCreateTable.Text);
    ModalResult := mrOk;
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    Quote := QuoteRegExprMetaChars(FConnection.QuoteChar);
    rx.Expression := '^\s*CREATE\s+TABLE\s+' + Quote + '[^'+Quote+']+' + Quote + '.' + Quote + '([^'+Quote+']+)' + Quote;
    Mainform.LogSQL(rx.Expression, lcDebug);
    if rx.Exec(SynMemoCreateTable.Text) then
      TableName := rx.Match[1]
    else
      TableName := '';
    Mainform.LogSQL(TableName, lcDebug);
    FLoadDataFrm.comboTablePopulate(TableName, True);
    Screen.Cursor := crDefault;
  except
    on E:EDbError do begin
      Screen.Cursor := crDefault;
      ErrorDialog(E.Message);
      ModalResult := mrNone;
    end;
  end;
end;


end.
