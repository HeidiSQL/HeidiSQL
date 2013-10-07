unit SynDBExplorerFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, ComCtrls, Types, ShellAPI, Menus,
  SynCommons, SQLite3Commons, SynDB,
  SQLite3i18n, SQLite3UI, SQLite3UILogin, SQLite3ToolBar, SQLite3Pages;

type
  TDBExplorerFrame = class(TFrame)
    Splitter1: TSplitter;
    PanelClient: TPanel;
    Splitter2: TSplitter;
    DrawGrid: TDrawGrid;
    PanelTop: TPanel;
    MemoSQL: TMemo;
    BtnExec: TButton;
    BtnExport: TButton;
    PopupMenuSQL: TPopupMenu;
    MenuInsertFieldName: TMenuItem;
    MenuInsertFieldValues: TMenuItem;
    BtnExecExport: TButton;
    PagesLeft: TPageControl;
    TabTables: TTabSheet;
    TabObjects: TTabSheet;
    EditTable: TEdit;
    ListTable: TListBox;
    ImageLogo: TImage;
    BtnQueryBuilder: TButton;
    BtnTablesExport: TButton;
    BtnLog: TButton;
    procedure EditTableChange(Sender: TObject);
    procedure ListTableDblClick(Sender: TObject);
    procedure BtnExecClick(Sender: TObject);
    procedure ListTableMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnExportClick(Sender: TObject);
    procedure PopupMenuSQLPopup(Sender: TObject);
    procedure ImageLogoClick(Sender: TObject);
    procedure BtnQueryBuilderClick(Sender: TObject);
    procedure ListTableClick(Sender: TObject);
    procedure BtnTablesExportClick(Sender: TObject);
    procedure BtnLogClick(Sender: TObject);
  private
    fHint: THintWindowDelayed;
    fGrid: TSQLTableToGrid;
    fJSONBuffer: RawUTF8;
    fPreviousSQL: RawUTF8;
    fSQLLogFile: TFileName;
    function OnText(Sender: TSQLTable; FieldIndex, RowIndex: Integer; var Text: string): boolean;
    function GetTableDescription(const TableName: string): string;
    procedure OnGridDblClick(Sender: TObject);
    procedure InsertMenu(Sender: TObject);
    function GridValue(Row,Col: integer): RawUTF8;
    function GetFileNameFromCurrentSelectedTable: TFileName;
    procedure AddSQL(SQL: string; AndExec: boolean);
    procedure LogClick(Sender: TObject);
    procedure LogDblClick(Sender: TObject);
  public
    Tables: TStringList;
    Props: TSQLDBConnectionProperties;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

resourcestring
  sCaptionStats = '%d rows (%s) in %s';
  sCaptionUpdated = '%d row(s) updated in %s';
  sNothingToExport = 'No result to export';
  sExportFilter = 'Excel file (*.csv)|*.csv|Text file (*.txt)|*.txt|'+
    'JSON file (*.json)|*.json|Acrobat PDF report (*.pdf)|*.pdf';
  sHugeFileExport = 'Do you want to export so much data?';
  sExportingN = 'Exporting %s...';
  sHugeFileConfirm = 'This could take a while to create the report';
  sExecExportFilter = 'Excel file (*.csv)|*.csv|Text file (*.txt)|*.txt|'+
    'Standard JSON file (*.json)|*.json|Smaller JSON file (*.json)|*.json|'+
    'SQLite3 database file (*.db3)|*.db3|'+
    'BigTable fixed sized record (*.rec)|*.rec|'+
    'BigTable variable length record (*.rec)|*.rec';
  sTableExportFilter = 'SQLite3 database file (*.db3)|*.db3';

implementation

uses
  SynDBExplorerMain, SynDBExplorerQueryBuilder, SynDBExplorerExportTables,
  SynTaskDialog, SynBigTable, SynDBSQLite3;

{$R *.dfm}

function RowsToSynBigTableRecord(const Dest: TFileName; Rows: TSQLDBStatement;
  VariableLength: boolean): integer;
const
  TOFIELD: array[Boolean,TSQLDBFieldType] of TSynTableFieldType = (
    (tftUnknown,tftUnknown,tftInt64,tftDouble,tftCurrency,tftDouble,tftUTF8,tftBlobInternal),
    (tftUnknown,tftUnknown,tftVarInt64,tftDouble,tftCurrency,tftDouble,tftUTF8,tftBlobInternal));
var F, FMax: integer;
    BT: TSynBigTableRecord;
    Blob: RawByteString;
    FT: TSynTableFieldType;
    ColName: RawUTF8;
    ColRowID: integer;
    FieldsMap: array of Integer; // column order vary depending on access speed
    FieldsValue: TRawByteStringDynArray;
begin
  result := 0;
  if (Dest='') or (Rows=nil) or (Rows.ColumnCount=0) then
    exit;
  FMax := Rows.ColumnCount-1;
  DeleteFile(Dest);
  SetLength(FieldsValue,FMax+1);
  BT := TSynBigTableRecord.Create(Dest,'SynDbExported');
  try
    while Rows.Step do begin
      if result=0 then begin
        // retrieve column layout (when first row of data is available)
        ColRowID := -1; 
        for F := 0 to FMax do begin
          ColName := Rows.ColumnName(F);
          FT := TOFIELD[VariableLength,Rows.ColumnType(F)];
          if FT=tftUnknown then
            raise Exception.CreateFmt('Invalid column type %s',[ColName]);
          if IsRowID(pointer(ColName)) then begin
            ColName := 'ID__'; // force accepted column name
            ColRowID := F;
          end;
          if not BT.AddField(ColName,FT) then
            raise Exception.CreateFmt('Impossible to add column %s',[ColName]);
        end;
        BT.AddFieldUpdate;
        if BT.Table.FieldCount<>FMax+1 then
          raise Exception.Create('Invalid column layout');
        SetLength(FieldsMap,FMax+1);
        for F := 0 to FMax do begin
          ColName := BT.Table.Field[F].Name;
          if (ColRowID>=0) and (ColName='ID__') then
            FieldsMap[F] := ColRowID else
            FieldsMap[F] := Rows.ColumnIndex(ColName);
          if FieldsMap[F]<0 then
            raise Exception.CreateFmt('Invalid column type %s',[ColName]);
        end;
      end;
      // recreate each record from scratch (fast in practice)
      for F := 0 to FMax do
      with BT.Table.Field[F] do begin
        case FieldType of
        tftInt64, tftVarInt64:
          FieldsValue[F] := SBF(Rows.ColumnInt(FieldsMap[F]));
        tftDouble:
          FieldsValue[F] := SBF(Rows.ColumnDouble(FieldsMap[F]));
        tftCurrency:
          FieldsValue[F] := SBFCurr(Rows.ColumnCurrency(FieldsMap[F]));
        tftUTF8:
          FieldsValue[F] := SBF(Rows.ColumnUTF8(FieldsMap[F]));
        tftBlobInternal: begin
          Blob := Rows.ColumnBlob(FieldsMap[F]);
          FieldsValue[F] := SBF(pointer(Blob),length(Blob));
        end;
        end;
      end;
      BT.Add(RawByteStringArrayConcat(FieldsValue));
      if BT.CurrentInMemoryDataSize>$5000000 then // write on disk every 80 MB
        BT.UpdateToFile;
      inc(result);
    end;
    assert(result=BT.Count);
  finally
    BT.Free;
  end;
end;

procedure TDBExplorerFrame.EditTableChange(Sender: TObject);
var s: string;
    i: integer;
begin
  s := SysUtils.UpperCase(SysUtils.trim(EditTable.Text));
  with ListTable.Items do
  try
    BeginUpdate;
    Clear;
    for i := 0 to Tables.Count-1 do
      if (s='') or (Pos(s,SysUtils.UpperCase(Tables[i]))>0) then
        Add(Tables[i]);
  finally
    EndUpdate;
  end;
  ListTableClick(nil);
end;

procedure TDBExplorerFrame.AddSQL(SQL: string; AndExec: boolean);
var len: integer;
    orig: string;
begin
  fHint.Hide;
  SQL := SysUtils.Trim(SQL);
  len := Length(SQL);
  if len=0 then
    exit;
  orig := MemoSQL.Lines.Text;
  if orig<>'' then
    SQL := #13#10#13#10+SQL;
  SQL := orig+SQL;
  MemoSQL.Lines.Text := SQL;
  MemoSQL.SelStart := length(SQL)-len;
  MemoSQL.SelLength := len;
  if AndExec then
    BtnExecClick(nil) else
    MemoSQL.SetFocus;
end;

procedure TDBExplorerFrame.ListTableDblClick(Sender: TObject);
var i: integer;
    s: string;
begin
  i := ListTable.ItemIndex;
  if i<0 then
    exit;
  s := ListTable.Items[i];
  if (Pos(' ',s)>0) or
     ((Pos('.',s)>0) and Props.InheritsFrom(TSQLDBSQLite3ConnectionProperties)) then
    s := '`'+s+'`';
  AddSQL('select * from '+s,true);
end;

procedure ShowException(E: Exception);
var Dlg: TTaskDialog;
    stack: string;
begin
  if E=nil then
    exit;
  Dlg.Content := E.Message;
  {$ifdef UNICODE}
  stack := sLineBreak+E.StackTrace;
  {$endif}
  Dlg.Info := Format('Exception class: %s%s',[E.ClassName,stack]);
  Dlg.Execute([],0,[],tiError);
end;

procedure TDBExplorerFrame.BtnExecClick(Sender: TObject);
var SQL, Stop: RawUTF8;
    Table: TSQLTable;
    Timer: TPrecisionTimer;
    SelStart, SelLength, RowsCount, Kind: integer;
    Rows: ISQLDBRows;
    RowsSize: Int64;
    FN: TFileName;
    FS: TFileStream;
begin
  FreeAndNil(fGrid);
  DrawGrid.RowCount := 0;
  SelStart := MemoSQL.SelStart;
  SelLength := MemoSQL.SelLength;
  if SelLength>10 then
    SQL := Trim(S2U(MemoSQL.SelText)) else
    SQL := Trim(S2U(MemoSQL.Lines.Text));
  Screen.Cursor := crSQLWait;
  Timer.Start;
  try
    try
      Caption := '';
      RowsCount := 0;
      RowsSize := 0;
      if SQL<>'' then
        if isSelect(Pointer(SQL)) then begin
          try
            Rows := Props.Execute(SQL,[]);
          except
            on Exception do
            if Props.InheritsFrom(TSQLDBSQLite3ConnectionProperties) then
              // SQLite3 engine is local -> so any failure is fatal
              raise else begin
              // DB error (loose remote connection?) -> retry once
              Props.ClearConnectionPool;
              Rows := Props.Execute(SQL,[]);
            end;
          end;
          if (Sender=BtnExec) or (Sender=nil) then begin
            fJSONBuffer := Rows.FetchAllAsJSON(false);
            Stop := Timer.Stop;
            Table := TSQLTableJSON.Create([],'',pointer(fJSONBuffer),length(fJSONBuffer));
            fGrid := TSQLTableToGrid.Create(DrawGrid,Table,nil);
            fGrid.OnValueText := OnText;
            fGrid.SetFieldFixedWidth(100);
            fGrid.FieldTitleTruncatedNotShownAsHint := true;
            DrawGrid.Options := DrawGrid.Options-[goRowSelect];
            DrawGrid.OnDblClick := self.OnGridDblClick;
            RowsCount := Table.RowCount;
            RowsSize := length(fJSONBuffer);
          end else
          if Sender=BtnExecExport then begin
            Timer.Pause;
            with TSaveDialog.Create(self) do
            try
              InitialDir := GetShellFolderPath(CSIDL_DOCUMENTS);
              Filter := sExecExportFilter; // csv,txt,json,json,record,record
              DefaultExt := '.csv';
              FilterIndex := 0;
              Title := BtnExecExport.Hint;
              Options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist,ofEnableSizing];
              FileName := GetFileNameFromCurrentSelectedTable;
              if not Execute then
                exit;
              Kind := FilterIndex;
              FN := FileName;
            finally
              Free;
            end;
            Timer.Resume;
            with CreateTempForm(format(sExportingN,[GetFileNameFromCurrentSelectedTable]),nil,True) do
            try
              case Kind of
                5: Rowscount := RowsToSQLite3(FN,S2U(GetFileNameFromCurrentSelectedTable),Rows.Instance,false);
                6: RowsCount := RowsToSynBigTableRecord(FN,Rows.Instance,False); // fixed length
                7: RowsCount := RowsToSynBigTableRecord(FN,Rows.Instance,True);  // variable length
              else begin
                FS := TFileStream.Create(FN,fmCreate);
                try
                  case Kind of
                  1: RowsCount := Rows.Instance.FetchAllToCSVValues(FS,False,AnsiChar(ListSeparator),true);
                  2: RowsCount := Rows.Instance.FetchAllToCSVValues(FS,true,#9,true);
                  3: RowsCount := Rows.Instance.FetchAllToJSON(FS,true);  // expanded=true
                  4: RowsCount := Rows.Instance.FetchAllToJSON(FS,false); // expanded=false
                  end;
                finally
                  FS.Free;
                end;
              end;
              end;
              Stop := Timer.Stop;
            finally
              Free;
            end;
            RowsSize := FileSize(FN);
          end;
          Caption := format(sCaptionStats,
            [RowsCount,Ansi7ToString(KB(RowsSize)),Ansi7ToString(Stop)]);
        end else
          Caption := Format(sCaptionUpdated,[Props.ExecuteNoResult(SQL,[]),
            Ansi7ToString(Timer.Stop)]);
      (Parent as TTabSheet).Caption := Caption;
      MemoSQL.SelStart := SelStart;
      MemoSQL.SelLength := SelLength;
      MemoSQL.SetFocus;
      if SQL<>fPreviousSQL then begin
        AppendToTextFile(SQL,fSQLLogFile);
        fPreviousSQL := SQL;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowException(E);
  end;
end;

function TDBExplorerFrame.GetTableDescription(const TableName: string): string;
var Fields: TRawUTF8DynArray;
begin
  Screen.Cursor := crSQLWait;
  try
    Props.GetFieldDefinitions(S2U(TableName),Fields,true);
    result := TableName+#13#10'  '+U2S(RawUTF8ArrayToCSV(Fields,#13#10'  '));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDBExplorerFrame.ListTableMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if Button=mbRight then begin
    i := ListTable.ItemIndex;
    if i>=0 then begin
      if (fHint.Tag=i) and fHint.Visible then begin
        fHint.Hide;
        exit;
      end;
      fHint.ShowDelayedString(GetTableDescription(ListTable.Items[i]),
        ListTable,X,Y,5000,clNavy,true);
      fHint.Tag := i;
    end;
  end;
end;

procedure TDBExplorerFrame.BtnQueryBuilderClick(Sender: TObject);
var SQL: string;
    O: TDBQueryObject;
begin
  case TDBQueryBuilderForm.BuildQuery(ListTable,Props,SQL) of
    mrOk:  AddSQL(SQL,false);  // Use SQL
    mrYes: AddSQL(SQL,True);   // Exec SQL
    mrRetry: begin // To Object (SQL=Object serialization)
      O.FromIniSection(StringToUTF8(SQL));
      assert(O.AsIniSection=SQL);
    end;
  end;
end;

constructor TDBExplorerFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fHint := THintWindowDelayed.Create(self);
  fSQLLogFile := ChangeFileExt(paramstr(0),'.history');
  PagesLeft.ActivePageIndex := 0;
end;

destructor TDBExplorerFrame.Destroy;
begin
  FreeAndNil(fGrid);
  inherited;
end;

function TDBExplorerFrame.OnText(Sender: TSQLTable; FieldIndex,
  RowIndex: Integer; var Text: string): boolean;
begin
  if RowIndex=0 then begin
    Text := U2S(Sender.GetU(RowIndex,FieldIndex)); // display true column name
    result := true;
  end else
    result := false;
end;

procedure TDBExplorerFrame.BtnExportClick(Sender: TObject);
var F: TStream;
    Rep: TGdiPages;
    Form: TCustomForm;
    TableName: string;
begin
  Form := Application.MainForm;
  if (fGrid=nil) or (fGrid.Table.RowCount=0) then
    ShowMessage(sNothingToExport,true) else
  with TSaveDialog.Create(Form) do
  try
    InitialDir := GetShellFolderPath(CSIDL_DOCUMENTS);
    DefaultExt := 'csv';
    Filter := sExportFilter; // csv,txt,json,pdf
    FilterIndex := 0;
    Title := BtnExport.Hint;
    Options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist,ofEnableSizing];
    TableName := GetFileNameFromCurrentSelectedTable;
    FileName := TableName;
    if not Execute then
      exit;
    with CreateTempForm(format(sExportingN,[TableName]),nil,True) do
    try
      if FilterIndex=4 then begin
        if fGrid.Table.RowCount>10000 then
          if YesNo(sHugeFileConfirm,sHugeFileExport,false,true)=mrNo then
            exit;
        Rep := TGDIPages.Create(Application);
        try
          Rep.BeginDoc;
          Rep.Font.Size := 11;
          Rep.DrawTitle(SysUtils.Trim(Form.Caption));
          Rep.NewLine;
          Rep.Font.Style := [fsBold];
          Rep.DrawText(MemoSQL.Text);
          Rep.Font.Style := [];
          Rep.Font.Size := 9;
          Rep.DrawText(self.Caption);
          Rep.NewLine;
          if ListTable.ItemIndex>=0 then begin
            Rep.DrawText(GetTableDescription(TableName));
            Rep.NewLine;
          end;
          Rep.WordWrapLeftCols := true;
          TSQLRibbon(nil).AddToReport(Rep,fGrid.Table,[]);
          Rep.EndDoc;
          Rep.Caption := GetFileNameWithoutExt(ExtractFileName(FileName));
          ExeVersionRetrieve;
          Rep.ExportPDFAuthor := U2S(ExeVersion.User);
          Rep.ExportPDFApplication := Form.Caption;
          Rep.ExportPDFSubject := BtnExport.Hint;
          Rep.ExportPDFKeywords := MemoSQL.Text;
          Rep.ExportPDF(FileName,True,false)
        finally
          Rep.Free;
        end;
      end else begin
        F := TFileStream.Create(FileName,fmCreate);
        try
          case FilterIndex of
          1: fGrid.Table.GetCSVValues(F,false,AnsiChar(ListSeparator),true);
          2: fGrid.Table.GetCSVValues(F,true,#9,true);
          3: fGrid.Table.GetJSONValues(F,true);
          end;
        finally
          F.Free;
        end;
      end;
    finally
      Screen.Cursor := crDefault;
      Free;
    end;
    ShellExecute(Form.Handle,nil,pointer(FileName),nil,nil,SW_SHOWNORMAL);
  finally
    Free;
  end;
end;

function TDBExplorerFrame.GridValue(Row, Col: integer): RawUTF8;
begin
  result := fGrid.Table.GetU(Row,Col);
  if Row>0 then
  case fGrid.Table.FieldType(Col,nil) of
  sftAnsiText, sftUTF8Text, sftObject:
    result := QuotedStr(result);
  sftDateTime:
    result := Props.SQLIso8601ToDate(result);
  sftTimeLog, sftModTime, sftCreateTime:
    result := Props.SQLIso8601ToDate(fGrid.Table.GetTimeLog(Row,Col,true,' '));
  sftBlob, sftBlobDynArray:
    result := ''; // BLOB won't work in SQL without parameter binding
  end;
end;

procedure TDBExplorerFrame.OnGridDblClick(Sender: TObject);
var R,C: integer;
    sql: string;
    sel: boolean;
    selStart: integer;
begin
  R := DrawGrid.Row;
  C := DrawGrid.Col;
  if (R<=0) or (fGrid.Table=nil) then
    exit;
  sel := MemoSQL.SelLength>5;
  if sel then
    sql := MemoSQL.SelText else
    sql := MemoSQL.Text;
  if Pos(' WHERE ',SysUtils.UpperCase(sql))=0 then
    sql := sql+' where ' else
    sql := sql+' and ';
  sql := sql+fGrid.Table.GetString(0,C)+'='+U2S(GridValue(R,C));
  if sel then begin
    selStart := MemoSQL.SelStart;
    MemoSQL.SelText := sql;
    MemoSQL.SelStart := selStart;
    MemoSQL.SelLength := length(sql);
    MemoSQL.SetFocus;
  end else
    MemoSQL.Text := sql;
end;

procedure TDBExplorerFrame.PopupMenuSQLPopup(Sender: TObject);
  procedure Add(M: TMenuItem; Row: integer);
    function New(caption: string; Col: integer=0; csv: PString=nil): TMenuItem;
    begin
      caption := SysUtils.trim(caption);
      if caption<>'' then begin
        result := TMenuItem.Create(self);
        result.Caption := Caption;
        result.Hint := ' '+Caption;
        result.OnClick := InsertMenu;
        M.Add(result);
      end else begin
        result := nil;
        caption := 'null';
      end;
      if csv<>nil then begin
        if csv^<>'' then
          csv^ := csv^+',';
        csv^ := csv^+caption;
      end;
    end;
  var i: integer;
      csv: string;
  begin
    M.Clear;
    csv := '';
    if (Row>=0) and (fGrid<>nil) and (fGrid.Table<>nil) then
      with fGrid.Table do
        for i := 0 to FieldCount-1 do
          New(U2S(GridValue(Row,i)),i,@csv);
    M.Enabled := M.Count>0;
    if M.Enabled then begin
      New('-');
      New(csv).Caption := M.Hint;
    end;
  end;
begin
  Add(MenuInsertFieldName,0);
  Add(MenuInsertFieldValues,DrawGrid.Row);    
end;

procedure TDBExplorerFrame.InsertMenu(Sender: TObject);
var Ins: string;
begin
  if Sender.InheritsFrom(TMenuItem) then begin
    Ins := TMenuItem(Sender).Hint;
    MemoSQL.SelText := Ins;
    MemoSQL.SelLength := length(Ins);
  end;
end;

procedure TDBExplorerFrame.ImageLogoClick(Sender: TObject);
begin
{$WARNINGS OFF}
  if DebugHook=0 then
    ShellExecute(0,nil,'http://synopse.info',nil,nil,SW_SHOWNORMAL);
{$WARNINGS ON}
end;

function TDBExplorerFrame.GetFileNameFromCurrentSelectedTable: TFileName;
begin
  if (ListTable.Count=0) or (ListTable.ItemIndex<0) then
    result := 'Export' else
    result := ListTable.Items[ListTable.ItemIndex];
end;


procedure TDBExplorerFrame.ListTableClick(Sender: TObject);
var MultiSel: boolean;
begin
  MultiSel := ListTable.SelCount>0;
  BtnQueryBuilder.Enabled := MultiSel;
  BtnTablesExport.Enabled := MultiSel;
end;

procedure TDBExplorerFrame.BtnTablesExportClick(Sender: TObject);
var RowsCount: integer;
    Timer: TPrecisionTimer;
    aFileName: TFileName;
begin
  with TSaveDialog.Create(self) do
  try
    InitialDir := GetShellFolderPath(CSIDL_DOCUMENTS);
    Filter := sTableExportFilter;
    DefaultExt := '.db3';
    FilterIndex := 0;
    Title := BtnTablesExport.Hint;
    Options := [ofOverwritePrompt,ofHideReadOnly,ofPathMustExist,ofEnableSizing];
    FileName := U2S(Props.ServerName+'_'+NowToString(false,'_'));
    if not Execute then
      exit;
    aFileName := FileName;
  finally
    Free;
  end;
  Timer.Start;
  RowsCount := TDBExportTablesForm.ExportTables(ListTable,Props,aFileName);
  (Parent as TTabSheet).Caption := format(sCaptionStats,
    [RowsCount,ExtractFileName(aFileName),Ansi7ToString(Timer.Stop)]);
end;

procedure TDBExplorerFrame.BtnLogClick(Sender: TObject);
const MAX_LINES_IN_HISTORY = 500;
var F: TForm;
    List: TListBox;
    Details: TMemo;
    i: integer;
begin
  F := TForm.Create(Application);
  try
    F.Caption := ' '+BtnLog.Hint;
    F.Font := Font;
    F.Width := 800;
    F.Height := Screen.Height-80;
    F.Position := poMainFormCenter;
    Details := TMemo.Create(F);
    Details.Parent := F;
    Details.Align := alBottom;
    Details.Height := 200;
    Details.ReadOnly := true;
    List := TListBox.Create(F);
    with List do begin
      Parent := F;
      Align := alClient;
      Tag := PtrInt(Details);
      Items.BeginUpdate;
      with TMemoryMapText.Create(fSQLLogFile) do
      try
        for i := Count-1 downto 0 do
          if Items.Add(Strings[i])>MAX_LINES_IN_HISTORY then
            break; // read last 500 lines from UTF-8 file
      finally
        Free;
        Items.EndUpdate;
      end;
      ItemIndex := 0;
      LogClick(List);
      OnClick := LogClick;
      OnDblClick := LogDblClick;
    end;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TDBExplorerFrame.LogClick(Sender: TObject);
var List: TListBox absolute Sender;
    ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx>=0 then
    TMemo(List.Tag).Text := copy(List.Items[ndx],21,maxInt);
end;

procedure TDBExplorerFrame.LogDblClick(Sender: TObject);
var List: TListBox absolute Sender;
    SQL: string;
    ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx>=0 then begin
    SQL := copy(List.Items[ndx],21,maxInt);
    AddSQL(SQL,IsSelect(pointer(StringToAnsi7(SQL))));
    TForm(List.Owner).Close;
  end;
end;

end.

