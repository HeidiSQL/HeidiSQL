unit exportgrid;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.ComCtrls, VirtualTrees, SynExportHTML, gnugettext, Vcl.ActnList,
  extra_controls, dbstructures, SynRegExpr, System.StrUtils, System.IOUtils, VirtualTrees.BaseTree, VirtualTrees.Types;

type
  TGridExportFormat = (
    efExcel,
    efCSV,
    efHTML,
    efXML,
    efSQLInsert,
    efSQLInsertIgnore,
    efSQLReplace,
    efSQLDeleteInsert,
    efSQLUpdate,
    efLaTeX,
    efTextile,
    efJiraTextile,
    efPHPArray,
    efMarkDown,
    efJSON,
    efJSONLines
    );

  TfrmExportGrid = class(TExtForm)
    btnOK: TButton;
    btnCancel: TButton;
    grpSelection: TRadioGroup;
    grpOutput: TGroupBox;
    radioOutputCopyToClipboard: TRadioButton;
    radioOutputFile: TRadioButton;
    editFilename: TButtonedEdit;
    grpOptions: TGroupBox;
    chkIncludeColumnNames: TCheckBox;
    editSeparator: TButtonedEdit;
    editEncloser: TButtonedEdit;
    editTerminator: TButtonedEdit;
    lblSeparator: TLabel;
    lblEncloser: TLabel;
    lblTerminator: TLabel;
    popupCSVchar: TPopupMenu;
    menuCSVtab: TMenuItem;
    menuCSVunixlinebreak: TMenuItem;
    menuCSVmaclinebreak: TMenuItem;
    menuCSVwinlinebreak: TMenuItem;
    menuCSVnul: TMenuItem;
    menuCSVbackspace: TMenuItem;
    menuCSVcontrolz: TMenuItem;
    comboEncoding: TComboBox;
    lblEncoding: TLabel;
    popupRecentFiles: TPopupMenu;
    menuCSVsinglequote: TMenuItem;
    menuCSVdoublequote: TMenuItem;
    menuCSVcomma: TMenuItem;
    menuCSVsemicolon: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    chkIncludeAutoIncrement: TCheckBox;
    chkIncludeQuery: TCheckBox;
    lblNull: TLabel;
    editNull: TButtonedEdit;
    btnSetClipboardDefaults: TButton;
    chkRemoveLinebreaks: TCheckBox;
    grpFormat: TGroupBox;
    comboFormat: TComboBoxEx;
    procedure FormCreate(Sender: TObject);
    procedure CalcSize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure editFilenameRightButtonClick(Sender: TObject);
    procedure editFilenameChange(Sender: TObject);
    procedure popupRecentFilesPopup(Sender: TObject);
    procedure menuCSVClick(Sender: TObject);
    procedure editCSVRightButtonClick(Sender: TObject);
    procedure editCSVChange(Sender: TObject);
    procedure ValidateControls(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure comboFormatSelect(Sender: TObject);
    procedure btnSetClipboardDefaultsClick(Sender: TObject);
  private
    { Private declarations }
    FCSVEditor: TButtonedEdit;
    FCSVSeparator, FCSVEncloser, FCSVTerminator, FCSVNull: String;
    FGrid: TVirtualStringTree;
    FRecentFiles: TStringList;
    FHiddenCopyMode: Boolean;
    procedure SaveDialogTypeChange(Sender: TObject);
    function GetExportFormat: TGridExportFormat;
    procedure SetExportFormat(Value: TGridExportFormat);
    procedure SetExportFormatByFilename;
    procedure SelectRecentFile(Sender: TObject);
    procedure PutFilenamePlaceholder(Sender: TObject);
    function FormatExcelCsv(Text, Encloser: String; DataType: TDBDatatype): String;
    function FormatJson(Text: String): String;
    function FormatPhp(Text: String): String;
    function FormatLatex(Text: String): String;
  public
    { Public declarations }
    const FormatToFileExtension: Array[TGridExportFormat] of String =
      (
        ('csv'),
        ('csv'),
        ('html'),
        ('xml'),
        ('sql'),
        ('sql'),
        ('sql'),
        ('sql'),
        ('sql'),
        ('LaTeX'),
        ('textile'),
        ('jira-textile'),
        ('php'),
        ('md'),
        ('json'),
        ('jsonl')
        );
    const FormatToDescription: Array[TGridExportFormat] of String =
      (
        ('Excel CSV'),
        ('Delimited text'),
        ('HTML table'),
        ('XML'),
        ('SQL INSERTs'),
        ('SQL INSERT IGNOREs'),
        ('SQL REPLACEs'),
        ('SQL DELETEs/INSERTs'),
        ('SQL UPDATEs'),
        ('LaTeX'),
        ('Textile'),
        ('Jira Textile'),
        ('PHP Array'),
        ('Markdown Here'),
        ('JSON'),
        ('JSON Lines')
        );
    const FormatToImageIndex: Array[TGridExportFormat] of Integer =
      (
        49,  // Excel
        50,  // CSV
        32,  // HTML
        48,  // XML
        201, // SQL
        201, // SQL
        201, // SQL
        201, // SQL
        201, // SQL
        153, // Latex
        154, // Textile
        154, // Jira
        202, // PHP
        199, // Markdown
        200, // JSON
        200  // JSON Lines
      );
    const CopyAsActionPrefix = 'actCopyAs';
    property Grid: TVirtualStringTree read FGrid write FGrid;
    property ExportFormat: TGridExportFormat read GetExportFormat write SetExportFormat;
  end;


implementation

uses main, apphelpers, dbconnection;

{$R *.dfm}



procedure TfrmExportGrid.FormCreate(Sender: TObject);
var
  ef: TGridExportFormat;
  SenderName: String;
  comboItem: TComboExItem;
begin
  HasSizeGrip := True;
  editFilename.Text := AppSettings.ReadString(asGridExportFilename);
  FRecentFiles := Explode(DELIM, AppSettings.ReadString(asGridExportRecentFiles));
  comboEncoding.Items.Assign(MainForm.FileEncodings);
  comboEncoding.Items.Delete(0); // Remove "Auto detect"
  comboEncoding.ItemIndex := AppSettings.ReadInt(asGridExportEncoding);
  comboFormat.Items.Clear;
  for ef:=Low(TGridExportFormat) to High(TGridExportFormat) do begin
    comboItem := TComboExItem.Create(comboFormat.ItemsEx);
    comboItem.Caption := FormatToDescription[ef];
    comboItem.ImageIndex := FormatToImageIndex[ef];
  end;
  SenderName := Owner.Name;
  FHiddenCopyMode := SenderName.StartsWith(CopyAsActionPrefix);

  if FHiddenCopyMode then begin
    radioOutputCopyToClipboard.Checked := True;
    comboFormat.ItemIndex := Owner.Tag;
    grpSelection.ItemIndex := 0; // Always use selected cells in copy mode
    chkIncludeColumnNames.Checked := AppSettings.ReadBool(asGridExportClpColumnNames);
    chkIncludeAutoIncrement.Checked := AppSettings.ReadBool(asGridExportClpIncludeAutoInc);
    chkIncludeQuery.Checked := False; // Always off in copy mode
    chkRemoveLinebreaks.Checked := AppSettings.ReadBool(asGridExportClpRemoveLinebreaks);
    FCSVSeparator := AppSettings.ReadString(asGridExportClpSeparator);
    FCSVEncloser := AppSettings.ReadString(asGridExportClpEncloser);
    FCSVTerminator := AppSettings.ReadString(asGridExportClpTerminator);
    FCSVNull := AppSettings.ReadString(asGridExportClpNull);
  end else begin
    radioOutputCopyToClipboard.Checked := AppSettings.ReadBool(asGridExportOutputCopy);
    radioOutputFile.Checked := AppSettings.ReadBool(asGridExportOutputFile);
    comboFormat.ItemIndex := AppSettings.ReadInt(asGridExportFormat);
    grpSelection.ItemIndex := AppSettings.ReadInt(asGridExportSelection);
    chkIncludeColumnNames.Checked := AppSettings.ReadBool(asGridExportColumnNames);
    chkIncludeAutoIncrement.Checked := AppSettings.ReadBool(asGridExportIncludeAutoInc);
    chkIncludeQuery.Checked := AppSettings.ReadBool(asGridExportIncludeQuery);
    chkRemoveLinebreaks.Checked := AppSettings.ReadBool(asGridExportRemoveLinebreaks);
    FCSVSeparator := AppSettings.ReadString(asGridExportSeparator);
    FCSVEncloser := AppSettings.ReadString(asGridExportEncloser);
    FCSVTerminator := AppSettings.ReadString(asGridExportTerminator);
    FCSVNull := AppSettings.ReadString(asGridExportNull);
  end;
  ValidateControls(Sender);
end;


procedure TfrmExportGrid.FormShow(Sender: TObject);
begin
  // Show dialog. Expect "Grid" property to be set now by the caller.
  Width := AppSettings.ReadIntDpiAware(asGridExportWindowWidth, Self);
  Height := AppSettings.ReadIntDpiAware(asGridExportWindowHeight, Self);
  chkIncludeAutoIncrement.OnClick := CalcSize;
  CalcSize(Sender);
end;


procedure TfrmExportGrid.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Store settings
  AppSettings.WriteIntDpiAware(asGridExportWindowWidth, Self, Width);
  AppSettings.WriteIntDpiAware(asGridExportWindowHeight, Self, Height);
  if ModalResult = mrOK then begin
    AppSettings.WriteBool(asGridExportOutputCopy, radioOutputCopyToClipboard.Checked);
    AppSettings.WriteBool(asGridExportOutputFile, radioOutputFile.Checked);
    AppSettings.WriteString(asGridExportFilename, editFilename.Text);
    AppSettings.WriteString(asGridExportRecentFiles, Implode(DELIM, FRecentFiles));
    AppSettings.WriteInt(asGridExportEncoding, comboEncoding.ItemIndex);
    AppSettings.WriteInt(asGridExportFormat, comboFormat.ItemIndex);
    AppSettings.WriteInt(asGridExportSelection, grpSelection.ItemIndex);
    AppSettings.WriteBool(asGridExportColumnNames, chkIncludeColumnNames.Checked);
    AppSettings.WriteBool(asGridExportIncludeAutoInc, chkIncludeAutoIncrement.Checked);
    AppSettings.WriteBool(asGridExportIncludeQuery, chkIncludeQuery.Checked);
    AppSettings.WriteBool(asGridExportRemoveLinebreaks, chkRemoveLinebreaks.Checked);
    AppSettings.WriteString(asGridExportSeparator, FCSVSeparator);
    AppSettings.WriteString(asGridExportEncloser, FCSVEncloser);
    AppSettings.WriteString(asGridExportTerminator, FCSVTerminator);
    AppSettings.WriteString(asGridExportNull, FCSVNull);
  end;
end;


procedure TfrmExportGrid.ValidateControls(Sender: TObject);
var
  Enable: Boolean;
begin
  // Display the actually used control characters, even if they cannot be changed
  case ExportFormat of
    efExcel: begin
      // Tab for pasting, semicolon if comma is also the decimal separator, and comma for the rest
      // see http://en.wikipedia.org/wiki/Comma-separated_values
      if radioOutputCopyToClipboard.Checked then
        editSeparator.Text := '\t'
      else if FormatSettings.DecimalSeparator=',' then
        editSeparator.Text := ';'
      else
        editSeparator.Text := ',';
      editEncloser.Text := '"';
      editTerminator.Text := '\r\n';
      editNull.Text := FCSVNull;
    end;
    efCSV: begin
      editSeparator.Text := FCSVSeparator;
      editEncloser.Text := FCSVEncloser;
      editTerminator.Text := FCSVTerminator;
      editNull.Text := FCSVNull;
    end;
    efMarkDown:
      editNull.Text := FCSVNull;
    else begin
      editSeparator.Text := '';
      editEncloser.Text := '';
      editTerminator.Text := '';
      editNull.Text := '';
    end;
  end;

  chkIncludeQuery.Enabled := ExportFormat in [efHTML, efXML, efMarkDown, efJSON];
  Enable := ExportFormat = efCSV;
  lblSeparator.Enabled := Enable;
  editSeparator.Enabled := Enable;
  editSeparator.RightButton.Enabled := Enable;
  lblEncloser.Enabled := Enable;
  editEncloser.Enabled := Enable;
  editEncloser.RightButton.Enabled := Enable;
  lblTerminator.Enabled := Enable;
  editTerminator.Enabled := Enable;
  editTerminator.RightButton.Enabled := Enable;
  lblNull.Enabled := ExportFormat in [efExcel, efCSV, efMarkDown];
  editNull.Enabled := lblNull.Enabled;
  editNull.RightButton.Enabled := lblNull.Enabled;
  btnOK.Enabled := radioOutputCopyToClipboard.Checked or (radioOutputFile.Checked and (editFilename.Text <> ''));
  if radioOutputFile.Checked then
    editFilename.Font.Color := GetThemeColor(clWindowText)
  else
    editFilename.Font.Color := GetThemeColor(clGrayText);
  comboEncoding.Enabled := radioOutputFile.Checked;
  lblEncoding.Enabled := radioOutputFile.Checked;
end;


function TfrmExportGrid.GetExportFormat: TGridExportFormat;
begin
  // This is slow, don't use in large loops
  Result := TGridExportFormat(comboFormat.ItemIndex);
end;


procedure TfrmExportGrid.SetExportFormat(Value: TGridExportFormat);
begin
  comboFormat.ItemIndex := Integer(Value);
  ValidateControls(Self);
end;


procedure TfrmExportGrid.comboFormatSelect(Sender: TObject);
var
  Filename: String;
begin
  // Auto-modify file extension when selecting export format
  // Be careful about triggering editFilename.OnChange event, as we may have come here from that event!
  if radioOutputFile.Checked then begin
    Filename := ExtractFilePath(editFilename.Text) +
      TPath.GetFileNameWithoutExtension(editFilename.Text) +
      '.' + FormatToFileExtension[ExportFormat];
    if CompareText(Filename, editFilename.Text) <> 0 then
      editFilename.Text := Filename;
  end;
  ValidateControls(Sender);
end;


procedure TfrmExportGrid.SetExportFormatByFilename;
var
  ext: String;
  efrm: TGridExportFormat;
begin
  // Set format by file extension
  ext := LowerCase(Copy(ExtractFileExt(editFilename.Text), 2, 10));
  for efrm :=Low(TGridExportFormat) to High(TGridExportFormat) do begin
    if ext = FormatToFileExtension[ExportFormat] then
      break;
    if ext = FormatToFileExtension[efrm] then begin
      ExportFormat := efrm;
      break;
    end;
  end;
end;


procedure TfrmExportGrid.editFilenameChange(Sender: TObject);
begin
  radioOutputFile.Checked := True;
end;


procedure TfrmExportGrid.editFilenameRightButtonClick(Sender: TObject);
var
  Dialog: TSaveDialog;
  ef: TGridExportFormat;
  Filename: String;
begin
  // Select file target
  Dialog := TSaveDialog.Create(Self);
  Filename := GetOutputFilename(editFilename.Text, MainForm.ActiveDbObj);
  Dialog.InitialDir := ExtractFilePath(Filename);
  Dialog.FileName := TPath.GetFileNameWithoutExtension(Filename);
  Dialog.Filter := '';
  for ef:=Low(TGridExportFormat) to High(TGridExportFormat) do
    Dialog.Filter := Dialog.Filter + FormatToDescription[ef] + ' (*.'+FormatToFileExtension[ef]+')|*.'+FormatToFileExtension[ef]+'|';
  Dialog.Filter := Dialog.Filter + _('All files')+' (*.*)|*.*';
  Dialog.OnTypeChange := SaveDialogTypeChange;
  Dialog.FilterIndex := comboFormat.ItemIndex+1;
  Dialog.OnTypeChange(Dialog);
  if Dialog.Execute then begin
    editFilename.Text := Dialog.FileName;
    SetExportFormatByFilename;
  end;
  Dialog.Free;
end;


procedure TfrmExportGrid.popupRecentFilesPopup(Sender: TObject);
var
  Filename: String;
  Menu: TPopupMenu;
  Item: TMenuItem;
  Placeholders: TStringList;
  i: Integer;
begin
  // Clear and populate drop down menu with recent files and filename placeholders
  Menu := Sender as TPopupMenu;
  Menu.Items.Clear;

  for Filename in FRecentFiles do begin
    Item := TMenuItem.Create(Menu);
    Menu.Items.Add(Item);
    Item.Caption := Filename;
    Item.Hint := Filename;
    Item.OnClick := SelectRecentFile;
    Item.Checked := Filename = editFilename.Text;
  end;

  Item := TMenuItem.Create(Menu);
  Menu.Items.Add(Item);
  Item.Caption := '-';

  Placeholders := GetOutputFilenamePlaceholders;
  for i:=0 to Placeholders.Count-1 do begin
    Item := TMenuItem.Create(Menu);
    Menu.Items.Add(Item);
    Item.Caption := '%' + Placeholders.Names[i] + ': ' + Placeholders.ValueFromIndex[i];
    Item.Hint := '%' + Placeholders.Names[i];
    Item.OnClick := PutFilenamePlaceholder;
  end;

  Placeholders.Free;
end;


procedure TfrmExportGrid.SelectRecentFile(Sender: TObject);
begin
  // Select file from recently used files
  editFilename.Text := (Sender as TMenuItem).Hint;
  SetExportFormatByFilename;
end;


procedure TfrmExportGrid.PutFilenamePlaceholder(Sender: TObject);
begin
  // Put filename placeholder
  editFilename.SelText := (Sender as TMenuItem).Hint;
end;


procedure TfrmExportGrid.btnSetClipboardDefaultsClick(Sender: TObject);
begin
  // Store copy-to-clipboard settings
  AppSettings.ResetPath;
  AppSettings.WriteBool(asGridExportClpColumnNames, chkIncludeColumnNames.Checked);
  AppSettings.WriteBool(asGridExportClpIncludeAutoInc, chkIncludeAutoIncrement.Checked);
  AppSettings.WriteBool(asGridExportRemoveLinebreaks, chkRemoveLinebreaks.Checked);
  AppSettings.WriteString(asGridExportClpSeparator, FCSVSeparator);
  AppSettings.WriteString(asGridExportClpEncloser, FCSVEncloser);
  AppSettings.WriteString(asGridExportClpTerminator, FCSVTerminator);
  AppSettings.WriteString(asGridExportClpNull, FCSVNull);
  MessageDialog(_('Clipboard settings changed.'), mtInformation, [mbOK]);
end;


procedure TfrmExportGrid.CalcSize(Sender: TObject);
var
  GridData: TDBQuery;
  Node: PVirtualNode;
  Col, ExcludeCol: TColumnIndex;
  ResultCol: Integer;
  RowNum: PInt64;
  SelectedSize, AllSize: Int64;
  CalculatedCount, SelectedCount, AllCount: Int64;
begin
  GridData := Mainform.GridResult(Grid);
  AllSize := 0;
  SelectedSize := 0;
  chkIncludeAutoIncrement.Enabled := GridData.AutoIncrementColumn > -1;
  ExcludeCol := -1;
  if chkIncludeAutoIncrement.Enabled and (not chkIncludeAutoIncrement.Checked) then
    ExcludeCol := GridData.AutoIncrementColumn;

  Node := GetNextNode(Grid, nil, False);
  CalculatedCount := 0;
  AllCount := 0;
  SelectedCount := 0;
  while Assigned(Node) do begin
    Inc(AllCount);
    if vsSelected in Node.States then
      Inc(SelectedCount);

    if CalculatedCount < 1000 then begin
      // Performance: use first rows only, and interpolate the rest, see issue #804
      RowNum := Grid.GetNodeData(Node);
      GridData.RecNo := RowNum^;
      Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
      while Col > NoColumn do begin
        ResultCol := Col - 1;
        if Col <> ExcludeCol then begin
          Inc(AllSize, GridData.ColumnLengths(ResultCol));
          if vsSelected in Node.States then
            Inc(SelectedSize, GridData.ColumnLengths(ResultCol));
        end;
        Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
      end;
      Inc(CalculatedCount);
    end;

    Node := GetNextNode(Grid, Node, False);
  end;
  if AllCount > CalculatedCount then begin
    AllSize := Round(AllSize / CalculatedCount * AllCount);
  end;
  grpSelection.Items[0] := f_('Selection (%s rows, %s)', [FormatNumber(SelectedCount), FormatByteNumber(SelectedSize)]);
  grpSelection.Items[1] := f_('Complete (%s rows, %s)', [FormatNumber(AllCount), FormatByteNumber(AllSize)]);
end;


procedure TfrmExportGrid.editCSVChange(Sender: TObject);
var
  Edit: TButtonedEdit;
begin
  // Remember csv settings
  Edit := Sender as TButtonedEdit;
  case ExportFormat of
    efExcel, efMarkDown: begin
      if Edit = editNull then             FCSVNull := Edit.Text;
    end;
    efCSV: begin
      if Edit = editSeparator then        FCSVSeparator := Edit.Text
      else if Edit = editEncloser then    FCSVEncloser := Edit.Text
      else if Edit = editTerminator then  FCSVTerminator := Edit.Text
      else if Edit = editNull then        FCSVNull := Edit.Text;
    end;
  end;
end;


procedure TfrmExportGrid.SaveDialogTypeChange(Sender: TObject);
var
  Dialog: TSaveDialog;
  ef: TGridExportFormat;
begin
  // Set default file-extension of saved file and options on the dialog to show
  Dialog := Sender as TSaveDialog;
  for ef:=Low(TGridExportFormat) to High(TGridExportFormat) do begin
    if Dialog.FilterIndex = Integer(ef)+1 then
      Dialog.DefaultExt := FormatToFileExtension[ef];
  end;
end;


procedure TfrmExportGrid.editCSVRightButtonClick(Sender: TObject);
var
  p: TPoint;
  Item: TMenuItem;
begin
  // Remember editor and prepare popup menu items
  FCSVEditor := Sender as TButtonedEdit;
  p := FCSVEditor.ClientToScreen(FCSVEditor.ClientRect.BottomRight);
  for Item in popupCSVchar.Items do begin
    Item.Checked := FCSVEditor.Text = Item.Hint;
  end;
  popupCSVchar.Popup(p.X-16, p.Y);
end;


procedure TfrmExportGrid.menuCSVClick(Sender: TObject);
begin
  // Insert char from menu
  FCSVEditor.Text := TMenuItem(Sender).Hint;
end;


function TfrmExportGrid.FormatExcelCsv(Text, Encloser: String; DataType: TDBDatatype): String;
begin
  Result := Text;
  // Escape encloser characters inside data per de-facto CSV.
  if not Encloser.IsEmpty then
    Result := StringReplace(Result, Encloser, Encloser+Encloser, [rfReplaceAll]);
  // Remove milliseconds from date/time values, unsupported by Excel. See issue #922
  if DataType.Category = dtcTemporal then begin
    Result := ReplaceRegExpr('\.(\d+)$', Result, '');
  end;
end;


function TfrmExportGrid.FormatJson(Text: String): String;
begin
  // String escaping for PHP output. Incompatible to TDBConnection.EscapeString.
  Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := '"' + Result + '"';
end;

function TfrmExportGrid.FormatPhp(Text: String): String;
begin
  if Text.IndexOfAny([#10, #13, #9, #11, #27, #12]) > -1 then begin
    // https://www.php.net/manual/it/language.types.string.php#language.types.string.syntax.double
    Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
    Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
    Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
    Result := StringReplace(Result, #11, '\v', [rfReplaceAll]);
    Result := StringReplace(Result, #27, '\e', [rfReplaceAll]);
    Result := StringReplace(Result, #12, '\f', [rfReplaceAll]);
    Result := StringReplace(Result, '$', '\$', [rfReplaceAll]);
    Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
    Result := '"' + Result + '"';
  end else begin
    // https://www.php.net/manual/it/language.types.string.php#language.types.string.syntax.single
    Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);
    Result := StringReplace(Text, '''', '\''', [rfReplaceAll]);
    Result := '''' + Result + '''';
  end;
end;


function TfrmExportGrid.FormatLatex(Text: String): String;
var
  TextChr: Char;
const
  NeedBackslash: TSysCharset = ['_', '$', '%', '&'];
begin
  // String escaping for LaTeX output. Mostly uses backslash. Probably incomplete.
  // See pm from H. Flick
  // See https://tex.stackexchange.com/a/301984
  Result := Text;
  for TextChr in NeedBackslash do begin
    Result := StringReplace(Result, TextChr, '\'+TextChr, [rfReplaceAll]);
  end;
end;


procedure TfrmExportGrid.btnOKClick(Sender: TObject);
var
  Col, ExcludeCol: TColumnIndex;
  ResultCol: Integer;
  Header, Data, tmp, Encloser, Separator, Terminator, TableName, Filename: String;
  Node: PVirtualNode;
  GridData: TDBQuery;
  SelectionOnly, HasNulls: Boolean;
  i: Integer;
  NodeCount: Cardinal;
  RowNum: PInt64;
  HTML: TStream;
  S: TStringStream;
  Exporter: TSynExporterHTML;
  Encoding: TEncoding;
  Bom: TBytes;
  CurrentExportFormat: TGridExportFormat;
begin
  Filename := GetOutputFilename(editFilename.Text, MainForm.ActiveDbObj);

  // Confirmation dialog if file exists
  if radioOutputFile.Checked
    and FileExists(Filename)
    and (MessageDialog(_('File exists'), f_('Overwrite file %s?', [Filename]), mtConfirmation, [mbYes, mbCancel]) = mrCancel)
    then begin
      ModalResult := mrNone;
      Exit;
  end;

  try
    Screen.Cursor := crHourglass;

    SelectionOnly := grpSelection.ItemIndex = 0;
    Mainform.DataGridEnsureFullRows(Grid, SelectionOnly);
    GridData := Mainform.GridResult(Grid);
    if SelectionOnly then
      NodeCount := Grid.SelectedCount
    else
      NodeCount := Grid.RootNodeCount;
    MainForm.EnableProgress(NodeCount);
    try
      TableName := GridData.TableName;
    except
      TableName := _('UnknownTable');
    end;
    ExcludeCol := NoColumn;
    if (not chkIncludeAutoIncrement.Checked) or (not chkIncludeAutoIncrement.Enabled) then
      ExcludeCol := GridData.AutoIncrementColumn + 1;
    // Calling (Get)ExportFormat is slow, so we store it in a local variable
    CurrentExportFormat := ExportFormat;

    if radioOutputCopyToClipboard.Checked then
      Encoding := TEncoding.UTF8
    else begin
      Encoding := MainForm.GetEncodingByName(comboEncoding.Text);
      // Add selected file to file list, and sort it onto the top of the list
      i := FRecentFiles.IndexOf(editFilename.Text);
      if i > -1 then
        FRecentFiles.Delete(i);
      FRecentFiles.Insert(0, editFilename.Text);
      for i:=FRecentFiles.Count-1 downto 10 do
        FRecentFiles.Delete(i);
    end;

    // Prepare stream
    // Note that TStringStream + TEncoding.UTF8 do not write a BOM (which is nice),
    // although it should do so according to TUTF8Encoding.GetPreamble.
    // Now, only newer Excel versions need that BOM, so we add it explicitly here
    // P.S.: Note the boolean/False parameter for OwnsEncoding, so our global encodings are not destroyed after usage
    S := TStringStream.Create(Header, Encoding, False);
    if (CurrentExportFormat = efExcel) and (Encoding = TEncoding.UTF8) and radioOutputFile.Checked then begin
      Bom := TBytes.Create($EF, $BB, $BF);
      S.Write(Bom, 3);
    end;

    Header := '';
    case CurrentExportFormat of
      efHTML: begin
        Header :=
          '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" ' + sLineBreak +
          CodeIndent + '"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + sLineBreak + sLineBreak +
          '<html>' + sLineBreak +
          CodeIndent + '<head>' + sLineBreak +
          CodeIndent(2) + '<title>' + TableName + '</title>' + sLineBreak +
          CodeIndent(2) + '<meta name="GENERATOR" content="'+ APPNAME+' '+Mainform.AppVersion + '">' + sLineBreak +
          CodeIndent(2) + '<meta http-equiv="Content-Type" content="text/html; charset='+GetHTMLCharsetByEncoding(Encoding)+'" />' + sLineBreak +
          CodeIndent(2) + '<style type="text/css">' + sLineBreak +
          CodeIndent(3) + 'th, td {vertical-align: top;}' + sLineBreak +
          CodeIndent(3) + 'table, td {border: 1px solid silver; padding: 2px;}' + sLineBreak +
          CodeIndent(3) + 'table {border-collapse: collapse;}' + sLineBreak;
        Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
        while Col > NoColumn do begin
          // Right-justify all cells to match the grid on screen.
          if Grid.Header.Columns[Col].Alignment = taRightJustify then
            Header := Header + CodeIndent(3) + '.col' + IntToStr(Col) + ' {text-align: right;}' + sLineBreak;
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header +
          CodeIndent(2) + '</style>' + sLineBreak +
          CodeIndent + '</head>' + sLineBreak + sLineBreak +
          CodeIndent + '<body>' + sLineBreak + sLineBreak;
        if chkIncludeQuery.Checked then
          Header := Header + '<p style="font-family: monospace; white-space: pre;">' + GridData.SQL + '</p>' + CRLF + CRLF;
        Header := Header + CodeIndent(2) + '<table caption="' + TableName + ' (' + inttostr(NodeCount) + ' rows)">' + sLineBreak;
        if chkIncludeColumnNames.Checked then begin
          Header := Header +
            CodeIndent(3) + '<thead>' + sLineBreak +
            CodeIndent(4) + '<tr>' + sLineBreak;
          Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
          while Col > NoColumn do begin
            if Col <> ExcludeCol then
              Header := Header + CodeIndent(5) + '<th class="col' + IntToStr(Col) + '">' + Grid.Header.Columns[Col].Text + '</th>' + sLineBreak;
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Header := Header +
            CodeIndent(4) + '</tr>' + sLineBreak +
            CodeIndent(3) + '</thead>' + sLineBreak;
        end;
        Header := Header + CodeIndent(3) + '<tbody>' + sLineBreak;
      end;

      efExcel, efCSV: begin
        Separator := GridData.Connection.UnescapeString(editSeparator.Text);
        Encloser := GridData.Connection.UnescapeString(editEncloser.Text);
        Terminator := GridData.Connection.UnescapeString(editTerminator.Text);
        if chkIncludeColumnNames.Checked then begin
          Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
          while Col > NoColumn do begin
            // Alter column name in header if data is not raw.
            ResultCol := Col - 1;
            if Col <> ExcludeCol then begin
              Data := Grid.Header.Columns[Col].Text;
              if (GridData.DataType(ResultCol).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
                Data := 'HEX(' + Data + ')';
              // Add header item.
              if Header <> '' then
                Header := Header + Separator;
              Header := Header + Encloser + Data + Encloser;
            end;
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Header := Header + Terminator;
        end;
      end;

      efXML: begin
        // Imitate mysqldump's XML style
        Header := '<?xml version="1.0" encoding="'+GetHTMLCharsetByEncoding(Encoding)+'"?>' + CRLF + CRLF;
        if chkIncludeQuery.Checked then
          Header := Header + '<resultset statement="'+HTMLSpecialChars(GridData.SQL)+'" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">' + CRLF
        else
          Header := Header + '<table_data name="'+HTMLSpecialChars(TableName)+'">' + CRLF;
      end;

      efLaTeX: begin
        Header := '\begin{tabular}';
        Separator := ' & ';
        Encloser := '';
        Terminator := '\\ '+CRLF;
        Header := Header + '{';
        Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
        while Col > NoColumn do begin
          if Col <> ExcludeCol then
            Header := Header + ' c ';
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header + '}' + CRLF;
        if chkIncludeColumnNames.Checked then begin
          Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
          while Col > NoColumn do begin
            if Col <> ExcludeCol then
              Header := Header + FormatLatex(Grid.Header.Columns[Col].Text) + Separator;
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Delete(Header, Length(Header)-Length(Separator)+1, Length(Separator));
          Header := Header + Terminator;
        end;
      end;

      efTextile, efJiraTextile: begin
        Separator := IfThen(CurrentExportFormat=efTextile, ' |_. ', ' || ');
        Encloser := '';
        Terminator := IfThen(CurrentExportFormat=efTextile, ' |', ' ||') + CRLF;
        if chkIncludeColumnNames.Checked then begin
          Header := TrimLeft(Separator);
          Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
          while Col > NoColumn do begin
            if Col <> ExcludeCol then
              Header := Header + Grid.Header.Columns[Col].Text  + Separator;
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Delete(Header, Length(Header)-Length(Separator)+1, Length(Separator));
          Header := Header + Terminator;
        end;
        Separator := ' | ';
        Terminator := ' |' + CRLF;
      end;

      efPHPArray: begin
        if radioOutputFile.Checked then
          Header := '<?php'+CRLF+'$'+TableName+' = ['+CRLF
        else
          Header := '$'+TableName+' = ['+CRLF;
      end;

      efMarkDown: begin
        Separator := ' | ';
        Encloser := '';
        Terminator := CRLF;
        Header := Header + TableName + CRLF + '---' + CRLF;
        if chkIncludeQuery.Checked then
          Header := Header + '```sql' + CRLF + GridData.SQL + CRLF + '```' + CRLF;
        Header := Header + TrimLeft(Separator);
        Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
        while Col > NoColumn do begin
          if Col <> ExcludeCol then begin
            if chkIncludeColumnNames.Checked then
              Header := Header + Grid.Header.Columns[Col].Text + Separator
            else
              Header := Header + Separator
          end;
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header + Terminator;
        // Write an extra line with dashes below the heading, otherwise the table won't parse
        Header := Header + TrimLeft(Separator);
        Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
        while Col > NoColumn do begin
          ResultCol := Col - 1;
          if Col <> ExcludeCol then begin
            Header := Header + '---';
            if GridData.DataType(ResultCol).Category in [dtcInteger, dtcReal] then
              Header := Header + ':';
            Header := Header + Separator;
          end;
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header + Terminator;
      end;

      efJSON: begin
        // JavaScript Object Notation
        Header := '{' + sLineBreak;
        if chkIncludeQuery.Checked then
          Header := Header + #9 + '"query": '+FormatJson(GridData.SQL)+',' + sLineBreak
        else
          Header := Header + #9 + '"table": '+FormatJson(TableName)+',' + sLineBreak;
        Header := Header + #9 + '"rows":' + sLineBreak + #9 + '[';
      end;

    end;
    S.WriteString(Header);

    Node := GetNextNode(Grid, nil, SelectionOnly);
    while Assigned(Node) do begin
      // Update status once in a while.
      if (Node.Index+1) mod 100 = 0 then begin
        MainForm.ShowStatusMsg(f_('Exporting row %s of %s (%d%%, %s)',
          [FormatNumber(Node.Index+1),
          FormatNumber(NodeCount),
          Trunc((Node.Index+1) / NodeCount *100),
          FormatByteNumber(S.Size)]
          ));
        MainForm.ProgressStep;
      end;
      RowNum := Grid.GetNodeData(Node);
      GridData.RecNo := RowNum^;

      // Row preamble
      case CurrentExportFormat of
        efHTML: tmp := CodeIndent(4) + '<tr>' + sLineBreak;

        efXML: tmp := CodeIndent + '<row>' + sLineBreak;

        efSQLUpdate: begin
          tmp := '';
          tmp := tmp + 'UPDATE ' + GridData.Connection.QuoteIdent(Tablename) + ' SET ';
        end;

        efSQLInsert, efSQLInsertIgnore, efSQLReplace, efSQLDeleteInsert: begin
          tmp := '';
          if CurrentExportFormat = efSQLDeleteInsert then begin
            tmp := tmp + 'DELETE FROM ' + GridData.Connection.QuoteIdent(Tablename) + ' WHERE' + GridData.GetWhereClause + ';' + CRLF;
          end;

          if CurrentExportFormat in [efSQLInsert, efSQLDeleteInsert] then
            tmp := tmp + 'INSERT'
          else if CurrentExportFormat = efSQLInsertIgnore then
            tmp := tmp + 'INSERT IGNORE'
          else
            tmp := tmp + 'REPLACE';
          tmp := tmp + ' INTO '+GridData.Connection.QuoteIdent(Tablename);
          if chkIncludeColumnNames.Checked then begin
            tmp := tmp + ' (';
            Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
            while Col > NoColumn do begin
              ResultCol := Col - 1;
              if (Col <> ExcludeCol) and (not GridData.ColIsVirtual(ResultCol)) then
                tmp := tmp + GridData.Connection.QuoteIdent(Grid.Header.Columns[Col].Text)+', ';
              Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
            end;
            Delete(tmp, Length(tmp)-1, 2);
            tmp := tmp + ')';
          end;
          tmp := tmp + ' VALUES (';
        end;

        efTextile, efJiraTextile: tmp := TrimLeft(Separator);

        efPHPArray: tmp := CodeIndent + '[' + sLineBreak;

        efMarkDown: tmp := '| ';

        efJSON: begin
          if chkIncludeColumnNames.Checked then
            tmp := sLineBreak + CodeIndent(2) + '{' + sLineBreak
          else
            tmp := sLineBreak + CodeIndent(2) + '[' + sLineBreak
        end;

        efJSONLines: begin
          if chkIncludeColumnNames.Checked then
            tmp := '{'
          else
            tmp := '[';
        end

        else tmp := '';
      end;

      // Row contents
      Col := Grid.Header.Columns.GetFirstVisibleColumn(True);
      while Col > NoColumn do begin
        ResultCol := Col - 1;
        if Col <> ExcludeCol then begin
          if (GridData.DataType(ResultCol).Category in [dtcBinary, dtcSpatial])
            and (not Mainform.actBlobAsText.Checked) then begin
            Data := GridData.HexValue(ResultCol);
          end else begin
            Data := GridData.Col(ResultCol);
            RemoveNullChars(Data, HasNulls);
          end;

          // Keep formatted numeric values
          if (GridData.DataType(ResultCol).Category in [dtcInteger, dtcReal])
            and (CurrentExportFormat in [efExcel, efHTML, efMarkDown])
            then begin
            Data := FormatNumber(Data, False);
          end;

          // Remove linebreaks, see #474
          if chkRemoveLinebreaks.Checked then begin
            StripNewLines(Data);
          end;

          case CurrentExportFormat of
            efHTML: begin
              // Escape HTML control characters in data.
              Data := HTMLSpecialChars(Data);
              tmp := tmp + CodeIndent(5) + '<td class="col' + IntToStr(Col) + '">' + Data + '</td>' + sLineBreak;
            end;

            efExcel, efCSV: begin
              if GridData.IsNull(ResultCol) then
                Data := editNull.Text
              else begin
                Data := FormatExcelCsv(Data, Encloser, GridData.DataType(ResultCol));
                Data := Encloser + Data + Encloser;
              end;
              tmp := tmp + Data + Separator;
            end;

            efLaTeX: begin
              Data := FormatLatex(Data);
              if (not GridData.IsNull(ResultCol)) and (GridData.DataType(ResultCol).Category in [dtcInteger, dtcReal]) then
                // Special encloser for numeric values, see https://www.heidisql.com/forum.php?t=36530
                Data := '$' + Data + '$';
              tmp := tmp + Data + Separator;
            end;

            efTextile, efJiraTextile: begin
              tmp := tmp + Data + Separator;
            end;

            efMarkDown: begin
              if GridData.IsNull(ResultCol) then
                Data := editNull.Text;
              tmp := tmp + Data + Separator;
            end;

            efXML: begin
              // Print cell start tag.
              tmp := tmp + CodeIndent(2) + '<field';
              if chkIncludeColumnNames.Checked then
                tmp := tmp + ' name="' + HTMLSpecialChars(Grid.Header.Columns[Col].Text) + '"';
              if GridData.IsNull(ResultCol) then
                tmp := tmp + ' xsi:nil="true" />' + CRLF
              else begin
                if (GridData.DataType(ResultCol).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
                  tmp := tmp + ' format="hex"';
                tmp := tmp + '>' + HTMLSpecialChars(Data) + '</field>' + CRLF;
              end;
            end;

            efSQLInsert, efSQLInsertIgnore, efSQLReplace, efSQLDeleteInsert, efSQLUpdate: begin
              if GridData.ColIsVirtual(ResultCol) then
                Data := ''
              else if GridData.IsNull(ResultCol) then
                Data := 'NULL'
              else if (GridData.DataType(ResultCol).Index = dbdtBit) and GridData.Connection.Parameters.IsAnyMySQL then
                Data := 'b' + GridData.Connection.EscapeString(Data)
              else if (GridData.DataType(ResultCol).Category in [dtcText, dtcTemporal, dtcOther])
                or ((GridData.DataType(ResultCol).Category in [dtcBinary, dtcSpatial]) and Mainform.actBlobAsText.Checked)
                then
                Data := GridData.Connection.EscapeString(Data)
              else if Data = '' then
                Data := GridData.Connection.EscapeString(Data);
              if not Data.IsEmpty then begin
                if CurrentExportFormat = efSQLUpdate then
                  tmp := tmp + GridData.Connection.QuoteIdent(Grid.Header.Columns[Col].Text) + '=';
                tmp := tmp + Data + ', ';
              end;
            end;

            efPHPArray: begin
              if GridData.IsNull(ResultCol) then
                Data := 'null'
              else case GridData.DataType(ResultCol).Category of
                dtcInteger, dtcReal: begin
                  // Remove zeropadding to avoid octal => integer conversion in PHP
                  Data := FormatNumber(Data);
                  Data := UnformatNumber(Data);
                end;
                else
                  Data := FormatPhp(Data);
              end;

              if chkIncludeColumnNames.Checked then
                tmp := tmp + CodeIndent(2) + FormatPhp(Grid.Header.Columns[Col].Text) + ' => ' + Data + ',' + sLineBreak
              else
                tmp := tmp + CodeIndent(2) + Data + ',' + sLineBreak;
            end;

            efJSON: begin
              tmp := tmp + CodeIndent(3);
              if chkIncludeColumnNames.Checked then
                tmp := tmp + FormatJson(Grid.Header.Columns[Col].Text) + ': ';
              if GridData.IsNull(ResultCol) then
                tmp := tmp + 'null,' +CRLF
              else begin
                case GridData.DataType(ResultCol).Category of
                  dtcInteger, dtcReal:
                    tmp := tmp + Data;
                  else
                    tmp := tmp + FormatJson(Data)
                end;
                tmp := tmp + ',' + CRLF;
              end;
            end;

            efJSONLines: begin
              if chkIncludeColumnNames.Checked then
                tmp := tmp + FormatJson(Grid.Header.Columns[Col].Text) + ': ';
              if GridData.IsNull(ResultCol) then
                tmp := tmp + 'null, '
              else begin
                case GridData.DataType(ResultCol).Category of
                  dtcInteger, dtcReal:
                    tmp := tmp + Data;
                  else
                    tmp := tmp + FormatJson(Data)
                end;
                tmp := tmp + ', ';
              end;
            end;

          end;
        end;

        Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
      end;

      // Row epilogue
      case CurrentExportFormat of
        efHTML:
          tmp := tmp + CodeIndent(4) + '</tr>' + sLineBreak;
        efExcel, efCSV, efLaTeX, efTextile, efJiraTextile: begin
          Delete(tmp, Length(tmp)-Length(Separator)+1, Length(Separator));
          tmp := tmp + Terminator;
        end;
        efXML:
          tmp := tmp + CodeIndent + '</row>' + sLineBreak;
        efSQLInsert, efSQLInsertIgnore, efSQLReplace, efSQLDeleteInsert: begin
          Delete(tmp, Length(tmp)-1, 2);
          tmp := tmp + ');' + CRLF;
        end;
        efSQLUpdate : begin
          Delete(tmp, length(tmp)-1,2);
          tmp := tmp + ' WHERE' + GridData.GetWhereClause + ';' + sLineBreak;
        end;
        efPHPArray:
          tmp := tmp + CodeIndent + '],' + sLineBreak;
        efMarkDown:
          tmp := tmp + Terminator;
        efJSON: begin
          Delete(tmp, length(tmp)-2,2);
          if chkIncludeColumnNames.Checked then
            tmp := tmp + CodeIndent(2) + '},'
          else
            tmp := tmp + CodeIndent(2) + '],';
        end;
        efJSONLines: begin
          Delete(tmp, length(tmp)-1,2);
          if chkIncludeColumnNames.Checked then
            tmp := tmp + '}' + #10
          else
            tmp := tmp + ']' + #10;
        end;
      end;
      S.WriteString(tmp);

      Node := GetNextNode(Grid, Node, SelectionOnly);
    end;

    // Footer
    case CurrentExportFormat of
      efHTML: begin
        tmp :=
          CodeIndent(3) + '</tbody>' + sLineBreak +
          CodeIndent(2) + '</table>' + sLineBreak + sLineBreak +
          CodeIndent(2) + '<p>' + sLineBreak +
          CodeIndent(3) + '<em>generated ' + DateToStr(now) + ' ' + TimeToStr(now) +
          CodeIndent(3) + 'by <a href="'+APPDOMAIN+'">' + APPNAME + ' ' + Mainform.AppVersion + '</a></em>' + sLineBreak +
          CodeIndent(2) + '</p>' + sLineBreak + sLineBreak +
          CodeIndent + '</body>' + sLineBreak +
          '</html>' + sLineBreak;
      end;
      efXML: begin
        if chkIncludeQuery.Checked then
          tmp := '</resultset>' + CRLF
        else
          tmp := '</table_data>' + CRLF;
      end;
      efLaTeX:
        tmp := '\end{tabular}' + CRLF;
      efPHPArray: begin
        tmp := '];' + CRLF;
      end;
      efJSON: begin
        S.Size := S.Size - 1;
        tmp := sLineBreak + CodeIndent + ']' + sLineBreak + '}';
      end;
      else
        tmp := '';
    end;
    S.WriteString(tmp);

    if radioOutputCopyToClipboard.Checked then begin
      HTML := nil;
      // SynEdit's exporter is slow on large strings, see issue #2903
      if S.Size < 100*SIZE_KB then begin
        case CurrentExportFormat of
          efSQLInsert, efSQLInsertIgnore, efSQLReplace, efSQLDeleteInsert: begin
            Exporter := TSynExporterHTML.Create(Self);
            Exporter.Highlighter := MainForm.SynSQLSynUsed;
            Exporter.ExportAll(Explode(CRLF, S.DataString));
            HTML := TMemoryStream.Create;
            Exporter.SaveToStream(HTML);
            Exporter.Free;
          end;
          efHTML: HTML := S;
        end;
      end;
      StreamToClipboard(S, HTML);
    end else begin
      try
        S.SaveToFile(Filename);
      except
        on E:EFCreateError do begin
          // Keep form open if file cannot be created
          ModalResult := mrNone;
          MainForm.SetProgressState(pbsError);
          ErrorDialog(E.Message);
        end;
      end;
    end;
    Mainform.ShowStatusMsg(_('Freeing data...'));
    FreeAndNil(S);

  except
    // Whole export code wrapped here
    on E:EDbError do begin
      Screen.Cursor := crDefault;
      ErrorDialog(E.Message);
    end
    else
      raise;
  end;

  Mainform.DisableProgress;
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


end.
