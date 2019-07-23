unit exportgrid;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, VirtualTrees, SynExportHTML, gnugettext, ActnList,
  extra_controls;

type
  TGridExportFormat = (efExcel, efCSV, efHTML, efXML, efSQLInsert, efSQLReplace, efSQLDeleteInsert, efLaTeX, efWiki, efPHPArray, efMarkDown, efJSON);

  TfrmExportGrid = class(TExtForm)
    btnOK: TButton;
    btnCancel: TButton;
    grpFormat: TRadioGroup;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure grpFormatClick(Sender: TObject);
    procedure btnSetClipboardDefaultsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FCSVEditor: TButtonedEdit;
    FCSVSeparator, FCSVEncloser, FCSVTerminator, FCSVNull: String;
    FGrid: TVirtualStringTree;
    FRecentFiles: TStringList;
    FHiddenCopyMode: Boolean;
    const FFormatToFileExtension: Array[TGridExportFormat] of String =
      (('csv'), ('csv'), ('html'), ('xml'), ('sql'), ('sql'), ('sql'), ('LaTeX'), ('wiki'), ('php'), ('md'), ('json'));
    const FFormatToDescription: Array[TGridExportFormat] of String =
      (('Excel CSV'), ('Delimited text'), ('HTML table'), ('XML'), ('SQL INSERTs'), ('SQL REPLACEs'), ('SQL DELETEs/INSERTs'), ('LaTeX'), ('Wiki markup'), ('PHP Array'), ('Markdown Here'), ('JSON'));
    procedure SaveDialogTypeChange(Sender: TObject);
    function GetExportFormat: TGridExportFormat;
    procedure SetExportFormat(Value: TGridExportFormat);
    procedure SetExportFormatByFilename;
    procedure SelectRecentFile(Sender: TObject);
    procedure PutFilenamePlaceholder(Sender: TObject);
    function EscapePHP(Text: String): String;
  public
    { Public declarations }
    property Grid: TVirtualStringTree read FGrid write FGrid;
    property ExportFormat: TGridExportFormat read GetExportFormat write SetExportFormat;
  end;


implementation

uses main, apphelpers, dbconnection, dbstructures;

{$R *.dfm}



procedure TfrmExportGrid.FormCreate(Sender: TObject);
var
  FormatDesc: String;
begin
  HasSizeGrip := True;
  Width := AppSettings.ReadInt(asGridExportWindowWidth);
  Height := AppSettings.ReadInt(asGridExportWindowHeight);
  editFilename.Text := AppSettings.ReadString(asGridExportFilename);
  FRecentFiles := Explode(DELIM, AppSettings.ReadString(asGridExportRecentFiles));
  comboEncoding.Items.Assign(MainForm.FileEncodings);
  comboEncoding.Items.Delete(0); // Remove "Auto detect"
  comboEncoding.ItemIndex := AppSettings.ReadInt(asGridExportEncoding);
  grpFormat.Items.Clear;
  for FormatDesc in FFormatToDescription do
    grpFormat.Items.Add(FormatDesc);
  FHiddenCopyMode := Owner = MainForm.actCopyRows;

  if FHiddenCopyMode then begin
    radioOutputCopyToClipboard.Checked := True;
    grpFormat.ItemIndex := AppSettings.ReadInt(asGridExportClpFormat);
    grpSelection.ItemIndex := 0; // Always use selected cells in copy mode
    chkIncludeColumnNames.Checked := AppSettings.ReadBool(asGridExportClpColumnNames);
    chkIncludeAutoIncrement.Checked := AppSettings.ReadBool(asGridExportClpIncludeAutoInc);
    chkIncludeQuery.Checked := False; // Always off in copy mode
    FCSVSeparator := AppSettings.ReadString(asGridExportClpSeparator);
    FCSVEncloser := AppSettings.ReadString(asGridExportClpEncloser);
    FCSVTerminator := AppSettings.ReadString(asGridExportClpTerminator);
    FCSVNull := AppSettings.ReadString(asGridExportClpNull);
  end else begin
    radioOutputCopyToClipboard.Checked := AppSettings.ReadBool(asGridExportOutputCopy);
    radioOutputFile.Checked := AppSettings.ReadBool(asGridExportOutputFile);
    grpFormat.ItemIndex := AppSettings.ReadInt(asGridExportFormat);
    grpSelection.ItemIndex := AppSettings.ReadInt(asGridExportSelection);
    chkIncludeColumnNames.Checked := AppSettings.ReadBool(asGridExportColumnNames);
    chkIncludeAutoIncrement.Checked := AppSettings.ReadBool(asGridExportIncludeAutoInc);
    chkIncludeQuery.Checked := AppSettings.ReadBool(asGridExportIncludeQuery);
    FCSVSeparator := AppSettings.ReadString(asGridExportSeparator);
    FCSVEncloser := AppSettings.ReadString(asGridExportEncloser);
    FCSVTerminator := AppSettings.ReadString(asGridExportTerminator);
    FCSVNull := AppSettings.ReadString(asGridExportNull);
  end;
  ValidateControls(Sender);
end;


procedure TfrmExportGrid.FormDestroy(Sender: TObject);
begin
  // Store settings
  if not FHiddenCopyMode then begin
    AppSettings.WriteInt(asGridExportWindowWidth, Width);
    AppSettings.WriteInt(asGridExportWindowHeight, Height);
    if ModalResult = mrOK then begin
      AppSettings.WriteBool(asGridExportOutputCopy, radioOutputCopyToClipboard.Checked);
      AppSettings.WriteBool(asGridExportOutputFile, radioOutputFile.Checked);
      AppSettings.WriteString(asGridExportFilename, editFilename.Text);
      AppSettings.WriteString(asGridExportRecentFiles, ImplodeStr(DELIM, FRecentFiles));
      AppSettings.WriteInt(asGridExportEncoding, comboEncoding.ItemIndex);
      AppSettings.WriteInt(asGridExportFormat, grpFormat.ItemIndex);
      AppSettings.WriteInt(asGridExportSelection, grpSelection.ItemIndex);
      AppSettings.WriteBool(asGridExportColumnNames, chkIncludeColumnNames.Checked);
      AppSettings.WriteBool(asGridExportIncludeAutoInc, chkIncludeAutoIncrement.Checked);
      AppSettings.WriteBool(asGridExportIncludeQuery, chkIncludeQuery.Checked);
      AppSettings.WriteString(asGridExportSeparator, FCSVSeparator);
      AppSettings.WriteString(asGridExportEncloser, FCSVEncloser);
      AppSettings.WriteString(asGridExportTerminator, FCSVTerminator);
      AppSettings.WriteString(asGridExportNull, FCSVNull);
    end;
  end;
end;


procedure TfrmExportGrid.FormResize(Sender: TObject);
begin
  grpFormat.Width := Width div 3;
  grpSelection.Left := grpFormat.Left + grpFormat.Width + 8;
  grpSelection.Width := Width - grpSelection.Left - 24;
  grpOptions.Left := grpSelection.Left;
  grpOptions.Width := grpSelection.Width;
end;

procedure TfrmExportGrid.FormShow(Sender: TObject);
begin
  // Show dialog. Expect "Grid" property to be set now by the caller.
  chkIncludeAutoIncrement.OnClick := CalcSize;
  CalcSize(Sender);
end;


procedure TfrmExportGrid.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Destroy dialog - not cached
  Action := caFree;
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
  if ExportFormat = efExcel then
    comboEncoding.ItemIndex := comboEncoding.Items.IndexOf('ANSI');
end;


function TfrmExportGrid.GetExportFormat: TGridExportFormat;
begin
  Result := TGridExportFormat(grpFormat.ItemIndex);
end;


procedure TfrmExportGrid.SetExportFormat(Value: TGridExportFormat);
begin
  grpFormat.ItemIndex := Integer(Value);
end;


procedure TfrmExportGrid.grpFormatClick(Sender: TObject);
var
  Filename: String;
begin
  // Auto-modify file extension when selecting export format
  // Be careful about triggering editFilename.OnChange event, as we may have come here from that event!
  if radioOutputFile.Checked then begin
    Filename := ExtractFilePath(editFilename.Text) +
      ExtractBaseFileName(editFilename.Text) +
      '.' + FFormatToFileExtension[ExportFormat];
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
    if ext = FFormatToFileExtension[ExportFormat] then
      break;
    if ext = FFormatToFileExtension[efrm] then begin
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
  Dialog.FileName := ExtractBaseFileName(Filename);
  Dialog.Filter := '';
  for ef:=Low(TGridExportFormat) to High(TGridExportFormat) do
    Dialog.Filter := Dialog.Filter + FFormatToDescription[ef] + ' (*.'+FFormatToFileExtension[ef]+')|*.'+FFormatToFileExtension[ef]+'|';
  Dialog.Filter := Dialog.Filter + _('All files')+' (*.*)|*.*';
  Dialog.OnTypeChange := SaveDialogTypeChange;
  Dialog.FilterIndex := grpFormat.ItemIndex+1;
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
  AppSettings.WriteInt(asGridExportClpFormat, grpFormat.ItemIndex);
  AppSettings.WriteBool(asGridExportClpColumnNames, chkIncludeColumnNames.Checked);
  AppSettings.WriteBool(asGridExportClpIncludeAutoInc, chkIncludeAutoIncrement.Checked);
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
  RowNum: PInt64;
  SelectionSize, AllSize: Int64;
begin
  GridData := Mainform.GridResult(Grid);
  AllSize := 0;
  SelectionSize := 0;
  chkIncludeAutoIncrement.Enabled := GridData.AutoIncrementColumn > -1;
  ExcludeCol := -1;
  if chkIncludeAutoIncrement.Enabled and (not chkIncludeAutoIncrement.Checked) then
    ExcludeCol := GridData.AutoIncrementColumn;

  Node := GetNextNode(Grid, nil, False);
  while Assigned(Node) do begin
    RowNum := Grid.GetNodeData(Node);
    GridData.RecNo := RowNum^;
    Col := Grid.Header.Columns.GetFirstVisibleColumn;
    while Col > NoColumn do begin
      if Col <> ExcludeCol then begin
        Inc(AllSize, GridData.ColumnLengths(Col));
        if vsSelected in Node.States then
          Inc(SelectionSize, GridData.ColumnLengths(Col));
      end;
      Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
    end;
    Node := GetNextNode(Grid, Node, False);
  end;
  grpSelection.Items[0] := f_('Selection (%s rows, %s)', [FormatNumber(Grid.SelectedCount), FormatByteNumber(SelectionSize)]);
  grpSelection.Items[1] := f_('Complete (%s rows, %s)', [FormatNumber(Grid.RootNodeCount), FormatByteNumber(AllSize)]);
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
      Dialog.DefaultExt := FFormatToFileExtension[ef];
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


function TfrmExportGrid.EscapePHP(Text: String): String;
begin
  // String escaping for PHP output. Incompatible to TDBConnection.EscapeString.
  Result := StringReplace(Text, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := '"' + Result + '"';
end;


procedure TfrmExportGrid.btnOKClick(Sender: TObject);
var
  Col, ExcludeCol: TColumnIndex;
  Header, Data, tmp, Encloser, Separator, Terminator, TableName, Filename: String;
  Node: PVirtualNode;
  GridData: TDBQuery;
  SelectionOnly: Boolean;
  i: Integer;
  NodeCount: Cardinal;
  RowNum: PInt64;
  HTML: TStream;
  S: TStringStream;
  Exporter: TSynExporterHTML;
  Encoding: TEncoding;
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
      ExcludeCol := GridData.AutoIncrementColumn;

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
    S := TStringStream.Create(Header, Encoding);
    Header := '';
    case ExportFormat of
      efHTML: begin
        Header :=
          '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" ' + CRLF +
          '  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">' + CRLF + CRLF +
          '<html>' + CRLF +
          '  <head>' + CRLF +
          '    <title>' + TableName + '</title>' + CRLF +
          '    <meta name="GENERATOR" content="'+ APPNAME+' '+Mainform.AppVersion + '">' + CRLF +
          '    <meta http-equiv="Content-Type" content="text/html; charset='+GetHTMLCharsetByEncoding(Encoding)+'" />' + CRLF +
          '    <style type="text/css">' + CRLF +
          '      thead tr {background-color: ActiveCaption; color: CaptionText;}' + CRLF +
          '      th, td {vertical-align: top; font-family: "'+Grid.Font.Name+'", Arial, Helvetica, sans-serif; font-size: '+IntToStr(Grid.Font.Size)+'pt; padding: '+IntToStr(Grid.TextMargin-1)+'px; }' + CRLF +
          '      table, td {border: 1px solid silver;}' + CRLF +
          '      table {border-collapse: collapse;}' + CRLF;
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
        while Col > NoColumn do begin
          // Adjust preferred width of columns.
          Header := Header +
           '      thead .col' + IntToStr(Col) + ' {width: ' + IntToStr(Grid.Header.Columns[Col].Width) + 'px;}' + CRLF;
          // Right-justify all cells to match the grid on screen.
          if Grid.Header.Columns[Col].Alignment = taRightJustify then
            Header := Header + '      .col' + IntToStr(Col) + ' {text-align: right;}' + CRLF;
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header +
          '    </style>' + CRLF +
          '  </head>' + CRLF + CRLF +
          '  <body>' + CRLF + CRLF;
        if chkIncludeQuery.Checked then
          Header := Header + '<p style="font-family: monospace; white-space: pre;">' + GridData.SQL + '</p>' + CRLF + CRLF;
        Header := Header + '    <table caption="' + TableName + ' (' + inttostr(NodeCount) + ' rows)">' + CRLF;
        if chkIncludeColumnNames.Checked then begin
          Header := Header +
            '      <thead>' + CRLF +
            '        <tr>' + CRLF;
          Col := Grid.Header.Columns.GetFirstVisibleColumn;
          while Col > NoColumn do begin
            if Col <> ExcludeCol then
              Header := Header + '          <th class="col' + IntToStr(Col) + '">' + Grid.Header.Columns[Col].Text + '</th>' + CRLF;
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Header := Header +
            '        </tr>' + CRLF +
            '      </thead>' + CRLF;
        end;
        Header := Header +
          '      <tbody>' + CRLF;
      end;

      efExcel, efCSV: begin
        Separator := GridData.Connection.UnescapeString(editSeparator.Text);
        Encloser := GridData.Connection.UnescapeString(editEncloser.Text);
        Terminator := GridData.Connection.UnescapeString(editTerminator.Text);
        if chkIncludeColumnNames.Checked then begin
          Col := Grid.Header.Columns.GetFirstVisibleColumn;
          while Col > NoColumn do begin
            // Alter column name in header if data is not raw.
            if Col <> ExcludeCol then begin
              Data := Grid.Header.Columns[Col].Text;
              if (GridData.DataType(Col).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
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
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
        while Col > NoColumn do begin
          if Col <> ExcludeCol then
            Header := Header + ' c ';
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header + '}' + CRLF;
        if chkIncludeColumnNames.Checked then begin
          Col := Grid.Header.Columns.GetFirstVisibleColumn;
          while Col > NoColumn do begin
            if Col <> ExcludeCol then
              Header := Header + Grid.Header.Columns[Col].Text + Separator;
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Delete(Header, Length(Header)-Length(Separator)+1, Length(Separator));
          Header := Header + Terminator;
        end;
      end;

      efWiki: begin
        Separator := ' || ';
        Encloser := '';
        Terminator := ' ||'+CRLF;
        if chkIncludeColumnNames.Checked then begin
          Header := '|| ';
          Col := Grid.Header.Columns.GetFirstVisibleColumn;
          while Col > NoColumn do begin
            if Col <> ExcludeCol then
              Header := Header + '*' + Grid.Header.Columns[Col].Text + '*' + Separator;
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Delete(Header, Length(Header)-Length(Separator)+1, Length(Separator));
          Header := Header + Terminator;
        end;
      end;

      efPHPArray: begin
        if radioOutputFile.Checked then
          Header := '<?php'+CRLF+'$'+TableName+' = array('+CRLF
        else
          Header := '$'+TableName+' = array('+CRLF;
      end;

      efMarkDown: begin
        Separator := ' | ';
        Encloser := '';
        Terminator := CRLF;
        Header := Header + TableName + CRLF + '---' + CRLF;
        if chkIncludeQuery.Checked then
          Header := Header + '```sql' + CRLF + GridData.SQL + CRLF + '```' + CRLF;
        Header := Header + TrimLeft(Separator);
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
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
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
        while Col > NoColumn do begin
          if Col <> ExcludeCol then begin
            Header := Header + '---';
            if GridData.DataType(Col).Category in [dtcInteger, dtcReal] then
              Header := Header + ':';
            Header := Header + Separator;
          end;
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header + Terminator;
      end;

      efJSON: begin
        // JavaScript Object Notation
        Header := '{' + CRLF;
        if chkIncludeQuery.Checked then
          Header := Header + #9 + '"query": '+EscapePHP(GridData.SQL)+',' + CRLF
        else
          Header := Header + #9 + '"table": '+EscapePHP(TableName)+',' + CRLF ;
        Header := Header + #9 + '"rows":' + CRLF + #9 + '[';
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
      case ExportFormat of
        efHTML: tmp := '        <tr>' + CRLF;

        efXML: tmp := #9'<row>' + CRLF;

        efSQLInsert, efSQLReplace, efSQLDeleteInsert: begin
          tmp := '';
          if ExportFormat = efSQLDeleteInsert then begin
            tmp := tmp + 'DELETE FROM ' + GridData.Connection.QuoteIdent(Tablename) + ' WHERE' + GridData.GetWhereClause + ';' + CRLF;
          end;

          if ExportFormat in [efSQLInsert, efSQLDeleteInsert] then
            tmp := tmp + 'INSERT'
          else
            tmp := tmp + 'REPLACE';
          tmp := tmp + ' INTO '+GridData.Connection.QuoteIdent(Tablename);
          if chkIncludeColumnNames.Checked then begin
            tmp := tmp + ' (';
            Col := Grid.Header.Columns.GetFirstVisibleColumn;
            while Col > NoColumn do begin
              if (Col <> ExcludeCol) and (not GridData.ColIsVirtual(Col)) then
                tmp := tmp + GridData.Connection.QuoteIdent(Grid.Header.Columns[Col].Text)+', ';
              Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
            end;
            Delete(tmp, Length(tmp)-1, 2);
            tmp := tmp + ')';
          end;
          tmp := tmp + ' VALUES (';
        end;

        efWiki: tmp := TrimLeft(Separator);

        efPHPArray: tmp := #9 + 'array('+CRLF;

        efMarkDown: tmp := '| ';

        efJSON: begin
          if chkIncludeColumnNames.Checked then
            tmp := CRLF + #9#9 + '{' + CRLF
          else
            tmp := CRLF + #9#9 + '[' + CRLF
        end

        else tmp := '';
      end;

      Col := Grid.Header.Columns.GetFirstVisibleColumn;
      while Col > NoColumn do begin
        if Col <> ExcludeCol then begin
          if (GridData.DataType(Col).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
            Data := GridData.HexValue(Col)
          else
            Data := GridData.Col(Col);
          // Keep formatted numeric values
          if (GridData.DataType(Col).Category in [dtcInteger, dtcReal])
            and (ExportFormat in [efExcel, efHTML, efMarkDown]) then
              Data := FormatNumber(Data, False);

          case ExportFormat of
            efHTML: begin
              // Escape HTML control characters in data.
              Data := HTMLSpecialChars(Data);
              tmp := tmp + '          <td class="col' + IntToStr(Col) + '">' + Data + '</td>' + CRLF;
            end;

            efExcel, efCSV, efLaTeX, efWiki, efMarkDown: begin
              // Escape encloser characters inside data per de-facto CSV.
              Data := StringReplace(Data, Encloser, Encloser+Encloser, [rfReplaceAll]);
              if GridData.IsNull(Col) and (ExportFormat in [efExcel, efCSV, efMarkDown]) then
                Data := editNull.Text
              else
                Data := Encloser + Data + Encloser;
              tmp := tmp + Data + Separator;
            end;

            efXML: begin
              // Print cell start tag.
              tmp := tmp + #9#9'<field';
              if chkIncludeColumnNames.Checked then
                tmp := tmp + ' name="' + HTMLSpecialChars(Grid.Header.Columns[Col].Text) + '"';
              if GridData.IsNull(Col) then
                tmp := tmp + ' xsi:nil="true" />' + CRLF
              else begin
                if (GridData.DataType(Col).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
                  tmp := tmp + ' format="hex"';
                tmp := tmp + '>' + HTMLSpecialChars(Data) + '</field>' + CRLF;
              end;
            end;

            efSQLInsert, efSQLReplace, efSQLDeleteInsert: begin
              if GridData.ColIsVirtual(Col) then
                Data := ''
              else if GridData.IsNull(Col) then
                Data := 'NULL'
              else if (GridData.DataType(Col).Index = dtBit) and GridData.Connection.Parameters.IsMySQL then
                Data := 'b' + esc(Data)
              else if (GridData.DataType(Col).Category in [dtcText, dtcTemporal, dtcOther])
                or ((GridData.DataType(Col).Category in [dtcBinary, dtcSpatial]) and Mainform.actBlobAsText.Checked)
                then
                Data := esc(Data)
              else if Data = '' then
                Data := esc(Data);
              if not Data.IsEmpty then
                tmp := tmp + Data + ', ';
            end;

            efPHPArray: begin
              if GridData.IsNull(Col) then
                Data := 'NULL'
              else case GridData.DataType(Col).Category of
                dtcInteger, dtcReal: begin
                  // Remove zeropadding to avoid octal => integer conversion in PHP
                  Data := FormatNumber(Data);
                  Data := UnformatNumber(Data);
                end;
                else
                  Data := EscapePHP(Data);
              end;

              if chkIncludeColumnNames.Checked then
                tmp := tmp + #9#9 + EscapePHP(Grid.Header.Columns[Col].Text) + ' => ' + Data + ','+CRLF
              else
                tmp := tmp + #9#9 + Data + ','+CRLF;
            end;

            efJSON: begin
              tmp := tmp + #9#9#9;
              if chkIncludeColumnNames.Checked then
                tmp := tmp + EscapePHP(Grid.Header.Columns[Col].Text) + ': ';
              if GridData.IsNull(Col) then
                tmp := tmp + 'null,' +CRLF
              else begin
                case GridData.DataType(Col).Category of
                  dtcInteger, dtcReal:
                    tmp := tmp + Data;
                  else
                    tmp := tmp + EscapePHP(Data)
                end;
                tmp := tmp + ',' + CRLF;
              end;
            end;

          end;
        end;

        Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
      end;

      // Row epilogue
      case ExportFormat of
        efHTML:
          tmp := tmp + '        </tr>' + CRLF;
        efExcel, efCSV, efLaTeX, efWiki: begin
          Delete(tmp, Length(tmp)-Length(Separator)+1, Length(Separator));
          tmp := tmp + Terminator;
        end;
        efXML:
          tmp := tmp + #9'</row>' + CRLF;
        efSQLInsert, efSQLReplace, efSQLDeleteInsert: begin
          Delete(tmp, Length(tmp)-1, 2);
          tmp := tmp + ');' + CRLF;
        end;
        efPHPArray:
          tmp := tmp + #9 + '),' + CRLF;
        efMarkDown:
          tmp := tmp + Terminator;
        efJSON: begin
          Delete(tmp, length(tmp)-2,2);
          if chkIncludeColumnNames.Checked then
            tmp := tmp + #9#9 + '},'
          else
            tmp := tmp + #9#9 + '],';
        end;
      end;
      S.WriteString(tmp);

      Node := GetNextNode(Grid, Node, SelectionOnly);
    end;

    // Footer
    case ExportFormat of
      efHTML: begin
        tmp :=
          '      </tbody>' + CRLF +
          '    </table>' + CRLF + CRLF +
          '    <p>' + CRLF +
          '      <em>generated ' + DateToStr(now) + ' ' + TimeToStr(now) +
          '      by <a href="'+APPDOMAIN+'">' + APPNAME + ' ' + Mainform.AppVersion + '</a></em>' + CRLF +
          '    </p>' + CRLF + CRLF +
          '  </body>' + CRLF +
          '</html>' + CRLF;
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
        tmp := ');' + CRLF;
        if radioOutputFile.Checked then
          tmp := tmp + '?>';
      end;
      efJSON: begin
        S.Size := S.Size - 1;
        tmp := CRLF + #9 + ']' + CRLF + '}';
      end;
      else
        tmp := '';
    end;
    S.WriteString(tmp);

    if radioOutputCopyToClipboard.Checked then begin
      HTML := nil;
      // SynEdit's exporter is slow on large strings, see issue #2903
      if S.Size < 100*SIZE_KB then begin
        case ExportFormat of
          efSQLInsert, efSQLReplace, efSQLDeleteInsert: begin
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
      StreamToClipboard(S, HTML, (ExportFormat=efHTML) and (HTML <> nil));
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
