unit exportgrid;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, VirtualTrees, SynExportHTML, gnugettext;

type
  TGridExportFormat = (efExcel, efCSV, efHTML, efXML, efSQLInsert, efSQLReplace, efLaTeX, efWiki, efPHPArray);

  TfrmExportGrid = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CalcSize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure editFilenameRightButtonClick(Sender: TObject);
    procedure editFilenameChange(Sender: TObject);
    procedure SelectRecentFile(Sender: TObject);
    procedure popupRecentFilesPopup(Sender: TObject);
    procedure menuCSVClick(Sender: TObject);
    procedure editCSVRightButtonClick(Sender: TObject);
    procedure editCSVChange(Sender: TObject);
    procedure ValidateControls(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure grpFormatClick(Sender: TObject);
  private
    { Private declarations }
    FCSVEditor: TButtonedEdit;
    FCSVSeparator, FCSVEncloser, FCSVTerminator: String;
    FGrid: TVirtualStringTree;
    FRecentFiles: TStringList;
    const FFormatToFileExtension: Array[TGridExportFormat] of String =
      (('csv'), ('csv'), ('html'), ('xml'), ('sql'), ('sql'), ('LaTeX'), ('wiki'), ('php'));
    const FFormatToDescription: Array[TGridExportFormat] of String =
      (('Excel CSV'), ('Delimited text'), ('HTML table'), ('XML'), ('SQL INSERTs'), ('SQL REPLACEs'), ('LaTeX'), ('Wiki markup'), ('PHP Array'));
    procedure SaveDialogTypeChange(Sender: TObject);
    function GetExportFormat: TGridExportFormat;
    procedure SetExportFormat(Value: TGridExportFormat);
    procedure SetExportFormatByFilename;
  public
    { Public declarations }
    property Grid: TVirtualStringTree read FGrid write FGrid;
    property ExportFormat: TGridExportFormat read GetExportFormat write SetExportFormat;
  end;


implementation

uses main, helpers, dbconnection, mysql_structures;

{$R *.dfm}



procedure TfrmExportGrid.FormCreate(Sender: TObject);
var
  FormatDesc: String;
begin
  TranslateComponent(Self);
  InheritFont(Font);
  editFilename.Text := AppSettings.ReadString(asGridExportFilename);
  radioOutputCopyToClipboard.Checked := AppSettings.ReadBool(asGridExportOutputCopy);
  radioOutputFile.Checked := AppSettings.ReadBool(asGridExportOutputFile);
  FRecentFiles := Explode(DELIM, AppSettings.ReadString(asGridExportRecentFiles));
  comboEncoding.Items.Assign(MainForm.FileEncodings);
  comboEncoding.Items.Delete(0); // Remove "Auto detect"
  comboEncoding.ItemIndex := AppSettings.ReadInt(asGridExportEncoding);
  grpFormat.Items.Clear;
  for FormatDesc in FFormatToDescription do
    grpFormat.Items.Add(FormatDesc);
  grpFormat.ItemIndex := AppSettings.ReadInt(asGridExportFormat);
  grpSelection.ItemIndex := AppSettings.ReadInt(asGridExportSelection);
  chkIncludeColumnNames.Checked := AppSettings.ReadBool(asGridExportColumnNames);
  chkIncludeQuery.Checked := AppSettings.ReadBool(asGridExportIncludeQuery);
  FCSVSeparator := AppSettings.ReadString(asGridExportSeparator);
  FCSVEncloser := AppSettings.ReadString(asGridExportEncloser);
  FCSVTerminator := AppSettings.ReadString(asGridExportTerminator);
  ValidateControls(Sender);
end;


procedure TfrmExportGrid.FormDestroy(Sender: TObject);
begin
  // Store settings
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
  end;
end;


procedure TfrmExportGrid.FormShow(Sender: TObject);
begin
  // Show dialog. Expect "Grid" property to be set now by the caller.
  chkIncludeAutoIncrement.Checked := AppSettings.ReadBool(asGridExportIncludeAutoInc);
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
    end;
    efCSV: begin
      editSeparator.Text := FCSVSeparator;
      editEncloser.Text := FCSVEncloser;
      editTerminator.Text := FCSVTerminator;
    end;
    else begin
      editSeparator.Text := '';
      editEncloser.Text := '';
      editTerminator.Text := '';
    end;
  end;

  chkIncludeQuery.Enabled := ExportFormat in [efHTML, efXML];
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
  btnOK.Enabled := radioOutputCopyToClipboard.Checked or (radioOutputFile.Checked and (editFilename.Text <> ''));
  if radioOutputFile.Checked then
    editFilename.Font.Color := clWindowText
  else
    editFilename.Font.Color := clGrayText;
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
begin
  // Select file target
  Dialog := TSaveDialog.Create(Self);
  Dialog.InitialDir := ExtractFilePath(editFilename.Text);
  Dialog.FileName := ExtractBaseFileName(editFilename.Text);
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
begin
  // Clear and populate drop down menu with recent files
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
end;


procedure TfrmExportGrid.SelectRecentFile(Sender: TObject);
begin
  // Select file from recently used files
  editFilename.Text := (Sender as TMenuItem).Hint;
  SetExportFormatByFilename;
end;


procedure TfrmExportGrid.CalcSize(Sender: TObject);
var
  GridData: TDBQuery;
  Node: PVirtualNode;
  Col, ExcludeCol: TColumnIndex;
  RowNum: PCardinal;
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
  grpSelection.Items[0] := f_('Selection (%d rows, %s)', [FormatNumber(Grid.SelectedCount), FormatByteNumber(SelectionSize)]);
  grpSelection.Items[1] := f_('Complete (%d rows, %s)', [FormatNumber(Grid.RootNodeCount), FormatByteNumber(AllSize)]);
end;


procedure TfrmExportGrid.editCSVChange(Sender: TObject);
var
  Edit: TButtonedEdit;
begin
  // Remember csv settings
  Edit := Sender as TButtonedEdit;
  if ExportFormat = efCSV then begin
    if Edit = editSeparator then
      FCSVSeparator := Edit.Text
    else if Edit = editEncloser then
      FCSVEncloser := Edit.Text
    else if Edit = editTerminator then
      FCSVTerminator := Edit.Text;
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
    Item.OnClick := menuCSVClick;
    Item.Checked := FCSVEditor.Text = Item.Hint;
  end;
  popupCSVchar.Popup(p.X-16, p.Y);
end;


procedure TfrmExportGrid.menuCSVClick(Sender: TObject);
begin
  // Insert char from menu
  FCSVEditor.Text := TMenuItem(Sender).Hint;
end;


procedure TfrmExportGrid.btnOKClick(Sender: TObject);
var
  Col, ExcludeCol: TColumnIndex;
  Header, Data, tmp, Encloser, Separator, Terminator, TableName: String;
  Node: PVirtualNode;
  GridData: TDBQuery;
  SelectionOnly: Boolean;
  i: Integer;
  NodeCount: Cardinal;
  RowNum: PCardinal;
  HTML: TStream;
  S: TStringStream;
  Exporter: TSynExporterHTML;
  Encoding: TEncoding;
begin
  // Confirmation dialog if file exists
  if radioOutputFile.Checked
    and FileExists(editFilename.Text)
    and (MessageDialog(_('File exists'), f_('Overwrite file %s?', [editFilename.Text]), mtConfirmation, [mbYes, mbCancel]) = mrCancel)
    then begin
      ModalResult := mrNone;
      Exit;
  end;

  Screen.Cursor := crHourglass;

  SelectionOnly := grpSelection.ItemIndex = 0;
  Mainform.DataGridEnsureFullRows(Grid, SelectionOnly);
  GridData := Mainform.GridResult(Grid);
  if SelectionOnly then
    NodeCount := Grid.SelectedCount
  else
    NodeCount := Grid.RootNodeCount;
  MainForm.EnableProgress(NodeCount);
  TableName := BestTableName(GridData);
  ExcludeCol := -1;
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
      Header := '<?xml version="1.0" encoding="'+MainForm.GetCharsetByEncoding(Encoding)+'"?>' + CRLF + CRLF;
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

      efSQLInsert, efSQLReplace: begin
        if ExportFormat = efSQLInsert then
          tmp := 'INSERT'
        else
          tmp := 'REPLACE';
        tmp := tmp + ' INTO '+GridData.Connection.QuoteIdent(Tablename);
        if chkIncludeColumnNames.Checked then begin
          tmp := tmp + ' (';
          Col := Grid.Header.Columns.GetFirstVisibleColumn;
          while Col > NoColumn do begin
            if Col <> ExcludeCol then
              tmp := tmp + GridData.Connection.QuoteIdent(Grid.Header.Columns[Col].Text)+', ';
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Delete(tmp, Length(tmp)-1, 2);
          tmp := tmp + ')';
        end;
        tmp := tmp + ' VALUES (';
      end;

      efWiki: tmp := TrimLeft(Separator);

      efPHPArray: tmp := #9 + 'array( // row #'+FormatNumber(GridData.RecNo)+CRLF;

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
          and (ExportFormat in [efExcel, efHTML]) then
            Data := FormatNumber(Data, False);

        case ExportFormat of
          efHTML: begin
            // Escape HTML control characters in data.
            Data := HTMLSpecialChars(Data);
            tmp := tmp + '          <td class="col' + IntToStr(Col) + '">' + Data + '</td>' + CRLF;
          end;

          efExcel, efCSV, efLaTeX, efWiki: begin
            // Escape encloser characters inside data per de-facto CSV.
            Data := StringReplace(Data, Encloser, Encloser+Encloser, [rfReplaceAll]);
            if GridData.IsNull(Col) and (ExportFormat = efCSV) then
              Data := '\N'
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

          efSQLInsert, efSQLReplace: begin
            if GridData.IsNull(Col) then
              Data := 'NULL'
            else if GridData.DataType(Col).Index = dtBit then
              Data := 'b' + esc(Data)
            else if not (GridData.DataType(Col).Category in [dtcInteger, dtcReal, dtcBinary, dtcSpatial]) then
              Data := esc(Data);
            tmp := tmp + Data + ', ';
          end;

          efPHPArray: begin
            if GridData.IsNull(Col) then
              Data := 'NULL'
            else if not (GridData.DataType(Col).Category in [dtcInteger, dtcReal]) then
              Data := esc(Data);
            if chkIncludeColumnNames.Checked then
              tmp := tmp + #9#9 + '''' + Grid.Header.Columns[Col].Text + ''' => ' + Data + ','+CRLF
            else
              tmp := tmp + #9#9 + Data + ','+CRLF;
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
      efSQLInsert, efSQLReplace: begin
        Delete(tmp, Length(tmp)-1, 2);
        tmp := tmp + ');' + CRLF;
      end;
      efPHPArray:
        tmp := tmp + #9 + '),' + CRLF;
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
    else
      tmp := '';
  end;
  S.WriteString(tmp);

  if radioOutputCopyToClipboard.Checked then begin
    // SynEdit's exporter is slow on large strings, see issue #2903
    if S.Size < 100*SIZE_KB then begin
      case ExportFormat of
        efSQLInsert, efSQLReplace: begin
          Exporter := TSynExporterHTML.Create(Self);
          Exporter.Highlighter := MainForm.SynSQLSyn1;
          Exporter.ExportAll(Explode(CRLF, S.DataString));
          HTML := TMemoryStream.Create;
          Exporter.SaveToStream(HTML);
          Exporter.Free;
        end;
        efHTML: HTML := S;
        else HTML := nil;
      end;
    end;
    StreamToClipboard(S, HTML, (ExportFormat=efHTML) and (HTML <> nil));
  end else begin
    try
      S.SaveToFile(editFilename.Text);
    except
      on E:EFCreateError do begin
        // Keep form open if file cannot be created
        ModalResult := mrNone;
        MainForm.SetProgressState(pbsError);
        ErrorDialog(E.Message);
      end;
    end;
  end;

  Mainform.DisableProgress;
  Mainform.ShowStatusMsg(_('Freeing data...'));
  FreeAndNil(S);
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


end.
