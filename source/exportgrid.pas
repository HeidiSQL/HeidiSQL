unit exportgrid;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, VirtualTrees, SynExportHTML;

type
  TGridExportFormat = (efExcel, efCSV, efHTML, efXML, efSQLInsert, efSQLReplace, efLaTeX, efWiki);

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
    chkColumnHeader: TCheckBox;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  private
    { Private declarations }
    FCSVEditor: TButtonedEdit;
    FCSVSeparator, FCSVEncloser, FCSVTerminator: String;
    FGrid: TVirtualStringTree;
    FRecentFiles: TStringList;
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
begin
  InheritFont(Font);
  OpenRegistry;
  editFilename.Text := GetRegValue(REGNAME_GEXP_FILENAME, editFilename.Text);
  radioOutputCopyToClipboard.Checked := GetRegValue(REGNAME_GEXP_OUTPUTCOPY, radioOutputCopyToClipboard.Checked);
  radioOutputFile.Checked := GetRegValue(REGNAME_GEXP_OUTPUTFILE, radioOutputFile.Checked);
  FRecentFiles := Explode(DELIM, GetRegValue(REGNAME_GEXP_RECENTFILES, ''));
  comboEncoding.Items.Assign(MainForm.FileEncodings);
  comboEncoding.Items.Delete(0); // Remove "Auto detect"
  comboEncoding.ItemIndex := GetRegValue(REGNAME_GEXP_ENCODING, 4);
  grpFormat.ItemIndex := GetRegValue(REGNAME_GEXP_FORMAT, grpFormat.ItemIndex);
  chkColumnHeader.Checked := GetRegValue(REGNAME_GEXP_COLUMNNAMES, chkColumnHeader.Checked);
  FCSVSeparator := GetRegValue(REGNAME_GEXP_SEPARATOR, editSeparator.Text);
  FCSVEncloser := GetRegValue(REGNAME_GEXP_ENCLOSER, editEncloser.Text);
  FCSVTerminator := GetRegValue(REGNAME_GEXP_TERMINATOR, editTerminator.Text);
  ValidateControls(Sender);
end;


procedure TfrmExportGrid.FormDestroy(Sender: TObject);
begin
  // Store settings
  if ModalResult = mrOK then begin
    MainReg.WriteBool(REGNAME_GEXP_OUTPUTCOPY, radioOutputCopyToClipboard.Checked);
    MainReg.WriteBool(REGNAME_GEXP_OUTPUTFILE, radioOutputFile.Checked);
    MainReg.WriteString(REGNAME_GEXP_FILENAME, editFilename.Text);
    MainReg.WriteString(REGNAME_GEXP_RECENTFILES, ImplodeStr(DELIM, FRecentFiles));
    MainReg.WriteInteger(REGNAME_GEXP_ENCODING, comboEncoding.ItemIndex);
    MainReg.WriteInteger(REGNAME_GEXP_FORMAT, grpFormat.ItemIndex);
    MainReg.WriteInteger(REGNAME_GEXP_SELECTION, grpSelection.ItemIndex);
    MainReg.WriteBool(REGNAME_GEXP_COLUMNNAMES, chkColumnHeader.Checked);
    MainReg.WriteString(REGNAME_GEXP_SEPARATOR, FCSVSeparator);
    MainReg.WriteString(REGNAME_GEXP_ENCLOSER, FCSVEncloser);
    MainReg.WriteString(REGNAME_GEXP_TERMINATOR, FCSVTerminator);
  end;
end;


procedure TfrmExportGrid.FormShow(Sender: TObject);
var
  GridData: TDBQuery;
  Node: PVirtualNode;
  Col: TColumnIndex;
  RowNum: PCardinal;
  SelectionSize, AllSize: Int64;
begin
  // Show dialog. Expect "Grid" property to be set now by the caller.
  grpSelection.Items.Clear;
  GridData := Mainform.GridResult(Grid);
  AllSize := 0;
  SelectionSize := 0;
  Node := GetNextNode(Grid, nil, False);
  while Assigned(Node) do begin
    RowNum := Grid.GetNodeData(Node);
    GridData.RecNo := RowNum^;
    Col := Grid.Header.Columns.GetFirstVisibleColumn;
    while Col > NoColumn do begin
      Inc(AllSize, GridData.ColumnLengths(Col));
      if vsSelected in Node.States then
        Inc(SelectionSize, GridData.ColumnLengths(Col));
      Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
    end;
    Node := GetNextNode(Grid, Node, False);
  end;
  grpSelection.Items.Add('Selection ('+FormatNumber(Grid.SelectedCount)+' rows, '+FormatByteNumber(SelectionSize)+')');
  grpSelection.Items.Add('Complete ('+FormatNumber(Grid.RootNodeCount)+' rows, '+FormatByteNumber(AllSize)+')');
  grpSelection.ItemIndex := GetRegValue(REGNAME_GEXP_SELECTION, grpSelection.ItemIndex);
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
end;


function TfrmExportGrid.GetExportFormat: TGridExportFormat;
begin
  Result := TGridExportFormat(grpFormat.ItemIndex);
end;


procedure TfrmExportGrid.SetExportFormat(Value: TGridExportFormat);
begin
  grpFormat.ItemIndex := Integer(Value);
end;


procedure TfrmExportGrid.SetExportFormatByFilename;
var
  ext: String;
begin
  // Set format by file extension
  ext := LowerCase(Copy(ExtractFileExt(editFilename.Text), 2, 10));
  if (ext = 'csv') and (not (ExportFormat in [efExcel, efCSV]))
                         then ExportFormat := efCSV
  else if ext = 'html'   then ExportFormat := efHTML
  else if ext = 'xml'    then ExportFormat := efXML
  else if ext = 'sql'    then ExportFormat := efSQLInsert
  else if ext = 'latex'  then ExportFormat := efLaTeX
  else if ext = 'wiki'   then ExportFormat := efWiki;
end;


procedure TfrmExportGrid.editFilenameChange(Sender: TObject);
begin
  radioOutputFile.Checked := True;
end;


procedure TfrmExportGrid.editFilenameRightButtonClick(Sender: TObject);
var
  Dialog: TSaveDialog;
begin
  // Select file target
  Dialog := TSaveDialog.Create(Self);
  Dialog.InitialDir := ExtractFilePath(editFilename.Text);
  Dialog.FileName := ExtractFileName(editFilename.Text);
  Dialog.FileName := Copy(Dialog.FileName, 1, Length(Dialog.FileName)-Length(ExtractFileExt(Dialog.FileName)));
  Dialog.OnTypeChange := SaveDialogTypeChange;
  Dialog.OnTypeChange(Dialog);
  Dialog.FilterIndex := grpFormat.ItemIndex+1;
  Dialog.Filter := 'Tab separated values (*.csv)|*.csv|'+
    'Comma separated values (*.csv)|*.csv|'+
    'Hypertext markup language (*.html)|*.html|'+
    'Extensible markup language (*.xml)|*.xml|'+
    'Structured query language (*.sql)|*.sql|'+
    'LaTeX table (*.latex)|*.latex|'+
    'Wiki markup table (*.wiki)|*.wiki|'+
    'All files (*.*)|*.*';
  if Dialog.Execute then begin
    editFilename.Text := Dialog.FileName;
    SetExportFormatByFilename;
    ValidateControls(Sender);
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
begin
  // Set default file-extension of saved file and options on the dialog to show
  Dialog := Sender as TSaveDialog;
  case Dialog.FilterIndex of
    1: Dialog.DefaultExt := 'csv';
    2: Dialog.DefaultExt := 'html';
    3: Dialog.DefaultExt := 'xml';
    4: Dialog.DefaultExt := 'sql';
    5: Dialog.DefaultExt := 'LaTeX';
    6: Dialog.DefaultExt := 'wiki';
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
  Col: TColumnIndex;
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
  Screen.Cursor := crHourglass;

  SelectionOnly := grpSelection.ItemIndex = 0;
  Mainform.DataGridEnsureFullRows(Grid, SelectionOnly);
  GridData := Mainform.GridResult(Grid);
  if SelectionOnly then
    NodeCount := Grid.SelectedCount
  else
    NodeCount := Grid.RootNodeCount;
  EnableProgressBar(NodeCount);
  TableName := BestTableName(GridData);

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
        '    <meta http-equiv="Content-Type" content="text/html; charset='+Mainform.GetCharsetByEncoding(Encoding)+'" />' + CRLF +
        '    <style type="text/css">' + CRLF +
        '      thead tr {background-color: ActiveCaption; color: CaptionText;}' + CRLF +
        '      th, td {vertical-align: top; font-family: "'+Grid.Font.Name+'"; font-size: '+IntToStr(Grid.Font.Size)+'pt; padding: '+IntToStr(Grid.TextMargin-1)+'px; }' + CRLF +
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
        '  <body>' + CRLF + CRLF +
        '    <table caption="' + TableName + ' (' + inttostr(NodeCount) + ' rows)">' + CRLF;
      if chkColumnHeader.Checked then begin
        Header := Header +
          '      <thead>' + CRLF +
          '        <tr>' + CRLF;
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
        while Col > NoColumn do begin
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
      if chkColumnHeader.Checked then begin
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
        while Col > NoColumn do begin
          // Alter column name in header if data is not raw.
          Data := Grid.Header.Columns[Col].Text;
          if (GridData.DataType(Col).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
            Data := 'HEX(' + Data + ')';
          // Add header item.
          if Header <> '' then
            Header := Header + Separator;
          Header := Header + Encloser + Data + Encloser;
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header + Terminator;
      end;
    end;

    efXML: begin
      Header := '<?xml version="1.0" encoding="'+MainForm.GetCharsetByEncoding(Encoding)+'"?>' + CRLF + CRLF +
          '<table name="'+TableName+'">' + CRLF;
    end;

    efLaTeX: begin
      Header := '\begin{tabular}';
      Separator := ' & ';
      Encloser := '';
      Terminator := '\\ '+CRLF;
      if chkColumnHeader.Checked then begin
        Header := Header + '{';
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
        while Col > NoColumn do begin
          Header := Header + ' c ';
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Header := Header + '}' + CRLF;
      end;
    end;

    efWiki: begin
      Separator := ' || ';
      Encloser := '';
      Terminator := ' ||'+CRLF;
      if chkColumnHeader.Checked then begin
        Header := '|| ';
        Col := Grid.Header.Columns.GetFirstVisibleColumn;
        while Col > NoColumn do begin
          Header := Header + '*' + Grid.Header.Columns[Col].Text + '*' + Separator;
          Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
        end;
        Delete(Header, Length(Header)-Length(Separator)+1, Length(Separator));
        Header := Header + Terminator;
      end;
    end;
  end;
  S.WriteString(Header);

  Node := GetNextNode(Grid, nil, SelectionOnly);
  while Assigned(Node) do begin
    // Update status once in a while.
    if (Node.Index+1) mod 100 = 0 then begin
      Mainform.ShowStatusMsg('Exporting row '+FormatNumber(Node.Index+1)+' of '+FormatNumber(NodeCount)+
        ' ('+IntToStr(Trunc((Node.Index+1) / NodeCount *100))+'%, '+FormatByteNumber(S.Size)+')'
        );
      Mainform.ProgressBarStatus.Position := Node.Index+1;
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
        if chkColumnHeader.Checked then begin
          tmp := tmp + ' (';
          Col := Grid.Header.Columns.GetFirstVisibleColumn;
          while Col > NoColumn do begin
            tmp := tmp + GridData.Connection.QuoteIdent(Grid.Header.Columns[Col].Text)+', ';
            Col := Grid.Header.Columns.GetNextVisibleColumn(Col);
          end;
          Delete(tmp, Length(tmp)-1, 2);
          tmp := tmp + ')';
        end;
        tmp := tmp + ' VALUES (';
      end;

      efWiki: tmp := TrimLeft(Separator);

      else tmp := '';
    end;

    Col := Grid.Header.Columns.GetFirstVisibleColumn;
    while Col > NoColumn do begin
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
          // Handle nulls.
          if GridData.IsNull(Col) then
            Data := TEXT_NULL;
          // Escape HTML control characters in data.
          Data := htmlentities(Data);
          tmp := tmp + '          <td class="col' + IntToStr(Col) + '">' + Data + '</td>' + CRLF;
        end;

        efExcel, efCSV, efLaTeX, efWiki: begin
          // Escape encloser characters inside data per de-facto CSV.
          Data := StringReplace(Data, Encloser, Encloser+Encloser, [rfReplaceAll]);
          // Special handling for NULL (MySQL-ism, not de-facto CSV: unquote value)
          if GridData.IsNull(Col) then begin
            Data := 'NULL';
            if ExportFormat = efWiki then
              Data := '_'+Data+'_';
          end else
            Data := Encloser + Data + Encloser;
          tmp := tmp + Data + Separator;
        end;

        efXML: begin
          // Print cell start tag.
          tmp := tmp + #9#9'<' + Grid.Header.Columns[Col].Text;
          if GridData.IsNull(Col) then
            tmp := tmp + ' isnull="true" />' + CRLF
          else begin
            if (GridData.DataType(Col).Category in [dtcBinary, dtcSpatial]) and (not Mainform.actBlobAsText.Checked) then
              tmp := tmp + ' format="hex"';
            tmp := tmp + '>' + htmlentities(Data) + '</' + Grid.Header.Columns[Col].Text + '>' + CRLF;
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
    efXML:
      tmp := '</table>' + CRLF;
    efLaTeX:
      tmp := '\end{tabular}' + CRLF;
    else
      tmp := '';
  end;
  S.WriteString(tmp);

  if radioOutputCopyToClipboard.Checked then begin
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
    StreamToClipboard(S, HTML, (ExportFormat=efHTML) and (HTML <> nil));
  end else begin
    try
      S.SaveToFile(editFilename.Text);
    except
      on E:EFCreateError do begin
        // Keep form open if file cannot be created
        ModalResult := mrNone;
        ErrorDialog(E.Message);
      end;
    end;
  end;

  Mainform.ProgressBarStatus.Visible := False;
  Mainform.ShowStatusMsg('Freeing data...');
  FreeAndNil(S);
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


end.
