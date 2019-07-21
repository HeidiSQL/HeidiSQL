unit insertfiles;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  ShellApi, Math, Graphics, ComCtrls, ToolWin, extra_controls,
  dbconnection, dbstructures, VirtualTrees, grideditlinks, SynRegExpr, gnugettext, apphelpers;

type
  TColInfo = class
    public
      Name: String;
      Datatype: TDBDatatype;
      Value: String;
  end;
  PColInfo = ^TColInfo;

  TFileInfo = class(TObject)
    public
      Filename: String;
      ImageIndex: Integer;
      Size: Int64;
      IsBinary: Boolean;
  end;
  PFileInfo = ^TFileInfo;

  TfrmInsertFiles = class(TExtForm)
    btnInsert: TButton;
    btnCancel: TButton;
    OpenDialog: TOpenDialog;
    grpSelectObject: TGroupBox;
    lblTable: TLabel;
    comboDBs: TComboBox;
    comboTables: TComboBox;
    GroupBox2: TGroupBox;
    lblDropHint: TLabel;
    lblFileCount: TLabel;
    ListFiles: TVirtualStringTree;
    ToolBar1: TToolBar;
    btnAddFiles: TToolButton;
    btnRemoveFiles: TToolButton;
    btnClearFiles: TToolButton;
    ListColumns: TVirtualStringTree;
    lblFilecontents: TLabel;
    procedure FormShow(Sender: TObject);
    procedure comboDBsChange(Sender: TObject);
    procedure comboTablesChange(Sender: TObject);
    procedure Modified;
    procedure btnAddFilesClick(Sender: TObject);
    procedure btnRemoveFilesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnInsertClick(Sender: TObject);
    procedure AddFile(Filename: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListFilesGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ListFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure ListFilesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure ListFilesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure ListFilesCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure ListFilesDblClick(Sender: TObject);
    procedure ListFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ListFilesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure ListFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure ListFilesStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Reason: TChangeReason);
    procedure ListColumnsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ListColumnsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure ListColumnsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure GridAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
    procedure GridClick(Sender: TObject);
    procedure GridKeyPress(Sender: TObject; var Key: Char);
    procedure GridHandleClickOrKeyPress(Sender: TVirtualStringTree;
      Node: PVirtualNode; Column: TColumnIndex; HitPositions: THitPositions);
    procedure ListColumnsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure ListColumnsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure ListColumnsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      NewText: string);
    procedure ListColumnsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure FormDestroy(Sender: TObject);
  private
    FConnection: TDBConnection;
    FMaxFileSize: Int64;
  public
    { Public declarations }
    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;
  end;


implementation

uses main;

const
  ColColname = 0;
  ColDatatype = 1;
  ColValue = 2;
  ColFilename = 0;
  ColBinary = 1;
  ColFilesize = 2;

{$R *.DFM}



procedure TfrmInsertFiles.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  ListFiles.Images := GetSystemImageList;
  DragAcceptFiles(Handle, True);
  MainForm.RestoreListSetup(ListColumns);
  MainForm.RestoreListSetup(ListFiles);
  FixVT(ListFiles);
  FixVT(ListColumns);
end;


procedure TfrmInsertFiles.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asFileImportWindowWidth, Width);
  AppSettings.WriteInt(asFileImportWindowHeight, Height);
  MainForm.SaveListSetup(ListColumns);
  MainForm.SaveListSetup(listFiles);
end;


procedure TfrmInsertFiles.FormShow(Sender: TObject);
begin
  Width := AppSettings.ReadInt(asFileImportWindowWidth);
  Height := AppSettings.ReadInt(asFileImportWindowHeight);
  FConnection := Mainform.ActiveConnection;
  Caption := FConnection.Parameters.SessionName + ' - ' + MainForm.actInsertFiles.Caption;
  comboDBs.Items.Clear;
  comboDBs.Items.Assign(FConnection.AllDatabases);
  comboDBs.ItemIndex := comboDBs.Items.IndexOf(FConnection.Database);
  if comboDBs.ItemIndex = -1 then
    comboDBs.ItemIndex := 0;
  comboDBs.OnChange(Sender);
  ListFilesChange(ListFiles, nil);
end;


procedure TfrmInsertFiles.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TfrmInsertFiles.ListColumnsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ColInfo: PColInfo;
begin
  // Free some memory
  ColInfo := Sender.GetNodeData(Node);
  ColInfo.Free;
end;


procedure TfrmInsertFiles.ListColumnsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TColInfo);
end;


procedure TfrmInsertFiles.ListColumnsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  ColInfo: PColInfo;
begin
  // Display cell value
  ColInfo := Sender.GetNodeData(Node);
  case Column of
    ColColname: CellText := ColInfo.Name;
    ColDatatype: CellText := ColInfo.Datatype.Name;
    ColValue: CellText := ColInfo.Value;
  end;
end;


procedure TfrmInsertFiles.ListColumnsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
var
  ColInfo: PColInfo;
begin
  // Datatype specific font color
  if Column = ColDatatype then begin
    ColInfo := Sender.GetNodeData(Node);
    TargetCanvas.Font.Color := DatatypeCategories[ColInfo.DataType.Category].Color;
  end;
end;


procedure TfrmInsertFiles.GridAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellRect: TRect);
var
  ImageIndex, X, Y: Integer;
  Grid: TVirtualStringTree;
  FileInfo: PFileInfo;
  PaintCheckbox, CheckState: Boolean;
begin
  // Paint checkbox image in certain columns
  Grid := Sender as TVirtualStringTree;
  PaintCheckbox := False;
  CheckState := False;
  if (Grid = listFiles) and (Column = ColBinary) then begin
    FileInfo := Sender.GetNodeData(Node);
    CheckState := FileInfo.IsBinary;
    PaintCheckbox := True;
  end;

  if PaintCheckbox then begin
    if CheckState then
      ImageIndex := 128
    else
      ImageIndex := 127;
    X := CellRect.Left + (Grid.Header.Columns[Column].Width div 2) - (MainForm.VirtualImageListMain.Width div 2);
    Y := CellRect.Top + Integer(Grid.NodeHeight[Node] div 2) - (MainForm.VirtualImageListMain.Height div 2);
    MainForm.VirtualImageListMain.Draw(TargetCanvas, X, Y, ImageIndex);
  end;
end;


procedure TfrmInsertFiles.ListColumnsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  // Editor only allowed for "value"
  Allowed := Column = ColValue;
end;


procedure TfrmInsertFiles.GridKeyPress(Sender: TObject; var Key: Char);
var
  Grid: TVirtualStringTree;
begin
  // Space/click on checkbox column
  Grid := Sender as TVirtualStringTree;
  if Ord(Key) = VK_SPACE then
    GridHandleClickOrKeyPress(Grid, Grid.FocusedNode, Grid.FocusedColumn, []);
end;


procedure TfrmInsertFiles.GridClick(Sender: TObject);
var
  Grid: TVirtualStringTree;
  Click: THitInfo;
begin
  // Handle click event
  Grid := Sender as TVirtualStringTree;
  Grid.GetHitTestInfoAt(Mouse.CursorPos.X-Grid.ClientOrigin.X, Mouse.CursorPos.Y-Grid.ClientOrigin.Y, True, Click);
  GridHandleClickOrKeyPress(Grid, Click.HitNode, Click.HitColumn, Click.HitPositions);
end;


procedure TfrmInsertFiles.ListColumnsCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; out EditLink: IVTEditLink);
var
  Grid: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
begin
  // Start cell editor
  Grid := Sender as TVirtualStringTree;
  if Column = ColValue then begin
    EnumEditor := TEnumEditorLink.Create(Grid);
    EnumEditor.AllowCustomText := True;
    EnumEditor.ValueList := TStringList.Create;
    EnumEditor.ValueList.Text := 'NULL'+CRLF+
      '''%filecontent%'''+CRLF+
      '''%filename%'''+CRLF+
      '''%filepath%'''+CRLF+
      '''%filesize%'''+CRLF+
      '''%filedate%'''+CRLF+
      '''%filedatetime%'''+CRLF+
      '''%filetime%'''+CRLF+
      'NOW()'+CRLF+
      'LOWER(''%filename%'')'+CRLF+
      'UPPER(''%filenname%'')'+CRLF+
      'UNIX_TIMESTAMP(''%filedatetime%'')'+CRLF+
      'ENCODE(''%filename%'', ''password'')';
    EditLink := EnumEditor;
  end;
end;


procedure TfrmInsertFiles.ListColumnsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var
  ColInfo: PColInfo;
begin
  // User entered new value
  ColInfo := Sender.GetNodeData(Node);
  if Column = ColValue then
    ColInfo.Value := NewText;
end;


procedure TfrmInsertFiles.GridHandleClickOrKeyPress(Sender: TVirtualStringTree;
  Node: PVirtualNode; Column: TColumnIndex; HitPositions: THitPositions);
var
  FileInfo: PFileInfo;
  Grid: TVirtualStringTree;
begin
  // "quote" checkbox, cell editor disabled, instead toggle its state
  if (not Assigned(Node)) or (Column = NoColumn) then
    Exit;
  Grid := Sender as TVirtualStringTree;
  if (Grid = listFiles) and (Column = ColBinary) then begin
    FileInfo := Grid.GetNodeData(Node);
    FileInfo.IsBinary := not FileInfo.IsBinary;
    Grid.OnChange(Sender, Node);
  end;
  Grid.InvalidateNode(Node);
end;


procedure TfrmInsertFiles.comboDBsChange(Sender: TObject);
var
  DBObjects: TDBObjectList;
  i: Integer;
begin
  // read tables from db
  comboTables.Items.Clear;
  DBObjects := FConnection.GetDBObjects(comboDBs.Text);
  for i:=0 to DBObjects.Count-1 do begin
    if DBObjects[i].NodeType in [lntTable, lntView] then
      comboTables.Items.Add(DBObjects[i].Name);
  end;
  if comboTables.Items.Count > 0 then
    comboTables.ItemIndex := 0;
  comboTables.OnChange(Sender);
end;


procedure TfrmInsertFiles.comboTablesChange(Sender: TObject);
var
  Selected: TDBObject;
  Columns: TTableColumnList;
  Col: TTableColumn;
  ColInfo: TColInfo;
  Node: PVirtualNode;
begin
  // Populate combobox with columns from selected table
  ListColumns.Clear;
  if comboTables.ItemIndex > -1 then begin
    Columns := TTableColumnList.Create(True);
    Selected := FConnection.FindObject(comboDBs.Text, comboTables.Text);
    FConnection.ParseTableStructure(Selected.CreateCode, Columns, nil, nil);
    Node := nil;
    for Col in Columns do begin
      ColInfo := TColInfo.Create;
      ColInfo.Name := Col.Name;
      ColInfo.Datatype := Col.DataType;
      ColInfo.Value := '';
      Node := ListColumns.InsertNode(Node, amInsertAfter, PColInfo(ColInfo));
    end;
    Modified;
  end;
end;


procedure TfrmInsertFiles.Modified;
begin
  // Buttons need to be checked for being enabled
  btnInsert.Enabled := (ListColumns.RootNodeCount > 0) and (ListFiles.RootNodeCount > 0);
end;


procedure TfrmInsertFiles.AddFile(Filename: String);
var
  FileInfo: TFileInfo;
  NewNode: PVirtualNode;
  rx: TRegExpr;
begin
  if DirectoryExists(filename) then
    Exit;
  FileInfo := TFileInfo.Create;
  FileInfo.Filename := Filename;
  FileInfo.ImageIndex := GetSystemImageIndex(Filename);
  FileInfo.Size := _GetFileSize(Filename);
  rx := TRegExpr.Create;
  // Decide if file is binary by excluding common text format file extensions
  rx.Expression := '\.(te?xt|html?|xml|cmd|bat|sql|ini|pas|php|h|conf|log|csv|reg|latex|wiki|patch)$';
  rx.ModifierI := True;
  FileInfo.IsBinary := not rx.Exec(Filename);
  NewNode := ListFiles.InsertNode(ListFiles.FocusedNode, amInsertAfter, PFileInfo(FileInfo));
  SelectNode(ListFiles, NewNode);
end;


procedure TfrmInsertFiles.btnAddFilesClick(Sender: TObject);
var
  i: Integer;
begin
  // Add file(s) to list
  if OpenDialog.Execute then begin
    Screen.Cursor := crHourglass;
    ListFiles.BeginUpdate;
    try
      for i:=0 to OpenDialog.Files.Count-1 do
        AddFile(OpenDialog.Files[i]);
    finally
      ListFiles.EndUpdate;
    end;
    Screen.Cursor := crDefault;
  end;
end;


procedure TfrmInsertFiles.ListFilesBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
  FileInfo: PFileInfo;
begin
  // Display bar in filesize column
  if Column = ColFilesize then begin
    FileInfo := Sender.GetNodeData(Node);
    MainForm.PaintColorBar(FileInfo.Size, FMaxFileSize, TargetCanvas, CellRect);
  end;
end;


procedure TfrmInsertFiles.ListFilesStructureChange(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Reason: TChangeReason);
begin
  (Sender as TVirtualStringTree).OnChange(Sender, Node);
end;


procedure TfrmInsertFiles.ListFilesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  FileInfo: PFileInfo;
  Bytes: Int64;
  Grid: TVirtualStringTree;
  LNode: PVirtualNode;
  Binaries: Int64;
  CheckState: TCheckState;
begin
  // Node focus or selection has changed
  Grid := Sender as TVirtualStringTree;
  btnRemoveFiles.Enabled := Grid.SelectedCount > 0;
  btnClearFiles.Enabled := Grid.RootNodeCount > 0;
  LNode := Grid.GetFirst;
  Bytes := 0;
  FMaxFileSize := 0;
  Binaries := 0;
  while Assigned(LNode) do begin
    FileInfo := Grid.GetNodeData(LNode);
    Inc(Bytes, FileInfo.Size);
    FMaxFileSize := Max(FMaxFileSize, FileInfo.Size);
    if FileInfo.IsBinary then
      Inc(Binaries);
    LNode := Grid.GetNextSibling(LNode);
  end;
  // Set column header checkstate
  if Binaries = 0 then
    CheckState := csUncheckedNormal
  else if Binaries = Grid.RootNodeCount then
    CheckState := csCheckedNormal
  else
    CheckState := csMixedNormal;
  Grid.Header.Columns[ColBinary].CheckState := CheckState;

  lblFileCount.Caption := f_('%u files, %s, %u files selected.', [Grid.RootNodeCount, FormatByteNumber(Bytes), Grid.SelectedCount]);
  Modified;
end;


procedure TfrmInsertFiles.ListFilesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
begin
  MainForm.AnyGridCompareNodes(Sender, Node1, Node2, Column, Result);
end;


procedure TfrmInsertFiles.ListFilesDblClick(Sender: TObject);
var
  FileInfo: PFileInfo;
begin
  // Run file on doubleclick
  if Assigned(ListFiles.FocusedNode) then begin
    FileInfo := ListFiles.GetNodeData(ListFiles.FocusedNode);
    ShellExec(FileInfo.Filename);
  end;
end;


procedure TfrmInsertFiles.ListFilesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  FileInfo: PFileInfo;
begin
  // Free some memory when file gets removed
  FileInfo := Sender.GetNodeData(Node);
  FileInfo.Free;
end;


procedure TfrmInsertFiles.btnRemoveFilesClick(Sender: TObject);
begin
  // Remove selected or all files from list
  Screen.Cursor := crHourglass;
  if Sender = btnRemoveFiles then
    ListFiles.DeleteSelectedNodes
  else
    ListFiles.Clear;
  Screen.Cursor := crDefault;
end;


procedure TfrmInsertFiles.ListFilesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  FileInfo: PFileInfo;
  Grid: TVirtualStringTree;
begin
  Grid := Sender as TVirtualStringTree;
  if not (Kind in [ikNormal, ikSelected]) then
    Exit;
  if Column <> Grid.Header.MainColumn then
    Exit;
  FileInfo := Sender.GetNodeData(Node);
  ImageIndex := FileInfo.ImageIndex;
end;


procedure TfrmInsertFiles.ListFilesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TFileInfo);
end;


procedure TfrmInsertFiles.ListFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  FileInfo: PFileInfo;
begin
  FileInfo := Sender.GetNodeData(Node);
  case Column of
    ColFilename: CellText := FileInfo.Filename;
    ColBinary: CellText := '';
    ColFilesize: CellText := FormatByteNumber(FileInfo.Size);
  end;
end;


procedure TfrmInsertFiles.ListFilesHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  Node: PVirtualNode;
  FileInfo: PFileInfo;
  CheckState: TCheckState;
begin
  // Header column click / check
  MainForm.AnyGridHeaderClick(Sender, HitInfo);
  CheckState := Sender.Columns[HitInfo.Column].CheckState;
  if (HitInfo.Column = ColBinary) and (not (CheckState in [csMixedNormal, csMixedPressed])) then begin
    Node := Sender.Treeview.GetFirst;
    while Assigned(Node) do begin
      FileInfo := Sender.Treeview.GetNodeData(Node);
      FileInfo.IsBinary := CheckState in CheckedStates;
      Node := Sender.Treeview.GetNextSibling(Node);
    end;
    Sender.Treeview.InvalidateChildren(nil, false);
  end;
end;


procedure TfrmInsertFiles.ListFilesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Remove focused file
  if Key = VK_DELETE then
    btnRemoveFiles.OnClick(btnRemoveFiles);
end;


procedure TfrmInsertFiles.btnInsertClick(Sender: TObject);
const
  ChunkSize = 131072;
var
  Value, sql, FileContent: String;
  FileDate: TDateTime;
  y, m, d, h, mi, s, ms: Word;
  Node, ColNode, DoneNode: PVirtualNode;
  FileInfo: PFileInfo;
  ColInfo: PColInfo;
  FileReadDone: Boolean;
  FileSize: Int64;
begin
  // Insert files
  Screen.Cursor := crHourglass;
  MainForm.EnableProgress(ListFiles.RootNodeCount);

  Node := ListFiles.GetFirst;
  while Assigned(Node) do begin
    ListFiles.FocusedNode := Node;
    FileInfo := ListFiles.GetNodeData(Node);
    FileSize := _GetFileSize(FileInfo.Filename);
    FileReadDone := False;
    sql := 'INSERT INTO '+FConnection.QuotedDbAndTableName(comboDBs.Text, comboTables.Text) + ' (';
    ColNode := ListColumns.GetFirst;
    while Assigned(ColNode) do begin
      ColInfo := ListColumns.GetNodeData(ColNode);
      if ColInfo.Value <> '' then
        sql := sql + FConnection.QuoteIdent(ColInfo.Name) + ', ';
      ColNode := ListColumns.GetNextSibling(ColNode);
    end;
    Delete(sql, Length(sql)-1, 2);
    sql := sql + ') VALUES (';

    ColNode := ListColumns.GetFirst;
    while Assigned(ColNode) do begin
      ColInfo := ListColumns.GetNodeData(ColNode);
      if ColInfo.Value <> '' then begin
        Value := ColInfo.Value;
        if pos('%', Value) > 0 then begin

          if Pos('%filecontent%', ColInfo.Value) > 0 then begin
            if not FileReadDone then begin
              // Import binaries as-is (byte for byte), and auto-detect encoding of text files.
              if FileInfo.IsBinary then begin
                FileContent := '';
                if FConnection.Parameters.IsMySQL then
                  FileContent := '_binary ';
                FileContent := FileContent + '0x' + BinToWideHex(ReadBinaryFile(FileInfo.Filename, 0))
              end else
                FileContent := esc(ReadTextfile(FileInfo.Filename, nil));
              FileReadDone := True;
            end;
            Value := FileContent;
          end else begin
            Value := StringReplace(Value, '%filesize%', IntToStr(FileSize), [rfReplaceAll]);
            Value := StringReplace(Value, '%filename%', esc(ExtractFileName(FileInfo.Filename), False, False), [rfReplaceAll]);
            Value := StringReplace(Value, '%filepath%', esc(ExtractFilePath(FileInfo.Filename), False, False), [rfReplaceAll]);
            FileAge(FileInfo.Filename, FileDate);
            DecodeDate(FileDate, y, m, d);
            DecodeTime(FileDate, h, mi, s, ms);
            Value := StringReplace(Value, '%filedate%', esc(Format('%.4d-%.2d-%.2d', [y,m,d]), False, False), [rfReplaceAll]);
            Value := StringReplace(Value, '%filedatetime%', esc(Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,mi,s]), False, False), [rfReplaceAll]);
            Value := StringReplace(Value, '%filetime%', esc(Format('%.2d:%.2d:%.2d', [h,mi,s]), False, False), [rfReplaceAll]);
          end;
        end;
        sql := sql + Value + ', ';
      end;
      ColNode := ListColumns.GetNextSibling(ColNode);
    end;
    // Strip last comma + space
    Delete(sql, Length(sql)-1, 2);
    sql := sql + ')';
    try
      FConnection.Query(sql);
      Mainform.ProgressStep;
    except
      on E:EDbError do begin
        Screen.Cursor := crDefault;
        MainForm.SetProgressState(pbsError);
        ErrorDialog(E.Message);
        ModalResult := mrNone;
        break;
      end;
    end;
    DoneNode := Node;
    Node := ListFiles.GetNextSibling(Node);
    ListFiles.DeleteNode(DoneNode);
  end;
  Screen.Cursor := crDefault;
  MainForm.DisableProgress;
end;


procedure TfrmInsertFiles.AcceptFiles(var msg : TMessage);
const
  MaxFileNameLen = 255;
var
  i, FileCount: integer;
  FileName: array [0..MaxFileNameLen] of char;
begin
  // Files dropped onto form
  Screen.Cursor := crHourglass;
  FileCount := DragQueryFile(msg.WParam, $FFFFFFFF, FileName, MaxFileNameLen);
  // Query Windows one at a time for the file name
  ListFiles.BeginUpdate;
  try
    for i:=0 to FileCount-1 do begin
      DragQueryFile(msg.WParam, i, FileName, MaxFileNameLen);
      AddFile(FileName);
    end;
  finally
    ListFiles.EndUpdate;
    Screen.Cursor := crDefault;
  end;
  DragFinish(msg.WParam);
end;


end.
