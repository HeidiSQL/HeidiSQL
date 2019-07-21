unit sqlhelp;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls,
  Buttons, SynMemo, SynEditHighlighter, SynHighlighterURI, extra_controls,
  SynURIOpener, SynEdit, VirtualTrees, Graphics,
  dbconnection, gnugettext;

type
  TfrmSQLhelp = class(TExtForm)
    URIOpenerDescription: TSynURIOpener;
    URIHighlighter: TSynURISyn;
    URIOpenerExample: TSynURIOpener;
    btnSearchOnline: TButton;
    ButtonClose: TButton;
    pnlMain: TPanel;
    pnlLeft: TPanel;
    treeTopics: TVirtualStringTree;
    editFilter: TButtonedEdit;
    pnlRight: TPanel;
    Splitter2: TSplitter;
    Splitter1: TSplitter;
    lblDescription: TLabel;
    lblKeyword: TLabel;
    memoDescription: TSynMemo;
    lblExample: TLabel;
    MemoExample: TSynMemo;
    timerSearch: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure memosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOnlinehelpClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure DoSearch(Sender: TObject);
    procedure treeTopicsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure treeTopicsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure treeTopicsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var ChildCount: Cardinal);
    procedure treeTopicsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure treeTopicsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure treeTopicsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure treeTopicsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure editFilterChange(Sender: TObject);
    procedure editFilterRightButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);

  private
    { Private declarations }
    FKeyword: String;
    FRootTopics: TDBQuery;
    function GetHelpResult(Node: PVirtualNode): TDBQuery;
    procedure SetKeyword(Value: String);
  public
    { Public declarations }
    property Keyword: String read FKeyword write SetKeyword;
  end;

  var
    SqlHelpDialog: TfrmSQLhelp; // global var, so we can apply settings via SetupSynEditors

  const
    DEFAULT_WINDOW_CAPTION      : String = 'Integrated SQL-help' ;
    ICONINDEX_CATEGORY_CLOSED   : Integer = 66;
    ICONINDEX_CATEGORY_OPENED   : Integer = 67;
    ICONINDEX_HELPITEM          : Integer = 68;

implementation

uses apphelpers, main;

{$I const.inc}

{$R *.dfm}


procedure TfrmSQLhelp.FormCreate(Sender: TObject);
begin
  // Set window-layout
  lblKeyword.Font.Style := [fsBold];
  Top := AppSettings.ReadInt(asSQLHelpWindowTop);
  Left := AppSettings.ReadInt(asSQLHelpWindowLeft);
  Width := AppSettings.ReadInt(asSQLHelpWindowWidth);
  Height := AppSettings.ReadInt(asSQLHelpWindowHeight);
  pnlLeft.Width := AppSettings.ReadInt(asSQLHelpPnlLeftWidth);
  memoDescription.Height := AppSettings.ReadInt(asSQLHelpPnlRightTopHeight);
  Caption := DEFAULT_WINDOW_CAPTION;
  FixVT(treeTopics);
  HasSizeGrip := True;

  treeTopics.Clear;
  FreeAndNil(FRootTopics);
  FRootTopics := MainForm.ActiveConnection.GetResults('HELP '+esc('CONTENTS'));
  treeTopics.RootNodeCount := FRootTopics.RecordCount;
end;


procedure TfrmSQLhelp.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure TfrmSQLhelp.treeTopicsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  Results: TDBQuery;
  VT: TVirtualStringTree;
begin
  // Topic selected
  VT := Sender as TVirtualStringTree;
  if not Assigned(VT.FocusedNode) then
    Exit;
  if VT.HasChildren[VT.FocusedNode] then
    Exit;
  FKeyword := VT.Text[VT.FocusedNode, VT.FocusedColumn];
  lblKeyword.Caption := Copy(FKeyword, 0, 100);
  MemoDescription.Lines.Clear;
  MemoExample.Lines.Clear;
  Caption := DEFAULT_WINDOW_CAPTION;

  if FKeyword <> '' then try
    Screen.Cursor := crHourglass;
    Results := MainForm.ActiveConnection.GetResults('HELP '+esc(FKeyword));
    Caption := Caption + ' - ' + FKeyword;
    MemoDescription.Text := fixNewlines(Results.Col('description', True));
    MemoExample.Text := fixNewlines(Results.Col('example', True));
  finally
    FreeAndNil(Results);
    Screen.Cursor := crDefault;
  end;

  // Show the user if topic is (not) available
  if memoDescription.GetTextLen = 0 then
    memoDescription.Text := _('No help available for this keyword or no keyword was selected.');
  if memoExample.GetTextLen = 0 then
    memoExample.Text := _('No example available or no keyword was selected.');
end;


procedure TfrmSQLhelp.treeTopicsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Results: PDBQuery;
begin
  // Node gets destroyed - free memory used for bound SQL result
  Results := Sender.GetNodeData(Node);
  if Assigned(Results^) then
    Results^.Free;
end;


procedure TfrmSQLhelp.treeTopicsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  // Return open or closed book icon for folders, or document icon for topics
  if not (Kind in [ikNormal, ikSelected]) then
    Exit;
  if Sender.HasChildren[Node] then begin
    if Sender.Expanded[Node] then
      ImageIndex := ICONINDEX_CATEGORY_OPENED
    else
      ImageIndex := ICONINDEX_CATEGORY_CLOSED;
  end else
    ImageIndex := ICONINDEX_HELPITEM;
end;


procedure TfrmSQLhelp.treeTopicsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  // We bind one TDBQuery to a node
  NodeDataSize := SizeOf(TDBQuery);
end;


function TfrmSQLhelp.GetHelpResult(Node: PVirtualNode): TDBQuery;
var
  P: PDBQuery;
begin
  // Find right result set for given node
  if treeTopics.GetNodeLevel(Node) = 0 then
    Result := FRootTopics
  else begin
    P := treeTopics.GetNodeData(Node.Parent);
    Result := P^;
  end;
end;


procedure TfrmSQLhelp.treeTopicsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Results: TDBQuery;
begin
  // Ask result set for node text
  Results := GetHelpResult(Node);
  Results.RecNo := Node.Index;
  CellText := Results.Col('name');
end;


procedure TfrmSQLhelp.treeTopicsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  Results: PDBQuery;
  VT: TVirtualStringTree;
begin
  // Return number of children for folder
  VT := Sender as TVirtualStringTree;
  Results := VT.GetNodeData(Node);
  Results^ := MainForm.ActiveConnection.GetResults('HELP '+esc(VT.Text[Node, VT.Header.MainColumn]));
  ChildCount := Results.RecordCount;
end;


procedure TfrmSQLhelp.treeTopicsInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Results: TDBQuery;
  ThisFolder, PrevFolder: String;
  N: PVirtualNode;
  VT: TVirtualStringTree;
  RecursionAlarm: Boolean;
begin
  // Display plus button for nodes which are folders
  VT := Sender as TVirtualStringTree;
  Results := GetHelpResult(Node);
  Results.RecNo := Node.Index;
  if Results.Col('is_it_category', True) = 'Y' then begin
    // Some random server versions have duplicated category names in help tables, which would cause
    // infinite tree recursion, e.g. for "Polygon properties" > "Contents". Do not display these
    // duplicates as folder
    RecursionAlarm := False;
    ThisFolder := Results.Col('name');
    N := VT.GetPreviousInitialized(Node);
    while Assigned(N) do begin
      PrevFolder := VT.Text[N, VT.Header.MainColumn];
      if VT.HasChildren[N] and ((ThisFolder=PrevFolder) or (ThisFolder='Contents')) then begin
        RecursionAlarm := True;
        break;
      end;
      N := VT.GetPreviousInitialized(N);
    end;
    if not RecursionAlarm then
      Include(InitialStates, ivsHasChildren);
  end;
end;


procedure TfrmSQLhelp.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TfrmSQLhelp.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteInt(asSQLHelpWindowLeft, Left );
  AppSettings.WriteInt(asSQLHelpWindowTop, Top );
  AppSettings.WriteInt(asSQLHelpWindowWidth, Width);
  AppSettings.WriteInt(asSQLHelpWindowHeight, Height);
  AppSettings.WriteInt(asSQLHelpPnlLeftWidth, pnlLeft.Width);
  AppSettings.WriteInt(asSQLHelpPnlRightTopHeight, memoDescription.Height);
  SqlHelpDialog := nil;
end;


procedure TfrmSQLhelp.FormShow(Sender: TObject);
begin
  // Apply themed colors in OnShow, not OnCreate, as a check with <> nil returns false otherwise
  MainForm.SetupSynEditors;
  // These SynMemo's don't have any (SQL) highligher, so we have to assign correct colors for basic text
  memoDescription.Font.Color := GetThemeColor(clWindowText);
  MemoExample.Font.Color := GetThemeColor(clWindowText);
  editFilter.SetFocus;
end;


procedure TfrmSQLhelp.ButtonOnlinehelpClick(Sender: TObject);
begin
  // Link/redirect to mysql.com for further help
  ShellExec(APPDOMAIN + 'sqlhelp.php?mysqlversion='+inttostr(MainForm.ActiveConnection.ServerVersionInt)+
    '&keyword='+EncodeURLParam(keyword));
end;


procedure TfrmSQLhelp.memosKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Esc pressed - close form.
  // Seems that if we're in a memo, the ButtonClose.Cancel=True doesn't have an effect
  if Key = VK_ESCAPE then
    Close;
end;


procedure TfrmSQLhelp.SetKeyword(Value: string);
var
  VT: TVirtualStringTree;
  Node: PVirtualNode;
  Results: TDBQuery;
  SearchNoInit: Boolean;
begin
  // Find keyword in tree
  FKeyword := Value;
  if FKeyword = '' then
    Exit;
  Results := MainForm.ActiveConnection.GetResults('HELP '+esc(FKeyword));
  while not Results.Eof do begin
    if Results.Col('is_it_category', true) = 'N' then begin
      FKeyword := Results.Col('name');
      break;
    end;
    Results.Next;
  end;
  FreeAndNil(Results);

  VT := treeTopics;
  if (not Assigned(VT.FocusedNode)) // No node selected
    or VT.HasChildren[VT.FocusedNode] // Selected node is a folder, not a document
    or (VT.Text[VT.FocusedNode, VT.Header.MainColumn] <> FKeyword) // Node is not the right one
    then begin
    // Start searching in initialized nodes, to minimize number of "HELP xyz" queries in certain cases
    Node := VT.GetFirst;
    SearchNoInit := False;
    while Assigned(Node) do begin
      if (not VT.HasChildren[Node]) and (UpperCase(VT.Text[Node, VT.Header.MainColumn]) = UpperCase(FKeyword)) then begin
        SelectNode(VT, Node);
        break;
      end;
      if not SearchNoInit then begin
        Node := VT.GetNextInitialized(Node);
        if not Assigned(Node) then begin
          SearchNoInit := True;
          Node := VT.GetFirst;
        end;
      end;
      if SearchNoInit then
        Node := VT.GetNext(Node);
    end;
  end;
end;


procedure TfrmSQLhelp.editFilterChange(Sender: TObject);
begin
  timerSearch.Enabled := False;
  timerSearch.Enabled := True;
  editFilter.RightButton.Visible := Trim(editFilter.Text) <> '';
end;


procedure TfrmSQLhelp.editFilterRightButtonClick(Sender: TObject);
begin
  editFilter.Clear;
end;


procedure TfrmSQLhelp.DoSearch(Sender: TObject);
var
  Node: PVirtualNode;
  Search: String;
  Vis: Boolean;

  function HasVisibleChildItems(Node: PVirtualNode): Boolean;
  var
    N: PVirtualNode;
  begin
    N := treeTopics.GetFirstChild(Node);
    Result := False;
    while Assigned(N) do begin
      if treeTopics.HasChildren[N] then
        Result := HasVisibleChildItems(N)
      else
        Result := treeTopics.IsVisible[N];
      if Result then
        Exit;
      N := treeTopics.GetNextSibling(N);
    end;
  end;
begin
  // Apply filter text
  Screen.Cursor := crHourglass;
  treeTopics.BeginUpdate;
  timerSearch.Enabled := False;
  Search := Trim(editFilter.Text);
  if Search = '' then begin
    // Show all items
    Node := treeTopics.GetFirstInitialized;
    while Assigned(Node) do begin
      treeTopics.IsVisible[Node] := True;
      Node := treeTopics.GetNextInitialized(Node);
    end;
  end else begin
    // Hide non matching child items
    Node := treeTopics.GetFirst;
    while Assigned(Node) do begin
      if not treeTopics.HasChildren[Node] then
        treeTopics.IsVisible[Node] := Pos(UpperCase(Search), UpperCase(treeTopics.Text[Node, treeTopics.Header.MainColumn])) > 0;
      Node := treeTopics.GetNext(Node);
    end;
    // Hide empty folders
    Node := treeTopics.GetFirst;
    while Assigned(Node) do begin
      if treeTopics.HasChildren[Node] then begin
        Vis := HasVisibleChildItems(Node);
        treeTopics.Expanded[Node] := Vis;
        treeTopics.IsVisible[Node] := Vis;
      end;
      Node := treeTopics.GetNext(Node);
    end;
  end;
  treeTopics.EndUpdate;
  Screen.Cursor := crDefault;
end;

end.
