unit sqlhelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ShellApi, Buttons, Registry,
  childwin;

type
  TfrmSQLhelp = class(TForm)
    pnlLeft: TPanel;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    treeTopics: TTreeView;
    pnlRight: TPanel;
    lblTopics: TLabel;
    pnlRightTop: TPanel;
    lblKeyword: TLabel;
    lblDescription: TLabel;
    memoDescription: TMemo;
    Splitter2: TSplitter;
    pnlRightBottom: TPanel;
    lblExample: TLabel;
    MemoExample: TMemo;
    ButtonClose: TButton;
    ButtonSearchOnline: TBitBtn;
    procedure treeTopicsExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure treeTopicsChange(Sender: TObject; Node: TTreeNode);
    procedure FormShow(Sender: TObject);
    procedure memosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOnlinehelpClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    function ShowHelpItem: Boolean;
    procedure fillTreeLevel( ParentNode: TTreeNode );
    procedure findKeywordInTree;

  private
    { Private declarations }
    Keyword: String;
    m : TMDIChild;
  public
    { Public declarations }
  end;

  function SQLHelpWindow (AOwner : TComponent; Keyword : String = '') : Boolean;

  const
    DEFAULT_WINDOW_CAPTION      : String = 'Integrated SQL-help' ;
    DUMMY_NODE_TEXT             : String = 'Dummy node, should never be visible';
    ICONINDEX_CATEGORY_CLOSED   : Integer = 96;
    ICONINDEX_CATEGORY_OPENED   : Integer = 97;
    ICONINDEX_HELPITEM          : Integer = 98;

implementation

uses ZDataset, helpers, main;

{$R *.dfm}


function SQLHelpWindow (AOwner : TComponent; Keyword : String = '') : Boolean;
var
  f : TfrmSQLHelp;
begin
  if Mainform.SQLHelpWindow_Instance = nil then
  begin
    f := TfrmSQLHelp.Create(AOwner);
    Mainform.SQLHelpWindow_Instance := f;
    f.m := TMDIChild(Application.Mainform.ActiveMDIChild);
    f.Top := Mainform.GetRegValue( 'SQLHelp_WindowTop', f.Top );
    f.Left := Mainform.GetRegValue( 'SQLHelp_WindowLeft', f.Left );
    f.Width := Mainform.GetRegValue( 'SQLHelp_WindowWidth', f.Width );
    f.Height := Mainform.GetRegValue( 'SQLHelp_WindowHeight', f.Height );
    f.pnlLeft.Width := Mainform.GetRegValue( 'SQLHelp_PnlLeftWidth', f.pnlLeft.Width );
    f.pnlRightTop.Height := Mainform.GetRegValue( 'SQLHelp_PnlRightTopHeight', f.pnlRightTop.Height );
    f.Caption := DEFAULT_WINDOW_CAPTION;
    // Gather help contents for treeview with SQL: HELP "CONTENTS"
    f.fillTreeLevel( nil );
    f.Keyword := Keyword;
    f.Show;
  end
  else
  begin
    f := Mainform.SQLHelpWindow_Instance;
    f.Keyword := Keyword;
    f.Show;
    f.FormShow( f );
  end;
end;


procedure TfrmSQLhelp.FormShow(Sender: TObject);
begin
  MemoDescription.Font.Name := m.SynMemoQuery.Font.Name;
  MemoDescription.Font.Size := m.SynMemoQuery.Font.size;
  MemoExample.Font.Name := m.SynMemoQuery.Font.Name;
  MemoExample.Font.Size := m.SynMemoQuery.Font.size;

  if ShowHelpItem then
    findKeywordInTree;
end;


procedure TfrmSQLhelp.fillTreeLevel( ParentNode: TTreeNode );
var
  tnode   : TTreeNode;
  ds      : TZReadOnlyQuery;
  topic   : String;
begin
  if ParentNode = nil then
  begin
    treeTopics.Items.Clear;
    topic := 'CONTENTS';
  end
  else
  begin
    ParentNode.DeleteChildren;
    topic := ParentNode.Text;
  end;
  ds := TZReadOnlyQuery.Create(self);
  try
    Screen.Cursor := crHourglass;
    ds.Connection := m.ZQuery3.Connection;
    m.GetResults( 'HELP "'+topic+'"', ds );
    while not ds.Eof do
    begin
      tnode := treeTopics.Items.AddChild( ParentNode, ds.FieldByName('name').AsString );
      if (ds.FindField('is_it_category') <> nil) and (ds.FieldByName('is_it_category').AsString = 'Y') then
      begin
        tnode.ImageIndex := ICONINDEX_CATEGORY_CLOSED;
        tnode.SelectedIndex := ICONINDEX_CATEGORY_OPENED;
        // Add a dummy item to show the plus-button so the user sees that there this
        // is a category. When the plus-button is clicked, fetch the content of the category
        treeTopics.Items.AddChild( tnode, DUMMY_NODE_TEXT );
      end
      else
      begin
        tnode.ImageIndex := ICONINDEX_HELPITEM;
        tnode.SelectedIndex := tnode.ImageIndex;
      end;
      ds.Next;
    end;
  finally
    FreeAndNil( ds );
    Screen.Cursor := crDefault;
  end;
end;


procedure TfrmSQLhelp.findKeywordInTree;
var
  tnode : TTreeNode;
  i : Integer;
  tmp : Boolean;
begin
  // Show selected keyword in Tree
  i := 0;
  while i < treeTopics.Items.Count do
  begin
    tnode := treeTopics.Items[i];
    inc(i);
    if tnode.Text = Keyword then
    begin
      tnode.MakeVisible;
      treeTopics.Selected := tnode;
      break;
    end;
    treeTopicsExpanding( self, tnode, tmp );
  end;
  treeTopics.SetFocus;
end;


procedure TfrmSQLhelp.treeTopicsChange(Sender: TObject; Node: TTreeNode);
  procedure OpenFolderIcons( ANode: TTreeNode );
  begin
    if ANode = nil then
      exit;
    if ANode.ImageIndex = ICONINDEX_CATEGORY_CLOSED then
    begin
      ANode.ImageIndex := ICONINDEX_CATEGORY_OPENED;
    end;
    // Recursively update ANode's parent node
    OpenFolderIcons( ANode.Parent );
  end;

var
  i : Integer;
  tNode : TTreeNode;
begin
  // Selected item in treeTopics has changed

  // 1. Show corresponding help-text
  if Node.ImageIndex = ICONINDEX_HELPITEM then
  begin
    Keyword := Node.Text;
    ShowHelpItem;
  end;

  // 2. Ensure the icons in the preceding tree-path of the selected item
  // show opened folders on each level
  i := 0;
  while i < treeTopics.Items.Count do
  begin
    tNode := treeTopics.Items[i];
    if tNode.ImageIndex = ICONINDEX_CATEGORY_OPENED then
    begin
      tNode.ImageIndex := ICONINDEX_CATEGORY_CLOSED;
    end;
    inc(i);
  end;
  OpenFolderIcons( Node );

end;


procedure TfrmSQLhelp.treeTopicsExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  // Get topics from category
  if (Node.getFirstChild <> nil) and (Node.getFirstChild.Text = DUMMY_NODE_TEXT) then
  begin
    fillTreeLevel( Node );
  end;
end;


function TfrmSQLhelp.ShowHelpItem: Boolean;
var
  ds : TZReadOnlyQuery;
begin
  lblKeyword.Caption := Copy(Keyword, 0, 100);
  MemoDescription.Lines.Clear;
  MemoExample.Lines.Clear;
  Caption := DEFAULT_WINDOW_CAPTION;
  result := false; // Keyword not found yet

  if Keyword <> '' then
  try
    Screen.Cursor := crHourglass;
    ds := TZReadOnlyQuery.Create(self);
    ds.Connection := m.ZQuery3.Connection;
    m.GetResults( 'HELP "'+lblKeyword.Caption+'"', ds );
    if ds.RecordCount = 1 then
    begin
      // We found exactly one matching help item
      lblKeyword.Caption := ds.FieldByName('name').AsString;
      Keyword := lblKeyword.Caption;
      if lblKeyword.Caption = '&' then
        lblKeyword.Caption := '&&'; // Avoid displaying "_" as alt-hotkey
      Caption := Caption + ' - ' + Keyword;
      MemoDescription.Text := fixNewlines(ds.FieldByName('description').AsString);
      MemoExample.Text := fixNewlines(ds.FieldByName('example').AsString);
      result := true;
    end;
  finally
    FreeAndNil( ds );
    Screen.Cursor := crDefault;
  end;

  // Show the user if topic is (not) available
  with MemoDescription do
  begin
    Enabled := Text <> '';
    if not Enabled then
      Text := 'No help available for this keyword or no keyword was selected.';
  end;
  with MemoExample do
  begin
    Enabled := Text <> '';
    if not Enabled then
      Text := 'No example available or no keyword was selected.';
  end;

end;




procedure TfrmSQLhelp.ButtonCloseClick(Sender: TObject);
begin
  Mainform.SaveRegValue( 'SQLHelp_WindowLeft', Left );
  Mainform.SaveRegValue( 'SQLHelp_WindowTop', Top );
  Mainform.SaveRegValue( 'SQLHelp_WindowWidth', Width );
  Mainform.SaveRegValue( 'SQLHelp_WindowHeight', Height );
  Mainform.SaveRegValue( 'SQLHelp_PnlLeftWidth', pnlLeft.Width );
  Mainform.SaveRegValue( 'SQLHelp_PnlRightTopHeight', PnlRightTop.Height );

  // Close
  Close;
end;


procedure TfrmSQLhelp.ButtonOnlinehelpClick(Sender: TObject);
begin
  // Search online
  ShellExec( 'http://www.heidisql.com/sqlhelp.php?mysqlversion='+inttostr(m.mysql_version)+
    '&hsversion='+urlencode(Main.appversion)+'&keyword='+urlencode(keyword) );
end;


procedure TfrmSQLhelp.memosKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Esc pressed - close form.
  // Seems that if we're in a memo, the ButtonClose.Cancel=True doesn't have an effect
  if Key = VK_ESCAPE then
    ButtonCloseClick(self);
end;



end.
