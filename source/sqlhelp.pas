unit sqlhelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ShellApi, Buttons,
  PngSpeedButton, SynMemo, SynEditHighlighter, SynHighlighterURI,
  SynURIOpener, SynEdit, mysql_connection;

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
    memoDescription: TSynMemo;
    Splitter2: TSplitter;
    pnlRightBottom: TPanel;
    lblExample: TLabel;
    MemoExample: TSynMemo;
    ButtonClose: TButton;
    btnSearchOnline: TPngSpeedButton;
    URIOpenerDescription: TSynURIOpener;
    URIHighlighter: TSynURISyn;
    URIOpenerExample: TSynURIOpener;
    procedure FormCreate(Sender: TObject);
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
  public
    { Public declarations }
    Keyword: String;
  end;

  const
    DEFAULT_WINDOW_CAPTION      : String = 'Integrated SQL-help' ;
    ICONINDEX_CATEGORY_CLOSED   : Integer = 66;
    ICONINDEX_CATEGORY_OPENED   : Integer = 67;
    ICONINDEX_HELPITEM          : Integer = 68;

implementation

uses helpers, main, db;

{$I const.inc}

{$R *.dfm}


{**
  FormCreate
}
procedure TfrmSQLhelp.FormCreate(Sender: TObject);
begin
  // Assign images from main imagelist to speedbuttons
  btnSearchOnline.PngImage := Mainform.PngImageListMain.PngImages[69].PngImage;
  InheritFont(Font);
end;


{***
  Startup
}
procedure TfrmSQLhelp.FormShow(Sender: TObject);
begin
  // Set window-layout
  Top := GetRegValue( REGNAME_SQLHELPWINTOP, Top );
  Left := GetRegValue( REGNAME_SQLHELPWINLEFT, Left );
  Width := GetRegValue( REGNAME_SQLHELPWINWIDTH, Width );
  Height := GetRegValue( REGNAME_SQLHELPWINHEIGHT, Height );
  pnlLeft.Width := GetRegValue( REGNAME_SQLHELPPLWIDTH, pnlLeft.Width );
  pnlRightTop.Height := GetRegValue( REGNAME_SQLHELPPRHEIGHT, pnlRightTop.Height );
  Caption := DEFAULT_WINDOW_CAPTION;
  MainForm.SetupSynEditors;

  // Gather help contents for treeview with SQL: HELP "CONTENTS"
  fillTreeLevel( nil );

  if ShowHelpItem then
    findKeywordInTree;
end;



{***
  Fills exactly one level of the folder-tree
  Call with NIL to generate the root folders,
  then call recursively to iterate through all folders and fill them
  @param TTreeNode Parent node to fill (or NIL)
}
procedure TfrmSQLhelp.fillTreeLevel( ParentNode: TTreeNode );
var
  tnode: TTreeNode;
  Results: TMySQLQuery;
  topic: String;
begin
  if ParentNode = nil then begin
    treeTopics.Items.Clear;
    topic := 'CONTENTS';
  end else begin
    ParentNode.DeleteChildren;
    topic := ParentNode.Text;
  end;
  try
    Screen.Cursor := crHourglass;
    Results := Mainform.Connection.GetResults( 'HELP "'+topic+'"' );
    while not Results.Eof do begin
      tnode := treeTopics.Items.AddChild( ParentNode, Results.Col('name'));
      if Results.ColExists('is_it_category') and (Results.Col('is_it_category') = 'Y') then begin
        tnode.ImageIndex := ICONINDEX_CATEGORY_CLOSED;
        tnode.SelectedIndex := ICONINDEX_CATEGORY_OPENED;
        // Add a dummy item to show the plus-button so the user sees that there this
        // is a category. When the plus-button is clicked, fetch the content of the category
        treeTopics.Items.AddChild( tnode, DUMMY_NODE_TEXT );
      end else begin
        tnode.ImageIndex := ICONINDEX_HELPITEM;
        tnode.SelectedIndex := tnode.ImageIndex;
      end;
      Results.Next;
    end;
  finally
    FreeAndNil(Results);
    Screen.Cursor := crDefault;
  end;
end;



{***
  Show selected keyword in Tree
}
procedure TfrmSQLhelp.findKeywordInTree;
var
  tnode : TTreeNode;
  i : Integer;
  tmp : Boolean;
begin
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



{***
  Selected item in treeTopics has changed
}
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



{***
  Get topics from category
}
procedure TfrmSQLhelp.treeTopicsExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  if (Node.getFirstChild <> nil) and (Node.getFirstChild.Text = DUMMY_NODE_TEXT) then
  begin
    fillTreeLevel( Node );
  end;
end;



{***
  Fetch and show text of help-item in synmemo's
  @return boolean Was the keyword found? 
}
function TfrmSQLhelp.ShowHelpItem: Boolean;
var
  Results: TMySQLQuery;
begin
  lblKeyword.Caption := Copy(Keyword, 0, 100);
  MemoDescription.Lines.Clear;
  MemoExample.Lines.Clear;
  Caption := DEFAULT_WINDOW_CAPTION;
  result := false; // Keyword not found yet

  if Keyword <> '' then
  try
    Screen.Cursor := crHourglass;
    Results := Mainform.Connection.GetResults('HELP "'+lblKeyword.Caption+'"');
    if Results.RecordCount = 1 then begin
      // We found exactly one matching help item
      lblKeyword.Caption := Results.Col('name');
      Keyword := lblKeyword.Caption;
      if lblKeyword.Caption = '&' then
        lblKeyword.Caption := '&&'; // Avoid displaying "_" as alt-hotkey
      Caption := Caption + ' - ' + Keyword;
      MemoDescription.Text := fixNewlines(Results.Col('description'));
      MemoExample.Text := fixNewlines(Results.Col('example'));
      result := true;
    end;
  finally
    FreeAndNil(Results);
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



{***
  Save layout and close window 
}
procedure TfrmSQLhelp.ButtonCloseClick(Sender: TObject);
begin
  OpenRegistry;
  MainReg.WriteInteger( REGNAME_SQLHELPWINLEFT, Left );
  MainReg.WriteInteger( REGNAME_SQLHELPWINTOP, Top );
  MainReg.WriteInteger( REGNAME_SQLHELPWINWIDTH, Width );
  MainReg.WriteInteger( REGNAME_SQLHELPWINHEIGHT, Height );
  MainReg.WriteInteger( REGNAME_SQLHELPPLWIDTH, pnlLeft.Width );
  MainReg.WriteInteger( REGNAME_SQLHELPPRHEIGHT, PnlRightTop.Height );
  Close;
end;



{***
  Link/redirect to mysql.com for further help
  @see http://www.heidisql.com/sqlhelp.php
}
procedure TfrmSQLhelp.ButtonOnlinehelpClick(Sender: TObject);
begin
  ShellExec( APPDOMAIN + 'sqlhelp.php?mysqlversion='+inttostr(Mainform.Connection.ServerVersionInt)+
    '&keyword='+urlencode(keyword) );
end;



{***
  Esc pressed - close form.
  Seems that if we're in a memo, the ButtonClose.Cancel=True doesn't have an effect
}
procedure TfrmSQLhelp.memosKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    ButtonCloseClick(self);
end;



end.
