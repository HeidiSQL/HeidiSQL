unit sqlhelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ShellApi, Buttons, Registry,
  childwin, PngSpeedButton;

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
    btnSearchOnline: TPngSpeedButton;
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
    m : TMDIChild;
    Keyword: String;
  public
    { Public declarations }
  end;

  const
    DEFAULT_WINDOW_CAPTION      : String = 'Integrated SQL-help' ;
    ICONINDEX_CATEGORY_CLOSED   : Integer = 66;
    ICONINDEX_CATEGORY_OPENED   : Integer = 67;
    ICONINDEX_HELPITEM          : Integer = 68;

  function SQLhelpWindow(AOwner: TComponent; Keyword: String = ''): Boolean;

implementation

uses ZDataset, helpers, main, db;

{$I const.inc}

{$R *.dfm}



{**
  Create form on demand
  @param TComponent Owner of form (should be calling form)
  @return Boolean Form closed using modalresult mrOK
}
function SQLhelpWindow(AOwner: TComponent; Keyword: String = ''): Boolean;
var
  f : TfrmSQLhelp;
begin
  f := TfrmSQLhelp.Create(AOwner);
  f.Keyword := Keyword;
  Result := (f.ShowModal=mrOK);
  FreeAndNil(f);
end;


{**
  FormCreate
}
procedure TfrmSQLhelp.FormCreate(Sender: TObject);
begin
  // Assign images from main imagelist to speedbuttons
  btnSearchOnline.PngImage := Mainform.PngImageListMain.PngImages[69].PngImage;
end;


{***
  Startup
}
procedure TfrmSQLhelp.FormShow(Sender: TObject);
begin
  m := Mainform.Childwin;

  // Set window-layout
  Top := Mainform.GetRegValue( REGNAME_SQLHELPWINTOP, Top );
  Left := Mainform.GetRegValue( REGNAME_SQLHELPWINLEFT, Left );
  Width := Mainform.GetRegValue( REGNAME_SQLHELPWINWIDTH, Width );
  Height := Mainform.GetRegValue( REGNAME_SQLHELPWINHEIGHT, Height );
  pnlLeft.Width := Mainform.GetRegValue( REGNAME_SQLHELPPLWIDTH, pnlLeft.Width );
  pnlRightTop.Height := Mainform.GetRegValue( REGNAME_SQLHELPPRHEIGHT, pnlRightTop.Height );
  Caption := DEFAULT_WINDOW_CAPTION;

  MemoDescription.Font.Name := m.SynMemoQuery.Font.Name;
  MemoDescription.Font.Size := m.SynMemoQuery.Font.size;
  MemoExample.Font.Name := m.SynMemoQuery.Font.Name;
  MemoExample.Font.Size := m.SynMemoQuery.Font.size;

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
  tnode   : TTreeNode;
  i       : integer;
  ds      : TDataSet;
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
  ds := nil;
  try
    Screen.Cursor := crHourglass;
    ds := m.GetResults( 'HELP "'+topic+'"' );
    for i:=1 to ds.RecordCount do
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
    if ds <> nil then ds.Close;
    FreeAndNil( ds );
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
  ds : TDataSet;
begin
  lblKeyword.Caption := Copy(Keyword, 0, 100);
  MemoDescription.Lines.Clear;
  MemoExample.Lines.Clear;
  Caption := DEFAULT_WINDOW_CAPTION;
  result := false; // Keyword not found yet

  ds := nil;
  if Keyword <> '' then
  try
    Screen.Cursor := crHourglass;
    ds := m.GetResults( 'HELP "'+lblKeyword.Caption+'"' );
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
    if ds <> nil then ds.Close;
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



{***
  Save layout and close window 
}
procedure TfrmSQLhelp.ButtonCloseClick(Sender: TObject);
var
  reg : TRegistry;
begin
  reg := TRegistry.Create;
  if reg.OpenKey(REGPATH, False) then begin
    reg.WriteInteger( REGNAME_SQLHELPWINLEFT, Left );
    reg.WriteInteger( REGNAME_SQLHELPWINTOP, Top );
    reg.WriteInteger( REGNAME_SQLHELPWINWIDTH, Width );
    reg.WriteInteger( REGNAME_SQLHELPWINHEIGHT, Height );
    reg.WriteInteger( REGNAME_SQLHELPPLWIDTH, pnlLeft.Width );
    reg.WriteInteger( REGNAME_SQLHELPPRHEIGHT, PnlRightTop.Height );
    reg.CloseKey;
  end;
  reg.Free;
  Close;
end;



{***
  Link/redirect to mysql.com for further help
  @see http://www.heidisql.com/sqlhelp.php
}
procedure TfrmSQLhelp.ButtonOnlinehelpClick(Sender: TObject);
begin
  ShellExec( APPDOMAIN + 'sqlhelp.php?mysqlversion='+inttostr(m.mysql_version)+
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
