unit sqlhelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ShellApi, Buttons, Registry,
  childwin, main;

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
    procedure ShowHelpItem;
    procedure fillTreeLevel( ParentNode: TTreeNode );
  private
    { Private declarations }
    Keyword: String;
    m : TMDIChild;
  public
    { Public declarations }
  end;

  function SQLHelpWindow (AOwner : TComponent; Keyword : String = '') : Boolean;

  const
    defaultWindowCaption : String = 'Integrated SQL-help' ;

implementation

uses ZDataset, helpers;

{$R *.dfm}


function SQLHelpWindow (AOwner : TComponent; Keyword : String = '') : Boolean;
var
  f : TfrmSQLHelp;
begin
  f := TfrmSQLHelp.Create(AOwner);
  f.Keyword := Keyword;
  f.m := TMDIChild(Application.Mainform.ActiveMDIChild);
  f.Top := Mainform.GetRegValue( 'SQLHelp_WindowTop', f.Top );
  f.Left := Mainform.GetRegValue( 'SQLHelp_WindowLeft', f.Left );
  f.Width := Mainform.GetRegValue( 'SQLHelp_WindowWidth', f.Width );
  f.Height := Mainform.GetRegValue( 'SQLHelp_WindowHeight', f.Height );
  f.pnlLeft.Width := Mainform.GetRegValue( 'SQLHelp_PnlLeftWidth', f.pnlLeft.Width );
  f.pnlRightTop.Height := Mainform.GetRegValue( 'SQLHelp_PnlRightTopHeight', f.pnlRightTop.Height );
  f.Caption := defaultWindowCaption;
  Result := (f.ShowModal=mrOK);
  FreeAndNil (f);
end;


procedure TfrmSQLhelp.FormShow(Sender: TObject);
begin
  ShowHelpItem;
  MemoDescription.Font.Name := m.SynMemoQuery.Font.Name;
  MemoDescription.Font.Size := m.SynMemoQuery.Font.size;
  MemoExample.Font.Name := m.SynMemoQuery.Font.Name;
  MemoExample.Font.Size := m.SynMemoQuery.Font.size;

  // Gather help contents for treeview with SQL: HELP "CONTENTS"
  fillTreeLevel( nil );
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
    ds.Connection := m.ZConn;
    m.GetResults( 'HELP "'+topic+'"', ds );
    while not ds.Eof do
    begin
      tnode := treeTopics.Items.AddChild( ParentNode, ds.FieldByName('name').AsString );
      if (ds.FindField('is_it_category') <> nil) and (ds.FieldByName('is_it_category').AsString = 'Y') then
      begin
        // Add a dummy item to show the plus-button so the user sees that there this
        // is a category. When the plus-button is clicked, fetch the content of the category
        treeTopics.Items.AddChild( tnode, '' );
      end;
      ds.Next;
    end;
  finally
    FreeAndNil( ds );
    Screen.Cursor := crDefault;
  end;
end;


procedure TfrmSQLhelp.ShowHelpItem;
var
  ds : TZReadOnlyQuery;
begin
  lblKeyword.Caption := Copy(Keyword, 0, 100);
  MemoDescription.Lines.Clear;
  MemoExample.Lines.Clear;
  Caption := defaultWindowCaption;

  if Keyword <> '' then
  try
    Screen.Cursor := crHourglass;
    ds := TZReadOnlyQuery.Create(self);
    ds.Connection := m.ZConn;
    m.GetResults( 'HELP "'+lblKeyword.Caption+'"', ds );
    if ds.RecordCount = 1 then
    begin
      // We found exactly one matching help item
      lblKeyword.Caption := fixNewlines(ds.FieldByName('name').AsString);
      Caption := Caption + ' - ' + lblKeyword.Caption;
      MemoDescription.Text := fixNewlines(ds.FieldByName('description').AsString);
      MemoExample.Text := fixNewlines(ds.FieldByName('example').AsString);
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
  ModalResult := mrCancel;
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
  if Key = VK_ESCAPE then
    ButtonCloseClick(self);
end;


procedure TfrmSQLhelp.treeTopicsChange(Sender: TObject; Node: TTreeNode);
begin
  // Selection in treeTopics has changed
  if not Node.HasChildren then
  begin
    Keyword := Node.Text;
    ShowHelpItem;
  end;
end;


procedure TfrmSQLhelp.treeTopicsExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  // Get topics from category
  fillTreeLevel( Node );
end;

end.
