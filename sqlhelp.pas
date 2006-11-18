unit sqlhelp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, childwin, ShellApi;

type
  TfrmSQLhelp = class(TForm)
    pnlLeft: TPanel;
    StatusBar1: TStatusBar;
    Splitter1: TSplitter;
    treevwTopics: TTreeView;
    editSearch: TEdit;
    btnSearch: TButton;
    pnlRight: TPanel;
    lblKeyword: TLabel;
    lblDescription: TLabel;
    memoDescription: TMemo;
    lblExample: TLabel;
    MemoExample: TMemo;
    ButtonClose: TButton;
    ButtonOnlinehelp: TButton;
    procedure FormShow(Sender: TObject);
    procedure memosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonOnlinehelpClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ShowHelp;
  private
    { Private declarations }
    Keyword: String;
    m : TMDIChild;
  public
    { Public declarations }
  end;

  function SQLHelpWindow (AOwner : TComponent; Keyword : String = '') : Boolean;

implementation

uses ZDataset, helpers, main;

{$R *.dfm}


function SQLHelpWindow (AOwner : TComponent; Keyword : String = '') : Boolean;
var
  f : TfrmSQLHelp;
begin
  f := TfrmSQLHelp.Create(AOwner);
  f.Keyword := Keyword;
  f.m := TMDIChild(Application.Mainform.ActiveMDIChild);
  Result := (f.ShowModal=mrOK);
  FreeAndNil (f);
end;


procedure TfrmSQLhelp.FormShow(Sender: TObject);
begin
  ShowHelp;
  MemoDescription.Font.Name := m.SynMemoQuery.Font.Name;
  MemoDescription.Font.Size := m.SynMemoQuery.Font.size;
  MemoExample.Font.Name := m.SynMemoQuery.Font.Name;
  MemoExample.Font.Size := m.SynMemoQuery.Font.size;

  // TODO: Gather help items for treeview with SQL: HELP "%"
end;


procedure TfrmSQLhelp.ShowHelp;
var
  desc, xmp : String;
  ds : TZReadOnlyQuery;
begin
  lblKeyword.Caption := Copy(Keyword, 0, 100);
  MemoDescription.Lines.Clear;
  MemoExample.Lines.Clear;

  ds := TZReadOnlyQuery.Create(self);
  ds.Connection := m.ZConn;
  m.GetResults( 'HELP "'+lblKeyword.Caption+'"', ds );
  if ds.RecordCount = 1 then
  begin
    // We found exactly one matching help item
    lblKeyword.Caption := fixNewlines(ds.FieldByName('name').AsString);
    MemoDescription.Text := fixNewlines(ds.FieldByName('description').AsString);
    MemoExample.Text := fixNewlines(ds.FieldByName('example').AsString);
  end;

  with MemoDescription do
  begin
    Enabled := Text <> '';
    if not Enabled then
      Text := 'No help available for this keyword';
  end;
  with MemoExample do
  begin
    Enabled := Text <> '';
    if not Enabled then
      Text := 'No example available';
  end;
end;


procedure TfrmSQLhelp.ButtonCloseClick(Sender: TObject);
begin
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


end.
