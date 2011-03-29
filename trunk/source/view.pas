unit view;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
  ExtCtrls, SynRegExpr,
  dbconnection, mysql_structures, helpers;

type
  TFrame = TDBObjectEditor;
  TfrmView = class(TFrame)
    editName: TEdit;
    lblName: TLabel;
    rgAlgorithm: TRadioGroup;
    SynMemoSelect: TSynMemo;
    lblSelect: TLabel;
    btnDiscard: TButton;
    btnSave: TButton;
    rgCheck: TRadioGroup;
    btnHelp: TButton;
    lblDisabledWhy: TLabel;
    lblDefiner: TLabel;
    comboDefiner: TComboBox;
    procedure btnHelpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure comboDefinerDropDown(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Init(Obj: TDBObject); override;
    function ApplyModifications: TModalResult; override;
  end;

  
implementation

uses main;

{$R *.dfm}


{**
  Create: Restore GUI setup
}
constructor TfrmView.Create(AOwner: TComponent);
begin
  inherited;
  SynMemoSelect.Highlighter := Mainform.SynSQLSyn1;
  Mainform.SynCompletionProposal.AddEditor(SynMemoSelect);
  editName.MaxLength := NAME_LEN;
end;


{**
  FormShow: Fill controls with content in edit mode
}
procedure TfrmView.Init(Obj: TDBObject);
var
  Algorithm, CheckOption, SelectCode, Definer: String;
begin
  inherited;
  lblDisabledWhy.Font.Color := clRed;
  comboDefiner.Text := '';
  comboDefiner.TextHint := 'Current user ('+Obj.Connection.CurrentUserHostCombination+')';
  comboDefiner.Hint := 'Leave empty for current user ('+Obj.Connection.CurrentUserHostCombination+')';
  if Obj.Name <> '' then begin
    // Edit mode
    editName.Text := Obj.Name;
    Obj.Connection.ParseViewStructure(Obj.CreateCode, Obj.Name, nil, Algorithm, Definer, CheckOption, SelectCode);
    comboDefiner.Text := Definer;
    rgAlgorithm.ItemIndex := rgAlgorithm.Items.IndexOf(Algorithm);
    rgCheck.ItemIndex := rgCheck.Items.IndexOf(CheckOption);
    if rgCheck.ItemIndex = -1 then
      rgCheck.ItemIndex := 0;
    SynMemoSelect.Text := SelectCode;
    // User may not be allowed to run SHOW CREATE VIEW, in which case we have an empty CreateCode.
    // Disable editor in this case.
    lblDisabledWhy.Visible := SelectCode = '';
    editName.Enabled := not lblDisabledWhy.Visible;
    rgAlgorithm.Enabled := editName.Enabled;
    rgCheck.Enabled := rgAlgorithm.Enabled;
    SynMemoSelect.Enabled := rgAlgorithm.Enabled;
  end else begin
    // Create mode
    editName.Text := '';
    rgAlgorithm.Enabled := True;
    rgAlgorithm.ItemIndex := 0;
    rgCheck.Enabled := True;
    rgCheck.ItemIndex := 0;
    SynMemoSelect.Text := 'SELECT ';
    lblDisabledWhy.Hide;
  end;
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


procedure TfrmView.comboDefinerDropDown(Sender: TObject);
begin
  // Populate definers from mysql.user
  (Sender as TComboBox).Items.Assign(GetDefiners);
end;


{**
  Lookup "Create|Alter View" in SQL help
}
procedure TfrmView.btnHelpClick(Sender: TObject);
var
  keyword: String;
begin
  if DBObject.Name = '' then
    keyword := 'CREATE VIEW'
  else
    keyword := 'ALTER VIEW';
  Mainform.CallSQLHelpWithKeyword(keyword);
end;


procedure TfrmView.btnDiscardClick(Sender: TObject);
begin
  // Reinit editor, discarding changes
  Modified := False;
  Init(DBObject);
end;


{**
  Apply changes
}
procedure TfrmView.btnSaveClick(Sender: TObject);
begin
  ApplyModifications;
end;


function TfrmView.ApplyModifications: TModalResult;
var
  sql, viewname, renamed: String;
begin
  // Save changes
  Result := mrOk;
  if DBObject.Name = '' then begin
    sql := 'CREATE ';
    viewname := editName.Text;
  end else begin
    sql := 'ALTER ';
    viewname := DBObject.Name;
  end;
  viewname := DBObject.Connection.QuoteIdent(viewname);
  if rgAlgorithm.Enabled and (rgAlgorithm.ItemIndex > -1) then
    sql := sql + 'ALGORITHM = '+Uppercase(rgAlgorithm.Items[rgAlgorithm.ItemIndex])+' ';
  if comboDefiner.Text <> '' then
    sql := sql + 'DEFINER='+DBObject.Connection.QuoteIdent(comboDefiner.Text, True, '@')+' ';
  sql := sql + 'VIEW ' + viewname+' AS '+SynMemoSelect.Text+' ';
  if rgCheck.Enabled and (rgCheck.ItemIndex > 0) then
    sql := sql + 'WITH '+Uppercase(rgCheck.Items[rgCheck.ItemIndex])+' CHECK OPTION';

  try
    DBObject.Connection.Query(sql);
    // Probably rename view
    if (DBObject.Name <> '') and (DBObject.Name <> editName.Text) then begin
      renamed := DBObject.Connection.QuoteIdent(editName.Text);
      DBObject.Connection.Query('RENAME TABLE '+viewname + ' TO '+renamed);
    end;
    DBObject.Name := editName.Text;
    DBObject.CreateCode := '';
    Mainform.UpdateEditorTab;
    Mainform.RefreshTree(DBObject);
    Mainform.ParseSelectedTableStructure;
    Modified := False;
    btnSave.Enabled := Modified;
    btnDiscard.Enabled := Modified;
  except
    on E:EDatabaseError do begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
      Result := mrAbort;
    end;
  end;
end;


procedure TfrmView.Modification(Sender: TObject);
begin
  Modified := True;
  btnSave.Enabled := Modified and (editName.Text <> '');
  btnDiscard.Enabled := Modified;
end;


end.
