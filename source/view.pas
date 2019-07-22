unit view;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
  ExtCtrls,
  dbconnection, dbstructures, apphelpers, gnugettext;

type
  TFrame = TDBObjectEditor;
  TfrmView = class(TFrame)
    editName: TEdit;
    lblName: TLabel;
    rgAlgorithm: TRadioGroup;
    SynMemoBody: TSynMemo;
    lblSelect: TLabel;
    btnDiscard: TButton;
    btnSave: TButton;
    rgCheck: TRadioGroup;
    btnHelp: TButton;
    lblDisabledWhy: TLabel;
    lblDefiner: TLabel;
    comboDefiner: TComboBox;
    lblSecurity: TLabel;
    comboSecurity: TComboBox;
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
  SynMemoBody.Highlighter := Mainform.SynSQLSynUsed;
  Mainform.SynCompletionProposal.AddEditor(SynMemoBody);
  editName.MaxLength := NAME_LEN;
  comboSecurity.Items.Add('Definer');
  comboSecurity.Items.Add('Invoker');
end;


{**
  FormShow: Fill controls with content in edit mode
}
procedure TfrmView.Init(Obj: TDBObject);
var
  Algorithm, CheckOption, SelectCode, Definer, SQLSecurity: String;
  i: Integer;
begin
  inherited;
  lblDisabledWhy.Font.Color := clRed;
  comboDefiner.Text := '';
  comboDefiner.TextHint := f_('Current user (%s)', [Obj.Connection.CurrentUserHostCombination]);
  comboDefiner.Hint := f_('Leave empty for current user (%s)', [Obj.Connection.CurrentUserHostCombination]);
  if Obj.Name <> '' then begin
    // Edit mode
    editName.Text := Obj.Name;
    Obj.Connection.ParseViewStructure(Obj.CreateCode, Obj, nil, Algorithm, Definer, SQLSecurity, CheckOption, SelectCode);
    comboDefiner.Text := Definer;
    rgAlgorithm.ItemIndex := rgAlgorithm.Items.IndexOf(Algorithm);
    rgCheck.ItemIndex := rgCheck.Items.IndexOf(CheckOption);
    if rgCheck.ItemIndex = -1 then
      rgCheck.ItemIndex := 0;
    for i:=0 to comboSecurity.Items.Count-1 do begin
      if LowerCase(SQLSecurity) = LowerCase(comboSecurity.Items[i]) then begin
        comboSecurity.ItemIndex := i;
        Break;
      end;
    end;
    SynMemoBody.Text := SelectCode;
    // User may not be allowed to run SHOW CREATE VIEW, in which case we have an empty CreateCode.
    // Disable editor in this case.
    lblDisabledWhy.Visible := SelectCode = '';
    editName.Enabled := not lblDisabledWhy.Visible;
    rgAlgorithm.Enabled := editName.Enabled;
    rgCheck.Enabled := rgAlgorithm.Enabled;
    SynMemoBody.Enabled := rgAlgorithm.Enabled;
  end else begin
    // Create mode
    editName.Text := '';
    rgAlgorithm.Enabled := True;
    rgAlgorithm.ItemIndex := 0;
    rgCheck.Enabled := True;
    rgCheck.ItemIndex := 0;
    comboSecurity.ItemIndex := 0;
    SynMemoBody.Text := 'SELECT ';
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
  (Sender as TComboBox).Items.Assign(DBObject.Connection.AllUserHostCombinations);
end;


{**
  Lookup "Create|Alter View" in SQL help
}
procedure TfrmView.btnHelpClick(Sender: TObject);
begin
  Help(Self, 'createview');
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
  if comboSecurity.Text <> '' then
    sql := sql + 'SQL SECURITY ' + UpperCase(comboSecurity.Text)+' ';
  sql := sql + 'VIEW ' + viewname+' AS '+SynMemoBody.Text+' ';
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
    Modified := False;
    btnSave.Enabled := Modified;
    btnDiscard.Enabled := Modified;
  except
    on E:EDbError do begin
      ErrorDialog(E.Message);
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
