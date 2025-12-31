unit view;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SynEdit,
  ExtCtrls, Menus, LCLProc,
  dbconnection, dbstructures, dbstructures.mysql, apphelpers, ComCtrls, extra_controls;

type
  TFrame = TDBObjectEditor;
  TfrmView = class(TFrame)
    SynMemoBody: TSynEdit;
    lblSelect: TLabel;
    btnDiscard: TButton;
    btnSave: TButton;
    btnHelp: TButton;
    lblDisabledWhy: TLabel;
    PageControlMain: TPageControl;
    tabOptions: TTabSheet;
    tabCreateCode: TTabSheet;
    rgAlgorithm: TRadioGroup;
    lblName: TLabel;
    editName: TEdit;
    lblDefiner: TLabel;
    comboDefiner: TComboBox;
    comboSecurity: TComboBox;
    lblSecurity: TLabel;
    rgCheck: TRadioGroup;
    SynMemoCreateCode: TSynEdit;
    procedure btnHelpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure comboDefinerDropDown(Sender: TObject);
    procedure PageControlMainChange(Sender: TObject);
  private
    { Private declarations }
    function ComposeCreateStatement: TSQLBatch;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Init(Obj: TDBObject); override;
    function ApplyModifications: TModalResult; override;
  end;

  
implementation

uses main;

{$R *.lfm}


{**
  Create: Restore GUI setup
}
constructor TfrmView.Create(AOwner: TComponent);
begin
  inherited;
  SynMemoBody.Highlighter := Mainform.SynSQLSynUsed;
  Mainform.SynCompletionProposal.AddEditor(SynMemoBody);
  SynMemoCreateCode.Highlighter := Mainform.SynSQLSynUsed;
  Mainform.SynCompletionProposal.AddEditor(SynMemoCreateCode);
  editName.MaxLength := NAME_LEN;
  comboSecurity.Items.Add('Definer');
  comboSecurity.Items.Add('Invoker');
  FMainSynMemo := SynMemoBody;
  btnSave.Hint := ShortCutToText(MainForm.actSaveSQL.ShortCut);
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
    Algorithm := '';
    Definer := '';
    SQLSecurity := '';
    CheckOption := '';
    SelectCode := '';
    Obj.Connection.ParseViewStructure(Obj.CreateCode, Obj, Algorithm, Definer, SQLSecurity, CheckOption, SelectCode);
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
    SynMemoBody.TopLine := FMainSynMemoPreviousTopLine;
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

  // Most clauses only supported by MySQL
  comboDefiner.Enabled := comboDefiner.Enabled and Obj.Connection.Parameters.IsAnyMySQL;
  lblDefiner.Enabled := comboDefiner.Enabled;
  comboSecurity.Enabled := comboSecurity.Enabled and Obj.Connection.Parameters.IsAnyMySQL;
  lblSecurity.Enabled := comboSecurity.Enabled;
  rgAlgorithm.Enabled := rgAlgorithm.Enabled and Obj.Connection.Parameters.IsAnyMySQL;
  rgCheck.Enabled := rgCheck.Enabled and Obj.Connection.Parameters.IsAnyMySQL;

  // Update create code tab
  Modification(Self);
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
  Mainform.ShowStatusMsg;
  TExtForm.PageControlTabHighlight(PageControlMain);
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
  Batch: TSQLBatch;
  Query: TSQLSentence;
begin
  // Save changes
  Result := mrOk;

  try
    Batch := ComposeCreateStatement;
    for Query in Batch do begin
      DBObject.Connection.Query(Query.SQL);
    end;
    Batch.Free;
    DBObject.Name := editName.Text;
    DBObject.UnloadDetails;
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
var
  Batch: TSQLBatch;
begin
  Modified := True;
  btnSave.Enabled := Modified and (editName.Text <> '');
  btnDiscard.Enabled := Modified;
  // Update create code
  Batch := ComposeCreateStatement;
  SynMemoCreateCode.Text := Batch.SQL;
  Batch.Free;
end;


procedure TfrmView.PageControlMainChange(Sender: TObject);
begin
  TExtForm.PageControlTabHighlight(PageControlMain);
end;


function TfrmView.ComposeCreateStatement: TSQLBatch;
var
  sql, ViewName, RenameView: String;
begin
  // Create or Alter code
  if not ObjectExists then begin
    sql := 'CREATE ';
    ViewName := editName.Text;
  end else begin
    sql := 'ALTER ';
    ViewName := DBObject.Name;
  end;
  ViewName := DBObject.Connection.QuoteIdent(ViewName);
  if rgAlgorithm.Enabled and (rgAlgorithm.ItemIndex > -1) then
    sql := sql + 'ALGORITHM = '+Uppercase(rgAlgorithm.Items[rgAlgorithm.ItemIndex])+' ';
  if comboDefiner.Enabled and (comboDefiner.Text <> '') then
    sql := sql + 'DEFINER='+DBObject.Connection.QuoteIdent(comboDefiner.Text, True, '@')+' ';
  if comboSecurity.Enabled and (comboSecurity.Text <> '') then
    sql := sql + 'SQL SECURITY ' + UpperCase(comboSecurity.Text)+' ';
  sql := sql + 'VIEW ' + ViewName+' AS '+SynMemoBody.Text+' ';
  if rgCheck.Enabled and (rgCheck.ItemIndex > 0) then
    sql := sql + 'WITH '+Uppercase(rgCheck.Items[rgCheck.ItemIndex])+' CHECK OPTION';
  sql := sql + ';' + sLineBreak;

  if ObjectExists and (DBObject.Name <> editName.Text) then begin
    RenameView := DBObject.Connection.QuoteIdent(editName.Text);
    sql := sql + 'RENAME TABLE '+ViewName + ' TO '+RenameView + ';' + sLineBreak;
  end;

  Result := TSQLBatch.Create;
  Result.SQL := Trim(SQL);
end;


end.
