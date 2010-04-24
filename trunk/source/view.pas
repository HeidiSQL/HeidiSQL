unit view;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
  ExtCtrls, SynRegExpr,
  mysql_connection, helpers, mysql_api;

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
    procedure btnHelpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure editNameChange(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure Modification(Sender: TObject);
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
  Results: TMySQLQuery;
  db: String;
  rx: TRegExpr;
begin
  inherited;
  if Obj.Name <> '' then begin
    // Edit mode
    editName.Text := Obj.Name;
    db := Mainform.ActiveDatabase;
    Results := Mainform.Connection.GetResults('SELECT * FROM '+Mainform.mask(DBNAME_INFORMATION_SCHEMA)+'.VIEWS ' +
      'WHERE TABLE_SCHEMA = '+esc(db)+' AND TABLE_NAME = '+esc(Obj.Name));
    if Results.RecordCount = 0 then
      raise Exception.Create('Can''t find view definition for "'+Obj.Name+'" in '+DBNAME_INFORMATION_SCHEMA);
    // Algorithm is not changeable as we cannot look up its current state!
    rgAlgorithm.Enabled := False;
    rgAlgorithm.ItemIndex := 0;
    rgCheck.ItemIndex := rgCheck.Items.IndexOf(Results.Col('CHECK_OPTION'));
    rgCheck.Enabled := Results.Col('IS_UPDATABLE') = 'YES';

    rx := TRegExpr.Create;
    rx.ModifierG := True;
    rx.ModifierI := True;
    rx.Expression := '\s+WITH\s+\w+\s+CHECK\s+OPTION$';
    SynMemoSelect.Text := rx.Replace(Results.Col('VIEW_DEFINITION'), '', false);
    rx.Free;
  end else begin
    // Create mode
    editName.Text := '';
    rgAlgorithm.Enabled := True;
    rgAlgorithm.ItemIndex := 0;
    rgCheck.Enabled := True;
    rgCheck.ItemIndex := 0;
    SynMemoSelect.Text := 'SELECT ';
  end;
  // Ensure name is validated
  editNameChange(Self);
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


{**
  View name has changed: Check for valid naming
}
procedure TfrmView.editNameChange(Sender: TObject);
begin
  try
    ensureValidIdentifier( editName.Text );
    editName.Font.Color := clWindowText;
    editName.Color := clWindow;
  except
    editName.Font.Color := clRed;
    editName.Color := clYellow;
  end;
  Modification(Sender);
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
  viewname := Mainform.mask(viewname);
  if rgAlgorithm.Enabled and (rgAlgorithm.ItemIndex > -1) then
    sql := sql + 'ALGORITHM = '+Uppercase(rgAlgorithm.Items[rgAlgorithm.ItemIndex])+' ';
  sql := sql + 'VIEW ' + viewname+' AS '+SynMemoSelect.Text+' ';
  if rgCheck.Enabled and (rgCheck.ItemIndex > 0) then
    sql := sql + 'WITH '+Uppercase(rgCheck.Items[rgCheck.ItemIndex])+' CHECK OPTION';

  try
    Mainform.Connection.Query(sql);
    // Probably rename view
    if (DBObject.Name <> '') and (DBObject.Name <> editName.Text) then begin
      renamed := Mainform.mask(editName.Text);
      Mainform.Connection.Query('RENAME TABLE '+viewname + ' TO '+renamed);
    end;
    DBObject.Name := editName.Text;
    Mainform.UpdateEditorTab;
    Mainform.RefreshActiveTreeDB(DBObject);
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
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
end;


end.
