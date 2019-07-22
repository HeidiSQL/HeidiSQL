unit trigger_editor;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
  SynCompletionProposal, SynRegExpr,
  dbconnection, dbstructures, apphelpers, gnugettext, ComCtrls;

type
  TFrame = TDBObjectEditor;
  TfrmTriggerEditor = class(TFrame)
    SynMemoBody: TSynMemo;
    btnHelp: TButton;
    btnDiscard: TButton;
    btnSave: TButton;
    lblBody: TLabel;
    SynCompletionProposalStatement: TSynCompletionProposal;
    PageControlMain: TPageControl;
    tabOptions: TTabSheet;
    tabCreateCode: TTabSheet;
    comboDefiner: TComboBox;
    lblDefiner: TLabel;
    editName: TEdit;
    lblName: TLabel;
    lblTable: TLabel;
    comboTable: TComboBox;
    lblEvent: TLabel;
    comboTiming: TComboBox;
    comboEvent: TComboBox;
    SynMemoCreateCode: TSynMemo;
    procedure btnHelpClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure SynCompletionProposalStatementExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: String; var x, y: Integer; var CanExecute: Boolean);
    procedure comboDefinerDropDown(Sender: TObject);
    procedure comboChange(Sender: TObject);
  private
    { Private declarations }
    function ComposeCreateStatement: String;
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
constructor TfrmTriggerEditor.Create(AOwner: TComponent);
var
  col: TProposalColumn;
  i: Integer;
begin
  inherited;
  SynMemoBody.Highlighter := Mainform.SynSQLSynUsed;
  editName.MaxLength := NAME_LEN;
  comboTiming.Items.Text := 'BEFORE'+CRLF+'AFTER';
  comboEvent.Items.Text := 'INSERT'+CRLF+'UPDATE'+CRLF+'DELETE';
  for i:=0 to Mainform.SynCompletionProposal.Columns.Count-1 do begin
    col := SynCompletionProposalStatement.Columns.Add;
    col.ColumnWidth := Mainform.SynCompletionProposal.Columns[i].ColumnWidth;
  end;
  SynCompletionProposalStatement.NbLinesInWindow := Mainform.SynCompletionProposal.NbLinesInWindow;
  SynCompletionProposalStatement.Width := Mainform.SynCompletionProposal.Width;
  SynCompletionProposalStatement.Options := Mainform.SynCompletionProposal.Options;
  SynCompletionProposalStatement.TimerInterval := Mainform.SynCompletionProposal.TimerInterval;
  SynCompletionProposalStatement.ItemHeight := Mainform.SynCompletionProposal.ItemHeight;
  SynCompletionProposalStatement.Margin := Mainform.SynCompletionProposal.Margin;
  SynCompletionProposalStatement.Font := Font;
end;


procedure TfrmTriggerEditor.Init(Obj: TDBObject);
var
  Definitions: TDBQuery;
  DBObjects: TDBObjectList;
  i: Integer;
  Found: Boolean;
  Body: String;
  rx: TRegExpr;
begin
  inherited;
  editName.Text := '';
  comboDefiner.Text := '';
  comboDefiner.TextHint := f_('Current user (%s)', [Obj.Connection.CurrentUserHostCombination]);
  comboDefiner.Hint := f_('Leave empty for current user (%s)', [Obj.Connection.CurrentUserHostCombination]);
  SynMemoBody.Text := 'BEGIN'+CRLF+CRLF+'END';
  comboEvent.ItemIndex := 0;
  comboTiming.ItemIndex := 0;
  DBObjects := MainForm.ActiveConnection.GetDBObjects(Mainform.ActiveDatabase);
  comboTable.Items.Clear;
  for i:=0 to DBObjects.Count-1 do begin
    if DBObjects[i].NodeType in [lntTable] then
      comboTable.Items.Add(DBObjects[i].Name);
  end;
  if comboTable.Items.Count > 0 then
    comboTable.ItemIndex := 0;
  if DBObject.Name <> '' then begin
    // Edit mode
    editName.Text := DBObject.Name;
    Definitions := MainForm.ActiveConnection.GetResults('SHOW TRIGGERS FROM '+Obj.Connection.QuoteIdent(Mainform.ActiveDatabase));
    Found := False;
    while not Definitions.Eof do begin
      if Definitions.Col('Trigger') = DBObject.Name then begin
        // "Definer" column available since MySQL 5.0.17
        comboDefiner.Text := Definitions.Col('Definer', True);
        comboTable.ItemIndex := comboTable.Items.IndexOf(Definitions.Col('Table'));
        comboTiming.ItemIndex := comboTiming.Items.IndexOf(UpperCase(Definitions.Col('Timing')));
        comboEvent.ItemIndex := comboEvent.Items.IndexOf(UpperCase(Definitions.Col('Event')));
        // "Statement" column from SHOW TRIGGERS does not escape single quotes where required.
        // See http://www.heidisql.com/forum.php?t=16501
        // But SHOW CREATE TRIGGER was introduced in MySQL 5.1.21
        // See http://www.heidisql.com/forum.php?t=16662
        if DBObject.Connection.ServerVersionInt < 50121 then begin
          Body := Definitions.Col('Statement');
        end else begin
          rx := TRegExpr.Create;
          rx.ModifierI := True;
          rx.Expression := 'FOR\s+EACH\s+ROW\s+(.+)$';
          try
            Body := DBObject.Connection.GetCreateCode(DBObject);
            if rx.Exec(Body) then
              Body := rx.Match[1]
            else
              raise EDbError.CreateFmt(_('Result from previous query does not contain expected pattern: %s'), [rx.Expression]);
          except
            on E:EDbError do begin
              DBObject.Connection.Log(lcError, E.Message);
              Body := Definitions.Col('Statement');
            end;
          end;
        end;
        SynMemoBody.Text := Body;
        Found := True;
        break;
      end;
      Definitions.Next;
    end;
    FreeAndNil(Definitions);
    if not Found then
      Raise Exception.Create(_('Trigger definition not found!'));
  end else begin
    editName.Text := '';
    if MainForm.FocusedTables.Count > 0 then begin
      for i:=0 to comboTable.Items.Count-1 do begin
        if comboTable.Items[i] = MainForm.FocusedTables[0].Name then begin
          comboTable.ItemIndex := i;
          comboChange(comboTable);
          Break;
        end;
      end;
    end;
  end;
  // Buttons are randomly moved, since VirtualTree update, see #440
  btnSave.Top := Height - btnSave.Height - 3;
  btnHelp.Top := btnSave.Top;
  btnDiscard.Top := btnSave.Top;
  Modification(Self);
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


procedure TfrmTriggerEditor.Modification(Sender: TObject);
begin
  // Enable buttons if anything has changed
  Modified := True;
  btnSave.Enabled := Modified
    and (editName.Text <> '') and (comboTable.ItemIndex > -1)
    and (comboTiming.ItemIndex > -1) and (comboEvent.ItemIndex > -1)
    and (SynMemoBody.Text <> '');
  btnDiscard.Enabled := Modified;
  SynMemoCreateCode.Text := ComposeCreateStatement;
end;


procedure TfrmTriggerEditor.btnDiscardClick(Sender: TObject);
begin
  // Reinit editor, discarding changes
  Modified := False;
  Init(DBObject);
end;


procedure TfrmTriggerEditor.btnSaveClick(Sender: TObject);
begin
  ApplyModifications;
end;


procedure TfrmTriggerEditor.comboChange(Sender: TObject);
begin
  // Auto generate trigger name as long as it was not user-edited. See issue #3477.
  if (DBObject.Name = '') and (not editName.Modified) then
    editName.Text := comboTable.Text+'_'+LowerCase(comboTiming.Text)+'_'+LowerCase(comboEvent.Text);
  Modification(Sender);
end;


procedure TfrmTriggerEditor.comboDefinerDropDown(Sender: TObject);
begin
  // Populate definers from mysql.user
  (Sender as TComboBox).Items.Assign(DBObject.Connection.AllUserHostCombinations);
end;


function TfrmTriggerEditor.ApplyModifications: TModalResult;
begin
  // Edit mode means we drop the trigger and recreate it, as there is no ALTER TRIGGER.
  Result := mrOk;
  try
    // In edit mode we could create a temporary trigger, but that would only cause an error a la
    // "This version of MySQL doesn't yet support multiple triggers with the same action time and event for one table"
    // So, we take the risk of loosing the trigger for cases in which the user has SQL errors in
    // his statement. The user must fix such errors and re-press "Save" while we have them in memory,
    // otherwise the trigger attributes are lost forever.
    if DBObject.Name <> '' then try
      DBObject.Connection.Query('DROP TRIGGER '+DBObject.Connection.QuoteIdent(DBObject.Name));
    except
    end;
    MainForm.ActiveConnection.Query(ComposeCreateStatement);
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


procedure TfrmTriggerEditor.SynCompletionProposalStatementExecute(Kind: SynCompletionType; Sender: TObject;
  var CurrentInput: String; var x, y: Integer; var CanExecute: Boolean);
var
  Proposal: TSynCompletionProposal;
  Token: String;
  Columns: TDBQuery;
begin
  // Propose column names from referencing table
  Proposal := Sender as TSynCompletionProposal;
  Token := UpperCase(Proposal.PreviousToken);
  Proposal.InsertList.Clear;
  Proposal.ItemList.Clear;
  if (Token = 'NEW') or (Token = 'OLD') then begin
    if comboTable.Text = '' then
      CanExecute := False
    else try
      Columns := DBObject.Connection.GetResults('SHOW COLUMNS FROM '+DBObject.Connection.QuoteIdent(comboTable.Text));
      while not Columns.Eof do begin
        Proposal.InsertList.Add(Columns.Col('Field'));
        Proposal.ItemList.Add(Format(SYNCOMPLETION_PATTERN, [ICONINDEX_FIELD, GetFirstWord(Columns.Col('Type')), Columns.Col('Field'), '']) );
        Columns.Next;
      end;
    except
    end;
  end else
    Mainform.SynCompletionProposalExecute(Kind, Sender, CurrentInput, x, y, CanExecute);
end;


procedure TfrmTriggerEditor.btnHelpClick(Sender: TObject);
begin
  Help(Self, 'createtrigger');
end;


function TfrmTriggerEditor.ComposeCreateStatement: String;
begin
  // CREATE
  //   [DEFINER = { user | CURRENT_USER }]
  //   TRIGGER trigger_name trigger_time trigger_event
  //   ON tbl_name FOR EACH ROW trigger_stmt
  Result := 'CREATE ';
  if comboDefiner.Text <> '' then
    Result := Result + 'DEFINER='+DBObject.Connection.QuoteIdent(comboDefiner.Text, True, '@')+' ';
  Result := Result + 'TRIGGER '+DBObject.Connection.QuoteIdent(editName.Text)+' '+
    comboTiming.Items[comboTiming.ItemIndex]+' '+comboEvent.Items[comboEvent.ItemIndex]+
    ' ON '+DBObject.Connection.QuoteIdent(comboTable.Text)+
    ' FOR EACH ROW '+SynMemoBody.Text;
end;


end.
