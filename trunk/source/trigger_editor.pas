unit trigger_editor;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, SynEdit, SynMemo,
  SynCompletionProposal,
  dbconnection, mysql_structures, helpers, gnugettext, ComCtrls;

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
  TranslateComponent(Self);
  SynMemoBody.Highlighter := Mainform.SynSQLSyn1;
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
        SynMemoBody.Text := Definitions.Col('Statement');
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
  end;
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


procedure TfrmTriggerEditor.comboDefinerDropDown(Sender: TObject);
begin
  // Populate definers from mysql.user
  (Sender as TComboBox).Items.Assign(GetDefiners);
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
    on E:EDatabaseError do begin
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
  Mainform.CallSQLHelpWithKeyword('TRIGGERS');
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
