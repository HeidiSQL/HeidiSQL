unit trigger_editor;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls, TntStdCtrls, SynEdit, SynMemo,
  SynCompletionProposal, VirtualTrees,
  mysql_connection, mysql_api, helpers;

type
  TFrame = TDBObjectEditor;
  TfrmTriggerEditor = class(TFrame)
    lblName: TLabel;
    editName: TTntEdit;
    SynMemoStatement: TSynMemo;
    btnHelp: TButton;
    btnDiscard: TButton;
    btnSave: TButton;
    comboTable: TTntComboBox;
    lblTable: TLabel;
    lblBody: TLabel;
    SynCompletionProposalStatement: TSynCompletionProposal;
    lblEvent: TLabel;
    comboTiming: TComboBox;
    comboEvent: TComboBox;
    procedure btnHelpClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure Modification(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure SynCompletionProposalStatementExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: WideString; var x, y: Integer; var CanExecute: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Init(ObjectName: WideString=''; ObjectType: TListNodeType=lntNone); override;
    procedure ApplyModifications; override;
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
  SynMemoStatement.Highlighter := Mainform.SynSQLSyn1;
  editName.MaxLength := NAME_LEN;
  comboTiming.Items.Text := 'BEFORE'+CRLF+'AFTER';
  comboEvent.Items.Text := 'INSERT'+CRLF+'UPDATE'+CRLF+'DELETE';
  for i:=0 to Mainform.SynCompletionProposal.Columns.Count-1 do begin
    col := SynCompletionProposalStatement.Columns.Add;
    col.BiggestWord := Mainform.SynCompletionProposal.Columns[i].BiggestWord;
  end;
  SynCompletionProposalStatement.NbLinesInWindow := Mainform.SynCompletionProposal.NbLinesInWindow;
  SynCompletionProposalStatement.Width := Mainform.SynCompletionProposal.Width;
  SynCompletionProposalStatement.Options := Mainform.SynCompletionProposal.Options;
  SynCompletionProposalStatement.TimerInterval := Mainform.SynCompletionProposal.TimerInterval;
  SynCompletionProposalStatement.ItemHeight := Mainform.SynCompletionProposal.ItemHeight;
  SynCompletionProposalStatement.Margin := Mainform.SynCompletionProposal.Margin;
  SynCompletionProposalStatement.Font := Font;
end;


procedure TfrmTriggerEditor.Init(ObjectName: WideString=''; ObjectType: TListNodeType=lntNone);
var
  Definitions: TMySQLQuery;
  DBObjects: TDBObjectList;
  i: Integer;
  Found: Boolean;
begin
  inherited;
  editName.Text := '';
  SynMemoStatement.Text := '';
  comboEvent.ItemIndex := 0;
  comboTiming.ItemIndex := 0;
  DBObjects := Mainform.Connection.GetDBObjects(Mainform.ActiveDatabase);
  comboTable.Items.Clear;
  for i:=0 to DBObjects.Count-1 do begin
    if DBObjects[i].NodeType in [lntTable] then
      comboTable.Items.Add(DBObjects[i].Name);
  end;
  if comboTable.Items.Count > 0 then
    comboTable.ItemIndex := 0;
  if FEditObjectName <> '' then begin
    // Edit mode
    editName.Text := FEditObjectName;
    Definitions := Mainform.Connection.GetResults('SHOW TRIGGERS FROM '+Mainform.mask(Mainform.ActiveDatabase));
    Found := False;
    while not Definitions.Eof do begin
      if Definitions.Col('Trigger') = FEditObjectName then begin
        comboTable.ItemIndex := comboTable.Items.IndexOf(Definitions.Col('Table'));
        comboTiming.ItemIndex := comboTiming.Items.IndexOf(UpperCase(Definitions.Col('Timing')));
        comboEvent.ItemIndex := comboEvent.Items.IndexOf(UpperCase(Definitions.Col('Event')));
        SynMemoStatement.Text := Definitions.Col('Statement');
        Found := True;
        break;
      end;
      Definitions.Next;
    end;
    FreeAndNil(Definitions);
    if not Found then
      Raise Exception.Create('Trigger definition not found!');
  end else begin
    editName.Text := 'Enter trigger name';
  end;
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
  Mainform.showstatus;
  Screen.Cursor := crDefault;
end;


procedure TfrmTriggerEditor.Modification(Sender: TObject);
begin
  // Enable buttons if anything has changed
  btnSave.Enabled := (editName.Text <> '') and (comboTable.ItemIndex > -1)
    and (comboTiming.ItemIndex > -1) and (comboEvent.ItemIndex > -1)
    and (SynMemoStatement.Text <> '');
  btnDiscard.Enabled := True;
  Modified := True;
end;


procedure TfrmTriggerEditor.btnDiscardClick(Sender: TObject);
begin
  // Reinit editor, discarding changes
  Modified := False;
  Init(FEditObjectName);
end;


procedure TfrmTriggerEditor.btnSaveClick(Sender: TObject);
begin
  ApplyModifications;
end;


procedure TfrmTriggerEditor.ApplyModifications;
var
  sql: WideString;
begin
  // Edit mode means we drop the trigger and recreate it, as there is no ALTER TRIGGER.
  try
    // In edit mode we could create a temporary trigger, but that would only cause an error a la
    // "This version of MySQL doesn't yet support multiple triggers with the same action time and event for one table"
    // So, we take the risk of loosing the trigger for cases in which the user has SQL errors in
    // his statement. The user must fix such errors and re-press "Save" while we have them in memory,
    // otherwise the trigger attributes are lost forever.
    if FEditObjectName <> '' then
      Mainform.Connection.Query('DROP TRIGGER IF EXISTS '+Mainform.mask(FEditObjectName));
    // CREATE
    //   [DEFINER = { user | CURRENT_USER }]
    //   TRIGGER trigger_name trigger_time trigger_event
    //   ON tbl_name FOR EACH ROW trigger_stmt
    sql := 'CREATE TRIGGER '+Mainform.mask(editName.Text)+' '+
      comboTiming.Items[comboTiming.ItemIndex]+' '+comboEvent.Items[comboEvent.ItemIndex]+
      ' ON '+Mainform.mask(comboTable.Text)+
      ' FOR EACH ROW '+SynMemoStatement.Text;
    Mainform.Connection.Query(sql);
    FEditObjectName := editName.Text;
    Mainform.SetEditorTabCaption(Self, FEditObjectName);
    Mainform.RefreshTreeDB(Mainform.ActiveDatabase);
    Modified := False;
    btnSave.Enabled := Modified;
    btnDiscard.Enabled := Modified;
  except on E:Exception do
    MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;


procedure TfrmTriggerEditor.SynCompletionProposalStatementExecute(Kind: SynCompletionType; Sender: TObject;
  var CurrentInput: WideString; var x, y: Integer; var CanExecute: Boolean);
var
  Proposal: TSynCompletionProposal;
  Token: WideString;
  Columns: TMySQLQuery;
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
      Columns := Mainform.Connection.GetResults('SHOW COLUMNS FROM '+Mainform.mask(comboTable.Text));
      while not Columns.Eof do begin
        Proposal.InsertList.Add(Columns.Col('Field'));
        Proposal.ItemList.Add(WideFormat(SYNCOMPLETION_PATTERN, [ICONINDEX_FIELD, GetFirstWord(Columns.Col('Type')), Columns.Col('Field')]) );
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

end.
