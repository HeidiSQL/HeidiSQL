unit routine_editor;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, SynEdit, SynMemo, StdCtrls,
  ComCtrls, ToolWin, VirtualTrees, SynRegExpr, WideStrUtils,
  dbconnection, helpers;

type
  TFrame = TDBObjectEditor;
  TfrmRoutineEditor = class(TFrame)
    btnSave: TButton;
    btnDiscard: TButton;
    btnHelp: TButton;
    lblSQLcode: TLabel;
    SynMemoBody: TSynMemo;
    PageControlMain: TPageControl;
    tabOptions: TTabSheet;
    tabParameters: TTabSheet;
    tabCreateCode: TTabSheet;
    chkDeterministic: TCheckBox;
    editComment: TEdit;
    comboSecurity: TComboBox;
    comboDataAccess: TComboBox;
    comboReturns: TComboBox;
    comboType: TComboBox;
    editName: TEdit;
    lblName: TLabel;
    lblType: TLabel;
    lblReturns: TLabel;
    lblSQL: TLabel;
    lblSecurity: TLabel;
    lblComment: TLabel;
    listParameters: TVirtualStringTree;
    tlbParameters: TToolBar;
    btnAddParam: TToolButton;
    btnRemoveParam: TToolButton;
    btnClearParams: TToolButton;
    SynMemoCREATEcode: TSynMemo;
    btnRunProc: TButton;
    lblDefiner: TLabel;
    comboDefiner: TComboBox;
    btnMoveUpParam: TToolButton;
    btnMoveDownParam: TToolButton;
    procedure comboTypeSelect(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnAddParamClick(Sender: TObject);
    procedure listParametersGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure listParametersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure btnClearParamsClick(Sender: TObject);
    procedure btnRemoveParamClick(Sender: TObject);
    procedure listParametersBeforePaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas);
    procedure listParametersFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure listParametersCreateEditor(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure listParametersNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure listParametersEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure Modification(Sender: TObject);
    procedure SynMemoBodyDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoBodyDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure listParametersPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure btnDiscardClick(Sender: TObject);
    procedure comboDefinerDropDown(Sender: TObject);
    procedure btnMoveParamClick(Sender: TObject);
  private
    { Private declarations }
    FAlterRoutineType: String;
    function ComposeCreateStatement(NameOfObject: String): String;
  public
    { Public declarations }
    Parameters: TRoutineParamList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(Obj: TDBObject); override;
    function ApplyModifications: TModalResult; override;
  end;


implementation

uses main, mysql_structures, grideditlinks;

{$R *.dfm}


constructor TfrmRoutineEditor.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  ScaleControls(Screen.PixelsPerInch, FORMS_DPI);
  // Combo items in a .dfm are sporadically lost after an IDE restart,
  // so we set them here to avoid developer annoyance
  comboType.Items.Add('Procedure (doesn''t return a result)');
  comboType.Items.Add('Function (returns a result)');
  comboDataAccess.Items.Add('Contains SQL');
  comboDataAccess.Items.Add('No SQL');
  comboDataAccess.Items.Add('Reads SQL data');
  comboDataAccess.Items.Add('Modifies SQL data');
  comboSecurity.Items.Add('Definer');
  comboSecurity.Items.Add('Invoker');
  for i := Low(Datatypes) to High(Datatypes) do
    comboReturns.Items.Add(Datatypes[i].Name);
  Mainform.SynCompletionProposal.AddEditor(SynMemoBody);
  FixVT(listParameters);
  Mainform.RestoreListSetup(listParameters);
  Parameters := TRoutineParamList.Create;
  editName.MaxLength := NAME_LEN;
end;


destructor TfrmRoutineEditor.Destroy;
begin
  // Store GUI setup
  OpenRegistry;
  Mainform.SaveListSetup(listParameters);
  inherited;
end;


procedure TfrmRoutineEditor.Init(Obj: TDBObject);
var
  Definer, Returns, DataAccess, Security, Comment, Body: String;
  Deterministic: Boolean;
begin
  inherited;
  if Obj.NodeType = lntProcedure then FAlterRoutineType := 'PROCEDURE'
  else FAlterRoutineType := 'FUNCTION';
  editName.Text := DBObject.Name;
  comboType.ItemIndex := 0;
  comboReturns.Text := '';
  chkDeterministic.Checked := False;
  listParameters.FocusedNode := nil;
  listParameters.Clear;
  Parameters.Clear;
  comboDataAccess.ItemIndex := 0;
  comboSecurity.ItemIndex := 0;
  editComment.Clear;
  comboDefiner.Text := '';
  comboDefiner.TextHint := 'Current user ('+Obj.Connection.CurrentUserHostCombination+')';
  comboDefiner.Hint := 'Leave empty for current user ('+Obj.Connection.CurrentUserHostCombination+')';
  SynMemoBody.Text := 'BEGIN'+CRLF+CRLF+'END';
  if DBObject.Name <> '' then begin
    // Editing existing routine
    comboType.ItemIndex := ListIndexByRegExpr(comboType.Items, '^'+FAlterRoutineType+'\b');
    DBObject.Connection.ParseRoutineStructure(Obj.CreateCode, Parameters, Deterministic, Definer, Returns, DataAccess, Security, Comment, Body);
    comboReturns.Text := Returns;
    chkDeterministic.Checked := Deterministic;
    if DataAccess <> '' then
      comboDataAccess.ItemIndex := comboDataAccess.Items.IndexOf(DataAccess);
    if Security <> '' then
      comboSecurity.ItemIndex := comboSecurity.Items.IndexOf(Security);
    editComment.Text := Comment;
    comboDefiner.Text := Definer;
    SynMemoBody.Text := Body;
  end else begin
    editName.Text := '';
  end;
  comboTypeSelect(comboType);
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
  Mainform.actRunRoutines.Enabled := DBObject.Name <> '';
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


procedure TfrmRoutineEditor.Modification(Sender: TObject);
begin
  Modified := True;
  btnSave.Enabled := Modified and (editName.Text <> '');
  btnDiscard.Enabled := Modified;
  SynMemoCreateCode.Text := ComposeCreateStatement(editName.Text);
end;


procedure TfrmRoutineEditor.comboTypeSelect(Sender: TObject);
var
  isfunc: Boolean;
begin
  isfunc := (Sender as TComboBox).ItemIndex = 1;
  lblReturns.Enabled := isfunc;
  comboReturns.Enabled := isfunc;
  Modification(Sender);
  listParameters.Repaint;
end;


procedure TfrmRoutineEditor.comboDefinerDropDown(Sender: TObject);
begin
  // Populate definers from mysql.user
  (Sender as TComboBox).Items.Assign(GetDefiners);
end;


procedure TfrmRoutineEditor.btnAddParamClick(Sender: TObject);
var
  Param: TRoutineParam;
begin
  Param := TRoutineParam.Create;
  Param.Name := 'Param'+IntToStr(Parameters.Count+1);
  Param.Datatype := 'INT';
  Param.Context := 'IN';
  Parameters.Add(Param);
  // See List.OnPaint:
  listParameters.Repaint;
  Modification(Sender);
end;


procedure TfrmRoutineEditor.btnRemoveParamClick(Sender: TObject);
begin
  Parameters.Delete(ListParameters.FocusedNode.Index);
  listParameters.Repaint;
  Modification(Sender);
end;


procedure TfrmRoutineEditor.btnClearParamsClick(Sender: TObject);
begin
  Parameters.Clear;
  listParameters.Repaint;
  Modification(Sender);
end;


procedure TfrmRoutineEditor.btnMoveParamClick(Sender: TObject);
var
  Source, Target: Integer;
begin
  // Move param up or down
  Source := ListParameters.FocusedNode.Index;
  if Sender = btnMoveUpParam then
    Target := Source -1
  else
    Target := Source +1;
  Parameters.Exchange(Source, Target);
  SelectNode(listParameters, Target);
  listParameters.Repaint;
  Modification(Sender);
end;


procedure TfrmRoutineEditor.listParametersBeforePaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas);
begin
  (Sender as TVirtualStringTree).RootNodeCount := Parameters.Count;
end;


procedure TfrmRoutineEditor.listParametersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  List: TVirtualStringTree;
  Context: String;
begin
  // Draw arrow icon to indicate in/out context
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  List := Sender as TVirtualStringTree;
  if Column <> 3 then
    ImageIndex := -1
  else begin
    Context := List.Text[Node, 3];
    if Context = 'IN' then ImageIndex := 120
    else if Context = 'OUT' then ImageIndex := 121
    else if Context = 'INOUT' then ImageIndex := 122;
  end;
end;


procedure TfrmRoutineEditor.listParametersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Param: TRoutineParam;
begin
  Param := Parameters[Node.Index];
  case Column of
    0: CellText := IntToStr(Node.Index+1);
    1: CellText := Param.Name;
    2: CellText := Param.Datatype;
    3: begin
      if comboType.ItemIndex = 1 then
        CellText := 'IN' // A function can only have IN parameters
      else
        CellText := Param.Context;
    end;
  end;
end;


procedure TfrmRoutineEditor.listParametersPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if (Column = 3) and (comboType.ItemIndex = 1) then
    TargetCanvas.Font.Color := clBtnShadow;
end;


procedure TfrmRoutineEditor.listParametersFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  HasNode: Boolean;
begin
  // Enable/disable buttons
  HasNode := Assigned(Node);
  btnRemoveParam.Enabled := HasNode;
  btnMoveUpParam.Enabled := HasNode and (Node <> Sender.GetFirst);
  btnMoveDownParam.Enabled := HasNode and (Node <> Sender.GetLast);

  if HasNode and (not ((comboType.ItemIndex = 1) and (Column=3))) then
    Sender.EditNode(Node, Column);
end;


procedure TfrmRoutineEditor.listParametersNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  Param: TRoutineParam;
begin
  Param := Parameters[Node.Index];
  case Column of
    1: Param.Name := NewText;
    2: Param.Datatype := NewText;
    3: Param.Context := NewText;
  end;
  Modification(Sender);
end;


procedure TfrmRoutineEditor.listParametersCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  VT: TVirtualStringTree;
  EnumEditor: TEnumEditorLink;
  i: Integer;
  Datatype: String;
begin
  VT := Sender as TVirtualStringTree;
  if Column = 1 then
    EditLink := TStringEditLink.Create
  else if Column = 2 then begin
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.AllowCustomText := True;
    EnumEditor.ValueList := TStringList.Create;
    for i:=Low(Datatypes) to High(Datatypes) do begin
      Datatype := Datatypes[i].Name;
      if Datatypes[i].RequiresLength then
        Datatype := Datatype + '(' + Datatypes[i].DefLengthSet + ')';
      EnumEditor.ValueList.Add(Datatype);
    end;
    EditLink := EnumEditor;
  end else if Column = 3 then begin
    EnumEditor := TEnumEditorLink.Create(VT);
    EnumEditor.ValueList := TStringList.Create;
    EnumEditor.ValueList.Add('IN');
    EnumEditor.ValueList.Add('OUT');
    EnumEditor.ValueList.Add('INOUT');
    EditLink := EnumEditor;
  end;
end;


procedure TfrmRoutineEditor.listParametersEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  // Do not allow the number cells to be edited
  Allowed := Column > 0;
  if (Column = 3) and (comboType.ItemIndex = 1) then begin
    Allowed := False;
    MessageDlg('A stored function can only have IN parameters so context editing is blocked.', mtInformation, [mbOK], 0);
  end;
end;


procedure TfrmRoutineEditor.SynMemoBodyDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  // Allow dragging parameters here
  Accept := Source = listParameters;
  // set cursor position
  SynMemoBody.CaretX := (x - SynMemoBody.Gutter.Width) div SynMemoBody.CharWidth - 1 + SynMemoBody.LeftChar;
  SynMemoBody.CaretY := y div SynMemoBody.LineHeight + SynMemoBody.TopLine;
  if not SynMemoBody.Focused then
    SynMemoBody.SetFocus;
end;


procedure TfrmRoutineEditor.SynMemoBodyDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  list: TVirtualStringTree;
  memo: TSynMemo;
begin
  list := Source as TVirtualStringTree;
  memo := Sender as TSynMemo;
  memo.SelText := list.Text[list.FocusedNode, 1];
end;


procedure TfrmRoutineEditor.btnSaveClick(Sender: TObject);
begin
  // Apply or OK button clicked
  ApplyModifications;
end;


function TfrmRoutineEditor.ApplyModifications: TModalResult;
var
  TempName: String;
  i: Integer;
  allRoutineNames: TStringList;
  ProcOrFunc: String;
  TargetExists: Boolean;
begin
  // Save changes
  Result := mrOk;
  ProcOrFunc := UpperCase(GetFirstWord(comboType.Text));

  // There is no way to ALTER parameters or the name of it.
  // Create a temp routine, check for syntax errors, then drop the old routine and create it.
  // See also: http://dev.mysql.com/doc/refman/5.0/en/alter-procedure.html
  try
    if DBObject.Name <> '' then begin
      // Create temp name
      i := 0;
      allRoutineNames := MainForm.ActiveConnection.GetCol('SELECT ROUTINE_NAME FROM '+QuoteIdent(DBNAME_INFORMATION_SCHEMA)+'.'+QuoteIdent('ROUTINES')+
        ' WHERE ROUTINE_SCHEMA = '+esc(Mainform.ActiveDatabase)+
        ' AND ROUTINE_TYPE = '+esc(ProcOrFunc)
        );
      TargetExists := ((editName.Text <> DBObject.Name) or (ProcOrFunc <> FAlterRoutineType)) and
        (allRoutineNames.IndexOf(editName.Text) > -1);
      if TargetExists then begin
        Result := MessageDlg('Routine "'+editName.Text+'" already exists. Overwrite it?',
          mtConfirmation, [mbYes, mbNo, mbCancel], 0);
        if Result = mrNo then
          Exit;
      end;
      while True do begin
        inc(i);
        TempName := APPNAME + '_temproutine_' + IntToStr(i);
        if allRoutineNames.IndexOf(TempName) = -1 then
          break;
      end;
      MainForm.ActiveConnection.Query(ComposeCreateStatement(tempName));
      // Drop temporary routine, used for syntax checking
      MainForm.ActiveConnection.Query('DROP '+ProcOrFunc+' IF EXISTS '+QuoteIdent(TempName));
      // Drop edited routine
      MainForm.ActiveConnection.Query('DROP '+FAlterRoutineType+' IF EXISTS '+QuoteIdent(DBObject.Name));
      if TargetExists then begin
        // Drop target routine - overwriting has been confirmed, see above
        MainForm.ActiveConnection.Query('DROP '+ProcOrFunc+' IF EXISTS '+QuoteIdent(editName.Text));
      end;
    end;
    MainForm.ActiveConnection.Query(ComposeCreateStatement(editName.Text));
    // Set editing name if create/alter query was successful
    DBObject.Name := editName.Text;
    DBObject.CreateCode := '';
    FAlterRoutineType := UpperCase(GetFirstWord(comboType.Text));
    if FAlterRoutineType = 'PROCEDURE' then DBObject.NodeType := lntProcedure
    else DBObject.NodeType := lntFunction;
    Mainform.UpdateEditorTab;
    Mainform.RefreshTree(DBObject);
    Modified := False;
    btnSave.Enabled := Modified;
    btnDiscard.Enabled := Modified;
    Mainform.actRunRoutines.Enabled := True;
  except
    on E:EDatabaseError do begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
      Result := mrAbort;
    end;
  end;
end;


function TfrmRoutineEditor.ComposeCreateStatement(NameOfObject: String): String;
var
  ProcOrFunc: String;
  i: Integer;
begin
  ProcOrFunc := UpperCase(GetFirstWord(comboType.Text));
  Result := 'CREATE ';
  if comboDefiner.Text <> '' then
    Result := Result + 'DEFINER='+QuoteIdent(comboDefiner.Text, True, '@')+' ';
  Result := Result + ProcOrFunc+' '+QuoteIdent(NameOfObject)+'(';
  for i:=0 to Parameters.Count-1 do begin
    if ProcOrFunc = 'PROCEDURE' then
      Result := Result + Parameters[i].Context + ' ';
    Result := Result + QuoteIdent(Parameters[i].Name) + ' ' + Parameters[i].Datatype;
    if i < Parameters.Count-1 then
      Result := Result + ', ';
  end;
  Result := Result + ')'+CRLF;
  if comboReturns.Enabled then
    Result := Result + #9 + 'RETURNS '+comboReturns.Text+CRLF;
  Result := Result + #9 + 'LANGUAGE SQL'+CRLF + #9;
  if not chkDeterministic.Checked then
    Result := Result + 'NOT ';
  Result := Result + 'DETERMINISTIC'+CRLF
    + #9 + UpperCase(comboDataAccess.Text)+CRLF
    + #9 + 'SQL SECURITY ' + UpperCase(comboSecurity.Text)+CRLF
    + #9 + 'COMMENT ' + esc(editComment.Text)+CRLF
    + SynMemoBody.Text;
end;


procedure TfrmRoutineEditor.btnDiscardClick(Sender: TObject);
begin
  Modified := False;
  Init(DBObject);
end;


procedure TfrmRoutineEditor.btnHelpClick(Sender: TObject);
begin
  // Help button
  Mainform.CallSQLHelpWithKeyword('CREATE PROCEDURE');
end;


end.
