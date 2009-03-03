unit routine_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynMemo, StdCtrls, TntStdCtrls, ComCtrls, ToolWin,
  VirtualTrees, WideStrings, db, SynRegExpr;

type
  TfrmRoutineEditor = class(TForm)
    btnApply: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    btnHelp: TButton;
    lblName: TLabel;
    lblType: TLabel;
    lblReturns: TLabel;
    comboReturns: TComboBox;
    comboType: TTNTComboBox;
    editName: TTntEdit;
    lblParameters: TLabel;
    tlbParameters: TToolBar;
    btnAddParam: TToolButton;
    btnRemoveParam: TToolButton;
    btnClearParams: TToolButton;
    listParameters: TVirtualStringTree;
    lblSQL: TLabel;
    comboDataAccess: TComboBox;
    lblSecurity: TLabel;
    comboSecurity: TComboBox;
    lblComment: TLabel;
    editComment: TTntEdit;
    chkDeterministic: TCheckBox;
    lblSQLcode: TLabel;
    SynMemoBody: TSynMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure comboTypeSelect(Sender: TObject);
    procedure PostChanges(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure editNameChange(Sender: TObject);
    procedure btnAddParamClick(Sender: TObject);
    procedure listParametersGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
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
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure listParametersEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure Modification(Sender: TObject);
    procedure SynMemoBodyDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynMemoBodyDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure listParametersPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  private
    { Private declarations }
    Parameters: TWideStringList;
    AlterRoutineType: String;
    FModified: Boolean;
    procedure SetModified(Value: Boolean);
    property Modified: Boolean read FModified write SetModified;
  public
    { Public declarations }
    AlterRoutineName: WideString;
  end;


implementation

uses main, helpers, mysql_structures, grideditlinks;

{$R *.dfm}

const
  DELIM = '|';


procedure TfrmRoutineEditor.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Restore form dimensions
  Width := GetRegValue(REGNAME_PROCEDITOR_WIDTH, Width);
  Height := GetRegValue(REGNAME_PROCEDITOR_HEIGHT, Height);
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
  for i := Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
    comboReturns.Items.Add(MySqlDataTypeArray[i].Name);
  SetWindowSizeGrip(Handle, True);
  InheritFont(Font);
  FixVT(listParameters);
  SynMemoBody.Font.Name := Mainform.SynMemoQuery.Font.Name;
  SynMemoBody.Font.Size := Mainform.SynMemoQuery.Font.Size;
  SynMemoBody.Options := Mainform.SynMemoQuery.Options;
  Parameters := TWideStringList.Create;
end;


procedure TfrmRoutineEditor.FormDestroy(Sender: TObject);
begin
  // Store form dimensions
  OpenRegistry;
  MainReg.WriteInteger(REGNAME_PROCEDITOR_WIDTH, Width);
  MainReg.WriteInteger(REGNAME_PROCEDITOR_HEIGHT, Height);
end;


procedure TfrmRoutineEditor.FormShow(Sender: TObject);
var
  ds: TDataSet;
  Create: WideString;
  Params: TWideStringlist;
  Context: String;
  rx: TRegExpr;
  i: Integer;
begin
  editName.Text := AlterRoutineName;
  comboType.ItemIndex := 0;
  comboReturns.Text := '';
  listParameters.Clear;
  comboDataAccess.ItemIndex := 0;
  comboSecurity.ItemIndex := 0;
  editComment.Clear;
  SynMemoBody.Text := 'BEGIN'+CRLF+CRLF+'END';
  if editName.Text <> '' then begin
    // Editing existing routine
    ds := Mainform.GetResults('SELECT * FROM '+DBNAME_INFORMATION_SCHEMA+'.ROUTINES'+
      ' WHERE ROUTINE_SCHEMA='+esc(Mainform.ActiveDatabase)+' AND ROUTINE_NAME='+esc(AlterRoutineName));
    if ds.RecordCount <> 1 then begin
      MessageDlg('Cannot find properties of stored routine '+AlterRoutineName, mtError, [mbOK], 0);
      Close;
    end;
    ds.First;
    AlterRoutineType := ds.FieldByName('ROUTINE_TYPE').AsString;
    comboType.ItemIndex := ListIndexByRegExpr(comboType.Items, '^'+AlterRoutineType+'\b');
    chkDeterministic.Checked := ds.FieldByName('IS_DETERMINISTIC').AsString = 'YES';
    comboReturns.Text := ds.FieldByName('DTD_IDENTIFIER').AsWideString;
    comboDataAccess.ItemIndex := comboDataAccess.Items.IndexOf(ds.FieldByName('SQL_DATA_ACCESS').AsString);
    comboSecurity.ItemIndex := comboSecurity.Items.IndexOf(ds.FieldByName('SECURITY_TYPE').AsString);
    editComment.Text := ds.FieldByName('ROUTINE_COMMENT').AsWideString;
    SynMemoBody.Text := ds.FieldByName('ROUTINE_DEFINITION').AsWideString;
    Create := Mainform.GetVar('SHOW CREATE '+AlterRoutineType+' '+Mainform.mask(editName.Text), 2);
    rx := TRegExpr.Create;
    rx.ModifierI := True;
    rx.ModifierG := False;
    // CREATE DEFINER=`root`@`localhost` PROCEDURE `bla2`(IN p1 INT, p2 VARCHAR(20))
    rx.Expression := '^CREATE\s.+\s(PROCEDURE|FUNCTION)\s.+`\((.*)\)\s';
    if rx.Exec(Create) then begin
      Params := explode(',', rx.Match[2]);
      rx.Expression := '^((IN|OUT|INOUT)\s+)?(\S+)\s+(\S+)$';
      for i := 0 to Params.Count - 1 do begin
        if rx.Exec(Trim(Params[i])) then begin
          Context := rx.Match[2];
          if Context = '' then
            Context := 'IN';
          Parameters.Add(rx.Match[3] + DELIM + rx.Match[4] + DELIM + Context);
        end;
      end;
      FreeAndNil(Params);
    end;
    FreeAndNil(ds);
  end;
  editName.SetFocus;
  editNameChange(Sender);
  comboTypeSelect(comboType);
  btnRemoveParam.Enabled := Assigned(listParameters.FocusedNode);
  Modified := False;
end;


procedure TfrmRoutineEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Reset edited proc name for the next call
  AlterRoutineName := '';
  Parameters.Clear;
end;


procedure TfrmRoutineEditor.editNameChange(Sender: TObject);
begin
  editName.Font.Color := clWindowText;
  editName.Color := clWindow;
  try
    ensureValidIdentifier( editName.Text );
  except
    // Invalid name
    if editName.Text <> '' then begin
      editName.Font.Color := clRed;
      editName.Color := clYellow;
    end;
  end;
  Modified := True;
end;


procedure TfrmRoutineEditor.Modification(Sender: TObject);
begin
  Modified := True;
end;


procedure TfrmRoutineEditor.comboTypeSelect(Sender: TObject);
var
  isfunc: Boolean;
begin
  isfunc := (Sender as TTNTComboBox).ItemIndex = 1;
  lblReturns.Enabled := isfunc;
  comboReturns.Enabled := isfunc;
  Modified := True;
  listParameters.Repaint;
end;


procedure TfrmRoutineEditor.btnAddParamClick(Sender: TObject);
begin
  Parameters.Add('Param'+IntToStr(Parameters.Count+1)+DELIM+'INT'+DELIM+'IN');
  // See List.OnPaint:
  listParameters.Repaint;
  Modified := True;
end;


procedure TfrmRoutineEditor.btnRemoveParamClick(Sender: TObject);
begin
  Parameters.Delete(ListParameters.FocusedNode.Index);
  listParameters.Repaint;
  Modified := True;
end;


procedure TfrmRoutineEditor.btnClearParamsClick(Sender: TObject);
begin
  Parameters.Clear;
  listParameters.Repaint;
  Modified := True;
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
  var CellText: WideString);
var
  Values: TWideStringList;
begin
  if Column = 0 then
    CellText := IntToStr(Node.Index+1)
  else if (Column = 3) and (comboType.ItemIndex = 1) then
    CellText := 'IN' // A function can only have IN parameters
  else begin
    Values := explode(DELIM, Parameters[Node.Index]);
    CellText := Values[Column-1];
    FreeAndNil(Values);
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
begin
  btnRemoveParam.Enabled := Assigned(Node);
  if Assigned(Node) and (not ((comboType.ItemIndex = 1) and (Column=3))) then
    Sender.EditNode(Node, Column);
end;


procedure TfrmRoutineEditor.listParametersNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  OldValues: TWideStringList;
  new: WideString;
begin
  OldValues := explode(DELIM, Parameters[Node.Index]);
  case Column of
    1: new := NewText + DELIM + OldValues[1] + DELIM + OldValues[2];
    2: new := OldValues[0] + DELIM + NewText + DELIM + OldValues[2];
    3: new := OldValues[0] + DELIM + OldValues[1] + DELIM + NewText;
  end;
  Parameters[Node.Index] := new;
  Modified := True;
end;


procedure TfrmRoutineEditor.listParametersCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  EnumEditor: TEnumEditorLink;
  i: Integer;
begin
  if Column = 1 then
    EditLink := TStringEditLink.Create
  else if Column = 2 then begin
    EnumEditor := TEnumEditorLink.Create;
    EnumEditor.AllowCustomText := True;
    EnumEditor.ValueList := TWideStringList.Create;
    for i:=Low(MySqlDataTypeArray) to High(MySqlDataTypeArray) do
      EnumEditor.ValueList.Add(MySqlDataTypeArray[i].Name);
    EditLink := EnumEditor;
  end else if Column = 3 then begin
    EnumEditor := TEnumEditorLink.Create;
    EnumEditor.ValueList := TWideStringList.Create;
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


procedure TfrmRoutineEditor.PostChanges(Sender: TObject);
var
  BaseSQL, TempSQL, FinalSQL, TempName: WideString;
  i: Integer;
  par, allRoutineNames: TWideStringList;
  ProcOrFunc: String;
begin
  // Apply or OK button clicked
  ProcOrFunc := UpperCase(GetFirstWord(comboType.Text));
  if editName.Text = '' then begin
    MessageDlg('Please specify the routine''s name.', mtError, [mbOK], 0);
    editName.SetFocus;
    Exit;
  end else if (ProcOrFunc = 'FUNCTION') and (comboReturns.Text = '') then begin
    MessageDlg('Please specify the function''s returning datatype.', mtError, [mbOK], 0);
    comboReturns.SetFocus;
    Exit;
  end;

  BaseSQL := '';
  for i := 0 to Parameters.Count - 1 do begin
    par := explode(DELIM, Parameters[i]);
    if ProcOrFunc = 'PROCEDURE' then
      BaseSQL := BaseSQL + par[2] + ' ';
    BaseSQL := BaseSQL + par[0] + ' ' + par[1];
    if i < Parameters.Count-1 then
      BaseSQL := BaseSQL + ', ';
  end;
  BaseSQL := BaseSQL + ') ';
  if comboReturns.Enabled then
    BaseSQL := BaseSQL + 'RETURNS '+comboReturns.Text+' ';
  BaseSQL := BaseSQL + 'LANGUAGE SQL ';
  if not chkDeterministic.Checked then
    BaseSQL := BaseSQL + 'NOT ';
  BaseSQL := BaseSQL + 'DETERMINISTIC ';
  BaseSQL := BaseSQL + UpperCase(comboDataAccess.Text)+' ';
  BaseSQL := BaseSQL + 'SQL SECURITY ' + UpperCase(comboSecurity.Text)+' ';
  BaseSQL := BaseSQL + 'COMMENT ' + esc(editComment.Text)+' ';
  BaseSQL := BaseSQL + SynMemoBody.Text;

  try
    // There is no way to ALTER parameters or the name of it.
    // Create a temp routine, check for syntax errors, then drop the old routine and create it.
    // See also: http://dev.mysql.com/doc/refman/5.0/en/alter-procedure.html
    if AlterRoutineName <> '' then begin
      // Create temp name
      i := 0;
      allRoutineNames := Mainform.GetCol('SELECT ROUTINE_NAME FROM '+Mainform.mask(DBNAME_INFORMATION_SCHEMA)+'.'+Mainform.mask('ROUTINES')+' WHERE ROUTINE_SCHEMA = '+esc(Mainform.ActiveDatabase));
      while True do begin
        inc(i);
        TempName := APPNAME + '_temproutine_' + IntToStr(i);
        if allRoutineNames.IndexOf(TempName) = -1 then
          break;
      end;
      TempSQL := 'CREATE '+ProcOrFunc+' '+Mainform.mask(tempName)+'(' + BaseSQL;
      Mainform.ExecUpdateQuery(TempSQL, False, True);
      Mainform.ExecUpdateQuery('DROP '+AlterRoutineType+' IF EXISTS '+Mainform.mask(TempName));
      Mainform.ExecUpdateQuery('DROP '+AlterRoutineType+' IF EXISTS '+Mainform.mask(AlterRoutineName));
    end;
    FinalSQL := 'CREATE '+ProcOrFunc+' '+Mainform.mask(editName.Text)+'(' + BaseSQL;
    Mainform.ExecUpdateQuery(FinalSQL, False, True);
    // Set editing name if create/alter query was successful
    AlterRoutineName := editName.Text;
    AlterRoutineType := ProcOrFunc;
    Mainform.actRefresh.Execute;
    Modified := False;
  except
    On E:Exception do begin
      ModalResult := mrNone;
    end;
  end;
end;


procedure TfrmRoutineEditor.SetModified(Value: Boolean);
begin
  FModified := Value;
  btnOK.Enabled := FModified;
  btnApply.Enabled := FModified;
end;


procedure TfrmRoutineEditor.btnHelpClick(Sender: TObject);
begin
  // Help button
  Mainform.CallSQLHelpWithKeyword('CREATE PROCEDURE');
end;


end.
