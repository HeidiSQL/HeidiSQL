unit searchreplace;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, SynMemo, SynEditTypes, gnugettext, VirtualTrees, SynRegExpr,
  SynEditRegexSearch, SynEditMiscClasses, SynEditSearch, extra_controls;

type
  TfrmSearchReplace = class(TExtForm)
    btnCancel: TButton;
    btnReplaceAll: TButton;
    lblSearch: TLabel;
    chkReplace: TCheckBox;
    comboSearch: TComboBox;
    comboReplace: TComboBox;
    grpOptions: TGroupBox;
    chkCaseSensitive: TCheckBox;
    chkWholeWords: TCheckBox;
    chkRegularExpression: TCheckBox;
    chkPromptOnReplace: TCheckBox;
    grpDirection: TRadioGroup;
    grpOrigin: TRadioGroup;
    grpScope: TRadioGroup;
    btnOK: TButton;
    lblReplaceHint: TLabel;
    SynEditSearch1: TSynEditSearch;
    SynEditRegexSearch1: TSynEditRegexSearch;
    lblSearchIn: TLabel;
    comboSearchIn: TComboBox;
    procedure ValidateControls(Sender: TObject);
    procedure chkReplaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure comboSearchReplaceExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DoSearchReplace(Sender: TObject);
  private
    { Private declarations }
    procedure DoSearchReplaceText;
    procedure DoSearchReplaceData;
    function GetEditor: TSynMemo;
    function GetGrid: TVirtualStringTree;
  public
    { Public declarations }
    Options: TSynSearchOptions;
    property Editor: TSynMemo read GetEditor;
    property Grid: TVirtualStringTree read GetGrid;
  end;


implementation

{$R *.dfm}

uses apphelpers, main;


procedure TfrmSearchReplace.FormCreate(Sender: TObject);
begin
  HasSizeGrip := True;
  comboSearch.Items.Text := AppSettings.ReadString(asFindDialogSearchHistory);
  comboReplace.Items.Text := AppSettings.ReadString(asFindDialogReplaceHistory);
  comboSearch.Text := '';
  comboReplace.Text := '';
  if comboSearch.Items.Count > 0 then comboSearch.Text := comboSearch.Items[0];
  if comboReplace.Items.Count > 0 then comboReplace.Text := comboReplace.Items[0];

  chkRegularExpression.Hint := _('Search patterns:')+CRLF+
    ' ^ '+_('Start of line')+CRLF+
    ' $ '+_('End of line')+CRLF+
    ' \w '+_('Any word character')+CRLF+
    ' \d '+_('Digit')+' (0-9)'+CRLF+
    ' \s '+_('Whitespace')+CRLF+
    _('Replacement patterns:')+CRLF+
    ' $0 .. $n '+_('Callback parentheses')
    ;
end;


procedure TfrmSearchReplace.FormShow(Sender: TObject);
var
  SearchText, ObjName: String;
  _Editor: TSynMemo;
  _Grid: TVirtualStringTree;
  IsQueryTab: Boolean;
begin
  // Populate "Search in" pulldown with grid and editor
  _Editor := MainForm.ActiveSynMemo(False);
  _Grid := MainForm.ActiveGrid;
  comboSearchIn.Items.Clear;
  IsQueryTab := MainForm.IsQueryTab(MainForm.PageControlMain.ActivePageIndex, True);
  SearchText := '';

  if Assigned(_Editor) then begin
    ObjName := _('SQL editor');
    if IsQueryTab then
      ObjName := ObjName + ': ' + MainForm.ActiveQueryTab.TabSheet.Caption;
    comboSearchIn.Items.AddObject(ObjName, _Editor);
    if _Editor.Focused then
      comboSearchIn.ItemIndex := comboSearchIn.Items.Count-1;
    if _Editor.SelAvail then
      SearchText := _Editor.SelText
    else
      SearchText := _Editor.WordAtCursor;
  end;

  if Assigned(_Grid) then begin
    ObjName := _('Data grid');
    if IsQueryTab then
      ObjName := _('Result grid')+': '+MainForm.ActiveQueryTab.tabsetQuery.Tabs[MainForm.ActiveQueryTab.tabsetQuery.TabIndex];
    comboSearchIn.Items.AddObject(ObjName, _Grid);
    if _Grid.Focused then
      comboSearchIn.ItemIndex := comboSearchIn.Items.Count-1;
    if Assigned(_Grid.FocusedNode) then
      SearchText := _Grid.Text[_Grid.FocusedNode, _Grid.FocusedColumn];
  end;

  if (comboSearchIn.ItemIndex = -1) and (comboSearchIn.Items.Count > 0) then
    comboSearchIn.ItemIndex := 0;

  // Prefill search editor with selected text
  if SearchText <> '' then
    comboSearch.Text := SearchText;

  ValidateControls(Sender);
  comboSearch.SetFocus;
end;


procedure TfrmSearchReplace.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteString(asFindDialogSearchHistory, comboSearch.Items.Text);
  AppSettings.WriteString(asFindDialogReplaceHistory, comboReplace.Items.Text);
end;


function TfrmSearchReplace.GetEditor: TSynMemo;
begin
  // Return selected target object as editor
  Result := nil;
  if (comboSearchIn.ItemIndex > -1) and (comboSearchIn.Items.Objects[comboSearchIn.ItemIndex] is TSynMemo) then
    Result := comboSearchIn.Items.Objects[comboSearchIn.ItemIndex] as TSynMemo;
end;


function TfrmSearchReplace.GetGrid: TVirtualStringTree;
begin
  // Return selected target object as grid
  Result := nil;
  if (comboSearchIn.ItemIndex > -1) and (comboSearchIn.Items.Objects[comboSearchIn.ItemIndex] is TVirtualStringTree) then
    Result := comboSearchIn.Items.Objects[comboSearchIn.ItemIndex] as TVirtualStringTree;
end;


procedure TfrmSearchReplace.chkReplaceClick(Sender: TObject);
begin
  // Jump to replace editor
  ValidateControls(Sender);
  if comboReplace.Enabled then
    ActiveControl := comboReplace;
end;


procedure TfrmSearchReplace.ValidateControls(Sender: TObject);
begin
  // Enable or disable various controls
  comboReplace.Enabled := chkReplace.Checked;
  chkPromptOnReplace.Enabled := chkReplace.Checked;
  btnReplaceAll.Enabled := chkReplace.Checked;
  lblReplaceHint.Enabled := chkReplace.Checked;
  if chkReplace.Checked then
    btnOK.Caption := _('Replace')
  else
    btnOK.Caption := _('Find');
end;


procedure TfrmSearchReplace.comboSearchReplaceExit(Sender: TObject);
var
  Combo: TComboBox;
  i, idx: Integer;
begin
  // Store search or replace text history
  Combo := Sender as TComboBox;
  if Combo.Text = '' then
    Exit;
  idx := -1;
  for i:=0 to Combo.Items.Count-1 do begin
    if Combo.Items[i] = Combo.Text then begin
      idx := i;
      break;
    end;
  end;
  if idx > -1 then
    Combo.Items.Move(idx, 0)
  else
    Combo.Items.Insert(0, Combo.Text);
  Combo.Text := Combo.Items[0];
end;


procedure TfrmSearchReplace.DoSearchReplace(Sender: TObject);
begin
  // Set SynEditSearch options once when user hits the dialog button,
  // not when "Find again" action was used
  if (Sender = btnOK) or (Sender = btnReplaceAll) then begin
    Options := [];
    if chkReplace.Checked then Include(Options, ssoReplace);
    if chkCaseSensitive.Checked then Include(Options, ssoMatchCase);
    if chkWholeWords.Checked then Include(Options, ssoWholeWord);
    if chkPromptOnReplace.Checked and chkPromptOnReplace.Enabled then Include(Options, ssoPrompt);
    if grpDirection.ItemIndex = 1 then Include(Options, ssoBackwards);
    if grpOrigin.ItemIndex = 1 then Include(Options, ssoEntireScope);
    if grpScope.ItemIndex = 1 then Include(Options, ssoSelectedOnly);
    if ModalResult = mrAll then Include(Options, ssoReplaceAll);

    // Work around multi line bug in SynEdit
    if (ssoReplaceAll in Options) and (Pos('\n', comboReplace.Text) > 0) then
      Include(Options, ssoBackwards);
  end;

  if Editor <> nil then
    DoSearchReplaceText
  else
    DoSearchReplaceData;
end;


procedure TfrmSearchReplace.DoSearchReplaceText;
var
  Occurences: Integer;
  OldCaretXY: TBufferCoord;
  Replacement: String;
begin
  if chkRegularExpression.Checked then
    Editor.SearchEngine := SynEditRegexSearch1
  else
    Editor.SearchEngine := SynEditSearch1;

  OldCaretXY := Editor.CaretXY;
  Replacement := comboReplace.Text;
  Replacement := StringReplace(Replacement, '\n', CRLF, [rfReplaceAll]);
  Replacement := StringReplace(Replacement, '\t', #9, [rfReplaceAll]);

  Editor.BeginUpdate;

  MainForm.ShowStatusMsg(_('Searching ...'));
  Occurences := Editor.SearchReplace(
    comboSearch.Text,
    Replacement,
    Options
    );

  Editor.EndUpdate;
  MainForm.ShowStatusMsg;

  if ssoReplaceAll in Options then
    MessageDialog(f_('Text "%s" replaced %s times.', [comboSearch.Text, FormatNumber(Occurences)]), mtInformation, [mbOk])
  else begin
    if (OldCaretXY.Char = Editor.CaretXY.Char) and
      (OldCaretXY.Line = Editor.CaretXY.Line) then
      MessageDialog(f_('Text "%s" not found.', [comboSearch.Text]), mtInformation, [mbOk]);
  end;
end;


procedure TfrmSearchReplace.DoSearchReplaceData;
var
  Search, Replacement, CellText: String;
  Node: PVirtualNode;
  Column, StartAtCol: TColumnIndex;
  Match, SelectedOnly, Backwards, UseRegEx: Boolean;
  MatchCount, ReplaceCount: Integer;
  rx: TRegExpr;
  Prompt: TModalResult;
  ReplaceFlags: TReplaceFlags;
begin
  // Data grid version of DoSearchReplaceText
  MainForm.ShowStatusMsg(_('Searching ...'));

  Search := comboSearch.Text;
  Replacement := comboReplace.Text;
  Replacement := StringReplace(Replacement, '\n', CRLF, [rfReplaceAll]);
  Replacement := StringReplace(Replacement, '\t', #9, [rfReplaceAll]);
  SelectedOnly := ssoSelectedOnly in Options;
  Backwards := ssoBackwards in Options;
  if ssoReplaceAll in Options then
    Prompt := mrYesToAll
  else
    Prompt := mrYes;
  Match := False;
  MatchCount := 0;
  ReplaceCount := 0;
  ReplaceFlags := [rfReplaceAll];
  if not (ssoMatchCase in Options) then
    Include(ReplaceFlags, rfIgnoreCase);

  // Init regular expression
  rx := TRegExpr.Create;
  UseRegEx := (ssoWholeWord in Options) or chkRegularExpression.Checked;
  if chkRegularExpression.Checked then
    rx.Expression := Search
  else // Still used for "whole word" search
    rx.Expression := '\b'+QuoteRegExprMetaChars(Search)+'\b';
  rx.ModifierI := not (ssoMatchCase in Options);

  // Set start row and column with regard to "Entire scope" and "Forward/Backward" mode
  if (ssoEntireScope in Options) or (not Assigned(Grid.FocusedNode)) then begin
    if Backwards then
      Node := GetPreviousNode(Grid, nil, SelectedOnly)
    else
      Node := GetNextNode(Grid, nil, SelectedOnly);
    StartAtCol := InvalidColumn;
  end else begin
    Node := Grid.FocusedNode;
    if Backwards then
      StartAtCol := Grid.Header.Columns.GetPreviousVisibleColumn(Grid.FocusedColumn)
    else
      StartAtCol := Grid.Header.Columns.GetNextVisibleColumn(Grid.FocusedColumn);
    // Advance to next row if focused column is the very last column
    if StartAtCol = InvalidColumn then begin
      if Backwards then
        Node := GetPreviousNode(Grid, Node, SelectedOnly)
      else
        Node := GetNextNode(Grid, Node, SelectedOnly);
    end;
  end;

  while Assigned(Node) do begin
    MainForm.AnyGridEnsureFullRow(Grid, Node);

    // Find the first column
    if StartAtCol > InvalidColumn then begin
      Column := StartAtCol;
      StartAtCol := InvalidColumn;
    end else begin
      if Backwards then
        Column := Grid.Header.Columns.GetLastVisibleColumn
      else
        Column := Grid.Header.Columns.GetFirstVisibleColumn;
    end;

    while Column >= 0 do begin
      CellText := Grid.Text[Node, Column];

      if UseRegEx then begin
        Match := rx.Exec(CellText);
      end else begin
        if ssoMatchCase in Options then
          Match := Pos(Search, CellText) > 0
        else
          Match := Pos(LowerCase(Search), LowerCase(CellText)) > 0;
      end;

      if Match then begin
        Inc(MatchCount);

        // Set focus on node and column
        SelectNode(Grid, Node);
        Grid.FocusedColumn := Column;

        // Replace logic
        if ssoReplace in Options then begin
          if ssoPrompt in Options then begin
            // Ask user
            Prompt := MessageDialog(f_('Replace this occurrence of "%s"?', [Search]),
              sstr(CellText, 500),
              mtConfirmation,
              [mbYes, mbYesToAll, mbNo, mbCancel]);
            case Prompt of
              mrYesToAll: begin
                Exclude(Options, ssoPrompt);
                Include(Options, ssoReplaceAll);
              end;
              mrCancel: Exclude(Options, ssoReplaceAll);
            end;
          end;
          if Prompt in [mrYes, mrYesToAll] then begin
            if UseRegEx then
              Grid.Text[Node, Column] := rx.Replace(CellText, Replacement, chkRegularExpression.Checked)
            else
              Grid.Text[Node, Column] := StringReplace(CellText, Search, Replacement, ReplaceFlags);
            Inc(ReplaceCount);
          end;
        end;

        if not (ssoReplaceAll in Options) then
          Break;
      end;

      // Advance to next column
      if Backwards then
        Column := Grid.Header.Columns.GetPreviousVisibleColumn(Column)
      else
        Column := Grid.Header.Columns.GetNextVisibleColumn(Column);
    end;

    if Match and (not (ssoReplaceAll in Options)) then
      Break;

    if Backwards then
      Node := GetPreviousNode(Grid, Node, SelectedOnly)
    else
      Node := GetNextNode(Grid, Node, SelectedOnly);
  end;

  if ssoReplaceAll in Options then begin
    if MatchCount > 0 then
      MessageDialog(f_('Text "%s" %d times replaced.', [Search, ReplaceCount]), mtInformation, [mbOk])
    else
      MessageDialog(f_('Text "%s" not found.', [Search]), mtInformation, [mbOk]);
  end else if MatchCount = 0 then
    MessageDialog(f_('Text "%s" not found.', [Search]), mtInformation, [mbOk]);

  MainForm.ShowStatusMsg;
end;


end.
