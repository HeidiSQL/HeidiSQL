unit searchreplace;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, SynMemo, SynEditTypes, gnugettext;

type
  TfrmSearchReplace = class(TForm)
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
    procedure ValidateControls(Sender: TObject);
    procedure chkReplaceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure comboSearchReplaceExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Editor: TSynMemo;
    Options: TSynSearchOptions;
  end;


implementation

{$R *.dfm}

uses helpers;


procedure TfrmSearchReplace.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
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
  SearchText: String;
begin
  // Prefill search editor with selected text
  if Editor.SelAvail then
    SearchText := Editor.SelText
  else
    SearchText := Editor.WordAtCursor;
  if SearchText <> '' then
    comboSearch.Text := SearchText;
  ValidateControls(Sender);
  comboSearch.SetFocus;
end;


procedure TfrmSearchReplace.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Set SynEditSearch options, which the caller can use in Editor.SearchReplace()
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


procedure TfrmSearchReplace.FormDestroy(Sender: TObject);
begin
  AppSettings.WriteString(asFindDialogSearchHistory, comboSearch.Items.Text);
  AppSettings.WriteString(asFindDialogReplaceHistory, comboReplace.Items.Text);
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
    btnOK.Caption := 'Replace'
  else
    btnOK.Caption := 'Find';
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


end.
