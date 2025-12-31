unit customize_highlighter;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, EditBtn,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  StrUtils, SynEditHighlighter, apphelpers, extra_controls;

type
  TfrmCustomizeHighlighter = class(TExtForm)
    comboHighlighter: TComboBox;
    listboxAttributes: TListBox;
    lblBackground: TLabel;
    lblForeground: TLabel;
    lblStyle: TLabel;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    btnCancel: TButton;
    btnOK: TButton;
    editBackground: TEditButton;
    editForeground: TEditButton;
    btnApply: TButton;
    procedure listboxAttributesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure comboHighlighterSelect(Sender: TObject);
    procedure SaveSettings(Sender: TObject);
    procedure editColorRightButtonClick(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
    FHighlighter: TSynCustomHighlighter;
    FAttr: TSynHighlighterAttributes;
    FOnChange: TNotifyEvent;
    procedure SetFriendlyLanguageName(FriendlyLanguageName: String);
    function GetFriendlyLanguageName: String;
  public
    { Public-Deklarationen }
    property FriendlyLanguageName: String read GetFriendlyLanguageName write SetFriendlyLanguageName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

uses main;

{$R *.lfm}

procedure TfrmCustomizeHighlighter.SaveSettings(Sender: TObject);
begin
  // Save highlighter settings
  FHighlighter.SaveToFile(AppSettings.DirnameHighlighters + FHighlighter.LanguageName + '.ini');
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TfrmCustomizeHighlighter.Modified(Sender: TObject);
begin
  // Apply modification to current attribute
  // Silence exception caused by invalid color strings
  FAttr.Background := StringToColorDef(editBackground.Text, clNone);
  FAttr.Foreground := StringToColorDef(editForeground.Text, clNone);
  if chkBold.Checked then
    FAttr.Style := FAttr.Style + [fsBold]
  else
    FAttr.Style := FAttr.Style - [fsBold];
  if chkItalic.Checked then
    FAttr.Style := FAttr.Style + [fsItalic]
  else
    FAttr.Style := FAttr.Style - [fsItalic];
end;

procedure TfrmCustomizeHighlighter.comboHighlighterSelect(Sender: TObject);
var
  i: Integer;
  Highlighters: TSynHighlighterList;
begin
  // Highlighter selected
  listboxAttributes.Clear;
  if Assigned(FHighlighter) then
    FHighlighter.Free;
  Highlighters := SynEditHighlighter.GetPlaceableHighlighters;
  for i:=0 to Highlighters.Count-1 do begin
    if Highlighters[i].GetLanguageName = comboHighlighter.Text then begin
      FHighlighter := Highlighters[i].Create(Self);
      Break;
    end;
  end;
  FHighlighter.LoadFromFile(AppSettings.DirnameHighlighters + FHighlighter.GetLanguageName + '.ini');
  for i:=0 to FHighlighter.AttrCount-1 do begin
    listboxAttributes.Items.Add(FHighlighter.Attribute[i].Name);
  end;
end;

procedure TfrmCustomizeHighlighter.editColorRightButtonClick(
  Sender: TObject);
var
  Dialog: TColorDialog;
  Edit: TEditButton;
begin
  // Color picker
  Edit := Sender as TEditButton;
  Dialog := TColorDialog.Create(Self);
  //Dialog.Options := [cdFullOpen, cdAnyColor];
  Dialog.Color := StringToColorDef(Edit.Text, clNone);
  if Dialog.Execute then begin
    Edit.Text := ColorToString(Dialog.Color);
  end;
  Dialog.Free;
  Modified(Sender);
end;

procedure TfrmCustomizeHighlighter.FormCreate(Sender: TObject);
var
  Highlighters: TSynHighlighterList;
  i: Integer;
begin
  // Form created
  FHighlighter := nil;
  FAttr := nil;
  FOnChange := nil;
  Highlighters := SynEditHighlighter.GetPlaceableHighlighters;
  for i:=0 to Highlighters.Count-1 do begin
    comboHighlighter.Items.Add(Highlighters[i].GetLanguageName);
  end;
end;

procedure TfrmCustomizeHighlighter.FormDestroy(Sender: TObject);
begin
  // Form destroyed
  if Assigned(FHighlighter) then
    FHighlighter.Free;
  // causes an exception when closing:
  //if Assigned(FAttr) then
  //  FAttr.Free;
end;

procedure TfrmCustomizeHighlighter.FormShow(Sender: TObject);
begin
  // Ensure controls are disabled as long as no attribute is selected
  listboxAttributes.OnClick(Sender);
end;

procedure TfrmCustomizeHighlighter.listboxAttributesClick(Sender: TObject);
var
  i: Integer;
  AttrSelected: Boolean;
begin
  // Attribute selected
  FAttr := nil;
  if listboxAttributes.ItemIndex > -1 then begin
    for i:=0 to FHighlighter.AttrCount-1 do begin
      if listboxAttributes.Items[listboxAttributes.ItemIndex] = FHighlighter.Attribute[i].Name then begin
        FAttr := FHighlighter.Attribute[i];
      end;
    end;
  end;
  // Enable/disable controls
  AttrSelected := FAttr <> nil;
  editBackground.Enabled := AttrSelected;
  editForeground.Enabled := AttrSelected;
  chkBold.Enabled := AttrSelected;
  chkItalic.Enabled := AttrSelected;
  // Overtake values
  if AttrSelected then begin
    editBackground.Text := IfThen(FAttr.Background <> clNone, ColorToString(FAttr.Background), '');
    editForeground.Text := IfThen(FAttr.Foreground <> clNone, ColorToString(FAttr.Foreground), '');
    chkBold.Checked := fsBold in FAttr.Style;
    chkItalic.Checked := fsItalic in FAttr.Style;
  end
  else begin
    editBackground.Text := '';
    editForeground.Text := '';
    chkBold.Checked := False;
    chkItalic.Checked := False;
  end;
end;

procedure TfrmCustomizeHighlighter.SetFriendlyLanguageName(FriendlyLanguageName: String);
begin
  // Set current highlighter by its language name
  comboHighlighter.ItemIndex := comboHighlighter.Items.IndexOf(FriendlyLanguageName);
  comboHighlighter.OnSelect(comboHighlighter);
end;

function TfrmCustomizeHighlighter.GetFriendlyLanguageName: String;
begin
  Result := FHighlighter.GetLanguageName;
end;


end.
