unit customize_highlighter;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.GraphUtil, System.Math,
  System.StrUtils, SynEditHighlighter, gnugettext, apphelpers;

type
  TfrmCustomizeHighlighter = class(TForm)
    comboHighlighter: TComboBox;
    listboxAttributes: TListBox;
    lblBackground: TLabel;
    lblForeground: TLabel;
    lblStyle: TLabel;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    btnCancel: TButton;
    btnOK: TButton;
    editBackground: TButtonedEdit;
    editForeground: TButtonedEdit;
    pnlSample: TPanel;
    btnApply: TButton;
    lblSample: TLabel;
    procedure listboxAttributesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure comboHighlighterSelect(Sender: TObject);
    procedure SaveSettings(Sender: TObject);
    procedure editColorRightButtonClick(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    FHighlighter: TSynCustomHighlighter;
    FAttr: TSynHighlighterAttributes;
    FOnChange: TNotifyEvent;
    procedure SetFriendlyLanguageName(FriendlyLanguageName: String);
    function GetFriendlyLanguageName: String;
    procedure UpdateSampleBox;
  public
    { Public-Deklarationen }
    property FriendlyLanguageName: String read GetFriendlyLanguageName write SetFriendlyLanguageName;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

uses main;

{$R *.dfm}

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
  try
    FAttr.Background := WebColorStrToColor(editBackground.Text);
    FAttr.Foreground := WebColorStrToColor(editForeground.Text);
  except
  end;
  if chkBold.Checked then
    FAttr.Style := FAttr.Style + [fsBold]
  else
    FAttr.Style := FAttr.Style - [fsBold];
  if chkItalic.Checked then
    FAttr.Style := FAttr.Style + [fsItalic]
  else
    FAttr.Style := FAttr.Style - [fsItalic];
  UpdateSampleBox;
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
    if Highlighters[i].GetFriendlyLanguageName = comboHighlighter.Text then begin
      FHighlighter := Highlighters[i].Create(Self);
      Break;
    end;
  end;
  FHighlighter.LoadFromFile(AppSettings.DirnameHighlighters + FHighlighter.GetLanguageName + '.ini');
  for i:=0 to FHighlighter.AttrCount-1 do begin
    listboxAttributes.Items.Add(FHighlighter.Attribute[i].FriendlyName);
  end;
end;

procedure TfrmCustomizeHighlighter.editColorRightButtonClick(
  Sender: TObject);
var
  Dialog: TColorDialog;
  Edit: TButtonedEdit;
begin
  // Color picker
  Edit := Sender as TButtonedEdit;
  Dialog := TColorDialog.Create(Self);
  Dialog.Options := [cdFullOpen, cdAnyColor];
  try
    Dialog.Color := WebColorStrToColor(Edit.Text);
  except
  end;
  if Dialog.Execute then begin
    Edit.Text := ColorToWebColorStr(Dialog.Color);
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
    comboHighlighter.Items.Add(Highlighters[i].GetFriendlyLanguageName);
  end;
end;

procedure TfrmCustomizeHighlighter.FormDestroy(Sender: TObject);
begin
  // Form destroyed
  if Assigned(FHighlighter) then
    FHighlighter.Free;
  if Assigned(FAttr) then
    FAttr.Free;
end;

procedure TfrmCustomizeHighlighter.listboxAttributesClick(Sender: TObject);
var
  i: Integer;
  AttrSelected: Boolean;
begin
  // Attribute selected
  FAttr := nil;
  for i:=0 to FHighlighter.AttrCount-1 do begin
    if listboxAttributes.Items[listboxAttributes.ItemIndex] = FHighlighter.Attribute[i].FriendlyName then begin
      FAttr := FHighlighter.Attribute[i];
    end;
  end;
  // Enable/disable controls
  AttrSelected := FAttr <> nil;
  editBackground.Enabled := AttrSelected;
  editForeground.Enabled := AttrSelected;
  chkBold.Enabled := AttrSelected;
  chkItalic.Enabled := AttrSelected;
  pnlSample.Enabled := AttrSelected;
  // Overtake values
  editBackground.Text := IfThen(AttrSelected, ColorToWebColorStr(FAttr.Background), '');
  editForeground.Text := IfThen(AttrSelected, ColorToWebColorStr(FAttr.Foreground), '');
  chkBold.Checked := AttrSelected and (fsBold in FAttr.Style);
  chkItalic.Checked := AttrSelected and (fsItalic in FAttr.Style);
  UpdateSampleBox;
end;

procedure TfrmCustomizeHighlighter.SetFriendlyLanguageName(FriendlyLanguageName: String);
begin
  // Set current highlighter by its language name
  comboHighlighter.ItemIndex := comboHighlighter.Items.IndexOf(FriendlyLanguageName);
  comboHighlighter.OnSelect(comboHighlighter);
end;

function TfrmCustomizeHighlighter.GetFriendlyLanguageName: String;
begin
  Result := FHighlighter.FriendlyLanguageName;
end;

procedure TfrmCustomizeHighlighter.UpdateSampleBox;
var
  AttrSelected: Boolean;
begin
  // Visualize current colors
  AttrSelected := FAttr <> nil;
  pnlSample.Color := TColor(IfThen(AttrSelected, FAttr.Background, clBtnFace));
  pnlSample.Font.Color := TColor(IfThen(AttrSelected, FAttr.Foreground, clWindowText));
  if AttrSelected then
    pnlSample.Font.Style := FAttr.Style;

  lblSample.Color := TColor(IfThen(AttrSelected, FAttr.Background, clBtnFace));
  lblSample.Font.Color := TColor(IfThen(AttrSelected, FAttr.Foreground, clWindowText));
  if AttrSelected then
    lblSample.Font.Style := FAttr.Style;
end;

end.
