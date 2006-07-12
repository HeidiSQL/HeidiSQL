{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EditU2.pas, released 2000-06-23.

The Original Code is the D4Demo project of the mwEdit component suite
by Martin Waldenburg and other developers.
The Original Author of the D4Demo project is Primoz Gabrijelcic.
Portions written by Primoz Gabrijelcic are copyright 1998 Primoz Gabrijelcic.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: EditU2.pas,v 1.6 2001/08/13 07:21:07 jjans Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
  - printing has been removed from D4Demo, since it is covered in greater
    detail in a dedicated example.
-------------------------------------------------------------------------------}

unit EditU2;

{$I SynEdit.inc}
{$J+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, {$IFDEF SYN_COMPILER_4_UP} ImgList, {$ENDIF}
  Menus, Buttons, Spin, SynEdit, SynHighlighterASM, SynHighlighterVB,
  SynHighlighterTclTk, SynHighlighterSML, SynHighlighterSQL,
  SynHighlighterProgress, SynHighlighterPHP, SynHighlighterPerl,
  SynHighlighterPas, SynHighlighterBat, SynHighlighterVBScript,
  SynHighlighterKix, SynHighlighterJScript, SynHighlighterJava,
  SynHighlighterInno, SynHighlighterIni, SynHighlighterHTML,
  SynHighlighterGeneral, SynHighlighterDml, SynHighlighterFoxpro,
  SynHighlighterFortran, SynHighlighterDfm, SynHighlighterCss,
  SynHighlighterCAC, SynHighlighterCache, SynHighlighterBaan,
  SynHighlighterADSP21xx, SynHighlighterHC11, SynEditHighlighter,
  SynHighlighterCpp, SynEditKeyCmds, SynExportRTF, SynEditExport,
  SynExportHTML, SynEditKeyCmdsEditor, SynCompletionProposal,
  SynEditPythonBehaviour, SynHighlighterPython, SynHighlighterHP48,
  SynHighlighterGalaxy, SynHighlighterAWK, SynHighlighterModelica,
  SynHighlighterM3, SynHighlighterGWS;

type
  TDemoMainForm = class(TForm)
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    tabBookmarks: TTabSheet;
    cbEnableKeys: TCheckBox;
    cbGlyphsVisible: TCheckBox;
    Label4: TLabel;
    inpLeftMargin: TSpinEdit;
    tabGutter: TTabSheet;
    Label5: TLabel;
    tabHighlighter: TTabSheet;
    Label3: TLabel;
    cbxHighlighterSelect: TComboBox;
    cbxSettingsSelect: TComboBox;
    Label1: TLabel;
    tabCaret: TTabSheet;
    cbxInsertCaret: TComboBox;
    Label7: TLabel;
    Label8: TLabel;
    cbxOverwriteCaret: TComboBox;
    cbInsertMode: TCheckBox;
    tabUndo: TTabSheet;
    btnUndo: TButton;
    tabFile: TTabSheet;
    btnLoad: TButton;
    tabInfo: TTabSheet;
    Label9: TLabel;
    Label10: TLabel;
    inpLineText: TEdit;
    Label11: TLabel;
    inpMaxUndo: TSpinEdit;
    cbReadonly: TCheckBox;
    tabDisplay: TTabSheet;
    outFilename: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    outLineCount: TEdit;
    tabEvents: TTabSheet;
    FontDialog1: TFontDialog;
    cbxGutterColor: TComboBox;
    inpLeftChar: TSpinEdit;
    inpTopLine: TSpinEdit;
    inpCaretX: TSpinEdit;
    inpCaretY: TSpinEdit;
    cbEnableEventLog: TCheckBox;
    lbEventLog: TListBox;
    Label19: TLabel;
    cbxAttrSelect: TComboBox;
    cbxAttrBackground: TComboBox;
    cbxAttrForeground: TComboBox;
    Label23: TLabel;
    Label24: TLabel;
    grbAttrStyle: TGroupBox;
    btnKeywords: TButton;
    grbAttrComments: TGroupBox;
    cbStyleStrikeOut: TCheckBox;
    cbStyleUnderline: TCheckBox;
    cbStyleItalic: TCheckBox;
    cbStyleBold: TCheckBox;
    btnSaveToReg: TButton;
    btnLoadFromReg: TButton;
    StatusBar: TStatusBar;
    TabSheet1: TTabSheet;
    KeyCmdList: TListView;
    btnEdit: TButton;
    ImageList1: TImageList;
    cbInternalImages: TCheckBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Label25: TLabel;
    inpXOffset: TSpinEdit;
    btnRedo: TButton;
    cbCommentsBas: TCheckBox;
    cbCommentsAsm: TCheckBox;
    cbCommentsPas: TCheckBox;
    cbCommentsAnsi: TCheckBox;
    cbCommentsC: TCheckBox;
    Label22: TLabel;
    cbLineNumbers: TCheckBox;
    cbLeadingZeros: TCheckBox;
    cbZeroStart: TCheckBox;
    cbMouse: TCheckBox;
    cbDrag: TCheckBox;
    cbKeyboard: TCheckBox;
    cbOther: TCheckBox;
    btnClear: TButton;
    tabExporter: TTabSheet;
    cbxExporterSelect: TComboBox;
    Label27: TLabel;
    tabEdit: TTabSheet;
    cbAutoIndent: TCheckBox;
    cbWantTabs: TCheckBox;
    Label6: TLabel;
    inpGutterWidth: TSpinEdit;
    Label29: TLabel;
    inpTabWidth: TSpinEdit;
    Label30: TLabel;
    inpDigitCount: TSpinEdit;
    Label31: TLabel;
    inpLeftOffset: TSpinEdit;
    inpRightOffset: TSpinEdit;
    Label32: TLabel;
    FindDialog1: TFindDialog;
    ReplaceDialog1: TReplaceDialog;
    tabSearch: TTabSheet;
    btnSearch: TButton;
    btnSearchNext: TButton;
    btnSearchPrev: TButton;
    lblSearchResult: TLabel;
    btnReplace: TButton;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label26: TLabel;
    Label28: TLabel;
    cbHideSelection: TCheckBox;
    inpRightEdge: TSpinEdit;
    cbScrollPastEOL: TCheckBox;
    cbxScrollBars: TComboBox;
    cbxColor: TComboBox;
    cbxForeground: TComboBox;
    cbxBackground: TComboBox;
    btnFont: TButton;
    inpExtraLineSpacing: TSpinEdit;
    cbxREColor: TComboBox;
    cbHalfPageScroll: TCheckBox;
    cbAutoSize: TCheckBox;
    cbDragDropEdit: TCheckBox;
    Label2: TMemo;
    cbGutterVisible: TCheckBox;
    cbUseFontStyle: TCheckBox;
    cbExportSelected: TCheckBox;
    btnExportToFile: TButton;
    SaveDialog1: TSaveDialog;
    btnExportToClipboard: TButton;
    SynEditor: TSynEdit;
    SynExporterHTML1: TSynExporterHTML;
    SynExporterRTF1: TSynExporterRTF;
    SynCompletionProposal1: TSynCompletionProposal;
    SynAutoComplete1: TSynAutoComplete;
    tabAbout: TTabSheet;
    Label33: TLabel;
    Memo1: TMemo;
    SynPasSyn1: TSynPasSyn;
    SynHC11Syn1: TSynHC11Syn;
    SynADSP21xxSyn1: TSynADSP21xxSyn;
    SynAWKSyn1: TSynAWKSyn;
    SynBaanSyn1: TSynBaanSyn;
    SynCppSyn1: TSynCppSyn;
    SynCacheSyn1: TSynCacheSyn;
    SynCACSyn1: TSynCACSyn;
    SynCssSyn1: TSynCssSyn;
    SynDfmSyn1: TSynDfmSyn;
    SynFortranSyn1: TSynFortranSyn;
    SynFoxproSyn1: TSynFoxproSyn;
    SynGalaxySyn1: TSynGalaxySyn;
    SynDmlSyn1: TSynDmlSyn;
    SynGeneralSyn1: TSynGeneralSyn;
    SynHP48Syn1: TSynHP48Syn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynIniSyn1: TSynIniSyn;
    SynInnoSyn1: TSynInnoSyn;
    SynJavaSyn1: TSynJavaSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynKixSyn1: TSynKixSyn;
    SynVBScriptSyn1: TSynVBScriptSyn;
    SynBatSyn1: TSynBatSyn;
    SynPerlSyn1: TSynPerlSyn;
    SynPHPSyn1: TSynPHPSyn;
    SynProgressSyn1: TSynProgressSyn;
    SynPythonSyn1: TSynPythonSyn;
    SynSQLSyn1: TSynSQLSyn;
    SynSMLSyn1: TSynSMLSyn;
    SynTclTkSyn1: TSynTclTkSyn;
    SynVBSyn1: TSynVBSyn;
    SynAsmSyn1: TSynAsmSyn;
    SynEditPythonBehaviour1: TSynEditPythonBehaviour;
    SynModelicaSyn1: TSynModelicaSyn;
    SynM3Syn1: TSynM3Syn;
    TabSheet2: TTabSheet;
    Memo2: TMemo;
    Memo3: TMemo;
    cbShrinkList: TCheckBox;
    Label34: TLabel;
    cbCompletionAttr: TComboBox;
    cbxCompletionColor: TComboBox;
    Label35: TLabel;
    SynGWScriptSyn1: TSynGWScriptSyn;
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbReadonlyClick(Sender: TObject);
    procedure cbHideSelectionClick(Sender: TObject);
    procedure cbScrollPastEOLClick(Sender: TObject);
    procedure cbHalfPageScrollClick(Sender: TObject);
    procedure inpExtraLineSpacingChange(Sender: TObject);
    procedure inpRightEdgeChange(Sender: TObject);
    procedure cbxREColorChange(Sender: TObject);
    procedure cbxScrollBarsChange(Sender: TObject);
    procedure cbxColorChange(Sender: TObject);
    procedure cbxForegroundChange(Sender: TObject);
    procedure cbxBackgroundChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure cbAutoIndentClick(Sender: TObject);
    procedure cbWantTabsClick(Sender: TObject);
    procedure inpTabWidthChange(Sender: TObject);
    procedure cbDragDropEditClick(Sender: TObject);
    procedure cbxInsertCaretChange(Sender: TObject);
    procedure cbxOverwriteCaretChange(Sender: TObject);
    procedure cbInsertModeClick(Sender: TObject);
    procedure cbxGutterColorChange(Sender: TObject);
    procedure inpGutterWidthChange(Sender: TObject);
    procedure inpDigitCountChange(Sender: TObject);
    procedure inpLeftOffsetChange(Sender: TObject);
    procedure inpRightOffsetChange(Sender: TObject);
    procedure cbLineNumbersClick(Sender: TObject);
    procedure cbLeadingZerosClick(Sender: TObject);
    procedure cbZeroStartClick(Sender: TObject);
    procedure cbAutoSizeClick(Sender: TObject);
    procedure cbGutterVisibleClick(Sender: TObject);
    procedure cbUseFontStyleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbEnableKeysClick(Sender: TObject);
    procedure cbGlyphsVisibleClick(Sender: TObject);
    procedure cbInternalImagesClick(Sender: TObject);
    procedure inpLeftMarginChange(Sender: TObject);
    procedure inpXOffsetChange(Sender: TObject);
    procedure btnUndoClick(Sender: TObject);
    procedure btnRedoClick(Sender: TObject);
    procedure inpMaxUndoChange(Sender: TObject);
    procedure SynEditorChange(Sender: TObject);
    procedure SynEditorClick(Sender: TObject);
    procedure SynEditorCommandProcessed(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    procedure SynEditorDblClick(Sender: TObject);
    procedure SynEditorDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SynEditorDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SynEditorDropFiles(Sender: TObject; X, Y: Integer;
      AFiles: TStrings);
    procedure SynEditorEnter(Sender: TObject);
    procedure SynEditorExit(Sender: TObject);
    procedure SynEditorGutterClick(Sender: TObject; X, Y, Line: Integer;
      mark: TSynEditMark);
    procedure SynEditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditorKeyPress(Sender: TObject; var Key: Char);
    procedure SynEditorKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SynEditorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEditorMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SynEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SynEditorPaint(Sender: TObject; ACanvas: TCanvas);
    procedure SynEditorEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure SynEditorPlaceBookmark(Sender: TObject;
      var Mark: TSynEditMark);
    procedure SynEditorProcessCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    procedure SynEditorProcessUserCommand(Sender: TObject;
      var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
    procedure SynEditorReplaceText(Sender: TObject; const ASearch,
      AReplace: String; Line, Column: Integer;
      var Action: TSynReplaceAction);
    procedure SynEditorStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure SynEditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
    procedure cbxExporterSelectChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure inpLeftCharChange(Sender: TObject);
    procedure inpTopLineChange(Sender: TObject);
    procedure inpCaretXChange(Sender: TObject);
    procedure inpCaretYChange(Sender: TObject);
    procedure cbEnableEventLogClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure cbxHighlighterSelectChange(Sender: TObject);
    procedure cbxAttrSelectChange(Sender: TObject);
    procedure cbxSettingsSelectChange(Sender: TObject);
    procedure cbxAttrForegroundChange(Sender: TObject);
    procedure cbxAttrBackgroundChange(Sender: TObject);
    procedure cbFontStyleClick(Sender: TObject);
    procedure btnSaveToRegClick(Sender: TObject);
    procedure btnLoadFromRegClick(Sender: TObject);
    procedure btnKeywordsClick(Sender: TObject);
    procedure cbCommentsClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure SpeedButtonClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnSearchNextPrevClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure DoFindText(Sender: TObject);
    procedure DoReplaceText(Sender: TObject);
    procedure btnExportToFileClick(Sender: TObject);
    procedure btnExportToClipboardClick(Sender: TObject);
    procedure SynEditorContextHelp(Sender: TObject; word: String);
    procedure cbShrinkListClick(Sender: TObject);
    procedure cbCompletionAttrChange(Sender: TObject);
    procedure cbxCompletionColorChange(Sender: TObject);
  private
    fDisableMarkButtons: boolean;
    fFileName: string;
    fHighlighters: TStringList;
    fMarkButtons: array [0..4] of TSpeedButton;
    fShown: boolean;
    procedure EnumerateExporters;
    function GetSelectedExporter: TSynCustomExporter;
    procedure LoadFile(const AFileName: string; AReadOnly: boolean);
    procedure LogEvent(AEvent: string);
    procedure RebuildMarks;
    procedure RecalcLeftMargin;
    procedure ReloadAttributes;
    procedure ResetMarkButtons;
    procedure ShowSuccess(ASuccess: boolean);
    procedure UpdateCanExport;
    procedure UpdateEditorSettings;
    procedure UpdateKeystrokesList;
  end;

var
  DemoMainForm: TDemoMainForm;

implementation

{$R *.DFM}

uses
  uHighlighterProcs, Unit2, Clipbrd;

const
  Colors: array[1..42 {sic!}] of TColor = (clBlack, clMaroon, clGreen, clOlive,
    clNavy, clPurple, clTeal, clDkGray, clLtGray, clRed, clLime,
    clYellow, clBlue, clFuchsia, clAqua, clWhite, clScrollBar,
    clBackground, clActiveCaption, clInactiveCaption, clMenu, clWindow,
    clWindowFrame, clMenuText, clWindowText, clCaptionText,
    clActiveBorder, clInactiveBorder, clAppWorkSpace, clHighlight,
    clHighlightText, clBtnFace, clBtnShadow, clGrayText, clBtnText,
    clInactiveCaptionText, clBtnHighlight, cl3DDkShadow, cl3DLight,
    clInfoText, clInfoBk, clNone);

function ColorToIndex(AColor: TColor): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Low(Colors) to High(Colors) do
    if Colors[i] = AColor then begin
      Result := i - 1;
      break;
    end;
end;

function IndexToColor(AIndex: integer): TColor;
begin
  Result := Colors[AIndex + 1];
end;

{ TDemoMainForm }

procedure TDemoMainForm.FormCreate(Sender: TObject);
var
  s: string;
  AColors: TStringList;
  i: integer;
begin
  fHighlighters := TStringList.Create;
  GetHighlighters(Self, fHighlighters, FALSE);
  EnumerateExporters;
  UpdateKeystrokesList;
  // setup highlighter selection combobox
  cbxHighlighterSelect.Items.Assign(fHighlighters);
  cbxHighlighterSelect.Items.Insert(0, '(none)');
  // setup file open dialog filter
  s := GetHighlightersFilter(fHighlighters);
  if (s <> '') and (s[Length(s)] <> '|') then
    s := s + '|';
  s := s + 'All files (*.*)|*.*';
  OpenDialog1.Filter := s;
  // fill comboboxes with color strings
  AColors := TStringList.Create;
  try
    for i := Low(Colors) to High(Colors) do begin
      if ColorToIdent(Colors[i], s) then
        AColors.Add(s);
    end;
    cbxREColor.Items.Assign(AColors);
    cbxColor.Items.Assign(AColors);
    cbxForeground.Items.Assign(AColors);
    cbxBackground.Items.Assign(AColors);
    cbxGutterColor.Items.Assign(AColors);
    cbxAttrForeground.Items.Assign(AColors);
    cbxAttrBackground.Items.Assign(AColors);
    cbxCompletionColor.Items.Assign(AColors);
  finally
    AColors.Free;
  end;
  // initialization of other controls
  PageControl1.ActivePage := tabFile;
  FontDialog1.Font.Assign(SynEditor.Font);

  fMarkButtons[0] := SpeedButton1;
  fMarkButtons[1] := SpeedButton2;
  fMarkButtons[2] := SpeedButton3;
  fMarkButtons[3] := SpeedButton4;
  fMarkButtons[4] := SpeedButton5;
end;

procedure TDemoMainForm.FormDestroy(Sender: TObject);
begin
  fHighlighters.Free;
end;

procedure TDemoMainForm.FormShow(Sender: TObject);
begin
  if not fShown then begin
    fShown := TRUE;
    UpdateEditorSettings;
    cbxHighlighterSelect.ItemIndex := 0;
    cbxHighlighterSelectChange(Sender);
    // update the statusbar panels
    SynEditorStatusChange(Self, [scAll]);
  end;
end;

procedure TDemoMainForm.FormResize(Sender: TObject);
begin
  inpGutterWidth.MaxValue := SynEditor.Width + 1;
  if inpGutterWidth.Value > inpGutterWidth.MaxValue then
    inpGutterWidth.Value := inpGutterWidth.MaxValue;
end;

procedure TDemoMainForm.EnumerateExporters;
var
  i: integer;
  s: string;
begin
  cbxExporterSelect.Items.Add('(All registered formats)');
  for i := 0 to ComponentCount - 1 do begin
    if not (Components[i] is TSynCustomExporter) then
      continue;
    s := (Components[i] as TSynCustomExporter).FormatName;
    if s <> '' then
      cbxExporterSelect.Items.Add(s);
  end;    
  cbxExporterSelect.ItemIndex := 0;
  cbxExporterSelectChange(Self);
end;

function TDemoMainForm.GetSelectedExporter: TSynCustomExporter;
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do begin
    if not (Components[i] is TSynCustomExporter) then
      continue;
    Result := TSynCustomExporter(Components[i]);
    if Result.FormatName = cbxExporterSelect.Text then
      exit;
  end;
  Result := nil;
end;

procedure TDemoMainForm.LoadFile(const AFileName: string; AReadOnly: boolean);
var
  backCursor: TCursor;
  bWasText: boolean;
begin
  backCursor := Cursor;
  try
    Cursor := crHourGlass;
    Windows.SetCursor(Screen.Cursors[crHourGlass]);

    fFileName := AFileName;
    outFilename.Caption := AFileName;
    outFilename.Visible := TRUE;
    with SynEditor do begin
      Highlighter := GetHighlighterFromFileExt(fHighlighters,
        ExtractFileExt(AFileName));
      with cbxHighlighterSelect do begin
        if Highlighter <> nil then
          ItemIndex := Items.IndexOf(Highlighter.LanguageName)
        else
          ItemIndex := 0;
      end;
      cbxHighlighterSelectChange(Self);
      if Highlighter = SynDfmSyn1 then
        LoadDFMFile2Strings(AFileName, SynEditor.Lines, bWasText)
      else
        Lines.LoadFromFile(AFileName);
      Modified := FALSE;
      ReadOnly := AReadOnly;
      cbReadonly.Checked := AReadOnly;
      SetFocus;
    end;
  finally
    Cursor := backCursor;
  end;
end;

procedure TDemoMainForm.LogEvent(AEvent: string);
begin
  if cbEnableEventLog.Checked then
    lbEventLog.Items.Insert(0, TimeToStr(Now) + ' ' + AEvent);
end;

procedure TDemoMainForm.RebuildMarks;
var
  i: integer;
begin
  with SynEditor do begin
    BeginUpdate;
    try
      for i := 0 to Marks.Count-1 do begin
        if Marks[i].IsBookmark then
          Marks[i].InternalImage := cbInternalImages.Checked;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TDemoMainForm.RecalcLeftMargin;

  procedure ValidateSpinEditValue(SE: TSpinEdit; Value: integer);
  begin
    if SE.Value <> Value then SE.Value := Value;
  end;

begin
  with SynEditor do begin
    inpLeftMargin.MaxValue := Gutter.Width;
    if inpLeftMargin.Value > inpLeftMargin.MaxValue then
      inpLeftMargin.Value := inpLeftMargin.MaxValue;
    ValidateSpinEditValue(inpGutterWidth, Gutter.Width);
    ValidateSpinEditValue(inpDigitCount, Gutter.DigitCount);
    ValidateSpinEditValue(inpLeftOffset, Gutter.LeftOffset);
    ValidateSpinEditValue(inpRightOffset, Gutter.RightOffset);
  end;
end;

procedure TDemoMainForm.ReloadAttributes;
var
  i: integer;
begin
  if SynEditor.Highlighter <> nil then begin
    cbxAttrSelect.Items.Clear;
    for i := 0 to SynEditor.Highlighter.AttrCount - 1 do
      cbxAttrSelect.Items.Add(SynEditor.Highlighter.Attribute[i].Name);
    cbxAttrSelect.ItemIndex := 0;
  end;
  cbxAttrSelectChange(Self);
end;

procedure TDemoMainForm.ResetMarkButtons;
var
  marks: TSynEditMarks;
  i: integer;
begin
  fDisableMarkButtons := true;
  try
    SynEditor.Marks.GetMarksForLine(SynEditor.CaretY, marks);
    for i := 0 to 4 do
      fMarkButtons[i].Down := false;
    for i := 1 to maxMarks do begin
      if not assigned(marks[i]) then break;
      if not marks[i].IsBookmark then
        fMarkButtons[marks[i].ImageIndex-10].Down := true;
    end;
  finally
    fDisableMarkButtons := false;
  end;
end;

procedure TDemoMainForm.ShowSuccess(ASuccess: boolean);
begin
  if ASuccess then
    StatusBar.Panels[3].Text := 'Success'
  else
    StatusBar.Panels[3].Text := 'Failure';
end;

procedure TDemoMainForm.UpdateCanExport;
begin
  btnExportToClipboard.Enabled := SynEditor.Highlighter <> nil;
  btnExportToFile.Enabled := (SynEditor.Highlighter <> nil)
    and (cbxExporterSelect.ItemIndex > 0);
end;

procedure TDemoMainForm.UpdateEditorSettings;
begin
  with SynEditor do begin
    cbReadonly.Checked := ReadOnly;
    cbHideSelection.Checked := HideSelection;
    cbScrollPastEOL.Checked := eoScrollPastEOL in Options;
    cbHalfPageScroll.Checked := eoHalfPageScroll in Options;
    inpExtraLineSpacing.Value := ExtraLineSpacing;
    inpRightEdge.Value := RightEdge;
    cbxREColor.ItemIndex := ColorToIndex(RightEdgeColor);
    cbxScrollBars.ItemIndex := Ord(Scrollbars);
    with SynEditor.Font do
      btnFont.Caption := Name + ' ' + IntToStr(Size);
    cbxColor.ItemIndex := ColorToIndex(Color);
    cbxForeground.ItemIndex := ColorToIndex(SelectedColor.Foreground);
    cbxBackground.ItemIndex := ColorToIndex(SelectedColor.Background);
    cbAutoIndent.Checked := eoAutoIndent in Options;
    cbWantTabs.Checked := WantTabs;
    inpTabWidth.Value := TabWidth;
    cbDragDropEdit.Checked := eoDragDropEditing in Options;
    cbxInsertCaret.ItemIndex := Ord(InsertCaret);
    cbxOverwriteCaret.ItemIndex := Ord(OverwriteCaret);
    cbInsertMode.Checked := InsertMode;
    cbxGutterColor.ItemIndex := ColorToIndex(Gutter.Color);
    inpGutterWidth.Value := Gutter.Width;
    inpDigitCount.Value := Gutter.DigitCount;
    inpLeftOffset.Value := Gutter.LeftOffset;
    inpRightOffset.Value := Gutter.RightOffset;
    cbLineNumbers.Checked := Gutter.ShowLineNumbers;
    cbLeadingZeros.Checked := Gutter.LeadingZeros;
    cbZeroStart.Checked := Gutter.ZeroStart;
    cbAutoSize.Checked := Gutter.AutoSize;
    cbGutterVisible.Checked := Gutter.Visible;
    cbUseFontStyle.Checked := Gutter.UseFontStyle;
    cbEnableKeys.Checked := BookMarkOptions.EnableKeys;
    cbGlyphsVisible.Checked := BookMarkOptions.GlyphsVisible;
    cbInternalImages.Checked := BookMarkOptions.BookmarkImages = nil;
    inpLeftMargin.Value := BookMarkOptions.LeftMargin;
    inpXOffset.Value := BookmarkOptions.XOffset;
    inpMaxUndo.Value := MaxUndo;
    cbxAttrSelect.ItemIndex := 0;
    cbxAttrSelectChange(Self);
  end;
end;

procedure TDemoMainForm.UpdateKeystrokesList;
var
  i: integer;
begin
  with KeyCmdList.Items do begin
    BeginUpdate;
    try
      Clear;
      for i := 0 to SynEditor.Keystrokes.Count - 1 do
        with Add do begin
          Caption := EditorCommandToCodeString(SynEditor.Keystrokes[i].Command);
          if SynEditor.Keystrokes[i].ShortCut = 0 then
            SubItems.Add('<none>')
          else
            SubItems.Add(Menus.ShortCutToText(SynEditor.Keystrokes[i].ShortCut));
        end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TDemoMainForm.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadFile(OpenDialog1.FileName, ofReadOnly in OpenDialog1.Options);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbReadonlyClick(Sender: TObject);
begin
  SynEditor.ReadOnly := cbReadonly.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbHideSelectionClick(Sender: TObject);
begin
  SynEditor.HideSelection := cbHideSelection.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbScrollPastEOLClick(Sender: TObject);
begin
  SynEditor.SetOptionFlag(eoScrollPastEol, cbScrollPastEOL.Checked);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbHalfPageScrollClick(Sender: TObject);
begin
  SynEditor.SetOptionFlag(eoHalfPageScroll, cbHalfPageScroll.Checked);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpExtraLineSpacingChange(Sender: TObject);
begin
  try SynEditor.ExtraLineSpacing := inpExtraLineSpacing.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpRightEdgeChange(Sender: TObject);
begin
  try SynEditor.RightEdge := inpRightEdge.Value;
  except end;
  RecalcLeftMargin;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxREColorChange(Sender: TObject);
begin
  SynEditor.RightEdgeColor := IndexToColor(cbxREColor.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxScrollBarsChange(Sender: TObject);
begin
  SynEditor.Scrollbars := TScrollStyle(cbxScrollBars.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxColorChange(Sender: TObject);
begin
  SynEditor.Color := IndexToColor(cbxColor.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxForegroundChange(Sender: TObject);
begin
  SynEditor.SelectedColor.Foreground := IndexToColor(cbxForeground.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxBackgroundChange(Sender: TObject);
begin
  SynEditor.SelectedColor.Background := IndexToColor(cbxBackground.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.btnFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then begin
    SynEditor.Font.Assign(FontDialog1.Font);
    UpdateEditorSettings;
    SynEditor.SetFocus;
  end;
end;

procedure TDemoMainForm.cbAutoIndentClick(Sender: TObject);
begin
  SynEditor.SetOptionFlag(eoAutoIndent, cbAutoIndent.Checked);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbWantTabsClick(Sender: TObject);
begin
  SynEditor.WantTabs := cbWantTabs.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpTabWidthChange(Sender: TObject);
begin
  try SynEditor.TabWidth := inpTabWidth.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbDragDropEditClick(Sender: TObject);
begin
  SynEditor.SetOptionFlag(eoDragDropEditing, cbDragDropEdit.Checked);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxInsertCaretChange(Sender: TObject);
begin
  SynEditor.InsertCaret := TSynEditCaretType(cbxInsertCaret.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxOverwriteCaretChange(Sender: TObject);
begin
  SynEditor.OverwriteCaret := TSynEditCaretType(cbxOverwriteCaret.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbInsertModeClick(Sender: TObject);
begin
  SynEditor.InsertMode := cbInsertMode.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxGutterColorChange(Sender: TObject);
begin
  SynEditor.Gutter.Color := IndexToColor(cbxGutterColor.ItemIndex);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpGutterWidthChange(Sender: TObject);
begin
  try SynEditor.Gutter.Width := inpGutterWidth.Value;
  except end;
  RecalcLeftMargin;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpDigitCountChange(Sender: TObject);
begin
  try SynEditor.Gutter.DigitCount := inpDigitCount.Value;
  except end;
  RecalcLeftMargin;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpLeftOffsetChange(Sender: TObject);
begin
  try SynEditor.Gutter.LeftOffset := inpLeftOffset.Value;
  except end;
  RecalcLeftMargin;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpRightOffsetChange(Sender: TObject);
begin
  try SynEditor.Gutter.RightOffset := inpRightOffset.Value;
  except end;
  RecalcLeftMargin;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbLineNumbersClick(Sender: TObject);
begin
  SynEditor.Gutter.ShowLineNumbers := cbLineNumbers.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbLeadingZerosClick(Sender: TObject);
begin
  SynEditor.Gutter.LeadingZeros := cbLeadingZeros.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbZeroStartClick(Sender: TObject);
begin
  SynEditor.Gutter.ZeroStart := cbZeroStart.Checked;            
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbAutoSizeClick(Sender: TObject);
begin
  SynEditor.Gutter.AutoSize := cbAutoSize.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbGutterVisibleClick(Sender: TObject);
begin
  SynEditor.Gutter.Visible := cbGutterVisible.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbUseFontStyleClick(Sender: TObject);
begin
  SynEditor.Gutter.UseFontStyle := cbUseFontStyle.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbEnableKeysClick(Sender: TObject);
begin
  SynEditor.BookMarkOptions.EnableKeys := cbEnableKeys.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbGlyphsVisibleClick(Sender: TObject);
begin
  SynEditor.BookMarkOptions.GlyphsVisible := cbGlyphsVisible.Checked;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbInternalImagesClick(Sender: TObject);
begin
  RebuildMarks;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpLeftMarginChange(Sender: TObject);
begin
  try SynEditor.BookMarkOptions.LeftMargin := inpLeftMargin.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpXOffsetChange(Sender: TObject);
begin
  try SynEditor.BookMarkOptions.XOffset := inpXOffset.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.btnUndoClick(Sender: TObject);
begin
  SynEditor.Undo;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.btnRedoClick(Sender: TObject);
begin
  SynEditor.Redo;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpMaxUndoChange(Sender: TObject);
begin
  try SynEditor.MaxUndo := inpMaxUndo.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.SynEditorChange(Sender: TObject);
begin
  btnUndo.Enabled := SynEditor.CanUndo;
  btnRedo.Enabled := SynEditor.CanRedo;
  outLineCount.Text := IntToStr(SynEditor.Lines.Count);
  if cbOther.Checked then
    LogEvent('OnChange');
end;

procedure TDemoMainForm.SynEditorClick(Sender: TObject);
begin
  if cbMouse.Checked then
    LogEvent('OnClick');
end;

procedure TDemoMainForm.SynEditorCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if cbKeyboard.Checked then
    LogEvent('OnCommandProcessed');
end;

procedure TDemoMainForm.SynEditorDblClick(Sender: TObject);
begin
  if cbMouse.Checked then
    LogEvent('OnDblClick');
end;

procedure TDemoMainForm.SynEditorDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if cbDrag.Checked then
    LogEvent('OnDragDrop');
end;

procedure TDemoMainForm.SynEditorDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if cbDrag.Checked then
    LogEvent('OnDragOver');
end;

procedure TDemoMainForm.SynEditorDropFiles(Sender: TObject; X, Y: Integer;
  AFiles: TStrings);
begin
  if cbOther.Checked then
    LogEvent('OnDropFiles');
  if AFiles.Count >= 1 then
    LoadFile(AFiles[0], FALSE);
end;

procedure TDemoMainForm.SynEditorEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  if cbDrag.Checked then
    LogEvent('OnEndDrag');
end;

procedure TDemoMainForm.SynEditorEnter(Sender: TObject);
begin
  if cbOther.Checked then
    LogEvent('OnEnter');
end;

procedure TDemoMainForm.SynEditorExit(Sender: TObject);
begin
  if cbOther.Checked then
    LogEvent('OnExit');
end;

procedure TDemoMainForm.SynEditorGutterClick(Sender: TObject; X, Y,
  Line: Integer; mark: TSynEditMark);
begin
  if cbOther.Checked then
    LogEvent('OnGutterClick');
  SynEditor.CaretY := Line;
  if not assigned(mark) then begin // place first mark
    SpeedButton1.Down := true;
    SpeedButton1.Click;
  end else
  if (not mark.IsBookmark) and (mark.ImageIndex >= SpeedButton1.Tag) then begin
    if mark.ImageIndex = SpeedButton5.Tag then begin // remove mark
      SpeedButton5.Down := false;
      SpeedButton5.Click;
    end else
      mark.ImageIndex := mark.ImageIndex + 1;
  end;
  ResetMarkButtons;
end;

procedure TDemoMainForm.SynEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if cbKeyboard.Checked then
    LogEvent('OnKeyDown');
end;

procedure TDemoMainForm.SynEditorKeyPress(Sender: TObject; var Key: Char);
begin
  if cbKeyboard.Checked then
    LogEvent('OnKeyPress');
end;

procedure TDemoMainForm.SynEditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if cbKeyboard.Checked then
    LogEvent('OnKeyUp');
end;

procedure TDemoMainForm.SynEditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cbMouse.Checked then
    LogEvent('OnMouseDown');
end;

procedure TDemoMainForm.SynEditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if cbMouse.Checked then
    LogEvent('OnMouseMove');
end;

procedure TDemoMainForm.SynEditorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if cbMouse.Checked then
    LogEvent('OnMouseUp');
end;

procedure TDemoMainForm.SynEditorPaint(Sender: TObject; ACanvas: TCanvas);
begin
  if cbOther.Checked then
    LogEvent('OnPaint');
end;

procedure TDemoMainForm.SynEditorPlaceBookmark(Sender: TObject;
  var Mark: TSynEditMark);
begin
  if cbOther.Checked then
    LogEvent('OnPlaceBookmark');
  if Mark.IsBookmark then
    Mark.InternalImage := cbInternalImages.Checked;
end;

procedure TDemoMainForm.SynEditorProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if cbKeyboard.Checked then
    LogEvent('OnProcessCommand');
end;

procedure TDemoMainForm.SynEditorProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: Char; Data: Pointer);
begin
  if cbKeyboard.Checked then
    LogEvent('OnProcessUserCommand');
end;

procedure TDemoMainForm.SynEditorReplaceText(Sender: TObject;
  const ASearch, AReplace: String; Line, Column: Integer;
  var Action: TSynReplaceAction);
begin
  if cbOther.Checked then
    LogEvent('OnReplaceText');
end;

procedure TDemoMainForm.SynEditorStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if cbDrag.Checked then
    LogEvent('OnStartDrag');
end;

procedure TDemoMainForm.SynEditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
const
  ModifiedStrs: array[boolean] of string = ('', 'Modified');
  InsertModeStrs: array[boolean] of string = ('Overwrite', 'Insert');
var
  p: TPoint;
  Token: string;
  Attri: TSynHighlighterAttributes;
begin
  // Note: scAll for new file loaded
  // caret position has changed
  if Changes * [scAll, scCaretX, scCaretY] <> [] then begin
    p := SynEditor.CaretXY;
    inpCaretX.Value := p.X;
    inpCaretY.Value := p.Y;
    inpLineText.Text := SynEditor.LineText;
    outLineCount.Text := IntToStr(SynEditor.Lines.Count);
    Statusbar.Panels[0].Text := Format('%6d:%3d', [p.Y, p.X]);
    ResetMarkButtons;
  end;
  // horz scroll position has changed
  if Changes * [scAll, scLeftChar] <> [] then
    inpLeftChar.Value := SynEditor.LeftChar;
  // vert scroll position has changed
  if Changes * [scAll, scTopLine] <> [] then
    inpTopLine.Value := SynEditor.TopLine;
  // InsertMode property has changed
  if Changes * [scAll, scInsertMode, scReadOnly] <> [] then begin
    if SynEditor.ReadOnly then
      Statusbar.Panels[2].Text := 'ReadOnly'
    else
      Statusbar.Panels[2].Text := InsertModeStrs[SynEditor.InsertMode];
  end;
  // Modified property has changed
  if Changes * [scAll, scModified] <> [] then
    Statusbar.Panels[1].Text := ModifiedStrs[SynEditor.Modified];
  // selection has changed  
  if Changes * [scAll, scSelection] <> [] then
    cbExportSelected.Enabled := SynEditor.SelAvail;
  // select highlighter attribute at caret
  if (SynEditor.Highlighter <> nil) and (Changes * [scAll, scCaretX, scCaretY] <> [])
  then begin
    if not SynEditor.GetHighlighterAttriAtRowCol(SynEditor.CaretXY, Token,
      Attri)
    then
      Attri := SynEditor.Highlighter.WhitespaceAttribute;
    if Assigned(Attri) then begin
      cbxAttrSelect.ItemIndex := cbxAttrSelect.Items.IndexOf(Attri.Name);
      cbxAttrSelectChange(Self);
    end;
  end;
end;

procedure TDemoMainForm.cbxExporterSelectChange(Sender: TObject);
begin
  UpdateCanExport;
end;

procedure TDemoMainForm.inpLeftCharChange(Sender: TObject);
begin
  try SynEditor.LeftChar := inpLeftChar.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpTopLineChange(Sender: TObject);
begin
  try SynEditor.TopLine := inpTopLine.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpCaretXChange(Sender: TObject);
begin
  try SynEditor.CaretX := inpCaretX.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.inpCaretYChange(Sender: TObject);
begin
  try SynEditor.CaretY := inpCaretY.Value;
  except end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbEnableEventLogClick(Sender: TObject);
begin
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.btnClearClick(Sender: TObject);
begin
  lbEventLog.Clear;
end;

procedure TDemoMainForm.cbxHighlighterSelectChange(Sender: TObject);
var
  i: integer;
  HasHighlighter: boolean;
begin
  SynEditor.Highlighter := nil;
  HasHighlighter := FALSE;
  for i := 0 to fHighlighters.Count - 1 do begin
    if cbxHighlighterSelect.Text = fHighlighters[i] then begin
      SynEditor.Highlighter := fHighlighters.Objects[i] as TSynCustomHighlighter;
      HasHighlighter := TRUE;
      break;
    end;
  end;
  cbxExporterSelect.Enabled := HasHighlighter;
  UpdateCanExport;
  StatusBar.Panels[3].Text := '';
  // highlighter user settings
  cbxSettingsSelect.Items.Clear;
  if HasHighlighter and (hcUserSettings in SynEditor.Highlighter.Capabilities) then
    SynEditor.Highlighter.EnumUserSettings(cbxSettingsSelect.Items);
  cbxSettingsSelect.Enabled := cbxSettingsSelect.Items.Count > 0;

  btnSaveToReg.Enabled := HasHighlighter
    and (hcRegistry in SynEditor.Highlighter.Capabilities);
  btnLoadFromReg.Enabled := btnSaveToReg.Enabled;

  cbxAttrSelect.Enabled := HasHighlighter;
  cbxAttrForeground.Enabled := HasHighlighter;
  cbxAttrBackground.Enabled := HasHighlighter;
  grbAttrStyle.Enabled := HasHighlighter;
  if SynEditor.Highlighter is TSynGeneralSyn then begin
    grbAttrComments.Enabled := TRUE;
    with SynEditor.Highlighter as TSynGeneralSyn do begin
      cbCommentsAnsi.Checked := csAnsiStyle in Comments;
      cbCommentsPas.Checked := csPasStyle in Comments;
      cbCommentsC.Checked := csCStyle in Comments;
      cbCommentsAsm.Checked := csAsmStyle in Comments;
      cbCommentsBas.Checked := csBasStyle in Comments;
    end;
  end else
    grbAttrComments.Enabled := FALSE;
  if SynEditor.Highlighter is TSynPythonSyn then
    SynEditPythonBehaviour1.Editor := SynEditor
  else
    SynEditPythonBehaviour1.Editor := nil;
  btnKeywords.Enabled := grbAttrComments.Enabled;
  ReloadAttributes;

  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxAttrSelectChange(Sender: TObject);
var
  Attr: TSynHighlighterAttributes;
begin
  Attr := TSynHighlighterAttributes.Create('');
  try
    if SynEditor.Highlighter <> nil then
      Attr.Assign(SynEditor.Highlighter.Attribute[cbxAttrSelect.ItemIndex]);
    cbxAttrForeground.ItemIndex := ColorToIndex(Attr.Foreground);
    cbxAttrBackground.ItemIndex := ColorToIndex(Attr.Background);
    cbStyleBold.Checked := (fsBold in Attr.Style);
    cbStyleItalic.Checked := (fsItalic in Attr.Style);
    cbStyleUnderLine.Checked := (fsUnderline in Attr.Style);
    cbStyleStrikeOut.Checked := (fsStrikeOut in Attr.Style);
  finally
    Attr.Free;
  end;
end;

procedure TDemoMainForm.cbxSettingsSelectChange(Sender: TObject);
begin
  ShowSuccess(SynEditor.Highlighter.UseUserSettings(cbxSettingsSelect.ItemIndex));
  ReloadAttributes;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.cbxAttrForegroundChange(Sender: TObject);
begin
  if SynEditor.Highlighter <> nil then begin
    SynEditor.Highlighter.Attribute[cbxAttrSelect.ItemIndex].Foreground :=
      IndexToColor(cbxAttrForeground.ItemIndex);
  end;
end;

procedure TDemoMainForm.cbxAttrBackgroundChange(Sender: TObject);
begin
  if SynEditor.Highlighter <> nil then begin
    SynEditor.Highlighter.Attribute[cbxAttrSelect.ItemIndex].Background :=
      IndexToColor(cbxAttrBackground.ItemIndex);
  end;
end;

procedure TDemoMainForm.cbFontStyleClick(Sender: TObject);
var
  Style: TFontStyles;
begin
  if SynEditor.Highlighter <> nil then begin
    Style := [];
    if cbStyleBold.Checked then
      Include(Style, fsBold);
    if cbStyleItalic.Checked then
      Include(Style, fsItalic);
    if cbStyleUnderLine.Checked then
      Include(Style, fsUnderline);
    if cbStyleStrikeOut.Checked then
      Include(Style, fsStrikeOut);
    SynEditor.Highlighter.Attribute[cbxAttrSelect.ItemIndex].Style := Style;
  end;
end;

const
  csRegKeyRoot = 'Software\SynEdit\Highlighters\';

procedure TDemoMainForm.btnSaveToRegClick(Sender: TObject);
begin
  ShowSuccess(SynEditor.Highlighter.SaveToRegistry(HKEY_CURRENT_USER,
    csRegKeyRoot + cbxHighlighterSelect.Text));
end;

procedure TDemoMainForm.btnLoadFromRegClick(Sender: TObject);
begin
  if SynEditor.Highlighter.LoadFromRegistry(HKEY_CURRENT_USER,
    csRegKeyRoot + cbxHighlighterSelect.Text)
  then begin
    ShowSuccess(TRUE);
    cbxAttrSelectChange(Self);
  end else
    ShowSuccess(FALSE);
end;

procedure TDemoMainForm.btnKeywordsClick(Sender: TObject);
var
  Highlighter: TSynGeneralSyn;
begin
  Highlighter := SynEditor.HighLighter as TSynGeneralSyn;

  Form2 := TForm2.Create(Self);
  try
    Form2.lbKeywords.Items.Assign(Highlighter.Keywords);
    if Form2.ShowModal = mrOk then
      Highlighter.Keywords := Form2.lbKeywords.Items;
  finally
    Form2.Free;
    Form2 := nil;
  end;
end;

procedure TDemoMainForm.cbCommentsClick(Sender: TObject);
var
  CmntSet: CommentStyles;
begin
  CmntSet := [];
  if cbCommentsAnsi.Checked then
    Include(CmntSet, csAnsiStyle);
  if cbCommentsPas.Checked then
    Include(CmntSet, csPasStyle);
  if cbCommentsC.Checked then
    Include(CmntSet, csCStyle);
  if cbCommentsAsm.Checked then
    Include(CmntSet, csAsmStyle);
  if cbCommentsBas.Checked then
    Include(CmntSet, csBasStyle);
  (SynEditor.Highlighter as TSynGeneralSyn).Comments := CmntSet;
end;

procedure TDemoMainForm.btnEditClick(Sender: TObject);
var
  Dlg: TSynEditKeystrokesEditorForm;
begin
  Dlg := TSynEditKeystrokesEditorForm.Create(Self);
  try
    Dlg.Caption := 'SynEdit Demo Keystroke Editor';
    Dlg.Keystrokes := SynEditor.Keystrokes;
    if Dlg.ShowModal = mrOk then begin
      SynEditor.Keystrokes := Dlg.Keystrokes;
      UpdateKeystrokesList;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TDemoMainForm.SpeedButtonClick(Sender: TObject);
var
  p: TPoint;
  Mark: TSynEditMark;
begin
  if not fDisableMarkButtons then with SynEditor do begin
    p := CaretXY;
    Marks.ClearLine(p.Y);
    if (Sender as TSpeedButton).Down then begin
      Mark := TSynEditMark.Create(SynEditor);
      with Mark do begin
        Line := p.Y;
        Column := p.X;
        ImageIndex := (Sender as TSpeedButton).Tag;
        Visible := TRUE;
        InternalImage := BookMarkOptions.BookMarkImages = nil;
      end;
      Marks.Place(Mark);
    end;
  end;
end;

procedure TDemoMainForm.btnSearchClick(Sender: TObject);
begin
  FindDialog1.Execute;
  btnSearchNext.Enabled := TRUE;
  btnSearchPrev.Enabled := TRUE;
end;

procedure TDemoMainForm.btnSearchNextPrevClick(Sender: TObject);
begin
  if (Sender = btnSearchNext) then
    FindDialog1.Options := FindDialog1.Options + [frDown]
  else if (Sender = btnSearchPrev) then
    FindDialog1.Options := FindDialog1.Options - [frDown];
  DoFindText(Sender);
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.DoFindText(Sender: TObject);
var
  rOptions: TSynSearchOptions;
  dlg: TFindDialog;
  sSearch: string;
begin
  if Sender = ReplaceDialog1 then
    dlg := ReplaceDialog1
  else
    dlg := FindDialog1;
  sSearch := dlg.FindText;
  if Length(sSearch) = 0 then begin
    Beep;
    lblSearchResult.Caption := 'Can''t search for empty text!';
    lblSearchResult.Visible := TRUE;
  end else begin
    rOptions := [];
    if not (frDown in dlg.Options) then
      Include(rOptions, ssoBackwards);
    if frMatchCase in dlg.Options then
      Include(rOptions, ssoMatchCase);
    if frWholeWord in dlg.Options then
      Include(rOptions, ssoWholeWord);
    if SynEditor.SearchReplace(sSearch, '', rOptions) = 0 then begin
      Beep;
      lblSearchResult.Caption := 'SearchText ''' + sSearch + ''' not found!';
      lblSearchResult.Visible := TRUE;
    end else
      lblSearchResult.Visible := FALSE;
  end;
end;

procedure TDemoMainForm.DoReplaceText(Sender: TObject);
var
  rOptions: TSynSearchOptions;
  sSearch: string;
begin
  sSearch := ReplaceDialog1.FindText;
  if Length(sSearch) = 0 then begin
    Beep;
    lblSearchResult.Caption := 'Can''t replace an empty text!';
    lblSearchResult.Visible := TRUE;
  end else begin
    rOptions := [ssoReplace];
    if frMatchCase in ReplaceDialog1.Options then
      Include(rOptions, ssoMatchCase);
    if frWholeWord in ReplaceDialog1.Options then
      Include(rOptions, ssoWholeWord);
    if frReplaceAll in ReplaceDialog1.Options then
      Include(rOptions, ssoReplaceAll);
    if SynEditor.SearchReplace(sSearch, ReplaceDialog1.ReplaceText, rOptions) = 0
    then begin
      Beep;
      lblSearchResult.Caption := 'SearchText ''' + sSearch +
        ''' could not be replaced!';
      lblSearchResult.Visible := TRUE;
    end else
      lblSearchResult.Visible := FALSE;
  end;
end;

procedure TDemoMainForm.btnReplaceClick(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;

procedure TDemoMainForm.btnExportToFileClick(Sender: TObject);
var
  Exporter: TSynCustomExporter;
begin
  Exporter := GetSelectedExporter;
  // can't export to file in several formats at the same time...
  if Assigned(Exporter) then
    with SaveDialog1 do begin
      Filter := Exporter.DefaultFilter;
      if Execute then with Exporter do begin
        ExportAsText := TRUE;
        Highlighter := SynEditor.Highlighter;
        if fFileName <> '' then
          Title := '"' + fFileName + '" exported as ' + FormatName;
        if cbExportSelected.Checked and SynEditor.SelAvail then
          ExportRange(SynEditor.Lines, SynEditor.BlockBegin, SynEditor.BlockEnd)
        else
          ExportAll(SynEditor.Lines);
        SaveToFile(FileName);
      end;
    end;
  SynEditor.SetFocus;
end;

procedure TDemoMainForm.btnExportToClipboardClick(Sender: TObject);
var
  Exporter: TSynCustomExporter;
  SelectedOnly: boolean;
  i: integer;

  procedure DoExport;
  begin
    with Exporter do begin
      ExportAsText := FALSE;
      Highlighter := SynEditor.Highlighter;
      if SelectedOnly then
        ExportRange(SynEditor.Lines, SynEditor.BlockBegin, SynEditor.BlockEnd)
      else
        ExportAll(SynEditor.Lines);
      CopyToClipboard;
      Highlighter := nil;
    end;
  end;

begin
  Clipboard.Open;
  try
    SelectedOnly := cbExportSelected.Checked and SynEditor.SelAvail;
    if SelectedOnly then
      Clipboard.AsText := SynEditor.SelText
    else
      Clipboard.AsText := SynEditor.Lines.Text;
    // is exporter selected?
    Exporter := GetSelectedExporter;
    if Exporter <> nil then
      DoExport
    else for i := 0 to ComponentCount - 1 do begin
      if not (Components[i] is TSynCustomExporter) then
        continue;
      Exporter := TSynCustomExporter(Components[i]);
      DoExport;
    end;
  finally
    Clipboard.Close;
  end;
end;

procedure TDemoMainForm.SynEditorContextHelp (Sender: TObject; word: String);
var
  hlpmsg : array[0..255] of char;
begin
  StrFmt (hlpmsg,'Need help for %s?',[word]);
  with Application do
    MessageBox (hlpmsg,PChar (Title),mb_ok or mb_iconquestion);
end;

procedure TDemoMainForm.cbShrinkListClick(Sender: TObject);
begin
  SynCompletionProposal1.ShrinkList := cbShrinkList.Checked;
end;

procedure TDemoMainForm.cbCompletionAttrChange(Sender: TObject);
var
  idx : Integer;
begin
  idx := -1;
  case cbCompletionAttr.ItemIndex of
    0 : idx := ColorToIndex(SynCompletionProposal1.ClBackground);
    1 : idx := ColorToIndex(SynCompletionProposal1.ClText);
    2 : idx := ColorToIndex(SynCompletionProposal1.ClSelect);
    3 : idx := ColorToIndex(SynCompletionProposal1.ClSelectedText);
  end;
  cbxCompletionColor.ItemIndex := idx;
end;

procedure TDemoMainForm.cbxCompletionColorChange(Sender: TObject);
begin
  case cbCompletionAttr.ItemIndex of
    0 : SynCompletionProposal1.ClBackground := IndexToColor(cbxCompletionColor.ItemIndex);
    1 : SynCompletionProposal1.ClText := IndexToColor(cbxCompletionColor.ItemIndex);
    2 : SynCompletionProposal1.ClSelect := IndexToColor(cbxCompletionColor.ItemIndex);
    3 : SynCompletionProposal1.ClSelectedText := IndexToColor(cbxCompletionColor.ItemIndex);
  end;
end;

end.

