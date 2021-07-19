unit preferences;


// -------------------------------------
// Preferences
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, SynEditHighlighter, SynHighlighterSQL,
  SynEdit, SynMemo, VirtualTrees, SynEditKeyCmds, ActnList, StdActns, Menus,
  dbstructures, gnugettext, Vcl.Themes, Vcl.Styles, SynRegExpr, Generics.Collections,
  Vcl.ImageCollection, extra_controls, theme_preview, Vcl.Buttons, System.Actions;

type
  TShortcutItemData = record
    Action: TAction;
    KeyStroke: TSynEditKeyStroke;
    ShortCut1, ShortCut2: TShortCut;
  end;
  PShortcutItemData = ^TShortcutItemData;

  // Color set for grid text, and preset class with a name
  TGridTextColors = Array[TDBDatatypeCategoryIndex] of TColor;
  TGridColorsPreset = class
    TextColors: TGridTextColors;
    Name: String;
  end;
  TGridColorsPresetList = TObjectList<TGridColorsPreset>;

  TfrmPreferences = class(TExtForm)
    pagecontrolMain: TPageControl;
    tabMisc: TTabSheet;
    btnCancel: TButton;
    btnOK: TButton;
    btnApply: TButton;
    tabSQL: TTabSheet;
    chkAutoReconnect: TCheckBox;
    tabGridFormatting: TTabSheet;
    lblDataFont: TLabel;
    comboDataFontName: TComboBox;
    editDataFontSize: TEdit;
    updownDataFontSize: TUpDown;
    lblDataFontHint: TLabel;
    lblMaxColWidth: TLabel;
    editMaxColWidth: TEdit;
    updownMaxColWidth: TUpDown;
    chkRestoreLastDB: TCheckBox;
    chkUpdatecheck: TCheckBox;
    editUpdatecheckInterval: TEdit;
    updownUpdatecheckInterval: TUpDown;
    chkUpdateCheckBuilds: TCheckBox;
    SynSQLSynSQLSample: TSynSQLSyn;
    btnRestoreDefaults: TButton;
    lblMaxTotalRows: TLabel;
    editGridRowCountMax: TEdit;
    chkDoStatistics: TCheckBox;
    tabShortcuts: TTabSheet;
    TreeShortcutItems: TVirtualStringTree;
    lblShortcut1: TLabel;
    lblShortcutHint: TLabel;
    lblShortcut2: TLabel;
    chkAllowMultiInstances: TCheckBox;
    tabLogging: TTabSheet;
    Label4: TLabel;
    editLogLines: TEdit;
    updownLogLines: TUpDown;
    lblLogLinesHint: TLabel;
    lblLogSnipHint: TLabel;
    updownLogSnip: TUpDown;
    editLogSnip: TEdit;
    lblLogSnip: TLabel;
    chkLogToFile: TCheckBox;
    editLogDir: TButtonedEdit;
    lblLogLevel: TLabel;
    chkLogEventErrors: TCheckBox;
    chkLogEventUserGeneratedSQL: TCheckBox;
    chkLogEventSQL: TCheckBox;
    chkLogEventInfo: TCheckBox;
    chkLogEventDebug: TCheckBox;
    editGridRowCountStep: TEdit;
    lblGridRowsLinecount: TLabel;
    editGridRowsLineCount: TEdit;
    updownGridRowsLineCount: TUpDown;
    chkColorBars: TCheckBox;
    comboSQLFontName: TComboBox;
    lblFont: TLabel;
    editSQLFontSize: TEdit;
    updownSQLFontSize: TUpDown;
    lblSQLFontSizeUnit: TLabel;
    chkCompletionProposal: TCheckBox;
    chkTabsToSpaces: TCheckBox;
    editSQLTabWidth: TEdit;
    updownSQLTabWidth: TUpDown;
    Label1: TLabel;
    lblMaxQueryResults: TLabel;
    editMaxQueryResults: TEdit;
    updownMaxQueryResults: TUpDown;
    lblGridTextColors: TLabel;
    comboGridTextColors: TComboBox;
    colorBoxGridTextColors: TColorBox;
    lblNullBackground: TLabel;
    cboxNullBackground: TColorBox;
    lblMySQLBinaries: TLabel;
    editMySQLBinaries: TButtonedEdit;
    lblLanguage: TLabel;
    comboAppLanguage: TComboBox;
    chkQueryHistory: TCheckBox;
    cboxRowBackgroundOdd: TColorBox;
    cboxRowBackgroundEven: TColorBox;
    Label2: TLabel;
    tabDataEditors: TTabSheet;
    chkEditorBinary: TCheckBox;
    chkEditorDatetime: TCheckBox;
    chkPrefillDateTime: TCheckBox;
    chkEditorEnum: TCheckBox;
    chkEditorSet: TCheckBox;
    chkReuseEditorConfiguration: TCheckBox;
    chkForeignDropDown: TCheckBox;
    chkLocalNumberFormat: TCheckBox;
    lblSQLColElement: TLabel;
    comboSQLColElement: TComboBox;
    chkSQLBold: TCheckBox;
    chkSQLItalic: TCheckBox;
    lblSQLColBackground: TLabel;
    lblSQLColForeground: TLabel;
    cboxSQLColForeground: TColorBox;
    cboxSQLColBackground: TColorBox;
    SynMemoSQLSample: TSynMemo;
    editCustomSnippetsDirectory: TButtonedEdit;
    lblCustomSnippetsDirectory: TLabel;
    chkHintsOnResultTabs: TCheckBox;
    lblLineBreakStyle: TLabel;
    comboLineBreakStyle: TComboBox;
    lblGUIFont: TLabel;
    comboGUIFont: TComboBox;
    editGUIFontSize: TEdit;
    updownGUIFontSize: TUpDown;
    lblGUIFontSize: TLabel;
    chkHorizontalScrollbar: TCheckBox;
    editQueryHistoryKeepDays: TEdit;
    updownQueryHistoryKeepDays: TUpDown;
    lblQueryHistoryKeepDays: TLabel;
    Label3: TLabel;
    cboxRowHighlightSameText: TColorBox;
    chkWheelZoom: TCheckBox;
    chkQueryWarningsMessage: TCheckBox;
    chkAutoUppercase: TCheckBox;
    lblTheme: TLabel;
    comboTheme: TComboBox;
    lblEditorColorsPreset: TLabel;
    comboEditorColorsPreset: TComboBox;
    SynSQLSyn_Dark: TSynSQLSyn;
    SynSQLSyn_Light: TSynSQLSyn;
    SynSQLSyn_Black: TSynSQLSyn;
    SynSQLSyn_White: TSynSQLSyn;
    comboGridTextColorsPreset: TComboBox;
    lblIconPack: TLabel;
    comboIconPack: TComboBox;
    tabFiles: TTabSheet;
    chkAskFileSave: TCheckBox;
    chkRestoreTabs: TCheckBox;
    chkLogEventScript: TCheckBox;
    lblWebSearchBaseUrl: TLabel;
    comboWebSearchBaseUrl: TComboBox;
    chkThemePreview: TCheckBox;
    chkCompletionProposalSearchOnMid: TCheckBox;
    lblLongSortRowNum: TLabel;
    editLongSortRowNum: TEdit;
    updownLongSortRowNum: TUpDown;
    HotKey1: THotKey;
    HotKey2: THotKey;
    chkLowercaseHex: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Modified(Sender: TObject);
    procedure Apply(Sender: TObject);
    procedure SQLFontChange(Sender: TObject);
    procedure DataFontsChange(Sender: TObject);
    procedure anyUpDownLimitChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure editLogDirRightButtonClick(Sender: TObject);
    procedure chkLogToFileClick(Sender: TObject);
    procedure chkUpdatecheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure comboSQLColElementChange(Sender: TObject);
    procedure pagecontrolMainChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure pagecontrolMainChange(Sender: TObject);
    procedure updownSQLFontSizeClick(Sender: TObject; Button: TUDBtnType);
    procedure SynMemoSQLSampleClick(Sender: TObject);
    procedure btnRestoreDefaultsClick(Sender: TObject);
    procedure TreeShortcutItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeShortcutItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure TreeShortcutItemsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure TreeShortcutItemsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure TreeShortcutItemsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeShortcutItemsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure HotKeyEnter(Sender: TObject);
    procedure HotKeyExit(Sender: TObject);
    procedure comboGridTextColorsSelect(Sender: TObject);
    procedure colorBoxGridTextColorsSelect(Sender: TObject);
    procedure editMySQLBinariesRightButtonClick(Sender: TObject);
    procedure editGridRowCountExit(Sender: TObject);
    procedure editCustomSnippetsDirectoryRightButtonClick(Sender: TObject);
    procedure comboGUIFontChange(Sender: TObject);
    procedure chkQueryHistoryClick(Sender: TObject);
    procedure comboEditorColorsPresetChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure comboGridTextColorsPresetSelect(Sender: TObject);
    procedure comboThemeSelect(Sender: TObject);
    procedure chkThemePreviewClick(Sender: TObject);
    procedure chkCompletionProposalClick(Sender: TObject);
    procedure HotKeyChange(Sender: TObject);
  private
    { Private declarations }
    FWasModified: Boolean;
    FShortcutCategories: TStringList;
    FGridTextColors: TGridTextColors;
    FGridColorsPresets: TGridColorsPresetList;
    FLanguages: TStringList;
    FRestartOptionTouched: Boolean;
    FRestartOptionApplied: Boolean;
    FThemePreview: TfrmThemePreview;
    procedure InitLanguages;
    procedure SelectDirectory(Sender: TObject; NewFolderButton: Boolean);
    function EnsureShortcutIsUnused(RequestShortcut: TShortCut): Boolean;
  public
    { Public declarations }
  end;


var
  frmPreferences: TfrmPreferences;

function EnumFixedProc(lpelf: PEnumLogFont; lpntm: PNewTextMetric; FontType: Integer; Data: LPARAM): Integer; stdcall;


implementation
uses main, apphelpers;
{$R *.DFM}


procedure TfrmPreferences.Modified(Sender: TObject);
begin
  // Modified
  btnApply.Enabled := True;
  // Sending controls with a Tag property > 0 (normally 1) need an application restart
  if (Sender is TComponent) and (TComponent(Sender).Tag <> 0) then begin
    FRestartOptionTouched := True;
  end;
end;


procedure TfrmPreferences.pagecontrolMainChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Remember modification state. First tab switch leads TEdit's with TUpDown
  // to fire OnChange. Avoid enabling the buttons in that case.
  FWasModified := btnApply.Enabled;
end;


procedure TfrmPreferences.pagecontrolMainChange(Sender: TObject);
begin
  // See OnChanging procedure
  btnApply.Enabled := FWasModified;
end;


{**
  Apply settings to registry and mainform
}
procedure TfrmPreferences.Apply(Sender: TObject);
var
  i: Integer;
  Attri: TSynHighlighterAttributes;
  CatNode, ItemNode: PVirtualNode;
  Data: PShortcutItemData;
  LangCode: String;
begin
  Screen.Cursor := crHourGlass;

  // Save values
  AppSettings.WriteBool(asAutoReconnect, chkAutoReconnect.Checked);
  AppSettings.WriteBool(asAllowMultipleInstances, chkAllowMultiInstances.Checked);
  AppSettings.WriteBool(asRestoreLastUsedDB, chkRestoreLastDB.Checked);
  AppSettings.WriteBool(asQueryWarningsMessage, chkQueryWarningsMessage.Checked);
  AppSettings.WriteString(asFontName, comboSQLFontName.Text);
  AppSettings.WriteInt(asFontSize, updownSQLFontSize.Position);
  AppSettings.WriteInt(asTabWidth, updownSQLTabWidth.Position);
  AppSettings.WriteInt(asLogsqlnum, updownLogLines.Position);
  AppSettings.WriteInt(asLogsqlwidth, updownLogSnip.Position);
  AppSettings.WriteString(asSessionLogsDirectory, editLogDir.Text);
  AppSettings.WriteBool(asLogErrors, chkLogEventErrors.Checked);
  AppSettings.WriteBool(asLogUserSQL, chkLogEventUserGeneratedSQL.Checked);
  AppSettings.WriteBool(asLogSQL, chkLogEventSQL.Checked);
  AppSettings.WriteBool(asLogScript, chkLogEventScript.Checked);
  AppSettings.WriteBool(asLogInfos, chkLogEventInfo.Checked);
  AppSettings.WriteBool(asLogDebug, chkLogEventDebug.Checked);
  AppSettings.WriteBool(asQueryHistoryEnabled, chkQueryHistory.Checked);
  AppSettings.WriteInt(asQueryHistoryKeepDays, updownQueryHistoryKeepDays.Position);
  AppSettings.WriteBool(asLogHorizontalScrollbar, chkHorizontalScrollbar.Checked);
  for i:=0 to SynSQLSynSQLSample.AttrCount - 1 do begin
    Attri := SynSQLSynSQLSample.Attribute[i];
    AppSettings.WriteInt(asHighlighterForeground, Attri.Foreground, Attri.Name);
    AppSettings.WriteInt(asHighlighterBackground, Attri.Background, Attri.Name);
    AppSettings.WriteInt(asHighlighterStyle, Attri.IntegerStyle, Attri.Name);
  end;
  AppSettings.WriteString(asSQLColActiveLine, ColorToString(SynMemoSQLSample.ActiveLineColor));
  AppSettings.WriteString(asSQLColMatchingBraceForeground, ColorToString(MainForm.MatchingBraceForegroundColor));
  AppSettings.WriteString(asSQLColMatchingBraceBackground, ColorToString(MainForm.MatchingBraceBackgroundColor));

  AppSettings.WriteInt(asMaxColWidth, updownMaxColWidth.Position);
  AppSettings.WriteInt(asDatagridRowsPerStep, StrToIntDef(editGridRowCountStep.Text, -1));
  AppSettings.WriteInt(asDatagridMaximumRows, StrToIntDef(editGridRowCountMax.Text, -1));
  AppSettings.WriteInt(asGridRowLineCount, updownGridRowsLineCount.Position);
  AppSettings.WriteString(asDataFontName, comboDataFontName.Text);
  AppSettings.WriteInt(asDataFontSize, updownDataFontSize.Position);
  AppSettings.WriteBool(asLogToFile, chkLogToFile.Checked);
  if not RunningAsUwp then begin
    AppSettings.WriteBool(asUpdatecheck, chkUpdatecheck.Checked);
    AppSettings.WriteBool(asUpdatecheckBuilds, chkUpdatecheckBuilds.Checked);
    AppSettings.WriteInt(asUpdatecheckInterval, updownUpdatecheckInterval.Position);
  end;
  AppSettings.WriteBool(asDoUsageStatistics, chkDoStatistics.Checked);
  AppSettings.WriteBool(asWheelZoom, chkWheelZoom.Checked);
  AppSettings.WriteBool(asDisplayBars, chkColorBars.Checked);
  AppSettings.WriteString(asMySQLBinaries, editMySQLBinaries.Text);
  AppSettings.WriteString(asCustomSnippetsDirectory, editCustomSnippetsDirectory.Text);

  if comboAppLanguage.ItemIndex > 0 then begin
    // Get language code from the left text in the dropdown item text, up to the colon
    LangCode := RegExprGetMatch('^(\w+)\b', comboAppLanguage.Text, 1);
  end else begin
    LangCode := '';
  end;
  AppSettings.WriteString(asAppLanguage, LangCode);

  if comboGUIFont.ItemIndex = 0 then
    AppSettings.WriteString(asGUIFontName, '')
  else
    AppSettings.WriteString(asGUIFontName, comboGUIFont.Text);
  AppSettings.WriteInt(asGUIFontSize, updownGUIFontSize.Position);
  AppSettings.WriteString(asTheme, comboTheme.Text);
  AppSettings.WriteString(asIconPack, comboIconPack.Text);
  AppSettings.WriteString(asWebSearchBaseUrl, comboWebSearchBaseUrl.Text);

  AppSettings.WriteInt(asMaxQueryResults, updownMaxQueryResults.Position);
  // Save color settings
  AppSettings.WriteInt(asFieldColorNumeric, FGridTextColors[dtcInteger]);
  AppSettings.WriteInt(asFieldColorReal, FGridTextColors[dtcReal]);
  AppSettings.WriteInt(asFieldColorText, FGridTextColors[dtcText]);
  AppSettings.WriteInt(asFieldColorBinary, FGridTextColors[dtcBinary]);
  AppSettings.WriteInt(asFieldColorDatetime, FGridTextColors[dtcTemporal]);
  AppSettings.WriteInt(asFieldColorSpatial, FGridTextColors[dtcSpatial]);
  AppSettings.WriteInt(asFieldColorOther, FGridTextColors[dtcOther]);
  AppSettings.WriteInt(asFieldNullBackground, cboxNullBackground.Selected);
  AppSettings.WriteInt(asRowBackgroundEven, cboxRowBackgroundEven.Selected);
  AppSettings.WriteInt(asRowBackgroundOdd, cboxRowBackgroundOdd.Selected);
  AppSettings.WriteInt(asHightlightSameTextBackground, cboxRowHighlightSameText.Selected);
  AppSettings.WriteBool(asDataLocalNumberFormat, chkLocalNumberFormat.Checked);
  AppSettings.WriteBool(asLowercaseHex, chkLowercaseHex.Checked);
  AppSettings.WriteBool(asHintsOnResultTabs, chkHintsOnResultTabs.Checked);
  AppSettings.WriteInt(asQueryGridLongSortRowNum, updownLongSortRowNum.Position);

  // Editor Configuration
  AppSettings.WriteBool(asFieldEditorBinary, chkEditorBinary.Checked);
  AppSettings.WriteBool(asFieldEditorDatetime, chkEditorDatetime.Checked);
  AppSettings.WriteBool(asFieldEditorDatetimePrefill, chkPrefillDatetime.Checked);
  AppSettings.WriteBool(asFieldEditorEnum, chkEditorEnum.Checked);
  AppSettings.WriteBool(asFieldEditorSet, chkEditorSet.Checked);
  AppSettings.WriteBool(asReuseEditorConfiguration, chkReuseEditorConfiguration.Checked);
  AppSettings.WriteBool(asForeignDropDown, chkForeignDropDown.Checked);
  case comboLineBreakStyle.ItemIndex of
    1: AppSettings.WriteInt(asLineBreakStyle, Integer(lbsUnix));
    2: AppSettings.WriteInt(asLineBreakStyle, Integer(lbsMac));
    else AppSettings.WriteInt(asLineBreakStyle, Integer(lbsWindows));
  end;

  AppSettings.WriteBool(asCompletionProposal, chkCompletionProposal.Checked);
  AppSettings.WriteBool(asCompletionProposalSearchOnMid, chkCompletionProposalSearchOnMid.Checked);
  AppSettings.WriteBool(asAutoUppercase, chkAutoUppercase.Checked);
  AppSettings.WriteBool(asTabsToSpaces, chkTabsToSpaces.Checked);

  // Shortcuts
  CatNode := TreeShortcutItems.GetFirst;
  while Assigned(CatNode) do begin
    ItemNode := TreeShortcutItems.GetFirstChild(CatNode);
    while Assigned(ItemNode) do begin
      Data := TreeShortcutItems.GetNodeData(ItemNode);
      // Save modified shortcuts
      if Assigned(Data.KeyStroke) then begin
        if Data.Shortcut1 <> Data.KeyStroke.ShortCut then
          AppSettings.WriteInt(asActionShortcut1, Data.Shortcut1, EditorCommandToCodeString(Data.KeyStroke.Command));
        if Data.Shortcut2 <> Data.KeyStroke.ShortCut2 then
          AppSettings.WriteInt(asActionShortcut2, Data.Shortcut2, EditorCommandToCodeString(Data.KeyStroke.Command));
      end else begin
        if Data.Shortcut1 <> Data.Action.ShortCut then
          AppSettings.WriteInt(asActionShortcut1, Data.Shortcut1, Data.Action.Name);
        // Apply shortcut for this session
        Data.Action.ShortCut := Data.Shortcut1;
      end;
      ItemNode := TreeShortcutItems.GetNextSibling(ItemNode);
    end;
    CatNode := TreeShortcutItems.GetNextSibling(CatNode);
  end;
  // Populate SynMemo settings to all instances
  Mainform.SetupSynEditors;

  // Files
  AppSettings.WriteBool(asPromptSaveFileOnTabClose, chkAskFileSave.Checked);
  AppSettings.WriteBool(asRestoreTabs, chkRestoreTabs.Checked);

  // Set relevant properties in mainform
  MainForm.ApplyFontToGrids;
  MainForm.PrepareImageList;

  Mainform.LogToFile := chkLogToFile.Checked;
  MainForm.actLogHorizontalScrollbar.Checked := chkHorizontalScrollbar.Checked;
  MainForm.actLogHorizontalScrollbar.OnExecute(MainForm.actLogHorizontalScrollbar);
  DatatypeCategories[dtcInteger].Color := FGridTextColors[dtcInteger];
  DatatypeCategories[dtcReal].Color := FGridTextColors[dtcReal];
  DatatypeCategories[dtcText].Color := FGridTextColors[dtcText];
  DatatypeCategories[dtcBinary].Color := FGridTextColors[dtcBinary];
  DatatypeCategories[dtcTemporal].Color := FGridTextColors[dtcTemporal];
  DatatypeCategories[dtcSpatial].Color := FGridTextColors[dtcSpatial];
  DatatypeCategories[dtcOther].Color := FGridTextColors[dtcOther];
  Mainform.DataLocalNumberFormat := chkLocalNumberFormat.Checked;
  Mainform.CalcNullColors;
  Mainform.DataGrid.Repaint;
  Mainform.QueryGrid.Repaint;
  Mainform.ListTables.Invalidate;
  Mainform.ListProcesses.Invalidate;
  Mainform.ListCommandStats.Invalidate;


  FRestartOptionApplied := FRestartOptionTouched;

  // Settings have been applied, send a signal to the user
  btnApply.Enabled := False;
  Screen.Cursor := crDefault;
end;


// Callback function used by EnumFontFamilies()
function EnumFixedProc(
  lpelf: PEnumLogFont;
  lpntm: PNewTextMetric;
  FontType: Integer;
  Data: LPARAM
  ): Integer; stdcall;
begin
  Result := 1;  // don't cancel
  if (lpelf^.elfLogFont.lfPitchAndFamily and FIXED_PITCH) <> 0 then
    (TStrings(Data)).Add(String(lpelf^.elfLogFont.lfFaceName));
end;


procedure TfrmPreferences.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FRestartOptionApplied then begin
    MessageDialog(f_('You should restart %s to apply changed critical settings, and to prevent unexpected behaviour.', [APPNAME]),
      mtInformation,
      [mbOk]);
  end;
  MainForm.ActionList1.State := asNormal;
  AppSettings.WriteInt(asPreferencesWindowWidth, Width);
  AppSettings.WriteInt(asPreferencesWindowHeight, Height);
end;


procedure TfrmPreferences.FormCreate(Sender: TObject);
const
  // Define grid colors as constants, for easy assignment
  GridColorsLight: TGridTextColors = ($00FF0000, $00FF0048, $00008000, $00800080, $00000080, $00808000, $00008080);
  GridColorsDark: TGridTextColors = ($00FF9785, $00D07D7D, $0073D573, $00C9767F, $007373C9, $00CECE73, $0073C1C1);
  GridColorsBlack: TGridTextColors = ($00000000, $00000000, $00000000, $00000000, $00000000, $00000000, $00000000);
  GridColorsWhite: TGridTextColors = ($00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF, $00FFFFFF);
var
  i: Integer;
  dtc: TDBDatatypeCategoryIndex;
  Styles: TArray<String>;
  Highlighter: TSynSQLSyn;
  Name: String;
  GridColorsPreset: TGridColorsPreset;
  IconPack: String;
begin
  HasSizeGrip := True;

  Width := AppSettings.ReadInt(asPreferencesWindowWidth);
  Height := AppSettings.ReadInt(asPreferencesWindowHeight);

  // Misecllaneous
  // Hide browse button on Wine, as the browse dialog returns Windows-style paths, while we need a Unix path
  if IsWine then begin
    editMySQLBinaries.RightButton.Visible := False;
    editMySQLBinaries.OnDblClick := nil;
  end;

  InitLanguages;
  comboAppLanguage.Items.AddStrings(FLanguages);

  comboGUIFont.Items.Assign(Screen.Fonts);
  comboGUIFont.Items.Insert(0, '<'+_('Default system font')+'>');

  Styles := TStyleManager.StyleNames;
  for i:=Low(Styles) to High(Styles) do begin
    comboTheme.Items.Add(Styles[i]);
  end;
  comboTheme.ItemIndex := comboTheme.Items.IndexOf(AppSettings.GetDefaultString(asTheme));

  // Populate icon pack dropdown from image collections on main form
  comboIconPack.Items.Clear;
  for i:=0 to MainForm.ComponentCount-1 do begin
    if MainForm.Components[i] is TImageCollection then begin
      IconPack := MainForm.Components[i].Name;
      IconPack := StringReplace(IconPack, 'ImageCollection', '', [rfIgnoreCase]);
      comboIconPack.Items.Add(IconPack);
    end;
  end;

  // Data
  // Populate datatype categories pulldown
  for dtc:=Low(TDBDatatypeCategoryIndex) to High(TDBDatatypeCategoryIndex) do
    comboGridTextColors.Items.Add(DatatypeCategories[dtc].Name);

  // SQL
  EnumFontFamilies(Canvas.Handle, nil, @EnumFixedProc, LPARAM(Pointer(comboSQLFontName.Items)));
  comboSQLFontName.Sorted := True;
  SynMemoSQLSample.Text := 'SELECT DATE_SUB(NOW(), INTERVAL 1 DAY),' + CRLF +
    #9'''String literal'' AS lit' + CRLF +
    'FROM tableA AS ta' + CRLF +
    'WHERE `columnA` IS NULL;' + CRLF +
    CRLF +
    '-- A comment' + CRLF +
    '# Old style comment' + CRLF +
    '/* Multi line comment */' + CRLF +
    CRLF +
    'CREATE TABLE /*!32312 IF NOT EXISTS*/ tableB (' + CRLF +
    #9'id INT,' + CRLF +
    #9'name VARCHAR(30) DEFAULT "standard"' + CRLF +
    ')';
  SynSQLSynSQLSample.TableNames.CommaText := 'tableA,tableB';
  for i:=0 to SynSQLSynSQLSample.AttrCount - 1 do begin
    SynSQLSynSQLSample.Attribute[i].AssignColorAndStyle(MainForm.SynSQLSynUsed.Attribute[i]);
    comboSQLColElement.Items.Add(SynSQLSynSQLSample.Attribute[i].FriendlyName);
  end;
  comboSQLColElement.Items.Add(_('Active line background'));
  comboSQLColElement.Items.Add(_('Brace matching color'));
  comboSQLColElement.ItemIndex := 0;
  // Enumerate highlighter presets
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TSynSQLSyn)
      and (Components[i] <> SynMemoSQLSample.Highlighter)
      then begin
      Highlighter := Components[i] as TSynSQLSyn;
      Name := Highlighter.Name;
      Name := RegExprGetMatch('_([^_]+)$', Name, 1);
      if Name <> '' then begin
        comboEditorColorsPreset.Items.Add(_(Name));
      end;
    end;
  end;

  // Grid formatting
  FGridColorsPresets := TGridColorsPresetList.Create;
  // Current colors - assign from global DatatypeCategories array
  GridColorsPreset := TGridColorsPreset.Create;
  GridColorsPreset.Name := _('Current custom settings');
  for dtc:=Low(TDBDatatypeCategoryIndex) to High(TDBDatatypeCategoryIndex) do begin
    GridColorsPreset.TextColors[dtc] := DatatypeCategories[dtc].Color;
  end;
  FGridColorsPresets.Add(GridColorsPreset);
  // Light - default values
  GridColorsPreset := TGridColorsPreset.Create;
  GridColorsPreset.Name := _('Light');
  GridColorsPreset.TextColors := GridColorsLight;
  FGridColorsPresets.Add(GridColorsPreset);
  // Dark
  GridColorsPreset := TGridColorsPreset.Create;
  GridColorsPreset.Name := _('Dark');
  GridColorsPreset.TextColors := GridColorsDark;
  FGridColorsPresets.Add(GridColorsPreset);
  // Black
  GridColorsPreset := TGridColorsPreset.Create;
  GridColorsPreset.Name := _('Black');
  GridColorsPreset.TextColors := GridColorsBlack;
  FGridColorsPresets.Add(GridColorsPreset);
  // White
  GridColorsPreset := TGridColorsPreset.Create;
  GridColorsPreset.Name := _('White');
  GridColorsPreset.TextColors := GridColorsWhite;
  FGridColorsPresets.Add(GridColorsPreset);
  // Add all to combo box
  comboGridTextColorsPreset.Clear;
  for GridColorsPreset in FGridColorsPresets do begin
    comboGridTextColorsPreset.Items.Add(GridColorsPreset.Name);
  end;

  // Shortcuts
  FShortcutCategories := TStringList.Create;
  for i:=0 to Mainform.ActionList1.ActionCount-1 do begin
    if FShortcutCategories.IndexOf(Mainform.ActionList1.Actions[i].Category) = -1 then
      FShortcutCategories.Add(Mainform.ActionList1.Actions[i].Category);
  end;
  FShortcutCategories.Add(_('SQL editing'));
  TreeShortcutItems.RootNodeCount := FShortcutCategories.Count;
  comboLineBreakStyle.Items := Explode(',', _('Windows linebreaks')+','+_('UNIX linebreaks')+','+_('Mac OS linebreaks'));
end;


procedure TfrmPreferences.FormShow(Sender: TObject);
var
  LangCode, GUIFont: String;
  i: Integer;
begin
  screen.Cursor := crHourGlass;

  // Read and display values
  chkAutoReconnect.Checked := AppSettings.ReadBool(asAutoReconnect);;
  chkAllowMultiInstances.Checked := AppSettings.ReadBool(asAllowMultipleInstances);
  chkRestoreLastDB.Checked := AppSettings.ReadBool(asRestoreLastUsedDB);
  if RunningAsUwp then begin
    chkUpdatecheck.Enabled := False;
    chkUpdatecheckBuilds.Enabled := False;
    updownUpdatecheckInterval.Enabled := False;
  end else begin
    chkUpdatecheck.Checked := AppSettings.ReadBool(asUpdatecheck);
    chkUpdatecheckBuilds.Checked := AppSettings.ReadBool(asUpdatecheckBuilds);
    updownUpdatecheckInterval.Position := AppSettings.ReadInt(asUpdatecheckInterval);
    chkUpdatecheckClick(Sender);
  end;
  chkDoStatistics.Checked := AppSettings.ReadBool(asDoUsageStatistics);
  chkWheelZoom.Checked := AppSettings.ReadBool(asWheelZoom);
  chkColorBars.Checked := AppSettings.ReadBool(asDisplayBars);
  editMySQLBinaries.Text := AppSettings.ReadString(asMySQLBinaries);
  editCustomSnippetsDirectory.Text := AppSettings.ReadString(asCustomSnippetsDirectory);
  LangCode := AppSettings.ReadString(asAppLanguage);
  for i:=0 to comboAppLanguage.Items.Count-1 do begin
    if RegExprGetMatch('^(\w+)\b', comboAppLanguage.Items[i], 1) = LangCode then begin
      comboAppLanguage.ItemIndex := i;
      Break;
    end;
  end;
  if comboAppLanguage.ItemIndex = -1 then
    comboAppLanguage.ItemIndex := 0;
  GUIFont := AppSettings.ReadString(asGUIFontName);
  if GUIFont.IsEmpty then
    comboGUIFont.ItemIndex := 0
  else
    comboGUIFont.ItemIndex := comboGUIFont.Items.IndexOf(GUIFont);
  updownGUIFontSize.Position := AppSettings.ReadInt(asGUIFontSize);
  comboGUIFont.OnChange(comboGUIFont);
  comboTheme.ItemIndex := comboTheme.Items.IndexOf(AppSettings.ReadString(asTheme));
  comboIconPack.ItemIndex := comboIconPack.Items.IndexOf(AppSettings.ReadString(asIconPack));
  comboWebSearchBaseUrl.Text := AppSettings.ReadString(asWebSearchBaseUrl);
  chkQueryWarningsMessage.Checked := AppSettings.ReadBool(asQueryWarningsMessage);

  // Logging
  updownLogLines.Position := AppSettings.ReadInt(asLogsqlnum);
  updownLogSnip.Position := AppSettings.ReadInt(asLogsqlwidth);
  chkLogToFile.Checked := AppSettings.ReadBool(asLogToFile);
  editLogDir.Text := AppSettings.ReadString(asSessionLogsDirectory);
  chkLogEventErrors.Checked := AppSettings.ReadBool(asLogErrors);
  chkLogEventUserGeneratedSQL.Checked := AppSettings.ReadBool(asLogUserSQL);
  chkLogEventSQL.Checked := AppSettings.ReadBool(asLogSQL);
  chkLogEventScript.Checked := AppSettings.ReadBool(asLogScript);
  chkLogEventInfo.Checked := AppSettings.ReadBool(asLogInfos);
  chkLogEventDebug.Checked := AppSettings.ReadBool(asLogDebug);
  chkQueryHistory.Checked := AppSettings.ReadBool(asQueryHistoryEnabled);
  updownQueryHistoryKeepDays.Position := AppSettings.ReadInt(asQueryHistoryKeepDays);
  chkHorizontalScrollbar.Checked := AppSettings.ReadBool(asLogHorizontalScrollbar);

  // Default column width in grids:
  updownMaxColWidth.Position := AppSettings.ReadInt(asMaxColWidth);
  editGridRowCountStep.Text := IntToStr(AppSettings.ReadInt(asDatagridRowsPerStep));
  editGridRowCountMax.Text := IntToStr(AppSettings.ReadInt(asDatagridMaximumRows));
  updownGridRowsLineCount.Position := AppSettings.ReadInt(asGridRowLineCount);

  // SQL:
  Mainform.SetupSynEditors;
  comboSQLFontName.ItemIndex := comboSQLFontName.Items.IndexOf(SynMemoSQLSample.Font.Name);
  updownSQLFontSize.Position := SynMemoSQLSample.Font.Size;
  updownSQLTabWidth.Position := SynMemoSQLSample.TabWidth;
  chkCompletionProposal.Checked := AppSettings.ReadBool(asCompletionProposal);
  chkCompletionProposalSearchOnMid.Checked := AppSettings.ReadBool(asCompletionProposalSearchOnMid);
  chkAutoUppercase.Checked := AppSettings.ReadBool(asAutoUppercase);
  chkTabsToSpaces.Checked := AppSettings.ReadBool(asTabsToSpaces);
  comboSQLColElementChange(Sender);

  // Grid formatting:
  comboDataFontName.Items := Screen.Fonts;
  comboDataFontName.ItemIndex := comboDataFontName.Items.IndexOf(AppSettings.ReadString(asDataFontName));
  updownDataFontSize.Position := AppSettings.ReadInt(asDataFontSize);
  updownMaxQueryResults.Position := AppSettings.ReadINt(asMaxQueryResults);
  // Load color settings
  FGridTextColors[dtcInteger] := AppSettings.ReadInt(asFieldColorNumeric);
  FGridTextColors[dtcReal] := AppSettings.ReadInt(asFieldColorReal);
  FGridTextColors[dtcText] := AppSettings.ReadInt(asFieldColorText);
  FGridTextColors[dtcBinary] := AppSettings.ReadInt(asFieldColorBinary);
  FGridTextColors[dtcTemporal] := AppSettings.ReadInt(asFieldColorDatetime);
  FGridTextColors[dtcSpatial] := AppSettings.ReadInt(asFieldColorSpatial);
  FGridTextColors[dtcOther] := AppSettings.ReadInt(asFieldColorOther);
  comboGridTextColorsPreset.ItemIndex := 0;
  comboGridTextColors.ItemIndex := 0;
  comboGridTextColors.OnSelect(comboGridTextColors);
  cboxNullBackground.Selected := AppSettings.ReadInt(asFieldNullBackground);
  cboxRowBackgroundEven.Selected := AppSettings.ReadInt(asRowBackgroundEven);
  cboxRowBackgroundOdd.Selected := AppSettings.ReadInt(asRowBackgroundOdd);
  cboxRowHighlightSameText.Selected := AppSettings.ReadInt(asHightlightSameTextBackground);
  chkLocalNumberFormat.Checked := AppSettings.ReadBool(asDataLocalNumberFormat);
  chkLowercaseHex.Checked := AppSettings.ReadBool(asLowercaseHex);
  chkHintsOnResultTabs.Checked := AppSettings.ReadBool(asHintsOnResultTabs);
  updownLongSortRowNum.Position := AppSettings.ReadInt(asQueryGridLongSortRowNum);

  // Editor Configuration
  chkEditorBinary.Checked := AppSettings.ReadBool(asFieldEditorBinary);
  chkEditorDatetime.Checked := AppSettings.ReadBool(asFieldEditorDatetime);
  chkPrefillDateTime.Checked := AppSettings.ReadBool(asFieldEditorDatetimePrefill);
  chkEditorEnum.Checked := AppSettings.ReadBool(asFieldEditorEnum);
  chkEditorSet.Checked := AppSettings.ReadBool(asFieldEditorEnum);
  chkReuseEditorConfiguration.Checked := AppSettings.ReadBool(asReuseEditorConfiguration);
  chkForeignDropDown.Checked := AppSettings.ReadBool(asForeignDropDown);
  case TLineBreaks(AppSettings.ReadInt(asLineBreakStyle)) of
    lbsNone, lbsWindows: comboLineBreakStyle.ItemIndex := 0;
    lbsUnix: comboLineBreakStyle.ItemIndex := 1;
    lbsMac: comboLineBreakStyle.ItemIndex := 2;
  end;

  // Shortcuts
  TreeShortcutItems.ReinitChildren(nil, True);
  SelectNode(TreeShortcutItems, nil);

  // Files
  chkAskFileSave.Checked := AppSettings.ReadBool(asPromptSaveFileOnTabClose);
  chkRestoreTabs.Checked := AppSettings.ReadBool(asRestoreTabs);

  // Disable global shortcuts
  MainForm.ActionList1.State := asSuspended;

  FRestartOptionTouched := False;
  btnApply.Enabled := False;
  screen.Cursor := crdefault;
end;



procedure TfrmPreferences.SQLFontChange(Sender: TObject);
var
  AttriIdx: Integer;
  Attri: TSynHighlighterAttributes;
  Foreground, Background: TColor;
begin
  SynMemoSQLSample.Font.Name := comboSQLFontName.Items[comboSQLFontName.ItemIndex];
  SynMemoSQLSample.Font.Size := updownSQLFontSize.Position;
  SynMemoSQLSample.TabWidth := updownSQLTabWidth.Position;
  AttriIdx := comboSQLColElement.ItemIndex;
  Foreground := cboxSQLColForeground.Selected;
  Background := cboxSQLColBackground.Selected;
  if AttriIdx = comboSQLColElement.Items.Count-1 then begin
    MainForm.MatchingBraceForegroundColor := Foreground;
    MainForm.MatchingBraceBackgroundColor := Background;
  end else if AttriIdx = comboSQLColElement.Items.Count-2 then begin
    SynMemoSQLSample.ActiveLineColor := Foreground;
  end else begin
    Attri := SynSqlSynSQLSample.Attribute[AttriIdx];
    Attri.Foreground := Foreground;
    Attri.Background := Background;
    if chkSQLBold.Checked then Attri.Style := Attri.Style + [fsBold]
    else Attri.Style := Attri.Style - [fsBold];
    if chkSQLItalic.Checked then Attri.Style := Attri.Style + [fsItalic]
    else Attri.Style := Attri.Style - [fsItalic];
  end;
  Modified(Sender);
end;


procedure TfrmPreferences.DataFontsChange(Sender: TObject);
begin
  Modified(Sender);
end;

procedure TfrmPreferences.anyUpDownLimitChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  Modified(Sender);
end;


procedure TfrmPreferences.editGridRowCountExit(Sender: TObject);
var
  Edit: TEdit;
begin
  // Row count step and maximum shall never be "0", to avoid problems in
  // data grids. See issue #3080.
  Edit := Sender as TEdit;
  if MakeInt(Edit.Text) <= 0 then
    Edit.Text := '1';
end;


procedure TfrmPreferences.SelectDirectory(Sender: TObject; NewFolderButton: Boolean);
var
  Browse: TBrowseForFolder;
  Edit: TButtonedEdit;
begin
  // Select folder for any option
  Edit := Sender as TButtonedEdit;
  Browse := TBrowseForFolder.Create(Self);
  Browse.Folder := Edit.Text;
  Browse.DialogCaption := _(Edit.TextHint);
  Browse.BrowseOptions := Browse.BrowseOptions + [bifNewDialogStyle];
  if not NewFolderButton then
    Browse.BrowseOptions := Browse.BrowseOptions + [bifNoNewFolderButton];
  if Browse.Execute then begin
    Edit.Text := Browse.Folder;
    Modified(Sender);
  end;
  Browse.Free;
end;


procedure TfrmPreferences.editLogDirRightButtonClick(Sender: TObject);
begin
  // Select folder for session logs
  SelectDirectory(Sender, True);
end;


procedure TfrmPreferences.editMySQLBinariesRightButtonClick(Sender: TObject);
begin
  // Select folder where MySQL binaries reside
  SelectDirectory(Sender, False);
end;


procedure TfrmPreferences.editCustomSnippetsDirectoryRightButtonClick(Sender: TObject);
begin
  // Set custom snippets directory
  SelectDirectory(Sender, True);
end;


{**
  Updatecheck checkbox was clicked
}
procedure TfrmPreferences.chkUpdatecheckClick(Sender: TObject);
begin
  updownUpdatecheckInterval.Enabled := chkUpdatecheck.Checked;
  editUpdatecheckInterval.Enabled := chkUpdatecheck.Checked;
  chkUpdatecheckBuilds.Enabled := chkUpdatecheck.Checked;
  Modified(Sender);
end;


procedure TfrmPreferences.chkCompletionProposalClick(Sender: TObject);
begin
  chkCompletionProposalSearchOnMid.Enabled := TCheckBox(Sender).Checked;
  Modified(Sender);
end;

procedure TfrmPreferences.chkLogToFileClick(Sender: TObject);
begin
  editLogDir.Enabled := TCheckBox(Sender).Checked;
  Modified(Sender);
end;


procedure TfrmPreferences.chkQueryHistoryClick(Sender: TObject);
begin
  editQueryHistoryKeepDays.Enabled := chkQueryHistory.Checked;
  updownQueryHistoryKeepDays.Enabled := chkQueryHistory.Checked;
  lblQueryHistoryKeepDays.Enabled := chkQueryHistory.Checked;
  Modified(Sender);
end;


procedure TfrmPreferences.comboEditorColorsPresetChange(Sender: TObject);
var
  i, j: Integer;
  Highlighter: TSynSQLSyn;
  FoundHighlighter: Boolean;
  rx: TRegExpr;
  TranslatedHighlighterName: String;
begin
  // Color preset selected
  FoundHighlighter := False;
  rx := TRegExpr.Create;
  rx.Expression := '.+_([a-zA-Z0-9]+)$';
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TSynSQLSyn) and (Components[i] <> SynMemoSQLSample.Highlighter) then begin
      Highlighter := Components[i] as TSynSQLSyn;

      // Translate highlighter postfix after last underscore ...
      TranslatedHighlighterName := '';
      if rx.Exec(Highlighter.Name) then begin
        TranslatedHighlighterName := _(rx.Match[1]);
      end;
      // ... so we can compare that with the selected dropdown text
      if TranslatedHighlighterName = comboEditorColorsPreset.Text then begin
        FoundHighlighter := True;
        for j:=0 to SynSQLSynSQLSample.AttrCount - 1 do begin
          SynSQLSynSQLSample.Attribute[j].AssignColorAndStyle(Highlighter.Attribute[j]);
        end;
        // Use 3 hardcoded default values for additional colors, which are not part
        // of the highlighter's attributes
        SynMemoSQLSample.ActiveLineColor := StringToColor(AppSettings.GetDefaultString(asSQLColActiveLine));
        MainForm.MatchingBraceForegroundColor := StringToColor(AppSettings.GetDefaultString(asSQLColMatchingBraceForeground));
        MainForm.MatchingBraceBackgroundColor := StringToColor(AppSettings.GetDefaultString(asSQLColMatchingBraceBackground));
        Break;
      end;
    end;
  end;
  if not FoundHighlighter then begin
    // Show current custom settings
    for i:=0 to SynSQLSynSQLSample.AttrCount - 1 do begin
      SynSQLSynSQLSample.Attribute[i].AssignColorAndStyle(MainForm.SynSQLSynUsed.Attribute[i]);
    end;
  end;
  Modified(Sender);
end;


procedure TfrmPreferences.comboGridTextColorsPresetSelect(Sender: TObject);
var
  Preset: TGridColorsPreset;
  dtc: TDBDatatypeCategoryIndex;
begin
  // Grid colors preset selected
  Preset := FGridColorsPresets[comboGridTextColorsPreset.ItemIndex];
  for dtc:=Low(Preset.TextColors) to High(Preset.TextColors) do begin
    FGridTextColors[dtc] := Preset.TextColors[dtc];
  end;
  comboGridTextColors.OnSelect(comboGridTextColors);
  if comboGridTextColorsPreset.ItemIndex > 0 then
    Modified(Sender);
end;


procedure TfrmPreferences.comboGridTextColorsSelect(Sender: TObject);
begin
  // Data type category selected
  colorboxGridTextColors.Selected := FGridTextColors[TDBDatatypeCategoryIndex(comboGridTextColors.ItemIndex)];
end;


procedure TfrmPreferences.comboGUIFontChange(Sender: TObject);
var
  UseCustomFont: Boolean;
begin
  // System font selected
  UseCustomFont := comboGUIFont.ItemIndex > 0;
  editGUIFontSize.Enabled := UseCustomFont;
  updownGUIFontSize.Enabled := UseCustomFont;
  lblGUIFontSize.Enabled := UseCustomFont;
  Modified(Sender);
end;


procedure TfrmPreferences.colorBoxGridTextColorsSelect(Sender: TObject);
begin
  // Color selected
  FGridTextColors[TDBDatatypeCategoryIndex(comboGridTextColors.ItemIndex)] := colorboxGridTextColors.Selected;
  Modified(Sender);
end;


procedure TfrmPreferences.comboSQLColElementChange(Sender: TObject);
var
  AttriIdx: Integer;
  Attri: TSynHighlighterAttributes;
  Foreground, Background: TColor;
begin
  AttriIdx := comboSQLColElement.ItemIndex;
  if AttriIdx = comboSQLColElement.Items.Count-1 then begin
    Foreground := MainForm.MatchingBraceForegroundColor;
    Background := MainForm.MatchingBraceBackgroundColor;
    chkSQLBold.Enabled := False;
    chkSQLItalic.Enabled := False;
  end else if AttriIdx = comboSQLColElement.Items.Count-2 then begin
    Foreground := SynMemoSQLSample.ActiveLineColor;
    Background := clNone;
    chkSQLBold.Enabled := False;
    chkSQLItalic.Enabled := False;
  end else begin
    Attri := SynSqlSynSQLSample.Attribute[AttriIdx];
    Foreground := Attri.Foreground;
    Background := Attri.Background;
    chkSQLBold.Enabled := True;
    chkSQLItalic.Enabled := True;
    chkSQLBold.OnClick := nil;
    chkSQLItalic.OnClick := nil;
    chkSQLBold.Checked := fsBold in Attri.Style;
    chkSQLItalic.Checked := fsItalic in Attri.Style;
    chkSQLBold.OnClick := SQLFontChange;
    chkSQLItalic.OnClick := SQLFontChange;
  end;
  cboxSQLColForeground.Selected := Foreground;
  cboxSQLColBackground.Selected := Background;
end;


procedure TfrmPreferences.comboThemeSelect(Sender: TObject);
begin
  // Select text colors so they fit to the selected theme
  if ThemeIsDark(comboTheme.Text) then begin
    comboGridTextColorsPreset.ItemIndex := comboGridTextColorsPreset.Items.IndexOf(_('Dark'));
    comboGridTextColorsPresetSelect(comboGridTextColorsPreset);
    comboEditorColorsPreset.ItemIndex := comboEditorColorsPreset.Items.IndexOf(_('Material'));
    comboEditorColorsPresetChange(comboEditorColorsPreset);
  end else begin
    comboGridTextColorsPreset.ItemIndex := comboGridTextColorsPreset.Items.IndexOf(_('Light'));
    comboGridTextColorsPresetSelect(comboGridTextColorsPreset);
    comboEditorColorsPreset.ItemIndex := comboEditorColorsPreset.Items.IndexOf(_('Light'));
    comboEditorColorsPresetChange(comboEditorColorsPreset);
  end;

  // Update preview window
  if chkThemePreview.Checked then begin
    FThemePreview.LoadTheme(comboTheme.Text);
  end;

  Modified(Sender);
end;


procedure TfrmPreferences.updownSQLFontSizeClick(Sender: TObject;
  Button: TUDBtnType);
begin
  SQLFontChange(Sender);
end;


{**
  Select attribute in pulldown by click into SynMemo
}
procedure TfrmPreferences.SynMemoSQLSampleClick(Sender: TObject);
var
  Token: UnicodeString;
  Attri: TSynHighlighterAttributes;
  AttriIdx: Integer;
  sm: TSynMemo;
begin
  sm := Sender as TSynMemo;
  sm.GetHighlighterAttriAtRowCol(sm.CaretXY, Token, Attri);
  if Attri = nil then
    Exit;
  AttriIdx := ComboSQLColElement.Items.IndexOf(Attri.FriendlyName);
  if AttriIdx = -1 then
    Exit;
  ComboSQLColElement.ItemIndex := AttriIdx;
  ComboSQLColElement.OnChange(Sender);
end;


procedure TfrmPreferences.btnRestoreDefaultsClick(Sender: TObject);
var
  ValueList: TStringlist;
  i: Integer;
begin
  // Restore defaults
  if MessageDialog(_('Reset all preference options to default values?'),
    _('This also applies to automatic settings, e.g. toolbar positions.'),
    mtConfirmation, [mbOK, mbCancel]) = mrCancel then
    Exit;
  AppSettings.ResetPath;
  ValueList := AppSettings.GetValueNames;
  for i:=0 to ValueList.Count-1 do
    AppSettings.DeleteValue(ValueList[i]);
  FormShow(Sender);
end;


procedure TfrmPreferences.chkThemePreviewClick(Sender: TObject);
begin
  // Show or hide theme preview window
  if chkThemePreview.Checked then begin
    FThemePreview := TfrmThemePreview.Create(chkThemePreview);
    FThemePreview.PopupMode := pmExplicit;
    FThemePreview.PopupParent := Self;
    FThemePreview.Show;
    FThemePreview.LoadTheme(comboTheme.Text);
  end else begin
    FThemePreview.Close;
  end;
end;

procedure TfrmPreferences.TreeShortcutItemsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  ShortcutFocused: Boolean;
  Data: PShortcutItemData;
begin
  // Shortcut item focus change in tree
  ShortcutFocused := Assigned(Node) and (Sender.GetNodeLevel(Node) = 1);
  lblShortcutHint.Enabled := ShortcutFocused;
  lblShortcut1.Enabled := ShortcutFocused;
  lblShortcut2.Enabled := ShortcutFocused;
  HotKey1.Enabled := lblShortcut1.Enabled;
  if ShortcutFocused then begin
    Data := Sender.GetNodeData(Node);
    lblShortcutHint.Caption := TreeShortcutItems.Text[Node, 0];
    if Assigned(Data.Action) then begin
      lblShortcut2.Enabled := False;
      if MainForm.ActionList1DefaultHints[Data.Action.Index] <> '' then
        lblShortcutHint.Caption := MainForm.ActionList1DefaultHints[Data.Action.Index];
    end;
    HotKey1.HotKey := Data.ShortCut1;
    HotKey2.HotKey := Data.ShortCut2;
  end;
  HotKey2.Enabled := lblShortcut2.Enabled;
end;


procedure TfrmPreferences.TreeShortcutItemsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  Data: PShortcutItemData;
begin
  // Fetch icon number of shortcut item
  if not (Kind in [ikNormal, ikSelected]) then Exit;
  if Sender.GetNodeLevel(Node) = 1 then begin
    Data := Sender.GetNodeData(Node);
    if Assigned(Data.KeyStroke) then
      ImageIndex := 114
    else if Assigned(Data.Action) then
      ImageIndex := Data.Action.ImageIndex;
  end;
end;


procedure TfrmPreferences.TreeShortcutItemsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TShortcutItemData);
end;


procedure TfrmPreferences.TreeShortcutItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data: PShortcutItemData;
  i: Integer;
  t: String;
begin
  // Fetch text of shortcut item
  case Sender.GetNodeLevel(Node) of
    0: CellText := FShortcutCategories[Node.Index];
    1: begin
      Data := Sender.GetNodeData(Node);
      if Assigned(Data.KeyStroke) then begin
        t := EditorCommandToCodeString(Data.KeyStroke.Command);
        t := Copy(t, 3, Length(t)-2);
        // Insert spaces before uppercase chars
        CellText := '';
        for i:=1 to Length(t) do begin
          if (i > 1) and (UpperCase(t[i]) = t[i]) then
            CellText := CellText + ' ';
          CellText := CellText + t[i];
        end;
        CellText := _(CellText);
      end else if Assigned(Data.Action) then begin
        CellText := MainForm.ActionList1DefaultCaptions[Data.Action.Index];
      end;
    end;
  end;
end;


procedure TfrmPreferences.TreeShortcutItemsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
  var ChildCount: Cardinal);
var
  i: Integer;
  Category: String;
begin
  // First initialization of shortcut items
  if Sender.GetNodeLevel(Node) = 0 then begin
    ChildCount := 0;
    if Integer(Node.Index) = FShortcutCategories.Count-1 then
      ChildCount := Mainform.SynMemoQuery.Keystrokes.Count
    else begin
      Category := (Sender as TVirtualStringTree).Text[Node, 0];
      for i:=0 to Mainform.ActionList1.ActionCount-1 do begin
        if Mainform.ActionList1.Actions[i].Category = Category then
          Inc(ChildCount);
      end;
    end;
  end;
end;


procedure TfrmPreferences.TreeShortcutItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PShortcutItemData;
  ItemIndex, i: Integer;
  Category: String;
begin
  if Sender.GetNodeLevel(Node) = 0 then
    Include(InitialStates, ivsHasChildren);
  Data := Sender.GetNodeData(Node);

  if Sender.GetNodeLevel(Node) = 1 then begin
    if Integer(Node.Parent.Index) = FShortcutCategories.Count-1 then begin
      Data^.KeyStroke := Mainform.SynMemoQuery.Keystrokes[Node.Index];
      Data^.Shortcut1 := Data.KeyStroke.ShortCut;
      Data^.Shortcut2 := Data.KeyStroke.ShortCut2;
    end else begin
      ItemIndex := -1;
      Category := (Sender as TVirtualStringTree).Text[Node.Parent, 0];
      for i:=0 to Mainform.ActionList1.ActionCount-1 do begin
        if Mainform.ActionList1.Actions[i].Category = Category then
          Inc(ItemIndex);
        if ItemIndex = Integer(Node.Index) then begin
          Data^.Action := TAction(Mainform.ActionList1.Actions[i]);
          Data^.Shortcut1 := Data.Action.ShortCut;
          break;
        end;
      end;
    end;
  end;
end;


function TfrmPreferences.EnsureShortcutIsUnused(RequestShortcut: TShortCut): Boolean;
var
  Node, NodeWantsIt: PVirtualNode;
  Data: PShortcutItemData;
  Tree: TVirtualStringTree;
  MsgFormat, Msg: String;
begin
  Result := True;
  if RequestShortcut = 0 then
    Exit;
  MsgFormat := _('Keyboard shortcut [%s] is already assigned to "%s".') + sLineBreak + sLineBreak +
    _('Remove it there and assign to "%s" instead?');
  Tree := TreeShortcutItems;
  NodeWantsIt := Tree.FocusedNode;
  Node := GetNextNode(Tree, nil, False);
  while Assigned(Node) do begin
    if Tree.GetNodeLevel(Node) = 1 then begin
      Data := Tree.GetNodeData(Node);
      Msg := Format(MsgFormat, [ShortCutToText(RequestShortcut), Tree.Text[Node, 0], Tree.Text[NodeWantsIt, 0]]);
      if Node = NodeWantsIt then begin
        // Ignore requesting node
      end else begin
        if Data.ShortCut1 = RequestShortcut then begin
          if MessageDialog(Msg, mtConfirmation, [mbYes, mbNo]) = mrYes then
            Data.ShortCut1 := 0 // Unassign shortcut 1
          else
            Result := False;
        end;
        if Data.ShortCut2 = RequestShortcut then begin
          if MessageDialog(Msg, mtConfirmation, [mbYes, mbNo]) = mrYes then
            Data.ShortCut2 := 0 // Unassign shortcut 2
          else
            Result := False;
        end;
      end;
    end;
    if Result = False then
      Break;
    Node := GetNextNode(Tree, Node, False);
  end;

end;


procedure TfrmPreferences.HotKeyChange(Sender: TObject);
var
  Data: PShortcutItemData;
  HotKeyEdit: THotKey;
  EventHandler: TNotifyEvent;
begin
  // Shortcut 1 or 2 changed
  HotKeyEdit := Sender as THotKey;
  Data := TreeShortcutItems.GetNodeData(TreeShortcutItems.FocusedNode);
  if EnsureShortcutIsUnused(HotKeyEdit.HotKey) then begin
    if HotKeyEdit = HotKey1 then
      Data.Shortcut1 := HotKeyEdit.HotKey
    else
      Data.Shortcut2 := HotKeyEdit.HotKey;
    Modified(Sender);
  end else begin
    // Undo change in hotkey editor, without triggering OnChange event
    EventHandler := HotKeyEdit.OnChange;
    if HotKeyEdit = HotKey1 then
      HotKeyEdit.HotKey := Data.ShortCut1
    else
      HotKeyEdit.HotKey := Data.ShortCut2;
    HotKeyEdit.OnChange := EventHandler;
  end;
end;


procedure TfrmPreferences.HotKeyEnter(Sender: TObject);
begin
  // Remove Esc and Enter shortcuts from buttons
  btnOk.Default := False;
  btnCancel.Cancel := False;
end;


procedure TfrmPreferences.HotKeyExit(Sender: TObject);
begin
  // Readd Esc and Enter shortcuts to buttons
  btnOk.Default := True;
  btnCancel.Cancel := True;
end;


procedure TfrmPreferences.InitLanguages;
var
  LangNames: String;
  AvailLangCodes: TStringList;
  i: Integer;

  procedure AddLang(LangCode: String);
  var
    LangName: String;
    rx: TRegExpr;
  begin
    rx := TRegExpr.Create;
    rx.Expression := '\b'+QuoteRegExprMetaChars(LangCode)+'\:([^#]+)';
    rx.ModifierI := True;
    if rx.Exec(LangNames) then
      LangName := rx.Match[1]
    else
      LangName := '';
    rx.Free;
    FLanguages.Add(LangCode + ': ' + LangName);
  end;

begin
  // Create list with present language code => language name
  // List taken from dxgettext/languagecodes.pas

  LangNames := 'aa:Afar#'+
    'aa:Afar#'+
    'ab:Abkhazian#'+
    'ae:Avestan#'+
    'af:Afrikaans#'+
    'ak:Akan#'+
    'am:Amharic#'+
    'an:Aragonese#'+
    'ar:Arabic#'+
    'as:Assamese#'+
    'av:Avaric#'+
    'ay:Aymara#'+
    'az:Azerbaijani#'+
    'ba:Bashkir#'+
    'be:Belarusian#'+
    'bg:Bulgarian#'+
    'bh:Bihari#'+
    'bi:Bislama#'+
    'bm:Bambara#'+
    'bn:Bengali#'+
    'bo:Tibetan#'+
    'br:Breton#'+
    'bs:Bosnian#'+
    'ca:Catalan#'+
    'ce:Chechen#'+
    'ch:Chamorro#'+
    'co:Corsican#'+
    'cr:Cree#'+
    'cs:Czech#'+
    'cv:Chuvash#'+
    'cy:Welsh#'+
    'da:Danish#'+
    'de:German#'+
    'de_AT:Austrian German#'+
    'de_CH:Swiss German#'+
    'dv:Divehi#'+
    'dz:Dzongkha#'+
    'ee:Ewe#'+
    'el:Greek#'+
    'en:English#'+
    'en_AU:Australian English#'+
    'en_CA:Canadian English#'+
    'en_GB:British English#'+
    'en_US:American English#'+
    'eo:Esperanto#'+
    'es:Spanish#'+
    'et:Estonian#'+
    'eu:Basque#'+
    'fa:Persian#'+
    'ff:Fulah#'+
    'fi:Finnish#'+
    'fj:Fijian#'+
    'fo:Faroese#'+
    'fr:French#'+
    'fr_BE:Walloon#'+
    'fy:Frisian#'+
    'ga:Irish#'+
    'gd:Gaelic#'+
    'gl:Gallegan#'+
    'gn:Guarani#'+
    'gu:Gujarati#'+
    'gv:Manx#'+
    'ha:Hausa#'+
    'he:Hebrew#'+
    'hi:Hindi#'+
    'ho:Hiri Motu#'+
    'hr:Croatian#'+
    'hr_HR:Croatian#'+ // Added, exists on Transifex
    'ht:Haitian#'+
    'hu:Hungarian#'+
    'hy:Armenian#'+
    'hz:Herero#'+
    'ia:Interlingua#'+
    'id:Indonesian#'+
    'ie:Interlingue#'+
    'ig:Igbo#'+
    'ii:Sichuan Yi#'+
    'ik:Inupiaq#'+
    'io:Ido#'+
    'is:Icelandic#'+
    'it:Italian#'+
    'iu:Inuktitut#'+
    'ja:Japanese#'+
    'jv:Javanese#'+
    'ka:Georgian#'+
    'kg:Kongo#'+
    'ki:Kikuyu#'+
    'kj:Kuanyama#'+
    'kk:Kazakh#'+
    'kl:Greenlandic#'+
    'km:Khmer#'+
    'kn:Kannada#'+
    'ko:Korean#'+
    'kr:Kanuri#'+
    'ks:Kashmiri#'+
    'ku:Kurdish#'+
    'kw:Cornish#'+
    'kv:Komi#'+
    'ky:Kirghiz#'+
    'la:Latin#'+
    'lb:Luxembourgish#'+
    'lg:Ganda#'+
    'li:Limburgan#'+
    'ln:Lingala#'+
    'lo:Lao#'+
    'lt:Lithuanian#'+
    'lu:Luba-Katanga#'+
    'lv:Latvian#'+
    'mg:Malagasy#'+
    'mh:Marshallese#'+
    'mi:Maori#'+
    'mk:Macedonian#'+
    'ml:Malayalam#'+
    'mn:Mongolian#'+
    'mo:Moldavian#'+
    'mr:Marathi#'+
    'ms:Malay#'+
    'mt:Maltese#'+
    'my:Burmese#'+
    'na:Nauru#'+
    'nb:Norwegian Bokmaal#'+
    'nd:Ndebele, North#'+
    'ne:Nepali#'+
    'ng:Ndonga#'+
    'nl:Dutch#'+
    'nl_BE:Flemish#'+
    'nn:Norwegian Nynorsk#'+
    'no:Norwegian#'+
    'nr:Ndebele, South#'+
    'nv:Navajo#'+
    'ny:Chichewa#'+
    'oc:Occitan#'+
    'oj:Ojibwa#'+
    'om:Oromo#'+
    'or:Oriya#'+
    'os:Ossetian#'+
    'pa:Panjabi#'+
    'pi:Pali#'+
    'pl:Polish#'+
    'ps:Pushto#'+
    'pt:Portuguese#'+
    'pt_BR:Brazilian Portuguese#'+
    'qu:Quechua#'+
    'rm:Raeto-Romance#'+
    'rn:Rundi#'+
    'ro:Romanian#'+
    'ru:Russian#'+
    'rw:Kinyarwanda#'+
    'sa:Sanskrit#'+
    'sc:Sardinian#'+
    'sd:Sindhi#'+
    'se:Northern Sami#'+
    'sg:Sango#'+
    'si:Sinhalese#'+
    'sk:Slovak#'+
    'sl:Slovenian#'+
    'sm:Samoan#'+
    'sn:Shona#'+
    'so:Somali#'+
    'sq:Albanian#'+
    'sr:Serbian#'+
    'ss:Swati#'+
    'st:Sotho, Southern#'+
    'su:Sundanese#'+
    'sv:Swedish#'+
    'sw:Swahili#'+
    'ta:Tamil#'+
    'te:Telugu#'+
    'tg:Tajik#'+
    'th:Thai#'+
    'ti:Tigrinya#'+
    'tk:Turkmen#'+
    'tl:Tagalog#'+
    'tn:Tswana#'+
    'to:Tonga#'+
    'tr:Turkish#'+
    'ts:Tsonga#'+
    'tt:Tatar#'+
    'tw:Twi#'+
    'ty:Tahitian#'+
    'ug:Uighur#'+
    'uk:Ukrainian#'+
    'ur:Urdu#'+
    'uz:Uzbek#'+
    've:Venda#'+
    'vi:Vietnamese#'+
    'vo:Volapuk#'+
    'wa:Walloon#'+
    'wo:Wolof#'+
    'xh:Xhosa#'+
    'yi:Yiddish#'+
    'yo:Yoruba#'+
    'za:Zhuang#'+
    'zh:Chinese (Simplified)#'+ // Added, see #498
    'zh_CN:Chinese (China)#'+
    'zh_TW:Chinese (Traditional)#'+
    'zu:Zulu#';

  FLanguages := TStringList.Create;
  AvailLangCodes := TStringList.Create;
  DefaultInstance.GetListOfLanguages('default', AvailLangCodes);
  for i:=0 to AvailLangCodes.Count-1 do begin
    AddLang(AvailLangCodes[i]);
  end;

  FLanguages.Sort;
  FLanguages.Insert(0, '*** '+f_('Auto detect (%s)', [SysLanguage]));

  AvailLangCodes.Free;
end;


end.
