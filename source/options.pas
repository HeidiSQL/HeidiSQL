unit options;


// -------------------------------------
// Preferences
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, SynEditHighlighter, SynHighlighterSQL,
  SynEdit, SynMemo, VirtualTrees, SynEditKeyCmds, ActnList, SynEditMiscClasses, StdActns, Menus,
  mysql_structures, gnugettext, Vcl.Themes, Vcl.Styles, SynRegExpr, Generics.Collections;

type
  TShortcutItemData = record
    Action: TAction;
    KeyStroke: TSynEditKeyStroke;
    Shortcut1, Shortcut2: TShortcut;
  end;
  PShortcutItemData = ^TShortcutItemData;

  // Color set for grid text, and preset class with a name
  TGridTextColors = Array[TDBDatatypeCategoryIndex] of TColor;
  TGridColorsPreset = class
    TextColors: TGridTextColors;
    Name: String;
  end;
  TGridColorsPresetList = TObjectList<TGridColorsPreset>;

  Toptionsform = class(TForm)
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
    Shortcut1: TSynHotKey;
    lblShortcut1: TLabel;
    lblShortcutHint: TLabel;
    Shortcut2: TSynHotKey;
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
    chkAskFileSave: TCheckBox;
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
    pnlDpiHelperGeneral: TPanel;
    pnlDpiHelperLogging: TPanel;
    pnlDpiHelperSql: TPanel;
    pnlDpiHelperGrid: TPanel;
    pnlDpiHelperData: TPanel;
    pnlDpiHelperShortcuts: TPanel;
    lblEditorColorsPreset: TLabel;
    comboEditorColorsPreset: TComboBox;
    lblSqlFontSize: TLabel;
    SynSQLSyn_Dark: TSynSQLSyn;
    SynSQLSyn_Light: TSynSQLSyn;
    SynSQLSyn_Black: TSynSQLSyn;
    SynSQLSyn_White: TSynSQLSyn;
    comboGridTextColorsPreset: TComboBox;
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
    procedure Shortcut1Change(Sender: TObject);
    procedure Shortcut2Change(Sender: TObject);
    procedure ShortcutEnter(Sender: TObject);
    procedure ShortcutExit(Sender: TObject);
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
  private
    { Private declarations }
    FWasModified: Boolean;
    FShortcutCategories: TStringList;
    FGridTextColors: TGridTextColors;
    FGridColorsPresets: TGridColorsPresetList;
    FLanguages: TStringList;
    FRestartOptionTouched: Boolean;
    FRestartOptionApplied: Boolean;
    procedure InitLanguages;
    procedure SelectDirectory(Sender: TObject; NewFolderButton: Boolean);
  public
    { Public declarations }
  end;


function EnumFixedProc(lpelf: PEnumLogFont; lpntm: PNewTextMetric; FontType: Integer; Data: LPARAM): Integer; stdcall;


implementation
uses main, apphelpers;
{$R *.DFM}


procedure Toptionsform.Modified(Sender: TObject);
begin
  // Modified
  btnApply.Enabled := True;
  if (Sender is TComponent) and (TComponent(Sender).Tag <> 0) then begin
    FRestartOptionTouched := True;
  end;
end;


procedure Toptionsform.pagecontrolMainChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  // Remember modification state. First tab switch leads TEdit's with TUpDown
  // to fire OnChange. Avoid enabling the buttons in that case.
  FWasModified := btnApply.Enabled;
end;


procedure Toptionsform.pagecontrolMainChange(Sender: TObject);
begin
  // See OnChanging procedure
  btnApply.Enabled := FWasModified;
end;


{**
  Apply settings to registry and mainform
}
procedure Toptionsform.Apply(Sender: TObject);
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
  AppSettings.WriteBool(asPromptSaveFileOnTabClose, chkAskFileSave.Checked);
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
    // There is no TStringList.Names[Value] getter, so we find the language code via loop
    LangCode := '';
    for i:=0 to FLanguages.Count-1 do begin
      if FLanguages.ValueFromIndex[i] = comboAppLanguage.Text then
        LangCode := FLanguages.Names[i];
    end;
    AppSettings.WriteString(asAppLanguage, LangCode);
  end else
    AppSettings.WriteString(asAppLanguage, '');
  if comboGUIFont.ItemIndex = 0 then
    AppSettings.WriteString(asGUIFontName, '')
  else
    AppSettings.WriteString(asGUIFontName, comboGUIFont.Text);
  AppSettings.WriteInt(asGUIFontSize, updownGUIFontSize.Position);
  AppSettings.WriteString(asTheme, comboTheme.Text);

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
  AppSettings.WriteBool(asHintsOnResultTabs, chkHintsOnResultTabs.Checked);

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

  // Set relevant properties in mainform
  MainForm.ApplyFontToGrids;

  TStyleManager.TrySetStyle(comboTheme.Text);
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


procedure Toptionsform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FRestartOptionApplied then begin
    MessageDialog(f_('You should restart %s to apply changed critical settings, and to prevent unexpected behaviour.', [APPNAME]),
      mtInformation,
      [mbOk]);
  end;
end;


procedure Toptionsform.FormCreate(Sender: TObject);
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
begin
  TranslateComponent(Self);

  // Misecllaneous
  // Hide browse button on Wine, as the browse dialog returns Windows-style paths, while we need a Unix path
  if MainForm.IsWine then begin
    editMySQLBinaries.RightButton.Visible := False;
    editMySQLBinaries.OnDblClick := nil;
  end;

  InitLanguages;
  for i:=0 to FLanguages.Count-1 do begin
    comboAppLanguage.Items.Add(FLanguages.ValueFromIndex[i]);
  end;

  comboGUIFont.Items.Assign(Screen.Fonts);
  comboGUIFont.Items.Insert(0, '<'+_('Default system font')+'>');

  Styles := TStyleManager.StyleNames;
  for i:=Low(Styles) to High(Styles) do begin
    comboTheme.Items.Add(Styles[i]);
  end;
  comboTheme.ItemIndex := comboTheme.Items.IndexOf(AppSettings.GetDefaultString(asTheme));

  // Data
  // Populate datatype categories pulldown
  for dtc:=Low(TDBDatatypeCategoryIndex) to High(TDBDatatypeCategoryIndex) do
    comboGridTextColors.Items.Add(DatatypeCategories[dtc].Name);

  // SQL
  EnumFontFamilies(Canvas.Handle, nil, @EnumFixedProc, LPARAM(Pointer(comboSQLFontName.Items)));
  comboSQLFontName.Sorted := True;
  SynMemoSQLSample.Text := SynMemoSQLSample.Highlighter.SampleSource;
  SynSQLSynSQLSample.TableNames.CommaText := 't,sample';
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
      Name := RegExprGetMatch('_([^_]+)$', Name, 1, False);
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


procedure Toptionsform.FormShow(Sender: TObject);
var
  LangCode, GUIFont: String;
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
  comboAppLanguage.ItemIndex := comboAppLanguage.Items.IndexOf(FLanguages.Values[LangCode]);
  GUIFont := AppSettings.ReadString(asGUIFontName);
  if GUIFont.IsEmpty then
    comboGUIFont.ItemIndex := 0
  else
    comboGUIFont.ItemIndex := comboGUIFont.Items.IndexOf(GUIFont);
  updownGUIFontSize.Position := AppSettings.ReadInt(asGUIFontSize);
  comboGUIFont.OnChange(comboGUIFont);
  comboTheme.ItemIndex := comboTheme.Items.IndexOf(AppSettings.ReadString(asTheme));
  chkAskFileSave.Checked := AppSettings.ReadBool(asPromptSaveFileOnTabClose);
  chkQueryWarningsMessage.Checked := AppSettings.ReadBool(asQueryWarningsMessage);

  // Logging
  updownLogLines.Position := AppSettings.ReadInt(asLogsqlnum);
  updownLogSnip.Position := AppSettings.ReadInt(asLogsqlwidth);
  chkLogToFile.Checked := AppSettings.ReadBool(asLogToFile);
  editLogDir.Text := AppSettings.ReadString(asSessionLogsDirectory);
  chkLogEventErrors.Checked := AppSettings.ReadBool(asLogErrors);
  chkLogEventUserGeneratedSQL.Checked := AppSettings.ReadBool(asLogUserSQL);
  chkLogEventSQL.Checked := AppSettings.ReadBool(asLogSQL);
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
  chkHintsOnResultTabs.Checked := AppSettings.ReadBool(asHintsOnResultTabs);

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

  FRestartOptionTouched := False;
  btnApply.Enabled := False;
  screen.Cursor := crdefault;
end;



procedure Toptionsform.SQLFontChange(Sender: TObject);
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


procedure Toptionsform.DataFontsChange(Sender: TObject);
begin
  Modified(Sender);
end;

procedure Toptionsform.anyUpDownLimitChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  Modified(Sender);
end;


procedure Toptionsform.editGridRowCountExit(Sender: TObject);
var
  Edit: TEdit;
begin
  // Row count step and maximum shall never be "0", to avoid problems in
  // data grids. See issue #3080.
  Edit := Sender as TEdit;
  if MakeInt(Edit.Text) <= 0 then
    Edit.Text := '1';
end;


procedure Toptionsform.SelectDirectory(Sender: TObject; NewFolderButton: Boolean);
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


procedure Toptionsform.editLogDirRightButtonClick(Sender: TObject);
begin
  // Select folder for session logs
  SelectDirectory(Sender, True);
end;


procedure Toptionsform.editMySQLBinariesRightButtonClick(Sender: TObject);
begin
  // Select folder where MySQL binaries reside
  SelectDirectory(Sender, False);
end;


procedure Toptionsform.editCustomSnippetsDirectoryRightButtonClick(Sender: TObject);
begin
  // Set custom snippets directory
  SelectDirectory(Sender, True);
end;


{**
  Updatecheck checkbox was clicked
}
procedure Toptionsform.chkUpdatecheckClick(Sender: TObject);
begin
  updownUpdatecheckInterval.Enabled := chkUpdatecheck.Checked;
  editUpdatecheckInterval.Enabled := chkUpdatecheck.Checked;
  chkUpdatecheckBuilds.Enabled := chkUpdatecheck.Checked;
  Modified(Sender);
end;


procedure Toptionsform.chkLogToFileClick(Sender: TObject);
begin
  editLogDir.Enabled := TCheckBox(Sender).Checked;
  Modified(Sender);
end;


procedure Toptionsform.chkQueryHistoryClick(Sender: TObject);
begin
  editQueryHistoryKeepDays.Enabled := chkQueryHistory.Checked;
  updownQueryHistoryKeepDays.Enabled := chkQueryHistory.Checked;
  lblQueryHistoryKeepDays.Enabled := chkQueryHistory.Checked;
  Modified(Sender);
end;


procedure Toptionsform.comboEditorColorsPresetChange(Sender: TObject);
var
  i, j: Integer;
  Highlighter: TSynSQLSyn;
  FoundHighlighter: Boolean;
begin
  // Color preset selected
  FoundHighlighter := False;
  for i:=0 to ComponentCount-1 do begin
    if (Components[i] is TSynSQLSyn)
      and (Components[i] <> SynMemoSQLSample.Highlighter)
      then begin
      Highlighter := Components[i] as TSynSQLSyn;
      if SynRegExpr.ExecRegExpr('[a-zA-Z]+_'+comboEditorColorsPreset.Text, Highlighter.Name) then begin
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


procedure Toptionsform.comboGridTextColorsPresetSelect(Sender: TObject);
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


procedure Toptionsform.comboGridTextColorsSelect(Sender: TObject);
begin
  // Data type category selected
  colorboxGridTextColors.Selected := FGridTextColors[TDBDatatypeCategoryIndex(comboGridTextColors.ItemIndex)];
end;


procedure Toptionsform.comboGUIFontChange(Sender: TObject);
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


procedure Toptionsform.colorBoxGridTextColorsSelect(Sender: TObject);
begin
  // Color selected
  FGridTextColors[TDBDatatypeCategoryIndex(comboGridTextColors.ItemIndex)] := colorboxGridTextColors.Selected;
  Modified(Sender);
end;


procedure Toptionsform.comboSQLColElementChange(Sender: TObject);
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


procedure Toptionsform.updownSQLFontSizeClick(Sender: TObject;
  Button: TUDBtnType);
begin
  SQLFontChange(Sender);
end;


{**
  Select attribute in pulldown by click into SynMemo
}
procedure Toptionsform.SynMemoSQLSampleClick(Sender: TObject);
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


procedure Toptionsform.btnRestoreDefaultsClick(Sender: TObject);
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


procedure Toptionsform.TreeShortcutItemsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
  Shortcut1.Enabled := lblShortcut1.Enabled;
  if ShortcutFocused then begin
    Data := Sender.GetNodeData(Node);
    lblShortcutHint.Caption := TreeShortcutItems.Text[Node, 0];
    if Assigned(Data.Action) then begin
      lblShortcut2.Enabled := False;
      if Data.Action.Hint <> '' then
        lblShortcutHint.Caption := Data.Action.Hint;
    end;
    Shortcut1.HotKey := Data.ShortCut1;
    Shortcut2.HotKey := Data.ShortCut2;
  end;
  Shortcut2.Enabled := lblShortcut2.Enabled;
end;


procedure Toptionsform.TreeShortcutItemsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
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


procedure Toptionsform.TreeShortcutItemsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TShortcutItemData);
end;


procedure Toptionsform.TreeShortcutItemsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
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
        CellText := Data.Action.Caption;
        CellText := StripHotkey(CellText);
      end;
    end;
  end;
end;


procedure Toptionsform.TreeShortcutItemsInitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
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


procedure Toptionsform.TreeShortcutItemsInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
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


procedure Toptionsform.Shortcut1Change(Sender: TObject);
var
  Data: PShortcutItemData;
begin
  // Shortcut 1 changed
  Data := TreeShortcutItems.GetNodeData(TreeShortcutItems.FocusedNode);
  Data.Shortcut1 := (Sender as TSynHotKey).HotKey;
  Modified(Sender);
end;


procedure Toptionsform.Shortcut2Change(Sender: TObject);
var
  Data: PShortcutItemData;
begin
  // Shortcut 2 changed
  Data := TreeShortcutItems.GetNodeData(TreeShortcutItems.FocusedNode);
  Data.Shortcut2 := (Sender as TSynHotKey).HotKey;
  Modified(Sender);
end;


procedure Toptionsform.ShortcutEnter(Sender: TObject);
begin
  // Remove Esc and Enter shortcuts from buttons
  btnOk.Default := False;
  btnCancel.Cancel := False;
end;


procedure Toptionsform.ShortcutExit(Sender: TObject);
begin
  // Readd Esc and Enter shortcuts to buttons
  btnOk.Default := True;
  btnCancel.Cancel := True;
end;


procedure Toptionsform.InitLanguages;
var
  AvailLangCodes: TStringList;

  procedure AddLang(LangCode, LangName: String);
  begin
    LangCode := LowerCase(LangCode);
    if AvailLangCodes.IndexOf(LangCode) > -1 then
      FLanguages.Add(LangCode + FLanguages.NameValueSeparator + LangName);
  end;

begin
  // Create list with present language code => language name
  // List taken from dxgettext/languagecodes.pas
  FLanguages := TStringList.Create;
  FLanguages.Add('' + FLanguages.NameValueSeparator + _('Auto detect'));
  AvailLangCodes := TStringList.Create;
  DefaultInstance.GetListOfLanguages('default', AvailLangCodes);
  AddLang('aa', 'Afar');
  AddLang('aa', 'Afar');
  AddLang('ab', 'Abkhazian');
  AddLang('ae', 'Avestan');
  AddLang('af', 'Afrikaans');
  AddLang('ak', 'Akan');
  AddLang('am', 'Amharic');
  AddLang('an', 'Aragonese');
  AddLang('ar', 'Arabic');
  AddLang('as', 'Assamese');
  AddLang('av', 'Avaric');
  AddLang('ay', 'Aymara');
  AddLang('az', 'Azerbaijani');
  AddLang('ba', 'Bashkir');
  AddLang('be', 'Belarusian');
  AddLang('bg', 'Bulgarian');
  AddLang('bh', 'Bihari');
  AddLang('bi', 'Bislama');
  AddLang('bm', 'Bambara');
  AddLang('bn', 'Bengali');
  AddLang('bo', 'Tibetan');
  AddLang('br', 'Breton');
  AddLang('bs', 'Bosnian');
  AddLang('ca', 'Catalan');
  AddLang('ce', 'Chechen');
  AddLang('ch', 'Chamorro');
  AddLang('co', 'Corsican');
  AddLang('cr', 'Cree');
  AddLang('cs', 'Czech');
  AddLang('cv', 'Chuvash');
  AddLang('cy', 'Welsh');
  AddLang('da', 'Danish');
  AddLang('de', 'German');
  AddLang('de_AT', 'Austrian German');
  AddLang('de_CH', 'Swiss German');
  AddLang('dv', 'Divehi');
  AddLang('dz', 'Dzongkha');
  AddLang('ee', 'Ewe');
  AddLang('el', 'Greek');
  AddLang('en', 'English');
  AddLang('en_AU', 'Australian English');
  AddLang('en_CA', 'Canadian English');
  AddLang('en_GB', 'British English');
  AddLang('en_US', 'American English');
  AddLang('eo', 'Esperanto');
  AddLang('es', 'Spanish');
  AddLang('et', 'Estonian');
  AddLang('eu', 'Basque');
  AddLang('fa', 'Persian');
  AddLang('ff', 'Fulah');
  AddLang('fi', 'Finnish');
  AddLang('fj', 'Fijian');
  AddLang('fo', 'Faroese');
  AddLang('fr', 'French');
  AddLang('fr_BE', 'Walloon');
  AddLang('fy', 'Frisian');
  AddLang('ga', 'Irish');
  AddLang('gd', 'Gaelic');
  AddLang('gl', 'Gallegan');
  AddLang('gn', 'Guarani');
  AddLang('gu', 'Gujarati');
  AddLang('gv', 'Manx');
  AddLang('ha', 'Hausa');
  AddLang('he', 'Hebrew');
  AddLang('hi', 'Hindi');
  AddLang('ho', 'Hiri Motu');
  AddLang('hr', 'Croatian');
  AddLang('ht', 'Haitian');
  AddLang('hu', 'Hungarian');
  AddLang('hy', 'Armenian');
  AddLang('hz', 'Herero');
  AddLang('ia', 'Interlingua');
  AddLang('id', 'Indonesian');
  AddLang('ie', 'Interlingue');
  AddLang('ig', 'Igbo');
  AddLang('ii', 'Sichuan Yi');
  AddLang('ik', 'Inupiaq');
  AddLang('io', 'Ido');
  AddLang('is', 'Icelandic');
  AddLang('it', 'Italian');
  AddLang('iu', 'Inuktitut');
  AddLang('ja', 'Japanese');
  AddLang('jv', 'Javanese');
  AddLang('ka', 'Georgian');
  AddLang('kg', 'Kongo');
  AddLang('ki', 'Kikuyu');
  AddLang('kj', 'Kuanyama');
  AddLang('kk', 'Kazakh');
  AddLang('kl', 'Greenlandic');
  AddLang('km', 'Khmer');
  AddLang('kn', 'Kannada');
  AddLang('ko', 'Korean');
  AddLang('kr', 'Kanuri');
  AddLang('ks', 'Kashmiri');
  AddLang('ku', 'Kurdish');
  AddLang('kw', 'Cornish');
  AddLang('kv', 'Komi');
  AddLang('ky', 'Kirghiz');
  AddLang('la', 'Latin');
  AddLang('lb', 'Luxembourgish');
  AddLang('lg', 'Ganda');
  AddLang('li', 'Limburgan');
  AddLang('ln', 'Lingala');
  AddLang('lo', 'Lao');
  AddLang('lt', 'Lithuanian');
  AddLang('lu', 'Luba-Katanga');
  AddLang('lv', 'Latvian');
  AddLang('mg', 'Malagasy');
  AddLang('mh', 'Marshallese');
  AddLang('mi', 'Maori');
  AddLang('mk', 'Macedonian');
  AddLang('ml', 'Malayalam');
  AddLang('mn', 'Mongolian');
  AddLang('mo', 'Moldavian');
  AddLang('mr', 'Marathi');
  AddLang('ms', 'Malay');
  AddLang('mt', 'Maltese');
  AddLang('my', 'Burmese');
  AddLang('na', 'Nauru');
  AddLang('nb', 'Norwegian Bokmaal');
  AddLang('nd', 'Ndebele, North');
  AddLang('ne', 'Nepali');
  AddLang('ng', 'Ndonga');
  AddLang('nl', 'Dutch');
  AddLang('nl_BE', 'Flemish');
  AddLang('nn', 'Norwegian Nynorsk');
  AddLang('no', 'Norwegian');
  AddLang('nr', 'Ndebele, South');
  AddLang('nv', 'Navajo');
  AddLang('ny', 'Chichewa');
  AddLang('oc', 'Occitan');
  AddLang('oj', 'Ojibwa');
  AddLang('om', 'Oromo');
  AddLang('or', 'Oriya');
  AddLang('os', 'Ossetian');
  AddLang('pa', 'Panjabi');
  AddLang('pi', 'Pali');
  AddLang('pl', 'Polish');
  AddLang('ps', 'Pushto');
  AddLang('pt', 'Portuguese');
  AddLang('pt_BR', 'Brazilian Portuguese');
  AddLang('qu', 'Quechua');
  AddLang('rm', 'Raeto-Romance');
  AddLang('rn', 'Rundi');
  AddLang('ro', 'Romanian');
  AddLang('ru', 'Russian');
  AddLang('rw', 'Kinyarwanda');
  AddLang('sa', 'Sanskrit');
  AddLang('sc', 'Sardinian');
  AddLang('sd', 'Sindhi');
  AddLang('se', 'Northern Sami');
  AddLang('sg', 'Sango');
  AddLang('si', 'Sinhalese');
  AddLang('sk', 'Slovak');
  AddLang('sl', 'Slovenian');
  AddLang('sm', 'Samoan');
  AddLang('sn', 'Shona');
  AddLang('so', 'Somali');
  AddLang('sq', 'Albanian');
  AddLang('sr', 'Serbian');
  AddLang('ss', 'Swati');
  AddLang('st', 'Sotho, Southern');
  AddLang('su', 'Sundanese');
  AddLang('sv', 'Swedish');
  AddLang('sw', 'Swahili');
  AddLang('ta', 'Tamil');
  AddLang('te', 'Telugu');
  AddLang('tg', 'Tajik');
  AddLang('th', 'Thai');
  AddLang('ti', 'Tigrinya');
  AddLang('tk', 'Turkmen');
  AddLang('tl', 'Tagalog');
  AddLang('tn', 'Tswana');
  AddLang('to', 'Tonga');
  AddLang('tr', 'Turkish');
  AddLang('ts', 'Tsonga');
  AddLang('tt', 'Tatar');
  AddLang('tw', 'Twi');
  AddLang('ty', 'Tahitian');
  AddLang('ug', 'Uighur');
  AddLang('uk', 'Ukrainian');
  AddLang('ur', 'Urdu');
  AddLang('uz', 'Uzbek');
  AddLang('ve', 'Venda');
  AddLang('vi', 'Vietnamese');
  AddLang('vo', 'Volapuk');
  AddLang('wa', 'Walloon');
  AddLang('wo', 'Wolof');
  AddLang('xh', 'Xhosa');
  AddLang('yi', 'Yiddish');
  AddLang('yo', 'Yoruba');
  AddLang('za', 'Zhuang');
  AddLang('zh', 'Chinese (Simplified)');
  AddLang('zh_TW', 'Chinese (Traditional)');
  AddLang('zu', 'Zulu');
  AvailLangCodes.Free;
end;


end.
