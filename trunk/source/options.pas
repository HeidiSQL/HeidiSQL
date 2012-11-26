unit options;


// -------------------------------------
// Preferences
// -------------------------------------


interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, SynEditHighlighter, SynHighlighterSQL,
  SynEdit, SynMemo, VirtualTrees, SynEditKeyCmds, ActnList, SynEditMiscClasses, StdActns, Menus,
  mysql_structures, gnugettext;

type
  TShortcutItemData = record
    Action: TAction;
    KeyStroke: TSynEditKeyStroke;
    Shortcut1, Shortcut2: TShortcut;
  end;
  PShortcutItemData = ^TShortcutItemData;

  Toptionsform = class(TForm)
    pagecontrolMain: TPageControl;
    tabMisc: TTabSheet;
    btnCancel: TButton;
    btnOK: TButton;
    btnApply: TButton;
    tabSQL: TTabSheet;
    chkAutoReconnect: TCheckBox;
    tabData: TTabSheet;
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
    chkLogEventUserFiredSQL: TCheckBox;
    chkLogEventSQL: TCheckBox;
    chkLogEventInfo: TCheckBox;
    chkLogEventDebug: TCheckBox;
    editGridRowCountStep: TEdit;
    lblGridRowsLinecount: TLabel;
    editGridRowsLineCount: TEdit;
    updownGridRowsLineCount: TUpDown;
    chkColorBars: TCheckBox;
    cboxColorBars: TColorBox;
    tabHighlighter: TTabSheet;
    lblSQLColElement: TLabel;
    chkSQLBold: TCheckBox;
    chkSQLItalic: TCheckBox;
    comboSQLColElement: TComboBox;
    lblSQLColForeground: TLabel;
    lblSQLColBackground: TLabel;
    cboxSQLColBackground: TColorBox;
    cboxSQLColForeground: TColorBox;
    SynMemoSQLSample: TSynMemo;
    comboSQLFontName: TComboBox;
    lblFont: TLabel;
    editSQLFontSize: TEdit;
    updownSQLFontSize: TUpDown;
    lblSQLFontSize: TLabel;
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
    chkEditorBinary: TCheckBox;
    chkEditorDatetime: TCheckBox;
    chkEditorEnum: TCheckBox;
    chkEditorSet: TCheckBox;
    chkPrefillDateTime: TCheckBox;
    lblMySQLBinaries: TLabel;
    editMySQLBinaries: TButtonedEdit;
    chkRememberFilters: TCheckBox;
    lblLanguage: TLabel;
    comboAppLanguage: TComboBox;
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
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeShortcutItemsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure TreeShortcutItemsGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure Shortcut1Change(Sender: TObject);
    procedure Shortcut2Change(Sender: TObject);
    procedure ShortcutEnter(Sender: TObject);
    procedure ShortcutExit(Sender: TObject);
    procedure chkColorBarsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure comboGridTextColorsSelect(Sender: TObject);
    procedure colorBoxGridTextColorsSelect(Sender: TObject);
    procedure editMySQLBinariesRightButtonClick(Sender: TObject);
  private
    { Private declarations }
    FWasModified: Boolean;
    FShortcutCategories: TStringList;
    FGridTextColors: Array[dtcInteger..dtcOther] of TColor;
  public
    { Public declarations }
  end;


implementation
uses main, helpers;
{$R *.DFM}


procedure Toptionsform.Modified(Sender: TObject);
begin
  // Modified
  btnApply.Enabled := True;
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
  i, j: Integer;
  Attri: TSynHighlighterAttributes;
  Grid: TVirtualStringTree;
  CatNode, ItemNode: PVirtualNode;
  Data: PShortcutItemData;
  QueryTab: TQueryTab;
begin
  Screen.Cursor := crHourGlass;

  // Save values
  AppSettings.WriteBool(asAutoReconnect, chkAutoReconnect.Checked);
  AppSettings.WriteBool(asAllowMultipleInstances, chkAllowMultiInstances.Checked);
  AppSettings.WriteBool(asRestoreLastUsedDB, chkRestoreLastDB.Checked);
  AppSettings.WriteBool(asPromptSaveFileOnTabClose, chkAskFileSave.Checked);
  AppSettings.WriteString(asFontName, comboSQLFontName.Text);
  AppSettings.WriteInt(asFontSize, updownSQLFontSize.Position);
  AppSettings.WriteInt(asTabWidth, updownSQLTabWidth.Position);
  AppSettings.WriteInt(asLogsqlnum, updownLogLines.Position);
  AppSettings.WriteInt(asLogsqlwidth, updownLogSnip.Position);
  AppSettings.WriteString(asSessionLogsDirectory, editLogDir.Text);
  AppSettings.WriteBool(asLogErrors, chkLogEventErrors.Checked);
  AppSettings.WriteBool(asLogUserSQL, chkLogEventUserFiredSQL.Checked);
  AppSettings.WriteBool(asLogSQL, chkLogEventSQL.Checked);
  AppSettings.WriteBool(asLogInfos, chkLogEventInfo.Checked);
  AppSettings.WriteBool(asLogDebug, chkLogEventDebug.Checked);
  for i:=0 to SynSQLSynSQLSample.AttrCount - 1 do begin
    Attri := SynSQLSynSQLSample.Attribute[i];
    AppSettings.WriteInt(asHighlighterForeground, Attri.Foreground, Attri.FriendlyName);
    AppSettings.WriteInt(asHighlighterBackground, Attri.Background, Attri.FriendlyName);
    AppSettings.WriteInt(asHighlighterStyle, Attri.IntegerStyle, Attri.FriendlyName);
  end;
  AppSettings.WriteString(asSQLColActiveLine, ColorToString(SynMemoSQLSample.ActiveLineColor));

  AppSettings.WriteInt(asMaxColWidth, updownMaxColWidth.Position);
  AppSettings.WriteInt(asDatagridRowsPerStep, StrToIntDef(editGridRowCountStep.Text, -1));
  AppSettings.WriteInt(asDatagridMaximumRows, StrToIntDef(editGridRowCountMax.Text, -1));
  AppSettings.WriteInt(asGridRowLineCount, updownGridRowsLineCount.Position);
  AppSettings.WriteString(asDataFontName, comboDataFontName.Text);
  AppSettings.WriteInt(asDataFontSize, updownDataFontSize.Position);
  AppSettings.WriteBool(asLogToFile, chkLogToFile.Checked);
  AppSettings.WriteBool(asUpdatecheck, chkUpdatecheck.Checked);
  AppSettings.WriteBool(asUpdatecheckBuilds, chkUpdatecheckBuilds.Checked);
  AppSettings.WriteInt(asUpdatecheckInterval, updownUpdatecheckInterval.Position);
  AppSettings.WriteBool(asDoUsageStatistics, chkDoStatistics.Checked);
  AppSettings.WriteBool(asDisplayBars, chkColorBars.Checked);
  AppSettings.WriteInt(asBarColor, cboxColorBars.Selected);
  AppSettings.WriteString(asMySQLBinaries, editMySQLBinaries.Text);
  if comboAppLanguage.ItemIndex > 0 then
    AppSettings.WriteString(asAppLanguage, comboAppLanguage.Text)
  else
    AppSettings.WriteString(asAppLanguage, '');
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
  // Editor enablings
  AppSettings.WriteBool(asFieldEditorBinary, chkEditorBinary.Checked);
  AppSettings.WriteBool(asFieldEditorDatetime, chkEditorDatetime.Checked);
  AppSettings.WriteBool(asFieldEditorDatetimePrefill, chkPrefillDatetime.Checked);
  AppSettings.WriteBool(asFieldEditorEnum, chkEditorEnum.Checked);
  AppSettings.WriteBool(asFieldEditorSet, chkEditorSet.Checked);
  AppSettings.WriteBool(asRememberFilters, chkRememberFilters.Checked);

  AppSettings.WriteBool(asCompletionProposal, chkCompletionProposal.Checked);
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
  Mainform.DataGrid.Font.Name := comboDataFontName.Text;
  Mainform.DataGrid.Font.Size := updownDataFontSize.Position;
  FixVT(Mainform.DataGrid, updownGridRowsLineCount.Position);
  for i:=Mainform.tabQuery.PageIndex to Mainform.PageControlMain.PageCount-1 do begin
    QueryTab := Mainform.QueryTabs[i-Mainform.tabQuery.PageIndex];
    for j:=0 to QueryTab.ResultTabs.Count-1 do begin
      Grid := QueryTab.ResultTabs[j].Grid;
      Grid.Font.Name := comboDataFontName.Text;
      Grid.Font.Size := updownDataFontSize.Position;
      FixVT(Grid, updownGridRowsLineCount.Position);
    end;
  end;

  Mainform.LogToFile := chkLogToFile.Checked;
  DatatypeCategories[dtcInteger].Color := FGridTextColors[dtcInteger];
  DatatypeCategories[dtcReal].Color := FGridTextColors[dtcReal];
  DatatypeCategories[dtcText].Color := FGridTextColors[dtcText];
  DatatypeCategories[dtcBinary].Color := FGridTextColors[dtcBinary];
  DatatypeCategories[dtcTemporal].Color := FGridTextColors[dtcTemporal];
  DatatypeCategories[dtcSpatial].Color := FGridTextColors[dtcSpatial];
  DatatypeCategories[dtcOther].Color := FGridTextColors[dtcOther];
  Mainform.CalcNullColors;
  Mainform.DataGrid.Repaint;
  Mainform.QueryGrid.Repaint;
  Mainform.ListTables.Invalidate;
  Mainform.ListProcesses.Invalidate;
  Mainform.ListCommandStats.Invalidate;

  // Settings have been applied, send a signal to the user
  btnApply.Enabled := False;
  Screen.Cursor := crDefault;
end;


procedure Toptionsform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;


procedure Toptionsform.FormCreate(Sender: TObject);
var
  i: Integer;
  dtc: TDBDatatypeCategoryIndex;
  LanguageCodes: TStringList;
  // Callback function used by EnumFontFamilies()
  function EnumFixedProc(lpelf: PEnumLogFont; lpntm: PNewTextMetric; FontType: Integer; Data: LPARAM): Integer; stdcall;
  begin
    Result := 1;  // don't cancel
    if (lpelf^.elfLogFont.lfPitchAndFamily and FIXED_PITCH) <> 0 then
      (TStrings(Data)).Add(String(lpelf^.elfLogFont.lfFaceName));
  end;
begin
  TranslateComponent(Self);
  InheritFont(Font);

  // Misecllaneous
  // Hide browse button on Wine, as the browse dialog returns Windows-style paths, while we need a Unix path
  if MainForm.IsWine then begin
    editMySQLBinaries.RightButton.Visible := False;
    editMySQLBinaries.OnDblClick := nil;
  end;
  LanguageCodes := TStringList.Create;
  DefaultInstance.GetListOfLanguages('default', LanguageCodes);
  comboAppLanguage.Items.AddStrings(LanguageCodes);
  LanguageCodes.Free;

  // Data
  // Populate datatype categories pulldown
  for dtc:=Low(TDBDatatypeCategoryIndex) to High(TDBDatatypeCategoryIndex) do
    comboGridTextColors.Items.Add(DatatypeCategories[dtc].Name);

  // SQL
  EnumFontFamilies(Canvas.Handle, nil, @EnumFixedProc, LPARAM(Pointer(comboSQLFontName.Items)));
  comboSQLFontName.Sorted := True;
  SynMemoSQLSample.Text := 'SELECT DATE_SUB(NOW(), INTERVAL 1 DAY),' + CRLF +
    #9'''String literal'' AS lit' + CRLF +
    'FROM tableA AS ta -- A comment' + CRLF +
    'WHERE `columnA` IS NULL; # More comment' + CRLF +
    CRLF +
    'CREATE TABLE /*!32312 IF NOT EXISTS*/ tableB' + CRLF +
    #9'(id INT, name VARCHAR(30) DEFAULT "standard")';
  SynSQLSynSQLSample.TableNames.CommaText := 'tableA,tableB';
  for i:=0 to SynSQLSynSQLSample.AttrCount - 1 do begin
    SynSQLSynSQLSample.Attribute[i].AssignColorAndStyle(MainForm.SynSQLSyn1.Attribute[i]);
    comboSQLColElement.Items.Add(SynSQLSynSQLSample.Attribute[i].FriendlyName);
  end;
  comboSQLColElement.Items.Add(_('Active line background'));
  comboSQLColElement.ItemIndex := 0;

  // Shortcuts
  FShortcutCategories := TStringList.Create;
  for i:=0 to Mainform.ActionList1.ActionCount-1 do begin
    if FShortcutCategories.IndexOf(Mainform.ActionList1.Actions[i].Category) = -1 then
      FShortcutCategories.Add(Mainform.ActionList1.Actions[i].Category);
  end;
  FShortcutCategories.Add(_('SQL editing'));
  TreeShortcutItems.RootNodeCount := FShortcutCategories.Count;
end;


procedure Toptionsform.FormShow(Sender: TObject);
begin
  screen.Cursor := crHourGlass;

  // Read and display values
  chkAutoReconnect.Checked := AppSettings.ReadBool(asAutoReconnect);;
  chkAllowMultiInstances.Checked := AppSettings.ReadBool(asAllowMultipleInstances);
  chkRestoreLastDB.Checked := AppSettings.ReadBool(asRestoreLastUsedDB);
  chkUpdatecheck.Checked := AppSettings.ReadBool(asUpdatecheck);
  chkUpdatecheckBuilds.Checked := AppSettings.ReadBool(asUpdatecheckBuilds);
  updownUpdatecheckInterval.Position := AppSettings.ReadInt(asUpdatecheckInterval);
  chkUpdatecheckClick(Sender);
  chkDoStatistics.Checked := AppSettings.ReadBool(asDoUsageStatistics);
  chkColorBars.Checked := AppSettings.ReadBool(asDisplayBars);
  cboxColorBars.Selected := AppSettings.ReadInt(asBarColor);
  editMySQLBinaries.Text := AppSettings.ReadString(asMySQLBinaries);
  comboAppLanguage.ItemIndex := comboAppLanguage.Items.IndexOf(AppSettings.ReadString(asAppLanguage));
  if comboAppLanguage.ItemIndex = -1 then
    comboAppLanguage.ItemIndex := 0;
  chkAskFileSave.Checked := AppSettings.ReadBool(asPromptSaveFileOnTabClose);

  // Logging
  updownLogLines.Position := AppSettings.ReadInt(asLogsqlnum);
  updownLogSnip.Position := AppSettings.ReadInt(asLogsqlwidth);
  chkLogToFile.Checked := AppSettings.ReadBool(asLogToFile);
  editLogDir.Text := AppSettings.ReadString(asSessionLogsDirectory);
  chkLogEventErrors.Checked := AppSettings.ReadBool(asLogErrors);
  chkLogEventUserFiredSQL.Checked := AppSettings.ReadBool(asLogUserSQL);
  chkLogEventSQL.Checked := AppSettings.ReadBool(asLogSQL);
  chkLogEventInfo.Checked := AppSettings.ReadBool(asLogInfos);
  chkLogEventDebug.Checked := AppSettings.ReadBool(asLogDebug);

  // Default Column-Width in DBGrids:
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
  chkTabsToSpaces.Checked := AppSettings.ReadBool(asTabsToSpaces);
  comboSQLColElementChange(Sender);

  // Data-Appearance:
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
  comboGridTextColors.ItemIndex := 0;
  comboGridTextColors.OnSelect(comboGridTextColors);
  cboxNullBackground.Selected := AppSettings.ReadInt(asFieldNullBackground);
  // Editor enablings
  chkEditorBinary.Checked := AppSettings.ReadBool(asFieldEditorBinary);
  chkEditorDatetime.Checked := AppSettings.ReadBool(asFieldEditorDatetime);
  chkPrefillDateTime.Checked := AppSettings.ReadBool(asFieldEditorDatetimePrefill);
  chkEditorEnum.Checked := AppSettings.ReadBool(asFieldEditorEnum);
  chkEditorSet.Checked := AppSettings.ReadBool(asFieldEditorEnum);
  chkRememberFilters.Checked := AppSettings.ReadBool(asRememberFilters);

  // Shortcuts
  TreeShortcutItems.ReinitChildren(nil, True);
  SelectNode(TreeShortcutItems, nil);

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


procedure Toptionsform.editLogDirRightButtonClick(Sender: TObject);
var
  Browse: TBrowseForFolder;
begin
  // Select folder for session logs
  Browse := TBrowseForFolder.Create(Self);
  Browse.Folder := (Sender as TButtonedEdit).Text;
  Browse.DialogCaption := _('Select output directory');
  // Enable "Create new folder" button
  Browse.BrowseOptions := Browse.BrowseOptions - [bifNoNewFolderButton] + [bifNewDialogStyle];
  if Browse.Execute then begin
    (Sender as TButtonedEdit).Text := Browse.Folder;
    Modified(Sender);
  end;
  Browse.Free;
end;


procedure Toptionsform.editMySQLBinariesRightButtonClick(Sender: TObject);
var
  Browse: TBrowseForFolder;
begin
  // Select folder where MySQL binaries reside
  Browse := TBrowseForFolder.Create(Self);
  Browse.Folder := (Sender as TButtonedEdit).Text;
  Browse.DialogCaption := _('Find mysql.exe directory');
  Browse.BrowseOptions := Browse.BrowseOptions + [bifNewDialogStyle];
  if Browse.Execute then begin
    (Sender as TButtonedEdit).Text := Browse.Folder;
    Modified(Sender);
  end;
  Browse.Free;
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


procedure Toptionsform.chkColorBarsClick(Sender: TObject);
begin
  cboxColorBars.Enabled := (Sender as TCheckbox).Checked;
  Modified(Sender);
end;


procedure Toptionsform.chkLogToFileClick(Sender: TObject);
begin
  editLogDir.Enabled := TCheckBox(Sender).Checked;
  Modified(Sender);
end;


procedure Toptionsform.comboGridTextColorsSelect(Sender: TObject);
begin
  // Data type category selected
  colorboxGridTextColors.Selected := FGridTextColors[TDBDatatypeCategoryIndex(comboGridTextColors.ItemIndex)];
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
  // Factory defaults
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
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
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


end.
