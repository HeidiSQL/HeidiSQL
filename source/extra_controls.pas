unit extra_controls;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Types, StdCtrls, Clipbrd, apphelpers,
  Graphics, Dialogs, ImgList, ComCtrls, Generics.Collections, Generics.Defaults,
  ExtCtrls, laz.VirtualTrees, RegExpr, Controls, EditBtn, Menus,
  GraphUtil, Math;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar

  { TControlTopComparer }

  TControlTopComparer = class(TComparer<TControl>)
    function Compare(constref Left, Right: TControl): Integer; override;
  end;

  TControlRow = class(TObjectList<TControl>)
    Height: Integer
  end;

  { TControlGrid }

  TControlGrid = class(TObjectList<TControlRow>)
    public
      constructor Create(aParentControl: TControl);
  end;

  { TExtForm }

  TExtForm = class(TForm)
    private
      //FSizeGrip: TSizeGripXP;
      FPixelsPerInchDesigned: Integer;
      function GetHasSizeGrip: Boolean;
      procedure SetHasSizeGrip(Value: Boolean);
    protected
      procedure DoShow; override;
      //procedure DoBeforeMonitorDpiChanged(OldDPI, NewDPI: Integer); override;
      //procedure DoAfterMonitorDpiChanged(OldDPI, NewDPI: Integer); override;
      procedure FilterNodesByEdit(Edit: TEditButton; Tree: TLazVirtualStringTree);
      procedure ArrangeControls(aParentControl: TControl);
    public
      constructor Create(AOwner: TComponent); override;
      class procedure InheritFont(AFont: TFont);
      property HasSizeGrip: Boolean read GetHasSizeGrip write SetHasSizeGrip default False;
      class procedure FixControls(ParentComp: TComponent);
      class procedure SaveListSetup(List: TLazVirtualStringTree);
      class procedure RestoreListSetup(List: TLazVirtualStringTree);
      function ScaleSize(x: Extended): Integer; overload;
      class function ScaleSize(x: Extended; Control: TControl): Integer; overload;
      class procedure PageControlTabHighlight(PageControl: TPageControl);
      property PixelsPerInchDesigned: Integer read FPixelsPerInchDesigned;
      procedure ShowPopup(ClickedControl: TControl; PopupMenu: TPopupMenu);
  end;

  // Modern file-open-dialog with high DPI support and encoding selector
  TExtFileOpenDialog = class(TOpenDialog)
    private
      FFilters: TStringList;
      FEncodings: TStringList;
      FEncodingIndex: Cardinal;
      const idEncodingCombo = 1;
      procedure FileOkClickNoOp(Sender: TObject; var CanClose: Boolean);
    protected
      //procedure DoOnExecute; override;
      //function DoOnFileOkClick: Boolean; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure AddFileType(FileMask, DisplayName: String);
      property Encodings: TStringList read FEncodings write FEncodings;
      property EncodingIndex: Cardinal read FEncodingIndex write FEncodingIndex;
  end;

  {TExtFileSaveDialog = class(TFileSaveDialog)
    private
      FLineBreaks: TStringList;
      FLineBreakIndex: TLineBreaks;
      const idLineBreakCombo = 1;
      procedure FileOkClickNoOp(Sender: TObject; var CanClose: Boolean);
    protected
      procedure DoOnExecute; override;
      function DoOnFileOkClick: Boolean; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure AddFileType(FileMask, DisplayName: String);
      property LineBreaks: TStringList read FLineBreaks;
      property LineBreakIndex: TLineBreaks read FLineBreakIndex write FLineBreakIndex;
  end;}

  {TExtSynHotKey = class(TSynHotKey)
    private
      FOnChange: TNotifyEvent;
      FOnEnter: TNotifyEvent;
      FOnExit: TNotifyEvent;
      procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
      procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    protected
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure Paint; override;
    published
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
      property OnExit: TNotifyEvent read FOnExit write FOnExit;
  end;}

  {TExtComboBox = class(TComboBox)
    private
      FcbHintIndex: Integer;
      FHintWindow: THintWindow;
    protected
      procedure Change; override;
      procedure DropDown; override;
      procedure CloseUp; override;
      procedure InitiateAction; override;
  end;}

  {TExtHintWindow = class(THintWindow)
    private
      const Padding: Integer = 8;
    protected
      procedure Paint; override;
    public
      function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect; override;
  end;}


implementation

{ TControlComparer }

function TControlTopComparer.Compare(constref Left, Right: TControl): Integer;
begin
  // Sort by Top position, so we can be sure if we find a first one in a row, there's no other above it
  if Left.Top > Right.Top then
    Result := 1
  else if Left.Top = Right.Top then
    Result := 0
  else
    Result := -1;
end;

{ TControlGrid }

constructor TControlGrid.Create(aParentControl: TControl);
const
  yCoordStep = 10;
var
  y, i: Integer;
  FoundControl: TControl;
  ControlRow: TControlRow;
  AllControls: TObjectList<TControl>;
begin
  OwnsObjects := True;
  y := 0;
  AllControls := TObjectList<TControl>.Create(TControlTopComparer.Create, False);
  for i:=0 to aParentControl.Owner.ComponentCount-1 do begin
    if aParentControl.Owner.Components[i] is TControl then begin
      FoundControl := aParentControl.Owner.Components[i] as TControl;
      if FoundControl.Parent = aParentControl then begin
        AllControls.Add(FoundControl);
      end;
    end;
  end;
  AllControls.Sort;

  while y < aParentControl.Height + 100 do begin
    ControlRow := TControlRow.Create(False);
    ControlRow.Height := 0;

    for FoundControl in AllControls do begin
      if (FoundControl.Top >= y) and (FoundControl.Top < y + yCoordStep) then begin
        ControlRow.Add(FoundControl);
        if ControlRow.Count = 1 then
          y := FoundControl.Top;
        ControlRow.Height := Max(ControlRow.Height, FoundControl.Height);
      end;
    end;

    // Add to grid if controls exist
    if ControlRow.Count = 0 then begin
      ControlRow.Free;
      Inc(y, 1);
    end
    else begin
      ControlRow.Height := aParentControl.ScaleDesignToForm(ControlRow.Height);
      Add(ControlRow);
      Inc(y, yCoordStep);
    end;

  end;
end;


{ TExtForm }

constructor TExtForm.Create(AOwner: TComponent);
var
  OldImageList: TCustomImageList;
begin
  inherited;

  FPixelsPerInchDesigned := DesignTimePPI;
  //InheritFont(Font);
  HasSizeGrip := False;

  // Reduce flicker on Windows 10
  // See https://www.heidisql.com/forum.php?t=19141
  //if CheckWin32Version(6, 2) then begin
  DoubleBuffered := True;
  //end;

  // Translation and related fixes
  // Issue #557: Apply images *after* translating main menu, so top items don't get unused
  // space left besides them.
  if (Menu <> nil) and (Menu.Images <> nil) then begin
    OldImageList := Menu.Images;
    Menu.Images := nil;
    //TranslateComponent(Self);
    Menu.Images := OldImageList;
  end else begin
    //TranslateComponent(Self);
  end;

end;


procedure TExtForm.DoShow;
begin
  // No need to fix anything
  FixControls(Self);
  inherited;
end;


{procedure TExtForm.DoBeforeMonitorDpiChanged(OldDPI, NewDPI: Integer);
begin
  // Reduce flicker
  inherited;
  LockWindowUpdate(Handle);
end;}

{procedure TExtForm.DoAfterMonitorDpiChanged(OldDPI, NewDPI: Integer);
begin
  // Release window updates
  LockWindowUpdate(0);
  inherited;
end;}


class procedure TExtForm.FixControls(ParentComp: TComponent);
var
  i: Integer;

  {procedure ProcessSingleComponent(Cmp: TComponent);
  begin
    if (Cmp is TButton) and (TButton(Cmp).Style = bsSplitButton) then begin
      // Work around broken dropdown (tool)button on Wine after translation:
      // https://sourceforge.net/p/dxgettext/bugs/80/
      TButton(Cmp).Style := bsPushButton;
      TButton(Cmp).Style := bsSplitButton;
    end;
    if (Cmp is TToolButton) and (TToolButton(Cmp).Style = tbsDropDown) then begin
      // similar fix as above
      TToolButton(Cmp).Style := tbsButton;
      TToolButton(Cmp).Style := tbsDropDown;
    end;
  end;}
begin
  // Passed component itself may also be some control to be fixed
  // e.g. TInplaceEditorLink.MainControl
  {ProcessSingleComponent(ParentComp);
  for i:=0 to ParentComp.ComponentCount-1 do begin
    ProcessSingleComponent(ParentComp.Components[i]);
  end;}
end;


function TExtForm.GetHasSizeGrip: Boolean;
begin
  //Result := FSizeGrip <> nil;
  Result := False;
end;


procedure TExtForm.SetHasSizeGrip(Value: Boolean);
begin
  {if Value then begin
    FSizeGrip := TSizeGripXP.Create(Self);
    FSizeGrip.Enabled := True;
  end else begin
    if FSizeGrip <> nil then
      FreeAndNil(FSizeGrip);
  end;}
end;


class procedure TExtForm.InheritFont(AFont: TFont);
var
  GUIFontName: String;
begin
  // Set custom font if set, or default system font.
  // In high-dpi mode, the font *size* is increased automatically somewhere in the VCL,
  // caused by a form's .Scaled property. So we don't increase it here again.
  // To test this, you really need to log off/on Windows!
  GUIFontName := AppSettings.ReadString(asGUIFontName);
  if not GUIFontName.IsEmpty then begin
    // Apply user specified font
    AFont.Name := GUIFontName;
    // Set size on top of automatic dpi-increased size
    AFont.Size := AppSettings.ReadInt(asGUIFontSize);
  end else begin
    // Apply system font. See issue #3204.
    AFont.Orientation := Screen.SystemFont.Orientation;
    AFont.CharSet := Screen.SystemFont.CharSet;
    AFont.Name := Screen.SystemFont.Name;
    AFont.Pitch := Screen.SystemFont.Pitch;
  end;
end;


{**
  Save setup of a VirtualStringTree to registry
}
class procedure TExtForm.SaveListSetup( List: TLazVirtualStringTree);
var
  i: Integer;
  ColWidth: Int64;
  ColWidths, ColsVisible, ColPos, Regname: String;
  OwnerForm: TWinControl;
begin
  // Prevent sporadic crash on startup
  if List = nil then
    Exit;
  OwnerForm := GetParentFormOrFrame(List);
  // On a windows shutdown, GetParentForm() seems sporadically unable to find the owner form
  // In that case we would cause an exception when accessing it. Emergency break in that case.
  // See issue #1462
  // TODO: Test this, probably fixed by implementing GetParentFormOrFrame, and then again, probably not.
  if not Assigned(OwnerForm) then
    Exit;

  ColWidths := '';
  ColsVisible := '';
  ColPos := '';

  for i := 0 to List.Header.Columns.Count - 1 do
  begin
    // Column widths
    if ColWidths <> '' then
      ColWidths := ColWidths + ',';
    ColWidth := List.Header.Columns[i].Width; // RoundCommercial(List.Header.Columns[i].Width / OwnerForm.ScaleFactor);
    ColWidths := ColWidths + IntToStr(ColWidth);

    // Column visibility
    if coVisible in List.Header.Columns[i].Options then
    begin
      if ColsVisible <> '' then
        ColsVisible := ColsVisible + ',';
      ColsVisible := ColsVisible + IntToStr(i);
    end;

    // Column position
    if ColPos <> '' then
      ColPos := ColPos + ',';
    ColPos := ColPos + IntToStr(List.Header.Columns[i].Position);

  end;

  // Lists can have the same name over different forms or frames. Find parent form or frame,
  // so we can prepend its name into the registry value name.
  Regname := OwnerForm.Name + '.' + List.Name;
  AppSettings.ResetPath;
  AppSettings.WriteString(asListColWidths, ColWidths, Regname);
  AppSettings.WriteString(asListColsVisible, ColsVisible, Regname);
  AppSettings.WriteString(asListColPositions, ColPos, Regname);
  AppSettings.WriteString(asListColSort, IntToStr(List.Header.SortColumn) + ',' + IntToStr(Integer(List.Header.SortDirection)), RegName);
end;


{**
  Restore setup of VirtualStringTree from registry
}
class procedure TExtForm.RestoreListSetup( List: TLazVirtualStringTree );
var
  i : Byte;
  colpos : Integer;
  ColWidth: Int64;
  Value : String;
  ValueList : TStringList;
  Regname: String;
  OwnerForm: TWinControl;
begin
  ValueList := TStringList.Create;

  // Column widths
  OwnerForm := GetParentFormOrFrame(List);
  Regname := OwnerForm.Name + '.' + List.Name;
  Value := AppSettings.ReadString(asListColWidths, Regname);
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    for i := 0 to ValueList.Count - 1 do
    begin
      ColWidth := MakeInt(ValueList[i]);
      //ColWidth := RoundCommercial(ColWidth * OwnerForm.ScaleFactor);
      // Check if column number exists and width is at least 1 pixel
      if (List.Header.Columns.Count > i) and (ColWidth > 0) and (ColWidth < 1000) then
        List.Header.Columns[i].Width := ColWidth;
    end;
  end;

  // Column visibility
  Value := AppSettings.ReadString(asListColsVisible, Regname);
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    for i:=0 to List.Header.Columns.Count-1 do begin
      if ValueList.IndexOf( IntToStr(i) ) > -1 then
        List.Header.Columns[i].Options := List.Header.Columns[i].Options + [coVisible]
      else
        List.Header.Columns[i].Options := List.Header.Columns[i].Options - [coVisible];
    end;
  end;

  // Column position
  Value := AppSettings.ReadString(asListColPositions, Regname);
  if Value <> '' then begin
    ValueList := Explode( ',', Value );
    for i := 0 to ValueList.Count - 1 do
    begin
      colpos := MakeInt(ValueList[i]);
      // Check if column number exists
      if List.Header.Columns.Count > i then
        List.Header.Columns[i].Position := colpos;
    end;
  end;

  // Sort column and direction
  Value := AppSettings.ReadString(asListColSort, Regname);
  if Value <> '' then begin
    ValueList := Explode(',', Value);
    if ValueList.Count = 2 then begin
      List.Header.SortColumn := MakeInt(ValueList[0]);
      if MakeInt(ValueList[1]) = 0 then
        List.Header.SortDirection := sdAscending
      else
        List.Header.SortDirection := sdDescending;
    end;
  end;

  ValueList.Free;
end;


procedure TExtForm.FilterNodesByEdit(Edit: TEditButton; Tree: TLazVirtualStringTree);
var
  rx: TRegExpr;
  Node: PVirtualNode;
  i: Integer;
  match: Boolean;
  CellText: String;
begin
  // Loop through all tree nodes and hide non matching
  Node := Tree.GetFirst;
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.Expression := Edit.Text;
  try
    rx.Exec('abc');
  except
    on E:ERegExpr do begin
      if rx.Expression <> '' then begin
        //LogSQL('Filter text is not a valid regular expression: "'+rx.Expression+'"', lcError);
        rx.Expression := '';
      end;
    end;
  end;

  Tree.BeginUpdate;
  while Assigned(Node) do begin
    if not Tree.HasChildren[Node] then begin
      // Don't filter anything if the filter text is empty
      match := rx.Expression = '';
      // Search for given text in node's captions
      if not match then for i := 0 to Tree.Header.Columns.Count - 1 do begin
        CellText := Tree.Text[Node, i];
        match := rx.Exec(CellText);
        if match then
          break;
      end;
      Tree.IsVisible[Node] := match;
      if match and IsNotEmpty(Edit.Text) then
        Tree.VisiblePath[Node] := True;
    end;
    Node := Tree.GetNext(Node);
  end;
  Tree.EndUpdate;
  Tree.Invalidate;
  rx.Free;

  // Hiding the button is not really supported by TEditButton, works dodgy
  //Edit.Button.Visible := IsNotEmpty(Edit.Text);
end;

procedure TExtForm.ArrangeControls(aParentControl: TControl);
const
  PaddingPx = 6;
  LabelMoveDown = 2;
var
  Grid: TControlGrid;
  Row: TControlRow;
  FoundControl: TControl;
  NewTopPos: Integer;
begin
  // Reposition edits and combo boxes due to different height on different OS's
  Grid := TControlGrid.Create(aParentControl);

  NewTopPos := PaddingPx;
  for Row in Grid do begin
    for FoundControl in Row do begin
      if (akBottom in FoundControl.Anchors) and (akTop in FoundControl.Anchors) then
        FoundControl.Height := FoundControl.Height + (FoundControl.Top - NewTopPos);
      if (FoundControl is TLabel) and (FoundControl.Left < 30) then begin
        FoundControl.Top := NewTopPos + LabelMoveDown;
        FoundControl.Left := PaddingPx;
      end
      else begin
        FoundControl.Top := NewTopPos;
      end;
      if (akRight in FoundControl.Anchors) and (FoundControl.Left + FoundControl.Width > aParentControl.Width - 30) then begin
        FoundControl.Width := aParentControl.Width - FoundControl.Left - PaddingPx;
      end;
    end;
    Inc(NewTopPos, Row.Height + PaddingPx);
  end;
  Grid.Free;
end;

function TExtForm.ScaleSize(x: Extended): Integer;
begin
  // Shorthand for dpi scaling hardcoded width/height values of controls
  Result := ScaleSize(x, Self);
end;

class function TExtForm.ScaleSize(x: Extended; Control: TControl): Integer;
begin
  // Same as above for callers without a form
  Result := Control.Scale96ToForm(Round(x));
end;


class procedure TExtForm.PageControlTabHighlight(PageControl: TPageControl);
var
  i, CurrentImage, CountOriginals: Integer;
  Images: TImageList;
  GrayscaleMode: Integer;
  IsQueryTab, DoGrayscale: Boolean;
begin
  // Set grayscale icon on inactive tabs
  if not (PageControl.Images is TImageList) then
    Exit;
  GrayscaleMode := AppSettings.ReadInt(asTabIconsGrayscaleMode);

  Images := PageControl.Images as TImageList;
  CountOriginals := Images.Count;

  for i:=0 to PageControl.PageCount-1 do begin
    CurrentImage := PageControl.Pages[i].ImageIndex;
    if PageControl.ActivePageIndex = i then begin
      if CurrentImage >= CountOriginals then begin
        // Grayscaled => Color
        PageControl.Pages[i].ImageIndex := CurrentImage - CountOriginals;
      end;
    end
    else begin
      if CurrentImage < CountOriginals then begin
        // Color => Grayscaled
        IsQueryTab := (PageControl.Owner.Name = 'MainForm') and ExecRegExpr('^tabQuery\d*$', PageControl.Pages[i].Name);
        if ((GrayscaleMode = 1) and IsQueryTab) or (GrayscaleMode = 2) then
          PageControl.Pages[i].ImageIndex := CurrentImage + CountOriginals;
      end;
    end;
  end;;
end;

procedure TExtForm.ShowPopup(ClickedControl: TControl; PopupMenu: TPopupMenu);
begin
  PopupMenu.Popup(ClickedControl.ClientOrigin.X, ClickedControl.ClientOrigin.Y + ClickedControl.Height);
end;


{ TExtFileOpenDialog }

constructor TExtFileOpenDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEncodings := TStringList.Create;
  FEncodingIndex := 0;
  FFilters := TStringList.Create;
end;


destructor TExtFileOpenDialog.Destroy;
begin
  FEncodings.Free;
  FFilters.Free;
  inherited;
end;


procedure TExtFileOpenDialog.AddFileType(FileMask, DisplayName: String);
var
  //FileType: TFileTypeItem;
  i: Integer;
  NewFilter: String;
begin
  // Shorthand for callers
  {FileType := FileTypes.Add;
  FileType.DisplayName := DisplayName;
  FileType.FileMask := FileMask;}
  FFilters.Values[DisplayName] := FileMask;
  NewFilter := '';
  for i:=FFilters.Count-1 downto 0 do begin
    NewFilter := NewFilter + Format('%s (%s)|%s', [FFilters.Names[i], FFilters.ValueFromIndex[i], FFilters.ValueFromIndex[i]]);
    if i > 0 then
      NewFilter := NewFilter + '|';
  end;
  Filter := NewFilter;
end;


{procedure TExtFileOpenDialog.DoOnExecute;
var
  iCustomize: IFileDialogCustomize;
  i: Integer;
begin
  // Add encodings selector
  if Dialog.QueryInterface(IFileDialogCustomize, iCustomize) = S_OK then
  begin
    iCustomize.StartVisualGroup(0, PChar(_('Encoding:')));
    try
      // note other controls available: AddCheckButton, AddEditBox, AddPushButton, AddRadioButtonList...
      iCustomize.AddComboBox(idEncodingCombo);
      for i:=0 to FEncodings.Count - 1 do begin
        iCustomize.AddControlItem(idEncodingCombo, i, PChar(FEncodings[i]));
      end;
      iCustomize.SetSelectedControlItem(idEncodingCombo, FEncodingIndex);
      if not Assigned(OnFileOkClick) then
        OnFileOkClick := FileOkClickNoOp;
    finally
      iCustomize.EndVisualGroup;
    end;
  end;
end;}


procedure TExtFileOpenDialog.FileOkClickNoOp(Sender: TObject; var CanClose: Boolean);
begin
  // Dummy procedure, just makes sure parent class calls DoOnFileOkClick
end;


{function TExtFileOpenDialog.DoOnFileOkClick: Boolean;
var
  iCustomize: IFileDialogCustomize;
begin
  Result := inherited;
  if Dialog.QueryInterface(IFileDialogCustomize, iCustomize) = S_OK then
  begin
    iCustomize.GetSelectedControlItem(idEncodingCombo, FEncodingIndex);
  end;
end;}



{ TExtFileSaveDialog }

{constructor TExtFileSaveDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLineBreaks := TStringList.Create;
  FLineBreaks.Add(_('Windows linebreaks'));
  FLineBreaks.Add(_('UNIX linebreaks'));
  FLineBreaks.Add(_('Mac OS linebreaks'));
  FLineBreakIndex := lbsWindows;
end;


destructor TExtFileSaveDialog.Destroy;
begin
  FLineBreaks.Free;
  inherited;
end;


procedure TExtFileSaveDialog.AddFileType(FileMask, DisplayName: String);
var
  FileType: TFileTypeItem;
begin
  // Shorthand for callers
  FileType := FileTypes.Add;
  FileType.DisplayName := DisplayName;
  FileType.FileMask := FileMask;
end;


procedure TExtFileSaveDialog.DoOnExecute;
var
  iCustomize: IFileDialogCustomize;
  i, ComboIndex: Integer;
begin
  // Add line break selector
  if Dialog.QueryInterface(IFileDialogCustomize, iCustomize) = S_OK then
  begin
    iCustomize.StartVisualGroup(0, PChar(_('Linebreaks')+':'));
    try
      iCustomize.AddComboBox(idLineBreakCombo);
      case FLineBreakIndex of
        lbsUnix: ComboIndex := 1;
        lbsMac: ComboIndex := 2;
        else ComboIndex := 0;
      end;
      for i:=0 to FLineBreaks.Count - 1 do begin
        iCustomize.AddControlItem(idLineBreakCombo, i, PChar(FLineBreaks[i]));
      end;
      iCustomize.SetSelectedControlItem(idLineBreakCombo, ComboIndex);
      if not Assigned(OnFileOkClick) then
        OnFileOkClick := FileOkClickNoOp;
    finally
      iCustomize.EndVisualGroup;
    end;
  end;
end;


procedure TExtFileSaveDialog.FileOkClickNoOp(Sender: TObject; var CanClose: Boolean);
begin
  // Dummy procedure, just makes sure parent class calls DoOnFileOkClick
end;


function TExtFileSaveDialog.DoOnFileOkClick: Boolean;
var
  iCustomize: IFileDialogCustomize;
  ComboIndex: Cardinal;
begin
  Result := inherited;
  if Dialog.QueryInterface(IFileDialogCustomize, iCustomize) = S_OK then
  begin
    iCustomize.GetSelectedControlItem(idLineBreakCombo, ComboIndex);
    case ComboIndex of
      0: FLineBreakIndex := lbsWindows;
      1: FLineBreakIndex := lbsUnix;
      2: FLineBreakIndex := lbsMac;
    end;
  end;
end;}


{ TExtSynHotKey }

{procedure TExtSynHotKey.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TExtSynHotKey.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TExtSynHotKey.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TExtSynHotKey.Paint;
var
  r: TRect;
begin
  r := ClientRect;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  InflateRect(r, -BorderWidth, -BorderWidth);
  Canvas.FillRect(r);
  if Enabled then
    Canvas.Font.Color := clWindowText
  else
    Canvas.Font.Color := clGrayText;
  SynUnicode.TextRect(Canvas, r, BorderWidth + 1, BorderWidth + 1, Text);
end;}



{ TExtComboBox }

{procedure TExtComboBox.Change;
var
  P: TPoint;
  HintRect: TRect;
  HintText: String;
  HintWidth, Padding: Integer;
begin
  inherited;
  if (ItemIndex > -1) and DroppedDown and GetCursorPos(P) then begin
    HintText := Items[ItemIndex];
    HintWidth := Canvas.TextWidth(HintText);
    if HintWidth > Width then begin
      Padding := TExtForm.ScaleSize(10, Self);
      HintRect := Rect(
        P.X + Padding,
        P.Y + Padding * 2,
        P.X + HintWidth + Padding * 3,
        P.Y + Padding * 4
        );
      FHintWindow.ActivateHint(HintRect, HintText);
    end;
  end;
end;

procedure TExtComboBox.CloseUp;
begin
  inherited;
  FHintWindow.Hide;
  ControlStyle := ControlStyle - [csActionClient];
end;

procedure TExtComboBox.DropDown;
begin
  inherited;
  if not Assigned(FHintWindow) then
    FHintWindow := THintWindow.Create(Self);
  FcbHintIndex := -1;
  ControlStyle := ControlStyle + [csActionClient];
end;

procedure TExtComboBox.InitiateAction;
var
  Idx: Integer;
begin
  inherited;
  Idx := ItemIndex;
  if Idx <> FcbHintIndex then
  begin
    FcbHintIndex := ItemIndex;
    Change;
  end;
end;}



{ TExtHintWindow }


{function TExtHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect;
begin
  Result := inherited;
  // Customized: enlarge surrounding rect to make space for padding
  if AHint.Contains(SLineBreak) then begin
    Result.Right := Result.Right + 2 * ScaleValue(Padding);
    Result.Bottom := Result.Bottom + 2 * ScaleValue(Padding);
  end;
end;


procedure TExtHintWindow.Paint;
var
  R, ClipRect: TRect;
  LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  LGradientStart, LGradientEnd, LTextColor: TColor;
begin
  R := ClientRect;
  LStyle := StyleServices(Screen.ActiveForm);
  LTextColor := Screen.HintFont.Color;
  if LStyle.Enabled then
  begin
    ClipRect := R;
    InflateRect(R, 4, 4);
    if TOSVersion.Check(6) and LStyle.IsSystemStyle then
    begin
      // Paint Windows gradient background
      LStyle.DrawElement(Canvas.Handle, LStyle.GetElementDetails(tttStandardNormal), R, ClipRect);
    end
    else
    begin
      LDetails := LStyle.GetElementDetails(thHintNormal);
      if LStyle.GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
        LGradientStart := LColor
      else
        LGradientStart := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
        LGradientEnd := LColor
      else
        LGradientEnd := clInfoBk;
      if LStyle.GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
        LTextColor := LColor
      else
        LTextColor := Screen.HintFont.Color;
      GradientFillCanvas(Canvas, LGradientStart, LGradientEnd, R, gdVertical);
    end;
    R := ClipRect;
  end;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  // Customized: move inner rect right+down to add padding to outer edge
  if String(Caption).Contains(SLineBreak) then begin
    Inc(R.Left, ScaleValue(Padding));
    Inc(R.Top, ScaleValue(Padding));
  end;
  Canvas.Font.Color := LTextColor;
  DrawText(Canvas.Handle, Caption, -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end;}


end.
