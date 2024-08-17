unit extra_controls;

interface

uses
  System.Classes, System.SysUtils, Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Types, Vcl.StdCtrls, Vcl.Clipbrd,
  SizeGrip, apphelpers, Vcl.Graphics, Vcl.Dialogs, gnugettext, Vcl.ImgList, Vcl.VirtualImageList, Vcl.ComCtrls,
  Winapi.ShLwApi, Vcl.ExtCtrls, VirtualTrees, VirtualTrees.Types, SynRegExpr, Vcl.Controls, Winapi.ShlObj,
  SynEditMiscClasses, SynUnicode;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar
  TExtForm = class(TForm)
    private
      FSizeGrip: TSizeGripXP;
      FPixelsPerInchDesigned: Integer;
      function GetHasSizeGrip: Boolean;
      procedure SetHasSizeGrip(Value: Boolean);
    protected
      procedure DoShow; override;
      procedure DoBeforeMonitorDpiChanged(OldDPI, NewDPI: Integer); override;
      procedure DoAfterMonitorDpiChanged(OldDPI, NewDPI: Integer); override;
      procedure FilterNodesByEdit(Edit: TButtonedEdit; Tree: TVirtualStringTree);
    public
      constructor Create(AOwner: TComponent); override;
      class procedure InheritFont(AFont: TFont);
      property HasSizeGrip: Boolean read GetHasSizeGrip write SetHasSizeGrip default False;
      class procedure FixControls(ParentComp: TComponent);
      class procedure SaveListSetup(List: TVirtualStringTree);
      class procedure RestoreListSetup(List: TVirtualStringTree);
      function ScaleSize(x: Extended): Integer; overload;
      class function ScaleSize(x: Extended; Control: TControl): Integer; overload;
      class procedure PageControlTabHighlight(PageControl: TPageControl);
      property PixelsPerInchDesigned: Integer read FPixelsPerInchDesigned;
  end;

  // Modern file-open-dialog with high DPI support and encoding selector
  TExtFileOpenDialog = class(TFileOpenDialog)
    private
      FEncodings: TStringList;
      FEncodingIndex: Cardinal;
      const idEncodingCombo = 1;
      procedure FileOkClickNoOp(Sender: TObject; var CanClose: Boolean);
    protected
      procedure DoOnExecute; override;
      function DoOnFileOkClick: Boolean; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure AddFileType(FileMask, DisplayName: String);
      property Encodings: TStringList read FEncodings write FEncodings;
      property EncodingIndex: Cardinal read FEncodingIndex write FEncodingIndex;
  end;

  TExtFileSaveDialog = class(TFileSaveDialog)
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
  end;

  TExtSynHotKey = class(TSynHotKey)
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
  end;

  TExtComboBox = class(TComboBox)
    private
      FcbHintIndex: Integer;
      FHintWindow: THintWindow;
    protected
      procedure Change; override;
      procedure DropDown; override;
      procedure CloseUp; override;
      procedure InitiateAction; override;
  end;


implementation


{ TExtForm }

constructor TExtForm.Create(AOwner: TComponent);
var
  OldImageList: TCustomImageList;
begin
  inherited;

  FPixelsPerInchDesigned := 96;
  InheritFont(Font);
  HasSizeGrip := False;

  // Reduce flicker on Windows 10
  // See https://www.heidisql.com/forum.php?t=19141
  if CheckWin32Version(6, 2) then begin
    DoubleBuffered := True;
  end;

  // Translation and related fixes
  // Issue #557: Apply images *after* translating main menu, so top items don't get unused
  // space left besides them.
  if (Menu <> nil) and (Menu.Images <> nil) then begin
    OldImageList := Menu.Images;
    Menu.Images := nil;
    TranslateComponent(Self);
    Menu.Images := OldImageList;
  end else begin
    TranslateComponent(Self);
  end;

end;


procedure TExtForm.DoShow;
begin
  FixControls(Self);
  inherited;
end;


procedure TExtForm.DoBeforeMonitorDpiChanged(OldDPI, NewDPI: Integer);
begin
  // Reduce flicker
  inherited;
  LockWindowUpdate(Handle);
end;

procedure TExtForm.DoAfterMonitorDpiChanged(OldDPI, NewDPI: Integer);
begin
  // Release window updates
  LockWindowUpdate(0);
  inherited;
end;


class procedure TExtForm.FixControls(ParentComp: TComponent);
var
  i: Integer;

  procedure ProcessSingleComponent(Cmp: TComponent);
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
  end;
begin
  // Passed component itself may also be some control to be fixed
  // e.g. TInplaceEditorLink.MainControl
  ProcessSingleComponent(ParentComp);
  for i:=0 to ParentComp.ComponentCount-1 do begin
    ProcessSingleComponent(ParentComp.Components[i]);
  end;
end;


function TExtForm.GetHasSizeGrip: Boolean;
begin
  Result := FSizeGrip <> nil;
end;


procedure TExtForm.SetHasSizeGrip(Value: Boolean);
begin
  if Value then begin
    FSizeGrip := TSizeGripXP.Create(Self);
    FSizeGrip.Enabled := True;
  end else begin
    if FSizeGrip <> nil then
      FreeAndNil(FSizeGrip);
  end;
end;


class procedure TExtForm.InheritFont(AFont: TFont);
var
  LogFont: TLogFont;
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
    // Code taken from http://www.gerixsoft.com/blog/delphi/system-font
    if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(TLogFont), @LogFont, 0) then begin
      // Leave font size at default, as the system's font size is probably scaled up
      //AFont.Height := LogFont.lfHeight;
      AFont.Orientation := LogFont.lfOrientation;
      AFont.Charset := TFontCharset(LogFont.lfCharSet);
      AFont.Name := PChar(@LogFont.lfFaceName);
      case LogFont.lfPitchAndFamily and $F of
        VARIABLE_PITCH: AFont.Pitch := fpVariable;
        FIXED_PITCH: AFont.Pitch := fpFixed;
        else AFont.Pitch := fpDefault;
      end;
    end else begin
      ErrorDialog('Could not detect system font, using SystemParametersInfo.');
    end;
  end;
end;


{**
  Save setup of a VirtualStringTree to registry
}
class procedure TExtForm.SaveListSetup( List: TVirtualStringTree );
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
    ColWidth := RoundCommercial(List.Header.Columns[i].Width / OwnerForm.ScaleFactor);
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
class procedure TExtForm.RestoreListSetup( List: TVirtualStringTree );
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
      ColWidth := RoundCommercial(ColWidth * OwnerForm.ScaleFactor);
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


procedure TExtForm.FilterNodesByEdit(Edit: TButtonedEdit; Tree: TVirtualStringTree);
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

  Edit.RightButton.Visible := IsNotEmpty(Edit.Text);
end;


function TExtForm.ScaleSize(x: Extended): Integer;
begin
  // Shorthand for dpi scaling hardcoded width/height values of controls
  Result := ScaleSize(x, Self);
end;

class function TExtForm.ScaleSize(x: Extended; Control: TControl): Integer;
begin
  // Same as above for callers without a form
  Result := Round(x * Control.ScaleFactor);
end;


class procedure TExtForm.PageControlTabHighlight(PageControl: TPageControl);
var
  i, CurrentImage, CountOriginals: Integer;
  Images: TVirtualImageList;
  GrayscaleMode: Integer;
  IsQueryTab, DoGrayscale: Boolean;
begin
  // Set grayscale icon on inactive tabs
  if not (PageControl.Images is TVirtualImageList) then
    Exit;
  GrayscaleMode := AppSettings.ReadInt(asTabIconsGrayscaleMode);

  Images := PageControl.Images as TVirtualImageList;
  CountOriginals := Images.ImageCollection.Count;

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
  end;
end;



{ TExtFileOpenDialog }

constructor TExtFileOpenDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEncodings := TStringList.Create;
  FEncodingIndex := 0;
end;


destructor TExtFileOpenDialog.Destroy;
begin
  FEncodings.Free;
  inherited;
end;


procedure TExtFileOpenDialog.AddFileType(FileMask, DisplayName: String);
var
  FileType: TFileTypeItem;
begin
  // Shorthand for callers
  FileType := FileTypes.Add;
  FileType.DisplayName := DisplayName;
  FileType.FileMask := FileMask;
end;


procedure TExtFileOpenDialog.DoOnExecute;
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
end;


procedure TExtFileOpenDialog.FileOkClickNoOp(Sender: TObject; var CanClose: Boolean);
begin
  // Dummy procedure, just makes sure parent class calls DoOnFileOkClick
end;


function TExtFileOpenDialog.DoOnFileOkClick: Boolean;
var
  iCustomize: IFileDialogCustomize;
begin
  Result := inherited;
  if Dialog.QueryInterface(IFileDialogCustomize, iCustomize) = S_OK then
  begin
    iCustomize.GetSelectedControlItem(idEncodingCombo, FEncodingIndex);
  end;
end;



{ TExtFileSaveDialog }

constructor TExtFileSaveDialog.Create(AOwner: TComponent);
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
end;


{ TExtSynHotKey }

procedure TExtSynHotKey.WMKillFocus(var Msg: TWMKillFocus);
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
end;



{ TExtComboBox }

procedure TExtComboBox.Change;
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
end;

end.
