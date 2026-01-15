unit extra_controls;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Types, StdCtrls, Clipbrd, apphelpers,
  Graphics, Dialogs, ImgList, ComCtrls, Generics.Defaults,
  ExtCtrls, laz.VirtualTrees, RegExpr, Controls, EditBtn, Menus,
  LCLIntf;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar

  { TExtForm }

  TExtForm = class(TForm)
    private
      FPixelsPerInchDesigned: Integer;
    protected
      //procedure DoBeforeMonitorDpiChanged(OldDPI, NewDPI: Integer); override;
      //procedure DoAfterMonitorDpiChanged(OldDPI, NewDPI: Integer); override;
      procedure FilterNodesByEdit(Edit: TEditButton; Tree: TLazVirtualStringTree);
    public
      constructor Create(AOwner: TComponent); override;
      class procedure InheritFont(AFont: TFont);
      class procedure SaveListSetup(List: TLazVirtualStringTree);
      class procedure RestoreListSetup(List: TLazVirtualStringTree);
      function ScaleSize(x: Extended): Integer; overload;
      class function ScaleSize(x: Extended; Control: TControl): Integer; overload;
      class procedure PageControlTabHighlight(PageControl: TPageControl);
      property PixelsPerInchDesigned: Integer read FPixelsPerInchDesigned;
      class procedure ShowPopup(ClickedControl: TControl; PopupMenu: TPopupMenu);
  end;

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

  TExtComboBox = class(TComboBox)
    private
      FcbHintIndex: Integer;
      FHintWindow: THintWindow;
    protected
      procedure Change; override;
      procedure DropDown; override;
      procedure CloseUp; override;
    public
      procedure InitiateAction; override;
  end;

  {TExtHintWindow = class(THintWindow)
    private
      const Padding: Integer = 8;
    protected
      procedure Paint; override;
    public
      function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect; override;
  end;}


implementation

uses generic_types;

{ TExtForm }

constructor TExtForm.Create(AOwner: TComponent);
var
  OldImageList: TCustomImageList;
begin
  inherited;

  FPixelsPerInchDesigned := DesignTimePPI;
  //InheritFont(Font);
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
    ColWidth := OwnerForm.ScaleFormToDesign(List.Header.Columns[i].Width); // RoundCommercial(List.Header.Columns[i].Width / OwnerForm.ScaleFactor);
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
      ColWidth := OwnerForm.ScaleDesignToForm(ColWidth);
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
  rx: TRegExprUmlauts;
  Node: PVirtualNode;
  i: Integer;
  match: Boolean;
  CellText: String;
begin
  // Loop through all tree nodes and hide non matching
  Node := Tree.GetFirst;
  rx := TRegExprUmlauts.Create;
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
  Images: TCustomImageList;
  GrayscaleMode: Integer;
  IsQueryTab: Boolean;
begin
  // Set grayscale icon on inactive tabs
  if not (PageControl.Images is TImageList) then
    Exit;
  GrayscaleMode := AppSettings.ReadInt(asTabIconsGrayscaleMode);

  Images := PageControl.Images;
  CountOriginals := Images.Count div 2;

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

class procedure TExtForm.ShowPopup(ClickedControl: TControl; PopupMenu: TPopupMenu);
begin
  PopupMenu.Popup(ClickedControl.ClientOrigin.X, ClickedControl.ClientOrigin.Y + ClickedControl.Height);
end;


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

procedure TExtComboBox.Change;
var
  P: TPoint;
  HintRect: TRect;
  HintText: String;
  HintWidth, Padding: Integer;
begin
  inherited;
  P := Point(0,0);
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
