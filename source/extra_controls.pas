unit extra_controls;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Types, StdCtrls, Clipbrd, apphelpers,
  Graphics, Dialogs, ImgList, ComCtrls, Generics.Defaults,
  ExtCtrls, laz.VirtualTrees, RegExpr, Controls, EditBtn, Menus,
  LCLIntf, Math, LCLType, LMessages;

const
  // Posted by TExtForm.DeferFreeControl, see there. Value is WM_USER + $0D00,
  // written explicitly to avoid pulling in the Windows unit here.
  WM_HEIDISQL_DEFERREDFREE = $1100;

type
  TMonitorDpiChangedEvent = procedure(Sender: TObject; OldDPI: Integer; NewDPI: Integer) of object;

  // Form with a sizegrip in the lower right corner, without the need for a statusbar

  { TExtForm }

  TExtForm = class(TForm)
    private
      FCurrentPPI: Integer;
      FOnMonitorDpiChanged: TMonitorDpiChangedEvent;
      procedure FixTreeFontsOnDPIChange(Owner: TComponent; NewDPI, OldDPI: Integer);
    protected
      procedure FilterNodesByEdit(Edit: TEditButton; Tree: TLazVirtualStringTree);
      procedure WMDeferredFree(var Message: TLMessage); message WM_HEIDISQL_DEFERREDFREE;
      procedure WMDPIChanged(var Msg: TLMessage); message LM_DPICHANGED;
      procedure DoShow; override;
      procedure DoMonitorDpiChanged(OldDPI, NewDPI: Integer); virtual;
    public
      constructor Create(AOwner: TComponent); override;
      // Applies the tree font/height fixes for the given DPI and fires
      // OnMonitorDpiChanged if the DPI changed. Called from WMDPIChanged and
      // once at startup from DoShow.
      procedure ApplyDPIFix(NewDPI: Integer);
      procedure DeferFreeControl(AControl: TControl);
      class procedure InheritFont(AFont: TFont);
      class procedure SaveListSetup(List: TLazVirtualStringTree);
      class procedure RestoreListSetup(List: TLazVirtualStringTree);
      class function GetCurrentPPI(Control: TControl): Integer;
      function ScaleFromDesign(x: Extended): Integer; overload;
      class function ScaleFromDesign(x: Extended; Control: TControl): Integer; overload;
      // Returns a PPI-aware width of a space character
      function Space(Multiply: Integer=1): Integer;
      class procedure PageControlTabHighlight(PageControl: TPageControl);
      class procedure ShowPopup(ClickedControl: TControl; PopupMenu: TPopupMenu);
      property OnMonitorDpiChanged: TMonitorDpiChangedEvent read FOnMonitorDpiChanged write FOnMonitorDpiChanged;
  end;

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



implementation

uses generic_types;

{ TExtForm }

constructor TExtForm.Create(AOwner: TComponent);
var
  OldImageList: TCustomImageList;
begin
  FCurrentPPI := 0;
  inherited;

  InheritFont(Font);
end;

procedure TExtForm.WMDPIChanged(var Msg: TLMessage);
begin
  // Let the LCL scale fonts and bounds first (AutoAdjustLayout inside
  // TCustomForm.WMDPIChanged); the new DPI comes directly from the message.
  inherited;
  ApplyDPIFix(hi(Cardinal(Msg.wParam)));
end;

procedure TExtForm.DoShow;
begin
  inherited;
  // Trigger the DPI fix once at startup (and when re-shown on a monitor with a
  // different DPI): on a high-DPI primary monitor no WM_DPICHANGED is sent at
  // startup, so without this the tree fonts/heights would not be scaled.
  ApplyDPIFix(Monitor.PixelsPerInch);
end;

procedure TExtForm.ApplyDPIFix(NewDPI: Integer);
var
  OldDPI: Integer;
begin
  OldDPI := FCurrentPPI;
  if OldDPI <= 0 then
    OldDPI := 96; // design-time DPI
  if (NewDPI > 0) and (NewDPI <> FCurrentPPI) then begin
    // Moving window to a different screen or user changed the DPI setting.
    FCurrentPPI := NewDPI;
    FixTreeFontsOnDPIChange(Self, NewDPI, OldDPI);
    DoMonitorDpiChanged(OldDPI, NewDPI);
  end;
end;

procedure TExtForm.FixTreeFontsOnDPIChange(Owner: TComponent; NewDPI, OldDPI: Integer);
var
  i: Integer;
  VT: TLazVirtualStringTree;
  Node: PVirtualNode;
begin
  // Recurse through owned components: query tabs etc. are owned by the form,
  // but their trees are owned by panels/tabsheets owned by them.
  for i := 0 to Owner.ComponentCount - 1 do begin
    if Owner.Components[i] is TLazVirtualStringTree then begin
      VT := TLazVirtualStringTree(Owner.Components[i]);
      // VirtualTrees paint into an offscreen bitmap whose canvas font PPI stays
      // at Screen.PixelsPerInch (primary monitor). TFont.Assign converts via Size
      // when PPIs differ, which would downconvert the LCL-scaled font back to
      // 96 dpi, so pin the PPI to the buffer canvas PPI and set the pixel height
      // for the monitor DPI directly. ParentFont must be set to False explicitly:
      // assigning Font clears it internally (TControl.FontChanged), and then the
      // parent font would no longer be copied on the next DPI change.
      VT.ParentFont := False;
      VT.Font.PixelsPerInch := Screen.PixelsPerInch;
      VT.Font.Height := -MulDiv(Font.Size, NewDPI, 72);
      // The header has its own font (ParentFont default False) which the LCL
      // does not scale either; apply the same fix.
      VT.Header.ParentFont := False;
      VT.Header.Font.PixelsPerInch := Screen.PixelsPerInch;
      VT.Header.Font.Height := VT.Font.Height;
      // VT does not scale node/header heights on a DPI change, so scale them by
      // the DPI ratio (this preserves multiline multipliers set by FixVT).
      VT.DefaultNodeHeight := MulDiv(VT.DefaultNodeHeight, NewDPI, OldDPI);
      VT.Header.Height := MulDiv(VT.Header.Height, NewDPI, OldDPI);
      VT.BeginUpdate;
      try
        Node := VT.GetFirstInitialized;
        while Assigned(Node) do begin
          VT.NodeHeight[Node] := MulDiv(VT.NodeHeight[Node], NewDPI, OldDPI);
          Node := VT.GetNextInitialized(Node);
        end;
      finally
        VT.EndUpdate;
      end;
    end;
    FixTreeFontsOnDPIChange(Owner.Components[i], NewDPI, OldDPI);
  end;
end;

procedure TExtForm.DoMonitorDpiChanged(OldDPI, NewDPI: Integer);
begin
  if Assigned(FOnMonitorDpiChanged) then
    FOnMonitorDpiChanged(Self, OldDPI, NewDPI);
end;


class procedure TExtForm.InheritFont(AFont: TFont);
var
  GUIFontName: String;
begin
  // Set custom font if set, or default system font.
  // The font *size* is deliberately not touched here: it is scaled by the LCL
  // according to the user's DPI (form scaling), and setting it again would
  // result in double enlargement. See issue #3204.
  GUIFontName := AppSettings.ReadString(asGUIFontName);
  if not GUIFontName.IsEmpty then begin
    // Apply user specified font
    AFont.Name := GUIFontName;
  end else begin
    // Apply system font. See issue #3204.
    AFont.Orientation := Screen.SystemFont.Orientation;
    AFont.CharSet := Screen.SystemFont.CharSet;
    AFont.Name := Screen.SystemFont.Name;
    AFont.Pitch := Screen.SystemFont.Pitch;
  end;
end;

procedure TExtForm.WMDeferredFree(var Message: TLMessage);
begin
  TObject(Message.WParam).Free;
end;

procedure TExtForm.DeferFreeControl(AControl: TControl);
begin
  // Free AControl after the current message chain has finished. Needed when
  // the control's own message handling is on the call stack, e.g. when an
  // editor control's OnKeyDown event ends a grid edit which destroys the
  // editor link. Also keeps the old editor focused until the new editor link
  // takes over the focus, avoiding the LCL's focus fallback to the next
  // control in tab order.
  PostMessage(Handle, WM_HEIDISQL_DEFERREDFREE, WParam(AControl), 0);
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
    ValueList.Free;
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
    ValueList.Free;
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
    ValueList.Free;
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
    ValueList.Free;
  end;

end;


procedure TExtForm.FilterNodesByEdit(Edit: TEditButton; Tree: TLazVirtualStringTree);
var
  rx: TRegExprUmlauts;
  Node: PVirtualNode;
  i, ColumnCount: Integer;
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
      // Suport trees with 0 defined columns, like the shortcut tree in preferences
      ColumnCount := Max(Tree.Header.Columns.Count, 1);
      // Search for given text in node's captions
      if not match then for i:=0 to ColumnCount - 1 do begin
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


class function TExtForm.GetCurrentPPI(Control: TControl): Integer;
var
  P: TPoint;
  Mon: TMonitor;
begin
  P := Control.ClientToScreen(Point(0, 0));
  Mon := Screen.MonitorFromPoint(P, mdNearest);
  Result := Mon.PixelsPerInch;
end;


function TExtForm.ScaleFromDesign(x: Extended): Integer;
begin
  // Shorthand for DPI upscaling hardcoded width/height values of controls
  Result := ScaleFromDesign(x, Self);
end;

class function TExtForm.ScaleFromDesign(x: Extended; Control: TControl): Integer;
begin
  // Same as above for callers without a form
  Result := Control.Scale96ToForm(Round(x));
end;

function TExtForm.Space(Multiply: Integer=1): Integer;
var
  CurPpi: Integer;
begin
  Result := Canvas.TextWidth(' ') * Multiply;
  CurPpi := GetCurrentPPI(Self);
  if CurPpi <> Canvas.Font.PixelsPerInch then
    Result := MulDiv(Result, CurPpi, Canvas.Font.PixelsPerInch);
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
      Padding := TExtForm.ScaleFromDesign(10, Self);
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
