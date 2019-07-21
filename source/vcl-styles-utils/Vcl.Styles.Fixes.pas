// **************************************************************************************************
//
// Unit Vcl.Styles.Fixes
// unit for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.Fixes
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
// Contributors
//
// Leonardo Cechet
//
// **************************************************************************************************

unit Vcl.Styles.Fixes;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Graphics;

{$IF CompilerVersion >= 23.0}
type
  /// <summary> The <c>TButtonStyleHookFix</c> vcl style hook fix these QC #103708, #107764 for Delphi XE2
  /// and the https://quality.embarcadero.com/browse/RSP-11619 issue present in X2-XE8
  /// </summary>
  /// <remarks>
  /// Use this hook in this way
  /// <code>
  /// TStyleManager.Engine.RegisterStyleHook(TButton, TButtonStyleHookFix);
  /// </code>
  /// </remarks>
  TButtonStyleHookFix = class(TButtonStyleHook)
  protected
    procedure Paint(Canvas: TCanvas); override;
  end;
{$IFEND}

{$IF CompilerVersion <= 27.0}
type
  /// <summary> The <c>TListViewStyleHookFix</c> vcl style hook fix these QC #108678, #108875 for Delphi XE2-XE6
  /// </summary>
  /// <remarks>
  /// Use this hook in this way
  /// <code>
  /// TStyleManager.Engine.RegisterStyleHook(TListView, TListViewStyleHookFix);
  /// </code>
  /// </remarks>
  TListViewStyleHookFix = class(TListViewStyleHook)
    procedure DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: Integer;
      const Text: string; IsPressed, IsBackground: Boolean); override;
  end;
{$IFEND}

{$IF CompilerVersion <= 24.0}
type
  /// <summary> This interposer class fix the QC #114032  for Delphi XE2 and Delphi XE3
  /// </summary>
  /// <remarks>
  /// To use this class add the Vcl.Styles.Fixes unit to your uses list after of  the Vcl.ExtCtrls unit
  /// </remarks>
  TColorBox = class(Vcl.ExtCtrls.TColorBox)
  private
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  end;

  /// <summary> The <c>TComboBoxExStyleHookFix</c> vcl style hook fix the QC #108678 for Delphi XE2 and Delphi XE3
  /// </summary>
  /// <remarks>
  /// Use this hook in this way
  /// <code>
  /// TStyleManager.Engine.RegisterStyleHook(TComboBoxEx, TComboBoxExStyleHookFix);
  /// </code>
  /// </remarks>
  TComboBoxExStyleHookFix = class(TComboBoxExStyleHook)
  strict protected
    procedure DrawListBoxItem(ADC: HDC; ARect: TRect; AIndex: Integer;
      ASelected: Boolean);
    procedure ComboBoxWndProc(var Msg: TMessage); override;
    procedure DrawComboBox(DC: HDC); override;
  end;
{$IFEND}

{$IF CompilerVersion <= 26.0}
  /// <summary> The <c>TComboBoxStyleHookFix</c> vcl style hook fix the QC #114632 for Delphi XE5 and earlier
  /// </summary>
  /// <remarks>
  /// Use this hook in this way
  /// <code>
  /// TStyleManager.Engine.RegisterStyleHook(TComboBox, TComboBoxStyleHookFix);
  /// </code>
  /// </remarks>
  TComboBoxStyleHookFix = class(TComboBoxStyleHook)
  strict private
    FTempItemIndex: Integer;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  strict protected
    procedure DrawItem(Canvas: TCanvas; Index: Integer;
      const R: TRect; Selected: Boolean); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;
{$IFEND}

implementation

uses
  Winapi.CommCtrl,
  Vcl.Themes,
  Vcl.Forms,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Types;

type
  TCustomButtonClass = class(TCustomButton);
  TWinControlClass = class(TWinControl);

{$IF CompilerVersion >= 23.0}

  // we need this helper to access some strict private fields
  TButtonStyleHookHelper = class Helper for TButtonStyleHook
  protected
    function Pressed: Boolean;
    function DropDown: Boolean;
  end;
{$IFEND}

{$IF CompilerVersion <= 27.0}
  TListViewStyleHookHelper = class helper for TListViewStyleHook
    function HeaderHandle: HWnd;
  end;
{$IFEND}

{$IF CompilerVersion <= 24.0}
  TComboBoxExStyleHookHelper = class helper for TComboBoxExStyleHook
    function DroppedDown: Boolean;
  end;
{$IFEND}

{$IF CompilerVersion <= 26.0}
  TComboBoxStyleHookHelper = class helper for TComboBoxStyleHook
  strict private
    function _getDroppedDown: Boolean;
  private
    property _DroppedDown : Boolean read _getDroppedDown;
  end;
{$IFEND}

{$IF CompilerVersion >= 23.0}

procedure TButtonStyleHookFix.Paint(Canvas: TCanvas);
const
  PBS_NORMAL = 0;
  PBS_HOT = 1;
  PBS_PRESSED = 2;
  PBS_DISABLED = 3;
  PBS_DEFAULTED = 4;
  PBS_STYLUSHOT = 5;
var
  LDetails: TThemedElementDetails;
  DrawRect: TRect;
  pbuttonImagelist: BUTTON_IMAGELIST;
  IW, IH, IY: Integer;
  LTextFormatFlags: TTextFormatFlags;
  ThemeTextColor: TColor;
  Buffer: string;
  BufferLength: Integer;
  SaveIndex: Integer;
  X, Y, I: Integer;
  IsDefault: Boolean;
  BCaption: String;
  LImageIndex: Integer;
begin
  LImageIndex:=PBS_NORMAL;
  IsDefault := (Control is TCustomButton) and (TCustomButton(Control).Default);

  if StyleServices.Available then
  begin
    BCaption := Text;

    if not Control.Enabled then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled);
      LImageIndex := PBS_DISABLED;
    end
    else
    if Pressed then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
      LImageIndex := PBS_PRESSED;
    end
    else
    if MouseInControl then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonHot);
      LImageIndex := PBS_HOT;
    end
    else
    if Control.Focused or (IsDefault and (Screen.ActiveControl<>nil) and not (Screen.ActiveControl is TCustomButton) ) then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonDefaulted);
      LImageIndex := PBS_DEFAULTED;
    end
    else if Control.Enabled then
      LDetails := StyleServices.GetElementDetails(tbPushButtonNormal);

    DrawRect := Control.ClientRect;
    StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);

    if Button_GetImageList(Handle, pbuttonImagelist) and
      (pbuttonImagelist.himl <> 0) and
      ImageList_GetIconSize(pbuttonImagelist.himl, IW, IH) then
    begin

      if (GetWindowLong(Handle, GWL_STYLE) and BS_COMMANDLINK) = BS_COMMANDLINK
      then
        IY := DrawRect.Top + 15
      else
        IY := DrawRect.Top + (DrawRect.Height - IH) div 2;

      // here the image is drawn properly according to the ImageAlignment value
      case TCustomButton(Control).ImageAlignment of
        TImageAlignment.iaLeft:
          begin
            ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle,
              DrawRect.Left + 3 + TCustomButton(Control).ImageMargins.Left, IY, ILD_NORMAL);
            Inc(DrawRect.Left, IW + 3 + TCustomButton(Control).ImageMargins.Left);
          end;

        TImageAlignment.iaRight:
          begin
            ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle,
              DrawRect.Right - IW - 3  - TCustomButton(Control).ImageMargins.Right, IY, ILD_NORMAL);
            Dec(DrawRect.Right, IW - 3 + TCustomButton(Control).ImageMargins.Right);
          end;

        TImageAlignment.iaCenter:
          begin
            ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle,
              (DrawRect.Right - IW) div 2, IY + TCustomButton(Control).ImageMargins.Top - TCustomButton(Control).ImageMargins.Bottom, ILD_NORMAL);
          end;

        TImageAlignment.iaTop:
          begin
            ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle,
              (DrawRect.Right - IW) div 2, 3 + TCustomButton(Control).ImageMargins.Top - TCustomButton(Control).ImageMargins.Bottom, ILD_NORMAL);
          end;

        TImageAlignment.iaBottom:
          begin
            ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle,
              (DrawRect.Right - IW) div 2, (DrawRect.Height - IH) - 3 + TCustomButton(Control).ImageMargins.Top - TCustomButton(Control).ImageMargins.Bottom,
              ILD_NORMAL);
          end;

      end;

    end;

    if (GetWindowLong(Handle, GWL_STYLE) and BS_COMMANDLINK) = BS_COMMANDLINK  then
    begin
      if pbuttonImagelist.himl = 0 then
        Inc(DrawRect.Left, 35);

      Inc(DrawRect.Top, 15);
      Inc(DrawRect.Left, 5);
      Canvas.Font := TCustomButtonClass(Control).Font;
      Canvas.Font.Style := [];
      Canvas.Font.Size := 12;
      LTextFormatFlags := TTextFormatFlags(DT_LEFT);
      if StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor)
      then
        Canvas.Font.Color := ThemeTextColor;
      StyleServices.DrawText(Canvas.Handle, LDetails, BCaption, DrawRect,
        LTextFormatFlags, Canvas.Font.Color);
      SetLength(Buffer, Button_GetNoteLength(Handle) + 1);
      if Length(Buffer) <> 0 then
      begin
        BufferLength := Length(Buffer);
        if Button_GetNote(Handle, PChar(Buffer), BufferLength) then
        begin
          LTextFormatFlags := TTextFormatFlags(DT_LEFT or DT_WORDBREAK);
          Inc(DrawRect.Top, Canvas.TextHeight('Wq') + 2);
          Canvas.Font.Size := 8;
          StyleServices.DrawText(Canvas.Handle, LDetails, Buffer, DrawRect,
            LTextFormatFlags, Canvas.Font.Color);
        end;
      end;

      if pbuttonImagelist.himl = 0 then
      begin
        if Pressed then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphPressed)
        else if MouseInControl then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphHot)
        else if Control.Enabled then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphNormal)
        else
          LDetails := StyleServices.GetElementDetails
            (tbCommandLinkGlyphDisabled);
        DrawRect.Right := 35;
        DrawRect.Left := 3;
        DrawRect.Top := 10;
        DrawRect.Bottom := DrawRect.Top + 32;
        StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);
      end;

    end
    else if (GetWindowLong(Handle, GWL_STYLE) and BS_SPLITBUTTON) = BS_SPLITBUTTON
    then
    begin
      Dec(DrawRect.Right, 15);
      DrawControlText(Canvas, LDetails, Text, DrawRect, DT_VCENTER or
        DT_CENTER);
      if DropDown then
      begin
        LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
        SaveIndex := SaveDC(Canvas.Handle);
        try
          IntersectClipRect(Canvas.Handle, Control.Width - 15, 0, Control.Width,
            Control.Height);
          DrawRect := Rect(Control.Width - 30, 0, Control.Width,
            Control.Height);
          StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);
        finally
          RestoreDC(Canvas.Handle, SaveIndex);
        end;
      end;

      with Canvas do
      begin
        Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
        MoveTo(Control.Width - 15, 3);
        LineTo(Control.Width - 15, Control.Height - 3);
        if Control.Enabled then
          Pen.Color := StyleServices.GetSystemColor(clBtnHighLight)
        else
          Pen.Color := Font.Color;
        MoveTo(Control.Width - 14, 3);
        LineTo(Control.Width - 14, Control.Height - 3);
        Pen.Color := Font.Color;
        X := Control.Width - 8;
        Y := Control.Height div 2 + 1;
        for I := 3 downto 0 do
        begin
          MoveTo(X - I, Y - I);
          LineTo(X + I + 1, Y - I);
        end;
      end;

    end
    else
    begin
      // finally the text is aligned and drawn depending of the value of the ImageAlignment property
      case TCustomButton(Control).ImageAlignment of
        TImageAlignment.iaLeft,
        TImageAlignment.iaRight,
        TImageAlignment.iaCenter:  if (Control is TCustomButton) and TCustomButtonClass(Control).WordWrap then
                                    DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_VCENTER or DT_CENTER or DT_WORDBREAK or Control.DrawTextBiDiModeFlags(0))
                                   else
                                    DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_VCENTER or DT_CENTER or Control.DrawTextBiDiModeFlags(0));

        TImageAlignment.iaBottom:  if (Control is TCustomButton) and TCustomButtonClass(Control).WordWrap then
                                     DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_TOP or DT_CENTER or DT_WORDBREAK or Control.DrawTextBiDiModeFlags(0))
                                   else
                                     DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_TOP or DT_CENTER or Control.DrawTextBiDiModeFlags(0));

        TImageAlignment.iaTop:     if (Control is TCustomButton) and TCustomButtonClass(Control).WordWrap then
                                     DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_BOTTOM or DT_CENTER or DT_WORDBREAK or Control.DrawTextBiDiModeFlags(0))
                                   else
                                     DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_BOTTOM or DT_CENTER or Control.DrawTextBiDiModeFlags(0));
      end;
    end;
  end;
end;

{ TButtonStyleHookHelper }

function TButtonStyleHookHelper.DropDown: Boolean;
begin
  Result := Self.FDropDown;
end;

function TButtonStyleHookHelper.Pressed: Boolean;
begin
  Result := Self.FPressed;
end;
{$IFEND}


{$IF CompilerVersion <= 27.0}
{ TListViewStyleHookHelper }
function TListViewStyleHookHelper.HeaderHandle: HWnd;
begin
  Result := Self.FHeaderHandle;
end;
{$IFEND}

{$IF CompilerVersion <= 24.0}
{ TComboBoxExStyleHookHelper }
function TComboBoxExStyleHookHelper.DroppedDown: Boolean;
begin
  Exit(Self.FDroppedDown);
end;
{$IFEND}

{$IF CompilerVersion <= 27.0}

{ TListViewStyleHookFix }

procedure TListViewStyleHookFix.DrawHeaderSection(Canvas: TCanvas; R: TRect;
  Index: Integer; const Text: string; IsPressed, IsBackground: Boolean);
var
  Item: THDItem;
  ImageList: HIMAGELIST;
  DrawState: TThemedHeader;
  IconWidth, IconHeight: Integer;
  Details: TThemedElementDetails;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Mask := HDI_FORMAT;
  Header_GetItem(HeaderHandle, Index, Item);
  if IsBackground then
    DrawState := thHeaderItemNormal
  else if IsPressed then
    DrawState := thHeaderItemPressed
  else
    DrawState := thHeaderItemNormal;

  Details := StyleServices.GetElementDetails(DrawState);
  StyleServices.DrawElement(Canvas.Handle, Details, R);

  ImageList := SendMessage(HeaderHandle, HDM_GETIMAGELIST, 0, 0);
  Item.Mask := HDI_FORMAT or HDI_IMAGE;
  InflateRect(R, -2, -2);
  IconWidth := 0;
  if (ImageList <> 0) and Header_GetItem(HeaderHandle, Index, Item) then
  begin
    if Item.fmt and HDF_IMAGE = HDF_IMAGE then
      ImageList_Draw(ImageList, Item.iImage, Canvas.Handle, R.Left, R.Top,
        ILD_TRANSPARENT);
    ImageList_GetIconSize(ImageList, IconWidth, IconHeight);
    Inc(R.Left, IconWidth + 5);
  end;

  if IconWidth = 0 then Inc(R.Left, 2);
  DrawControlText(Canvas, Details, Text, R, DT_VCENTER or DT_LEFT or
    DT_SINGLELINE or DT_END_ELLIPSIS);
end;
{$IFEND}

{$IF CompilerVersion <= 24.0}
{ TColorBox }

procedure TColorBox.CNDrawItem(var Message: TWMDrawItem);
const
  ColorStates: array [Boolean] of TStyleColor = (scComboBoxDisabled,
    scComboBox);
  FontStates: array [Boolean] of TStyleFont = (sfComboBoxItemDisabled,
    sfComboBoxItemNormal);
var
  LState: TOwnerDrawState;
begin
  LState := TOwnerDrawState(LoWord(Message.DrawItemStruct^.itemState));
  if Message.DrawItemStruct^.itemState and ODS_COMBOBOXEDIT <> 0 then
    Include(LState, odComboBoxEdit);
  if Message.DrawItemStruct^.itemState and ODS_DEFAULT <> 0 then
    Include(LState, odDefault);
  Canvas.Handle := Message.DrawItemStruct^.HDC;
  Canvas.Font := Font;
  if TStyleManager.IsCustomStyleActive then
  begin
{$IF CompilerVersion<=23}  //XE2
    Canvas.Brush.Color := StyleServices.GetStyleColor(ColorStates[Enabled]);
    Canvas.Font.Color := StyleServices.GetStyleFontColor(FontStates[Enabled]);
{$ELSE}
    if seClient in StyleElements then
      Canvas.Brush.Color := StyleServices.GetStyleColor(ColorStates[Enabled])
    else
      Canvas.Brush := Brush;
    if seFont in StyleElements then
      Canvas.Font.Color := StyleServices.GetStyleFontColor(FontStates[Enabled]);
{$IFEND}
  end
  else
    Canvas.Brush := Brush;
  if (Integer(Message.DrawItemStruct^.itemID) >= 0) and
    (odSelected in LState){$IF CompilerVersion>23}  and (seClient in StyleElements) {$IFEND} then
  begin
    if TStyleManager.IsCustomStyleActive then
    begin
      Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
      Canvas.Font.Color := StyleServices.GetSystemColor(clHighlightText);
    end
    else
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText
    end;
  end;

  if Integer(Message.DrawItemStruct^.itemID) >= 0 then
    DrawItem(Message.DrawItemStruct^.itemID,
      Message.DrawItemStruct^.rcItem, LState)
  else
    Canvas.FillRect(Message.DrawItemStruct^.rcItem);

  if (odFocused in LState) and (TStyleManager.ActiveStyle.IsSystemStyle) then
    DrawFocusRect(Message.DrawItemStruct^.HDC, Message.DrawItemStruct^.rcItem);
  Canvas.Handle := 0;
end;

{ TComboBoxExStyleHookFix }

procedure TComboBoxExStyleHookFix.ComboBoxWndProc(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_DRAWITEM:
      begin
        DrawListBoxItem(TWMDrawItem(Msg).DrawItemStruct.HDC,
          TWMDrawItem(Msg).DrawItemStruct.rcItem,
          TWMDrawItem(Msg).DrawItemStruct.itemID,
          TWMDrawItem(Msg).DrawItemStruct.itemState and ODS_SELECTED <> 0);
      end
  else
    inherited;
  end;
end;

procedure TComboBoxExStyleHookFix.DrawComboBox(DC: HDC);
var
  DX, DY: Integer;
  LCanvas: TCanvas;
  LDetails: TThemedElementDetails;
  LRect: TRect;
  LThemedComboBox: TThemedComboBox;
  LCaption: string;
  LBitmap: TBitmap;
  LDrawState: TThemedComboBox;
begin
  if not StyleServices.Available or (Control.Width = 0) or (Control.Height = 0)
  then
    Exit;

  LCanvas := TCanvas.Create;
  try
    LCanvas.Handle := DC;
    LBitmap := TBitmap.Create;
    try
      LBitmap.Width := Control.Width;
      LBitmap.Height := Control.Height;
      if not Control.Enabled then
        LDrawState := tcBorderDisabled
      else if Control.Focused then
        LDrawState := tcBorderFocused
      else if MouseInControl then
        LDrawState := tcBorderHot
      else
        LDrawState := tcBorderNormal;

      LRect := Rect(0, 0, Control.Width, Control.Height);
      LDetails := StyleServices.GetElementDetails(LDrawState);
      StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, LRect);

{$IF CompilerVersion > 23.0}
      if not(seClient in Control.StyleElements) then
      begin
        LRect := Control.ClientRect;
        InflateRect(LRect, -3, -3);
        LRect.Right := ButtonRect.Left - 2;
        LBitmap.Canvas.Brush.Color := TWinControlClass(Control).Color;
        LBitmap.Canvas.FillRect(LRect);
      end;
{$IFEND}
      if not Control.Enabled then
        LThemedComboBox := tcDropDownButtonDisabled
      else if DroppedDown then
        LThemedComboBox := tcDropDownButtonPressed
      else if MouseOnButton then
        LThemedComboBox := tcDropDownButtonHot
      else
        LThemedComboBox := tcDropDownButtonNormal;

      if TCustomComboBoxEx(Control).Style <> csExSimple then
      begin
        LDetails := StyleServices.GetElementDetails(LThemedComboBox);
        StyleServices.DrawElement(LBitmap.Canvas.Handle, LDetails, ButtonRect);
      end;

      LRect := Control.ClientRect;
      InflateRect(LRect, -3, -3);
      LRect.Right := ButtonRect.Left - 2;
      LBitmap.Canvas.Font.Assign(TComboBoxEx(Control).Font);
{$IF CompilerVersion > 23.0}
      if seFont in Control.StyleElements then
{$IFEND}
        if Control.Enabled then
          LBitmap.Canvas.Font.Color := StyleServices.GetStyleFontColor
            (sfComboBoxItemNormal)
        else
          LBitmap.Canvas.Font.Color := StyleServices.GetStyleFontColor
            (sfComboBoxItemDisabled);

      if TComboBoxEx(Control).Style = csExDropDownList then
      begin
        if TComboBoxEx(Control).Focused then
        begin
          if TComboBoxEx(Control).ItemIndex <> -1 then
          begin
            LBitmap.Canvas.Brush.Color := StyleServices.GetSystemColor
              (clHighlight);
            LBitmap.Canvas.Brush.Style := bsSolid;
            LBitmap.Canvas.FillRect(LRect);
            LBitmap.Canvas.Font.Color := StyleServices.GetSystemColor
              (clHighlightText);
          end;
          LBitmap.Canvas.DrawFocusRect(LRect);
        end
        else
        begin
          LBitmap.Canvas.Brush.Color := Self.Brush.Color;
          LBitmap.Canvas.Brush.Style := bsSolid;
          LBitmap.Canvas.FillRect(LRect);
        end;
      end;

      if TComboBoxEx(Control).Style <> csExSimple then
      begin
        { image }
        if (TComboBoxEx(Control).Images <> nil) and
          (TComboBoxEx(Control).ItemIndex <> -1) then
        begin
          DX := 5;
          DY := LRect.Top + LRect.Height div 2 - TComboBoxEx(Control)
            .Images.Height div 2;
          if DY < LRect.Top then
            DY := LRect.Top;
          if (TComboBoxEx(Control).ItemsEx[TComboBoxEx(Control).ItemIndex]
            .ImageIndex >= 0) and
            (TComboBoxEx(Control).ItemsEx[TComboBoxEx(Control).ItemIndex]
            .ImageIndex < TComboBoxEx(Control).Images.Count) then
            TComboBoxEx(Control).Images.Draw(LBitmap.Canvas, DX, DY,
              TComboBoxEx(Control).ItemsEx[TComboBoxEx(Control).ItemIndex]
              .ImageIndex, Control.Enabled);

          LRect.Left := DX + TComboBoxEx(Control).Images.Width + 5;
        end
        else
          Inc(LRect.Left, 5);
        { text }
        if (TComboBoxEx(Control).ItemIndex <> -1) then
        begin
          LBitmap.Canvas.Brush.Style := bsClear;
          LCaption := TComboBoxEx(Control).ItemsEx
            [TComboBoxEx(Control).ItemIndex].Caption;
          if LCaption <> '' then
            DrawText(LBitmap.Canvas.Handle, PWideChar(LCaption),
              Length(LCaption), LRect, DT_LEFT OR DT_VCENTER or DT_SINGLELINE);
        end;
      end;

      LCanvas.Draw(0, 0, LBitmap);
    finally
      LBitmap.Free;
    end;
  finally
    LCanvas.Handle := 0;
    LCanvas.Free;
  end;
  Handled := True;
end;

procedure TComboBoxExStyleHookFix.DrawListBoxItem(ADC: HDC; ARect: TRect;
  AIndex: Integer; ASelected: Boolean);
var
  LCanvas: TCanvas;
  Offset: Integer;
  DX, DY: Integer;
  Buffer: TBitmap;
  LCaption: String;
  LRect: TRect;
begin
  if (AIndex < 0) or (AIndex >= TComboBoxEx(Control).ItemsEx.Count) then
    Exit;
  LCanvas := TCanvas.Create;
  LCanvas.Handle := ADC;
  Buffer := TBitmap.Create;
  Buffer.Width := ARect.Width;
  Buffer.Height := ARect.Height;
  try
    Buffer.Canvas.Font.Assign(TComboBoxEx(Control).Font);
    begin
      { background }
      Buffer.Canvas.Brush.Style := bsSolid;
      if ASelected then
      begin
        Buffer.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
        Buffer.Canvas.Font.Color := StyleServices.GetSystemColor
          (clHighlightText);
      end
      else
      begin
{$IF CompilerVersion > 23.0}
        if seClient in Control.StyleElements then
          Buffer.Canvas.Brush.Color := StyleServices.GetStyleColor(scComboBox)
        else
          Buffer.Canvas.Brush.Color := TWinControlClass(Control).Color;
{$ELSE}
        Buffer.Canvas.Brush.Color := StyleServices.GetStyleColor(scComboBox);
{$IFEND}
{$IF CompilerVersion > 23.0}
        if seFont in Control.StyleElements then
          Buffer.Canvas.Font.Color := StyleServices.GetStyleFontColor
            (sfComboBoxItemNormal)
        else
          Buffer.Canvas.Font.Color := TWinControlClass(Control).Font.Color;
{$ELSE}
        Buffer.Canvas.Font.Color := StyleServices.GetStyleFontColor
          (sfComboBoxItemNormal);
{$IFEND}
      end;
      Buffer.Canvas.FillRect(Rect(0, 0, Buffer.Width, Buffer.Height));
      Offset := TComboExItem(TComboBoxEx(Control).ItemsEx[AIndex]).Indent;
      if Offset > 0 then
        Offset := (Offset * 10) + 5
      else
        Offset := 5;
      { image }
      if (TComboBoxEx(Control).Images <> nil) then
      begin
        DX := Offset;
        DY := Buffer.Height div 2 - TComboBoxEx(Control).Images.Height div 2;
        if DY < 0 then
          DY := 0;
        if (TComboBoxEx(Control).ItemsEx[AIndex].ImageIndex >= 0) and
          (TComboBoxEx(Control).ItemsEx[AIndex].ImageIndex <
          TComboBoxEx(Control).Images.Count) then
          TComboBoxEx(Control).Images.Draw(Buffer.Canvas, DX, DY,
            TComboBoxEx(Control).ItemsEx[AIndex].ImageIndex, True);
        Offset := Offset + TComboBoxEx(Control).Images.Width + 5;
      end;
      { text }
      LRect := Rect(Offset, 0, Buffer.Width, Buffer.Height);
      Buffer.Canvas.Brush.Style := bsClear;
      LCaption := TComboBoxEx(Control).ItemsEx[AIndex].Caption;
      if LCaption <> '' then
        DrawText(Buffer.Canvas.Handle, PWideChar(LCaption), Length(LCaption),
          LRect, DT_LEFT OR DT_VCENTER or DT_SINGLELINE);
    end;
    LCanvas.Draw(ARect.Left, ARect.Top, Buffer);
  finally
    Buffer.Free;
    LCanvas.Handle := 0;
    LCanvas.Free;
  end;
end;
{$IFEND}

{$IF CompilerVersion <= 26.0}
constructor TComboBoxStyleHookFix.Create(AControl: TWinControl);
begin
  inherited;
  FTempItemIndex := -1;
end;

procedure TComboBoxStyleHookFix.WMCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or (Message.NotifyCode = CBN_SELENDOK) or
     (Message.NotifyCode = CBN_CLOSEUP) or (Message.NotifyCode = CBN_DROPDOWN) or
     (Message.NotifyCode = CBN_SELCHANGE) then
  begin
    if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE) then
      FTempItemIndex := TComboBox(Control).ItemIndex;
  end;
  inherited;
end;

procedure TComboBoxStyleHookFix.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = CBN_SELENDCANCEL) or (Message.NotifyCode = CBN_SELENDOK) or
     (Message.NotifyCode = CBN_CLOSEUP) or (Message.NotifyCode = CBN_DROPDOWN) or
     (Message.NotifyCode = CBN_SELCHANGE)  then
  begin
    if (Message.NotifyCode = CBN_DROPDOWN) or (Message.NotifyCode = CBN_SELCHANGE) then
      FTempItemIndex := TComboBox(Control).ItemIndex;
  end;
  inherited;
end;

procedure TComboBoxStyleHookFix.DrawItem(Canvas: TCanvas; Index: Integer;
      const R: TRect; Selected: Boolean);
begin
  if _DroppedDown then
    inherited DrawItem(Canvas, FTempItemIndex, R, Selected)
  else
    inherited;
end;

function TComboBoxStyleHookHelper._getDroppedDown: Boolean;
begin
  Result := Self.DroppedDown;
end;
{$IFEND}

end.
