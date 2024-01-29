unit VirtualTrees.AncestorVCL;

{$SCOPEDENUMS ON}

{****************************************************************************************************************}
{ Project          : VirtualTrees                                                                                }
{                                                                                                                }
{ author           : Karol Bieniaszewski, look at VirtualTrees.pas as some code moved from there                 }
{ year             : 2022                                                                                        }
{ contibutors      :                                                                                             }
{****************************************************************************************************************}

interface

uses
  Vcl.Controls,
  Vcl.Themes,
  Winapi.Messages,
  Winapi.Windows,
  Winapi.oleacc,
  Winapi.ActiveX,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  TVTRenderOLEDataEvent = procedure(Sender: TBaseVirtualTree; const FormatEtcIn: TFormatEtc; out Medium: TStgMedium;
    ForClipboard: Boolean; var Result: HRESULT) of object;

  TVTAncestorVcl = class abstract(TBaseVirtualTree)
  private
    FOnRenderOLEData: TVTRenderOLEDataEvent;     // application/descendant defined clipboard formats

  protected
    function GetHintWindowClass: THintWindowClass; override;
    function GetTreeFromDataObject(const DataObject: TVTDragDataObject): TBaseVirtualTree; override;
    function DoRenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HRESULT; override;
    property OnRenderOLEData: TVTRenderOLEDataEvent read FOnRenderOLEData write FOnRenderOLEData;
  public //methods
    function PasteFromClipboard(): Boolean; override;
 end;

   // The trees need an own hint window class because of Unicode output and adjusted font.
  TVirtualTreeHintWindow = class(THintWindow)
  strict private
    FHintData: TVTHintData;
    FTextHeight: TDimension;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  strict protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    // Mitigator function to use the correct style service for this context (either the style assigned to the control for Delphi > 10.4 or the application style)
    function StyleServices(AControl: TControl = nil): TCustomStyleServices;
  public
    function CalcHintRect(MaxWidth: TDimension; const AHint: string; AData: Pointer): TRect; override;
    function IsHintMsg(var Msg: TMsg): Boolean; override;
  end;

implementation
uses
  System.Classes,
  Vcl.Graphics,
  System.UITypes,
  Vcl.AxCtrls,
  Vcl.Forms,
  Vcl.GraphUtil,
  VirtualTrees.ClipBoard,
  VirtualTrees.DataObject,
  VirtualTrees.DragnDrop,
  VirtualTrees.StyleHooks;

resourcestring
  SClipboardFailed = 'Clipboard operation failed.';

type
  TBVTCracker = class(TBaseVirtualTree);

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorVcl.DoRenderOLEData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium; ForClipboard: Boolean): HRESULT;
begin
  Result := E_FAIL;
  if Assigned(FOnRenderOLEData) then
    FOnRenderOLEData(Self, FormatEtcIn, Medium, ForClipboard, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorVcl.GetHintWindowClass: THintWindowClass;

// Returns the default hint window class used for the tree. Descendants can override it to use their own classes.

begin
  Result := TVirtualTreeHintWindow;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorVcl.GetTreeFromDataObject(const DataObject: TVTDragDataObject): TBaseVirtualTree;

// Returns the owner/sender of the given data object by means of a special clipboard format
// or nil if the sender is in another process or no virtual tree at all.

var
  Medium: TStgMedium;
  Data: PVTReference;

begin
  Result := nil;
  if Assigned(DataObject) then
  begin
    StandardOLEFormat.cfFormat := CF_VTREFERENCE;
    if DataObject.GetData(StandardOLEFormat, Medium) = S_OK then
    begin
      Data := GlobalLock(Medium.hGlobal);
      if Assigned(Data) then
      begin
        if Data.Process = GetCurrentProcessID then
          Result := Data.Tree;
        GlobalUnlock(Medium.hGlobal);
      end;
      ReleaseStgMedium(Medium);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTAncestorVcl.PasteFromClipboard(): Boolean;

// Reads what is currently on the clipboard into the tree (if the format is supported).
// Note: If the application wants to have text or special formats to be inserted then it must implement
//       its own code (OLE). Here only the native tree format is accepted.

var
  Data: IDataObject;
  Source: TBaseVirtualTree;

begin
  Result := False;
  if not (toReadOnly in TreeOptions.MiscOptions) then
  begin
    if OleGetClipboard(Data) <> S_OK then
      RaiseVTError(SClipboardFailed, hcTFClipboardFailed)
    else
    begin
      // Try to get the source tree of the operation to optimize the operation.
      Source := GetTreeFromDataObject(Data);
      Result := ProcessOLEData(Source, Data, FocusedNode, DefaultPasteMode, Assigned(Source) and
        (tsCutPending in Source.TreeStates));
      if Assigned(Source) then
      begin
        if Source <> Self then
          Source.FinishCutOrCopy
        else
          DoStateChange([], [tsCutPending]);
      end;
    end;
  end;
end;

//----------------- TVirtualTreeHintWindow -----------------------------------------------------------------------------

procedure TVirtualTreeHintWindow.CMTextChanged(var Message: TMessage);

begin
  // swallow this message to prevent the ancestor from resizing the window (we don't use the caption anyway)
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeHintWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);

// The control is fully painted by own code so don't erase its background as this causes flickering.

begin
  Message.Result := 1;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeHintWindow.CreateParams(var Params: TCreateParams);

begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := WS_POPUP;
    ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeHintWindow.Paint();
var
  R: TRect;
  Y: Integer;
  S: string;
  DrawFormat: Cardinal;
  HintKind: TVTHintKind;
  LClipRect: TRect;

  LColor: TColor;
  LDetails: TThemedElementDetails;
  LGradientStart: TColor;
  LGradientEnd: TColor;

begin
  with FHintData do
  begin
    // Do actual painting only in the very first run.
    // If the given node is nil then we have to display a header hint.
    if (Node = nil) or (TBVTCracker(Tree).HintMode <> hmToolTip) then
    begin
      Canvas.Font := Screen.HintFont;
      Canvas.Font.Height := MulDiv(Canvas.Font.Height, Tree.ScaledPixels(96), Screen.PixelsPerInch); // See issue #992
      Y := 2;
    end
    else
    begin
      Tree.GetTextInfo(Node, Column, Canvas.Font, R, S);
      if LineBreakStyle = hlbForceMultiLine then
        Y := 1
      else
        Y := (R.Top - R.Bottom  + Self.Height) div 2;
    end;

    R := Rect(0, 0, Width, Height);

    HintKind := vhkText;
    if Assigned(Node) then
      TBVTCracker(Tree).DoGetHintKind(Node, Column, HintKind);

    if HintKind = vhkOwnerDraw then
    begin
      TBVTCracker(Tree).DoDrawHint(Canvas, Node, R, Column);
    end
    else
      with Canvas do
      begin
        if TBVTCracker(Tree).VclStyleEnabled  then
        begin
          InflateRect(R, -1, -1); // Fixes missing border when VCL styles are used
          LDetails := StyleServices(Tree).GetElementDetails(thHintNormal);
          if StyleServices(Tree).GetElementColor(LDetails, ecGradientColor1, LColor) and (LColor <> clNone) then
            LGradientStart := LColor
          else
            LGradientStart := clInfoBk;
          if StyleServices(Tree).GetElementColor(LDetails, ecGradientColor2, LColor) and (LColor <> clNone) then
            LGradientEnd := LColor
          else
            LGradientEnd := clInfoBk;
          if StyleServices(Tree).GetElementColor(LDetails, ecTextColor, LColor) and (LColor <> clNone) then
            Font.Color := LColor
          else
            Font.Color := Screen.HintFont.Color;
          GradientFillCanvas(Canvas, LGradientStart, LGradientEnd, R, gdVertical);
        end
        else
        begin
          // Still force tooltip back and text color.
          Font.Color := clInfoText;
          Pen.Color := clBlack;
          Brush.Color := clInfoBk;
          if StyleServices(Tree).Enabled and ((toThemeAware in TBVTCracker(Tree).TreeOptions.PaintOptions) or
             (toUseExplorerTheme in TBVTCracker(Tree).TreeOptions.PaintOptions)) then
          begin
            if toUseExplorerTheme in TBVTCracker(Tree).TreeOptions.PaintOptions then // ToolTip style
              StyleServices(Tree).DrawElement(Canvas.Handle, StyleServices(Tree).GetElementDetails(tttStandardNormal), R {$IF CompilerVersion >= 34}, nil, FCurrentPPI{$IFEND})
            else
              begin // Hint style
                LClipRect := R;
                InflateRect(R, 4, 4);
                StyleServices(Tree).DrawElement(Handle, StyleServices(Tree).GetElementDetails(tttStandardNormal), R, @LClipRect{$IF CompilerVersion >= 34}, FCurrentPPI{$IFEND});
                R := LClipRect;
                StyleServices(Tree).DrawEdge(Handle, StyleServices(Tree).GetElementDetails(twWindowRoot), R, [eeRaisedOuter], [efRect]);
              end;
          end
          else
            if TBVTCracker(Tree).VclStyleEnabled then
              StyleServices(Tree).DrawElement(Canvas.Handle, StyleServices(Tree).GetElementDetails(tttStandardNormal), R {$IF CompilerVersion >= 34}, nil, FCurrentPPI{$IFEND})
            else
              Rectangle(R);
        end;
        // Determine text position and don't forget the border.
        InflateRect(R, -1, -1);
        DrawFormat := DT_TOP or DT_NOPREFIX;
        SetBkMode(Handle, Winapi.Windows.TRANSPARENT);
        R.Top := Y;
        R.Left := R.Left + 3; // Make the text more centered
        if Assigned(Node) and (LineBreakStyle = hlbForceMultiLine) then
          DrawFormat := DrawFormat or DT_WORDBREAK;
        Winapi.Windows.DrawTextW(Handle, PWideChar(HintText), Length(HintText), R, DrawFormat);
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeHintWindow.StyleServices(AControl: TControl): TCustomStyleServices;
begin
  Result := VTStyleServices(AControl);
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;

var
  TM: TTextMetric;
  R: TRect;

begin
  try
    if AData = nil then
      // Defensive approach, it *can* happen that AData is nil. Maybe when several user defined hint classes are used.
      Result := Rect(0, 0, 0, 0)
    else
    begin
      // The hint window does not need any bidi mode setting but the caller of this method (TApplication.ActivateHint)
      // does some unneccessary actions if the hint window is not left-to-right.
      // The text alignment is based on the bidi mode passed in the hint data, hence we can
      // simply set the window's mode to left-to-right (it might have been modified by the caller, if the
      // tree window is right-to-left aligned).
      BidiMode := bdLeftToRight;

      FHintData := PVTHintData(AData)^;

      with FHintData do
      begin
        // The draw tree gets its hint size by the application (but only if not a header hint is about to show).
        // If the user will be drawing the hint, it gets its hint size by the application
        // (but only if not a header hint is about to show).
        // This size has already been determined in CMHintShow.
        if Assigned(Node) and (not IsRectEmpty(HintRect)) then
          Result := HintRect
        else
        begin
          if Column <= NoColumn then
          begin
            BidiMode := Tree.BidiMode;
            Alignment := TBVTCracker(Tree).Alignment;
          end
          else
          begin
            BidiMode := Tree.Header.Columns[Column].BidiMode;
            Alignment := Tree.Header.Columns[Column].Alignment;
          end;

          if BidiMode <> bdLeftToRight then
            ChangeBidiModeAlignment(Alignment);

          if (Node = nil) or (TBVTCracker(Tree).HintMode <> hmToolTip) then
          begin
            Canvas.Font := Screen.HintFont;
            Canvas.Font.Height := MulDiv(Canvas.Font.Height, Tree.ScaledPixels(96), Screen.PixelsPerInch); // See issue #992
          end
          else
          begin
            Canvas.Font := Tree.Font;
            with TBVTCracker(Tree) do
              DoPaintText(Node, Self.Canvas, Column, ttNormal);
          end;

          GetTextMetrics(Canvas.Handle, TM);
          FTextHeight := TM.tmHeight;

          if Length(HintText) = 0 then
            Result := Rect(0, 0, 0, 0)
          else
          begin
            if Assigned(Node) and (TBVTCracker(Tree).HintMode = hmToolTip) then
            begin
              // Determine actual line break style depending on what was returned by the methods and what's in the node.
              if LineBreakStyle = hlbDefault then
                if (vsMultiline in Node.States) or HintText.Contains(#13) then
                  LineBreakStyle := hlbForceMultiLine
                else
                  LineBreakStyle := hlbForceSingleLine;

              // Hint for a node.
              if LineBreakStyle = hlbForceMultiLine then
              begin
                // Multiline tooltips use the columns width but extend the bottom border to fit the whole caption.
                Result := Tree.GetDisplayRect(Node, Column, True, False);
                R := Result;

                // On Windows NT/2K/XP the behavior of the tooltip is slightly different to that on Windows 9x/Me.
                // We don't have Unicode word wrap on the latter so the tooltip gets as wide as the largest line
                // in the caption (limited by carriage return), which results in unoptimal overlay of the tooltip.
                Winapi.Windows.DrawTextW(Canvas.Handle, PWideChar(HintText), Length(HintText), R, DT_CALCRECT or DT_WORDBREAK);
                if BidiMode = bdLeftToRight then
                  Result.Right := R.Right + TBVTCracker(Tree).TextMargin
                else
                  Result.Left := R.Left - TBVTCracker(Tree).TextMargin + 1;
                Result.Bottom := R.Bottom;

                Inc(Result.Right);

                // If the node height and the column width are both already large enough to cover the entire text,
                // then we don't need the hint, though.
                // However if the text is partially scrolled out of the client area then a hint is useful as well.
                if (Tree.Header.Columns.Count > 0) and ((Tree.NodeHeight[Node] + 2) >= (Result.Bottom - Result.Top)) and
                   ((Tree.Header.Columns[Column].Width + 2) >= (Result.Right - Result.Left)) and not
                   ((Result.Left < 0) or (Result.Right > Tree.ClientWidth + 3) or
                    (Result.Top < 0) or (Result.Bottom > Tree.ClientHeight + 3)) then
                begin
                  Result := Rect(0, 0, 0, 0);
                  Exit;
                end;
              end
              else
              begin
                Result := TBVTCracker(Tree).LastHintRect; // = Tree.GetDisplayRect(Node, Column, True, True, True); see TBaseVirtualTree.CMHintShow

                { Fixes issue #623

                  Measure the rectangle to draw the text. The width of the result
                  is always adjusted according to the hint text because it may
                  be a custom hint coming in which can be larger or smaller than
                  the node text.
                  Earlier logic was using the current width of the node that was
                  either cutting off the hint text or producing undesired space
                  on the right.
                }
                R := Rect(0, 0, MaxWidth, FTextHeight);
                Winapi.Windows.DrawTextW(Canvas.Handle, PWideChar(HintText), Length(HintText), R, DT_CALCRECT or DT_TOP or DT_NOPREFIX or DT_WORDBREAK);
                if R.Right <> result.right - result.left then
                begin
                  result.Right := result.Left + r.Right;

                  //Space on right--taken from the code in the hmHint branch below.
                  if Assigned(Tree) then
                    Inc(Result.Right, TBVTCracker(Tree).TextMargin + TBVTCracker(Tree).Margin + Tree.ScaledPixels(4));
                end;
                // Fix ends.

                if toShowHorzGridLines in TBVTCracker(Tree).TreeOptions.PaintOptions then
                  Dec(Result.Bottom);
              end;

              // Include a one pixel border.
              InflateRect(Result, 1, 1);

              // Make the coordinates relative. They will again be offset by the caller code.
              OffsetRect(Result, -Result.Left - 1, -Result.Top - 1);
            end
            else
            begin
              // Hint for a header or non-tooltip hint.

              // Start with the base size of the hint in client coordinates.
              Result := Rect(0, 0, MaxWidth, FTextHeight);
              // Calculate the true size of the text rectangle.
              Winapi.Windows.DrawTextW(Canvas.Handle, PWideChar(HintText), Length(HintText), Result, DT_CALCRECT or DT_TOP or DT_NOPREFIX or DT_WORDBREAK);
              // The height of the text plus 2 pixels vertical margin plus the border determine the hint window height.
              // Minus 4 because THintWindow.ActivateHint adds 4 to Rect.Bottom anyway. Note that it is not scaled because the RTL itself does not do any scaling either.
              Inc(Result.Bottom, Tree.ScaledPixels(6) - 4);
              // The text is centered horizontally with usual text margin for left and right borders (plus border).
              if not Assigned(Tree) then
                Exit; // Workaround, because we have seen several exceptions here caught by Eurekalog. Submitted as issue #114 to http://code.google.com/p/virtual-treeview/
              { Issue #623 Fix for strange space on the right.
                Original logic was adding FTextHeight. Changed it to add FMargin instead and
                it looks OK even if the hint font is larger.
              }
              Inc(Result.Right, TBVTCracker(Tree).TextMargin
                  + TBVTCracker(Tree).Margin + Tree.ScaledPixels(4)); //Issue #623 space on right
                  //+ FTextHeight); // Old code: We are extending the width here, but the text height scales with the text width and has a similar value as AveCharWdith * 2.
            end;
          end;
        end;
      end;
    end;
  except
    Application.HandleException(Self);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualTreeHintWindow.IsHintMsg(var Msg: TMsg): Boolean;

// The VCL is a bit too generous when telling that an existing hint can be cancelled. Need to specify further here.

begin
  Result := inherited IsHintMsg(Msg) and HandleAllocated and IsWindowVisible(Handle);
  // Avoid that mouse moves over the non-client area or cursor key presses cancel the current hint.
  if Result and ((Msg.Message = WM_NCMOUSEMOVE) or ((Msg.Message >= WM_KEYFIRST) and (Msg.Message <= WM_KEYLAST) and (Msg.wparam in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]))) then
    Result := False;
end;

end.
