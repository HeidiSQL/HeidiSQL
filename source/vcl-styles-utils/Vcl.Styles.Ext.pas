// **************************************************************************************************
//
// Unit Vcl.Styles.Ext
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
// The Original Code is Vcl.Styles.Ext.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2021 Rodrigo Ruz V.
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Ext;

interface

{$IF RTLVersion>=24}
{$LEGACYIFEND ON}
{$IFEND}
{$DEFINE USE_VCL_STYLESAPI}

uses
  System.Classes,
  System.Generics.Collections,
  Winapi.Windows,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls;

type
  TStyleHookList = TList<TStyleHookClass>;

type
  /// <summary> The <c>TVclStylesPreview</c> class, is a control for display  a preview of any Vcl style loaded
  /// </summary>
  /// <remarks>
  /// sample of use
  /// <code>
  /// var <para></para>
  /// StyleName: string;<para></para>
  /// SourceInfo: TSourceInfo;<para></para>
  /// LStyle: TCustomStyleServices;<para></para>
  /// FPreview: TVclStylesPreview;<para></para>
  /// begin<para></para>
  /// FPreview := TVclStylesPreview.Create(Self);<para></para>
  /// FPreview.Parent := PanelPreview;<para></para>
  /// FPreview.BoundsRect := PanelPreview.ClientRect;<para></para>
  /// StyleName := 'Carbon';<para></para>
  ///   if (StyleName &lt;&gt;'') and (not SameText(StyleName, 'Windows')) then<para></para>
  ///   begin<para></para>
  ///     TStyleManager.StyleNames;//call DiscoverStyleResources<para></para>
  ///     LStyle := TStyleManager.Style[StyleName];<para></para>
  ///     FPreview.Caption := StyleName;<para></para>
  ///     FPreview.Style := LStyle;<para></para>
  ///     TVclStylesPreviewClass(FPreview).Paint;<para></para>
  ///   end;<para></para>
  /// ....<para></para>
  /// end;<para></para>
  /// </code>
  /// </remarks>
  TVclStylesPreview = class(TCustomControl)
  private
    FStyle: TCustomStyleServices; // TCustomStyle;
    FIcon: HICON;
    FCaption: TCaption;
    FRegion: HRGN;
    FBitmap: TBitmap;
  protected
    procedure Paint; override;
  public
    property Icon: HICON read FIcon Write FIcon;
    property Style: TCustomStyleServices read FStyle Write FStyle;
    property Caption: TCaption read FCaption write FCaption;
    property BitMap: TBitmap read FBitmap write FBitmap;
    constructor Create(AControl: TComponent); override;
    destructor Destroy; override;
  end;

  TStyleServicesHandle = type Pointer;

  TSourceInfo = record
    Data: TStyleServicesHandle;
    StyleClass: TCustomStyleServicesClass;
  end;

{$REGION 'Documentation'}
  /// <summary>Helper class for the TStyleManager
  /// </summary>
{$ENDREGION}

  TStyleManagerHelper = Class Helper for TStyleManager
  strict private
    class function GetStyleSourceInfo(const StyleName: string): TSourceInfo; static;
    class function GetStyles: TList<TCustomStyleServices>;
    class function _GetStyles: TList<TCustomStyleServices>; static;
  public
    class function RegisteredStyles: TDictionary<string, TSourceInfo>;
{$REGION 'Documentation'}
    /// <summary>Get the TSourceInfo for a Style
    /// </summary>
{$ENDREGION}
    class property StyleSourceInfo[const StyleName: string]: TSourceInfo read GetStyleSourceInfo;
{$REGION 'Documentation'}
    /// <summary>Send the CM_CUSTOMSTYLECHANGED message to all the forms
    /// </summary>
{$ENDREGION}
    class procedure RefreshCurrentTheme;
{$REGION 'Documentation'}
    /// <summary>Return the loaded styles (TCustomStyleServices) in the system
    /// </summary>
{$ENDREGION}
    class property Styles: TList<TCustomStyleServices> read _GetStyles;
{$REGION 'Documentation'}
    /// <summary>Force to reload a modified vcl style
    /// </summary>
{$ENDREGION}
    class procedure ReloadStyle(const StyleName: string);
{$REGION 'Documentation'}
    /// <summary>remove a vcl style
    /// </summary>
{$ENDREGION}
    class procedure RemoveStyle(const StyleName: string);
    class function StyleLoaded(const StyleName: string): Boolean;
  end;

const
  VclStyles_MaxSysColor = 23;
  VclStyles_SysColors: array [0 .. VclStyles_MaxSysColor - 1] of TIdentMapEntry = (
    (Value: Vcl.Graphics.clActiveBorder;Name: 'clActiveBorder'),
    (Value: Vcl.Graphics.clActiveCaption; Name: 'clActiveCaption'),
    (Value: Vcl.Graphics.clBtnFace; Name: 'clBtnFace'),
    (Value: Vcl.Graphics.clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: Vcl.Graphics.clBtnShadow; Name: 'clBtnShadow'),
    (Value: Vcl.Graphics.clBtnText; Name: 'clBtnText'),
    (Value: Vcl.Graphics.clCaptionText; Name: 'clCaptionText'),
    (Value: Vcl.Graphics.clGrayText; Name: 'clGrayText'),
    (Value: Vcl.Graphics.clHighlight; Name: 'clHighlight'),
    (Value: Vcl.Graphics.clHighlightText; Name: 'clHighlightText'),
    (Value: Vcl.Graphics.clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: Vcl.Graphics.clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: Vcl.Graphics.clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: Vcl.Graphics.clInfoBk; Name: 'clInfoBk'),
    (Value: Vcl.Graphics.clInfoText; Name: 'clInfoText'),
    (Value: Vcl.Graphics.clMenu; Name: 'clMenu'),
    (Value: Vcl.Graphics.clMenuText; Name: 'clMenuText'),
    (Value: Vcl.Graphics.clScrollBar; Name: 'clScrollBar'),
    (Value: Vcl.Graphics.cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: Vcl.Graphics.cl3DLight; Name: 'cl3DLight'),
    (Value: Vcl.Graphics.clWindow; Name: 'clWindow'),
    (Value: Vcl.Graphics.clWindowFrame; Name: 'clWindowFrame'),
    (Value: Vcl.Graphics.clWindowText; Name: 'clWindowText'));

procedure ApplyEmptyVCLStyleHook(ControlClass: TClass);
procedure RemoveEmptyVCLStyleHook(ControlClass: TClass);
function IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass): Boolean;
function GetRegisteredStylesHooks(ControlClass: TClass): TStyleHookList;
procedure DrawSampleWindow(Style: TCustomStyle; Canvas: TCanvas; ARect: TRect; const ACaption: string;
  HICON: HICON = 0); overload;

{$IFDEF USE_VCL_STYLESAPI}

type
  TCustomStyleExt = class(TCustomStyle)
  strict private
    FStream: TStream;
  public
    function GetStyleInfo: TStyleInfo;
  private
    function GetBitmapList: TObjectList<TBitmap>;
    procedure SetStyleInfo(const Value: TStyleInfo);
    function GetSource: TObject;
  public
{$REGION 'Documentation'}
    /// <summary>Create a  TCustomStyleExt using a vcl style stored in a file
    /// </summary>
{$ENDREGION}
    constructor Create(const FileName: string); reintroduce; overload;
{$REGION 'Documentation'}
    /// <summary>Create a  TCustomStyleExt using a vcl style stored in a stream
    /// </summary>
{$ENDREGION}
    constructor Create(const Stream: TStream); reintroduce; overload;
    constructor Create(const Style: TCustomStyle); reintroduce; overload;
    destructor Destroy; override;
{$REGION 'Documentation'}
    /// <summary>Replace a internal bitmap of the Style
    /// </summary>
{$ENDREGION}
    procedure ReplaceBitmap(DestIndex: Integer; Src: TBitmap);
{$REGION 'Documentation'}
    /// <summary>Set a returns the TStyleInfo fo the current style
    /// </summary>
{$ENDREGION}
    property StyleInfo: TStyleInfo read GetStyleInfo write SetStyleInfo;
{$REGION 'Documentation'}
    /// <summary>Return the list of the bitmaps of the style
    /// </summary>
{$ENDREGION}
    property BitmapList: TObjectList<TBitmap> read GetBitmapList;
    property LocalStream: TStream read FStream;
{$REGION 'Documentation'}
    /// <summary>Copy the modified style to an Stream
    /// </summary>
{$ENDREGION}
    procedure CopyToStream(Stream: TStream);

    property Source: TObject read GetSource;
    procedure SetStyleColor(Color: TStyleColor; NewColor: TColor);
    procedure SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
    procedure SetSystemColor(Color: TColor; NewColor: TColor);
    procedure SetStyleFont(Font: TStyleFont; NewFont: TFont);
  end;

  {
    TCustomStyleHelper = Class Helper for TCustomStyle
    private
    function GetSource: TObject;
    public
    property Source: TObject read GetSource;
    procedure SetStyleColor(Color: TStyleColor; NewColor: TColor);
    procedure SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
    procedure SetSystemColor(Color: TColor; NewColor: TColor);
    End;
  }
  // function DoHasElementFixedPosition(Details: TThemedElementDetails): Boolean;

{$ENDIF}

implementation

uses
  System.Rtti,
  System.Types,
  System.Sysutils,
{$IFDEF USE_VCL_STYLESAPI}
  System.ZLib,
  System.UITypes,
  Vcl.StdCtrls,
  Vcl.ImgList,
  Vcl.Consts,
  Vcl.GraphUtil,
  Vcl.Imaging.pngimage,
{$IF CompilerVersion >= 34}
  Vcl.Direct2D,
  System.StrUtils,
  Winapi.D2D1,
{$IFEND}  
  Winapi.Messages,
{$ENDIF}
  Vcl.Dialogs, Vcl.Styles.Utils.Misc, Vcl.Styles.Utils.Graphics;

{$IF (DEFINED (USE_VCL_STYLESAPI) AND (CompilerVersion >= 23))}
{$I '..\source\vcl\StyleUtils.inc'}
{$I '..\source\vcl\StyleAPI.inc'}
{$IFEND}

type
  TCustomControlClass = class(TCustomControl);

  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;

  TCustomStyleEngineHelper = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks: TStyleHookDictionary;
  End;
  {
    const
    THEME_WP_CAPTION = 77;
    THEME_WP_SMALLCAPTION = 78;
    THEME_WP_MINCAPTION = 79;
    THEME_WP_SMALLMINCAPTION = 80;
    THEME_WP_MAXCAPTION = 81;
    THEME_WP_SMALLMAXCAPTION = 82;
    THEME_WP_FRAMELEFT = 83;
    THEME_WP_FRAMERIGHT = 84;
    THEME_WP_FRAMEBOTTOM = 85;
    THEME_WP_SMALLFRAMELEFT = 86;
    THEME_WP_SMALLFRAMERIGHT = 87;
    THEME_WP_SMALLFRAMEBOTTOM = 88;

    THEME_WP_SYSBUTTON = 89;
    THEME_WP_MDISYSBUTTON = 90;
    THEME_WP_MINBUTTON = 91;
    THEME_WP_MDIMINBUTTON = 92;
    THEME_WP_MAXBUTTON = 93;
    THEME_WP_CLOSEBUTTON = 94;
    THEME_WP_SMALLCLOSEBUTTON = 95;
    THEME_WP_MDICLOSEBUTTON = 96;
    THEME_WP_RESTOREBUTTON = 97;
    THEME_WP_MDIRESTOREBUTTON = 98;
    THEME_WP_HELPBUTTON = 99;
    THEME_WP_MDIHELPBUTTON = 100;
    THEME_WP_HORZSCROLL = 101;
    THEME_WP_HORZTHUMB = 102;
    THEME_WP_VERTSCROLL = 103;
    THEME_WP_VERTTHUMB = 104;
    THEME_WP_DIALOG = 105;
    THEME_WP_CAPTIONSIZINGTEMPLATE = 106;
    THEME_WP_SMALLCAPTIONSIZINGTEMPLATE = 107;
    THEME_WP_FRAMELEFTSIZINGTEMPLATE = 108;
    THEME_WP_SMALLFRAMELEFTSIZINGTEMPLATE = 109;
    THEME_WP_FRAMERIGHTSIZINGTEMPLATE = 110;
    THEME_WP_SMALLFRAMERIGHTSIZINGTEMPLATE = 111;
    THEME_WP_FRAMEBOTTOMSIZINGTEMPLATE = 112;
    THEME_WP_SMALLFRAMEBOTTOMSIZINGTEMPLATE = 113;
    THEME_WP_FRAME = 114;

    function DoHasElementFixedPosition(Details: TThemedElementDetails): Boolean;
    begin
    Result := False;
    if Details.Element <> teWindow then Exit;
    case Details.Part of
    THEME_WP_SMALLCLOSEBUTTON, THEME_WP_SMALLCAPTION:
    Result := TseStyle(FSource).WindowGetFixPosition(kwscToolWindow, kwbClose);
    THEME_WP_CLOSEBUTTON:
    Result := TseStyle(FSource).WindowGetFixPosition(kwscStandard, kwbClose);
    THEME_WP_HELPBUTTON:
    Result := TseStyle(FSource).WindowGetFixPosition(kwscStandard, kwbHelp);
    THEME_WP_MAXBUTTON, THEME_WP_RESTOREBUTTON:
    Result := TseStyle(FSource).WindowGetFixPosition(kwscStandard, kwbMax);
    THEME_WP_MINBUTTON:
    Result := TseStyle(FSource).WindowGetFixPosition(kwscStandard, kwbMin);
    THEME_WP_SYSBUTTON, THEME_WP_CAPTION:
    Result := TseStyle(FSource).WindowGetFixPosition(kwscStandard, kwbSysMenu);
    end;
    end;
  }

class function TCustomStyleEngineHelper.GetRegisteredStyleHooks: TStyleHookDictionary;
{$IF (CompilerVersion >= 31)}
const
  Offset = SizeOf(Pointer) * 3;
var
  p: Pointer;
{$IFEND}
begin
{$IF (CompilerVersion <31)}
  Result := Self.FRegisteredStyleHooks;
{$ELSE}
  {
    TCustomStyleEngine.FRegisteredStyleHooks:
    00651030 3052AA           xor [edx-$56],dl
    00651033 02F7             add dh,bh
    00651035 097623           or [esi+$23],esi
    TCustomStyleEngine.$ClassInitFlag:
    00651038 FFFF             db $ff $ff
    0065103A FFFF             db $ff $ff
    TCustomStyleEngine.FRegSysStylesList:
    0065103C D037             shl [edi],1
  }
  // Use the address of the Self.FRegSysStylesList property to calculate the offset of the FRegisteredStyleHooks
  p := Pointer(PByte(@Self.FRegSysStylesList) - Offset);
  Result := TStyleHookDictionary(p^);
{$IFEND}
end;

{ TStyleManagerHelper }
class function TStyleManagerHelper.RegisteredStyles: TDictionary<string, TSourceInfo>;
{$IF (CompilerVersion >= 31)}
const
  Offset = SizeOf(Pointer) * 3;
{$IFEND}
var
  t: TPair<string, TSourceInfo>;
  SourceInfo: TSourceInfo;
  LRegisteredStyles: TDictionary<string, TSourceInfo>;
{$IF (CompilerVersion >= 31)}
  p: Pointer;
{$IFEND}
begin
  Result := TDictionary<string, TSourceInfo>.Create;
{$IF (CompilerVersion < 31)}
  LRegisteredStyles := TDictionary<string, TSourceInfo>(Self.FRegisteredStyles);
{$ELSE}
  {
    TStyleManager.FFlags:
    006CD058 0100             add [eax],eax
    006CD05A 0000             add [eax],al
    TStyleManager.FRegisteredStyles:
    006CD05C 7050             jo $006cd0ae
    006CD05E B702             mov bh,$02
    TStyleManager.FStyleClassDescriptors:
    006CD060 A850             test al,$50
    006CD062 B702             mov bh,$02
    TStyleManager.FStyleEngines:
    006CD064 1851B7           sbb [ecx-$49],dl
    006CD067 02E0             add ah,al
    006CD069 50               push eax
    006CD06A B702             mov bh,$02
    TStyleManager.FSystemStyle:
    006CD06C 2077B0           and [edi-$50],dh
    006CD06F 0200             add al,[eax]
    TStyleManager.FSystemHooks:
    006CD071 07               pop es  006CD076 FFFF             db $ff $ff
  }
  // Use the address of the Self.Flags property to calculate the offset of the FRegisteredStyles
{$IFDEF CPUX64}
  p := Pointer(PByte(@Self.Flags) + 8);
{$ELSE}
  p := Pointer(PByte(@Self.Flags) + 4);
{$ENDIF CPUX64}
  LRegisteredStyles := TDictionary<string, TSourceInfo>(p^);
{$IFEND}
  for t in LRegisteredStyles do
  begin
    SourceInfo.Data := t.Value.Data;
    SourceInfo.StyleClass := t.Value.StyleClass;
    Result.Add(t.Key, SourceInfo);
  end;
end;

class function TStyleManagerHelper.GetStyles: TList<TCustomStyleServices>;
{$IF (CompilerVersion >= 31)}
var
  p: Pointer;
{$IFEND}
begin
{$IF (CompilerVersion <31)}
  Result := Self.FStyles;
{$ELSE}
  {
    TStyleManager.FStyles:
    0067E06C E050             loopne $0067e0be
    0067E06E AD               lodsd
    0067E06F 0220             add ah,[eax]
    0067E071 77A6             jnbe $0067e019
    0067E073 0200             add al,[eax]
    ....
    ....
    TStyleManager.FFlags:
    0067E05C 0001             add [ecx],al
    0067E05E 0000             add [eax],al
    TStyleManager.FRegisteredStyles:
    0067E060 7050             jo $0067e0b2
    0067E062 AD               lodsd
    0067E063 02A850AD0218     add ch,[eax+$1802ad50]
  }
{$IFDEF CPUX64}
  p := Pointer(PByte(@Self.Flags) + 32);
{$ELSE}
  p := Pointer(PByte(@Self.Flags) + 16);
{$ENDIF CPUX64}
  Result := TList<TCustomStyleServices>(p^);
{$IFEND}
end;

class function TStyleManagerHelper.GetStyleSourceInfo(const StyleName: string): TSourceInfo;
Var
  LRegisteredStyles: TDictionary<string, TSourceInfo>;
begin
  Result.Data := nil;
  Result.StyleClass := nil;

  LRegisteredStyles := TStyleManager.RegisteredStyles;
  try
    if LRegisteredStyles.ContainsKey(StyleName) then
      Result := LRegisteredStyles[StyleName];
  finally
    LRegisteredStyles.Free;
  end;
end;

class procedure TStyleManagerHelper.RefreshCurrentTheme;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].HandleAllocated then
      if IsWindowVisible(Screen.Forms[I].Handle) then
        PostMessage(Screen.Forms[I].Handle, CM_CUSTOMSTYLECHANGED, 0, 0)
      else
        SendMessage(Screen.Forms[I].Handle, CM_CUSTOMSTYLECHANGED, 0, 0);
end;

class procedure TStyleManagerHelper.ReloadStyle(const StyleName: string);
var
  LStyle: TCustomStyleServices;
  LPair: TPair<string, TSourceInfo>;
  LRegisteredStyles: TDictionary<string, TSourceInfo>;
begin

  if SameText(StyleName, ActiveStyle.Name, loUserLocale) then
    SetStyle(SystemStyle);

  for LStyle in Styles do
    if SameText(StyleName, LStyle.Name, loUserLocale) then
    begin
      LStyle.Free;
      Styles.Remove(LStyle);
    end;

  LRegisteredStyles := Self.RegisteredStyles;
  try
    for LPair in LRegisteredStyles do
      if SameText(StyleName, LPair.Key, loUserLocale) then
        if (LPair.Value.Data <> nil) then
        begin
          TStream(LPair.Value.Data).Position := 0;
          break;
        end;
  finally
    LRegisteredStyles.Free;
  end;

  SetStyle(StyleName);
end;

class procedure TStyleManagerHelper.RemoveStyle(const StyleName: string);
var
  LStyle: TCustomStyleServices;
  LPair: TPair<string, TSourceInfo>;
begin
  if SameText(StyleName, ActiveStyle.Name, loUserLocale) then
    SetStyle(SystemStyle);

  for LStyle in Styles do
    if SameText(StyleName, LStyle.Name, loUserLocale) then
    begin
      LStyle.Free;
      Styles.Remove(LStyle);
    end;

  for LPair in Self.RegisteredStyles do
    if SameText(StyleName, LPair.Key, loUserLocale) then
    begin
      TMemoryStream(LPair.Value.Data).Free;
      Self.RegisteredStyles.Remove(LPair.Key);
    end;
end;

class function TStyleManagerHelper._GetStyles: TList<TCustomStyleServices>;
begin
  Result := TStyleManager.GetStyles;
end;

class function TStyleManagerHelper.StyleLoaded(const StyleName: string): Boolean;
begin
  Result := TStyleManager.Style[StyleName] <> nil;
end;

function GetRegisteredStylesHooks(ControlClass: TClass): TStyleHookList;
begin
  Result := nil;
  if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
    Result := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
end;

function IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass): Boolean;
var
  List: TStyleHookList;
begin
  Result := False;
  if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
  begin
    List := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
    Result := List.IndexOf(StyleHookClass) <> -1;
  end;
end;

procedure ApplyEmptyVCLStyleHook(ControlClass: TClass);
begin
  if not IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(ControlClass, TStyleHook);
end;

procedure RemoveEmptyVCLStyleHook(ControlClass: TClass);
begin
  if IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.UnRegisterStyleHook(ControlClass, TStyleHook);
end;

{$IFDEF USE_VCL_STYLESAPI}
{ TVCLStyleExt }

constructor TCustomStyleExt.Create(const FileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Create(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TCustomStyleExt.CopyToStream(Stream: TStream);
var
  I: Integer;
begin
  Stream.Size := 0;
  Stream.Position := 0;

  TseStyle(Source).FCleanCopy.Name := TseStyle(Source).StyleSource.Name;
  TseStyle(Source).FCleanCopy.Author := TseStyle(Source).StyleSource.Author;
  TseStyle(Source).FCleanCopy.AuthorEMail := TseStyle(Source).StyleSource.AuthorEMail;
  TseStyle(Source).FCleanCopy.AuthorURL := TseStyle(Source).StyleSource.AuthorURL;
  TseStyle(Source).FCleanCopy.Version := TseStyle(Source).StyleSource.Version;

  // Replace the modified bitmaps
  for I := 0 to TseStyle(Source).FCleanCopy.Bitmaps.Count - 1 do
    TseStyle(Source).FCleanCopy.Bitmaps[I].Assign(TseStyle(Source).StyleSource.Bitmaps[I]);

  // TseStyle(Source).StyleSource.SysColors.Assign(TseStyle(Source).SysColors);

  // Replace the modified colors
  TseStyle(Source).FCleanCopy.SysColors.Assign(TseStyle(Source).SysColors);
  TseStyle(Source).FCleanCopy.Colors.Assign(TseStyle(Source).Colors);
  TseStyle(Source).FCleanCopy.Fonts.Assign(TseStyle(Source).Fonts);

  // ShowMessage(ColorToString(TseStyle(Source).SysColors[clWindow]));
  TseStyle(Source).SaveToStream(Stream);
  {
    TseStyle(Source).StyleSource.Fonts.Assign(TseStyle(Source).Fonts);
    TseStyle(Source).StyleSource.Colors.Assign(TseStyle(Source).Colors);
    TseStyle(Source).StyleSource.SysColors.Assign(TseStyle(Source).SysColors);
    TseStyle(Source).StyleSource.SaveToStream(Stream);
  }
end;

constructor TCustomStyleExt.Create(const Style: TCustomStyle);
begin
  // Style.Source
  // inherited Create(TStream(Style.));
end;

constructor TCustomStyleExt.Create(const Stream: TStream);
var
  LSource: TObject;
begin
  inherited Create;
  FStream := TMemoryStream.Create;

  Stream.Seek(0, soBeginning); // index 0 to load
  FStream.CopyFrom(Stream, Stream.Size);
  Stream.Seek(0, soBeginning); // restore index 0 after
  LSource := Source;
  FStream.Seek(0, soBeginning); // index 0 to load
  TseStyle(LSource).LoadFromStream(FStream);
end;

destructor TCustomStyleExt.Destroy;
begin
  if Assigned(FStream) then
    FStream.Free;
  inherited Destroy;
end;

function TCustomStyleExt.GetBitmapList: TObjectList<TBitmap>;
var
  LSource: TObject;
  I: Integer;
  LseBitmap: TseBitmap;
begin
  LSource := Source;
  Result := TObjectList<TBitmap>.Create;
  for I := 0 to TseStyle(LSource).StyleSource.Bitmaps.Count - 1 do
  begin
    Result.Add(TBitmap.Create);
    Result[I].PixelFormat := pf32bit;
    LseBitmap := TseStyle(LSource).StyleSource.Bitmaps[I];
    Result[I].Width := LseBitmap.Width;
    Result[I].Height := LseBitmap.Height;
    LseBitmap.Draw(Result[I].Canvas, 0, 0);
  end;
end;

procedure TCustomStyleExt.ReplaceBitmap(DestIndex: Integer; Src: TBitmap);
var
  BF: TBlendFunction;
  Canvas: TCanvas;
  LBitMap: TseBitmap;
  DstRect, SrcRect: TRect;
begin
  LBitMap := TseStyle(Source).StyleSource.Bitmaps[DestIndex];
  SrcRect := Rect(0, 0, Src.Width, Src.Height);
  DstRect := Rect(0, 0, Src.Width, Src.Height);
  Canvas := LBitMap.Canvas;
  SetStretchBltMode(Canvas.Handle, COLORONCOLOR);
  if LBitMap.AlphaBlend then
  begin
    BF.BlendOp := AC_SRC_OVER;
    BF.BlendFlags := 0;
    BF.SourceConstantAlpha := 255;
    BF.AlphaFormat := AC_SRC_ALPHA;
    Winapi.Windows.AlphaBlend(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
      SrcRect.Bottom - SrcRect.Top, BF);
  end
  else if LBitMap.Transparent then
  begin
    Winapi.Windows.TransparentBlt(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
      SrcRect.Bottom - SrcRect.Top, seTransparent);
  end
  else
  begin
    Winapi.Windows.StretchBlt(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left,
      DstRect.Bottom - DstRect.Top, Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
      SrcRect.Bottom - SrcRect.Top, SRCCOPY);
  end;
end;

procedure TCustomStyleExt.SetStyleColor(Color: TStyleColor; NewColor: TColor);
begin
  case Color of
    scBorder:
      if TseStyle(Source).Colors[ktcBorder] <> NewColor then
        TseStyle(Source).Colors[ktcBorder] := NewColor;
    scButtonDisabled:
      if TseStyle(Source).Colors[ktcButtonDisabled] <> NewColor then
        TseStyle(Source).Colors[ktcButtonDisabled] := NewColor;
    scButtonFocused:
      if TseStyle(Source).Colors[ktcButtonFocused] <> NewColor then
        TseStyle(Source).Colors[ktcButtonFocused] := NewColor;
    scButtonHot:
      if TseStyle(Source).Colors[ktcButtonHot] <> NewColor then
        TseStyle(Source).Colors[ktcButtonHot] := NewColor;
    scButtonNormal:
      if TseStyle(Source).Colors[ktcButton] <> NewColor then
        TseStyle(Source).Colors[ktcButton] := NewColor;
    scButtonPressed:
      if TseStyle(Source).Colors[ktcButtonPressed] <> NewColor then
        TseStyle(Source).Colors[ktcButtonPressed] := NewColor;
    scCategoryButtons:
      if TseStyle(Source).Colors[ktcCategoryButtons] <> NewColor then
        TseStyle(Source).Colors[ktcCategoryButtons] := NewColor;
    scCategoryButtonsGradientBase:
      if TseStyle(Source).Colors[ktcCategoryButtonsGradientBase] <> NewColor then
        TseStyle(Source).Colors[ktcCategoryButtonsGradientBase] := NewColor;
    scCategoryButtonsGradientEnd:
      if TseStyle(Source).Colors[ktcCategoryButtonsGradientEnd] <> NewColor then
        TseStyle(Source).Colors[ktcCategoryButtonsGradientEnd] := NewColor;
    scCategoryPanelGroup:
      if TseStyle(Source).Colors[ktcCategoryPanelGroup] <> NewColor then
        TseStyle(Source).Colors[ktcCategoryPanelGroup] := NewColor;
    scComboBox:
      if TseStyle(Source).Colors[ktcComboBox] <> NewColor then
        TseStyle(Source).Colors[ktcComboBox] := NewColor;
    scComboBoxDisabled:
      if TseStyle(Source).Colors[ktcComboBoxDisabled] <> NewColor then
        TseStyle(Source).Colors[ktcComboBoxDisabled] := NewColor;
    scEdit:
      if TseStyle(Source).Colors[ktcEdit] <> NewColor then
        TseStyle(Source).Colors[ktcEdit] := NewColor;
    scEditDisabled:
      if TseStyle(Source).Colors[ktcEditDisabled] <> NewColor then
        TseStyle(Source).Colors[ktcEditDisabled] := NewColor;
    scGrid:
      if TseStyle(Source).Colors[ktcGrid] <> NewColor then
        TseStyle(Source).Colors[ktcGrid] := NewColor;
    scGenericBackground:
      if TseStyle(Source).Colors[ktcGenericBackground] <> NewColor then
        TseStyle(Source).Colors[ktcGenericBackground] := NewColor;
    scGenericGradientEnd:
      if TseStyle(Source).Colors[ktcGenericGradientEnd] <> NewColor then
        TseStyle(Source).Colors[ktcGenericGradientEnd] := NewColor;
    scGenericGradientBase:
      if TseStyle(Source).Colors[ktcGenericGradientBase] <> NewColor then
        TseStyle(Source).Colors[ktcGenericGradientBase] := NewColor;
    scHintGradientBase:
      if TseStyle(Source).Colors[ktcHintGradientBase] <> NewColor then
        TseStyle(Source).Colors[ktcHintGradientBase] := NewColor;
    scHintGradientEnd:
      if TseStyle(Source).Colors[ktcHintGradientEnd] <> NewColor then
        TseStyle(Source).Colors[ktcHintGradientEnd] := NewColor;
    scListBox:
      if TseStyle(Source).Colors[ktcListBox] <> NewColor then
        TseStyle(Source).Colors[ktcListBox] := NewColor;
    scListBoxDisabled:
      if TseStyle(Source).Colors[ktcListBoxDisabled] <> NewColor then
        TseStyle(Source).Colors[ktcListBoxDisabled] := NewColor;
    scListView:
      if TseStyle(Source).Colors[ktcListView] <> NewColor then
        TseStyle(Source).Colors[ktcListView] := NewColor;
    scPanel:
      if TseStyle(Source).Colors[ktcPanel] <> NewColor then
        TseStyle(Source).Colors[ktcPanel] := NewColor;
    scPanelDisabled:
      if TseStyle(Source).Colors[ktcPanelDisabled] <> NewColor then
        TseStyle(Source).Colors[ktcPanelDisabled] := NewColor;
    scSplitter:
      if TseStyle(Source).Colors[ktcSplitter] <> NewColor then
        TseStyle(Source).Colors[ktcSplitter] := NewColor;
    scToolBarGradientBase:
      if TseStyle(Source).Colors[ktcToolBarGradientBase] <> NewColor then
        TseStyle(Source).Colors[ktcToolBarGradientBase] := NewColor;
    scToolBarGradientEnd:
      if TseStyle(Source).Colors[ktcToolBarGradientEnd] <> NewColor then
        TseStyle(Source).Colors[ktcToolBarGradientEnd] := NewColor;
    scTreeView:
      if TseStyle(Source).Colors[ktcTreeView] <> NewColor then
        TseStyle(Source).Colors[ktcTreeView] := NewColor;
    scWindow:
      if TseStyle(Source).Colors[ktcWindow] <> NewColor then
        TseStyle(Source).Colors[ktcWindow] := NewColor;
  end;
end;

procedure TCustomStyleExt.SetStyleFont(Font: TStyleFont; NewFont: TFont);
begin
  case Font of
    sfButtonTextDisabled:
      if TseStyle(Source).Fonts[ktfButtonTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfButtonTextDisabled] := NewFont;
    sfButtonTextFocused:
      if TseStyle(Source).Fonts[ktfButtonTextFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfButtonTextFocused] := NewFont;
    sfButtonTextHot:
      if TseStyle(Source).Fonts[ktfButtonTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfButtonTextHot] := NewFont;
    sfButtonTextNormal:
      if TseStyle(Source).Fonts[ktfButtonTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfButtonTextNormal] := NewFont;
    sfButtonTextPressed:
      if TseStyle(Source).Fonts[ktfButtonTextPressed] <> NewFont then
        TseStyle(Source).Fonts[ktfButtonTextPressed] := NewFont;
    sfCaptionTextInactive:
      if TseStyle(Source).Fonts[ktfCaptionTextInactive] <> NewFont then
        TseStyle(Source).Fonts[ktfCaptionTextInactive] := NewFont;
    sfCaptionTextNormal:
      if TseStyle(Source).Fonts[ktfCaptionTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfCaptionTextNormal] := NewFont;
    sfCategoryPanelGroupHeaderHot:
      if TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot] <> NewFont then
        TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot] := NewFont;
    sfCategoryPanelGroupHeaderNormal:
      if TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal] := NewFont;
    sfCatgeoryButtonsCategoryNormal:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal] := NewFont;
    sfCatgeoryButtonsCategorySelected:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected] <> NewFont then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected] := NewFont;
    sfCatgeoryButtonsHot:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsHot] <> NewFont then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsHot] := NewFont;
    sfCatgeoryButtonsNormal:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsNormal] := NewFont;
    sfCatgeoryButtonsSelected:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsSelected] := NewFont;
    sfCheckBoxTextDisabled:
      if TseStyle(Source).Fonts[ktfCheckBoxTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfCheckBoxTextDisabled] := NewFont;
    sfCheckBoxTextFocused:
      if TseStyle(Source).Fonts[ktfCheckBoxTextFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfCheckBoxTextFocused] := NewFont;
    sfCheckBoxTextHot:
      if TseStyle(Source).Fonts[ktfCheckBoxTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfCheckBoxTextHot] := NewFont;
    sfCheckBoxTextNormal:
      if TseStyle(Source).Fonts[ktfCheckBoxTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfCheckBoxTextNormal] := NewFont;
    sfCheckBoxTextPressed:
      if TseStyle(Source).Fonts[ktfCheckBoxTextPressed] <> NewFont then
        TseStyle(Source).Fonts[ktfCheckBoxTextPressed] := NewFont;
    sfComboBoxItemDisabled:
      if TseStyle(Source).Fonts[ktfComboBoxItemDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfComboBoxItemDisabled] := NewFont;
    sfComboBoxItemFocused:
      if TseStyle(Source).Fonts[ktfComboBoxItemFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfComboBoxItemFocused] := NewFont;
    sfComboBoxItemHot:
      if TseStyle(Source).Fonts[ktfComboBoxItemHot] <> NewFont then
        TseStyle(Source).Fonts[ktfComboBoxItemHot] := NewFont;
    sfComboBoxItemNormal:
      if TseStyle(Source).Fonts[ktfComboBoxItemNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfComboBoxItemNormal] := NewFont;
    sfComboBoxItemSelected:
      if TseStyle(Source).Fonts[ktfComboBoxItemSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfComboBoxItemSelected] := NewFont;
    sfEditBoxTextDisabled:
      if TseStyle(Source).Fonts[ktfEditBoxTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfEditBoxTextDisabled] := NewFont;
    sfEditBoxTextFocused:
      if TseStyle(Source).Fonts[ktfEditBoxTextFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfEditBoxTextFocused] := NewFont;
    sfEditBoxTextHot:
      if TseStyle(Source).Fonts[ktfEditBoxTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfEditBoxTextHot] := NewFont;
    sfEditBoxTextNormal:
      if TseStyle(Source).Fonts[ktfEditBoxTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfEditBoxTextNormal] := NewFont;
    sfEditBoxTextSelected:
      if TseStyle(Source).Fonts[ktfEditBoxTextSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfEditBoxTextSelected] := NewFont;
    sfGridItemFixedHot:
      if TseStyle(Source).Fonts[ktfGridItemFixedHot] <> NewFont then
        TseStyle(Source).Fonts[ktfGridItemFixedHot] := NewFont;
    sfGridItemFixedNormal:
      if TseStyle(Source).Fonts[ktfGridItemFixedNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfGridItemFixedNormal] := NewFont;
    sfGridItemFixedPressed:
      if TseStyle(Source).Fonts[ktfGridItemFixedPressed] <> NewFont then
        TseStyle(Source).Fonts[ktfGridItemFixedPressed] := NewFont;
    sfGridItemNormal:
      if TseStyle(Source).Fonts[ktfGridItemNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfGridItemNormal] := NewFont;
    sfGridItemSelected:
      if TseStyle(Source).Fonts[ktfGridItemSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfGridItemSelected] := NewFont;
    sfGroupBoxTextDisabled:
      if TseStyle(Source).Fonts[ktfGroupBoxTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfGroupBoxTextDisabled] := NewFont;
    sfGroupBoxTextNormal:
      if TseStyle(Source).Fonts[ktfGroupBoxTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfGroupBoxTextNormal] := NewFont;
    sfHeaderSectionTextDisabled:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfHeaderSectionTextDisabled] := NewFont;
    sfHeaderSectionTextHot:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfHeaderSectionTextHot] := NewFont;
    sfHeaderSectionTextNormal:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfHeaderSectionTextNormal] := NewFont;
    sfHeaderSectionTextPressed:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextPressed] <> NewFont then
        TseStyle(Source).Fonts[ktfHeaderSectionTextPressed] := NewFont;
    sfListItemTextDisabled:
      if TseStyle(Source).Fonts[ktfListItemTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfListItemTextDisabled] := NewFont;
    sfListItemTextFocused:
      if TseStyle(Source).Fonts[ktfListItemTextFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfListItemTextFocused] := NewFont;
    sfListItemTextHot:
      if TseStyle(Source).Fonts[ktfListItemTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfListItemTextHot] := NewFont;
    sfListItemTextNormal:
      if TseStyle(Source).Fonts[ktfListItemTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfListItemTextNormal] := NewFont;
    sfListItemTextSelected:
      if TseStyle(Source).Fonts[ktfListItemTextSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfListItemTextSelected] := NewFont;
    sfMenuItemTextDisabled:
      if TseStyle(Source).Fonts[ktfMenuItemTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfMenuItemTextDisabled] := NewFont;
    sfMenuItemTextHot:
      if TseStyle(Source).Fonts[ktfMenuItemTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfMenuItemTextHot] := NewFont;
    sfMenuItemTextNormal:
      if TseStyle(Source).Fonts[ktfMenuItemTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfMenuItemTextNormal] := NewFont;
    sfMenuItemTextSelected:
      if TseStyle(Source).Fonts[ktfMenuItemTextSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfMenuItemTextSelected] := NewFont;
    sfPanelTextDisabled:
      if TseStyle(Source).Fonts[ktfPanelTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfPanelTextDisabled] := NewFont;
    sfPanelTextNormal:
      if TseStyle(Source).Fonts[ktfPanelTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfPanelTextNormal] := NewFont;
    sfPopupMenuItemTextDisabled:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextDisabled] := NewFont;
    sfPopupMenuItemTextHot:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextHot] := NewFont;
    sfPopupMenuItemTextNormal:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextNormal] := NewFont;
    sfPopupMenuItemTextSelected:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextSelected] := NewFont;
    sfRadioButtonTextDisabled:
      if TseStyle(Source).Fonts[ktfRadioButtonTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfRadioButtonTextDisabled] := NewFont;
    sfRadioButtonTextFocused:
      if TseStyle(Source).Fonts[ktfRadioButtonTextFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfRadioButtonTextFocused] := NewFont;
    sfRadioButtonTextHot:
      if TseStyle(Source).Fonts[ktfRadioButtonTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfRadioButtonTextHot] := NewFont;
    sfRadioButtonTextNormal:
      if TseStyle(Source).Fonts[ktfRadioButtonTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfRadioButtonTextNormal] := NewFont;
    sfRadioButtonTextPressed:
      if TseStyle(Source).Fonts[ktfRadioButtonTextPressed] <> NewFont then
        TseStyle(Source).Fonts[ktfRadioButtonTextPressed] := NewFont;
    sfSmCaptionTextInactive:
      if TseStyle(Source).Fonts[ktfSmCaptionTextInactive] <> NewFont then
        TseStyle(Source).Fonts[ktfSmCaptionTextInactive] := NewFont;
    sfSmCaptionTextNormal:
      if TseStyle(Source).Fonts[ktfSmCaptionTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfSmCaptionTextNormal] := NewFont;
    sfStatusPanelTextDisabled:
      if TseStyle(Source).Fonts[ktfStatusPanelTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfStatusPanelTextDisabled] := NewFont;
    sfStatusPanelTextNormal:
      if TseStyle(Source).Fonts[ktfStatusPanelTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfStatusPanelTextNormal] := NewFont;
    sfTabTextActiveDisabled:
      if TseStyle(Source).Fonts[ktfTabTextActiveDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfTabTextActiveDisabled] := NewFont;
    sfTabTextActiveHot:
      if TseStyle(Source).Fonts[ktfTabTextActiveHot] <> NewFont then
        TseStyle(Source).Fonts[ktfTabTextActiveHot] := NewFont;
    sfTabTextActiveNormal:
      if TseStyle(Source).Fonts[ktfTabTextActiveNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfTabTextActiveNormal] := NewFont;
    sfTabTextInactiveDisabled:
      if TseStyle(Source).Fonts[ktfTabTextInactiveDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfTabTextInactiveDisabled] := NewFont;
    sfTabTextInactiveHot:
      if TseStyle(Source).Fonts[ktfTabTextInactiveHot] <> NewFont then
        TseStyle(Source).Fonts[ktfTabTextInactiveHot] := NewFont;
    sfTabTextInactiveNormal:
      if TseStyle(Source).Fonts[ktfTabTextInactiveNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfTabTextInactiveNormal] := NewFont;
    sfTextLabelDisabled:
      if TseStyle(Source).Fonts[ktfStaticTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfStaticTextDisabled] := NewFont;
    sfTextLabelFocused:
      if TseStyle(Source).Fonts[ktfStaticTextFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfStaticTextFocused] := NewFont;
    sfTextLabelHot:
      if TseStyle(Source).Fonts[ktfStaticTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfStaticTextHot] := NewFont;
    sfTextLabelNormal:
      if TseStyle(Source).Fonts[ktfStaticTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfStaticTextNormal] := NewFont;
    sfToolItemTextDisabled:
      if TseStyle(Source).Fonts[ktfToolItemTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfToolItemTextDisabled] := NewFont;
    sfToolItemTextHot:
      if TseStyle(Source).Fonts[ktfToolItemTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfToolItemTextHot] := NewFont;
    sfToolItemTextNormal:
      if TseStyle(Source).Fonts[ktfToolItemTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfToolItemTextNormal] := NewFont;
    sfToolItemTextSelected:
      if TseStyle(Source).Fonts[ktfToolItemTextSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfToolItemTextSelected] := NewFont;
    sfTreeItemTextDisabled:
      if TseStyle(Source).Fonts[ktfTreeItemTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfTreeItemTextDisabled] := NewFont;
    sfTreeItemTextFocused:
      if TseStyle(Source).Fonts[ktfTreeItemTextFocused] <> NewFont then
        TseStyle(Source).Fonts[ktfTreeItemTextFocused] := NewFont;
    sfTreeItemTextHot:
      if TseStyle(Source).Fonts[ktfTreeItemTextHot] <> NewFont then
        TseStyle(Source).Fonts[ktfTreeItemTextHot] := NewFont;
    sfTreeItemTextNormal:
      if TseStyle(Source).Fonts[ktfTreeItemTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfTreeItemTextNormal] := NewFont;
    sfTreeItemTextSelected:
      if TseStyle(Source).Fonts[ktfTreeItemTextSelected] <> NewFont then
        TseStyle(Source).Fonts[ktfTreeItemTextSelected] := NewFont;
    sfWindowTextDisabled:
      if TseStyle(Source).Fonts[ktfWindowTextDisabled] <> NewFont then
        TseStyle(Source).Fonts[ktfWindowTextDisabled] := NewFont;
    sfWindowTextNormal:
      if TseStyle(Source).Fonts[ktfWindowTextNormal] <> NewFont then
        TseStyle(Source).Fonts[ktfWindowTextNormal] := NewFont;
  end;
end;

procedure TCustomStyleExt.SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
begin
  case Font of
    sfButtonTextDisabled:
      if TseStyle(Source).Fonts[ktfButtonTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfButtonTextDisabled].Color := NewColor;
    sfButtonTextFocused:
      if TseStyle(Source).Fonts[ktfButtonTextFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfButtonTextFocused].Color := NewColor;
    sfButtonTextHot:
      if TseStyle(Source).Fonts[ktfButtonTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfButtonTextHot].Color := NewColor;
    sfButtonTextNormal:
      if TseStyle(Source).Fonts[ktfButtonTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfButtonTextNormal].Color := NewColor;
    sfButtonTextPressed:
      if TseStyle(Source).Fonts[ktfButtonTextPressed].Color <> NewColor then
        TseStyle(Source).Fonts[ktfButtonTextPressed].Color := NewColor;
    sfCaptionTextInactive:
      if TseStyle(Source).Fonts[ktfCaptionTextInactive].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCaptionTextInactive].Color := NewColor;
    sfCaptionTextNormal:
      if TseStyle(Source).Fonts[ktfCaptionTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCaptionTextNormal].Color := NewColor;
    sfCategoryPanelGroupHeaderHot:
      if TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot].Color := NewColor;
    sfCategoryPanelGroupHeaderNormal:
      if TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal].Color := NewColor;
    sfCatgeoryButtonsCategoryNormal:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal].Color := NewColor;
    sfCatgeoryButtonsCategorySelected:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected].Color := NewColor;
    sfCatgeoryButtonsHot:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsHot].Color := NewColor;
    sfCatgeoryButtonsNormal:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsNormal].Color := NewColor;
    sfCatgeoryButtonsSelected:
      if TseStyle(Source).Fonts[ktfCatgeoryButtonsSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCatgeoryButtonsSelected].Color := NewColor;
    sfCheckBoxTextDisabled:
      if TseStyle(Source).Fonts[ktfCheckBoxTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCheckBoxTextDisabled].Color := NewColor;
    sfCheckBoxTextFocused:
      if TseStyle(Source).Fonts[ktfCheckBoxTextFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCheckBoxTextFocused].Color := NewColor;
    sfCheckBoxTextHot:
      if TseStyle(Source).Fonts[ktfCheckBoxTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCheckBoxTextHot].Color := NewColor;
    sfCheckBoxTextNormal:
      if TseStyle(Source).Fonts[ktfCheckBoxTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCheckBoxTextNormal].Color := NewColor;
    sfCheckBoxTextPressed:
      if TseStyle(Source).Fonts[ktfCheckBoxTextPressed].Color <> NewColor then
        TseStyle(Source).Fonts[ktfCheckBoxTextPressed].Color := NewColor;
    sfComboBoxItemDisabled:
      if TseStyle(Source).Fonts[ktfComboBoxItemDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfComboBoxItemDisabled].Color := NewColor;
    sfComboBoxItemFocused:
      if TseStyle(Source).Fonts[ktfComboBoxItemFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfComboBoxItemFocused].Color := NewColor;
    sfComboBoxItemHot:
      if TseStyle(Source).Fonts[ktfComboBoxItemHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfComboBoxItemHot].Color := NewColor;
    sfComboBoxItemNormal:
      if TseStyle(Source).Fonts[ktfComboBoxItemNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfComboBoxItemNormal].Color := NewColor;
    sfComboBoxItemSelected:
      if TseStyle(Source).Fonts[ktfComboBoxItemSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfComboBoxItemSelected].Color := NewColor;
    sfEditBoxTextDisabled:
      if TseStyle(Source).Fonts[ktfEditBoxTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfEditBoxTextDisabled].Color := NewColor;
    sfEditBoxTextFocused:
      if TseStyle(Source).Fonts[ktfEditBoxTextFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfEditBoxTextFocused].Color := NewColor;
    sfEditBoxTextHot:
      if TseStyle(Source).Fonts[ktfEditBoxTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfEditBoxTextHot].Color := NewColor;
    sfEditBoxTextNormal:
      if TseStyle(Source).Fonts[ktfEditBoxTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfEditBoxTextNormal].Color := NewColor;
    sfEditBoxTextSelected:
      if TseStyle(Source).Fonts[ktfEditBoxTextSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfEditBoxTextSelected].Color := NewColor;
    sfGridItemFixedHot:
      if TseStyle(Source).Fonts[ktfGridItemFixedHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfGridItemFixedHot].Color := NewColor;
    sfGridItemFixedNormal:
      if TseStyle(Source).Fonts[ktfGridItemFixedNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfGridItemFixedNormal].Color := NewColor;
    sfGridItemFixedPressed:
      if TseStyle(Source).Fonts[ktfGridItemFixedPressed].Color <> NewColor then
        TseStyle(Source).Fonts[ktfGridItemFixedPressed].Color := NewColor;
    sfGridItemNormal:
      if TseStyle(Source).Fonts[ktfGridItemNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfGridItemNormal].Color := NewColor;
    sfGridItemSelected:
      if TseStyle(Source).Fonts[ktfGridItemSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfGridItemSelected].Color := NewColor;
    sfGroupBoxTextDisabled:
      if TseStyle(Source).Fonts[ktfGroupBoxTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfGroupBoxTextDisabled].Color := NewColor;
    sfGroupBoxTextNormal:
      if TseStyle(Source).Fonts[ktfGroupBoxTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfGroupBoxTextNormal].Color := NewColor;
    sfHeaderSectionTextDisabled:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfHeaderSectionTextDisabled].Color := NewColor;
    sfHeaderSectionTextHot:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfHeaderSectionTextHot].Color := NewColor;
    sfHeaderSectionTextNormal:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfHeaderSectionTextNormal].Color := NewColor;
    sfHeaderSectionTextPressed:
      if TseStyle(Source).Fonts[ktfHeaderSectionTextPressed].Color <> NewColor then
        TseStyle(Source).Fonts[ktfHeaderSectionTextPressed].Color := NewColor;
    sfListItemTextDisabled:
      if TseStyle(Source).Fonts[ktfListItemTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfListItemTextDisabled].Color := NewColor;
    sfListItemTextFocused:
      if TseStyle(Source).Fonts[ktfListItemTextFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfListItemTextFocused].Color := NewColor;
    sfListItemTextHot:
      if TseStyle(Source).Fonts[ktfListItemTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfListItemTextHot].Color := NewColor;
    sfListItemTextNormal:
      if TseStyle(Source).Fonts[ktfListItemTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfListItemTextNormal].Color := NewColor;
    sfListItemTextSelected:
      if TseStyle(Source).Fonts[ktfListItemTextSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfListItemTextSelected].Color := NewColor;
    sfMenuItemTextDisabled:
      if TseStyle(Source).Fonts[ktfMenuItemTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfMenuItemTextDisabled].Color := NewColor;
    sfMenuItemTextHot:
      if TseStyle(Source).Fonts[ktfMenuItemTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfMenuItemTextHot].Color := NewColor;
    sfMenuItemTextNormal:
      if TseStyle(Source).Fonts[ktfMenuItemTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfMenuItemTextNormal].Color := NewColor;
    sfMenuItemTextSelected:
      if TseStyle(Source).Fonts[ktfMenuItemTextSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfMenuItemTextSelected].Color := NewColor;
    sfPanelTextDisabled:
      if TseStyle(Source).Fonts[ktfPanelTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfPanelTextDisabled].Color := NewColor;
    sfPanelTextNormal:
      if TseStyle(Source).Fonts[ktfPanelTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfPanelTextNormal].Color := NewColor;
    sfPopupMenuItemTextDisabled:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextDisabled].Color := NewColor;
    sfPopupMenuItemTextHot:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextHot].Color := NewColor;
    sfPopupMenuItemTextNormal:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextNormal].Color := NewColor;
    sfPopupMenuItemTextSelected:
      if TseStyle(Source).Fonts[ktfPopupMenuItemTextSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfPopupMenuItemTextSelected].Color := NewColor;
    sfRadioButtonTextDisabled:
      if TseStyle(Source).Fonts[ktfRadioButtonTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfRadioButtonTextDisabled].Color := NewColor;
    sfRadioButtonTextFocused:
      if TseStyle(Source).Fonts[ktfRadioButtonTextFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfRadioButtonTextFocused].Color := NewColor;
    sfRadioButtonTextHot:
      if TseStyle(Source).Fonts[ktfRadioButtonTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfRadioButtonTextHot].Color := NewColor;
    sfRadioButtonTextNormal:
      if TseStyle(Source).Fonts[ktfRadioButtonTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfRadioButtonTextNormal].Color := NewColor;
    sfRadioButtonTextPressed:
      if TseStyle(Source).Fonts[ktfRadioButtonTextPressed].Color <> NewColor then
        TseStyle(Source).Fonts[ktfRadioButtonTextPressed].Color := NewColor;
    sfSmCaptionTextInactive:
      if TseStyle(Source).Fonts[ktfSmCaptionTextInactive].Color <> NewColor then
        TseStyle(Source).Fonts[ktfSmCaptionTextInactive].Color := NewColor;
    sfSmCaptionTextNormal:
      if TseStyle(Source).Fonts[ktfSmCaptionTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfSmCaptionTextNormal].Color := NewColor;
    sfStatusPanelTextDisabled:
      if TseStyle(Source).Fonts[ktfStatusPanelTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfStatusPanelTextDisabled].Color := NewColor;
    sfStatusPanelTextNormal:
      if TseStyle(Source).Fonts[ktfStatusPanelTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfStatusPanelTextNormal].Color := NewColor;
    sfTabTextActiveDisabled:
      if TseStyle(Source).Fonts[ktfTabTextActiveDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTabTextActiveDisabled].Color := NewColor;
    sfTabTextActiveHot:
      if TseStyle(Source).Fonts[ktfTabTextActiveHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTabTextActiveHot].Color := NewColor;
    sfTabTextActiveNormal:
      if TseStyle(Source).Fonts[ktfTabTextActiveNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTabTextActiveNormal].Color := NewColor;
    sfTabTextInactiveDisabled:
      if TseStyle(Source).Fonts[ktfTabTextInactiveDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTabTextInactiveDisabled].Color := NewColor;
    sfTabTextInactiveHot:
      if TseStyle(Source).Fonts[ktfTabTextInactiveHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTabTextInactiveHot].Color := NewColor;
    sfTabTextInactiveNormal:
      if TseStyle(Source).Fonts[ktfTabTextInactiveNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTabTextInactiveNormal].Color := NewColor;
    sfTextLabelDisabled:
      if TseStyle(Source).Fonts[ktfStaticTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfStaticTextDisabled].Color := NewColor;
    sfTextLabelFocused:
      if TseStyle(Source).Fonts[ktfStaticTextFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfStaticTextFocused].Color := NewColor;
    sfTextLabelHot:
      if TseStyle(Source).Fonts[ktfStaticTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfStaticTextHot].Color := NewColor;
    sfTextLabelNormal:
      if TseStyle(Source).Fonts[ktfStaticTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfStaticTextNormal].Color := NewColor;
    sfToolItemTextDisabled:
      if TseStyle(Source).Fonts[ktfToolItemTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfToolItemTextDisabled].Color := NewColor;
    sfToolItemTextHot:
      if TseStyle(Source).Fonts[ktfToolItemTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfToolItemTextHot].Color := NewColor;
    sfToolItemTextNormal:
      if TseStyle(Source).Fonts[ktfToolItemTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfToolItemTextNormal].Color := NewColor;
    sfToolItemTextSelected:
      if TseStyle(Source).Fonts[ktfToolItemTextSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfToolItemTextSelected].Color := NewColor;
    sfTreeItemTextDisabled:
      if TseStyle(Source).Fonts[ktfTreeItemTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTreeItemTextDisabled].Color := NewColor;
    sfTreeItemTextFocused:
      if TseStyle(Source).Fonts[ktfTreeItemTextFocused].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTreeItemTextFocused].Color := NewColor;
    sfTreeItemTextHot:
      if TseStyle(Source).Fonts[ktfTreeItemTextHot].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTreeItemTextHot].Color := NewColor;
    sfTreeItemTextNormal:
      if TseStyle(Source).Fonts[ktfTreeItemTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTreeItemTextNormal].Color := NewColor;
    sfTreeItemTextSelected:
      if TseStyle(Source).Fonts[ktfTreeItemTextSelected].Color <> NewColor then
        TseStyle(Source).Fonts[ktfTreeItemTextSelected].Color := NewColor;
    sfWindowTextDisabled:
      if TseStyle(Source).Fonts[ktfWindowTextDisabled].Color <> NewColor then
        TseStyle(Source).Fonts[ktfWindowTextDisabled].Color := NewColor;
    sfWindowTextNormal:
      if TseStyle(Source).Fonts[ktfWindowTextNormal].Color <> NewColor then
        TseStyle(Source).Fonts[ktfWindowTextNormal].Color := NewColor;
  end;
end;

procedure TCustomStyleExt.SetSystemColor(Color, NewColor: TColor);
begin
  if TseStyle(Source).SysColors[Color] <> NewColor then
    TseStyle(Source).SysColors[Color] := NewColor;
end;

function TCustomStyleExt.GetSource: TObject;
begin
  Result := TRttiContext.Create.GetType(Self.ClassType).GetField('FSource').GetValue(Self).AsObject;
end;

procedure TCustomStyleExt.SetStyleInfo(const Value: TStyleInfo);
begin
  TseStyle(Source).StyleSource.Name := Value.Name;
  TseStyle(Source).StyleSource.Author := Value.Author;
  TseStyle(Source).StyleSource.AuthorEMail := Value.AuthorEMail;
  TseStyle(Source).StyleSource.AuthorURL := Value.AuthorURL;
  TseStyle(Source).StyleSource.Version := Value.Version;
end;

function TCustomStyleExt.GetStyleInfo: TStyleInfo;
begin
  Result.Name := TseStyle(Source).StyleSource.Name;
  Result.Author := TseStyle(Source).StyleSource.Author;
  Result.AuthorEMail := TseStyle(Source).StyleSource.AuthorEMail;
  Result.AuthorURL := TseStyle(Source).StyleSource.AuthorURL;
  Result.Version := TseStyle(Source).StyleSource.Version;
end;

{ TCustomStyleHelper }
// function TCustomStyleHelper.GetSource: TObject;
// begin
// {$IFDEF USE_RTTI}
//   Result := TRttiContext.Create.GetType(Self.ClassType).GetField('FSource').GetValue(Self).AsObject;
// {$ELSE}
//   Result := Self.FSource;
// {$ENDIF}
// end;
//
{$ENDIF}

procedure DrawSampleWindow(Style: TCustomStyle; Canvas: TCanvas; ARect: TRect; const ACaption: string;
  HICON: HICON = 0);
var
  LDetails, CaptionDetails, IconDetails: TThemedElementDetails;
  IconRect, BorderRect, CaptionRect, ButtonRect, TextRect: TRect;
  CaptionBitmap: TBitmap;
  ThemeTextColor: TColor;

  function GetBorderSize: TRect;
  var
    Size: TSize;
    Details: TThemedElementDetails;
    Detail: TThemedWindow;
  begin
    Result := Rect(0, 0, 0, 0);
    Detail := twCaptionActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Top := Size.cy;
    Detail := twFrameLeftActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Left := Size.cx;
    Detail := twFrameRightActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Right := Size.cx;
    Detail := twFrameBottomActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Bottom := Size.cy;
  end;

  function RectVCenter(var R: TRect; Bounds: TRect): TRect;
  begin
    OffsetRect(R, -R.Left, -R.Top);
    OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
    OffsetRect(R, Bounds.Left, Bounds.Top);
    Result := R;
  end;

begin
  BorderRect := GetBorderSize;

  CaptionBitmap := TBitmap.Create;
  CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);

  // Draw background
  LDetails.Element := teWindow;
  LDetails.Part := 0;
  Style.DrawElement(Canvas.Handle, LDetails, ARect);

  // Draw caption border
  CaptionRect := Rect(0, 0, CaptionBitmap.Width, CaptionBitmap.Height);
  LDetails := Style.GetElementDetails(twCaptionActive);
  Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect);
  TextRect := CaptionRect;
  CaptionDetails := LDetails;

  // Draw icon
  IconDetails := Style.GetElementDetails(twSysButtonNormal);
  if not Style.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
    ButtonRect := Rect(0, 0, 0, 0);
  IconRect := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
  RectVCenter(IconRect, ButtonRect);
  if ButtonRect.Width > 0 then
    {
      if Assigned(Application.MainForm) then
      DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, Application.MainForm.Icon.Handle, 0, 0, 0, 0, DI_NORMAL);
    }
    if (HICON <> 0) then
      DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, HICON, 0, 0, 0, 0, DI_NORMAL);

  Inc(TextRect.Left, ButtonRect.Width + 5);

  // Draw buttons

  // Close button
  LDetails := Style.GetElementDetails(twCloseButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  // Maximize button
  LDetails := Style.GetElementDetails(twMaxButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  // Minimize button
  LDetails := Style.GetElementDetails(twMinButtonNormal);

  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  // Help button
  LDetails := Style.GetElementDetails(twHelpButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  if (ButtonRect.Left > 0) then
    TextRect.Right := ButtonRect.Left;

  // Draw text
  Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, ACaption, TextRect,
    [tfLeft, tfSingleLine, tfVerticalCenter]);

  // Draw caption
  Canvas.Draw(0, 0, CaptionBitmap);

  CaptionBitmap.Free;

  // Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  // Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  // Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  // Draw Ok button
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left := 30;
  ButtonRect.Top := ARect.Height - 45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(Canvas.Handle, LDetails, 'OK', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  // Draw Cancel button
  ButtonRect.Left := 110;
  ButtonRect.Top := ARect.Height - 45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(Canvas.Handle, LDetails, 'Cancel', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER),
    ThemeTextColor);
end;

{ TVclStylePreview }

constructor TVclStylesPreview.Create(AControl: TComponent);
begin
  inherited;
  FRegion := 0;
  FStyle := nil;
  FCaption := '';
  FIcon := 0;
  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;
end;

destructor TVclStylesPreview.Destroy;
begin
  if FRegion <> 0 then
  begin
    DeleteObject(FRegion);
    FRegion := 0;
  end;
  FBitmap.Free;
  inherited;
end;

procedure TVclStylesPreview.Paint;
var
  LDetails, CaptionDetails, IconDetails: TThemedElementDetails;
  IconRect, BorderRect, CaptionRect, ButtonRect, TextRect: TRect;
  CaptionBitmap: TBitmap;
  ThemeTextColor: TColor;
  ARect, LRect: TRect;
  LRegion: HRGN;
  I: Integer;

  function GetBorderSize: TRect;
  var
    Size: TSize;
    Details: TThemedElementDetails;
    Detail: TThemedWindow;
  begin
    Result := Rect(0, 0, 0, 0);
    Detail := twCaptionActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Top := Size.cy;
    Detail := twFrameLeftActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Left := Size.cx;
    Detail := twFrameRightActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Right := Size.cx;
    Detail := twFrameBottomActive;
    Details := Style.GetElementDetails(Detail);
    Style.GetElementSize(0, Details, esActual, Size);
    Result.Bottom := Size.cy;
  end;

  function RectVCenter(var R: TRect; Bounds: TRect): TRect;
  begin
    OffsetRect(R, -R.Left, -R.Top);
    OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
    OffsetRect(R, Bounds.Left, Bounds.Top);
    Result := R;
  end;

begin
  if FStyle = nil then
    Exit;

  BorderRect := GetBorderSize;
  ARect := ClientRect;
  CaptionBitmap := TBitmap.Create;
  try
    CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);
    FBitmap.Width := ClientRect.Width;
    FBitmap.Height := ClientRect.Height;

    // Draw background
    LDetails.Element := teWindow;
    LDetails.Part := 0;
    DrawStyleElement(FBitmap.Canvas.Handle, LDetails, ARect, True, FStyle);

    // Draw caption border
    CaptionRect := Rect(0, 0, CaptionBitmap.Width, CaptionBitmap.Height);
    LDetails := Style.GetElementDetails(twCaptionActive);

    LRegion := FRegion;
    try
      Style.GetElementRegion(LDetails, ARect, FRegion);
      SetWindowRgn(Handle, FRegion, True);
    finally
      if LRegion <> 0 then
        DeleteObject(LRegion);
    end;

    {
      Style.GetElementRegion(LDetails, ARect, Region);
      SetWindowRgn(Handle, Region, True);
    }

    DrawStyleElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);
    TextRect := CaptionRect;
    CaptionDetails := LDetails;

    // Draw icon
    IconDetails := Style.GetElementDetails(twSysButtonNormal);
    if not Style.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    IconRect := Rect(0, 0, GetSysMetrics(SM_CXSMICON), GetSysMetrics(SM_CYSMICON));
    RectVCenter(IconRect, ButtonRect);

    if (ButtonRect.Width > 0) and (FIcon <> 0) then
      DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, FIcon, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);

    // Draw buttons

    // Close button
    LDetails := Style.GetElementDetails(twCloseButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      DrawStyleElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

    // Maximize button
    LDetails := Style.GetElementDetails(twMaxButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      DrawStyleElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

    // Minimize button
    LDetails := Style.GetElementDetails(twMinButtonNormal);

    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      DrawStyleElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

    // Help button
    LDetails := Style.GetElementDetails(twHelpButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      DrawStyleElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;

    // Draw text
    {$IF RTLVersion > 28}
    if Assigned(Application.Mainform) then
      CaptionBitmap.Canvas.Font.Size := Round(8*Application.MainForm.Monitor.PixelsPerInch / 96)
    else
    {$IFEND}
      CaptionBitmap.Canvas.Font.Size := Round(8*Screen.PixelsPerInch / 96);
    Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, FCaption, TextRect,
      [tfLeft, tfSingleLine, tfVerticalCenter]);

    // Draw caption
    FBitmap.Canvas.Draw(0, 0, CaptionBitmap);
  finally
    CaptionBitmap.Free;
  end;

  // Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    DrawStyleElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);

  // Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);

  // Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);

  {$IF RTLVersion > 28}
  if Assigned(Application.Mainform) then
    FBitmap.Canvas.Font.Size := Round(8 * Application.MainForm.Monitor.PixelsPerInch / Screen.PixelsPerInch)
  else
  {$IFEND}
    FBitmap.Canvas.Font.Size := 8;

  // Draw Main Menu
  LDetails := Style.GetElementDetails(tmMenuBarBackgroundActive);
  LRect := Rect(BorderRect.Left, BorderRect.Top + 1, ARect.Width - BorderRect.Left,BorderRect.Top + FBitmap.Canvas.TextHeight('Tq')+4);
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, LRect, True, FStyle);

  LDetails := Style.GetElementDetails(tmMenuBarItemNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  CaptionRect := Rect(LRect.Left+10,LRect.Top+3, LRect.Left+10+FBitmap.Canvas.TextWidth('File') + 8 ,LRect.Bottom);
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);
  FBitmap.Canvas.Font.Color := ThemeTextColor;
  DrawText(FBitmap.Canvas, 'File', CaptionRect, DT_CENTER);
  CaptionRect.Left := CaptionRect.Right + 2;

  CaptionRect.Right := CaptionRect.Left + FBitmap.Canvas.TextWidth('Edit') + 8;
  LDetails := Style.GetElementDetails(tmMenuBarItemHot);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);
  FBitmap.Canvas.Font.Color := ThemeTextColor;
  DrawText(FBitmap.Canvas, 'Edit', CaptionRect, DT_CENTER);
  CaptionRect.Left := CaptionRect.Right + 2;

  CaptionRect.Right := CaptionRect.Left + FBitmap.Canvas.TextWidth('View') + 8;
  LDetails := Style.GetElementDetails(tmMenuBarItemNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);
  FBitmap.Canvas.Font.Color := ThemeTextColor;
  DrawText(FBitmap.Canvas, 'View', CaptionRect, DT_CENTER);
  CaptionRect.Left := CaptionRect.Right + 2;

  CaptionRect.Right := CaptionRect.Left + FBitmap.Canvas.TextWidth('Help') + 8;
  LDetails := Style.GetElementDetails(tmMenuBarItemDisabled);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, CaptionRect, True, FStyle);
  FBitmap.Canvas.Font.Color := ThemeTextColor;
  DrawText(FBitmap.Canvas, 'Help', CaptionRect, DT_CENTER);

  // Draw ToolButtons
  LDetails := Style.GetElementDetails(ttbButtonNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  ButtonRect.Left := BorderRect.Left + 2;
  for i := 1 to 3 do
  begin
    ButtonRect.Top := LRect.Top + 30;
    {$IF RTLVersion > 28}
    if Assigned(Application.Mainform) then
    begin
      ButtonRect.Width := Round(65 * Application.MainForm.Monitor.PixelsPerInch / 96);
      ButtonRect.Height := Round(25 * Application.MainForm.Monitor.PixelsPerInch / 96);
    end
    else
    {$IFEND}
    begin
      ButtonRect.Width := Round(65 * Screen.PixelsPerInch / 96);
      ButtonRect.Height := Round(25 * Screen.PixelsPerInch / 96);
    end;

    DrawStyleElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);
    Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'ToolButton' + IntToStr(I), ButtonRect,
      TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

    ButtonRect.Left := ButtonRect.Right + 2;
  end;

  // Draw Normal
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left := BorderRect.Left + 2;
  ButtonRect.Top := ARect.Height - 45;
  {$IF RTLVersion > 28}
  if Assigned(Application.Mainform) then
  begin
    ButtonRect.Width := Round(65 * Application.MainForm.Monitor.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Application.MainForm.Monitor.PixelsPerInch / 96);
  end
  else
  {$IFEND}
  begin
    ButtonRect.Width := Round(65 * Screen.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Screen.PixelsPerInch / 96);
  end;
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Normal', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER),
    ThemeTextColor);

  // Draw Hot
  LDetails := Style.GetElementDetails(tbPushButtonHot);
  ButtonRect.Left := ButtonRect.Right + 2;
  ButtonRect.Top := ARect.Height - 45;
  {$IF RTLVersion > 28}
  if Assigned(Application.Mainform) then
  begin
    ButtonRect.Width := Round(65 * Application.MainForm.Monitor.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Application.MainForm.Monitor.PixelsPerInch / 96);
  end
  else
  {$IFEND}
  begin
    ButtonRect.Width := Round(65 * Screen.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Screen.PixelsPerInch / 96);
  end;
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Hot', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER),
    ThemeTextColor);

  // Draw Pressed
  LDetails := Style.GetElementDetails(tbPushButtonPressed);
  ButtonRect.Left := ButtonRect.Right + 2;
  ButtonRect.Top := ARect.Height - 45;
  {$IF RTLVersion > 28}
  if Assigned(Application.Mainform) then
  begin
    ButtonRect.Width := Round(65 * Application.MainForm.Monitor.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Application.MainForm.Monitor.PixelsPerInch / 96);
  end
  else
  {$IFEND}
  begin
    ButtonRect.Width := Round(65 * Screen.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Screen.PixelsPerInch / 96);
  end;
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Pressed', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER),
    ThemeTextColor);

  // Draw Disabled
  LDetails := Style.GetElementDetails(tbPushButtonDisabled);
  ButtonRect.Left := ButtonRect.Right + 2;
  ButtonRect.Top := ARect.Height - 45;
  {$IF RTLVersion > 28}
  if Assigned(Application.Mainform) then
  begin
    ButtonRect.Width := Round(65 * Application.MainForm.Monitor.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Application.MainForm.Monitor.PixelsPerInch / 96);
  end
  else
  {$IFEND}
  begin
    ButtonRect.Width := Round(65 * Screen.PixelsPerInch / 96);
    ButtonRect.Height := Round(25 * Screen.PixelsPerInch / 96);
  end;
  DrawStyleElement(FBitmap.Canvas.Handle, LDetails, ButtonRect, True, FStyle);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Disabled', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER),
    ThemeTextColor);

  Canvas.Draw(0, 0, FBitmap);
end;

initialization

{$IFDEF USE_VCL_STYLESAPI}
  InitStyleAPI;
{$ENDIF}

finalization

{$IFDEF USE_VCL_STYLESAPI}
  FinalizeStyleAPI;
{$ENDIF}

end.
