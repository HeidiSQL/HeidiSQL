//**************************************************************************************************
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
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2019 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Vcl.Styles.Ext;

interface

{$IF RTLVersion>=24}
  {$LEGACYIFEND ON}
{$IFEND}
{$DEFINE USE_VCL_STYLESAPI}


Uses
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
  ///   StyleName : string;<para></para>
  ///   SourceInfo: TSourceInfo;<para></para>
  ///   LStyle    : TCustomStyleServices;<para></para>
  ///   FPreview  : TVclStylesPreview;<para></para>
  /// begin<para></para>
  ///    FPreview:=TVclStylesPreview.Create(Self);<para></para>
  ///    FPreview.Parent:=PanelPreview;<para></para>
  ///    FPreview.BoundsRect := PanelPreview.ClientRect;<para></para>
  ///    StyleName:='Carbon';<para></para>
  ///    if (StyleName &lt;&gt;'') and (not SameText(StyleName, 'Windows')) then<para></para>
  ///    begin<para></para>
  ///      TStyleManager.StyleNames;//call DiscoverStyleResources<para></para>
  ///      LStyle:=TStyleManager.Style[StyleName];<para></para>
  ///      FPreview.Caption:=StyleName;<para></para>
  ///      FPreview.Style:=LStyle;<para></para>
  ///      TVclStylesPreviewClass(FPreview).Paint;<para></para>
  ///    end;<para></para>
  ///    ....<para></para>
  /// end;<para></para>
  /// </code>
  /// </remarks>
  TVclStylesPreview = class(TCustomControl)
  private
    FStyle: TCustomStyleServices;//TCustomStyle;
    FIcon: HICON;
    FCaption: TCaption;
    FRegion : HRGN;
    FBitmap: TBitmap;
  protected
    procedure Paint; override;
  public
    property Icon:HICON read FIcon Write FIcon;
    property Style:TCustomStyleServices read FStyle Write FStyle;
    property Caption : TCaption read FCaption write FCaption;
    property BitMap : TBitmap read FBitmap write FBitmap;
    constructor Create(AControl: TComponent); override;
    destructor Destroy; override;
  end;


  TStyleServicesHandle = type Pointer;
  TSourceInfo = record
    Data: TStyleServicesHandle;
    StyleClass: TCustomStyleServicesClass;
  end;

  {$REGION 'Documentation'}
  ///	<summary>Helper class for the TStyleManager
  ///	</summary>
  {$ENDREGION}
  TStyleManagerHelper = Class Helper for TStyleManager
  strict private
   class function GetStyleSourceInfo(const StyleName: string): TSourceInfo; static;
   class function GetStyles: TList<TCustomStyleServices>;
   class function _GetStyles: TList<TCustomStyleServices>; static;
  public
   class function RegisteredStyles: TDictionary<string, TSourceInfo>;
   {$REGION 'Documentation'}
   ///	<summary>Get the TSourceInfo for a Style
   ///	</summary>
   {$ENDREGION}
   class property StyleSourceInfo[const StyleName: string]: TSourceInfo read GetStyleSourceInfo;
   {$REGION 'Documentation'}
   ///	<summary>Send the CM_CUSTOMSTYLECHANGED message to all the forms
   ///	</summary>
   {$ENDREGION}
   class procedure RefreshCurrentTheme;
   {$REGION 'Documentation'}
   ///	<summary>Return the loaded styles (TCustomStyleServices) in the system
   ///	</summary>
   {$ENDREGION}
   class property Styles: TList<TCustomStyleServices> read _GetStyles;
   {$REGION 'Documentation'}
   ///	<summary>Force to reload a modified vcl style
   ///	</summary>
   {$ENDREGION}
   class procedure ReloadStyle(const StyleName : string);
   {$REGION 'Documentation'}
   ///	<summary>remove a vcl style
   ///	</summary>
   {$ENDREGION}
   class procedure RemoveStyle(const StyleName : string);
   class function  StyleLoaded(const StyleName : string) : Boolean;
   end;

const
  VclStyles_MaxSysColor = 23;
  VclStyles_SysColors: array[0..VclStyles_MaxSysColor - 1] of TIdentMapEntry = (
    (Value: Vcl.Graphics.clActiveBorder; Name: 'clActiveBorder'),
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

    procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
    procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
    function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
    function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
    procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string;hIcon:HICON=0);overload;


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
    ///	<summary>Create a  TCustomStyleExt using a vcl style stored in a file
    ///	</summary>
    {$ENDREGION}
    constructor Create(const FileName :string);reintroduce; overload;
    {$REGION 'Documentation'}
    ///	<summary>Create a  TCustomStyleExt using a vcl style stored in a stream
    ///	</summary>
    {$ENDREGION}
    constructor Create(const Stream:TStream);reintroduce; overload;
    constructor Create(const Style:TCustomStyle);reintroduce; overload;
    destructor Destroy;override;
    {$REGION 'Documentation'}
    ///	<summary>Replace a internal bitmap of the Style
    ///	</summary>
    {$ENDREGION}
    procedure ReplaceBitmap(DestIndex : Integer;Src: TBitmap);
    {$REGION 'Documentation'}
    ///	<summary>Set a returns the TStyleInfo fo the current style
    ///	</summary>
    {$ENDREGION}
    property StyleInfo: TStyleInfo read GetStyleInfo write SetStyleInfo;
    {$REGION 'Documentation'}
    ///	<summary>Return the list of the bitmaps of the style
    ///	</summary>
    {$ENDREGION}
    property BitmapList: TObjectList<TBitmap> read GetBitmapList;
    property LocalStream: TStream read FStream;
    {$REGION 'Documentation'}
    ///	<summary>Copy the modified style to an Stream
    ///	</summary>
    {$ENDREGION}
    procedure CopyToStream(Stream : TStream);

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
//function DoHasElementFixedPosition(Details: TThemedElementDetails): Boolean;

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
 Winapi.Messages,
{$ENDIF}
 Vcl.Dialogs;


{$IF (DEFINED (USE_VCL_STYLESAPI) and (CompilerVersion >=23))}
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
  p : Pointer;
{$IFEND}
begin
  {$IF (CompilerVersion <31)}
  Result:= Self.FRegisteredStyleHooks;
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
  //Use the address of the Self.FRegSysStylesList property to calculate the offset of the FRegisteredStyleHooks
   p      := Pointer(PByte(@Self.FRegSysStylesList) - Offset);
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
  t: TPair<string, TStyleManager.TSourceInfo>;
  SourceInfo: TSourceInfo;
  LRegisteredStyles: TDictionary<string, TStyleManager.TSourceInfo>;
  {$IF (CompilerVersion >= 31)}
  p: Pointer;
  {$IFEND}
begin
  Result := TDictionary<string, TSourceInfo>.Create;
  {$IF (CompilerVersion < 31)}
  LRegisteredStyles := Self.FRegisteredStyles;
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
  //Use the address of the Self.Flags property to calculate the offset of the FRegisteredStyles
  {$IFDEF CPUX64}
  p := Pointer(PByte(@Self.Flags) + 8);
  {$ELSE}
  p := Pointer(PByte(@Self.Flags) + 4);
  {$ENDIF CPUX64}
  LRegisteredStyles := TDictionary<string, TStyleManager.TSourceInfo>(p^);
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


class procedure TStyleManagerHelper.ReloadStyle(const StyleName : string);
var
  LStyle: TCustomStyleServices;
  LPair : TPair<string, TSourceInfo>;
  LRegisteredStyles : TDictionary<string, TSourceInfo>;
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
       if (LPair.Value.Data<>nil) then
       begin
         TStream(LPair.Value.Data).Position:=0;
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
  LPair : TPair<string, TSourceInfo>;
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

class function TStyleManagerHelper.StyleLoaded(
  const StyleName: string): Boolean;
begin
   Result := TStyleManager.Style[StyleName] <> nil;
end;

function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
begin
 Result:=nil;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
      Result:=TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
end;

function IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
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

procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
begin
  if not IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(ControlClass, TStyleHook);
end;

procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
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
 I :  Integer;
begin
  Stream.Size := 0;
  Stream.Position := 0;

  TseStyle(Source).FCleanCopy.Name := TseStyle(Source).StyleSource.Name;
  TseStyle(Source).FCleanCopy.Author := TseStyle(Source).StyleSource.Author;
  TseStyle(Source).FCleanCopy.AuthorEMail := TseStyle(Source).StyleSource.AuthorEMail;
  TseStyle(Source).FCleanCopy.AuthorURL := TseStyle(Source).StyleSource.AuthorURL;
  TseStyle(Source).FCleanCopy.Version := TseStyle(Source).StyleSource.Version;

  //Replace the modified bitmaps
  for i := 0 to TseStyle(Source).FCleanCopy.Bitmaps.Count-1  do
    TseStyle(Source).FCleanCopy.Bitmaps[i].Assign(TseStyle(Source).StyleSource.Bitmaps[i]);

  //TseStyle(Source).StyleSource.SysColors.Assign(TseStyle(Source).SysColors);

  //Replace the modified colors
  TseStyle(Source).FCleanCopy.SysColors.Assign(TseStyle(Source).SysColors);
  TseStyle(Source).FCleanCopy.Colors.Assign(TseStyle(Source).Colors);
  TseStyle(Source).FCleanCopy.Fonts.Assign(TseStyle(Source).Fonts);

  //ShowMessage(ColorToString(TseStyle(Source).SysColors[clWindow]));
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
  //Style.Source
  //inherited Create(TStream(Style.));
end;

constructor TCustomStyleExt.Create(const Stream: TStream);
var
  LSource: TObject;
begin
  inherited Create;
  FStream := TMemoryStream.Create;

  Stream.Seek(0, soBeginning); //index 0 to load
  FStream.CopyFrom(Stream, Stream.Size);
  Stream.Seek(0, soBeginning); //restore index 0 after
  LSource:=Source;
  FStream.Seek(0, soBeginning);//index 0 to load
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
  LseBitmap : TseBitmap;
begin
  LSource := Source;
  Result := TObjectList<TBitmap>.Create;
  for I:=0 to TseStyle(LSource).StyleSource.Bitmaps.Count-1 do
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
  SrcRect := Rect(0 ,0, Src.Width, Src.Height);
  DstRect := Rect(0 ,0, Src.Width, Src.Height);
  Canvas := LBitMap.Canvas;
  SetStretchBltMode(Canvas.Handle, COLORONCOLOR);
  if LBitMap.AlphaBlend then
  begin
    BF.BlendOp := AC_SRC_OVER;
    BF.BlendFlags := 0;
    BF.SourceConstantAlpha := 255;
    BF.AlphaFormat := AC_SRC_ALPHA;
    Winapi.Windows.AlphaBlend(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, BF);
  end
  else
  if LBitMap.Transparent then
  begin
    Winapi.Windows.TransparentBlt(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, seTransparent);
  end
  else
  begin
    Winapi.Windows.StretchBlt(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);
  end;
end;

procedure TCustomStyleExt.SetStyleColor(Color: TStyleColor; NewColor: TColor);
begin
  case Color of
    scBorder: if TSeStyle(Source).Colors[ktcBorder] <> NewColor then TSeStyle(Source).Colors[ktcBorder] := NewColor;
    scButtonDisabled:  if TSeStyle(Source).Colors[ktcButtonDisabled] <> NewColor then TSeStyle(Source).Colors[ktcButtonDisabled] := NewColor;
    scButtonFocused:  if TSeStyle(Source).Colors[ktcButtonFocused] <> NewColor then TSeStyle(Source).Colors[ktcButtonFocused] := NewColor;
    scButtonHot:  if TSeStyle(Source).Colors[ktcButtonHot] <> NewColor then TSeStyle(Source).Colors[ktcButtonHot] := NewColor;
    scButtonNormal:  if TSeStyle(Source).Colors[ktcButton] <> NewColor then TSeStyle(Source).Colors[ktcButton] := NewColor;
    scButtonPressed:  if TSeStyle(Source).Colors[ktcButtonPressed] <> NewColor then TSeStyle(Source).Colors[ktcButtonPressed] := NewColor;
    scCategoryButtons:  if TSeStyle(Source).Colors[ktcCategoryButtons] <> NewColor then TSeStyle(Source).Colors[ktcCategoryButtons] := NewColor;
    scCategoryButtonsGradientBase:  if TSeStyle(Source).Colors[ktcCategoryButtonsGradientBase] <> NewColor then TSeStyle(Source).Colors[ktcCategoryButtonsGradientBase] := NewColor;
    scCategoryButtonsGradientEnd:  if TSeStyle(Source).Colors[ktcCategoryButtonsGradientEnd] <> NewColor then TSeStyle(Source).Colors[ktcCategoryButtonsGradientEnd] := NewColor;
    scCategoryPanelGroup:  if TSeStyle(Source).Colors[ktcCategoryPanelGroup] <> NewColor then TSeStyle(Source).Colors[ktcCategoryPanelGroup] := NewColor;
    scComboBox:  if TSeStyle(Source).Colors[ktcComboBox] <> NewColor then TSeStyle(Source).Colors[ktcComboBox] := NewColor;
    scComboBoxDisabled:  if TSeStyle(Source).Colors[ktcComboBoxDisabled] <> NewColor then TSeStyle(Source).Colors[ktcComboBoxDisabled] := NewColor;
    scEdit:  if TSeStyle(Source).Colors[ktcEdit] <> NewColor then TSeStyle(Source).Colors[ktcEdit] := NewColor;
    scEditDisabled:  if TSeStyle(Source).Colors[ktcEditDisabled] <> NewColor then TSeStyle(Source).Colors[ktcEditDisabled] := NewColor;
    scGrid:  if TSeStyle(Source).Colors[ktcGrid] <> NewColor then TSeStyle(Source).Colors[ktcGrid] := NewColor;
    scGenericBackground:  if TSeStyle(Source).Colors[ktcGenericBackground] <> NewColor then TSeStyle(Source).Colors[ktcGenericBackground] := NewColor;
    scGenericGradientEnd:  if TSeStyle(Source).Colors[ktcGenericGradientEnd] <> NewColor then TSeStyle(Source).Colors[ktcGenericGradientEnd] := NewColor;
    scGenericGradientBase:  if TSeStyle(Source).Colors[ktcGenericGradientBase] <> NewColor then TSeStyle(Source).Colors[ktcGenericGradientBase] := NewColor;
    scHintGradientBase:  if TSeStyle(Source).Colors[ktcHintGradientBase] <> NewColor then TSeStyle(Source).Colors[ktcHintGradientBase] := NewColor;
    scHintGradientEnd:  if TSeStyle(Source).Colors[ktcHintGradientEnd] <> NewColor then TSeStyle(Source).Colors[ktcHintGradientEnd] := NewColor;
    scListBox:  if TSeStyle(Source).Colors[ktcListBox] <> NewColor then TSeStyle(Source).Colors[ktcListBox] := NewColor;
    scListBoxDisabled:  if TSeStyle(Source).Colors[ktcListBoxDisabled] <> NewColor then TSeStyle(Source).Colors[ktcListBoxDisabled] := NewColor;
    scListView:  if TSeStyle(Source).Colors[ktcListView] <> NewColor then TSeStyle(Source).Colors[ktcListView] := NewColor;
    scPanel:  if TSeStyle(Source).Colors[ktcPanel] <> NewColor then TSeStyle(Source).Colors[ktcPanel] := NewColor;
    scPanelDisabled:  if TSeStyle(Source).Colors[ktcPanelDisabled] <> NewColor then TSeStyle(Source).Colors[ktcPanelDisabled] := NewColor;
    scSplitter:  if TSeStyle(Source).Colors[ktcSplitter] <> NewColor then TSeStyle(Source).Colors[ktcSplitter] := NewColor;
    scToolBarGradientBase:  if TSeStyle(Source).Colors[ktcToolBarGradientBase] <> NewColor then TSeStyle(Source).Colors[ktcToolBarGradientBase] := NewColor;
    scToolBarGradientEnd:  if TSeStyle(Source).Colors[ktcToolBarGradientEnd] <> NewColor then TSeStyle(Source).Colors[ktcToolBarGradientEnd] := NewColor;
    scTreeView:  if TSeStyle(Source).Colors[ktcTreeView] <> NewColor then TSeStyle(Source).Colors[ktcTreeView] := NewColor;
    scWindow: if TSeStyle(Source).Colors[ktcWindow] <> NewColor then TSeStyle(Source).Colors[ktcWindow] := NewColor;
  end;
end;

procedure TCustomStyleExt.SetStyleFont(Font: TStyleFont; NewFont: TFont);
begin
  case Font of
    sfButtonTextDisabled: if TSeStyle(Source).Fonts[ktfButtonTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfButtonTextDisabled] := NewFont;
    sfButtonTextFocused: if TSeStyle(Source).Fonts[ktfButtonTextFocused] <> NewFont then TSeStyle(Source).Fonts[ktfButtonTextFocused] := NewFont;
    sfButtonTextHot: if TSeStyle(Source).Fonts[ktfButtonTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfButtonTextHot] := NewFont;
    sfButtonTextNormal: if TSeStyle(Source).Fonts[ktfButtonTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfButtonTextNormal] := NewFont;
    sfButtonTextPressed: if TSeStyle(Source).Fonts[ktfButtonTextPressed] <> NewFont then TSeStyle(Source).Fonts[ktfButtonTextPressed] := NewFont;
    sfCaptionTextInactive: if TSeStyle(Source).Fonts[ktfCaptionTextInactive] <> NewFont then TSeStyle(Source).Fonts[ktfCaptionTextInactive] := NewFont;
    sfCaptionTextNormal: if TSeStyle(Source).Fonts[ktfCaptionTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfCaptionTextNormal] := NewFont;
    sfCategoryPanelGroupHeaderHot: if TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot] <> NewFont then TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot] := NewFont;
    sfCategoryPanelGroupHeaderNormal: if TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal] <> NewFont then TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal] := NewFont;
    sfCatgeoryButtonsCategoryNormal: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal] <> NewFont then TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal] := NewFont;
    sfCatgeoryButtonsCategorySelected: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected] <> NewFont then TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected] := NewFont;
    sfCatgeoryButtonsHot: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsHot] <> NewFont then TSeStyle(Source).Fonts[ktfCatgeoryButtonsHot] := NewFont;
    sfCatgeoryButtonsNormal: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsNormal] <> NewFont then TSeStyle(Source).Fonts[ktfCatgeoryButtonsNormal] := NewFont;
    sfCatgeoryButtonsSelected: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsSelected] <> NewFont then TSeStyle(Source).Fonts[ktfCatgeoryButtonsSelected] := NewFont;
    sfCheckBoxTextDisabled: if TSeStyle(Source).Fonts[ktfCheckBoxTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfCheckBoxTextDisabled] := NewFont;
    sfCheckBoxTextFocused: if TSeStyle(Source).Fonts[ktfCheckBoxTextFocused] <> NewFont then TSeStyle(Source).Fonts[ktfCheckBoxTextFocused] := NewFont;
    sfCheckBoxTextHot: if TSeStyle(Source).Fonts[ktfCheckBoxTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfCheckBoxTextHot] := NewFont;
    sfCheckBoxTextNormal: if TSeStyle(Source).Fonts[ktfCheckBoxTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfCheckBoxTextNormal] := NewFont;
    sfCheckBoxTextPressed: if TSeStyle(Source).Fonts[ktfCheckBoxTextPressed] <> NewFont then TSeStyle(Source).Fonts[ktfCheckBoxTextPressed] := NewFont;
    sfComboBoxItemDisabled: if TSeStyle(Source).Fonts[ktfComboBoxItemDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfComboBoxItemDisabled] := NewFont;
    sfComboBoxItemFocused: if TSeStyle(Source).Fonts[ktfComboBoxItemFocused] <> NewFont then TSeStyle(Source).Fonts[ktfComboBoxItemFocused] := NewFont;
    sfComboBoxItemHot: if TSeStyle(Source).Fonts[ktfComboBoxItemHot] <> NewFont then TSeStyle(Source).Fonts[ktfComboBoxItemHot] := NewFont;
    sfComboBoxItemNormal: if TSeStyle(Source).Fonts[ktfComboBoxItemNormal] <> NewFont then TSeStyle(Source).Fonts[ktfComboBoxItemNormal] := NewFont;
    sfComboBoxItemSelected: if TSeStyle(Source).Fonts[ktfComboBoxItemSelected] <> NewFont then TSeStyle(Source).Fonts[ktfComboBoxItemSelected] := NewFont;
    sfEditBoxTextDisabled: if TSeStyle(Source).Fonts[ktfEditBoxTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfEditBoxTextDisabled] := NewFont;
    sfEditBoxTextFocused: if TSeStyle(Source).Fonts[ktfEditBoxTextFocused] <> NewFont then TSeStyle(Source).Fonts[ktfEditBoxTextFocused] := NewFont;
    sfEditBoxTextHot: if TSeStyle(Source).Fonts[ktfEditBoxTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfEditBoxTextHot] := NewFont;
    sfEditBoxTextNormal: if TSeStyle(Source).Fonts[ktfEditBoxTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfEditBoxTextNormal] := NewFont;
    sfEditBoxTextSelected: if TSeStyle(Source).Fonts[ktfEditBoxTextSelected] <> NewFont then TSeStyle(Source).Fonts[ktfEditBoxTextSelected] := NewFont;
    sfGridItemFixedHot: if TSeStyle(Source).Fonts[ktfGridItemFixedHot] <> NewFont then TSeStyle(Source).Fonts[ktfGridItemFixedHot] := NewFont;
    sfGridItemFixedNormal: if TSeStyle(Source).Fonts[ktfGridItemFixedNormal] <> NewFont then TSeStyle(Source).Fonts[ktfGridItemFixedNormal] := NewFont;
    sfGridItemFixedPressed: if TSeStyle(Source).Fonts[ktfGridItemFixedPressed] <> NewFont then TSeStyle(Source).Fonts[ktfGridItemFixedPressed] := NewFont;
    sfGridItemNormal: if TSeStyle(Source).Fonts[ktfGridItemNormal] <> NewFont then TSeStyle(Source).Fonts[ktfGridItemNormal] := NewFont;
    sfGridItemSelected: if TSeStyle(Source).Fonts[ktfGridItemSelected] <> NewFont then TSeStyle(Source).Fonts[ktfGridItemSelected] := NewFont;
    sfGroupBoxTextDisabled: if TSeStyle(Source).Fonts[ktfGroupBoxTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfGroupBoxTextDisabled] := NewFont;
    sfGroupBoxTextNormal: if TSeStyle(Source).Fonts[ktfGroupBoxTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfGroupBoxTextNormal] := NewFont;
    sfHeaderSectionTextDisabled: if TSeStyle(Source).Fonts[ktfHeaderSectionTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfHeaderSectionTextDisabled] := NewFont;
    sfHeaderSectionTextHot: if TSeStyle(Source).Fonts[ktfHeaderSectionTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfHeaderSectionTextHot] := NewFont;
    sfHeaderSectionTextNormal: if TSeStyle(Source).Fonts[ktfHeaderSectionTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfHeaderSectionTextNormal] := NewFont;
    sfHeaderSectionTextPressed: if TSeStyle(Source).Fonts[ktfHeaderSectionTextPressed] <> NewFont then TSeStyle(Source).Fonts[ktfHeaderSectionTextPressed] := NewFont;
    sfListItemTextDisabled: if TSeStyle(Source).Fonts[ktfListItemTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfListItemTextDisabled] := NewFont;
    sfListItemTextFocused: if TSeStyle(Source).Fonts[ktfListItemTextFocused] <> NewFont then TSeStyle(Source).Fonts[ktfListItemTextFocused] := NewFont;
    sfListItemTextHot: if TSeStyle(Source).Fonts[ktfListItemTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfListItemTextHot] := NewFont;
    sfListItemTextNormal: if TSeStyle(Source).Fonts[ktfListItemTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfListItemTextNormal] := NewFont;
    sfListItemTextSelected: if TSeStyle(Source).Fonts[ktfListItemTextSelected] <> NewFont then TSeStyle(Source).Fonts[ktfListItemTextSelected] := NewFont;
    sfMenuItemTextDisabled: if TSeStyle(Source).Fonts[ktfMenuItemTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfMenuItemTextDisabled] := NewFont;
    sfMenuItemTextHot: if TSeStyle(Source).Fonts[ktfMenuItemTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfMenuItemTextHot] := NewFont;
    sfMenuItemTextNormal: if TSeStyle(Source).Fonts[ktfMenuItemTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfMenuItemTextNormal] := NewFont;
    sfMenuItemTextSelected: if TSeStyle(Source).Fonts[ktfMenuItemTextSelected] <> NewFont then TSeStyle(Source).Fonts[ktfMenuItemTextSelected] := NewFont;
    sfPanelTextDisabled: if TSeStyle(Source).Fonts[ktfPanelTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfPanelTextDisabled] := NewFont;
    sfPanelTextNormal: if TSeStyle(Source).Fonts[ktfPanelTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfPanelTextNormal] := NewFont;
    sfPopupMenuItemTextDisabled: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfPopupMenuItemTextDisabled] := NewFont;
    sfPopupMenuItemTextHot: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfPopupMenuItemTextHot] := NewFont;
    sfPopupMenuItemTextNormal: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfPopupMenuItemTextNormal] := NewFont;
    sfPopupMenuItemTextSelected: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextSelected] <> NewFont then TSeStyle(Source).Fonts[ktfPopupMenuItemTextSelected] := NewFont;
    sfRadioButtonTextDisabled: if TSeStyle(Source).Fonts[ktfRadioButtonTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfRadioButtonTextDisabled] := NewFont;
    sfRadioButtonTextFocused: if TSeStyle(Source).Fonts[ktfRadioButtonTextFocused] <> NewFont then TSeStyle(Source).Fonts[ktfRadioButtonTextFocused] := NewFont;
    sfRadioButtonTextHot: if TSeStyle(Source).Fonts[ktfRadioButtonTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfRadioButtonTextHot] := NewFont;
    sfRadioButtonTextNormal: if TSeStyle(Source).Fonts[ktfRadioButtonTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfRadioButtonTextNormal] := NewFont;
    sfRadioButtonTextPressed: if TSeStyle(Source).Fonts[ktfRadioButtonTextPressed] <> NewFont then TSeStyle(Source).Fonts[ktfRadioButtonTextPressed] := NewFont;
    sfSmCaptionTextInactive: if TSeStyle(Source).Fonts[ktfSmCaptionTextInactive] <> NewFont then TSeStyle(Source).Fonts[ktfSmCaptionTextInactive] := NewFont;
    sfSmCaptionTextNormal: if TSeStyle(Source).Fonts[ktfSmCaptionTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfSmCaptionTextNormal] := NewFont;
    sfStatusPanelTextDisabled: if TSeStyle(Source).Fonts[ktfStatusPanelTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfStatusPanelTextDisabled] := NewFont;
    sfStatusPanelTextNormal: if TSeStyle(Source).Fonts[ktfStatusPanelTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfStatusPanelTextNormal] := NewFont;
    sfTabTextActiveDisabled: if TSeStyle(Source).Fonts[ktfTabTextActiveDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfTabTextActiveDisabled] := NewFont;
    sfTabTextActiveHot: if TSeStyle(Source).Fonts[ktfTabTextActiveHot] <> NewFont then TSeStyle(Source).Fonts[ktfTabTextActiveHot] := NewFont;
    sfTabTextActiveNormal: if TSeStyle(Source).Fonts[ktfTabTextActiveNormal] <> NewFont then TSeStyle(Source).Fonts[ktfTabTextActiveNormal] := NewFont;
    sfTabTextInactiveDisabled: if TSeStyle(Source).Fonts[ktfTabTextInactiveDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfTabTextInactiveDisabled] := NewFont;
    sfTabTextInactiveHot: if TSeStyle(Source).Fonts[ktfTabTextInactiveHot] <> NewFont then TSeStyle(Source).Fonts[ktfTabTextInactiveHot] := NewFont;
    sfTabTextInactiveNormal: if TSeStyle(Source).Fonts[ktfTabTextInactiveNormal] <> NewFont then TSeStyle(Source).Fonts[ktfTabTextInactiveNormal] := NewFont;
    sfTextLabelDisabled: if TSeStyle(Source).Fonts[ktfStaticTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfStaticTextDisabled] := NewFont;
    sfTextLabelFocused: if TSeStyle(Source).Fonts[ktfStaticTextFocused] <> NewFont then TSeStyle(Source).Fonts[ktfStaticTextFocused] := NewFont;
    sfTextLabelHot: if TSeStyle(Source).Fonts[ktfStaticTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfStaticTextHot] := NewFont;
    sfTextLabelNormal: if TSeStyle(Source).Fonts[ktfStaticTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfStaticTextNormal] := NewFont;
    sfToolItemTextDisabled: if TSeStyle(Source).Fonts[ktfToolItemTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfToolItemTextDisabled] := NewFont;
    sfToolItemTextHot: if TSeStyle(Source).Fonts[ktfToolItemTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfToolItemTextHot] := NewFont;
    sfToolItemTextNormal: if TSeStyle(Source).Fonts[ktfToolItemTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfToolItemTextNormal] := NewFont;
    sfToolItemTextSelected: if TSeStyle(Source).Fonts[ktfToolItemTextSelected] <> NewFont then TSeStyle(Source).Fonts[ktfToolItemTextSelected] := NewFont;
    sfTreeItemTextDisabled: if TSeStyle(Source).Fonts[ktfTreeItemTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfTreeItemTextDisabled] := NewFont;
    sfTreeItemTextFocused: if TSeStyle(Source).Fonts[ktfTreeItemTextFocused] <> NewFont then TSeStyle(Source).Fonts[ktfTreeItemTextFocused] := NewFont;
    sfTreeItemTextHot: if TSeStyle(Source).Fonts[ktfTreeItemTextHot] <> NewFont then TSeStyle(Source).Fonts[ktfTreeItemTextHot] := NewFont;
    sfTreeItemTextNormal: if TSeStyle(Source).Fonts[ktfTreeItemTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfTreeItemTextNormal] := NewFont;
    sfTreeItemTextSelected: if TSeStyle(Source).Fonts[ktfTreeItemTextSelected] <> NewFont then TSeStyle(Source).Fonts[ktfTreeItemTextSelected] := NewFont;
    sfWindowTextDisabled: if TSeStyle(Source).Fonts[ktfWindowTextDisabled] <> NewFont then TSeStyle(Source).Fonts[ktfWindowTextDisabled] := NewFont;
    sfWindowTextNormal: if TSeStyle(Source).Fonts[ktfWindowTextNormal] <> NewFont then TSeStyle(Source).Fonts[ktfWindowTextNormal] := NewFont;
  end;
end;

procedure TCustomStyleExt.SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
begin
  case Font of
    sfButtonTextDisabled: if TSeStyle(Source).Fonts[ktfButtonTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfButtonTextDisabled].Color := NewColor;
    sfButtonTextFocused: if TSeStyle(Source).Fonts[ktfButtonTextFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfButtonTextFocused].Color := NewColor;
    sfButtonTextHot: if TSeStyle(Source).Fonts[ktfButtonTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfButtonTextHot].Color := NewColor;
    sfButtonTextNormal: if TSeStyle(Source).Fonts[ktfButtonTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfButtonTextNormal].Color := NewColor;
    sfButtonTextPressed: if TSeStyle(Source).Fonts[ktfButtonTextPressed].Color <> NewColor then TSeStyle(Source).Fonts[ktfButtonTextPressed].Color := NewColor;
    sfCaptionTextInactive: if TSeStyle(Source).Fonts[ktfCaptionTextInactive].Color <> NewColor then TSeStyle(Source).Fonts[ktfCaptionTextInactive].Color := NewColor;
    sfCaptionTextNormal: if TSeStyle(Source).Fonts[ktfCaptionTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfCaptionTextNormal].Color := NewColor;
    sfCategoryPanelGroupHeaderHot: if TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot].Color := NewColor;
    sfCategoryPanelGroupHeaderNormal: if TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal].Color := NewColor;
    sfCatgeoryButtonsCategoryNormal: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal].Color := NewColor;
    sfCatgeoryButtonsCategorySelected: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected].Color := NewColor;
    sfCatgeoryButtonsHot: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfCatgeoryButtonsHot].Color := NewColor;
    sfCatgeoryButtonsNormal: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfCatgeoryButtonsNormal].Color := NewColor;
    sfCatgeoryButtonsSelected: if TSeStyle(Source).Fonts[ktfCatgeoryButtonsSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfCatgeoryButtonsSelected].Color := NewColor;
    sfCheckBoxTextDisabled: if TSeStyle(Source).Fonts[ktfCheckBoxTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfCheckBoxTextDisabled].Color := NewColor;
    sfCheckBoxTextFocused: if TSeStyle(Source).Fonts[ktfCheckBoxTextFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfCheckBoxTextFocused].Color := NewColor;
    sfCheckBoxTextHot: if TSeStyle(Source).Fonts[ktfCheckBoxTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfCheckBoxTextHot].Color := NewColor;
    sfCheckBoxTextNormal: if TSeStyle(Source).Fonts[ktfCheckBoxTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfCheckBoxTextNormal].Color := NewColor;
    sfCheckBoxTextPressed: if TSeStyle(Source).Fonts[ktfCheckBoxTextPressed].Color <> NewColor then TSeStyle(Source).Fonts[ktfCheckBoxTextPressed].Color := NewColor;
    sfComboBoxItemDisabled: if TSeStyle(Source).Fonts[ktfComboBoxItemDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfComboBoxItemDisabled].Color := NewColor;
    sfComboBoxItemFocused: if TSeStyle(Source).Fonts[ktfComboBoxItemFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfComboBoxItemFocused].Color := NewColor;
    sfComboBoxItemHot: if TSeStyle(Source).Fonts[ktfComboBoxItemHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfComboBoxItemHot].Color := NewColor;
    sfComboBoxItemNormal: if TSeStyle(Source).Fonts[ktfComboBoxItemNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfComboBoxItemNormal].Color := NewColor;
    sfComboBoxItemSelected: if TSeStyle(Source).Fonts[ktfComboBoxItemSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfComboBoxItemSelected].Color := NewColor;
    sfEditBoxTextDisabled: if TSeStyle(Source).Fonts[ktfEditBoxTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfEditBoxTextDisabled].Color := NewColor;
    sfEditBoxTextFocused: if TSeStyle(Source).Fonts[ktfEditBoxTextFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfEditBoxTextFocused].Color := NewColor;
    sfEditBoxTextHot: if TSeStyle(Source).Fonts[ktfEditBoxTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfEditBoxTextHot].Color := NewColor;
    sfEditBoxTextNormal: if TSeStyle(Source).Fonts[ktfEditBoxTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfEditBoxTextNormal].Color := NewColor;
    sfEditBoxTextSelected: if TSeStyle(Source).Fonts[ktfEditBoxTextSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfEditBoxTextSelected].Color := NewColor;
    sfGridItemFixedHot: if TSeStyle(Source).Fonts[ktfGridItemFixedHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfGridItemFixedHot].Color := NewColor;
    sfGridItemFixedNormal: if TSeStyle(Source).Fonts[ktfGridItemFixedNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfGridItemFixedNormal].Color := NewColor;
    sfGridItemFixedPressed: if TSeStyle(Source).Fonts[ktfGridItemFixedPressed].Color <> NewColor then TSeStyle(Source).Fonts[ktfGridItemFixedPressed].Color := NewColor;
    sfGridItemNormal: if TSeStyle(Source).Fonts[ktfGridItemNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfGridItemNormal].Color := NewColor;
    sfGridItemSelected: if TSeStyle(Source).Fonts[ktfGridItemSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfGridItemSelected].Color := NewColor;
    sfGroupBoxTextDisabled: if TSeStyle(Source).Fonts[ktfGroupBoxTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfGroupBoxTextDisabled].Color := NewColor;
    sfGroupBoxTextNormal: if TSeStyle(Source).Fonts[ktfGroupBoxTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfGroupBoxTextNormal].Color := NewColor;
    sfHeaderSectionTextDisabled: if TSeStyle(Source).Fonts[ktfHeaderSectionTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfHeaderSectionTextDisabled].Color := NewColor;
    sfHeaderSectionTextHot: if TSeStyle(Source).Fonts[ktfHeaderSectionTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfHeaderSectionTextHot].Color := NewColor;
    sfHeaderSectionTextNormal: if TSeStyle(Source).Fonts[ktfHeaderSectionTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfHeaderSectionTextNormal].Color := NewColor;
    sfHeaderSectionTextPressed: if TSeStyle(Source).Fonts[ktfHeaderSectionTextPressed].Color <> NewColor then TSeStyle(Source).Fonts[ktfHeaderSectionTextPressed].Color := NewColor;
    sfListItemTextDisabled: if TSeStyle(Source).Fonts[ktfListItemTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfListItemTextDisabled].Color := NewColor;
    sfListItemTextFocused: if TSeStyle(Source).Fonts[ktfListItemTextFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfListItemTextFocused].Color := NewColor;
    sfListItemTextHot: if TSeStyle(Source).Fonts[ktfListItemTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfListItemTextHot].Color := NewColor;
    sfListItemTextNormal: if TSeStyle(Source).Fonts[ktfListItemTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfListItemTextNormal].Color := NewColor;
    sfListItemTextSelected: if TSeStyle(Source).Fonts[ktfListItemTextSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfListItemTextSelected].Color := NewColor;
    sfMenuItemTextDisabled: if TSeStyle(Source).Fonts[ktfMenuItemTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfMenuItemTextDisabled].Color := NewColor;
    sfMenuItemTextHot: if TSeStyle(Source).Fonts[ktfMenuItemTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfMenuItemTextHot].Color := NewColor;
    sfMenuItemTextNormal: if TSeStyle(Source).Fonts[ktfMenuItemTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfMenuItemTextNormal].Color := NewColor;
    sfMenuItemTextSelected: if TSeStyle(Source).Fonts[ktfMenuItemTextSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfMenuItemTextSelected].Color := NewColor;
    sfPanelTextDisabled: if TSeStyle(Source).Fonts[ktfPanelTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfPanelTextDisabled].Color := NewColor;
    sfPanelTextNormal: if TSeStyle(Source).Fonts[ktfPanelTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfPanelTextNormal].Color := NewColor;
    sfPopupMenuItemTextDisabled: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfPopupMenuItemTextDisabled].Color := NewColor;
    sfPopupMenuItemTextHot: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfPopupMenuItemTextHot].Color := NewColor;
    sfPopupMenuItemTextNormal: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfPopupMenuItemTextNormal].Color := NewColor;
    sfPopupMenuItemTextSelected: if TSeStyle(Source).Fonts[ktfPopupMenuItemTextSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfPopupMenuItemTextSelected].Color := NewColor;
    sfRadioButtonTextDisabled: if TSeStyle(Source).Fonts[ktfRadioButtonTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfRadioButtonTextDisabled].Color := NewColor;
    sfRadioButtonTextFocused: if TSeStyle(Source).Fonts[ktfRadioButtonTextFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfRadioButtonTextFocused].Color := NewColor;
    sfRadioButtonTextHot: if TSeStyle(Source).Fonts[ktfRadioButtonTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfRadioButtonTextHot].Color := NewColor;
    sfRadioButtonTextNormal: if TSeStyle(Source).Fonts[ktfRadioButtonTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfRadioButtonTextNormal].Color := NewColor;
    sfRadioButtonTextPressed: if TSeStyle(Source).Fonts[ktfRadioButtonTextPressed].Color <> NewColor then TSeStyle(Source).Fonts[ktfRadioButtonTextPressed].Color := NewColor;
    sfSmCaptionTextInactive: if TSeStyle(Source).Fonts[ktfSmCaptionTextInactive].Color <> NewColor then TSeStyle(Source).Fonts[ktfSmCaptionTextInactive].Color := NewColor;
    sfSmCaptionTextNormal: if TSeStyle(Source).Fonts[ktfSmCaptionTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfSmCaptionTextNormal].Color := NewColor;
    sfStatusPanelTextDisabled: if TSeStyle(Source).Fonts[ktfStatusPanelTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfStatusPanelTextDisabled].Color := NewColor;
    sfStatusPanelTextNormal: if TSeStyle(Source).Fonts[ktfStatusPanelTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfStatusPanelTextNormal].Color := NewColor;
    sfTabTextActiveDisabled: if TSeStyle(Source).Fonts[ktfTabTextActiveDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfTabTextActiveDisabled].Color := NewColor;
    sfTabTextActiveHot: if TSeStyle(Source).Fonts[ktfTabTextActiveHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfTabTextActiveHot].Color := NewColor;
    sfTabTextActiveNormal: if TSeStyle(Source).Fonts[ktfTabTextActiveNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfTabTextActiveNormal].Color := NewColor;
    sfTabTextInactiveDisabled: if TSeStyle(Source).Fonts[ktfTabTextInactiveDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfTabTextInactiveDisabled].Color := NewColor;
    sfTabTextInactiveHot: if TSeStyle(Source).Fonts[ktfTabTextInactiveHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfTabTextInactiveHot].Color := NewColor;
    sfTabTextInactiveNormal: if TSeStyle(Source).Fonts[ktfTabTextInactiveNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfTabTextInactiveNormal].Color := NewColor;
    sfTextLabelDisabled: if TSeStyle(Source).Fonts[ktfStaticTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfStaticTextDisabled].Color := NewColor;
    sfTextLabelFocused: if TSeStyle(Source).Fonts[ktfStaticTextFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfStaticTextFocused].Color := NewColor;
    sfTextLabelHot: if TSeStyle(Source).Fonts[ktfStaticTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfStaticTextHot].Color := NewColor;
    sfTextLabelNormal: if TSeStyle(Source).Fonts[ktfStaticTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfStaticTextNormal].Color := NewColor;
    sfToolItemTextDisabled: if TSeStyle(Source).Fonts[ktfToolItemTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfToolItemTextDisabled].Color := NewColor;
    sfToolItemTextHot: if TSeStyle(Source).Fonts[ktfToolItemTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfToolItemTextHot].Color := NewColor;
    sfToolItemTextNormal: if TSeStyle(Source).Fonts[ktfToolItemTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfToolItemTextNormal].Color := NewColor;
    sfToolItemTextSelected: if TSeStyle(Source).Fonts[ktfToolItemTextSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfToolItemTextSelected].Color := NewColor;
    sfTreeItemTextDisabled: if TSeStyle(Source).Fonts[ktfTreeItemTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfTreeItemTextDisabled].Color := NewColor;
    sfTreeItemTextFocused: if TSeStyle(Source).Fonts[ktfTreeItemTextFocused].Color <> NewColor then TSeStyle(Source).Fonts[ktfTreeItemTextFocused].Color := NewColor;
    sfTreeItemTextHot: if TSeStyle(Source).Fonts[ktfTreeItemTextHot].Color <> NewColor then TSeStyle(Source).Fonts[ktfTreeItemTextHot].Color := NewColor;
    sfTreeItemTextNormal: if TSeStyle(Source).Fonts[ktfTreeItemTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfTreeItemTextNormal].Color := NewColor;
    sfTreeItemTextSelected: if TSeStyle(Source).Fonts[ktfTreeItemTextSelected].Color <> NewColor then TSeStyle(Source).Fonts[ktfTreeItemTextSelected].Color := NewColor;
    sfWindowTextDisabled: if TSeStyle(Source).Fonts[ktfWindowTextDisabled].Color <> NewColor then TSeStyle(Source).Fonts[ktfWindowTextDisabled].Color := NewColor;
    sfWindowTextNormal: if TSeStyle(Source).Fonts[ktfWindowTextNormal].Color <> NewColor then TSeStyle(Source).Fonts[ktfWindowTextNormal].Color := NewColor;
  end;
end;

procedure TCustomStyleExt.SetSystemColor(Color, NewColor: TColor);
begin
  if TseStyle(Source).SysColors[Color]<>NewColor then
    TseStyle(Source).SysColors[Color]:=NewColor;
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
//function TCustomStyleHelper.GetSource: TObject;
//begin
//  {$IFDEF USE_RTTI}
//  Result:=TRttiContext.Create.GetType(Self.ClassType).GetField('FSource').GetValue(Self).AsObject;
//  {$ELSE}
//  Result:=Self.FSource;
//  {$ENDIF}
//end;
//
{$ENDIF}

procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string;hIcon:HICON=0);
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
      Result  := Rect(0, 0, 0, 0);
      Detail  := twCaptionActive;
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

  //Draw background
  LDetails.Element := teWindow;
  LDetails.Part := 0;
  Style.DrawElement(Canvas.Handle, LDetails, ARect);

  //Draw caption border
  CaptionRect := Rect(0, 0, CaptionBitmap.Width, CaptionBitmap.Height);
  LDetails := Style.GetElementDetails(twCaptionActive);
  Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect);
  TextRect := CaptionRect;
  CaptionDetails := LDetails;

  //Draw icon
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
   if (hIcon <> 0) then
     DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, hIcon, 0, 0, 0, 0, DI_NORMAL);

  Inc(TextRect.Left, ButtonRect.Width + 5);

  //Draw buttons

  //Close button
  LDetails := Style.GetElementDetails(twCloseButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  //Maximize button
  LDetails := Style.GetElementDetails(twMaxButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  //Minimize button
  LDetails := Style.GetElementDetails(twMinButtonNormal);

  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  //Help button
  LDetails := Style.GetElementDetails(twHelpButtonNormal);
  if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

  if (ButtonRect.Left > 0) then
    TextRect.Right := ButtonRect.Left;

  //Draw text
  Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, ACaption, TextRect, [tfLeft, tfSingleLine, tfVerticalCenter]);

  //Draw caption
  Canvas.Draw(0, 0, CaptionBitmap);


  CaptionBitmap.Free;

  //Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  //Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  //Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);


  //Draw Ok button
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left := 30;
  ButtonRect.Top := ARect.Height -45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(Canvas.Handle, LDetails, 'OK', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Cancel button
  ButtonRect.Left := 110;
  ButtonRect.Top := ARect.Height -45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(Canvas.Handle, LDetails, 'Cancel', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);
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
  IconRect, BorderRect, CaptionRect, ButtonRect , TextRect: TRect;
  CaptionBitmap : TBitmap;
  ThemeTextColor: TColor;
  ARect, LRect: TRect;
  LRegion: HRgn;
  i: Integer;

    function GetBorderSize: TRect;
    var
      Size: TSize;
      Details: TThemedElementDetails;
      Detail: TThemedWindow;
    begin
      Result  := Rect(0, 0, 0, 0);
      Detail  := twCaptionActive;
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
  if FStyle = nil then Exit;

  BorderRect := GetBorderSize;
  ARect := ClientRect;
  CaptionBitmap := TBitmap.Create;
  try
    CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);
        {
    LBitmap:=TBitmap.Create;
    LBitmap.PixelFormat:=pf32bit;
    }
    FBitmap.Width := ClientRect.Width;
    FBitmap.Height := ClientRect.Height;

    //Draw background
    LDetails.Element := teWindow;
    LDetails.Part := 0;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ARect);

    //Draw caption border
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

    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect);
    TextRect := CaptionRect;
    CaptionDetails := LDetails;

    //Draw icon
    IconDetails := Style.GetElementDetails(twSysButtonNormal);
    if not Style.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    IconRect := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(IconRect, ButtonRect);

    if (ButtonRect.Width > 0) and (FIcon <> 0) then
      DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, FIcon, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);

    //Draw buttons

    //Close button
    LDetails := Style.GetElementDetails(twCloseButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Maximize button
    LDetails := Style.GetElementDetails(twMaxButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Minimize button
    LDetails := Style.GetElementDetails(twMinButtonNormal);

    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Help button
    LDetails := Style.GetElementDetails(twHelpButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;

    //Draw text
    Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, FCaption, TextRect, [tfLeft, tfSingleLine, tfVerticalCenter]);

    //Draw caption
    FBitmap.Canvas.Draw(0, 0, CaptionBitmap);
  finally
    CaptionBitmap.Free;
  end;

  //Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw Main Menu
  LDetails:= Style.GetElementDetails(tmMenuBarBackgroundActive);
  LRect := Rect(BorderRect.Left, BorderRect.Top+1, ARect.Width-BorderRect.Left,BorderRect.Top+1+20);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect);

  LDetails := Style.GetElementDetails(tmMenuBarItemNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);

//    function DrawText(DC: HDC; Details: TThemedElementDetails;
//      const S: string; var R: TRect; Flags: TTextFormat; Color: TColor = clNone): Boolean; overload;
//    function DrawText(DC: HDC; Details: TThemedElementDetails;
//      const S: string; var R: TRect; Flags: TTextFormat; Options: TStyleTextOptions): Boolean; overload;

  CaptionRect := Rect(LRect.Left+10,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'File', CaptionRect, [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left+40,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Edit', CaptionRect,  [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left+70,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'View', CaptionRect,  [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left+110,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Help', CaptionRect,  [tfLeft], ThemeTextColor);

  //Draw ToolButtons
  for i := 1 to 3 do
  begin
    LDetails := Style.GetElementDetails(ttbButtonNormal);
    ButtonRect.Left := BorderRect.Left + 5 +((i - 1) * 76);
    ButtonRect.Top := LRect.Top + 30;
    ButtonRect.Width := 75;
    ButtonRect.Height := 25;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

    Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
    Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'ToolButton'+IntToStr(i), ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);
  end;

  //Draw Normal
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left := BorderRect.Left + 5;
  ButtonRect.Top := ARect.Height - 45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Normal', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Hot
  LDetails := Style.GetElementDetails(tbPushButtonHot);
  ButtonRect.Left := BorderRect.Left + 85;
  ButtonRect.Top := ARect.Height - 45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Hot', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Pressed
  LDetails := Style.GetElementDetails(tbPushButtonPressed);
  ButtonRect.Left := BorderRect.Left + 165;
  ButtonRect.Top := ARect.Height - 45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Pressed', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Disabled
  LDetails := Style.GetElementDetails(tbPushButtonDisabled);
  ButtonRect.Left := BorderRect.Left + 245;
  ButtonRect.Top := ARect.Height - 45;
  ButtonRect.Width := 75;
  ButtonRect.Height := 25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Disabled', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

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
