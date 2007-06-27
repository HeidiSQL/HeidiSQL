{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditMiscClasses.pas, released 2000-04-07.
The Original Code is based on the mwSupportClasses.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditMiscClasses.pas,v 1.11 2002/06/05 10:23:01 plpolak Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditMiscClasses;

{$I SynEdit.inc}

interface

uses
  SynEditKeyConst,
{$IFDEF SYN_CLX}
  Types,
  QGraphics,
  QControls,
  QImgList,
  QStdCtrls,
  QMenus,
  kTextDrawer,
{$ELSE}
  Windows,
  Messages,
  Graphics,
  Controls,
  stdctrls,
  Menus,
{$ENDIF}
  Classes,
  SysUtils;

type
  TSynSelectedColor = class(TPersistent)
  private
    fBG: TColor;
    fFG: TColor;
    fOnChange: TNotifyEvent;
    procedure SetBG(Value: TColor);
    procedure SetFG(Value: TColor);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;                            //jcr 2000-12-08
  published
    property Background: TColor read fBG write SetBG default clHighLight;
    property Foreground: TColor read fFG write SetFG default clHighLightText;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TSynGutter = class(TPersistent)
  private
    fFont: TFont;                                                               //DDH 10/16/01
    fColor: TColor;
    fWidth: integer;
    fShowLineNumbers: boolean;
    fDigitCount: integer;
    fLeadingZeros: boolean;
    fZeroStart: boolean;
    fLeftOffset: integer;
    fRightOffset: integer;
    fOnChange: TNotifyEvent;
    fCursor: TCursor;
    fVisible: boolean;
    fUseFontStyle: boolean;
    fAutoSize: boolean;
    fAutoSizeDigitCount: integer;
    procedure SetAutoSize(const Value: boolean);
    procedure SetColor(const Value: TColor);
    procedure SetDigitCount(Value: integer);
    procedure SetLeadingZeros(const Value: boolean);
    procedure SetLeftOffset(Value: integer);
    procedure SetRightOffset(Value: integer);
    procedure SetShowLineNumbers(const Value: boolean);
    procedure SetUseFontStyle(Value: boolean);
    procedure SetVisible(Value: boolean);
    procedure SetWidth(Value: integer);
    procedure SetZeroStart(const Value: boolean);
    procedure SetFont(Value: TFont);                                            //DDH 10/16/01
    procedure OnFontChange(Sender: TObject);                                    //DDH 10/16/01
  public
    constructor Create;
    destructor Destroy; override;                                               //DDH 10/16/01
    procedure Assign(Source: TPersistent); override;
    procedure AutoSizeDigitCount(LinesCount: integer);
    function FormatLineNumber(Line: integer): string;
    function RealGutterWidth(CharWidth: integer): integer;
  published
    property AutoSize: boolean read fAutoSize write SetAutoSize default FALSE;
    property Color: TColor read fColor write SetColor default clBtnFace;
    property Cursor: TCursor read fCursor write fCursor default crDefault;
    property DigitCount: integer read fDigitCount write SetDigitCount
      default 4;
    property Font: TFont read fFont write SetFont;                              //DDH 10/16/01
    property LeadingZeros: boolean read fLeadingZeros write SetLeadingZeros
      default FALSE;
    property LeftOffset: integer read fLeftOffset write SetLeftOffset
      default 16;
    property RightOffset: integer read fRightOffset write SetRightOffset
      default 2;
    property ShowLineNumbers: boolean read fShowLineNumbers
      write SetShowLineNumbers default FALSE;
    property UseFontStyle: boolean read fUseFontStyle write SetUseFontStyle
      default TRUE;                                                             //DDH 10/16/01
    property Visible: boolean read fVisible write SetVisible default TRUE;
    property Width: integer read fWidth write SetWidth default 30;
    property ZeroStart: boolean read fZeroStart write SetZeroStart
      default FALSE;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  TSynBookMarkOpt = class(TPersistent)
  private
    fBookmarkImages: TImageList;
    fDrawBookmarksFirst: boolean;
    fEnableKeys: Boolean;
    fGlyphsVisible: Boolean;
    fLeftMargin: Integer;
    fOwner: TComponent;
    fXoffset: integer;
    fOnChange: TNotifyEvent;
    procedure SetBookmarkImages(const Value: TImageList);
    procedure SetDrawBookmarksFirst(Value: boolean);
    procedure SetGlyphsVisible(Value: Boolean);
    procedure SetLeftMargin(Value: Integer);
    procedure SetXOffset(Value: integer);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TPersistent); override;                            //jcr 2000-12-08
  published
    property BookmarkImages: TImageList
      read fBookmarkImages write SetBookmarkImages;
    property DrawBookmarksFirst: boolean read fDrawBookmarksFirst
      write SetDrawBookmarksFirst default True;
    property EnableKeys: Boolean
      read fEnableKeys write fEnableKeys default True;
    property GlyphsVisible: Boolean
      read fGlyphsVisible write SetGlyphsVisible default True;
    property LeftMargin: Integer read fLeftMargin write SetLeftMargin default 2;
    property Xoffset: integer read fXoffset write SetXOffset default 12;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;

  { TSynMethodChain }

  ESynMethodChain = class(Exception);
  TSynExceptionEvent = procedure (Sender: TObject; E: Exception;
    var DoContinue: Boolean) of object;

  TSynMethodChain = class
  private
    FNotifyProcs: TList;
    FExceptionHandler: TSynExceptionEvent;
  protected
    procedure DoFire(AEvent: TMethod); virtual; abstract;
    function DoHandleException(E: Exception): Boolean; virtual;
    property ExceptionHandler: TSynExceptionEvent read FExceptionHandler
      write FExceptionHandler;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(AEvent: TMethod);
    procedure Remove(AEvent: TMethod);
    procedure Fire;
  end;

  { TSynNotifyEventChain }

  TSynNotifyEventChain = class(TSynMethodChain)
  private
    FSender: TObject;
  protected
    procedure DoFire(AEvent: TMethod); override;
  public
    constructor CreateEx(ASender: TObject);
    procedure Add(AEvent: TNotifyEvent);
    procedure Remove(AEvent: TNotifyEvent);
    property ExceptionHandler;
    property Sender: TObject read FSender write FSender;
  end;

  { TSynInternalImage }
  
  TSynInternalImage = class(TObject)
  private
    fImages : TBitmap;
    fWidth  : Integer;
    fHeight : Integer;
    fCount  : Integer;

    function CreateBitmapFromInternalList (const Name: string): TBitmap;
    procedure FreeBitmapFromInternalList;
  public
    constructor Create(const Name: string; Count: integer);
    destructor Destroy; override;
    procedure DrawMark(ACanvas: TCanvas; Number, X, Y, LineHeight: integer);
    procedure DrawMarkTransparent(ACanvas: TCanvas; Number, X, Y,
      LineHeight: integer; TransparentColor: TColor);
  end;

  TSynHotKey=class(TEdit)
  private
    function GetHotKey: TShortcut;
    procedure SetHotKey(const Value: TShortcut);
  protected
  {$IFDEF SYN_CLX}
  {$ELSE}
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  {$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoExit; override;
  published
    property HotKey: TShortcut read GetHotKey write SetHotKey;
  end;

implementation

uses
  SynEditMiscProcs;

{ TSynSelectedColor }

constructor TSynSelectedColor.Create;
begin
  inherited Create;
  fBG := clHighLight;
  fFG := clHighLightText;
end;

procedure TSynSelectedColor.Assign(Source: TPersistent);                        //jcr 2000-12-08
var
  Src: TSynSelectedColor;
begin
  if (Source <> nil) and (Source is TSynSelectedColor) then begin
    Src := TSynSelectedColor(Source);
    fBG := Src.fBG;
    fFG := Src.fFG;
    if Assigned(fOnChange) then fOnChange(Self);
  end else
    inherited Assign(Source);
end;

procedure TSynSelectedColor.SetBG(Value: TColor);
begin
  if (fBG <> Value) then begin
    fBG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynSelectedColor.SetFG(Value: TColor);
begin
  if (fFG <> Value) then begin
    fFG := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TSynGutter }

constructor TSynGutter.Create;
begin
  inherited Create;
  fFont := TFont.Create;                                                        //DDH 10/16/01 Start
  fFont.Name := 'Terminal';
  fFont.Size := 8;
  fFont.Style := [];
  fUseFontStyle := True;
  fFont.OnChange := OnFontChange;                                               //DDH 10/16/01 End

  fColor := clBtnFace;
  fVisible := TRUE;
  fWidth := 30;
  fLeftOffset := 16;
  fDigitCount := 4;
  fAutoSizeDigitCount := fDigitCount;
  fRightOffset := 2;
end;

destructor TSynGutter.Destroy;                                                   //DDH 10/16/01
begin
  fFont.Free;
  inherited Destroy;
end;

procedure TSynGutter.Assign(Source: TPersistent);
var
  Src: TSynGutter;
begin
  if Assigned(Source) and (Source is TSynGutter) then begin
    Src := TSynGutter(Source);
    fFont.Assign(src.Font);                                                     //DDH 10/16/01
    fUseFontStyle := src.fUseFontStyle;                                         //DDH 10/16/01
    fColor := Src.fColor;
    fVisible := Src.fVisible;
    fWidth := Src.fWidth;
    fShowLineNumbers := Src.fShowLineNumbers;
    fLeadingZeros := Src.fLeadingZeros;
    fZeroStart := Src.fZeroStart;
    fLeftOffset := Src.fLeftOffset;
    fDigitCount := Src.fDigitCount;
    fRightOffset := Src.fRightOffset;
    fAutoSize := Src.fAutoSize;
    fAutoSizeDigitCount := Src.fAutoSizeDigitCount;
    if Assigned(fOnChange) then fOnChange(Self);
  end else
    inherited;
end;

procedure TSynGutter.AutoSizeDigitCount(LinesCount: integer);
var
  nDigits: integer;
begin
  if fVisible and fAutoSize and fShowLineNumbers then begin            
    if fZeroStart then Dec(LinesCount);
    nDigits := Max(Length(IntToStr(LinesCount)), fDigitCount);
    if fAutoSizeDigitCount <> nDigits then begin
      fAutoSizeDigitCount := nDigits;
      if Assigned(fOnChange) then fOnChange(Self);
    end;
  end else
    fAutoSizeDigitCount := fDigitCount;
end;

function TSynGutter.FormatLineNumber(Line: integer): string;
var
  i: integer;
begin
  if fZeroStart then Dec(Line);
  Str(Line : fAutoSizeDigitCount, Result);
  if fLeadingZeros then
    for i := 1 to fAutoSizeDigitCount - 1 do begin
      if (Result[i] <> ' ') then break;
      Result[i] := '0';
    end;
end;

function TSynGutter.RealGutterWidth(CharWidth: integer): integer;
begin
  if not fVisible then
    Result := 0
  else if fShowLineNumbers then
    Result := fLeftOffset + fRightOffset + fAutoSizeDigitCount * CharWidth + 2
  else
    Result := fWidth;
end;

procedure TSynGutter.SetAutoSize(const Value: boolean);
begin
  if fAutoSize <> Value then begin
    fAutoSize := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetColor(const Value: TColor);
begin
  if fColor <> Value then begin
    fColor := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

//DDH 10/16/01 Start
procedure TSynGutter.SetFont(Value: TFont);
begin
  fFont.Assign(Value);
end;

procedure TSynGutter.OnFontChange(Sender: TObject);
begin
  if Assigned(fOnChange) then fOnChange(Self);
end;
//DDH 10/16/01 End

procedure TSynGutter.SetDigitCount(Value: integer);
begin
  Value := MinMax(Value, 2, 12);
  if fDigitCount <> Value then begin
    fDigitCount := Value;
    fAutoSizeDigitCount := fDigitCount;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetLeadingZeros(const Value: boolean);
begin
  if fLeadingZeros <> Value then begin
    fLeadingZeros := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetLeftOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fLeftOffset <> Value then begin
    fLeftOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetRightOffset(Value: integer);
begin
  Value := Max(0, Value);
  if fRightOffset <> Value then begin
    fRightOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetShowLineNumbers(const Value: boolean);
begin
  if fShowLineNumbers <> Value then begin
    fShowLineNumbers := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetUseFontStyle(Value: boolean);
begin
  if fUseFontStyle <> Value then begin
    fUseFontStyle := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetVisible(Value: boolean);
begin
  if fVisible <> Value then begin
    fVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetWidth(Value: integer);
begin
  Value := Max(0, Value);
  if fWidth <> Value then begin
    fWidth := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynGutter.SetZeroStart(const Value: boolean);
begin
  if fZeroStart <> Value then begin
    fZeroStart := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TSynBookMarkOpt }

constructor TSynBookMarkOpt.Create(AOwner: TComponent);
begin
  inherited Create;
  fDrawBookmarksFirst := TRUE;                                          
  fEnableKeys := True;
  fGlyphsVisible := True;
  fLeftMargin := 2;
  fOwner := AOwner;
  fXOffset := 12;
end;

procedure TSynBookMarkOpt.Assign(Source: TPersistent);                          //jcr 2000-12-08
var
  Src: TSynBookMarkOpt;
begin
  if (Source <> nil) and (Source is TSynBookMarkOpt) then begin
    Src := TSynBookMarkOpt(Source);
    fBookmarkImages := Src.fBookmarkImages;
    fDrawBookmarksFirst := Src.fDrawBookmarksFirst;
    fEnableKeys := Src.fEnableKeys;
    fGlyphsVisible := Src.fGlyphsVisible;
    fLeftMargin := Src.fLeftMargin;
    fXoffset := Src.fXoffset;
    if Assigned(fOnChange) then fOnChange(Self);
  end else
    inherited Assign(Source);
end;

procedure TSynBookMarkOpt.SetBookmarkImages(const Value: TImageList);
begin
  if fBookmarkImages <> Value then begin
    fBookmarkImages := Value;
    if Assigned(fBookmarkImages) then fBookmarkImages.FreeNotification(fOwner);
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetDrawBookmarksFirst(Value: boolean);
begin
  if Value <> fDrawBookmarksFirst then begin
    fDrawBookmarksFirst := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetGlyphsVisible(Value: Boolean);
begin
  if fGlyphsVisible <> Value then begin
    fGlyphsVisible := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetLeftMargin(Value: Integer);
begin
  if fLeftMargin <> Value then begin
    fLeftMargin := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TSynBookMarkOpt.SetXOffset(Value: integer);
begin
  if fXOffset <> Value then begin
    fXOffset := Value;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

{ TSynMethodChain }

procedure TSynMethodChain.Add(AEvent: TMethod);
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt(
      '%s.Entry : the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    Add(Code);
    Add(Data);
  end
end;

constructor TSynMethodChain.Create;
begin
  inherited;
  FNotifyProcs := TList.Create;
end;

destructor TSynMethodChain.Destroy;
begin
  FNotifyProcs.Free;
  inherited;
end;

function TSynMethodChain.DoHandleException(E: Exception): Boolean;
begin
  if not Assigned(FExceptionHandler) then
    raise E
  else
    try
      Result := True;
      FExceptionHandler(Self, E, Result);
    except
      raise ESynMethodChain.CreateFmt(
        '%s.DoHandleException : MUST NOT occur any kind of exception in '+
        'ExceptionHandler', [ClassName]);
    end;
end;

procedure TSynMethodChain.Fire;
var
  AMethod: TMethod;
  i: Integer;
begin
  i := 0;
  with FNotifyProcs, AMethod do
    while i < Count do
      try
        repeat
          Code := Items[i];
          Inc(i);
          Data := Items[i];
          Inc(i);

          DoFire(AMethod)
        until i >= Count;
      except
        on E: Exception do
          if not DoHandleException(E) then
            i := MaxInt;
      end;
end;

procedure TSynMethodChain.Remove(AEvent: TMethod);
var
  i: Integer;
begin
  if not Assigned(@AEvent) then
    raise ESynMethodChain.CreateFmt(
      '%s.Remove: the parameter `AEvent'' must be specified.', [ClassName]);

  with FNotifyProcs, AEvent do
  begin
    i := Count - 1;
    while i > 0 do
      if Items[i] <> Data then
        Dec(i, 2)
      else
      begin
        Dec(i);
        if Items[i] = Code then
        begin
          Delete(i);
          Delete(i);
        end;
        Dec(i);
      end;
  end;
end;

{ TSynNotifyEventChain }

procedure TSynNotifyEventChain.Add(AEvent: TNotifyEvent);
begin
  inherited Add(TMethod(AEvent));
end;

constructor TSynNotifyEventChain.CreateEx(ASender: TObject);
begin
  inherited Create;
  FSender := ASender;
end;

procedure TSynNotifyEventChain.DoFire(AEvent: TMethod);
begin
  TNotifyEvent(AEvent)(FSender);
end;

procedure TSynNotifyEventChain.Remove(AEvent: TNotifyEvent);
begin
  inherited Remove(TMethod(AEvent));
end;


{ TSynInternalImage }

type
  TInternalResource = class (TObject)
    public
      UsageCount : Integer;
      Name       : string;
      Bitmap     : TBitmap;
  end;

var
  InternalResources: TList;

constructor TSynInternalImage.Create(const Name: string; Count: integer);
begin
  inherited Create;
  fImages := CreateBitmapFromInternalList (Name);
  fWidth := (fImages.Width + Count shr 1) div Count;
  fHeight := fImages.Height;
  fCount := Count;
  end;

destructor TSynInternalImage.Destroy;
begin
  FreeBitmapFromInternalList;
  inherited Destroy;
end;

function TSynInternalImage.CreateBitmapFromInternalList(const Name: string): TBitmap;
var
  idx: Integer;
  newIntRes: TInternalResource;
begin
  { There is no list until now }
  if (InternalResources = nil) then
    InternalResources := TList.Create;

  { Search the list for the needed resource }
  for idx := 0 to InternalResources.Count - 1 do
    if (TInternalResource (InternalResources[idx]).Name = UpperCase (Name)) then
      with TInternalResource (InternalResources[idx]) do begin
        UsageCount := UsageCount + 1;
        Result := Bitmap;
        exit;
      end;

  { There is no loaded resource in the list so let's create a new one }
  Result := TBitmap.Create;
  Result.LoadFromResourceName (HInstance,Name);

  { Add the new resource to our list }
  newIntRes:= TInternalResource.Create;
  newIntRes.UsageCount := 1;
  newIntRes.Name := UpperCase (Name);
  newIntRes.Bitmap := Result;
  InternalResources.Add (newIntRes);
end;

procedure TSynInternalImage.FreeBitmapFromInternalList;
var
  idx: Integer;
  intRes: TInternalResource;
  function FindImageInList: Integer;
  begin
    for Result := 0 to InternalResources.Count - 1 do
      if (TInternalResource (InternalResources[Result]).Bitmap = fImages) then
        exit;
    Result := -1;
  end;
begin
  { Search the index of our resource in the list }
  idx := FindImageInList;

  { Ey, what's this ???? }
  if (idx = -1) then
    exit;

  { Decrement the usagecount in the object. If there are no more users
    remove the object from the list and free it }
  intRes := TInternalResource (InternalResources[idx]);
  with intRes do begin
    UsageCount := UsageCount - 1;
    if (UsageCount = 0) then begin
      Bitmap.Free;
      InternalResources.Delete (idx);
      intRes.Free;
    end;
  end;

  { If there are no more entries in the list free it }
  if (InternalResources.Count = 0) then begin
    InternalResources.Free;
    InternalResources := nil;
  end;
end;

procedure TSynInternalImage.DrawMark(ACanvas: TCanvas;
  Number, X, Y, LineHeight: integer);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < fCount) then
  begin
    if LineHeight >= fHeight then begin
      rcSrc := Rect(Number * fWidth, 0, (Number + 1) * fWidth, fHeight);
      Inc(Y, (LineHeight - fHeight) div 2);
      rcDest := Rect(X, Y, X + fWidth, Y + fHeight);
    end else begin
      rcDest := Rect(X, Y, X + fWidth, Y + LineHeight);
      Y := (fHeight - LineHeight) div 2;
      rcSrc := Rect(Number * fWidth, Y, (Number + 1) * fWidth,
        Y + LineHeight);
    end;
    ACanvas.CopyRect(rcDest, fImages.Canvas, rcSrc);
  end;
end;

procedure TSynInternalImage.DrawMarkTransparent(ACanvas: TCanvas; Number, X, Y,
  LineHeight: integer; TransparentColor: TColor);
var
  rcSrc, rcDest: TRect;
begin
  if (Number >= 0) and (Number < fCount) then
  begin
    if LineHeight >= fHeight then begin
      rcSrc := Rect(Number * fWidth, 0, (Number + 1) * fWidth, fHeight);
      Inc(Y, (LineHeight - fHeight) div 2);
      rcDest := Rect(X, Y, X + fWidth, Y + fHeight);
    end else begin
      rcDest := Rect(X, Y, X + fWidth, Y + LineHeight);
      Y := (fHeight - LineHeight) div 2;
      rcSrc := Rect(Number * fWidth, Y, (Number + 1) * fWidth,
        Y + LineHeight);
    end;
{$IFDEF SYN_CLX}
    ACanvas.CopyMode := cmMergeCopy;
    ACanvas.CopyRect(rcDest, fImages.Canvas, rcSrc);
{$ELSE}
    ACanvas.BrushCopy(rcDest, fImages, rcSrc, TransparentColor);
{$ENDIF}
  end;
end;

{ TSynHotKey }

function RemoveStates(St: string): string;
begin
  if Copy(St, 1, 5) = 'Ctrl+' then
    Delete(St, 1, 5);

  if Copy(St, 1, 4) = 'Alt+' then
    Delete(St, 1, 4);

  if Copy(St, 1, 6) = 'Shift+' then
    Delete(St, 1, 6);

  Result := St;
end;

procedure TSynHotKey.DoExit;
begin
  inherited;
  if (length(Text) > 0) and (RemoveStates(Text) = '') then
  begin
    Text := 'None';
    SelStart := length(Text);
  end;
end;

function TSynHotKey.GetHotKey: TShortcut;
begin
{$IFDEF SYN_CLX}
  Result := QMenus.TextToShortCut(Text);
{$ELSE}
  Result := Menus.TextToShortCut(Text);
{$ENDIF}
end;

procedure TSynHotKey.KeyDown(var Key: Word; Shift: TShiftState);
VAR TmpString : String;
begin
//  inherited;
  TmpString := '';
  if ssCtrl in Shift then
    TmpString := TmpString + 'Ctrl+';
  if ssAlt in Shift then
    TmpString := TmpString + 'Alt+';
  if ssShift in Shift then
    TmpString := TmpString + 'Shift+';

  if (key = SYNEDIT_CONTROL) or (key = SYNEDIT_MENU) or (key = SYNEDIT_SHIFT) then
  begin
    //Nothing, the Shift state takes care of it
  end else begin
    {$IFDEF SYN_CLX}
      TmpString := TmpString + QMenus.ShortCutToText(Key);
    {$ELSE}
      TmpString := TmpString + Menus.ShortCutToText(Key);
    {$ENDIF}
  end;

  if Text <> TmpString then
    Text := TmpString;

  SelStart := length(Text);
end;

procedure TSynHotKey.KeyPress(var Key: Char);
begin
//  inherited;
  if (length(Text) > 0) and (RemoveStates(Text) <> '') then
    Key := #0;
end;

procedure TSynHotKey.KeyUp(var Key: Word; Shift: TShiftState);
begin
//  inherited;
  if (length(Text) > 0) and (RemoveStates(Text) = '') then
  begin
    Text := 'None';
    SelStart := length(Text);
  end;
end;

procedure TSynHotKey.SetHotKey(const Value: TShortcut);
begin
  if Value = 0 then
    Text := 'None'
  else
{$IFDEF SYN_CLX}
    Text := QMenus.ShortCutToText(Value);
{$ELSE}
    Text := Menus.ShortCutToText(Value);
{$ENDIF}
  SelStart := length(Text);
end;

{$IFDEF SYN_CLX}
{$ELSE}
procedure TSynHotKey.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
//  Message.Result := Message.Result or DLGC_WANTTAB  //This is causing an invalid pointer op right now.
end;
{$ENDIF}

begin
  InternalResources := nil;
end.

