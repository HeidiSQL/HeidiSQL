
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntMenus;

{$INCLUDE TntCompilers.inc}

interface

uses
  Windows, Classes, Menus, Graphics, Messages;

type
{TNT-WARN TMenuItem}
  TTntMenuItem = class(TMenuItem{TNT-ALLOW TMenuItem})
  private
    FIgnoreMenuChanged: Boolean;
    FCaption: WideString;
    FHint: WideString;
    FKeyboardLayout: HKL;
    function GetCaption: WideString;
    procedure SetInheritedCaption(const Value: AnsiString);
    procedure SetCaption(const Value: WideString);
    function IsCaptionStored: Boolean;
    procedure UpdateMenuString(ParentMenu: TMenu);
    function GetAlignmentDrawStyle: Word;
    function MeasureItemTextWidth(ACanvas: TCanvas; const Text: WideString): Integer;
    function GetHint: WideString;
    procedure SetInheritedHint(const Value: AnsiString);
    procedure SetHint(const Value: WideString);
    function IsHintStored: Boolean;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TMenuActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure MenuChanged(Rebuild: Boolean); override;
    procedure AdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
      State: TOwnerDrawState; TopLevel: Boolean); override;
    procedure DoDrawText(ACanvas: TCanvas; const ACaption: WideString;
      var Rect: TRect; Selected: Boolean; Flags: Integer);
    procedure MeasureItem(ACanvas: TCanvas; var Width, Height: Integer); override; 
  public
    procedure InitiateAction; override;
    procedure Loaded; override;
    function Find(ACaption: WideString): TMenuItem{TNT-ALLOW TMenuItem};
  published
    property Caption: WideString read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  end;

{TNT-WARN TMainMenu}
  TTntMainMenu = class(TMainMenu{TNT-ALLOW TMainMenu})
  protected
    procedure DoChange(Source: TMenuItem{TNT-ALLOW TMenuItem}; Rebuild: Boolean); override;
  public
    {$IFDEF COMPILER_9_UP}
    function CreateMenuItem: TMenuItem{TNT-ALLOW TMenuItem}; override;
    {$ENDIF}
  end;

{TNT-WARN TPopupMenu}
  TTntPopupMenu = class(TPopupMenu{TNT-ALLOW TPopupMenu})
  protected
    procedure DoChange(Source: TMenuItem{TNT-ALLOW TMenuItem}; Rebuild: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF COMPILER_9_UP}
    function CreateMenuItem: TMenuItem{TNT-ALLOW TMenuItem}; override;
    {$ENDIF}
    destructor Destroy; override;
    procedure Popup(X, Y: Integer); override;
  end;

{TNT-WARN NewSubMenu}
function WideNewSubMenu(const ACaption: WideString; hCtx: THelpContext;
  const AName: TComponentName; const Items: array of TTntMenuItem;
    AEnabled: Boolean): TTntMenuItem;
{TNT-WARN NewItem}
function WideNewItem(const ACaption: WideString; AShortCut: TShortCut;
  AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent; hCtx: THelpContext;
    const AName: TComponentName): TTntMenuItem;

function MessageToShortCut(Msg: TWMKeyDown): TShortCut;

{TNT-WARN ShortCutToText}
function WideShortCutToText(WordShortCut: Word): WideString;
{TNT-WARN TextToShortCut}
function WideTextToShortCut(Text: WideString): TShortCut;
{TNT-WARN GetHotKey}
function WideGetHotkey(const Text: WideString): WideString;
{TNT-WARN StripHotkey}
function WideStripHotkey(const Text: WideString): WideString;
{TNT-WARN AnsiSameCaption}
function WideSameCaption(const Text1, Text2: WideString): Boolean;

function WideGetMenuItemCaption(MenuItem: TMenuItem{TNT-ALLOW TMenuItem}): WideString;
function WideGetMenuItemHint(MenuItem: TMenuItem{TNT-ALLOW TMenuItem}): WideString;

procedure NoOwnerDrawTopLevelItems(Menu: TMainMenu{TNT-ALLOW TMainMenu});

procedure FixMenuBiDiProblem(Menu: TMenu);

function MenuItemHasBitmap(MenuItem: TMenuItem{TNT-ALLOW TMenuItem}): Boolean;

type
  TTntPopupList = class(TPopupList)
  private
    SavedPopupList: TPopupList;
  protected
    procedure WndProc(var Message: TMessage); override;
  end;

var
  TntPopupList: TTntPopupList;

implementation

uses
  Forms, SysUtils, Consts, ActnList, ImgList, TntControls, TntGraphics,
  TntActnList, TntClasses, TntForms, TntSysUtils, TntWindows;

function WideNewSubMenu(const ACaption: WideString; hCtx: THelpContext;
  const AName: TComponentName; const Items: array of TTntMenuItem;
    AEnabled: Boolean): TTntMenuItem;
var
  I: Integer;
begin
  Result := TTntMenuItem.Create(nil);
  for I := Low(Items) to High(Items) do
    Result.Add(Items[I]);
  Result.Caption := ACaption;
  Result.HelpContext := hCtx;
  Result.Name := AName;
  Result.Enabled := AEnabled;
end;

function WideNewItem(const ACaption: WideString; AShortCut: TShortCut;
  AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent; hCtx: THelpContext;
    const AName: TComponentName): TTntMenuItem;
begin
  Result := TTntMenuItem.Create(nil);
  with Result do
  begin
    Caption := ACaption;
    ShortCut := AShortCut;
    OnClick := AOnClick;
    HelpContext := hCtx;
    Checked := AChecked;
    Enabled := AEnabled;
    Name := AName;
  end;
end;

function MessageToShortCut(Msg: TWMKeyDown): TShortCut;
var
  ShiftState: TShiftState;
begin
  ShiftState := Forms.KeyDataToShiftState(TWMKeyDown(Msg).KeyData);
  Result := Menus.ShortCut(TWMKeyDown(Msg).CharCode, ShiftState);
end;

function WideGetSpecialName(WordShortCut: Word): WideString;
var
  ScanCode: Integer;
  KeyName: array[0..255] of WideChar;
begin
  Assert(Win32PlatformIsUnicode);
  Result := '';
  ScanCode := MapVirtualKeyW(WordRec(WordShortCut).Lo, 0) shl 16;
  if ScanCode <> 0 then
  begin
    GetKeyNameTextW(ScanCode, KeyName, SizeOf(KeyName));
    Result := KeyName;
  end;
end;

function WideGetKeyboardChar(Key: Word): WideChar;
var
  LatinNumChar: WideChar;
begin
  Assert(Win32PlatformIsUnicode);
  Result := WideChar(MapVirtualKeyW(Key, 2));
  if (Key in [$30..$39]) then
  begin
    // Check to see if "0" - "9" can be used if all that differs is shift state
    LatinNumChar := WideChar(Key - $30 + Ord('0'));
    if (Result <> LatinNumChar)
    and (Byte(Key) = WordRec(VkKeyScanW(LatinNumChar)).Lo) then  // .Hi would be the shift state
      Result := LatinNumChar;
  end;
end;

function WideShortCutToText(WordShortCut: Word): WideString;
var
  Name: WideString;
begin
  if (not Win32PlatformIsUnicode)
  or (WordRec(WordShortCut).Lo in [$08..$09 {BKSP, TAB}, $0D {ENTER}, $1B {ESC}, $20..$28 {Misc Nav},
                               $2D..$2E {INS, DEL}, $70..$87 {F1 - F24}])
  then
    Result := ShortCutToText{TNT-ALLOW ShortCutToText}(WordShortCut)
  else begin
    case WordRec(WordShortCut).Lo of
      $30..$39: Name := WideGetKeyboardChar(WordRec(WordShortCut).Lo); {1-9,0}
      $41..$5A: Name := WideGetKeyboardChar(WordRec(WordShortCut).Lo); {A-Z}
      $60..$69: Name := WideGetKeyboardChar(WordRec(WordShortCut).Lo); {numpad 1-9,0}
    else
      Name := WideGetSpecialName(WordShortCut);
    end;
    if Name <> '' then
    begin
      Result := '';
      if WordShortCut and scShift <> 0 then Result := Result + SmkcShift;
      if WordShortCut and scCtrl <> 0 then Result := Result + SmkcCtrl;
      if WordShortCut and scAlt <> 0 then Result := Result + SmkcAlt;
      Result := Result + Name;
    end
    else Result := '';
  end;
end;

{ This function is *very* slow.  Use sparingly.  Return 0 if no VK code was
  found for the text }

function WideTextToShortCut(Text: WideString): TShortCut;

  { If the front of Text is equal to Front then remove the matching piece
    from Text and return True, otherwise return False }

  function CompareFront(var Text: WideString; const Front: WideString): Boolean;
  begin
    Result := (Pos(Front, Text) = 1);
    if Result then
      Delete(Text, 1, Length(Front));
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
begin
  Result := 0;
  Shift := 0;
  while True do
  begin
    if      CompareFront(Text, SmkcShift) then Shift := Shift or scShift
    else if CompareFront(Text, '^')       then Shift := Shift or scCtrl
    else if CompareFront(Text, SmkcCtrl)  then Shift := Shift or scCtrl
    else if CompareFront(Text, SmkcAlt)   then Shift := Shift or scAlt
    else Break;
  end;
  if Text = '' then Exit;
  for Key := $08 to $255 do { Copy range from table in ShortCutToText }
    if WideSameText(Text, WideShortCutToText(Key)) then
    begin
      Result := Key or Shift;
      Exit;
    end;
end;

function WideGetHotkeyPos(const Text: WideString): Integer;
var
  I, L: Integer;
begin
  Result := 0;
  I := 1;
  L := Length(Text);
  while I <= L do
  begin
    if (Text[I] = cHotkeyPrefix) and (L - I >= 1) then
    begin
      Inc(I);
      if Text[I] <> cHotkeyPrefix then
        Result := I; // this might not be the last
    end;
    Inc(I);
  end;
end;

function WideGetHotkey(const Text: WideString): WideString;
var
  I: Integer;
begin
  I := WideGetHotkeyPos(Text);
  if I = 0 then
    Result := ''
  else
    Result := Text[I];
end;

function WideStripHotkey(const Text: WideString): WideString;
var
  I: Integer;
begin
  Result := Text;
  I := 1;
  while I <= Length(Result) do
  begin
    if Result[I] = cHotkeyPrefix then
      if SysLocale.FarEast
      and ((I > 1) and (Length(Result) - I >= 2)
      and (Result[I - 1] = '(') and (Result[I + 2] = ')')) then begin
        Delete(Result, I - 1, 4);
        Dec(I, 2);
      end else
        Delete(Result, I, 1);
    Inc(I);
  end;
end;

function WideSameCaption(const Text1, Text2: WideString): Boolean;
begin
  Result := WideSameText(WideStripHotkey(Text1), WideStripHotkey(Text2));
end;

function WideSameCaptionStr(const Text1, Text2: WideString): Boolean;
begin
  Result := WideSameStr(WideStripHotkey(Text1), WideStripHotkey(Text2));
end;

function WideGetMenuItemCaption(MenuItem: TMenuItem{TNT-ALLOW TMenuItem}): WideString;
begin
  if MenuItem is TTntMenuItem then
    Result := TTntMenuItem(MenuItem).Caption
  else
    Result := MenuItem.Caption;
end;

function WideGetMenuItemHint(MenuItem: TMenuItem{TNT-ALLOW TMenuItem}): WideString;
begin
  if MenuItem is TTntMenuItem then
    Result := TTntMenuItem(MenuItem).Hint
  else
    Result := MenuItem.Hint;
end;

procedure NoOwnerDrawTopLevelItems(Menu: TMainMenu{TNT-ALLOW TMainMenu});
{If top-level items are created as owner-drawn, they will not appear as raised
buttons when the mouse hovers over them. The VCL will often create top-level
items as owner-drawn even when they don't need to be (owner-drawn state can be
set on an item-by-item basis). This routine turns off the owner-drawn flag for
top-level items if it appears unnecessary}

  function ItemHasValidImage(Item: TMenuItem{TNT-ALLOW TMenuItem}): boolean;
  var
    Images: TCustomImageList;
  begin
    Assert(Item <> nil, 'TNT Internal Error: ItemHasValidImage: item = nil');
    Images := Item.GetImageList;
    Result := (Assigned(Images) and (Item.ImageIndex >= 0) and (Item.ImageIndex < Images.Count))
           or (MenuItemHasBitmap(Item) and (not Item.Bitmap.Empty))
  end;

var
  HM: HMenu;
  i: integer;
  Info: TMenuItemInfoA;
  Item: TMenuItem{TNT-ALLOW TMenuItem};
  Win98Plus: boolean;
begin
  if Assigned(Menu) then begin
    Win98Plus:= (Win32MajorVersion > 4)
      or((Win32MajorVersion = 4) and (Win32MinorVersion > 0));
    if not Win98Plus then
      Exit; {exit if Windows 95 or NT 4.0}
    HM:= Menu.Handle;
    Info.cbSize:= sizeof(Info);
    for i := 0 to GetMenuItemCount(HM) - 1 do begin
      Info.fMask:= MIIM_FTYPE or MIIM_ID;
      if not GetMenuItemInfo(HM, i, true, Info) then
        Break;
      if Info.fType and MFT_OWNERDRAW <> 0 then begin
        Item:= Menu.FindItem(Info.wID, fkCommand);
        if not Assigned(Item) then
          continue;
        if Assigned(Item.OnDrawItem)
        or Assigned(Item.OnAdvancedDrawItem)
        or ItemHasValidImage(Item) then
          Continue;
        Info.fMask:= MIIM_FTYPE or MIIM_STRING;
        Info.fType:= (Info.fType and not MFT_OWNERDRAW) or MFT_STRING;
        if Win32PlatformIsUnicode and (Item is TTntMenuItem) then begin
          // Unicode
          TMenuItemInfoW(Info).dwTypeData:= PWideChar(TTntMenuItem(Item).Caption);
          SetMenuItemInfoW(HM, i, true, TMenuItemInfoW(Info));
        end else begin
          // Ansi
          Info.dwTypeData:= PAnsiChar(Item.Caption);
          SetMenuItemInfoA(HM, i, true, Info);
        end;
      end;
    end;
  end;
end;

{ TTntMenuItem's utility procs }

procedure SyncHotKeyPosition(const Source: WideString; var Dest: WideString);
var
  I: Integer;
  FarEastHotString: WideString;
begin
  if (AnsiString(Source) <> AnsiString(Dest))
  and WideSameCaptionStr(AnsiString(Source), AnsiString(Dest)) then begin
    // when reduced to ansi, the only difference is hot key positions
    Dest := WideStripHotkey(Dest);
    I := 1;
    while I <= Length(Source) do
    begin
      if Source[I] = cHotkeyPrefix then begin
        if SysLocale.FarEast
        and ((I > 1) and (Length(Source) - I >= 2)
        and (Source[I - 1] = '(') and (Source[I + 2] = ')')) then begin
          FarEastHotString := Copy(Source, I - 1, 4);
          Dec(I);
          Insert(FarEastHotString, Dest, I);
          Inc(I, 3);
        end else begin
          Insert(cHotkeyPrefix, Dest, I);
          Inc(I);
        end;
      end;
      Inc(I);
    end;
    // test work
    if AnsiString(Source) <> AnsiString(Dest) then
      raise ETntInternalError.CreateFmt('Internal Error: SyncHotKeyPosition Failed ("%s" <> "%s").',
        [AnsiString(Source), AnsiString(Dest)]);
  end;
end;

procedure UpdateMenuItems(Items: TMenuItem{TNT-ALLOW TMenuItem}; ParentMenu: TMenu);
var
  i: integer;
begin
  if (Items.ComponentState * [csReading, csDestroying] = []) then begin
    for i := Items.Count - 1 downto 0 do
      UpdateMenuItems(Items[i], ParentMenu);
    if Items is TTntMenuItem then
      TTntMenuItem(Items).UpdateMenuString(ParentMenu);
  end;
end;

procedure FixMenuBiDiProblem(Menu: TMenu);
var
  i: integer;
begin
  // TMenu sometimes sets bidi on first visible item which can convert caption to ansi
  if (SysLocale.MiddleEast)
  and (Menu <> nil)
  and (Menu.Items.Count > 0) then
  begin
    for i := 0 to Menu.Items.Count - 1 do begin
      if Menu.Items[i].Visible then begin
        if (Menu.Items[i] is TTntMenuItem) then
          (Menu.Items[i] as TTntMenuItem).UpdateMenuString(Menu);
        break; // found first visible menu item!
      end;
    end;
  end;
end;


{$IFDEF COMPILER_6} // verified against VCL source in Delphi 6 and BCB 6
type
  THackMenuItem = class(TComponent)
  protected
    FxxxxCaption: Ansistring;
    FxxxxHandle: HMENU;
    FxxxxChecked: Boolean;
    FxxxxEnabled: Boolean;
    FxxxxDefault: Boolean;
    FxxxxAutoHotkeys: TMenuItemAutoFlag;
    FxxxxAutoLineReduction: TMenuItemAutoFlag;
    FxxxxRadioItem: Boolean;
    FxxxxVisible: Boolean;
    FxxxxGroupIndex: Byte;
    FxxxxImageIndex: TImageIndex;
    FxxxxActionLink: TMenuActionLink{TNT-ALLOW TMenuActionLink};
    FxxxxBreak: TMenuBreak;
    FBitmap: TBitmap;
    FxxxxCommand: Word;
    FxxxxHelpContext: THelpContext;
    FxxxxHint: AnsiString;
    FxxxxItems: TList;
    FxxxxShortCut: TShortCut;
    FxxxxParent: TMenuItem{TNT-ALLOW TMenuItem};
    FMerged: TMenuItem{TNT-ALLOW TMenuItem};
    FMergedWith: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}
{$IFDEF DELPHI_7} // verified against VCL source in Delphi 7
type
  THackMenuItem = class(TComponent)
  protected
    FxxxxCaption: AnsiString;
    FxxxxHandle: HMENU;
    FxxxxChecked: Boolean;
    FxxxxEnabled: Boolean;
    FxxxxDefault: Boolean;
    FxxxxAutoHotkeys: TMenuItemAutoFlag;
    FxxxxAutoLineReduction: TMenuItemAutoFlag;
    FxxxxRadioItem: Boolean;
    FxxxxVisible: Boolean;
    FxxxxGroupIndex: Byte;
    FxxxxImageIndex: TImageIndex;
    FxxxxActionLink: TMenuActionLink{TNT-ALLOW TMenuActionLink};
    FxxxxBreak: TMenuBreak;
    FBitmap: TBitmap;
    FxxxxCommand: Word;
    FxxxxHelpContext: THelpContext;
    FxxxxHint: AnsiString;
    FxxxxItems: TList;
    FxxxxShortCut: TShortCut;
    FxxxxParent: TMenuItem{TNT-ALLOW TMenuItem};
    FMerged: TMenuItem{TNT-ALLOW TMenuItem};
    FMergedWith: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}
{$IFDEF DELPHI_9} // verified against VCL source in Delphi 9
type
  THackMenuItem = class(TComponent)
  protected
    FxxxxCaption: AnsiString;
    FxxxxHandle: HMENU;
    FxxxxChecked: Boolean;
    FxxxxEnabled: Boolean;
    FxxxxDefault: Boolean;
    FxxxxAutoHotkeys: TMenuItemAutoFlag;
    FxxxxAutoLineReduction: TMenuItemAutoFlag;
    FxxxxRadioItem: Boolean;
    FxxxxVisible: Boolean;
    FxxxxGroupIndex: Byte;
    FxxxxImageIndex: TImageIndex;
    FxxxxActionLink: TMenuActionLink{TNT-ALLOW TMenuActionLink};
    FxxxxBreak: TMenuBreak;
    FBitmap: TBitmap;
    FxxxxCommand: Word;
    FxxxxHelpContext: THelpContext;
    FxxxxHint: AnsiString;
    FxxxxItems: TList;
    FxxxxShortCut: TShortCut;
    FxxxxParent: TMenuItem{TNT-ALLOW TMenuItem};
    FMerged: TMenuItem{TNT-ALLOW TMenuItem};
    FMergedWith: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}
{$IFDEF DELPHI_10} // verified against VCL source in Delphi 10
type
  THackMenuItem = class(TComponent)
  protected
    FxxxxCaption: AnsiString;
    FxxxxHandle: HMENU;
    FxxxxChecked: Boolean;
    FxxxxEnabled: Boolean;
    FxxxxDefault: Boolean;
    FxxxxAutoHotkeys: TMenuItemAutoFlag;
    FxxxxAutoLineReduction: TMenuItemAutoFlag;
    FxxxxRadioItem: Boolean;
    FxxxxVisible: Boolean;
    FxxxxGroupIndex: Byte;
    FxxxxImageIndex: TImageIndex;
    FxxxxActionLink: TMenuActionLink{TNT-ALLOW TMenuActionLink};
    FxxxxBreak: TMenuBreak;
    FBitmap: TBitmap;
    FxxxxCommand: Word;
    FxxxxHelpContext: THelpContext;
    FxxxxHint: AnsiString;
    FxxxxItems: TList;
    FxxxxShortCut: TShortCut;
    FxxxxParent: TMenuItem{TNT-ALLOW TMenuItem};
    FMerged: TMenuItem{TNT-ALLOW TMenuItem};
    FMergedWith: TMenuItem{TNT-ALLOW TMenuItem};
  end;
{$ENDIF}

function MenuItemHasBitmap(MenuItem: TMenuItem{TNT-ALLOW TMenuItem}): Boolean;
begin
  Result := Assigned(THackMenuItem(MenuItem).FBitmap);
end;

{ TTntMenuItem }

procedure TTntMenuItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

type TAccessActionlink = class(TActionLink);

procedure TTntMenuItem.InitiateAction;
begin
  if GetKeyboardLayout(0) <> FKeyboardLayout then
    MenuChanged(False);
  inherited;
end;

function TTntMenuItem.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or (not TAccessActionlink(ActionLink).IsCaptionLinked);
end;

procedure TTntMenuItem.SetInheritedCaption(const Value: AnsiString);
begin
  inherited Caption := Value;
end;

function TTntMenuItem.GetCaption: WideString;
begin
  if (AnsiString(FCaption) <> inherited Caption)
  and WideSameCaptionStr(AnsiString(FCaption), inherited Caption) then
  begin
    // only difference is hotkey position, update caption with new hotkey position
    SyncHotKeyPosition(inherited Caption, FCaption);
  end;
  Result := GetSyncedWideString(FCaption, (inherited Caption));
end;

procedure TTntMenuItem.SetCaption(const Value: WideString);
begin
  GetCaption; // auto adjust for hot key changes
  SetSyncedWideString(Value, FCaption, (inherited Caption), SetInheritedCaption);
end;

function TTntMenuItem.GetHint: WideString;
begin
  Result := GetSyncedWideString(FHint, inherited Hint);
end;

procedure TTntMenuItem.SetInheritedHint(const Value: AnsiString);
begin
  inherited Hint := Value;
end;

procedure TTntMenuItem.SetHint(const Value: WideString);
begin
  SetSyncedWideString(Value, FHint, inherited Hint, SetInheritedHint);
end;

function TTntMenuItem.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not TAccessActionlink(ActionLink).IsHintLinked;
end;

procedure TTntMenuItem.Loaded;
begin
  inherited;
  UpdateMenuString(GetParentMenu);
end;

procedure TTntMenuItem.MenuChanged(Rebuild: Boolean);
begin
  if (not FIgnoreMenuChanged) then begin
    inherited;
    UpdateMenuItems(Self, GetParentMenu);
    FixMenuBiDiProblem(GetParentMenu);
  end;
end;

procedure TTntMenuItem.UpdateMenuString(ParentMenu: TMenu);
var
  ParentHandle: THandle;

  function NativeMenuTypeIsString: Boolean;
  var
    MenuItemInfo: TMenuItemInfoW;
    Buffer: array[0..79] of WideChar;
  begin
    MenuItemInfo.cbSize := 44; // Required for Windows NT 4.0
    MenuItemInfo.fMask := MIIM_TYPE;
    MenuItemInfo.dwTypeData := Buffer; // ??
    MenuItemInfo.cch := Length(Buffer); // ??
    Result := GetMenuItemInfoW(ParentHandle, Command, False, MenuItemInfo)
         and ((MenuItemInfo.fType and (MFT_BITMAP or MFT_SEPARATOR or MFT_OWNERDRAW)) = 0)
  end;

  function NativeMenuString: WideString;
  var
    Len: Integer;
  begin
    Assert(Win32PlatformIsUnicode);
    Len := GetMenuStringW(ParentHandle, Command, nil, 0, MF_BYCOMMAND);
    if Len = 0 then
      Result := ''
    else begin
      SetLength(Result, Len + 1);
      Len := GetMenuStringW(ParentHandle, Command, PWideChar(Result), Len + 1, MF_BYCOMMAND);
      SetLength(Result, Len);
    end;
  end;

  procedure SetMenuString(const Value: WideString);
  var
    MenuItemInfo: TMenuItemInfoW;
    Buffer: array[0..79] of WideChar;
  begin
    MenuItemInfo.cbSize := 44; // Required for Windows NT 4.0
    MenuItemInfo.fMask := MIIM_TYPE;
    MenuItemInfo.dwTypeData := Buffer; // ??
    MenuItemInfo.cch := Length(Buffer); // ??
    if GetMenuItemInfoW(ParentHandle, Command, False, MenuItemInfo)
    and ((MenuItemInfo.fType and (MFT_BITMAP or MFT_SEPARATOR or MFT_OWNERDRAW)) = 0) then
    begin
      MenuItemInfo.dwTypeData := PWideChar(Value);
      MenuItemInfo.cch := Length(Value);
      Win32Check(SetMenuItemInfoW(ParentHandle, Command, False, MenuItemInfo));
    end;
  end;

  function SameEvent(A, B: TMenuMeasureItemEvent): Boolean;
  begin
    Result := @A = @B;
  end;

var
  MenuCaption: WideString;
begin
  FKeyboardLayout := GetKeyboardLayout(0);
  if Parent = nil then
    ParentHandle := 0
  else if (THackMenuItem(Self.Parent).FMergedWith <> nil) then
    ParentHandle := THackMenuItem(Self.Parent).FMergedWith.Handle
  else
    ParentHandle := Parent.Handle;

  if (Win32PlatformIsUnicode)
  and (Parent <> nil) and (ParentMenu <> nil)
  and (ComponentState * [csReading, csDestroying] = [])
  and (Visible)
  and (NativeMenuTypeIsString) then begin
    MenuCaption := Caption;
    if (Count = 0)
    and ((ShortCut <> scNone)
    and ((Parent = nil) or (Parent.Parent <> nil) or not (Parent.Owner is TMainMenu{TNT-ALLOW TMainMenu}))) then
      MenuCaption := MenuCaption + #9 + WideShortCutToText(ShortCut);
    if (NativeMenuString <> MenuCaption) then
    begin
      SetMenuString(MenuCaption);
      if  ((Parent = ParentMenu.Items) or (THackMenuItem(Self.Parent).FMergedWith <> nil))
      and (ParentMenu is TMainMenu{TNT-ALLOW TMainMenu})
      and (ParentMenu.WindowHandle <> 0) then
        DrawMenuBar(ParentMenu.WindowHandle) {top level menu bar items}
    end;
  end;
end;

function TTntMenuItem.GetAlignmentDrawStyle: Word;
const
  Alignments: array[TPopupAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  ParentMenu: TMenu;
  Alignment: TPopupAlignment;
begin
  ParentMenu := GetParentMenu;
  if ParentMenu is TMenu then
    Alignment := paLeft
  else if ParentMenu is TPopupMenu{TNT-ALLOW TPopupMenu} then
    Alignment := TPopupMenu{TNT-ALLOW TPopupMenu}(ParentMenu).Alignment
  else
    Alignment := paLeft;
  Result := Alignments[Alignment];
end;

procedure TTntMenuItem.AdvancedDrawItem(ACanvas: TCanvas; ARect: TRect;
  State: TOwnerDrawState; TopLevel: Boolean);

  procedure DrawMenuText(BiDi: Boolean);
  var
    ImageList: TCustomImageList;
    DrawImage, DrawGlyph: Boolean;
    GlyphRect, SaveRect: TRect;
    DrawStyle: Longint;
    Selected: Boolean;
    Win98Plus: Boolean;
    Win2K: Boolean;
  begin
    ImageList := GetImageList;
    Selected := odSelected in State;
    Win98Plus := (Win32MajorVersion > 4) or
      ((Win32MajorVersion = 4) and (Win32MinorVersion > 0));
    Win2K := (Win32MajorVersion > 4) and (Win32Platform = VER_PLATFORM_WIN32_NT);
    with ACanvas do
    begin
      GlyphRect.Left := ARect.Left + 1;
      DrawImage := (ImageList <> nil) and ((ImageIndex > -1) and
        (ImageIndex < ImageList.Count) or Checked and ((not MenuItemHasBitmap(Self)) or
        Bitmap.Empty));
      if DrawImage or MenuItemHasBitmap(Self) and not Bitmap.Empty then
      begin
        DrawGlyph := True;
        if DrawImage then
          GlyphRect.Right := GlyphRect.Left + ImageList.Width
        else begin
          { Need to add BitmapWidth/Height properties for TMenuItem if we're to
            support them.  Right now let's hardcode them to 16x16. }
          GlyphRect.Right := GlyphRect.Left + 16;
        end;
        { Draw background pattern brush if selected }
        if Checked then
        begin
          Inc(GlyphRect.Right);
          if not Selected then
            Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
          Inc(GlyphRect.Left);
        end;
        if Checked then
          Dec(GlyphRect.Right);
      end else begin
        if (ImageList <> nil) and (not TopLevel) then
          GlyphRect.Right := GlyphRect.Left + ImageList.Width
        else
          GlyphRect.Right := GlyphRect.Left;
        DrawGlyph := False;
      end;
      if BiDi then begin
        SaveRect := GlyphRect;
        GlyphRect.Left := ARect.Right - (SaveRect.Right - ARect.Left);
        GlyphRect.Right := ARect.Right - (SaveRect.Left - ARect.Left);
      end;
      with GlyphRect do begin
        Dec(Left);
        Inc(Right, 2);
      end;
      if Selected then begin
        if DrawGlyph then begin
          if BiDi then
            ARect.Right := GlyphRect.Left - 1
          else
            ARect.Left := GlyphRect.Right + 1;
        end;
        if not (Win98Plus and TopLevel) then
          Brush.Color := clHighlight;
      end;
      if TopLevel and Win98Plus and (not Selected)
      {$IFDEF COMPILER_7_UP}
      and (not Win32PlatformIsXP)
      {$ENDIF}
      then
        OffsetRect(ARect, 0, -1);
      if not (Selected and DrawGlyph) then begin
        if BiDi then
          ARect.Right := GlyphRect.Left - 1
        else
          ARect.Left := GlyphRect.Right + 1;
      end;
      Inc(ARect.Left, 2);
      Dec(ARect.Right, 1);
      DrawStyle := DT_EXPANDTABS or DT_SINGLELINE or GetAlignmentDrawStyle;
      if Win2K and (odNoAccel in State) then
        DrawStyle := DrawStyle or DT_HIDEPREFIX;
      { Calculate vertical layout }
      SaveRect := ARect;
      if odDefault in State then
        Font.Style := [fsBold];
      DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle or DT_CALCRECT or DT_NOCLIP);
      if BiDi then begin
        { the DT_CALCRECT does not take into account alignment }
        ARect.Left := SaveRect.Left;
        ARect.Right := SaveRect.Right;
      end;
      OffsetRect(ARect, 0, ((SaveRect.Bottom - SaveRect.Top) - (ARect.Bottom - ARect.Top)) div 2);
      if TopLevel and Selected and Win98Plus
      {$IFDEF COMPILER_7_UP}
      and (not Win32PlatformIsXP)
      {$ENDIF}
      then
        OffsetRect(ARect, 1, 0);
      DoDrawText(ACanvas, Caption, ARect, Selected, DrawStyle);
      if (ShortCut <> scNone) and not TopLevel then
      begin
        if BiDi then begin
          ARect.Left := 10;
          ARect.Right := ARect.Left + WideCanvasTextWidth(ACanvas, WideShortCutToText(ShortCut));
        end else begin
          ARect.Left := ARect.Right;
          ARect.Right := SaveRect.Right - 10;
        end;
        DoDrawText(ACanvas, WideShortCutToText(ShortCut), ARect, Selected, DT_RIGHT);
      end;
    end;
  end;

var
  ParentMenu: TMenu;
  SaveCaption: WideString;
  SaveShortCut: TShortCut;
begin
  ParentMenu := GetParentMenu;
  if (not Win32PlatformIsUnicode)
  or (Self.IsLine)
  or (     (ParentMenu <> nil) and (ParentMenu.OwnerDraw or (GetImageList <> nil))
       and (Assigned(OnAdvancedDrawItem) or Assigned(OnDrawItem))    ) then
    inherited
  else begin
    SaveCaption := Caption;
    SaveShortCut := ShortCut;
    try
      FIgnoreMenuChanged := True;
      try
        Caption := '';
        ShortCut := scNone;
      finally
        FIgnoreMenuChanged := False;
      end;
      inherited;
    finally
      FIgnoreMenuChanged := True;
      try
        Caption := SaveCaption;
        ShortCut := SaveShortcut;
      finally
        FIgnoreMenuChanged := False;
      end;
    end;
    DrawMenuText((ParentMenu <> nil) and (ParentMenu.IsRightToLeft))
  end;
end;

procedure TTntMenuItem.DoDrawText(ACanvas: TCanvas; const ACaption: WideString;
  var Rect: TRect; Selected: Boolean; Flags: Longint);
var
  Text: WideString;
  ParentMenu: TMenu;
begin
  if (not Win32PlatformIsUnicode)
  or (IsLine) then
    inherited DoDrawText(ACanvas, ACaption, Rect, Selected, Flags)
  else begin
    ParentMenu := GetParentMenu;
    if (ParentMenu <> nil) and (ParentMenu.IsRightToLeft) then
    begin
      if Flags and DT_LEFT = DT_LEFT then
        Flags := Flags and (not DT_LEFT) or DT_RIGHT
      else if Flags and DT_RIGHT = DT_RIGHT then
        Flags := Flags and (not DT_RIGHT) or DT_LEFT;
      Flags := Flags or DT_RTLREADING;
    end;
    Text := ACaption;
    if (Flags and DT_CALCRECT <> 0) and ((Text = '') or
      (Text[1] = cHotkeyPrefix) and (Text[2] = #0)) then Text := Text + ' ';
    with ACanvas do
    begin
      Brush.Style := bsClear;
      if Default then
        Font.Style := Font.Style + [fsBold];
      if not Enabled then
      begin
        if not Selected then
        begin
          OffsetRect(Rect, 1, 1);
          Font.Color := clBtnHighlight;
          Tnt_DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Flags);
          OffsetRect(Rect, -1, -1);
        end;
        if Selected and (ColorToRGB(clHighlight) = ColorToRGB(clBtnShadow)) then
          Font.Color := clBtnHighlight else
          Font.Color := clBtnShadow;
      end;
      Tnt_DrawTextW(Handle, PWideChar(Text), Length(Text), Rect, Flags);
    end;
  end;
end;

function TTntMenuItem.MeasureItemTextWidth(ACanvas: TCanvas; const Text: WideString): Integer;
var
  R: TRect;
begin
  FillChar(R, SizeOf(R), 0);
  DoDrawText(ACanvas, Text, R, False,
    GetAlignmentDrawStyle or DT_EXPANDTABS or DT_SINGLELINE or DT_NOCLIP or DT_CALCRECT);
  Result := R.Right - R.Left;
end;

procedure TTntMenuItem.MeasureItem(ACanvas: TCanvas; var Width, Height: Integer);
var
  SaveMeasureItemEvent: TMenuMeasureItemEvent;
begin
  if (not Win32PlatformIsUnicode)
  or (Self.IsLine) then
    inherited
  else begin
    SaveMeasureItemEvent := inherited OnMeasureItem;
    try
      inherited OnMeasureItem := nil;
      inherited;
      Inc(Width, MeasureItemTextWidth(ACanvas, Caption));
      Dec(Width, MeasureItemTextWidth(ACanvas, inherited Caption));
      if ShortCut <> scNone then begin
        Inc(Width, MeasureItemTextWidth(ACanvas, WideShortCutToText(ShortCut)));
        Dec(Width, MeasureItemTextWidth(ACanvas, ShortCutToText{TNT-ALLOW ShortCutToText}(ShortCut)));
      end;
    finally
      inherited OnMeasureItem := SaveMeasureItemEvent;
    end;
    if Assigned(OnMeasureItem) then OnMeasureItem(Self, ACanvas, Width, Height);
  end;
end;

function TTntMenuItem.Find(ACaption: WideString): TMenuItem{TNT-ALLOW TMenuItem};
var
  I: Integer;
begin
  Result := nil;
  ACaption := WideStripHotkey(ACaption);
  for I := 0 to Count - 1 do
    if WideSameText(ACaption, WideStripHotkey(WideGetMenuItemCaption(Items[I]))) then
    begin
      Result := Items[I];
      System.Break;
    end;
end;

function TTntMenuItem.GetActionLinkClass: TMenuActionLinkClass;
begin
  Result := TTntMenuActionLink;
end;

procedure TTntMenuItem.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  if (Sender is TCustomAction{TNT-ALLOW TCustomAction}) and Supports(Sender, ITntAction) then begin
    if not CheckDefaults or (Caption = '') then
      Caption := TntAction_GetCaption(TCustomAction{TNT-ALLOW TCustomAction}(Sender));
    if not CheckDefaults or (Hint = '') then
      Hint := TntAction_GetHint(TCustomAction{TNT-ALLOW TCustomAction}(Sender));
  end;
  inherited;
end;

{ TTntMainMenu }

{$IFDEF COMPILER_9_UP}
function TTntMainMenu.CreateMenuItem: TMenuItem{TNT-ALLOW TMenuItem};
begin
  Result := TTntMenuItem.Create(Self);
end;
{$ENDIF}

procedure TTntMainMenu.DoChange(Source: TMenuItem{TNT-ALLOW TMenuItem}; Rebuild: Boolean);
begin
  inherited;
  UpdateMenuItems(Items, Self);
  if (THackMenuItem(Items).FMerged <> nil) then begin
    UpdateMenuItems(THackMenuItem(Items).FMerged, Self);
  end;
end;

{ TTntPopupMenu }

constructor TTntPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  PopupList.Remove(Self);
  if TntPopupList <> nil then
    TntPopupList.Add(Self);
end;

{$IFDEF COMPILER_9_UP}
function TTntPopupMenu.CreateMenuItem: TMenuItem{TNT-ALLOW TMenuItem};
begin
  Result := TTntMenuItem.Create(Self);
end;
{$ENDIF}

destructor TTntPopupMenu.Destroy;
begin
  if TntPopupList <> nil then
    TntPopupList.Remove(Self);
  PopupList.Add(Self);
  inherited;
end;

procedure TTntPopupMenu.DoChange(Source: TMenuItem{TNT-ALLOW TMenuItem}; Rebuild: Boolean);
begin
  inherited;
  UpdateMenuItems(Items, Self);
end;

procedure TTntPopupMenu.Popup(X, Y: Integer);
begin
  Menus.PopupList := TntPopupList;
  try
    inherited;
  finally
    Menus.PopupList := TntPopupList.SavedPopupList;
  end;
end;

{ TTntPopupList }

procedure TTntPopupList.WndProc(var Message: TMessage);
var
  I, Item: Integer;
  MenuItem: TMenuItem{TNT-ALLOW TMenuItem};
  FindKind: TFindItemKind;
begin
  case Message.Msg of
    WM_ENTERMENULOOP:
      begin
        Menus.PopupList := SavedPopupList;
        for i := 0 to Count - 1 do
          FixMenuBiDiProblem(Items[i]);
      end;
    WM_MENUSELECT:
      with TWMMenuSelect(Message) do
      begin
        FindKind := fkCommand;
        if MenuFlag and MF_POPUP <> 0 then FindKind := fkHandle;
        for I := 0 to Count - 1 do
        begin
          if FindKind = fkHandle then
          begin
            if Menu <> 0 then
              Item := Integer(GetSubMenu(Menu, IDItem)) else
              Item := -1;
          end
          else
            Item := IDItem;
          MenuItem := TPopupMenu{TNT-ALLOW TPopupMenu}(Items[I]).FindItem(Item, FindKind);
          if MenuItem <> nil then
          begin
            TntApplication.Hint := WideGetLongHint(WideGetMenuItemHint(MenuItem));
            Exit;
          end;
        end;
        TntApplication.Hint := '';
      end;
  end;
  inherited;
end;

initialization
  TntPopupList := TTntPopupList.Create;
  TntPopupList.SavedPopupList := Menus.PopupList;

finalization
  FreeAndNil(TntPopupList);

end.
