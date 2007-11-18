
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntWideStringProperty_Design;

{$INCLUDE ..\Source\TntCompilers.inc}

interface

{*****************************************************}
{  TWideCharProperty-editor implemented by Maël Hörz  }
{*****************************************************}

{$IFDEF COMPILER_9_UP}
  {$MESSAGE FATAL 'The Object Inspector in Delphi 9 is already Unicode enabled.'}
{$ENDIF}

uses
  Classes, Messages, Windows, Graphics, TypInfo, TntDesignEditors_Design,
  DesignIntf, DesignEditors, VCLEditors;

type
  TWideStringProperty = class(TPropertyEditor, ICustomPropertyDrawing)
  private
    FActivateWithoutGetValue: Boolean;
    FPropList: PInstPropList;
  protected
    procedure SetPropEntry(Index: Integer; AInstance: TPersistent; APropInfo: PPropInfo); override;
    function GetWideStrValueAt(Index: Integer): WideString; dynamic;
    function GetWideStrValue: WideString;
    procedure SetWideStrValue(const Value: WideString); dynamic;
    function GetWideVisualValue: WideString;
  public
    constructor Create(const ADesigner: ITntDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    procedure Activate; override;
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    function AllEqual: Boolean; override;
    function GetEditLimit: Integer; override;
    function GetValue: AnsiString; override;
    procedure SetValue(const Value: AnsiString); override;
    {$IFDEF MULTI_LINE_STRING_EDITOR}
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    {$ENDIF}
  end;

  TWideCaptionProperty = class(TWideStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TWideCharProperty = class(TWideStringProperty)
  protected
    {$IFDEF COMPILER_7_UP}
    function GetIsDefault: Boolean; override;
    {$ENDIF}
    function GetWideStrValueAt(Index: Integer): WideString; override;
    procedure SetWideStrValue(const Value: WideString); override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
  end;

procedure Register;

implementation

uses
  Controls, Forms, SysUtils, StdCtrls, TntGraphics, TntControls,
  TntSysUtils, TntSystem, Consts,
  RTLConsts;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(WideString), nil, '', TWideStringProperty);
  RegisterPropertyEditor(TypeInfo(TWideCaption), nil, '', TWideCaptionProperty);
  RegisterPropertyEditor(TypeInfo(WideChar), nil, '', TWideCharProperty);
end;

function GetOIInspListBox: TWinControl;
var
  ObjectInspectorForm: TCustomForm;
  Comp: TComponent;
begin
  Result := nil;
  ObjectInspectorForm := GetObjectInspectorForm;
  if ObjectInspectorForm <> nil then begin
    Comp := ObjectInspectorForm.FindComponent('PropList');
    if Comp is TWinControl then
      Result := TWinControl(Comp);
  end;
end;

function GetOIPropInspEdit: TCustomEdit{TNT-ALLOW TCustomEdit};
var
  OIInspListBox: TWinControl;
  Comp: TComponent;
begin
  Result := nil;
  OIInspListBox := GetOIInspListBox;
  if OIInspListBox <> nil then begin
    Comp := OIInspListBox.FindComponent('EditControl');
    if Comp is TCustomEdit{TNT-ALLOW TCustomEdit} then
      Result := TCustomEdit{TNT-ALLOW TCustomEdit}(Comp);
  end;
end;
//------------------------------

type TAccessWinControl = class(TWinControl);

{ TWideStringProperty }

var
  WideStringPropertyCount: Integer = 0;

constructor TWideStringProperty.Create(const ADesigner: ITntDesigner; APropCount: Integer);
begin
  inherited;
  Inc(WideStringPropertyCount);
  GetMem(FPropList, APropCount * SizeOf(TInstProp));
end;

procedure ConvertObjectInspectorBackToANSI;
var
  Edit: TCustomEdit{TNT-ALLOW TCustomEdit};
begin
  if (Win32PlatformIsUnicode) then begin
    Edit := GetOIPropInspEdit;
    if Assigned(Edit)
    and IsWindowUnicode(Edit.Handle) then
      TAccessWinControl(Edit).RecreateWnd;
  end;
end;

destructor TWideStringProperty.Destroy;
begin
  Dec(WideStringPropertyCount);
  if (WideStringPropertyCount = 0) then
    ConvertObjectInspectorBackToANSI;
  if FPropList <> nil then
    FreeMem(FPropList, PropCount * SizeOf(TInstProp));
  inherited;
end;

{$IFDEF DELPHI_7} // verified against VCL source in Delphi 7
type
  THackPropertyEditor = class
    FDesigner: IDesigner;
    FPropList: PInstPropList;
  end;
{$ENDIF}

procedure TWideStringProperty.Activate;
var
  Edit: TCustomEdit{TNT-ALLOW TCustomEdit};
begin
  FActivateWithoutGetValue := True;
  if (Win32PlatformIsUnicode) then begin
    Edit := GetOIPropInspEdit;
    if Assigned(Edit)
    and (not IsWindowUnicode(Edit.Handle)) then
      ReCreateUnicodeWnd(Edit, 'EDIT', True);
  end;
end;

procedure TWideStringProperty.SetPropEntry(Index: Integer;
  AInstance: TPersistent; APropInfo: PPropInfo);
begin
  inherited;
  with FPropList^[Index] do
  begin
    Instance := AInstance;
    PropInfo := APropInfo;
  end;
end;

function TWideStringProperty.GetWideStrValueAt(Index: Integer): WideString;
begin
  with FPropList^[Index] do Result := GetWideStrProp(Instance, PropInfo);
end;

function TWideStringProperty.GetWideStrValue: WideString;
begin
  Result := GetWideStrValueAt(0);
end;

procedure TWideStringProperty.SetWideStrValue(const Value: WideString);
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
    with FPropList^[I] do SetWideStrProp(Instance, PropInfo, Value);
  Modified;
end;

function TWideStringProperty.GetWideVisualValue: WideString;
begin
  if AllEqual then
    Result := GetWideStrValue
  else
    Result := '';
end;

procedure TWideStringProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TWideStringProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  WideCanvasTextRect(ACanvas, ARect, ARect.Left + 1, ARect.Top + 1, GetWideVisualValue);
end;

function TWideStringProperty.AllEqual: Boolean;
var
  I: Integer;
  V: WideString;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetWideStrValue;
    for I := 1 to PropCount - 1 do
      if GetWideStrValueAt(I) <> V then Exit;
  end;
  Result := True;
end;

function TWideStringProperty.GetEditLimit: Integer;
var
  Edit: TCustomEdit{TNT-ALLOW TCustomEdit};
begin
  Result := MaxInt;
  // GetEditLimit is called right before the inplace editor text has been set
  if Win32PlatformIsUnicode then begin
    Edit := GetOIPropInspEdit;
    if Assigned(Edit) then begin
      TntControl_SetText(Edit, GetWideStrValue);
      TntControl_SetHint(Edit, GetWideStrValue);
    end;
  end;
end;

function TWideStringProperty.GetValue: AnsiString;
begin
  FActivateWithoutGetValue := False;
  Result := WideStringToStringEx(GetWideStrValue, CP_ACP{TNT-ALLOW CP_ACP}); // use the same code page as the inplace editor
end;

procedure TWideStringProperty.SetValue(const Value: AnsiString);
var
  Edit: TCustomEdit{TNT-ALLOW TCustomEdit};
begin
  if (not FActivateWithoutGetValue) then begin
    Edit := GetOIPropInspEdit;
    if Assigned(Edit) and Win32PlatformIsUnicode then
      SetWideStrValue(TntControl_GetText(Edit))
    else
      SetWideStrValue(StringToWideStringEx(Value, CP_ACP{TNT-ALLOW CP_ACP})); // use the same code page as the inplace editor
  end;
end;

{$IFDEF MULTI_LINE_STRING_EDITOR}
function TWideStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

procedure TWideStringProperty.Edit;
var
  Temp: WideString;
begin
  with TTntStrEditDlg.Create(Application) do
  try
    PrepareForWideStringEdit;
    Memo.Text := GetWideStrValue;
    UpdateStatus(nil);
    if ShowModal = mrOk then begin
      Temp := Memo.Text;
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1); { trim control characters from end }
      SetWideStrValue(Temp);
    end;
  finally
    Free;
  end;
end;
{$ENDIF}

{ TWideCaptionProperty }

function TWideCaptionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paAutoUpdate];
end;

{ TWideCharProperty }

function TWideCharProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
end;            

function TWideCharProperty.GetEditLimit: Integer;
begin
  inherited GetEditLimit;
  Result := 63;
end;

{$IFDEF COMPILER_7_UP}
function TWideCharProperty.GetIsDefault: Boolean;
var
  i: Integer;
  OldPropList: PInstPropList;
begin
  Result := True;
  if PropCount > 0 then
  begin
    OldPropList := THackPropertyEditor(Self).FPropList;
    // The memory FPropList points to is write-protected.
    // In the constructor we dynamically allocated our own PropList,
    // which can be written, so point there instead.
    THackPropertyEditor(Self).FPropList := FPropList;

    // Delphi can't handle WideChar-type, but does well with Word-type,
    // which has exactly the same size as WideChar (i.e. 2 Bytes)
    for i := 0 to PropCount - 1 do
      FPropList^[i].PropInfo^.PropType^ := TypeInfo(Word);

    Result := inherited GetIsDefault;

    for i := 0 to PropCount - 1 do
      FPropList^[i].PropInfo^.PropType^ := TypeInfo(WideChar);

    THackPropertyEditor(Self).FPropList := OldPropList;
  end;
end;
{$ENDIF}

function IsCharGraphic(C: WideChar): Boolean;
begin
  if Win32PlatformIsUnicode then
    Result := not IsWideCharCntrl(C) and not IsWideCharSpace(C)
  else // representation as charcode avoids corruption on ANSI-systems
    Result := (C >= #33) and (C <= #127);
end;

function TWideCharProperty.GetWideStrValueAt(Index: Integer): WideString;
var
  C: WideChar;
begin
  with FPropList^[Index] do
    C := WideChar(GetOrdProp(Instance, PropInfo));

  if IsCharGraphic(C) then
    Result := C
  else
    Result := WideFormat('#%d', [Ord(C)]);
end;

procedure TWideCharProperty.SetWideStrValue(const Value: WideString);
var
  C: Longint;
  I: Integer;
begin
  if Length(Value) = 0 then
    C := 0
  else if Length(Value) = 1 then
    C := Ord(Value[1])
  else if Value[1] = '#' then
    C := StrToInt(Copy(Value, 2, Maxint))
  else
    raise EPropertyError.Create(SInvalidPropertyValue);

  with GetTypeData(GetPropType)^ do
    if (C < MinValue) or (C > MaxValue) then
      raise EPropertyError.CreateFmt(SOutOfRange, [MinValue, MaxValue]);

  for I := 0 to PropCount - 1 do
    with FPropList^[I] do SetOrdProp(Instance, PropInfo, C);
    
  Modified;
end;

initialization

finalization
  ConvertObjectInspectorBackToANSI;

end.
