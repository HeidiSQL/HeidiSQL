unit VirtualTreesReg;

// This unit is an addendum to VirtualTrees.pas and contains code of design time editors as well as
// for theirs and the tree's registration.

interface

// For some things to work we need code, which is classified as being unsafe for .NET.
{$warn UNSAFE_TYPE off}
{$warn UNSAFE_CAST off}
{$warn UNSAFE_CODE off}

uses
  Windows, Classes, DesignIntf, DesignEditors, VCLEditors, PropertyCategories,
  ColnEdit, VirtualTrees, VirtualTrees.HeaderPopup;

type
  TVirtualTreeEditor = class (TDefaultEditor)
  public
    procedure Edit; override;
  end;

procedure Register;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  StrEdit, Dialogs, TypInfo, SysUtils, Graphics, CommCtrl, ImgList, Controls,
  VirtualTrees.ClipBoard, VirtualTrees.Actions;

type
  // The usual trick to make a protected property accessible in the ShowCollectionEditor call below.
  TVirtualTreeCast = class(TBaseVirtualTree);

  TClipboardElement = class(TNestedProperty, ICustomPropertyDrawing)
  private
    FElement: string;
  protected
    constructor Create(Parent: TPropertyEditor; AElement: string); reintroduce;
  public
    function AllEqual: Boolean; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;

    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  // This is a special property editor to make the strings in the clipboard format string list
  // being shown as subproperties in the object inspector. This way it is shown what formats are actually available
  // and the user can pick them with a simple yes/no choice.

  TGetPropEditProc = TGetPropProc;

  TClipboardFormatsProperty = class(TStringListProperty, ICustomPropertyDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  resourcestring
    sVTHeaderCategoryName = 'Header';
    sVTPaintingCategoryName = 'Custom painting';
    sVTIncremenalCategoryName = 'Incremental search';

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualTreeEditor.Edit;

begin
  ShowCollectionEditor(Designer, Component, TVirtualTreeCast(Component).Header.Columns, 'Columns');
end;

//----------------------------------------------------------------------------------------------------------------------

constructor TClipboardElement.Create(Parent: TPropertyEditor; AElement: string);

begin
  inherited Create(Parent);
  FElement := AElement;
end;

//----------------------------------------------------------------------------------------------------------------------

function TClipboardElement.AllEqual: Boolean;

// Determines if this element is included or excluded in all selected components it belongs to.

var
  I, Index: Integer;
  List: TClipboardFormats;
  V: Boolean;

begin
  Result := False;
  if PropCount > 1 then
  begin
    List := TClipboardFormats(GetOrdValue);
    V := List.Find(FElement, Index);
    for I := 1 to PropCount - 1 do
    begin
      List := TClipboardFormats(GetOrdValue);
      if List.Find(FElement, Index) <> V then
        Exit;
    end;
  end;
  Result := True;
end;

//----------------------------------------------------------------------------------------------------------------------

function TClipboardElement.GetAttributes: TPropertyAttributes;

begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

//----------------------------------------------------------------------------------------------------------------------

function TClipboardElement.GetName: string;

begin
  Result := FElement;
end;

//----------------------------------------------------------------------------------------------------------------------

function TClipboardElement.GetValue: string;

var
  List: TClipboardFormats;

begin
  List := TClipboardFormats(GetOrdValue);
  Result := BooleanIdents[List.IndexOf(FElement) > -1];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardElement.GetValues(Proc: TGetStrProc);

begin
  Proc(BooleanIdents[False]);
  Proc(BooleanIdents[True]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardElement.SetValue(const Value: string);

var
  List: TClipboardFormats;
  I, Index: Integer;

begin
  if CompareText(Value, 'True') = 0 then
  begin
    for I := 0 to PropCount - 1 do
    begin
      List := TClipboardFormats(GetOrdValueAt(I));
      List.Add(FElement);
    end;
  end
  else
  begin
    for I := 0 to PropCount - 1 do
    begin
      List := TClipboardFormats(GetOrdValueAt(I));
      if List.Find(FElement, Index) then
        List.Delete(Index);
    end;
  end;
  Modified;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure DrawBoolean(Checked: Boolean; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

var
  BoxSize,
  EntryWidth: Integer;
  R: TRect;
  State: Cardinal;

begin
  with ACanvas do
  begin
    FillRect(ARect);

    BoxSize := ARect.Bottom - ARect.Top;
    EntryWidth := ARect.Right - ARect.Left;

    R := Rect(ARect.Left + (EntryWidth - BoxSize) div 2, ARect.Top, ARect.Left + (EntryWidth + BoxSize) div 2,
      ARect.Bottom);
    InflateRect(R, -1, -1);
    State := DFCS_BUTTONCHECK;
    if Checked then
      State := State or DFCS_CHECKED;
    DrawFrameControl(Handle, R, DFC_BUTTON, State);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardElement.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardElement.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

begin
  DrawBoolean(CompareText(GetVisualValue, 'True') = 0, ACanvas, ARect, ASelected);
end;

//----------------- TClipboardFormatsProperty --------------------------------------------------------------------------

function TClipboardFormatsProperty.GetAttributes: TPropertyAttributes;

begin
  Result := inherited GetAttributes + [paSubProperties, paFullWidthName];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardFormatsProperty.GetProperties(Proc: TGetPropEditProc);

var
  List: TStringList;
  I: Integer;
  Tree: TBaseVirtualTree;

begin
  List := TStringList.Create;
  Tree := TClipboardFormats(GetOrdValue).Owner;
  EnumerateVTClipboardFormats(TVirtualTreeClass(Tree.ClassType), List);
  for I := 0 to List.Count - 1 do
    Proc(TClipboardElement.Create(Self, List[I]));
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardFormatsProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

var
  S: string;
  Width: Integer;
  R: TRect;

begin
  with ACanvas do
  begin
    Font.Name := 'Arial';
    R := ARect;
    Font.Color := clBlack;
    S := GetName;
    Width := TextWidth(S);
    TextRect(R, R.Left + 1, R.Top + 1, S);

    Inc(R.Left, Width + 8);
    Font.Height := 14;
    Font.Color := clBtnHighlight;
    S := '(OLE drag and clipboard)';
    SetBkMode(Handle, TRANSPARENT);
    ExtTextOut(Handle, R.Left + 1, R.Top + 1, ETO_CLIPPED, @R, PChar(S), Length(S), nil);
    Font.Color := clBtnShadow;
    ExtTextOut(Handle, R.Left, R.Top, ETO_CLIPPED, @R, PChar(S), Length(S), nil);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TClipboardFormatsProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

begin
  // Nothing to do here.
end;

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterComponents('Virtual Controls', [TVirtualStringTree, TVirtualDrawTree, TVTHeaderPopupMenu]);
  RegisterComponentEditor(TVirtualStringTree, TVirtualTreeEditor);
  RegisterComponentEditor(TVirtualDrawTree, TVirtualTreeEditor);
  RegisterPropertyEditor(TypeInfo(TClipboardFormats), nil, '', TClipboardFormatsProperty);

  // Categories:
  RegisterPropertiesInCategory(sActionCategoryName, TBaseVirtualTree, ['ChangeDelay', 'EditDelay']);

  RegisterPropertiesInCategory(sDataCategoryName,
      TBaseVirtualTree,
      ['NodeDataSize',
       'RootNodeCount',
       'OnCompareNodes',
       'OnGetNodeDataSize',
       'OnInitNode',
       'OnInitChildren',
       'OnFreeNode',
       'OnGetNodeWidth',
       'OnGetPopupMenu',
       'OnLoadNode',
       'OnSaveNode',
       'OnResetNode',
       'OnNodeMov*',
       'OnStructureChange',
       'OnUpdating',
       'OnGetText',
       'OnNewText',
       'OnShortenString']);

    RegisterPropertiesInCategory(slayoutCategoryName,
      TBaseVirtualTree,
      ['AnimationDuration',
       'AutoExpandDelay',
       'AutoScroll*',
       'ButtonStyle',
       'DefaultNodeHeight',
       '*Images*', 'OnGetImageIndex', 'OnGetImageText',
       'Header',
       'Indent',
       'LineStyle', 'OnGetLineStyle',
       'CheckImageKind',
       'Options',
       'Margin',
       'NodeAlignment',
       'ScrollBarOptions',
       'SelectionCurveRadius',
       'TextMargin']);

    RegisterPropertiesInCategory(sVisualCategoryName,
      TBaseVirtualTree,
      ['Background*',
       'ButtonFillMode',
       'CustomCheckimages',
       'Colors',
       'LineMode']);

    RegisterPropertiesInCategory(sHelpCategoryName,
      TBaseVirtualTree,
      ['AccessibleName', 'Hint*', 'On*Hint*', 'On*Help*']);

    RegisterPropertiesInCategory(sDragNDropCategoryName,
      TBaseVirtualTree,
      ['ClipboardFormats',
       'DefaultPasteMode',
       'OnCreateDataObject',
       'OnCreateDragManager',
       'OnGetUserClipboardFormats',
       'OnNodeCop*',
       'OnDragAllowed',
       'OnRenderOLEData']);

    RegisterPropertiesInCategory(sInputCategoryName,
      TBaseVirtualTree,
      ['DefaultText',
       'DrawSelectionMode',
       'WantTabs',
       'OnChang*',
       'OnCollaps*',
       'OnExpand*',
       'OnCheck*',
       'OnEdit*',
       'On*Click',
       'OnFocus*',
       'OnCreateEditor',
       'OnScroll',
       'OnNodeHeightTracking',
       'OnHotChange']);

    RegisterPropertiesInCategory(sVTHeaderCategoryName,
      TBaseVirtualTree,
      ['OnHeader*', 'OnGetHeader*']);

    RegisterPropertiesInCategory(sVTPaintingCategoryName,
      TBaseVirtualTree,
      ['On*Paint*',
       'OnDraw*',
       'On*Erase*']);

    RegisterPropertiesInCategory(sVTIncremenalCategoryName,
      TBaseVirtualTree,
      ['*Incremental*']);
end;

//----------------------------------------------------------------------------------------------------------------------

end.
