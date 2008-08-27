unit VirtualTreesReg;

// This unit is an addendum to VirtualTrees.pas and contains code of design time editors as well as
// for theirs and the tree's registration.

interface

{$include Compilers.inc}

{$ifdef COMPILER_7_UP}
  // For some things to work we need code, which is classified as being unsafe for .NET.
  {$warn UNSAFE_TYPE off}
  {$warn UNSAFE_CAST off}
  {$warn UNSAFE_CODE off}
{$endif COMPILER_7_UP}

{$ifdef COMPILER_4}
  {$R 'VirtualTreesD.dcr'}
{$endif COMPILER_4}

uses
  Windows, Classes,
  {$ifdef COMPILER_6_UP}
    DesignIntf, DesignEditors, VCLEditors, PropertyCategories,
  {$else}
    DsgnIntf,
  {$endif}                       
  ColnEdit,
  VirtualTrees, VTHeaderPopup;

type
  TVirtualTreeEditor = class (TDefaultEditor)
  public
    procedure Edit; override;
  end;

procedure Register;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
  {$ifdef COMPILER_5_UP}
    StrEdit,
  {$else}
    StrEditD4,
  {$endif COMPILER_5_UP}
  Dialogs, TypInfo, SysUtils, Graphics, CommCtrl, ImgList, Controls;

type
  // The usual trick to make a protected property accessible in the ShowCollectionEditor call below.
  TVirtualTreeCast = class(TBaseVirtualTree);

  TClipboardElement = class(TNestedProperty {$ifdef COMPILER_6_UP}, ICustomPropertyDrawing {$endif COMPILER_6_UP})
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

    {$ifdef COMPILER_5_UP}
      {$ifdef COMPILER_6_UP}
        procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
      {$endif COMPILER_6_UP}
      procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
        {$ifndef COMPILER_6_UP} override; {$endif COMPILER_6_UP}
    {$endif COMPILER_5_UP}
  end;

  // This is a special property editor to make the strings in the clipboard format string list
  // being shown as subproperties in the object inspector. This way it is shown what formats are actually available
  // and the user can pick them with a simple yes/no choice.

  {$ifdef COMPILER_6_UP}
    TGetPropEditProc = TGetPropProc;
  {$endif}

  TClipboardFormatsProperty = class(TStringListProperty {$ifdef COMPILER_6_UP}, ICustomPropertyDrawing {$endif COMPILER_6_UP})
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropEditProc); override;
    {$ifdef COMPILER_5_UP}
      procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
        {$ifndef COMPILER_6_UP} override; {$endif}
      procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
        {$ifndef COMPILER_6_UP} override; {$endif}
    {$endif}
  end;

  // Property categories. They are defined this way only for Delphi 5 & BCB 5.
  {$ifdef COMPILER_5}
    TVTHeaderCategory = class(TPropertyCategory)
    public
      class function Name: string; override;
      class function Description: string; override;
    end;

    TVTPaintingCategory = class(TPropertyCategory)
    public
      class function Name: string; override;
      class function Description: string; override;
    end;

    TVTIncrementalSearchCategory = class(TPropertyCategory)
    public
      class function Name: string; override;
      class function Description: string; override;
    end;
  {$endif COMPILER_5}

  TCheckImageKindProperty = class(TEnumProperty {$ifdef COMPILER_6_UP}, ICustomPropertyDrawing, ICustomPropertyListDrawing {$endif COMPILER_6_UP})
  public
    {$ifdef COMPILER_5_UP}
      procedure ListMeasureHeight(const Value: string; Canvas: TCanvas; var AHeight: Integer);
        {$ifndef COMPILER_6_UP} override; {$endif}
      procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
        {$ifndef COMPILER_6_UP} override; {$endif}
      procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
        {$ifndef COMPILER_6_UP} override; {$endif}
      {$ifdef COMPILER_6_UP}
      procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
      {$endif}
      procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
        {$ifndef COMPILER_6_UP} override; {$endif}
    {$endif}
  end;

  {$ifdef COMPILER_6_UP}
    resourcestring
      sVTHeaderCategoryName = 'Header';
      sVTPaintingCategoryName = 'Custom painting';
      sVTIncremenalCategoryName = 'Incremental search';
  {$endif}

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

{$ifdef COMPILER_5_UP}

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

  {$ifdef COMPILER_6_UP}

    procedure TClipboardElement.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    begin
      DefaultPropertyDrawName(Self, ACanvas, ARect);
    end;

  {$endif COMPILER_6_UP}

//----------------------------------------------------------------------------------------------------------------------

  procedure TClipboardElement.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

  begin
    DrawBoolean(CompareText(GetVisualValue, 'True') = 0, ACanvas, ARect, ASelected);
  end;

{$endif COMPILER_5_UP}

//----------------- TClipboardFormatsProperty --------------------------------------------------------------------------

function TClipboardFormatsProperty.GetAttributes: TPropertyAttributes;

begin
  Result := inherited GetAttributes + [paSubProperties {$ifdef COMPILER_5_UP}, paFullWidthName {$endif COMPILER_5_UP}];
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

{$ifdef COMPILER_5_UP}

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

{$endif COMPILER_5_UP}

{$ifdef COMPILER_5}

//----------------- TVTPaintingCategory --------------------------------------------------------------------------------

  class function TVTPaintingCategory.Name: string;

  begin
    Result := 'Custom Painting';
  end;

//----------------------------------------------------------------------------------------------------------------------

  class function TVTPaintingCategory.Description: string;

  begin
    Result := 'Custom Painting';
  end;

//----------------- TVTHeaderCategory ----------------------------------------------------------------------------------

  class function TVTHeaderCategory.Name: string;

  begin
    Result := 'Header';
  end;

//----------------------------------------------------------------------------------------------------------------------

  class function TVTHeaderCategory.Description: string;

  begin
    Result := 'Header';
  end;

//----------------- TVTIncrementalSearchCategory -----------------------------------------------------------------------

  class function TVTIncrementalSearchCategory.Name: string;

  begin
    Result := 'Incremental Search';
  end;

//----------------------------------------------------------------------------------------------------------------------

  class function TVTIncrementalSearchCategory.Description: string;

  begin
    Result := 'Incremental Search';
  end;

//----------------------------------------------------------------------------------------------------------------------

{$endif COMPILER_5}

//----------------- TCheckImageKindProperty ----------------------------------------------------------------------------

{$ifdef COMPILER_5_UP}

  const
    cCheckImageKindComboItemBorder   = 0;
    cCheckImageKindComboItemSpacing  = 2;
    cCheckImageKindComboBitmapHeight = 16;
    cCheckImageKindComboBitmapWidth  = 16;

//----------------------------------------------------------------------------------------------------------------------

  {$ifdef COMPILER_6_UP}

    procedure TCheckImageKindProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    begin
      DefaultPropertyDrawName(Self, ACanvas, ARect);
    end;

  {$endif}

//----------------------------------------------------------------------------------------------------------------------

  procedure TCheckImageKindProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

  begin
    if GetVisualValue <> '' then
      ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected)
    else
      {$ifdef COMPILER_6_UP}
        DefaultPropertyDrawValue(Self, ACanvas, ARect);
      {$else}
        inherited PropDrawValue(ACanvas, ARect, ASelected);
      {$endif}
  end;

//----------------------------------------------------------------------------------------------------------------------

  procedure TCheckImageKindProperty.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

  var
    RighPosition: Integer;
    OldPenColor: TColor;
    CheckKind: TCheckImageKind;
    ImageList: TCustomImageList;
    RemainingRect: TRect;

  begin
    RighPosition := ARect.Left + cCheckImageKindComboBitmapWidth;
    with ACanvas do
    try
      OldPenColor := Pen.Color;
      Pen.Color := Brush.Color;
      Rectangle(ARect.Left, ARect.Top, RighPosition, ARect.Bottom);

      CheckKind := TCheckImageKind(GetEnumValue(GetPropInfo^.PropType^, Value));
      ImageList := TVirtualTreeCast.GetCheckImageListFor(CheckKind);
      if ImageList <> nil then
      begin
        ImageList_DrawEx(ImageList.Handle, ckCheckCheckedNormal, ACanvas.Handle, ARect.Left + cCheckImageKindComboItemBorder,
          ARect.Top + cCheckImageKindComboItemBorder, 0, 0, CLR_NONE, CLR_NONE, ILD_TRANSPARENT);
      end;

      Pen.Color := OldPenColor;
    finally
      RemainingRect := Rect(RighPosition, ARect.Top, ARect.Right, ARect.Bottom);
      {$ifdef COMPILER_6_UP}
        DefaultPropertyListDrawValue(Value, ACanvas, RemainingRect, ASelected);
      {$else}
        inherited ListDrawValue(Value, ACanvas, RemainingRect, ASelected);
      {$endif}
    end;
  end;

//----------------------------------------------------------------------------------------------------------------------

  procedure TCheckImageKindProperty.ListMeasureHeight(const Value: string; Canvas: TCanvas; var AHeight: Integer);

  begin
    AHeight := cCheckImageKindComboBitmapHeight;
  end;

//----------------------------------------------------------------------------------------------------------------------

  procedure TCheckImageKindProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);

  begin
    AWidth := AWidth + cCheckImageKindComboBitmapWidth;
  end;

{$endif COMPILER_5_UP}

//----------------------------------------------------------------------------------------------------------------------

procedure Register;

begin
  RegisterComponents('Virtual Controls', [TVirtualStringTree, TVirtualDrawTree, TVTHeaderPopupMenu]);
  RegisterComponentEditor(TVirtualStringTree, TVirtualTreeEditor);
  RegisterComponentEditor(TVirtualDrawTree, TVirtualTreeEditor);
  RegisterPropertyEditor(TypeInfo(TClipboardFormats), nil, '', TClipboardFormatsProperty);
  RegisterPropertyEditor(TypeInfo(TCheckImageKind), nil, '', TCheckImageKindProperty);  

  // Categories:
  {$ifdef COMPILER_5_UP}
    RegisterPropertiesInCategory({$ifdef COMPILER_5} TActionCategory, {$endif} {$ifdef COMPILER_6_UP} sActionCategoryName, {$endif COMPILER_6_UP}
      TBaseVirtualTree,
      ['ChangeDelay',
       'EditDelay']);

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TDataCategory, {$endif} {$ifdef COMPILER_6_UP} sDataCategoryName, {$endif COMPILER_6_UP}
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

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TLayoutCategory, {$endif} {$ifdef COMPILER_6_UP} slayoutCategoryName, {$endif COMPILER_6_UP}
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

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TVisualCategory, {$endif} {$ifdef COMPILER_6_UP} sVisualCategoryName, {$endif COMPILER_6_UP}
      TBaseVirtualTree,
      ['Background*',
       'ButtonFillMode',
       'CustomCheckimages',
       'Colors',
       'LineMode']);

    RegisterPropertiesInCategory({$ifdef COMPILER_5} THelpCategory, {$endif} {$ifdef COMPILER_6_UP} sHelpCategoryName, {$endif COMPILER_6_UP}
      TBaseVirtualTree,
      ['AccessibleName', 'Hint*', 'On*Hint*', 'On*Help*']);

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TDragNDropCategory, {$endif} {$ifdef COMPILER_6_UP} sDragNDropCategoryName, {$endif COMPILER_6_UP}
      TBaseVirtualTree,
      ['ClipboardFormats',
       'DefaultPasteMode',
       'OnCreateDataObject',
       'OnCreateDragManager',
       'OnGetUserClipboardFormats',
       'OnNodeCop*',
       'OnDragAllowed',
       'OnRenderOLEData']);

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TInputCategory, {$endif} {$ifdef COMPILER_6_UP} sInputCategoryName, {$endif COMPILER_6_UP}
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
       'OnHotChange']);

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TVTHeaderCategory, {$endif} {$ifdef COMPILER_6_UP} sVTHeaderCategoryName, {$endif COMPILER_6_UP}
      TBaseVirtualTree,
      ['OnHeader*', 'OnGetHeader*']);

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TVTPaintingCategory, {$endif} {$ifdef COMPILER_6_UP} sVTPaintingCategoryName, {$endif COMPILER_6_UP}
      TBaseVirtualTree,
      ['On*Paint*',
       'OnDraw*',
       'On*Erase*']);

    RegisterPropertiesInCategory({$ifdef COMPILER_5} TVTIncrementalSearchCategory, {$endif} {$ifdef COMPILER_6_UP} sVTIncremenalCategoryName, {$endif COMPILER_6_UP}
      TBaseVirtualTree,
      ['*Incremental*']);
  {$endif COMPILER_5_UP}
end;

//----------------------------------------------------------------------------------------------------------------------

end.
