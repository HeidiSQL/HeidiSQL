unit PngComponentsRegister;

{$I compilers.inc}

interface

uses
  Classes, {$IFDEF COMPILER_6_UP} DesignIntf {$ELSE} DsgnIntf {$ENDIF},
  PngSpeedButton, PngBitBtn, PngImageList, PngCheckListBox, PngComponentEditors,
  TypInfo;

procedure Register;

implementation

{$IFNDEF COMPILER_7_UP}

procedure UnlistPublishedProperty(ComponentClass: TClass; const PropertyName: string);
var
   LPropInfo: PPropInfo;
begin
LPropInfo := GetPropInfo(ComponentClass, PropertyName);
if LPropInfo <> nil
then RegisterPropertyEditor(LPropInfo^.PropType^, ComponentClass, PropertyName, nil);
end;

{$ENDIF}

procedure Register;
begin
//Register all components
RegisterComponents('Png', [TPngSpeedButton, TPngBitBtn, TPngImageList, TPngImageCollection, TPngCheckListBox]);

//Register component editors
RegisterComponentEditor(TPngImageList, TPngImageListEditor);
RegisterComponentEditor(TPngImageCollection, TPngImageCollectionEditor);
RegisterComponentEditor(TPngBitBtn, TPngButtonEditor);
RegisterComponentEditor(TPngSpeedButton, TPngButtonEditor);

//Register property editors
RegisterPropertyEditor(TypeInfo(TPngImageCollectionItems), TPngImageList, 'PngImages', TPngImageListImagesEditor);
RegisterPropertyEditor(TypeInfo(TPngImageCollectionItems), TPngImageCollection, 'Items', TPngImageCollectionItemsEditor);

//Hide properties that should be omitted
UnlistPublishedProperty(TPngSpeedButton, 'NumGlyphs');
UnlistPublishedProperty(TPngSpeedButton, 'Glyph');
UnlistPublishedProperty(TPngBitBtn, 'NumGlyphs');
UnlistPublishedProperty(TPngBitBtn, 'Glyph');
end;

end.
