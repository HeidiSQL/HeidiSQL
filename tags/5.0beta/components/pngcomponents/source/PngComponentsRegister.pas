unit PngComponentsRegister;

{$I compilers.inc}

interface

uses
  Classes, DesignIntf, TypInfo,
  PngSpeedButton, PngBitBtn, PngImageList, PngCheckListBox, PngComponentEditors;

procedure Register;

implementation

const
  SPageName = 'Png';

procedure Register;
begin
  //Register all components
  RegisterComponents(SPageName, [TPngSpeedButton, TPngBitBtn, TPngImageList,
    TPngImageCollection, TPngCheckListBox]);

  //Register component editors
  RegisterComponentEditor(TPngImageList, TPngImageListEditor);
  RegisterComponentEditor(TPngImageCollection, TPngImageCollectionEditor);
  RegisterComponentEditor(TPngBitBtn, TPngButtonEditor);
  RegisterComponentEditor(TPngSpeedButton, TPngButtonEditor);

  //Register property editors
  RegisterPropertyEditor(TypeInfo(TPngImageCollectionItems), TPngImageList,
    'PngImages', TPngImageListImagesEditor); // do not localize
  RegisterPropertyEditor(TypeInfo(TPngImageCollectionItems), TPngImageCollection,
    'Items', TPngImageCollectionItemsEditor); // do not localize

  //Hide properties that should be omitted
  UnlistPublishedProperty(TPngSpeedButton, 'NumGlyphs'); // do not localize
  UnlistPublishedProperty(TPngSpeedButton, 'Glyph'); // do not localize
  UnlistPublishedProperty(TPngBitBtn, 'NumGlyphs'); // do not localize
  UnlistPublishedProperty(TPngBitBtn, 'Glyph'); // do not localize
end;

end.
