unit PngComponentEditors;

{$I compilers.inc}

interface

uses
  Windows, SysUtils, Forms, Classes, Controls, PngImageList, TypInfo,
  DesignIntf, DesignEditors, ColnEdit;

type
  TPngImageListEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TPngImageCollectionEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TPngButtonEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TPngImageListImagesEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TPngImageCollectionItemsEditor = class(TPngImageListImagesEditor)
  public
    procedure Edit; override;
  end;

  TEditProperty = class
  private
    FPropery: string;
    procedure EnumProperty(const Prop: IProperty);
  public
    constructor Create(Component: TComponent; const Prop: string; Designer: IDesigner);
  end;

implementation

uses
  PngImageListEditor;

resourcestring
  SEditImage = '&Edit image...';
  SRecreateImages = '&Recreate images...';
  SEditImages = '&Edit images...';
  SEditing = 'Editing %s.%s';
  SPNGObjectsHaveBeenCopied = 'The PNG objects have been copied to the internal imagelist.';

//This type is neccesary to be able to call CopyPngs without having to make it
//public in the TPngImageList class.
type
  TPngImageListAccess = class(TPngImageList);

procedure EditProperty(Component: TComponent; const Prop: string; Designer: IDesigner);
begin
  TEditProperty.Create(Component, Prop, Designer).Free;
end;

{ TPngImageListEditor }

procedure TPngImageListEditor.Edit;
var
  Component: TPngImageList;
begin
  Component := GetComponent as TPngImageList;
  EditProperty(Component, 'PngImages', Designer); // do not localize
end;

procedure TPngImageListEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
    1: begin
        TPngImageListAccess(GetComponent).CopyPngs;
        MessageBox(0, PChar(SPNGObjectsHaveBeenCopied),
          PChar(string(GetComponent.ClassName)), MB_ICONINFORMATION or MB_OK);
      end;
  end;
end;

function TPngImageListEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := SEditImages;
    1: Result := SRecreateImages;
  end;
end;

function TPngImageListEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TPngImageCollectionEditor }

procedure TPngImageCollectionEditor.Edit;
var
  Component: TPngImageCollection;
begin
  Component := GetComponent as TPngImageCollection;
  EditProperty(Component, 'Items', Designer); // do not localize
end;

procedure TPngImageCollectionEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

function TPngImageCollectionEditor.GetVerb(Index: Integer): string;
begin
  Result := SEditImages;
end;

function TPngImageCollectionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TPngButtonEditor.Edit;
begin
  EditProperty(GetComponent, 'OnClick', Designer); // do not localize
end;

{ TPngButtonEditor }

procedure TPngButtonEditor.ExecuteVerb(Index: Integer);
begin
  EditProperty(GetComponent, 'PngImage', Designer); // do not localize
end;

function TPngButtonEditor.GetVerb(Index: Integer): string;
begin
  Result := SEditImage;
end;

function TPngButtonEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TEditProperty }

constructor TEditProperty.Create(Component: TComponent; const Prop: string; Designer: IDesigner);
var
  Components: IDesignerSelections;
begin
  inherited Create;
  FPropery := Prop;
  Components := TDesignerSelections.Create;
  Components.Add(Component);
  GetComponentProperties(Components, tkAny, Designer, EnumProperty);
end;

procedure TEditProperty.EnumProperty(const Prop: IProperty);
begin
  if Prop.GetName = FPropery then
    Prop.Edit;
end;

{ TPngImageListImagesEditor }

procedure TPngImageListImagesEditor.Edit;
var
  ImageList: TPngImageList;
  dlg: TPngImageListEditorDlg;
begin
  dlg := TPngImageListEditorDlg.Create(nil);
  ImageList := GetComponent(0) as TPngImageList;
  dlg.Caption := Format(SEditing, [ImageList.Name, GetName]);
  dlg.Images.Items.Assign(ImageList.PngImages);
  dlg.ImageWidth := ImageList.Width;
  dlg.ImageHeight := ImageList.Height;
  if dlg.ShowModal = mrOK then begin
    ImageList.PngImages.Assign(dlg.Images.Items);
    Designer.Modified;
  end;
end;

function TPngImageListImagesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TPngImageListImagesEditor.GetValue: string;
begin
  Result := '(PNG images)'; // do not localize
end;

{ TPngImageCollectionItemsEditor }

procedure TPngImageCollectionItemsEditor.Edit;
var
  Collection: TPngImageCollection;
  dlg: TPngImageListEditorDlg;
begin
  Collection := GetComponent(0) as TPngImageCollection;
  dlg := TPngImageListEditorDlg.Create(nil);
  dlg.Caption := Format(SEditing, [Collection.Name, GetName]);
  dlg.Images.Items.Assign(Collection.Items);
  if dlg.ShowModal = mrOK then begin
    Collection.Items.Assign(dlg.Images.Items);
    Designer.Modified;
  end;
end;

end.
