unit PngComponentEditors;

{$I compilers.inc}

interface

uses
  Windows, SysUtils, Forms, Classes, Controls, PngImageList, TypInfo,
  {$IFDEF COMPILER_6_UP} DesignIntf, DesignEditors, ColnEdit {$ELSE} DsgnIntf {$ENDIF};

type
  {$IFNDEF COMPILER_6_UP}
  IProperty = TPropertyEditor;
  IDesignerSelections = TDesignerSelectionList;
  IDesigner = IFormDesigner;

  TThanyComponentEditor = class(TComponentEditor)
  public
    function GetComponent: TComponent;
  end;
  {$ELSE}
  TThanyComponentEditor = TComponentEditor;
  {$ENDIF}

  TPngImageListEditor = class(TThanyComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TPngImageCollectionEditor = class(TThanyComponentEditor)
  public
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TPngButtonEditor = class(TThanyComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TPngImageListImagesEditor = class(TStringProperty)
  public
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TPngImageCollectionItemsEditor = class(TPngImageListImagesEditor)
  public
    procedure Edit; override;
  end;

  TEditProperty = class
  private
    FPropery: string;
    procedure EnumProperty({$IFDEF COMPILER_6_UP}const{$ENDIF} Prop: IProperty);
  public
    constructor Create(Component: TComponent; const Prop: string; Designer: IDesigner);
  end;

implementation

uses PngImageListEditor;

//This type is neccesary to be able to call CopyPngs without having to make it
//public in the TPngImageList class.
type
  TPngImageListAccess = class(TPngImageList)
  public
    procedure CopyPngs; override;
  end;

procedure EditProperty(Component: TComponent; const Prop: string; Designer: IDesigner);
begin
TEditProperty.Create(Component, Prop, Designer).Free;
end;

{ TPngImageListAccess }

procedure TPngImageListAccess.CopyPngs;
begin
inherited CopyPngs;
end;

{ TPngImageListEditor }

procedure TPngImageListEditor.Edit;
var
   Component: TPngImageList;
begin
Component := GetComponent as TPngImageList;
EditProperty(Component, 'PngImages', Designer);
end;

procedure TPngImageListEditor.ExecuteVerb(Index: Integer);
begin
case Index of
     0: Edit;
     1: begin
        TPngImageListAccess(GetComponent).CopyPngs;
        MessageBox(0, 'The PNG objects have been copied to the internal imagelist.', PChar(string(GetComponent.ClassName)), MB_ICONINFORMATION or MB_OK);
        end;
     end;
end;

function TPngImageListEditor.GetVerb(Index: Integer): string;
begin
case Index of
     0: Result := '&Edit images...';
     1: Result := '&Recreate images...';
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
EditProperty(Component, 'Items', Designer);
end;

procedure TPngImageCollectionEditor.ExecuteVerb(Index: Integer);
begin
Edit;
end;

function TPngImageCollectionEditor.GetVerb(Index: Integer): string;
begin
Result := '&Edit images...';
end;

function TPngImageCollectionEditor.GetVerbCount: Integer;
begin
Result := 1;
end;

{ TPngButtonEditor }

procedure TPngButtonEditor.ExecuteVerb(Index: Integer);
begin
EditProperty(GetComponent, 'PngImage', Designer);
end;

function TPngButtonEditor.GetVerb(Index: Integer): string;
begin
Result := '&Edit image...';
end;

function TPngButtonEditor.GetVerbCount: Integer;
begin
Result := 1;
end;

procedure TPngButtonEditor.Edit;
begin
EditProperty(GetComponent, 'OnClick', Designer);
end;

{ TD5ComponentEditor }

{$IFNDEF COMPILER_6_UP}

function TThanyComponentEditor.GetComponent: TComponent;
begin
Result := Component;
end;

{$ENDIF}

{ TEditProperty }

{$IFDEF COMPILER_6_UP}

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

{$ELSE}

constructor TEditProperty.Create(Component: TComponent; const Prop: string; Designer: IDesigner);
var
   Components: TDesignerSelectionList;
begin
inherited Create;
FPropery := Prop;
Components := TDesignerSelectionList.Create;
try
  Components.Add(Component);
  GetComponentProperties(Components, tkAny, Designer, EnumProperty);
finally
  Components.Free;
 end;
end;

{$ENDIF}

procedure TEditProperty.EnumProperty({$IFDEF COMPILER_6_UP}const{$ENDIF} Prop: IProperty);
begin
if Prop.GetName = FPropery
then Prop.Edit;
end;

{ TPngImageListImagesEditor }

procedure TPngImageListImagesEditor.Edit;
var
   ImageList: TPngImageList;
begin
with TPngImageListEditorDlg.Create(nil)
do begin
   ImageList := TPngImageList(Self.GetComponent(0));
   Caption := 'Editing ' + ImageList.Name + '.' + Self.GetName;
   Images.Items.Assign(ImageList.PngImages);
   ImageWidth := ImageList.Width;
   ImageHeight := ImageList.Height;
   if ShowModal = mrOK
   then begin
        ImageList.PngImages.Assign(Images.Items);
        Self.Designer.Modified;
        end;
   end;
end;

function TPngImageListImagesEditor.GetAttributes: TPropertyAttributes;
begin
Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TPngImageListImagesEditor.GetValue: string;
begin
Result := '(PNG images)';
end;

{ TPngImageCollectionItemsEditor }

procedure TPngImageCollectionItemsEditor.Edit;
var
   Collection: TPngImageCollection;
begin
with TPngImageListEditorDlg.Create(nil)
do begin
   Collection := TPngImageCollection(Self.GetComponent(0));
   Caption := 'Editing ' + Collection.Name + '.' + Self.GetName;
   Images.Items.Assign(Collection.Items);
   if ShowModal = mrOK
   then begin
        Collection.Items.Assign(Images.Items);
        Self.Designer.Modified;
        end;
   end;
end;

end.
