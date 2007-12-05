
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntForms_Design;

{$INCLUDE compilers.inc}

interface

uses
  Classes, Windows, DesignIntf, ToolsApi;

type HICON = LongWord;

type
  TTntNewFormWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTAFormWizard
    {$IFDEF COMPILER_6_UP}, IOTARepositoryWizard60{$ENDIF}
    {$IFDEF COMPILER_9_UP}, IOTARepositoryWizard80{$ENDIF})
  protected
    function ThisFormName: WideString;
    function ThisFormClass: TComponentClass; virtual; abstract;
    function ThisFormUnit: WideString; 
  public
    // IOTAWizard
    function GetIDString: AnsiString;
    function GetName: AnsiString; virtual;
    function GetState: TWizardState;
    procedure Execute;
    // IOTARepositoryWizard
    function GetAuthor: AnsiString;
    function GetComment: AnsiString; virtual; abstract;
    function GetPage: AnsiString;
    function GetGlyph: HICON;
    {$IFDEF COMPILER_6_UP}
    // IOTARepositoryWizard60
    function GetDesigner: AnsiString;
    {$ENDIF}
    {$IFDEF COMPILER_9_UP}
    // IOTARepositoryWizard80
    function GetGalleryCategory: IOTAGalleryCategory; 
    function GetPersonality: AnsiString; 
    {$ENDIF}
  end;

procedure Register;

implementation

uses
  TntForms, DesignEditors, WCtlForm, TypInfo, SysUtils;

type
  TTntNewTntFormWizard = class(TTntNewFormWizard)
  protected
    function ThisFormClass: TComponentClass; override;
  public
    function GetName: AnsiString; override;
    function GetComment: AnsiString; override;
  end;

  TTntNewTntFrameWizard = class(TTntNewFormWizard)
  protected
    function ThisFormClass: TComponentClass; override;
  public
    function GetName: AnsiString; override;
    function GetComment: AnsiString; override;
  end;

  TTntFrameCustomModule = class(TWinControlCustomModule)
  public
    function Nestable: Boolean; override;
  end;

  TTntFormCustomModule = class(TCustomModule)
  public
    class function DesignClass: TComponentClass; override;
  end;

procedure Register;
begin
  RegisterCustomModule(TTntFrame, TTntFrameCustomModule);
  RegisterPackageWizard(TTntNewTntFrameWizard.Create);
  //--
  RegisterCustomModule(TTntForm, TTntFormCustomModule);
  //--
  RegisterPackageWizard(TTntNewTntFormWizard.Create);
end;

function GetFirstModuleSupporting(const IID: TGUID): IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  i: integer;
begin
  Result := nil;
  if Assigned(BorlandIDEServices) then
  begin
    // look for the first project
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    for i := 0 to ModuleServices.ModuleCount - 1 do
      if Supports(ModuleServices.Modules[i], IID, Result) then
        Break;
  end;
end;

function MyGetActiveProject: IOTAProject;
{$IFDEF COMPILER_7_UP}
begin
  Result := ToolsAPI.GetActiveProject;
{$ELSE}
var
  ProjectGroup: IOTAProjectGroup;
begin
  ProjectGroup := GetFirstModuleSupporting(IOTAProjectGroup) as IOTAProjectGroup;
  if ProjectGroup = nil then
    Result := nil
  else
    Result := ProjectGroup.ActiveProject;
{$ENDIF}
  if (Result = nil) then
    Result := GetFirstModuleSupporting(IOTAProject) as IOTAProject;
end;

{ TTntNewFormCreator }
type
  TTntNewFormCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FAncestorName: WideString;
    FUnitName: WideString;
  public
    // IOTACreator
    function GetCreatorType: AnsiString;
    function GetExisting: Boolean;
    function GetFileSystem: AnsiString;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    // IOTAModuleCreator
    function GetAncestorName: AnsiString;
    function GetImplFileName: AnsiString;
    function GetIntfFileName: AnsiString;
    function GetFormName: AnsiString;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: AnsiString): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: AnsiString): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: AnsiString): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  public
    constructor Create(const UnitName, AncestorName: WideString);
  end;

  TTntSourceFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: AnsiString;
  public
    function GetSource: AnsiString;
    function GetAge: TDateTime;
    constructor Create(const Source: AnsiString);
  end;

constructor TTntNewFormCreator.Create(const UnitName, AncestorName: WideString);
begin
  inherited Create;
  FUnitName := UnitName;
  FAncestorName := AncestorName;
end;

procedure TTntNewFormCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TTntNewFormCreator.GetAncestorName: AnsiString;
begin
  Result := FAncestorName;
end;

function TTntNewFormCreator.GetCreatorType: AnsiString;
begin
  Result := sForm;
end;

function TTntNewFormCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TTntNewFormCreator.GetFileSystem: AnsiString;
begin
  Result := '';
end;

function TTntNewFormCreator.GetFormName: AnsiString;
begin
  Result := '';
end;

function TTntNewFormCreator.GetImplFileName: AnsiString;
begin
  Result := '';
end;

function TTntNewFormCreator.GetIntfFileName: AnsiString;
begin
  Result := '';
end;

function TTntNewFormCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TTntNewFormCreator.GetOwner: IOTAModule;
begin
  Result := MyGetActiveProject;
end;

function TTntNewFormCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TTntNewFormCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TTntNewFormCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TTntNewFormCreator.NewFormFile(const FormIdent, AncestorIdent: AnsiString): IOTAFile;
begin
  Result := nil;
end;

function TTntNewFormCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: AnsiString): IOTAFile;
const
  cSource =
    'unit %s;' + #13#10 +
    '' + #13#10 +
    'interface' + #13#10 +
    '' + #13#10 +
    'uses' + #13#10 +
    '  Windows, Messages, SysUtils' + {$IFDEF COMPILER_6_UP}', Variants' + {$ENDIF}
    ', Classes, Graphics, Controls, Forms,' + #13#10 + '  Dialogs, %s;' + #13#10 +
    '' + #13#10 +
    'type' + #13#10 +
    '  T%s = class(T%s)' + #13#10 +
    '  private' + #13#10 +
    '    { Private declarations }' + #13#10 +
    '  public' + #13#10 +
    '    { Public declarations }' + #13#10 +
    '  end;' + #13#10 +
    '' + #13#10 +
    'var' + #13#10 +
    '  %s: T%s;' + #13#10 +
    '' + #13#10 +
    'implementation' + #13#10 +
    '' + #13#10 +
    '{$R *.DFM}' + #13#10 +
    '' + #13#10 +
    'end.';
begin
  Result := TTntSourceFile.Create(Format{TNT-ALLOW Format}(cSource,
    [ModuleIdent, FUnitName, FormIdent, AncestorIdent, FormIdent, FormIdent]));
end;

function TTntNewFormCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: AnsiString): IOTAFile;
begin
  Result := nil;
end;

{ TTntNewFormWizard }

function TTntNewFormWizard.ThisFormName: WideString;
begin
  Result := ThisFormClass.ClassName;
  Delete(Result, 1, 1); // drop the 'T'
end;

function TTntNewFormWizard.ThisFormUnit: WideString;
begin
  Result := GetTypeData(ThisFormClass.ClassInfo).UnitName;
end;

function TTntNewFormWizard.GetName: AnsiString;
begin
  Result := ThisFormName;
end;

function TTntNewFormWizard.GetAuthor: AnsiString;
begin
  Result := 'Troy Wolbrink';
end;

function TTntNewFormWizard.GetPage: AnsiString;
begin
  Result := 'New';
end;

function TTntNewFormWizard.GetGlyph: HICON;
begin
  Result := 0;
end;

function TTntNewFormWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TTntNewFormWizard.GetIDString: AnsiString;
begin
  Result := 'Tnt.Create_'+ThisFormName+'.Wizard';
end;

procedure TTntNewFormWizard.Execute;
var
  Module: IOTAModule;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).CreateModule(TTntNewFormCreator.Create(ThisFormUnit, ThisFormName));
end;

{$IFDEF COMPILER_6_UP}
function TTntNewFormWizard.GetDesigner: AnsiString;
begin
  Result := dVCL;
end;
{$ENDIF}

{$IFDEF COMPILER_9_UP}
function TTntNewFormWizard.GetGalleryCategory: IOTAGalleryCategory; 
var
  Manager: IOTAGalleryCategoryManager;
begin
  Result := nil;
  Manager := BorlandIDEServices as IOTAGalleryCategoryManager;
  if Assigned(Manager) then
    Result := Manager.FindCategory(sCategoryDelphiNew);
end;

function TTntNewFormWizard.GetPersonality: AnsiString; 
begin
  Result := sDelphiPersonality;
end;
{$ENDIF}

{ TTntSourceFile }

constructor TTntSourceFile.Create(const Source: AnsiString);
begin
  FSource := Source;
end;

function TTntSourceFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TTntSourceFile.GetSource: AnsiString;
begin
  Result := FSource;
end;

{ TTntNewTntFormWizard }

function TTntNewTntFormWizard.ThisFormClass: TComponentClass;
begin
  Result := TTntForm;
end;

function TTntNewTntFormWizard.GetName: AnsiString;
begin
  Result := ThisFormName + ' (Unicode)'
end;

function TTntNewTntFormWizard.GetComment: AnsiString;
begin
  Result := 'Creates a new Unicode enabled TntForm';
end;

{ TTntNewTntFrameWizard }

function TTntNewTntFrameWizard.ThisFormClass: TComponentClass;
begin
  Result := TTntFrame;
end;

function TTntNewTntFrameWizard.GetName: AnsiString;
begin
  Result := ThisFormName + ' (Unicode)'
end;

function TTntNewTntFrameWizard.GetComment: AnsiString;
begin
  Result := 'Creates a new Unicode enabled TntFrame';
end;

{ TTntFrameCustomModule }

function TTntFrameCustomModule.Nestable: Boolean;
begin
  Result := True;
end;

{ TTntFormCustomModule }

class function TTntFormCustomModule.DesignClass: TComponentClass;
begin
  Result := TTntForm;
end;

end.
