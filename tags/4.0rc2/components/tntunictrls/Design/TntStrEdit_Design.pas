
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntStrEdit_Design;

{$INCLUDE compilers.inc}

// The following unit is adapted from StrEdit.pas.

interface

uses
  Windows, Classes, Graphics, Controls, Buttons, Menus, StdCtrls,
  TntStdCtrls, ExtCtrls, DesignEditors, DesignIntf,
  TntForms, TntMenus, TntClasses, TntDialogs;

type
  TTntStrEditDlg = class(TTntForm)
    CodeWndBtn: TTntButton;
    OpenDialog: TTntOpenDialog;
    SaveDialog: TTntSaveDialog;
    HelpButton: TTntButton;
    OKButton: TTntButton;
    CancelButton: TTntButton;
    StringEditorMenu: TTntPopupMenu;
    LoadItem: TTntMenuItem;
    SaveItem: TTntMenuItem;
    CodeEditorItem: TTntMenuItem;
    TntGroupBox1: TTntGroupBox;
    UnicodeEnabledLbl: TTntLabel;
    Memo: TTntMemo;
    LineCount: TTntLabel;
    procedure FileOpenClick(Sender: TObject);
    procedure FileSaveClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure CodeWndBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UpdateStatus(Sender: TObject);
  private
    SingleLine: WideString;
    MultipleLines: WideString;
  protected
    FModified: Boolean;
    function GetLines: TTntStrings;
    procedure SetLines(const Value: TTntStrings);
    function GetLinesControl: TWinControl;
  public
    property Lines: TTntStrings read GetLines write SetLines;
    procedure PrepareForWideStringEdit;
  end;

type
  TWideStringListProperty = class(TClassProperty)
  protected
    function EditDialog: TTntStrEditDlg; virtual;
    function GetStrings: TTntStrings; virtual;
    procedure SetStrings(const Value: TTntStrings); virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

{$R *.dfm}

uses
  ActiveX, Forms, SysUtils, DesignConst, ToolsAPI, IStreams, LibHelp,
  StFilSys, TypInfo, TntSystem, TntDesignEditors_Design;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TTntStrings), nil, '', TWideStringListProperty);
end;

{$IFDEF COMPILER_10_UP}
type
  TStringsModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FFileName: AnsiString;
    FStream: TStringStream{TNT-ALLOW TStringStream};
    FAge: TDateTime;
  public
    constructor Create(const FileName: AnsiString; Stream: TStringStream{TNT-ALLOW TStringStream}; Age: TDateTime);
    destructor Destroy; override;
    { IOTACreator }
    function GetCreatorType: AnsiString;
    function GetExisting: Boolean;
    function GetFileSystem: AnsiString;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    { IOTAModuleCreator }
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
  end;

  TOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FSource: AnsiString;
    FAge: TDateTime;
  public
    constructor Create(const ASource: AnsiString; AAge: TDateTime);
    { IOTAFile }
    function GetSource: AnsiString;
    function GetAge: TDateTime;
  end;

{ TOTAFile }

constructor TOTAFile.Create(const ASource: AnsiString; AAge: TDateTime);
begin
  inherited Create;
  FSource := ASource;
  FAge := AAge;
end;

function TOTAFile.GetAge: TDateTime;
begin
  Result := FAge;
end;

function TOTAFile.GetSource: AnsiString;
begin
  Result := FSource;
end;

{ TStringsModuleCreator }

constructor TStringsModuleCreator.Create(const FileName: AnsiString; Stream: TStringStream{TNT-ALLOW TStringStream};
  Age: TDateTime);
begin
  inherited Create;
  FFileName := FileName;
  FStream := Stream;
  FAge := Age;
end;

destructor TStringsModuleCreator.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TStringsModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
  { Nothing to do }
end;

function TStringsModuleCreator.GetAncestorName: AnsiString;
begin
  Result := '';
end;

function TStringsModuleCreator.GetCreatorType: AnsiString;
begin
  Result := sText;
end;

function TStringsModuleCreator.GetExisting: Boolean;
begin
  Result := True;
end;

function TStringsModuleCreator.GetFileSystem: AnsiString;
begin
  Result := sTStringsFileSystem;
end;

function TStringsModuleCreator.GetFormName: AnsiString;
begin
  Result := '';
end;

function TStringsModuleCreator.GetImplFileName: AnsiString;
begin
  Result := FFileName;
end;

function TStringsModuleCreator.GetIntfFileName: AnsiString;
begin
  Result := '';
end;

function TStringsModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TStringsModuleCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TStringsModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TStringsModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TStringsModuleCreator.GetUnnamed: Boolean;
begin
  Result := False;
end;

function TStringsModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: AnsiString): IOTAFile;
begin
  Result := nil;
end;

function TStringsModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: AnsiString): IOTAFile;
begin
  Result := TOTAFile.Create(FStream.DataString, FAge);
end;

function TStringsModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: AnsiString): IOTAFile;
begin
  Result := nil;
end;
{$ENDIF}

{ TTntStrEditDlg }

procedure TTntStrEditDlg.FormCreate(Sender: TObject);
begin
  HelpContext := hcDStringListEditor;
  OpenDialog.HelpContext := hcDStringListLoad;
  SaveDialog.HelpContext := hcDStringListSave;
  SingleLine := srLine;
  MultipleLines := srLines;
  UnicodeEnabledLbl.Visible := IsWindowUnicode(Memo.Handle);
end;

procedure TTntStrEditDlg.PrepareForWideStringEdit;
begin
  Caption := 'WideString Editor';
  CodeWndBtn.Visible := False;
  CodeEditorItem.Visible := False;
end;

procedure TTntStrEditDlg.FileOpenClick(Sender: TObject);
begin
  with OpenDialog do
    if Execute then Lines.LoadFromFile(FileName);
end;

procedure TTntStrEditDlg.FileSaveClick(Sender: TObject);
begin
  SaveDialog.FileName := OpenDialog.FileName;
  with SaveDialog do
    if Execute then Lines.SaveToFile(FileName);
end;

procedure TTntStrEditDlg.HelpButtonClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TTntStrEditDlg.CodeWndBtnClick(Sender: TObject);
begin
  ModalResult := mrYes;
end;

function TTntStrEditDlg.GetLinesControl: TWinControl;
begin
  Result := Memo;
end;

procedure TTntStrEditDlg.Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then CancelButton.Click;
end;

procedure TTntStrEditDlg.UpdateStatus(Sender: TObject);
var
  Count: Integer;
  LineText: WideString;
begin
  if Sender = Memo then FModified := True;
  Count := Lines.Count;
  if Count = 1 then LineText := SingleLine
  else LineText := MultipleLines;
  LineCount.Caption := WideFormat('%d %s', [Count, LineText]);
end;

function TTntStrEditDlg.GetLines: TTntStrings;
begin
  Result := Memo.Lines;
end;

procedure TTntStrEditDlg.SetLines(const Value: TTntStrings);
begin
  Memo.Lines.Assign(Value);
end;

{ TWideStringListProperty }

function TWideStringListProperty.EditDialog: TTntStrEditDlg;
begin
  Result := TTntStrEditDlg.Create(Application);
end;

function TWideStringListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

function TWideStringListProperty.GetStrings: TTntStrings;
begin
  Result := TTntStrings(GetOrdValue);
end;

procedure TWideStringListProperty.SetStrings(const Value: TTntStrings);
begin
  SetOrdValue(Longint(Value));
end;

procedure TWideStringListProperty.Edit;
{$IFDEF COMPILER_10_UP}
const
  DotSep = '.'; // Temp fix for opening the strings in the editor.
var
  Ident: AnsiString;
  Component: TComponent;
  Module: IOTAModule;
  Editor: IOTAEditor;
  ModuleServices: IOTAModuleServices;
  Stream: TStringStream{TNT-ALLOW TStringStream};
  Age: TDateTime;
{$ENDIF}
begin
  {$IFDEF COMPILER_10_UP}
  Component := TComponent(GetComponent(0));
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if (TObject(Component) is TComponent)
  and (Component.Owner = Self.Designer.GetRoot)
  and (Self.Designer.GetRoot.Name <> '')
  then begin
    Ident := Self.Designer.GetRoot.Name + DotSep +
      Component.Name + DotSep + GetName;
    Ident := Self.Designer.GetDesignerExtension + DotSep + Ident;
    Module := ModuleServices.FindModule(Ident);
  end else begin
    Ident := '';
    Module := nil;
  end;
  if (Module <> nil) and (Module.GetModuleFileCount > 0) then
    Module.GetModuleFileEditor(0).Show
  else
  {$ENDIF}
  with EditDialog do
  try
    if GetObjectInspectorForm <> nil then
      Font.Assign(GetObjectInspectorForm.Font);
    Lines := GetStrings;
    UpdateStatus(nil);
    FModified := False;
    ActiveControl := GetLinesControl;
    {$IFDEF COMPILER_10_UP}
    CodeEditorItem.Enabled := Ident <> '';
    CodeWndBtn.Enabled := Ident <> '';
    {$ENDIF}
    case ShowModal of
      mrOk: SetStrings(Lines);
      {$IFDEF COMPILER_10_UP}
      mrYes:
        begin
          // this used to be done in LibMain's TLibrary.Create but now its done here
          //  the unregister is done over in ComponentDesigner's finalization
          //StFilSys.Register;
          Stream := TStringStream{TNT-ALLOW TStringStream}.Create(WideStringToUTF8(Lines.Text));
          Stream.Position := 0;
          Age := Now;
          Module := ModuleServices.CreateModule(
            TStringsModuleCreator.Create(Ident, Stream, Age));
          if Module <> nil then
          begin
            with StringsFileSystem.GetTStringsProperty(Ident, Component, GetName) do
              DiskAge := DateTimeToFileDate(Age);
            Editor := Module.GetModuleFileEditor(0);
            if FModified then
              Editor.MarkModified;
            Editor.Show;
          end;
        end;
      {$ENDIF}
    end;
  finally
    Free;
  end;
end;

end.
