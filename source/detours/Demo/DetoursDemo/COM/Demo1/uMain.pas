unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, DDetours;

type
  TMain = class(TForm)
    BtnOpenDialog: TButton;
    MemLog: TMemo;
    BtnEnableHook: TButton;
    BtnDisableHook: TButton;
    procedure BtnOpenDialogClick(Sender: TObject);
    procedure BtnEnableHookClick(Sender: TObject);
    procedure BtnDisableHookClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses ShlObj, ComObj;
{$R *.dfm}

var
  Trampoline_FileOpenDialog_Show: function(const Self; hwndParent: HWND): HRESULT; stdcall;
  Trampoline_FileOpenDialog_SetTitle: function(const Self; pszTitle: LPCWSTR): HRESULT; stdcall;

function FileOpenDialog_SetTitle_Hook(const Self; pszTitle: LPCWSTR): HRESULT; stdcall;
begin
  Result := Trampoline_FileOpenDialog_SetTitle(Self, 'Hooked');
end;

function FileOpenDialog_Show_Hook(const Self; hwndParent: HWND): HRESULT; stdcall;
begin
  Main.MemLog.Lines.Add('Execution FileOpenDialog.Show ..');
  Result := Trampoline_FileOpenDialog_Show(Self, hwndParent);
end;

var
  FileOpenDialog: IFileOpenDialog;

procedure TMain.BtnOpenDialogClick(Sender: TObject);
begin
  MemLog.Clear;
  FileOpenDialog.SetTitle('Open..');
  FileOpenDialog.Show(Handle);
end;

procedure TMain.BtnEnableHookClick(Sender: TObject);
begin
  if not Assigned(Trampoline_FileOpenDialog_Show) then
    @Trampoline_FileOpenDialog_Show := InterceptCreate(FileOpenDialog, 3, @FileOpenDialog_Show_Hook);
  Trampoline_FileOpenDialog_SetTitle := InterceptCreate(FileOpenDialog, 17, @FileOpenDialog_SetTitle_Hook);
end;

procedure TMain.BtnDisableHookClick(Sender: TObject);
begin
  if Assigned(Trampoline_FileOpenDialog_Show) then
  begin
    InterceptRemove(@Trampoline_FileOpenDialog_Show);
    Trampoline_FileOpenDialog_Show := nil;
  end;
  InterceptRemove(@Trampoline_FileOpenDialog_SetTitle)
end;

initialization

FileOpenDialog := CreateComObject(CLSID_FileOpenDialog) as IFileOpenDialog;

end.
