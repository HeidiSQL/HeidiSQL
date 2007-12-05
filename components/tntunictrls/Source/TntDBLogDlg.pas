
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntDBLogDlg;

{$INCLUDE compilers.inc}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics,  
  TntForms, TntStdCtrls, TntExtCtrls, StdCtrls, ExtCtrls, Controls;

type
  TTntLoginDialog = class(TTntForm)
    Panel: TTntPanel;
    Bevel: TTntBevel;
    DatabaseName: TTntLabel;
    OKButton: TTntButton;
    CancelButton: TTntButton;
    Panel1: TTntPanel;
    Label1: TTntLabel;
    Label2: TTntLabel;
    Label3: TTntLabel;
    Password: TTntEdit;
    UserName: TTntEdit;
    procedure FormShow(Sender: TObject);
  end;

{TNT-WARN LoginDialog}
function TntLoginDialog(const ADatabaseName: WideString;
  var AUserName, APassword: WideString): Boolean;

{TNT-WARN LoginDialogEx}
function TntLoginDialogEx(const ADatabaseName: WideString;
  var AUserName, APassword: WideString; NameReadOnly: Boolean): Boolean;

{TNT-WARN RemoteLoginDialog}
function TntRemoteLoginDialog(var AUserName, APassword: WideString): Boolean;

implementation

{$R *.dfm}

uses
  Forms, VDBConsts;

function TntLoginDialog(const ADatabaseName: WideString;
  var AUserName, APassword: WideString): Boolean;
begin
  with TTntLoginDialog.Create(Application) do
  try
    DatabaseName.Caption := ADatabaseName;
    UserName.Text := AUserName;
    Result := False;
    if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := UserName.Text;
      APassword := Password.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TntLoginDialogEx(const ADatabaseName: WideString;
  var AUserName, APassword: WideString; NameReadOnly: Boolean): Boolean;
begin
  with TTntLoginDialog.Create(Application) do
  try
    DatabaseName.Caption := ADatabaseName;
    UserName.Text := AUserName;
    Result := False;
    if NameReadOnly then
      UserName.Enabled := False
    else
      if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := UserName.Text;
      APassword := Password.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

function TntRemoteLoginDialog(var AUserName, APassword: WideString): Boolean;
begin
  with TTntLoginDialog.Create(Application) do
  try
    Caption := SRemoteLogin;
    Bevel.Visible := False;
    DatabaseName.Visible := False;
    Label3.Visible := False;
    Panel.Height := Panel.Height - Bevel.Top;
    OKButton.Top := OKButton.Top - Bevel.Top;
    CancelButton.Top := CancelButton.Top - Bevel.Top;
    Height := Height - Bevel.Top;
    UserName.Text := AUserName;
    Result := False;
    if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := UserName.Text;
      APassword := Password.Text;
      Result := True;
    end;
  finally
    Free;
  end;
end;

{ TTntLoginDialog }

procedure TTntLoginDialog.FormShow(Sender: TObject);
begin
  if (DatabaseName.Width + DatabaseName.Left) >= Panel.ClientWidth then
    DatabaseName.Width := (Panel.ClientWidth - DatabaseName.Left) - 5;
end;

end.
