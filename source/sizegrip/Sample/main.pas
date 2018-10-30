{
  main.pas

  This file is part of the SizeGrip.pas sample application.
  Info at http://flocke.vssd.de/prog/code/pascal/sizegrip/

  Copyright (C) 2005, 2006 Volker Siebert <flocke@vssd.de>
  All rights reserved.
}

unit main;

interface

// If you define the following symbol (remove the slashes) you will
// test the nonVCL version, otherwise the VCL based version.
//{$DEFINE USE_NONVCL}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
{$IFDEF USE_NONVCL}
  SizeGripHWND;
{$ELSE}
  SizeGrip, SizeGripThemed;
{$ENDIF}

type
  TForm1 = class(TForm)
    Label1: TLabel;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
{$IFNDEF USE_NONVCL}
    FGripper: TSizeGripThemed;
{$ENDIF}
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{$IFNDEF USE_NONVCL}
const
  CNewStyle: array [boolean] of TSizeGripStyle = ( sgsClassic, sgsWinXP );
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  
  CheckBox1.Checked := DoubleBuffered;

{$IFNDEF USE_NONVCL}
  FGripper := TSizeGripThemed.Create(Self);
  FGripper.Themed := RadioButton2.Checked;
  FGripper.Enabled := CheckBox2.Checked;
  FGripper.Style := CNewStyle[CheckBox3.Checked];
{$ELSE}
  SetWindowSizeGrip(Handle, true);
  RadioButton2.Enabled := false;
  CheckBox3.Enabled := false;
{$ENDIF}
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  DoubleBuffered := CheckBox1.Checked;
  Invalidate;
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
{$IFNDEF USE_NONVCL}
  FGripper.Themed := RadioButton2.Checked;
{$ENDIF}
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
{$IFNDEF USE_NONVCL}
  FGripper.Enabled := CheckBox2.Checked;
{$ELSE}
  SetWindowSizeGrip(Handle, CheckBox2.Checked);
{$ENDIF}
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
{$IFNDEF USE_NONVCL}
  FGripper.Style := CNewStyle[CheckBox3.Checked];
{$ENDIF}
end;

end.
