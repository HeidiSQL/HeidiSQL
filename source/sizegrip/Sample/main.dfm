object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Flocke'#39's SizeGrip Test'
  ClientHeight = 145
  ClientWidth = 233
  Color = clBtnFace
  Constraints.MinHeight = 172
  Constraints.MinWidth = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 146
    Height = 13
    Caption = 'Look at the lower right corner!'
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 32
    Width = 153
    Height = 17
    Caption = 'Double buffered window'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object RadioButton1: TRadioButton
    Left = 8
    Top = 56
    Width = 113
    Height = 17
    Caption = 'Standard'
    Checked = True
    TabOrder = 2
    TabStop = True
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 8
    Top = 72
    Width = 113
    Height = 17
    Caption = 'Themed'
    TabOrder = 3
    OnClick = RadioButton1Click
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 96
    Width = 73
    Height = 17
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox2Click
  end
  object CheckBox3: TCheckBox
    Left = 8
    Top = 120
    Width = 97
    Height = 17
    Caption = 'New style'
    TabOrder = 4
    OnClick = CheckBox3Click
  end
end
