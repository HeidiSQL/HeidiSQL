object FormEditUser: TFormEditUser
  Left = 792
  Top = 134
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'Edit User...'
  ClientHeight = 168
  ClientWidth = 313
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 237
    Top = 143
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 154
    Top = 143
    Width = 75
    Height = 25
    Caption = 'Save'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = Button2Click
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 313
    Height = 137
    Caption = 'Edit users credentials'
    TabOrder = 2
    object Label1: TLabel
      Left = 24
      Top = 32
      Width = 52
      Height = 13
      Caption = 'Username:'
    end
    object Label2: TLabel
      Left = 24
      Top = 56
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object Label3: TLabel
      Left = 24
      Top = 80
      Width = 74
      Height = 13
      Caption = 'New Password:'
    end
    object Label4: TLabel
      Left = 24
      Top = 104
      Width = 88
      Height = 13
      Caption = 'Retype Password:'
    end
    object EditUsername: TEdit
      Left = 125
      Top = 28
      Width = 150
      Height = 21
      TabOrder = 0
    end
    object EditHost: TEdit
      Left = 125
      Top = 52
      Width = 150
      Height = 21
      TabOrder = 1
    end
    object EditPassword1: TEdit
      Left = 125
      Top = 76
      Width = 150
      Height = 21
      PasswordChar = '*'
      TabOrder = 2
    end
    object EditPassword2: TEdit
      Left = 125
      Top = 100
      Width = 150
      Height = 21
      PasswordChar = '*'
      TabOrder = 3
    end
  end
end
