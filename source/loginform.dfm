object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 117
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    251
    117)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    Left = 8
    Top = 8
    Width = 44
    Height = 13
    Caption = 'lblPrompt'
  end
  object lblUsername: TLabel
    Left = 8
    Top = 33
    Width = 52
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Username:'
    FocusControl = editUsername
  end
  object lblPassword: TLabel
    Left = 8
    Top = 60
    Width = 50
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '&Password:'
    FocusControl = editPassword
  end
  object btnOK: TButton
    Left = 168
    Top = 84
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Login'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object editPassword: TEdit
    Left = 72
    Top = 57
    Width = 171
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    PasswordChar = '*'
    TabOrder = 1
    Text = 'editPassword'
  end
  object editUsername: TEdit
    Left = 72
    Top = 30
    Width = 171
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 0
    Text = 'editUsername'
  end
end
