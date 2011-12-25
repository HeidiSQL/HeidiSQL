object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 137
  ClientWidth = 266
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
    266
    137)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 183
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Login'
    Default = True
    ModalResult = 1
    TabOrder = 1
    ExplicitLeft = 168
    ExplicitTop = 84
  end
  object pnlBackground: TPanel
    Left = 0
    Top = 0
    Width = 266
    Height = 98
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlBackground'
    Color = clWhite
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 305
    ExplicitHeight = 114
    DesignSize = (
      266
      98)
    object lblPrompt: TLabel
      Left = 38
      Top = 13
      Width = 44
      Height = 13
      Caption = 'lblPrompt'
    end
    object lblUsername: TLabel
      Left = 38
      Top = 43
      Width = 52
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '&Username:'
      FocusControl = editUsername
    end
    object lblPassword: TLabel
      Left = 38
      Top = 70
      Width = 50
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '&Password:'
      FocusControl = editPassword
    end
    object imgIcon: TImage
      Left = 10
      Top = 13
      Width = 16
      Height = 16
    end
    object editPassword: TEdit
      Left = 104
      Top = 67
      Width = 154
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      PasswordChar = '*'
      TabOrder = 1
      Text = 'editPassword'
    end
    object editUsername: TEdit
      Left = 104
      Top = 40
      Width = 154
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 0
      Text = 'editUsername'
    end
  end
end
