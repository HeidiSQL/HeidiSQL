object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 176
  ClientWidth = 270
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
    270
    176)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 164
    Top = 143
    Width = 98
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Login'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object pnlBackground: TPanel
    Left = 0
    Top = 0
    Width = 270
    Height = 137
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pnlBackground'
    Color = clWhite
    ParentBackground = False
    ShowCaption = False
    TabOrder = 0
    DesignSize = (
      270
      137)
    object lblPrompt: TLabel
      Left = 38
      Top = 13
      Width = 44
      Height = 13
      Caption = 'lblPrompt'
    end
    object lblUsername: TLabel
      Left = 38
      Top = 44
      Width = 52
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = '&Username:'
      FocusControl = editUsername
    end
    object lblPassword: TLabel
      Left = 38
      Top = 90
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
      Left = 38
      Top = 109
      Width = 224
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      PasswordChar = '*'
      TabOrder = 1
      Text = 'editPassword'
    end
    object editUsername: TEdit
      Left = 38
      Top = 63
      Width = 224
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 0
      Text = 'editUsername'
    end
  end
end
