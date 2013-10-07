object LoginForm: TLoginForm
  Left = 693
  Top = 343
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 201
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    316
    201)
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 16
    Top = 93
    Width = 66
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = '&User name: '
  end
  object Label2: TLabel
    Left = 16
    Top = 125
    Width = 55
    Height = 14
    Anchors = [akLeft, akBottom]
    Caption = '&Password:'
    FocusControl = Edit2
  end
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 316
    Height = 60
    Align = alTop
  end
  object Label3: TLabel
    Left = 0
    Top = 70
    Width = 4
    Height = 16
    Alignment = taCenter
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 8912896
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Bevel1: TBevel
    Left = 0
    Top = 80
    Width = 321
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object Edit2: TEdit
    Left = 104
    Top = 125
    Width = 193
    Height = 22
    Anchors = [akLeft, akBottom]
    PasswordChar = '*'
    TabOrder = 0
  end
end
