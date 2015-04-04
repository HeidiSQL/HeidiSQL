object frmEditVariable: TfrmEditVariable
  Left = 0
  Top = 0
  Caption = 'Edit server variable'
  ClientHeight = 222
  ClientWidth = 291
  Color = clBtnFace
  Constraints.MinHeight = 260
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    291
    222)
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 127
    Top = 189
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 208
    Top = 189
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object grpScope: TGroupBox
    Left = 8
    Top = 142
    Width = 275
    Height = 41
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Scope'
    TabOrder = 2
    object radioScopeSession: TRadioButton
      Left = 16
      Top = 14
      Width = 105
      Height = 17
      Caption = 'This session'
      TabOrder = 0
    end
    object radioScopeGlobal: TRadioButton
      Left = 122
      Top = 14
      Width = 113
      Height = 17
      Caption = 'Global'
      TabOrder = 1
    end
  end
  object gbValue: TGroupBox
    Left = 8
    Top = 8
    Width = 275
    Height = 128
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'name of variable'
    TabOrder = 3
    DesignSize = (
      275
      128)
    object lblString: TLabel
      Left = 8
      Top = 26
      Width = 32
      Height = 13
      Caption = 'String:'
    end
    object lblNumber: TLabel
      Left = 8
      Top = 53
      Width = 41
      Height = 13
      Caption = 'Number:'
    end
    object lblEnum: TLabel
      Left = 8
      Top = 80
      Width = 64
      Height = 13
      Caption = 'Enumeration:'
    end
    object lblBoolean: TLabel
      Left = 8
      Top = 105
      Width = 42
      Height = 13
      Caption = 'Boolean:'
    end
    object radioBooleanOn: TRadioButton
      Left = 88
      Top = 104
      Width = 82
      Height = 17
      Caption = 'On'
      TabOrder = 0
    end
    object radioBooleanOff: TRadioButton
      Left = 176
      Top = 104
      Width = 91
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Off'
      TabOrder = 1
    end
    object comboEnum: TComboBox
      Left = 88
      Top = 77
      Width = 180
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
    end
    object editNumber: TEdit
      Left = 88
      Top = 50
      Width = 163
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 3
      Text = '0'
    end
    object UpDownNumber: TUpDown
      Left = 251
      Top = 50
      Width = 16
      Height = 21
      Anchors = [akTop, akRight]
      Associate = editNumber
      Min = -2147483648
      Max = 2147483647
      TabOrder = 4
    end
    object editString: TEdit
      Left = 88
      Top = 23
      Width = 180
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
    end
  end
  object btnHelp: TButton
    Left = 46
    Top = 189
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 4
    OnClick = btnHelpClick
  end
end
