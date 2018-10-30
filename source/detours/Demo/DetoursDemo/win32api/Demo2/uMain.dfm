object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Hooking GetSysColor function'
  ClientHeight = 234
  ClientWidth = 219
  Color = clBtnFace
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
    Left = 16
    Top = 141
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Memo1: TMemo
    Left = 16
    Top = 40
    Width = 185
    Height = 89
    Lines.Strings = (
      'Select me before hooking , and '
      'reselect me after hooking !')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 16
    Top = 13
    Width = 185
    Height = 21
    TabOrder = 1
    Text = 'Select Me..'
  end
  object BtnEnableHook: TButton
    Left = 16
    Top = 169
    Width = 185
    Height = 25
    Caption = 'Enable Hook'
    TabOrder = 2
    OnClick = BtnEnableHookClick
  end
  object BtnDisableHook: TButton
    Left = 16
    Top = 200
    Width = 185
    Height = 25
    Caption = 'Disable Hook'
    TabOrder = 3
    OnClick = BtnDisableHookClick
  end
end
