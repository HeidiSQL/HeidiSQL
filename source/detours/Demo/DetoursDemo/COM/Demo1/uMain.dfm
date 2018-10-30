object Main: TMain
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Hooking IFileOpenDialog.Show Method'
  ClientHeight = 196
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOpenDialog: TButton
    Left = 304
    Top = 97
    Width = 121
    Height = 25
    Caption = 'Open File Dialog'
    TabOrder = 0
    OnClick = BtnOpenDialogClick
  end
  object MemLog: TMemo
    Left = 0
    Top = 0
    Width = 447
    Height = 81
    Align = alTop
    Lines.Strings = (
      'MemLog')
    TabOrder = 1
  end
  object BtnEnableHook: TButton
    Left = 304
    Top = 128
    Width = 121
    Height = 25
    Caption = 'Enable Hook'
    TabOrder = 2
    OnClick = BtnEnableHookClick
  end
  object BtnDisableHook: TButton
    Left = 304
    Top = 159
    Width = 121
    Height = 25
    Caption = 'Disable Hook'
    TabOrder = 3
    OnClick = BtnDisableHookClick
  end
end
