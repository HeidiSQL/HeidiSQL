object RunSQLFileForm: TRunSQLFileForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Run SQL file'
  ClientHeight = 254
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  DesignSize = (
    394
    254)
  PixelsPerInch = 96
  TextHeight = 13
  object lblFilenameValue: TLabel
    Left = 104
    Top = 17
    Width = 272
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'lblFilenameValue'
  end
  object lblFilenameName: TLabel
    Left = 16
    Top = 17
    Width = 60
    Height = 13
    Caption = 'Running file:'
  end
  object lblQueryName: TLabel
    Left = 16
    Top = 117
    Width = 55
    Height = 13
    Caption = 'Last query:'
  end
  object lblPositionName: TLabel
    Left = 16
    Top = 97
    Width = 69
    Height = 13
    Caption = 'Position in file:'
  end
  object lblPositionValue: TLabel
    Left = 104
    Top = 97
    Width = 73
    Height = 13
    Caption = 'lblPositionValue'
  end
  object lblQueryCountName: TLabel
    Left = 16
    Top = 37
    Width = 41
    Height = 13
    Caption = 'Queries:'
  end
  object lblQueryCountValue: TLabel
    Left = 104
    Top = 37
    Width = 6
    Height = 13
    Caption = '0'
  end
  object lblTimeName: TLabel
    Left = 16
    Top = 77
    Width = 66
    Height = 13
    Caption = 'Time elapsed:'
  end
  object lblTimeValue: TLabel
    Left = 104
    Top = 77
    Width = 58
    Height = 13
    Caption = 'lblTimeValue'
  end
  object lblAffectedRowsName: TLabel
    Left = 16
    Top = 57
    Width = 72
    Height = 13
    Caption = 'Affected rows:'
  end
  object lblAffectedRowsValue: TLabel
    Left = 104
    Top = 57
    Width = 104
    Height = 13
    Caption = 'lblAffectedRowsValue'
  end
  object btnClose: TButton
    Left = 162
    Top = 221
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
  end
  object prbarRun: TProgressBar
    Left = 16
    Top = 192
    Width = 360
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Step = 1
    TabOrder = 1
  end
  object memoQueryValue: TMemo
    Left = 104
    Top = 117
    Width = 272
    Height = 56
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'memoQueryValue')
    ReadOnly = True
    TabOrder = 2
  end
end
