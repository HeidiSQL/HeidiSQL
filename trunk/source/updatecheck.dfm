object frmUpdateCheck: TfrmUpdateCheck
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Check for updates ...'
  ClientHeight = 273
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  DesignSize = (
    362
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 8
    Top = 245
    Width = 245
    Height = 13
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'lblStatus'
  end
  object btnCancel: TButton
    Left = 226
    Top = 240
    Width = 128
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 0
  end
  object groupBuild: TGroupBox
    Left = 8
    Top = 123
    Width = 346
    Height = 110
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Nightly build'
    TabOrder = 1
    DesignSize = (
      346
      110)
    object btnBuild: TButton
      Left = 6
      Top = 78
      Width = 333
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Download nightly build'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnBuildClick
    end
    object memoBuild: TMemo
      Left = 6
      Top = 16
      Width = 333
      Height = 60
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'memoBuild')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object groupRelease: TGroupBox
    Left = 8
    Top = 8
    Width = 346
    Height = 110
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Official release'
    TabOrder = 2
    DesignSize = (
      346
      110)
    object btnRelease: TButton
      Left = 6
      Top = 78
      Width = 333
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Download new release'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnReleaseClick
    end
    object memoRelease: TMemo
      Left = 6
      Top = 16
      Width = 333
      Height = 60
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'memoRelease')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
end
