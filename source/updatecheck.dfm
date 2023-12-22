object frmUpdateCheck: TfrmUpdateCheck
  Left = 0
  Top = 0
  Caption = 'Check for updates ...'
  ClientHeight = 404
  ClientWidth = 360
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    360
    404)
  TextHeight = 14
  object lblStatus: TLabel
    Left = 8
    Top = 364
    Width = 210
    Height = 38
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'lblStatus'
    Layout = tlCenter
    WordWrap = True
  end
  object btnCancel: TButton
    Left = 246
    Top = 371
    Width = 106
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    Default = True
    ModalResult = 2
    TabOrder = 3
  end
  object groupBuild: TGroupBox
    Left = 8
    Top = 192
    Width = 344
    Height = 172
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Nightly build'
    TabOrder = 2
    DesignSize = (
      344
      172)
    object btnBuild: TButton
      Left = 6
      Top = 140
      Width = 331
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Download nightly build'
      ElevationRequired = True
      ModalResult = 1
      TabOrder = 2
      OnClick = btnBuildClick
    end
    object memoBuild: TMemo
      Left = 6
      Top = 16
      Width = 331
      Height = 92
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'memoBuild')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object btnChangelog: TButton
      Left = 6
      Top = 109
      Width = 331
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'View changelog'
      TabOrder = 1
      OnClick = btnChangelogClick
    end
  end
  object groupRelease: TGroupBox
    Left = 8
    Top = 88
    Width = 344
    Height = 98
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Official release'
    Enabled = False
    TabOrder = 1
    DesignSize = (
      344
      98)
    object memoRelease: TMemo
      Left = 6
      Top = 16
      Width = 331
      Height = 53
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      Color = clBtnFace
      Lines.Strings = (
        'memoRelease')
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object LinkLabelRelease: TLinkLabel
      Left = 6
      Top = 75
      Width = 122
      Height = 19
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Caption = 'Download new release'
      PopupMenu = popupDownloadRelease
      TabOrder = 1
      UseVisualStyle = True
      OnLinkClick = LinkLabelReleaseLinkClick
    end
  end
  object btnDonate: TButton
    Left = 8
    Top = 8
    Width = 344
    Height = 74
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Donate'
    CommandLinkHint = 
      'Send an arbitrary amount as donation to the author - per PayPal ' +
      '(also supports credit cards)'
    Style = bsCommandLink
    TabOrder = 0
  end
  object popupDownloadRelease: TPopupMenu
    Images = MainForm.VirtualImageListMain
    Left = 32
    Top = 116
    object CopydownloadURL1: TMenuItem
      Caption = 'Copy to clipboard'
      ImageIndex = 3
      OnClick = CopydownloadURL1Click
    end
  end
end
