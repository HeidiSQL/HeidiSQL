object frmSQLhelp: TfrmSQLhelp
  Left = 0
  Top = 0
  Caption = 'Integrated SQL-help'
  ClientHeight = 327
  ClientWidth = 534
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 121
    Top = 0
    Height = 308
    Cursor = crSizeWE
    ResizeStyle = rsUpdate
    ExplicitLeft = 208
    ExplicitTop = 104
    ExplicitHeight = 100
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 121
    Height = 308
    Align = alLeft
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 0
    DesignSize = (
      121
      308)
    object treevwTopics: TTreeView
      Left = 5
      Top = 47
      Width = 111
      Height = 256
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Indent = 19
      TabOrder = 0
    end
    object editSearch: TEdit
      Left = 5
      Top = 19
      Width = 62
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'editSearch'
      OnKeyDown = memosKeyDown
    end
    object btnSearch: TButton
      Left = 71
      Top = 19
      Width = 44
      Height = 21
      Anchors = [akTop, akRight]
      Caption = 'Search'
      TabOrder = 2
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 308
    Width = 534
    Height = 19
    Panels = <>
  end
  object pnlRight: TPanel
    Left = 124
    Top = 0
    Width = 410
    Height = 308
    Align = alClient
    BevelOuter = bvNone
    Padding.Left = 5
    Padding.Top = 5
    Padding.Right = 5
    Padding.Bottom = 5
    TabOrder = 2
    DesignSize = (
      410
      308)
    object lblKeyword: TLabel
      Left = 6
      Top = 8
      Width = 71
      Height = 16
      Caption = 'lblKeyword'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblDescription: TLabel
      Left = 6
      Top = 27
      Width = 57
      Height = 13
      Caption = 'Description:'
    end
    object lblExample: TLabel
      Left = 6
      Top = 172
      Width = 44
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Example:'
      ExplicitTop = 117
    end
    object memoDescription: TMemo
      Left = 6
      Top = 46
      Width = 400
      Height = 120
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'memoDescription')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      OnKeyDown = memosKeyDown
    end
    object MemoExample: TMemo
      Left = 6
      Top = 191
      Width = 400
      Height = 75
      Anchors = [akLeft, akRight, akBottom]
      Lines.Strings = (
        'MemoExample')
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 1
      OnKeyDown = memosKeyDown
    end
    object ButtonClose: TButton
      Left = 311
      Top = 277
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      Default = True
      TabOrder = 2
      OnClick = ButtonCloseClick
    end
    object ButtonOnlinehelp: TButton
      Left = 210
      Top = 277
      Width = 95
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Search Online'
      TabOrder = 3
      OnClick = ButtonOnlinehelpClick
    end
  end
end
