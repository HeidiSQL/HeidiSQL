object frmSQLhelp: TfrmSQLhelp
  Left = 0
  Top = 0
  Caption = 'Integrated SQL-help'
  ClientHeight = 355
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 153
    Top = 0
    Width = 8
    Height = 336
    Cursor = crSizeWE
    ResizeStyle = rsUpdate
    ExplicitLeft = 121
    ExplicitHeight = 308
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 153
    Height = 336
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object lblTopics: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 7
      Width = 147
      Height = 14
      Align = alBottom
      Caption = 'Topics:'
      ExplicitTop = 4
    end
    object treeTopics: TTreeView
      Left = 0
      Top = 24
      Width = 153
      Height = 312
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Images = MainForm.ImageList1
      Indent = 19
      ReadOnly = True
      ShowLines = False
      TabOrder = 0
      OnChange = treeTopicsChange
      OnExpanding = treeTopicsExpanding
      ExplicitTop = 29
      ExplicitWidth = 148
      ExplicitHeight = 302
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 336
    Width = 582
    Height = 19
    Panels = <>
  end
  object pnlRight: TPanel
    Left = 161
    Top = 0
    Width = 421
    Height = 336
    Align = alClient
    BevelOuter = bvNone
    Padding.Top = 3
    Padding.Right = 3
    Padding.Bottom = 3
    TabOrder = 2
    object Splitter2: TSplitter
      Left = 0
      Top = 171
      Width = 418
      Height = 8
      Cursor = crSizeNS
      Align = alTop
      ResizeStyle = rsUpdate
      ExplicitLeft = 5
      ExplicitTop = 209
      ExplicitWidth = 471
    end
    object pnlRightTop: TPanel
      Left = 0
      Top = 3
      Width = 418
      Height = 168
      Align = alTop
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 5
      ExplicitTop = 5
      ExplicitWidth = 411
      ExplicitHeight = 164
      object lblKeyword: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 5
        Width = 418
        Height = 16
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'lblKeyword'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 71
      end
      object lblDescription: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 26
        Width = 418
        Height = 13
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Description:'
        ExplicitWidth = 57
      end
      object memoDescription: TMemo
        AlignWithMargins = True
        Left = 0
        Top = 44
        Width = 418
        Height = 124
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alClient
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
        ExplicitLeft = -2
        ExplicitWidth = 405
        ExplicitHeight = 120
      end
    end
    object pnlRightBottom: TPanel
      Left = 0
      Top = 179
      Width = 418
      Height = 154
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 5
      ExplicitTop = 177
      ExplicitWidth = 411
      DesignSize = (
        418
        154)
      object lblExample: TLabel
        AlignWithMargins = True
        Left = 0
        Top = 5
        Width = 418
        Height = 13
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Example:'
        ExplicitWidth = 44
      end
      object MemoExample: TMemo
        AlignWithMargins = True
        Left = 0
        Top = 23
        Width = 418
        Height = 90
        Margins.Left = 0
        Margins.Top = 5
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Anchors = [akLeft, akTop, akRight, akBottom]
        Lines.Strings = (
          'MemoExample')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        OnKeyDown = memosKeyDown
        ExplicitWidth = 411
      end
      object ButtonClose: TButton
        Left = 316
        Top = 128
        Width = 102
        Height = 25
        Anchors = [akRight, akBottom]
        Cancel = True
        Caption = 'Close'
        Default = True
        TabOrder = 1
        OnClick = ButtonCloseClick
        ExplicitLeft = 309
      end
      object ButtonSearchOnline: TBitBtn
        Left = 207
        Top = 128
        Width = 103
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Search Online'
        TabOrder = 2
        OnClick = ButtonOnlinehelpClick
        Glyph.Data = {
          36050000424D3605000000000000360400002800000010000000100000000100
          0800000000000001000000000000000000000001000000010000299C000031B5
          000052BD08004A7B10003194100042A510004AA510004AAD180039BD180042C6
          1800845A2100736321006B8421005A9C21004AA5210052BD210052C62100B531
          2900A5392900AD392900B5392900AD4229007352290094522900A55A2900AD5A
          29007B6329008C63290094632900BD6B2900638C290042942900529C29005ACE
          2900A5523100AD523100B55231009C5A3100B55A3100A563310073843100D68C
          310063A5310084A5310010BD310063CE31006BCE31004AD6310052D63100AD52
          3900BD5A3900C65A390094633900A5633900AD633900B56339008C6B3900BD6B
          3900AD733900BD733900D6A53900DEA53900DEAD3900E7AD390063CE39006BCE
          390031D639006BD63900C66B4200BD734200C6734200CE734200CE7B4200BD8C
          4200D694420021D64200AD5A4A00AD6B4A00C66B4A00947B4A00C67B4A00C684
          4A00CE844A009C8C4A00C68C4A00DEAD4A00EFC64A006BDE4A00AD5A5200B563
          5200C67B5200C6845200D6945200C69C5200CEA55200E7BD520063DE5200B56B
          5A009C735A00BD845A00B58C5A00CE8C5A00CE945A00CE9C5A00D6A55A0084AD
          5A009CAD5A00D6AD5A00E7C65A00F7D65A0031DE5A00B5736300D6A563009CAD
          6300DEAD6300D6B563007BBD6300E7C66300EFCE63005ADE6300B5736B00B57B
          6B00AD846B00BD846B00BD8C6B008CA56B00B5A56B007BDE6B00B57B7300B584
          7300BD847300BD947300ADA57300A5AD73008CBD730039E77300B5847B00BD94
          7B0039E77B00B58C8400BD8C8400BD94840042EF8400BD9C8C00C6A58C00A5D6
          8C00E7DE8C0042EF8C005AEF8C00BDA59C00C6ADA5008CF7A500C6ADAD00C6B5
          AD00CEB5AD00CEB5B500CEBDB500DEC6B500CEBDBD00D6C6BD00D6C6C600DECE
          CE00DED6CE00BDEFCE00DED6D600E7DEDE00EFE7DE00C6F7DE00F7EFE700D6FF
          F700E7FFF700FF00FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00ABABABABABAB
          ABABABABABABABABABABABABABA09562161B4C8095A0ABABABABABAB9C781303
          06082B49518199ABABABAB9C59130A050F09092E3A447B9AABABAB6F13170001
          102D2D2F35674688A0AB9613120E024041576A46455C6B4E96AB791315233060
          7F7144333339363688AB1A1C26191818273B4844320D1E2558AB0B261D293C3E
          3E3D4A52444B2C0431AB382218354D896D56555C50936E1F61AB7A000E4197A6
          90765F685B8A87288BAB990C2177A7AAA5926C7065948E3496ABAB8D2042A3A9
          9D7375705B854F8BA1ABABA5832A7484915D5E705A64819EABABABABA58F5369
          867E5B665A7D9EABABABABABABAB9A8F7C637B8D99A4ABABABAB}
        ExplicitLeft = 200
      end
    end
  end
end
