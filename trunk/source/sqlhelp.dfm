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
    end
    object treeTopics: TTreeView
      Left = 0
      Top = 24
      Width = 153
      Height = 312
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      ChangeDelay = 50
      Images = MainForm.PngImageListMain
      Indent = 19
      ReadOnly = True
      ShowLines = False
      TabOrder = 0
      OnChange = treeTopicsChange
      OnExpanding = treeTopicsExpanding
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
        Lines.Strings = (
          'memoDescription')
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        OnKeyDown = memosKeyDown
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
      end
      object btnSearchOnline: TPngSpeedButton
        Left = 207
        Top = 128
        Width = 103
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = 'Search online'
        OnClick = ButtonOnlinehelpClick
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000003764944415478DA55936D6C535518C7FFB7EDA55DD7EEA5
          69479BAE2373B8C0C690340803E60C084C9630612A44E31842982F71BE46FC62
          A209066362FC22C1C444191F0431F261E240948D652D939139DEC6325760525B
          DA6EC2D6BBF5EEF6DEF3E2D9E2089EE49F7372F23CBFE725CF2371CEF1F07933
          FCF213E26A157A5CC82364114A0BF571C60F7F59DBF6CBC3F6D23C4038CE1AB6
          093DB7B564A3D595E3415ACD85AA03846A2048A1F3EE194209FD8D10BAE3DBFA
          13D30F00C25916EFF32B3D95EBEA021BA0685664A123A24858C0ADB8A73270A2
          23071924B221F4C7FBAF1183D41EDFD19E9E077C177497BFF8A47F0D34D8C069
          1E74DDC0902243365B119FE0300C02B3A6A0D43385A8D2899EC89533279B4ED7
          4BADA1DD35227AD7FB2B9AE5AB24034D0272A90350DD886764640D09C377A600
          A663A1EF1E8ADCA35869F3E3A3535F5191C5B3B3809375FED58D5E6700231602
          13F7A194D8108A5BC0880591E43424DB7DD8FDB761B225C19886E0F44248DA0C
          8EF476FC3A0B88EE5BDA18B8AC26316E33A1C25C8D424A706C8043D328F27D1C
          8AEB07DC6593220B8E02624569C28A8DA5417CF8E3A1C42C20F3565593FD5832
          8431B38A2AFBD370EA79387D9121DF338345252A6EB0FBB00D1C06D532180F2C
          41F1841D7BD73E83D7BF393023BDD1D3ACBE53B52BE748B40313082098BF0CDD
          BDA207390E64520AFC6580A32885B12B9F2350B80843F10114E406F1EAFA9D68
          F9FA634D7AADAB29D652D9E8EF1EBF0EDD56036DC282C1213B7CE6B745BD143A
          33A01302BF6B3196076A7069B40BD7627DD85FFF19F61FFD2225ED3BFBC24F5B
          4AAAB7CAD63C9C4BC4047D316E0E0690CFDEC5A665CDA09C810A909804242763
          287414E1F75BE770F9EF300CE5D16E69F7A9E7371383767C50B3C7D276F56750
          4906C96E839A7A0FF58FEDC59D7F22301801119918D480C9B40045CE62846F9F
          45DFE8053237483BBF6F685FE55DD250EE2DC31FC9BFC04C4FE1D69FAD6858F1
          8A70A46294E95C268974142E875744EF45E7F0795523BA7B0ED078748BD3D069
          784D71C5F20DC15A0C8FB9D01EDE034DD7450F7468C440997B29AA1FD98CFE68
          2FBA477AB2765A5C3E78F066F4C132D51D5AEF1493754294B369D73AB158D40B
          CEED904C12DC050A0E74BC844ADF6A84462E4CE5F240C5F58391D8FFB671FEAC
          FD64D576B1712D94B22A4659E17FDFE949F30D376190671895C73FE564DEFE5F
          67A6B969C4CDA1940000000049454E44AE426082}
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
        ModalResult = 1
        TabOrder = 1
        OnClick = ButtonCloseClick
      end
    end
  end
end
