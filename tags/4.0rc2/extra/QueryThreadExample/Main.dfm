object Form1: TForm1
  Left = 293
  Top = 115
  Anchors = [akLeft, akTop, akBottom]
  Caption = 'Threaded Mysql Query Example'
  ClientHeight = 433
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 308
    Width = 450
    Height = 4
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitTop = 133
    ExplicitWidth = 464
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 450
    Height = 152
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      450
      152)
    object Label1: TLabel
      Left = 8
      Top = 83
      Width = 112
      Height = 13
      Caption = 'Enter single SQL string:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label2: TLabel
      Left = 8
      Top = 32
      Width = 22
      Height = 13
      Caption = 'Host'
    end
    object Label3: TLabel
      Left = 144
      Top = 32
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object TLabel
      Left = 208
      Top = 32
      Width = 22
      Height = 13
      Caption = 'User'
    end
    object Label5: TLabel
      Left = 280
      Top = 32
      Width = 22
      Height = 13
      Caption = 'Pass'
    end
    object Label6: TLabel
      Left = 370
      Top = 32
      Width = 46
      Height = 13
      Caption = 'Database'
    end
    object Label4: TLabel
      Left = 8
      Top = 8
      Width = 140
      Height = 13
      Caption = 'Enter connection credentials:'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label7: TLabel
      Left = 8
      Top = 138
      Width = 46
      Height = 13
      Caption = 'Query list'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Button1: TButton
      Left = 286
      Top = 100
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Exec Async'
      TabOrder = 6
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 8
      Top = 102
      Width = 272
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
    end
    object edHost: TEdit
      Left = 8
      Top = 48
      Width = 130
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object edPort: TEdit
      Left = 144
      Top = 48
      Width = 41
      Height = 21
      TabOrder = 1
      Text = '3306'
    end
    object edUser: TEdit
      Left = 208
      Top = 48
      Width = 66
      Height = 21
      TabOrder = 2
    end
    object edPass: TEdit
      Left = 280
      Top = 48
      Width = 66
      Height = 21
      TabOrder = 3
    end
    object edDatabase: TEdit
      Left = 370
      Top = 48
      Width = 73
      Height = 21
      TabOrder = 4
    end
    object Button3: TButton
      Left = 368
      Top = 100
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Exec Blocking'
      TabOrder = 7
      OnClick = Button3Click
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 399
    Width = 450
    Height = 34
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object bnKillThread: TButton
      Left = 9
      Top = 4
      Width = 75
      Height = 25
      Caption = '&Kill Thread'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnClick = bnKillThreadClick
    end
    object Button2: TButton
      Left = 90
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Free Result'
      Enabled = False
      TabOrder = 1
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 152
    Width = 450
    Height = 156
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 2
    DesignSize = (
      450
      156)
    object Panel4: TPanel
      Left = 7
      Top = 5
      Width = 437
      Height = 147
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object lvResult: TListView
        Left = 2
        Top = 2
        Width = 433
        Height = 143
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'ThreadID'
            Width = 65
          end
          item
            Caption = 'ConnID'
            Width = 54
          end
          item
            Caption = 'Event'
            Width = 65
          end
          item
            AutoSize = True
            Caption = 'Statement'
          end
          item
            Caption = 'Result'
            Width = 91
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lvResultSelectItem
      end
    end
  end
  object Panel5: TPanel
    Left = 0
    Top = 312
    Width = 450
    Height = 87
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      450
      87)
    object pnResultset: TPanel
      Left = 8
      Top = 2
      Width = 434
      Height = 79
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      object DBGrid1: TDBGrid
        Left = 1
        Top = 1
        Width = 432
        Height = 77
        Align = alClient
        DataSource = DataSource1
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
  end
  object DataSource1: TDataSource
    Left = 216
  end
end
