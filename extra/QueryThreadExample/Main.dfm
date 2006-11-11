object Form1: TForm1
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akBottom]
  Caption = 'Threaded Mysql Query Example'
  ClientHeight = 346
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 152
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      451
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
      Width = 81
      Height = 13
      Caption = 'Active thread list'
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
      Left = 367
      Top = 100
      Width = 76
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Exec Async'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 8
      Top = 102
      Width = 353
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object edHost: TEdit
      Left = 8
      Top = 48
      Width = 130
      Height = 21
      TabOrder = 2
      Text = 'localhost'
    end
    object edPort: TEdit
      Left = 144
      Top = 48
      Width = 41
      Height = 21
      TabOrder = 3
      Text = '3306'
    end
    object edUser: TEdit
      Left = 208
      Top = 48
      Width = 66
      Height = 21
      TabOrder = 4
    end
    object edPass: TEdit
      Left = 280
      Top = 48
      Width = 66
      Height = 21
      TabOrder = 5
    end
    object edDatabase: TEdit
      Left = 370
      Top = 48
      Width = 73
      Height = 21
      TabOrder = 6
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 312
    Width = 451
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
  end
  object Panel1: TPanel
    Left = 0
    Top = 152
    Width = 451
    Height = 160
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 2
    ExplicitLeft = 104
    ExplicitTop = 176
    ExplicitWidth = 129
    ExplicitHeight = 65
    DesignSize = (
      451
      160)
    object Panel4: TPanel
      Left = 7
      Top = 5
      Width = 438
      Height = 146
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 0
      object lvResult: TListView
        Left = 2
        Top = 2
        Width = 434
        Height = 142
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'ThreadID'
            Width = 65
          end
          item
            Caption = 'ConnectionID'
            Width = 82
          end
          item
            Caption = 'Statement'
            Width = 211
          end
          item
            Caption = 'Result'
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lvResultSelectItem
        ExplicitWidth = 429
        ExplicitHeight = 151
      end
    end
  end
end
