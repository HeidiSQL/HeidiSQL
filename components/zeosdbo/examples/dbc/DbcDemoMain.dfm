object frmDBCDemo: TfrmDBCDemo
  Left = 380
  Top = 201
  Caption = 'DBC demo'
  ClientHeight = 622
  ClientWidth = 720
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Pannel1: TPanel
    Left = 0
    Top = 0
    Width = 720
    Height = 57
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 39
      Height = 13
      Caption = 'Protocol'
    end
    object Label2: TLabel
      Left = 115
      Top = 8
      Width = 48
      Height = 13
      Caption = 'Hostname'
    end
    object Label3: TLabel
      Left = 228
      Top = 8
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object Label4: TLabel
      Left = 282
      Top = 8
      Width = 46
      Height = 13
      Caption = 'Database'
    end
    object Label5: TLabel
      Left = 395
      Top = 8
      Width = 48
      Height = 13
      Caption = 'Username'
    end
    object Label6: TLabel
      Left = 507
      Top = 8
      Width = 46
      Height = 13
      Caption = 'Password'
    end
    object Button1: TButton
      Left = 632
      Top = 21
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = Button1Click
    end
    object cobProtocol: TComboBox
      Left = 8
      Top = 24
      Width = 105
      Height = 21
      ItemHeight = 13
      TabOrder = 1
    end
    object edtHostName: TEdit
      Left = 115
      Top = 24
      Width = 110
      Height = 21
      TabOrder = 2
      Text = 'localhost'
    end
    object spePort: TSpinEdit
      Left = 228
      Top = 24
      Width = 51
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 3
      Value = 0
    end
    object edtDatabase: TEdit
      Left = 282
      Top = 24
      Width = 110
      Height = 21
      TabOrder = 4
      Text = 'c:\zeoslib.fdb'
    end
    object edtUsername: TEdit
      Left = 395
      Top = 24
      Width = 110
      Height = 21
      TabOrder = 5
      Text = 'sysdba'
    end
    object edtPassword: TEdit
      Left = 507
      Top = 24
      Width = 110
      Height = 21
      TabOrder = 6
      Text = 'masterkey'
    end
  end
  object sgResults: TStringGrid
    Left = 0
    Top = 178
    Width = 720
    Height = 444
    Align = alClient
    ColCount = 1
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 1
    OnSelectCell = sgResultsSelectCell
    OnSetEditText = sgResultsSetEditText
  end
  object Panel1: TPanel
    Left = 0
    Top = 57
    Width = 720
    Height = 80
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 2
    object Label7: TLabel
      Left = 8
      Top = 8
      Width = 70
      Height = 13
      Caption = 'SQL statement'
    end
    object memSQL: TMemo
      Left = 7
      Top = 23
      Width = 612
      Height = 50
      Lines.Strings = (
        'select * from people')
      TabOrder = 0
    end
    object Button2: TButton
      Left = 632
      Top = 24
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 137
    Width = 720
    Height = 41
    Align = alTop
    TabOrder = 3
    object Button3: TButton
      Left = 168
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 88
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Post'
      TabOrder = 1
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Append'
      TabOrder = 0
      OnClick = Button5Click
    end
  end
end
