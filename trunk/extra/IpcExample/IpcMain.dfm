object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 388
  ClientWidth = 517
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 517
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 1
      Top = 8
      Width = 76
      Height = 13
      Caption = 'Synchronization'
    end
    object Button4: TButton
      Left = 83
      Top = 6
      Width = 62
      Height = 19
      Caption = 'Initialize'
      TabOrder = 0
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 151
      Top = 6
      Width = 62
      Height = 19
      Caption = 'Deinitialize'
      TabOrder = 1
      OnClick = Button5Click
    end
    object pnSessionSettings: TPanel
      Left = 1
      Top = 29
      Width = 331
      Height = 23
      BevelOuter = bvNone
      TabOrder = 2
      object Label1: TLabel
        Left = 2
        Top = 5
        Width = 27
        Height = 13
        Caption = 'Name'
      end
      object Label5: TLabel
        Left = 196
        Top = 5
        Width = 76
        Height = 13
        Caption = 'Connected-Flag'
      end
      object edConnName: TEdit
        Left = 40
        Top = 1
        Width = 89
        Height = 21
        TabOrder = 0
        Text = 'IpcTestApp'
      end
      object Button1: TButton
        Left = 135
        Top = 1
        Width = 34
        Height = 21
        Caption = 'SET'
        TabOrder = 1
        OnClick = Button1Click
      end
      object CheckBox1: TCheckBox
        Left = 278
        Top = 2
        Width = 19
        Height = 17
        TabOrder = 2
        OnClick = CheckBox1Click
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 369
    Width = 517
    Height = 19
    Panels = <>
  end
  object pnMain: TPanel
    Left = 0
    Top = 58
    Width = 517
    Height = 311
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter1: TSplitter
      Left = 329
      Top = 0
      Height = 311
      Align = alRight
      ResizeStyle = rsUpdate
      ExplicitLeft = 304
      ExplicitTop = 272
      ExplicitHeight = 100
    end
    object pnWindowList: TPanel
      Left = 332
      Top = 0
      Width = 185
      Height = 311
      Align = alRight
      Enabled = False
      TabOrder = 0
      object Panel5: TPanel
        Left = 1
        Top = 284
        Width = 183
        Height = 26
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object Button2: TButton
          Left = 4
          Top = 3
          Width = 45
          Height = 21
          Caption = 'Refresh'
          TabOrder = 0
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 54
          Top = 3
          Width = 67
          Height = 21
          Caption = 'CrashCheck'
          TabOrder = 1
          OnClick = Button3Click
        end
      end
      object Panel6: TPanel
        Left = 1
        Top = 1
        Width = 183
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          Left = 8
          Top = 6
          Width = 64
          Height = 13
          Caption = 'Window list'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object Panel7: TPanel
        Left = 1
        Top = 25
        Width = 183
        Height = 259
        Align = alClient
        Caption = 'Panel6'
        TabOrder = 2
        object lvWindows: TListView
          Left = 1
          Top = 1
          Width = 181
          Height = 257
          Align = alClient
          Columns = <
            item
              Caption = 'Slot'
              Width = 38
            end
            item
              Caption = 'Handle'
              Width = 60
            end
            item
              Caption = 'Name'
              Width = 131
            end
            item
              Caption = 'Connected'
              Width = 80
            end>
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnDblClick = lvWindowsDblClick
        end
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 329
      Height = 311
      Align = alClient
      Caption = 'Panel3'
      TabOrder = 1
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 327
        Height = 309
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Memo1: TMemo
          Left = 0
          Top = 0
          Width = 327
          Height = 309
          Align = alClient
          BevelOuter = bvNone
          BorderStyle = bsNone
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
end
