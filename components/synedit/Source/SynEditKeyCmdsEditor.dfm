object SynEditKeystrokesEditorForm: TSynEditKeystrokesEditorForm
  Left = 300
  Top = 241
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Keystroke Editor'
  ClientHeight = 319
  ClientWidth = 382
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 8
    Top = 8
    Width = 365
    Height = 308
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object lnlInfo: TLabel
      Left = 5
      Top = 271
      Width = 229
      Height = 13
      Caption = 'NOTE: To have multiple keystrokes do the same'
    end
    object lnlInfo2: TLabel
      Left = 42
      Top = 287
      Width = 217
      Height = 13
      Caption = 'command, assign the command multiple times.'
    end
    object pnlCommands: TPanel
      Left = 16
      Top = 16
      Width = 246
      Height = 244
      BevelInner = bvLowered
      BorderWidth = 4
      Caption = 'pnlCommands'
      TabOrder = 0
      object KeyCmdList: TListView
        Left = 6
        Top = 6
        Width = 234
        Height = 232
        Align = alClient
        BorderStyle = bsNone
        Columns = <
          item
            Caption = 'Command'
            Width = 117
          end
          item
            Caption = 'Keystroke'
            Width = 101
          end>
        ColumnClick = False
        HideSelection = False
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = KeyCmdListClick
        OnDblClick = btnEditClick
      end
    end
    object btnAdd: TButton
      Left = 276
      Top = 20
      Width = 75
      Height = 25
      Caption = '&Add'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnEdit: TButton
      Left = 276
      Top = 52
      Width = 75
      Height = 25
      Caption = '&Edit'
      Enabled = False
      TabOrder = 2
      OnClick = btnEditClick
    end
    object btnDelete: TButton
      Left = 276
      Top = 84
      Width = 75
      Height = 25
      Caption = '&Delete'
      Enabled = False
      TabOrder = 3
      OnClick = btnDeleteClick
    end
    object btnClear: TButton
      Left = 276
      Top = 116
      Width = 75
      Height = 25
      Caption = 'C&lear List'
      TabOrder = 4
      OnClick = btnClearClick
    end
    object btnReset: TButton
      Left = 276
      Top = 148
      Width = 75
      Height = 25
      Caption = '&Reset List'
      TabOrder = 5
      OnClick = btnResetClick
    end
    object btnOK: TButton
      Left = 276
      Top = 241
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 6
      OnClick = btnOKClick
    end
    object btnCancel: TButton
      Left = 276
      Top = 273
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      TabOrder = 7
      OnClick = btnCancelClick
    end
  end
end
