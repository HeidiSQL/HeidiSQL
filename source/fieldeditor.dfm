object FieldEditForm: TFieldEditForm
  Left = 572
  Top = 111
  BorderStyle = bsDialog
  BorderWidth = 5
  Caption = 'Field-Editor'
  ClientHeight = 354
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pc: TPageControl
    Left = 0
    Top = 0
    Width = 297
    Height = 321
    ActivePage = tabField
    Align = alTop
    Images = MainForm.PngImageListMain
    TabOrder = 0
    OnChange = pcChange
    object tabField: TTabSheet
      Caption = 'Field'
      ImageIndex = 42
      object lblName: TLabel
        Left = 8
        Top = 40
        Width = 31
        Height = 13
        Caption = '&Name:'
        FocusControl = EditFieldname
      end
      object lblType: TLabel
        Left = 8
        Top = 64
        Width = 28
        Height = 13
        Caption = '&Type:'
        FocusControl = ComboBoxType
      end
      object lblLengthSet: TLabel
        Left = 8
        Top = 88
        Width = 63
        Height = 13
        Caption = '&Length / Set:'
        FocusControl = EditLength
      end
      object lblDefault: TLabel
        Left = 8
        Top = 112
        Width = 39
        Height = 13
        Caption = '&Default:'
        FocusControl = EditDefault
      end
      object lblPosition: TLabel
        Left = 8
        Top = 17
        Width = 41
        Height = 13
        Caption = '&Position:'
        FocusControl = ComboBoxPosition
      end
      object lblComment: TLabel
        Left = 8
        Top = 136
        Width = 49
        Height = 13
        Caption = '&Comment:'
        FocusControl = EditComment
      end
      object btnDatatypeHelp: TButton
        Left = 259
        Top = 61
        Width = 21
        Height = 21
        Hint = 'Help on selected datatype'
        Caption = '?'
        TabOrder = 3
        OnClick = btnDatatypeHelpClick
      end
      object EditDefault: TEdit
        Left = 88
        Top = 109
        Width = 192
        Height = 21
        TabOrder = 5
      end
      object EditLength: TEdit
        Left = 88
        Top = 85
        Width = 192
        Height = 21
        TabOrder = 4
      end
      object ComboBoxType: TComboBox
        Left = 88
        Top = 61
        Width = 165
        Height = 19
        Style = csOwnerDrawFixed
        ItemHeight = 13
        TabOrder = 2
        OnChange = ComboBoxTypeChange
        OnDrawItem = ComboBoxTypeDrawItem
        OnKeyDown = ComboBoxTypeKeyDown
        OnKeyUp = ComboBoxTypeKeyUp
      end
      object EditFieldname: TEdit
        Left = 88
        Top = 37
        Width = 192
        Height = 21
        TabOrder = 1
      end
      object GroupBoxAttributes: TGroupBox
        Left = 8
        Top = 168
        Width = 272
        Height = 113
        Caption = 'Attributes'
        TabOrder = 7
        object CheckBoxBinary: TCheckBox
          Left = 16
          Top = 24
          Width = 65
          Height = 17
          Caption = 'Binary'
          TabOrder = 0
        end
        object CheckBoxUnsigned: TCheckBox
          Left = 16
          Top = 48
          Width = 65
          Height = 17
          Caption = 'Unsigned'
          TabOrder = 1
          OnClick = CheckBoxUnsignedClick
        end
        object CheckBoxZerofill: TCheckBox
          Left = 16
          Top = 72
          Width = 57
          Height = 17
          Caption = 'Zerofill'
          TabOrder = 2
          OnClick = CheckBoxZerofillClick
        end
        object CheckBoxNotNull: TCheckBox
          Left = 128
          Top = 24
          Width = 69
          Height = 17
          Caption = 'Not Null'
          TabOrder = 3
        end
        object CheckBoxAutoIncrement: TCheckBox
          Left = 128
          Top = 48
          Width = 97
          Height = 17
          Caption = 'AutoIncrement'
          TabOrder = 4
        end
      end
      object ComboBoxPosition: TComboBox
        Left = 88
        Top = 14
        Width = 192
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object EditComment: TEdit
        Left = 88
        Top = 133
        Width = 192
        Height = 21
        TabOrder = 6
      end
    end
    object tabIndexes: TTabSheet
      Caption = 'Indexes'
      ImageIndex = 13
      ParentShowHint = False
      ShowHint = True
      object lblIndexName: TLabel
        Left = 8
        Top = 11
        Width = 63
        Height = 13
        Caption = '&Index-Name:'
        FocusControl = ComboBoxKeys
      end
      object lblColumnsUsed: TLabel
        Left = 8
        Top = 104
        Width = 70
        Height = 13
        Caption = 'Columns used:'
      end
      object lblColumnsAvailable: TLabel
        Left = 168
        Top = 104
        Width = 90
        Height = 13
        Caption = 'Available Columns:'
      end
      object btnAddColumnToIndex: TPngSpeedButton
        Tag = 1
        Left = 133
        Top = 152
        Width = 25
        Height = 25
        Hint = 'Add field to index'
        Flat = True
        OnClick = AddField
      end
      object btnDeleteColumnFromIndex: TPngSpeedButton
        Tag = 2
        Left = 133
        Top = 184
        Width = 25
        Height = 25
        Hint = 'Remove field from index'
        Flat = True
        OnClick = RemoveField
      end
      object btnAddAllColumnsToIndex: TPngSpeedButton
        Left = 133
        Top = 120
        Width = 25
        Height = 25
        Hint = 'Add all fields to index'
        Flat = True
        OnClick = btnAddAllColumnsToIndexClick
      end
      object btnDeleteAllColumnsFromIndex: TPngSpeedButton
        Left = 133
        Top = 216
        Width = 25
        Height = 25
        Hint = 'Remove all fields from index'
        Flat = True
        OnClick = btnDeleteAllColumnsFromIndexClick
      end
      object ComboBoxKeys: TComboBoxEx
        Left = 72
        Top = 8
        Width = 209
        Height = 22
        ItemsEx = <>
        Style = csExDropDownList
        ItemHeight = 16
        TabOrder = 0
        OnChange = ComboBoxKeysChange
        Images = MainForm.PngImageListMain
      end
      object CheckBoxUnique: TCheckBox
        Left = 8
        Top = 72
        Width = 57
        Height = 17
        Caption = 'Unique'
        TabOrder = 1
        OnClick = CheckBoxUniqueClick
      end
      object ButtonAdd: TButton
        Left = 144
        Top = 32
        Width = 65
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = ButtonAddClick
      end
      object ButtonDelete: TButton
        Left = 216
        Top = 32
        Width = 65
        Height = 25
        Caption = 'Delete'
        Enabled = False
        TabOrder = 3
        OnClick = ButtonDeleteClick
      end
      object listColumnsUsed: TTntListBox
        Left = 8
        Top = 120
        Width = 113
        Height = 161
        Enabled = False
        ItemHeight = 13
        TabOrder = 4
        OnClick = listClick
        OnDblClick = RemoveField
      end
      object listColumnsAvailable: TTntListBox
        Left = 168
        Top = 120
        Width = 113
        Height = 161
        Enabled = False
        ItemHeight = 13
        TabOrder = 5
        OnClick = listClick
        OnDblClick = AddField
      end
      object ButtonAddPrimary: TButton
        Left = 72
        Top = 32
        Width = 67
        Height = 25
        Caption = 'Add Primary'
        Enabled = False
        TabOrder = 6
        OnClick = ButtonAddPrimaryClick
      end
      object CheckBoxFulltext: TCheckBox
        Left = 88
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Fulltext'
        TabOrder = 7
        OnClick = CheckBoxFulltextClick
      end
    end
  end
  object ButtonCancel: TButton
    Left = 207
    Top = 327
    Width = 90
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ButtonOK: TButton
    Left = 111
    Top = 327
    Width = 90
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OKClick
  end
end
