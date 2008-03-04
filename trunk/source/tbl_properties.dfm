object tbl_properties_form: Ttbl_properties_form
  Left = 771
  Top = 113
  BorderIcons = []
  Caption = 'Table-Properties'
  ClientHeight = 351
  ClientWidth = 312
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 180
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 8
    Top = 20
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = editName
  end
  object lblCharset: TLabel
    Left = 8
    Top = 95
    Width = 70
    Height = 13
    Caption = '&Character set:'
    FocusControl = comboCharset
  end
  object lblCollation: TLabel
    Left = 8
    Top = 120
    Width = 45
    Height = 13
    Caption = 'C&ollation:'
    FocusControl = comboCollation
  end
  object lblComment: TLabel
    Left = 8
    Top = 45
    Width = 49
    Height = 13
    Caption = 'Co&mment:'
    FocusControl = editComment
  end
  object lblEngine: TLabel
    Left = 8
    Top = 70
    Width = 36
    Height = 11
    Caption = '&Engine:'
    FocusControl = comboEngine
  end
  object lblCreate: TLabel
    Left = 8
    Top = 213
    Width = 156
    Height = 13
    Caption = 'SQL &preview for CREATE TABLE:'
    FocusControl = SynMemoCreate
  end
  object lblAutoincrement: TLabel
    Left = 8
    Top = 145
    Width = 77
    Height = 13
    Caption = '&Auto increment:'
    FocusControl = editAutoincrement
  end
  object SynMemoCreate: TSynMemo
    Left = 8
    Top = 232
    Width = 296
    Height = 111
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = popupSynMemo
    TabOrder = 8
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.Visible = False
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    ReadOnly = True
  end
  object btnCancel: TButton
    Left = 229
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 7
  end
  object editName: TEdit
    Left = 88
    Top = 17
    Width = 216
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'editName'
    OnChange = editNameChange
  end
  object editComment: TEdit
    Left = 88
    Top = 42
    Width = 216
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    MaxLength = 60
    TabOrder = 1
    Text = 'editComment'
  end
  object comboCollation: TComboBox
    Left = 88
    Top = 117
    Width = 216
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
  end
  object comboCharset: TComboBox
    Left = 88
    Top = 92
    Width = 216
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
    OnChange = comboCharsetChange
  end
  object comboEngine: TComboBox
    Left = 88
    Top = 67
    Width = 216
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnOK: TButton
    Left = 148
    Top = 178
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 6
  end
  object editAutoincrement: TEdit
    Left = 88
    Top = 143
    Width = 216
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    Text = 'editAutoincrement'
  end
  object popupSynMemo: TPopupMenu
    Images = MainForm.PngImageListMain
    Left = 16
    Top = 240
    object menuCopy: TMenuItem
      Action = MainForm.EditCopy1
    end
    object menuSelectAll: TMenuItem
      Caption = 'Select all'
      ShortCut = 16449
      OnClick = menuSelectAllClick
    end
  end
end
