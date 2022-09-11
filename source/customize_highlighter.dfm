object frmCustomizeHighlighter: TfrmCustomizeHighlighter
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Customize highlighter'
  ClientHeight = 249
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    441
    249)
  TextHeight = 15
  object lblBackground: TLabel
    Left = 159
    Top = 37
    Width = 67
    Height = 15
    Caption = 'Background:'
  end
  object lblForeground: TLabel
    Left = 159
    Top = 65
    Width = 65
    Height = 15
    Caption = 'Foreground:'
  end
  object lblStyle: TLabel
    Left = 159
    Top = 90
    Width = 28
    Height = 15
    Caption = 'Style:'
  end
  object lblSample: TLabel
    Left = 277
    Top = 175
    Width = 156
    Height = 26
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'Example text'
    Color = clBtnFace
    ParentColor = False
    Transparent = False
    Layout = tlCenter
  end
  object comboHighlighter: TComboBox
    Left = 8
    Top = 8
    Width = 145
    Height = 23
    Style = csDropDownList
    Sorted = True
    TabOrder = 0
    OnSelect = comboHighlighterSelect
  end
  object listboxAttributes: TListBox
    Left = 8
    Top = 37
    Width = 145
    Height = 173
    ItemHeight = 15
    TabOrder = 1
    OnClick = listboxAttributesClick
  end
  object chkBold: TCheckBox
    Left = 277
    Top = 90
    Width = 156
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Bold'
    TabOrder = 4
    OnClick = Modified
  end
  object chkItalic: TCheckBox
    Left = 277
    Top = 113
    Width = 156
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Italic'
    TabOrder = 5
    OnClick = Modified
  end
  object btnCancel: TButton
    Left = 277
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object btnOK: TButton
    Left = 196
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
    OnClick = SaveSettings
  end
  object editBackground: TButtonedEdit
    Left = 277
    Top = 34
    Width = 156
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Images = MainForm.VirtualImageListMain
    RightButton.Hint = 'Color picker'
    RightButton.ImageIndex = 33
    RightButton.Visible = True
    TabOrder = 2
    OnExit = Modified
    OnRightButtonClick = editColorRightButtonClick
  end
  object editForeground: TButtonedEdit
    Left = 277
    Top = 62
    Width = 156
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Images = MainForm.VirtualImageListMain
    RightButton.Hint = 'Color picker'
    RightButton.ImageIndex = 33
    RightButton.Visible = True
    TabOrder = 3
    OnExit = Modified
    OnRightButtonClick = editColorRightButtonClick
  end
  object pnlSample: TPanel
    Left = 277
    Top = 136
    Width = 156
    Height = 33
    Caption = 'Example text'
    Color = clGradientActiveCaption
    ParentBackground = False
    TabOrder = 6
  end
  object btnApply: TButton
    Left = 358
    Top = 216
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 9
    OnClick = SaveSettings
  end
end
