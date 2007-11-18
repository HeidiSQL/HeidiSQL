object MainForm: TMainForm
  Left = 266
  Top = 190
  Width = 559
  Height = 451
  Caption = 'MainForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TTntLabel
    Left = 16
    Top = 16
    Width = 184
    Height = 13
    Caption = 'TntEdit (Data aware version available)'
  end
  object Label2: TTntLabel
    Left = 16
    Top = 64
    Width = 217
    Height = 13
    Caption = 'TntComboBox (Data aware version available)'
  end
  object Label3: TTntLabel
    Left = 16
    Top = 176
    Width = 50
    Height = 13
    Caption = 'TntListBox'
  end
  object Label4: TTntLabel
    Left = 256
    Top = 176
    Width = 79
    Height = 13
    Caption = 'TntCheckListBox'
  end
  object Label5: TTntLabel
    Left = 16
    Top = 304
    Width = 44
    Height = 13
    Caption = 'TntMemo'
  end
  object Label6: TTntLabel
    Left = 16
    Top = 120
    Width = 217
    Height = 49
    Caption = 
      'This program will fully support Unicode characters only when run' +
      'ning on Windows NT/2000/XP.'
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object TntComboBox1: TTntComboBox
    Left = 16
    Top = 80
    Width = 185
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
  object TntEdit1: TTntEdit
    Left = 16
    Top = 32
    Width = 185
    Height = 21
    TabOrder = 1
  end
  object TntMemo1: TTntMemo
    Left = 16
    Top = 320
    Width = 481
    Height = 89
    TabOrder = 2
  end
  object TntListBox1: TTntListBox
    Left = 16
    Top = 192
    Width = 225
    Height = 97
    ItemHeight = 13
    TabOrder = 3
  end
  object TntCheckListBox1: TTntCheckListBox
    Left = 256
    Top = 192
    Width = 241
    Height = 97
    ItemHeight = 13
    TabOrder = 4
  end
  object Button1: TTntButton
    Left = 256
    Top = 8
    Width = 241
    Height = 25
    Caption = 'TntEdit.Text -> Form.Caption'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TTntButton
    Left = 256
    Top = 40
    Width = 241
    Height = 25
    Caption = 'Add TntEdit.Text to TntComboBox.Items'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button3: TTntButton
    Left = 256
    Top = 72
    Width = 241
    Height = 25
    Caption = 'Add TntEdit.Text to TntListBox.Items'
    TabOrder = 7
    OnClick = Button3Click
  end
  object Button4: TTntButton
    Left = 256
    Top = 104
    Width = 241
    Height = 25
    Caption = 'Add TntEdit.Text to TntCheckListBox.Items'
    TabOrder = 8
    OnClick = Button4Click
  end
end
