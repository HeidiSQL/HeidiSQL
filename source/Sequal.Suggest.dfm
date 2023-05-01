object SequalSuggestForm: TSequalSuggestForm
  Left = 0
  Top = 0
  Caption = 'SequalSuggestForm'
  ClientHeight = 292
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    418
    292)
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 15
    Caption = 'Label1'
  end
  object memoPrompt: TMemo
    Left = 8
    Top = 32
    Width = 402
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Lines.Strings = (
      'Give me column a, b and c from table foobar')
    TabOrder = 0
  end
  object btnGenerateSQL: TButton
    Left = 288
    Top = 127
    Width = 122
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Generate SQL'
    TabOrder = 1
    OnClick = btnGenerateSQLClick
  end
  object memoGeneratedSQL: TSynMemo
    Left = 8
    Top = 158
    Width = 402
    Height = 95
    SingleLineMode = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGrayText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 2
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.AutoSize = True
    Gutter.DigitCount = 2
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.LeftOffset = 0
    Gutter.ShowLineNumbers = True
    Highlighter = MainForm.SynSQLSynUsed
    Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
    ReadOnly = True
    FontSmoothing = fsmNone
  end
  object btnExecute: TButton
    Left = 288
    Top = 259
    Width = 122
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Execute in new tab'
    Enabled = False
    ModalResult = 1
    TabOrder = 3
    OnClick = btnExecuteClick
  end
  object btnClose: TButton
    Left = 207
    Top = 259
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
  end
  object btnHelp: TButton
    Left = 126
    Top = 259
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 5
    OnClick = btnHelpClick
  end
end
