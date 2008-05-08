object frmSelectDBObject: TfrmSelectDBObject
  Left = 0
  Top = 0
  Caption = 'Select database object ...'
  ClientHeight = 316
  ClientWidth = 232
  Color = clBtnFace
  Constraints.MinHeight = 343
  Constraints.MinWidth = 240
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    232
    316)
  PixelsPerInch = 96
  TextHeight = 13
  object lblSelect: TLabel
    Left = 8
    Top = 8
    Width = 161
    Height = 13
    Caption = 'Select database, table or column:'
  end
  object lblDB: TLabel
    Left = 8
    Top = 218
    Width = 17
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'DB:'
  end
  object lblTable: TLabel
    Left = 69
    Top = 218
    Width = 30
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Table:'
  end
  object lblCol: TLabel
    Left = 131
    Top = 218
    Width = 39
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Column:'
  end
  object lblHint: TLabel
    Left = 8
    Top = 260
    Width = 135
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '(% and _ wildcards allowed)'
  end
  object TreeDBO: TVirtualStringTree
    Left = 8
    Top = 27
    Width = 216
    Height = 184
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Images = MainForm.PngImageListMain
    Indent = 16
    Margin = 2
    TabOrder = 0
    TreeOptions.PaintOptions = [toHotTrack, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    OnFocusChanged = TreeDBOFocusChanged
    OnGetText = TreeDBOGetText
    OnGetImageIndex = TreeDBOGetImageIndex
    OnGetNodeDataSize = TreeDBOGetNodeDataSize
    OnInitChildren = TreeDBOInitChildren
    OnInitNode = TreeDBOInitNode
    Columns = <>
  end
  object btnOK: TButton
    Left = 68
    Top = 283
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 149
    Top = 283
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object editDB: TEdit
    Left = 8
    Top = 233
    Width = 57
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = 'editDB'
    OnChange = editChange
  end
  object editTable: TEdit
    Left = 69
    Top = 233
    Width = 58
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Text = 'editTable'
    OnChange = editChange
  end
  object editCol: TEdit
    Left = 131
    Top = 233
    Width = 53
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    Text = 'editCol'
    OnChange = editChange
  end
end
