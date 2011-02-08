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
  object lblCustom: TLabel
    Left = 8
    Top = 239
    Width = 174
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Custom, wildcards % and _ allowed:'
  end
  object TreeDBO: TVirtualStringTree
    Left = 8
    Top = 27
    Width = 216
    Height = 206
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
    Images = MainForm.ImageListMain
    Indent = 16
    Margin = 2
    TabOrder = 0
    TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
    OnFocusChanged = TreeDBOFocusChanged
    OnGetText = TreeDBOGetText
    OnGetImageIndex = TreeDBOGetImageIndex
    OnGetNodeDataSize = TreeDBOGetNodeDataSize
    OnInitChildren = TreeDBOInitChildren
    OnInitNode = TreeDBOInitNode
    Columns = <
      item
        Position = 0
        Width = 212
        WideText = 'Name'
      end
      item
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coParentColor, coResizable, coShowDropMark, coAllowFocus]
        Position = 1
        WideText = 'Size'
      end>
  end
  object btnOK: TButton
    Left = 68
    Top = 283
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 4
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
    TabOrder = 5
  end
  object editDb: TEdit
    Left = 8
    Top = 255
    Width = 58
    Height = 21
    Anchors = [akBottom]
    TabOrder = 1
    Text = 'editDb'
    TextHint = 'database'
  end
  object editTable: TEdit
    Left = 72
    Top = 255
    Width = 71
    Height = 21
    Anchors = [akBottom]
    TabOrder = 2
    Text = 'editTable'
    TextHint = 'all tables'
  end
  object editColumn: TEdit
    Left = 149
    Top = 255
    Width = 75
    Height = 21
    Anchors = [akBottom]
    TabOrder = 3
    Text = 'editColumn'
    TextHint = 'all columns'
  end
end
