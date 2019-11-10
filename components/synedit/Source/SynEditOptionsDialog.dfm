object fmEditorOptionsDialog: TfmEditorOptionsDialog
  Left = 580
  Top = 154
  BorderStyle = bsDialog
  Caption = 'Editor Options'
  ClientHeight = 394
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 6
    Top = 8
    Width = 355
    Height = 345
    ActivePage = Display
    TabOrder = 0
    object Display: TTabSheet
      Caption = 'Display'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbRightEdge: TGroupBox
        Left = 8
        Top = 136
        Width = 159
        Height = 88
        Caption = 'Right Edge'
        TabOrder = 1
        object lblEdgeColor: TLabel
          Left = 9
          Top = 56
          Width = 54
          Height = 13
          Caption = 'Edge color:'
        end
        object lblEdgeColumn: TLabel
          Left = 9
          Top = 26
          Width = 66
          Height = 13
          Caption = 'Edge Column:'
        end
        object pnlRightEdgeBack: TPanel
          Left = 80
          Top = 54
          Width = 52
          Height = 21
          BorderWidth = 1
          TabOrder = 1
          object pnlRightEdgeColor: TPanel
            Left = 2
            Top = 2
            Width = 38
            Height = 17
            Align = alClient
            BevelOuter = bvLowered
            Color = clGray
            TabOrder = 0
            OnClick = pnlRightEdgeColorClick
          end
          object btnRightEdge: TPanel
            Left = 40
            Top = 2
            Width = 10
            Height = 17
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            OnMouseDown = btnRightEdgeMouseDown
            object Image1: TImage
              Left = 3
              Top = 6
              Width = 5
              Height = 5
              Picture.Data = {
                07544269746D61708A000000424D8A0000000000000076000000280000000500
                0000050000000100040000000000140000000000000000000000100000001000
                0000000000000000800000800000008080008000000080008000808000008080
                8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
                FF00DDDDD000DD0DD000D000D00000000000DDDDD000}
              Transparent = True
              OnMouseDown = btnRightEdgeMouseDown
            end
          end
        end
        object eRightEdge: TEdit
          Left = 80
          Top = 23
          Width = 51
          Height = 21
          TabOrder = 0
          Text = '0'
        end
      end
      object gbGutter: TGroupBox
        Left = 8
        Top = 8
        Width = 330
        Height = 121
        Caption = 'Gutter'
        TabOrder = 0
        object lblGutterColor: TLabel
          Left = 176
          Top = 89
          Width = 58
          Height = 13
          Caption = 'Gutter color:'
        end
        object ckGutterAutosize: TCheckBox
          Left = 9
          Top = 37
          Width = 120
          Height = 17
          Caption = 'Autosize'
          TabOrder = 1
        end
        object ckGutterShowLineNumbers: TCheckBox
          Left = 9
          Top = 56
          Width = 120
          Height = 17
          Caption = 'Show line numbers'
          TabOrder = 2
        end
        object ckGutterShowLeaderZeros: TCheckBox
          Left = 9
          Top = 94
          Width = 120
          Height = 17
          Caption = 'Show leading zeros'
          TabOrder = 4
        end
        object ckGutterStartAtZero: TCheckBox
          Left = 9
          Top = 75
          Width = 120
          Height = 17
          Caption = 'Start at zero'
          TabOrder = 3
        end
        object ckGutterVisible: TCheckBox
          Left = 9
          Top = 18
          Width = 120
          Height = 17
          Caption = 'Visible'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object cbGutterFont: TCheckBox
          Left = 176
          Top = 18
          Width = 120
          Height = 17
          Caption = 'Use Gutter Font'
          TabOrder = 5
          OnClick = cbGutterFontClick
        end
        object btnGutterFont: TButton
          Left = 282
          Top = 13
          Width = 40
          Height = 25
          Caption = 'Font'
          TabOrder = 6
          OnClick = btnGutterFontClick
        end
        object pnlGutterBack: TPanel
          Left = 252
          Top = 85
          Width = 52
          Height = 21
          BorderWidth = 1
          TabOrder = 8
          object pnlGutterColor: TPanel
            Left = 2
            Top = 2
            Width = 38
            Height = 17
            Align = alClient
            BevelOuter = bvLowered
            Color = clGray
            TabOrder = 0
            OnClick = pnlGutterColorClick
          end
          object btnGutterColor: TPanel
            Left = 40
            Top = 2
            Width = 10
            Height = 17
            Align = alRight
            BevelOuter = bvNone
            TabOrder = 1
            OnMouseDown = btnGutterColorMouseDown
            object Image2: TImage
              Left = 3
              Top = 6
              Width = 5
              Height = 5
              Picture.Data = {
                07544269746D61708A000000424D8A0000000000000076000000280000000500
                0000050000000100040000000000140000000000000000000000100000001000
                0000000000000000800000800000008080008000000080008000808000008080
                8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
                FF00DDDDD000DD0DD000D000D00000000000DDDDD000}
              Transparent = True
              OnMouseDown = btnGutterColorMouseDown
            end
          end
        end
        object pnlGutterFontDisplay: TPanel
          Left = 176
          Top = 40
          Width = 145
          Height = 33
          BevelOuter = bvNone
          TabOrder = 7
          object lblGutterFont: TLabel
            Left = 19
            Top = 9
            Width = 105
            Height = 14
            Caption = 'Courier New 8pt'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
          end
        end
      end
      object gbBookmarks: TGroupBox
        Left = 8
        Top = 232
        Width = 159
        Height = 79
        Caption = 'Bookmarks'
        TabOrder = 3
        object ckBookmarkKeys: TCheckBox
          Left = 9
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Bookmark keys'
          TabOrder = 0
        end
        object ckBookmarkVisible: TCheckBox
          Left = 9
          Top = 48
          Width = 121
          Height = 17
          Caption = 'Bookmarks visible'
          TabOrder = 1
        end
      end
      object gbEditorFont: TGroupBox
        Left = 180
        Top = 232
        Width = 159
        Height = 79
        Caption = 'Editor Font'
        TabOrder = 4
        object btnFont: TButton
          Left = 64
          Top = 49
          Width = 84
          Height = 25
          Caption = 'Font'
          TabOrder = 0
          OnClick = btnFontClick
        end
        object pnlEditorFont: TPanel
          Left = 8
          Top = 19
          Width = 143
          Height = 30
          BevelOuter = bvNone
          TabOrder = 1
          object lblFont: TLabel
            Left = 2
            Top = 1
            Width = 128
            Height = 16
            Caption = 'Courier New 10pt'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier New'
            Font.Style = []
            ParentFont = False
          end
        end
      end
      object gbLineSpacing: TGroupBox
        Left = 180
        Top = 136
        Width = 159
        Height = 88
        Caption = 'Line spacing / Tab spacing'
        TabOrder = 2
        object lblExtraLines: TLabel
          Left = 9
          Top = 27
          Width = 55
          Height = 13
          Caption = 'Extra Lines:'
        end
        object lblTabWidth: TLabel
          Left = 9
          Top = 56
          Width = 53
          Height = 13
          Caption = 'Tab Width:'
        end
        object eLineSpacing: TEdit
          Left = 80
          Top = 23
          Width = 52
          Height = 21
          TabOrder = 0
          Text = '0'
        end
        object eTabWidth: TEdit
          Left = 80
          Top = 53
          Width = 52
          Height = 21
          TabOrder = 1
          Text = '8'
        end
      end
    end
    object Options: TTabSheet
      Caption = 'Options'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object gbOptions: TGroupBox
        Left = 8
        Top = 0
        Width = 330
        Height = 247
        Caption = 'Options'
        TabOrder = 0
        object ckAutoIndent: TCheckBox
          Left = 9
          Top = 15
          Width = 130
          Height = 17
          Hint = 
            'Will indent the caret on new lines with the same amount of leadi' +
            'ng white space as the preceding line'
          Caption = 'Auto indent'
          TabOrder = 0
        end
        object ckDragAndDropEditing: TCheckBox
          Left = 9
          Top = 53
          Width = 130
          Height = 17
          Hint = 
            'Allows you to select a block of text and drag it within the docu' +
            'ment to another location'
          Caption = 'Drag and drop editing'
          TabOrder = 2
        end
        object ckAutoSizeMaxWidth: TCheckBox
          Left = 9
          Top = 34
          Width = 130
          Height = 17
          Hint = 'Allows the editor accept OLE file drops'
          Caption = 'Auto size scroll width'
          TabOrder = 1
        end
        object ckHalfPageScroll: TCheckBox
          Left = 176
          Top = 15
          Width = 130
          Height = 17
          Hint = 
            'When scrolling with page-up and page-down commands, only scroll ' +
            'a half page at a time'
          Caption = 'Half page scroll'
          TabOrder = 12
        end
        object ckEnhanceEndKey: TCheckBox
          Left = 9
          Top = 186
          Width = 130
          Height = 17
          Hint = 'Makes it so the caret is never visible'
          Caption = 'Enhance End Key'
          TabOrder = 9
        end
        object ckScrollByOneLess: TCheckBox
          Left = 176
          Top = 34
          Width = 130
          Height = 17
          Hint = 'Forces scrolling to be one less'
          Caption = 'Scroll by one less'
          TabOrder = 13
        end
        object ckScrollPastEOF: TCheckBox
          Left = 176
          Top = 53
          Width = 130
          Height = 17
          Hint = 'Allows the cursor to go past the end of file marker'
          Caption = 'Scroll past end of file'
          TabOrder = 14
        end
        object ckScrollPastEOL: TCheckBox
          Left = 176
          Top = 72
          Width = 130
          Height = 17
          Hint = 
            'Allows the cursor to go past the last character into the white s' +
            'pace at the end of a line'
          Caption = 'Scroll past end of line'
          TabOrder = 15
        end
        object ckShowScrollHint: TCheckBox
          Left = 176
          Top = 91
          Width = 130
          Height = 17
          Hint = 
            'Shows a hint of the visible line numbers when scrolling vertical' +
            'ly'
          Caption = 'Show scroll hint'
          TabOrder = 16
        end
        object ckSmartTabs: TCheckBox
          Left = 9
          Top = 129
          Width = 130
          Height = 17
          Hint = 
            'When tabbing, the cursor will go to the next non-white space cha' +
            'racter of the previous line'
          Caption = 'Smart tabs'
          TabOrder = 6
        end
        object ckTabsToSpaces: TCheckBox
          Left = 176
          Top = 148
          Width = 130
          Height = 17
          Hint = 'Converts a tab character to the number of spaces in Tab Width'
          Caption = 'Tabs to spaces'
          TabOrder = 18
        end
        object ckTrimTrailingSpaces: TCheckBox
          Left = 176
          Top = 167
          Width = 130
          Height = 17
          Hint = 'Spaces at the end of lines will be trimmed and not saved'
          Caption = 'Trim trailing spaces'
          TabOrder = 19
        end
        object ckWantTabs: TCheckBox
          Left = 9
          Top = 110
          Width = 130
          Height = 17
          Hint = 
            'Let the editor accept tab characters instead of going to the nex' +
            't control'
          Caption = 'Want tabs'
          TabOrder = 5
        end
        object ckAltSetsColumnMode: TCheckBox
          Left = 9
          Top = 72
          Width = 130
          Height = 17
          Hint = 
            'Holding down the Alt Key will put the selection mode into column' +
            'ar format'
          Caption = 'Alt sets column mode'
          TabOrder = 3
        end
        object ckKeepCaretX: TCheckBox
          Left = 9
          Top = 91
          Width = 130
          Height = 17
          Hint = 
            'When moving through lines the X position will always stay the sa' +
            'me'
          Caption = 'Maintain caret column'
          TabOrder = 4
        end
        object ckScrollHintFollows: TCheckBox
          Left = 176
          Top = 110
          Width = 152
          Height = 17
          Hint = 'The scroll hint follows the mouse when scrolling vertically'
          Caption = 'Scroll hint follows mouse'
          TabOrder = 17
        end
        object ckGroupUndo: TCheckBox
          Left = 176
          Top = 186
          Width = 130
          Height = 17
          Hint = 
            'When undoing/redoing actions, handle all continous changes of th' +
            'e same kind in one call instead undoing/redoing each command sep' +
            'arately'
          Caption = 'Group undo'
          TabOrder = 20
        end
        object ckSmartTabDelete: TCheckBox
          Left = 9
          Top = 148
          Width = 130
          Height = 17
          Hint = 'similar to Smart Tabs, but when you delete characters'
          Caption = 'Smart tab delete'
          TabOrder = 7
        end
        object ckRightMouseMoves: TCheckBox
          Left = 176
          Top = 205
          Width = 146
          Height = 17
          Hint = 
            'When clicking with the right mouse for a popup menu, move the cu' +
            'rsor to that location'
          Caption = 'Right mouse moves cursor'
          TabOrder = 21
        end
        object ckEnhanceHomeKey: TCheckBox
          Left = 9
          Top = 167
          Width = 146
          Height = 17
          Hint = 'enhances home key positioning, similar to visual studio'
          Caption = 'Enhance Home Key'
          TabOrder = 8
        end
        object ckHideShowScrollbars: TCheckBox
          Left = 9
          Top = 205
          Width = 156
          Height = 17
          Hint = 
            'if enabled, then the scrollbars will only show when necessary.  ' +
            'If you have ScrollPastEOL, then it the horizontal bar will alway' +
            's be there (it uses MaxLength instead)'
          Caption = 'Hide scrollbars as necessary'
          TabOrder = 10
        end
        object ckDisableScrollArrows: TCheckBox
          Left = 9
          Top = 224
          Width = 130
          Height = 17
          Hint = 
            'Disables the scroll bar arrow buttons when you can'#39't scroll in t' +
            'hat direction any more'
          Caption = 'Disable scroll arrows'
          TabOrder = 11
        end
        object ckShowSpecialChars: TCheckBox
          Left = 176
          Top = 224
          Width = 130
          Height = 17
          Hint = 'Shows linebreaks, spaces and tabs using special symbols'
          Caption = 'Show special chars'
          TabOrder = 22
        end
        object ckTabIndent: TCheckBox
          Left = 176
          Top = 129
          Width = 130
          Height = 17
          Hint = 'Use tab for indention'
          Caption = 'Tab indent'
          TabOrder = 23
        end
      end
      object gbCaret: TGroupBox
        Left = 8
        Top = 249
        Width = 330
        Height = 62
        Caption = 'Caret'
        TabOrder = 1
        object lblInsertCaret: TLabel
          Left = 16
          Top = 17
          Width = 56
          Height = 13
          Caption = 'Insert caret:'
        end
        object lblOverwriteCaret: TLabel
          Left = 16
          Top = 41
          Width = 75
          Height = 13
          Caption = 'Overwrite caret:'
        end
        object cInsertCaret: TComboBox
          Left = 120
          Top = 13
          Width = 186
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
        object cOverwriteCaret: TComboBox
          Left = 120
          Top = 37
          Width = 186
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          Items.Strings = (
            'Vertical Line'
            'Horizontal Line'
            'Half Block'
            'Block')
        end
      end
    end
    object Keystrokes: TTabSheet
      Caption = 'Keystrokes'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object btnAddKey: TButton
        Left = 96
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Add'
        TabOrder = 2
        OnClick = btnAddKeyClick
      end
      object btnRemKey: TButton
        Left = 176
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Remove'
        TabOrder = 3
        OnClick = btnRemKeyClick
      end
      object gbKeyStrokes: TGroupBox
        Left = 8
        Top = 192
        Width = 330
        Height = 119
        Caption = 'Keystroke Options'
        TabOrder = 4
        object lblCommand: TLabel
          Left = 16
          Top = 28
          Width = 50
          Height = 13
          Caption = 'Command:'
        end
        object lblKeystroke2: TLabel
          Left = 16
          Top = 91
          Width = 50
          Height = 13
          Caption = 'Keystroke:'
        end
        object lblKeystroke: TLabel
          Left = 16
          Top = 59
          Width = 50
          Height = 13
          Caption = 'Keystroke:'
        end
        object cKeyCommand: TComboBox
          Left = 120
          Top = 23
          Width = 186
          Height = 21
          TabOrder = 0
          OnExit = cKeyCommandExit
          OnKeyPress = cKeyCommandKeyPress
          OnKeyUp = cKeyCommandKeyUp
        end
      end
      object btnUpdateKey: TButton
        Left = 16
        Top = 152
        Width = 75
        Height = 25
        Caption = '&Update'
        TabOrder = 1
        OnClick = btnUpdateKeyClick
      end
      object pnlCommands: TPanel
        Left = 8
        Top = 13
        Width = 330
        Height = 132
        BevelInner = bvRaised
        BevelOuter = bvLowered
        Caption = 'pnlCommands'
        TabOrder = 0
        object KeyList: TListView
          Left = 2
          Top = 2
          Width = 326
          Height = 128
          Align = alClient
          BorderStyle = bsNone
          Columns = <
            item
              Caption = 'Command'
              Width = 167
            end
            item
              Caption = 'Keystroke'
              Width = 142
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnChanging = KeyListChanging
        end
      end
    end
  end
  object btnOk: TButton
    Left = 200
    Top = 362
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 280
    Top = 362
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ColorDialog: TColorDialog
    Left = 8
    Top = 368
  end
  object ColorPopup: TPopupMenu
    Left = 40
    Top = 368
    object mnuNone: TMenuItem
      Tag = -1
      Caption = 'None'
      OnClick = PopupMenuClick
    end
    object mnuScrollBar: TMenuItem
      Caption = 'Scrollbar'
      OnClick = PopupMenuClick
    end
    object mnuBackground: TMenuItem
      Tag = 1
      Caption = 'Background'
      OnClick = PopupMenuClick
    end
    object mnuActiveCaption: TMenuItem
      Tag = 2
      Caption = 'Active Caption'
      OnClick = PopupMenuClick
    end
    object mnuInactiveCaption: TMenuItem
      Tag = 3
      Caption = 'Inactive Caption'
      OnClick = PopupMenuClick
    end
    object mnuMenu: TMenuItem
      Tag = 4
      Caption = 'Menu'
      OnClick = PopupMenuClick
    end
    object mnuWindow: TMenuItem
      Tag = 5
      Caption = 'Window'
      OnClick = PopupMenuClick
    end
    object mnuWindowFrame: TMenuItem
      Tag = 6
      Caption = 'Window Frame'
      OnClick = PopupMenuClick
    end
    object Menu2: TMenuItem
      Tag = 7
      Caption = 'Menu Text'
      OnClick = PopupMenuClick
    end
    object mnuWindowText: TMenuItem
      Tag = 8
      Caption = 'Window Text'
      OnClick = PopupMenuClick
    end
    object mnuCaptionText: TMenuItem
      Tag = 9
      Caption = 'Caption Text'
      OnClick = PopupMenuClick
    end
    object mnuActiveBorder: TMenuItem
      Tag = 10
      Caption = 'Active Border'
      OnClick = PopupMenuClick
    end
    object mnuInactiveBorder: TMenuItem
      Tag = 11
      Caption = 'Inactive Border'
      OnClick = PopupMenuClick
    end
    object mnuApplicationWorkspace: TMenuItem
      Tag = 12
      Caption = 'Application Workspace'
      OnClick = PopupMenuClick
    end
    object mnuHighlight: TMenuItem
      Tag = 13
      Caption = 'Highlight'
      OnClick = PopupMenuClick
    end
    object mnuHighlightText: TMenuItem
      Tag = 14
      Caption = 'Highlight Text'
      OnClick = PopupMenuClick
    end
    object mnuButtonFace: TMenuItem
      Tag = 15
      Caption = 'Button Face'
      OnClick = PopupMenuClick
    end
    object mnuButtonShadow: TMenuItem
      Tag = 16
      Caption = 'Button Shadow'
      OnClick = PopupMenuClick
    end
    object mnuGrayText: TMenuItem
      Tag = 17
      Caption = 'Gray Text'
      OnClick = PopupMenuClick
    end
    object mnuButtonText: TMenuItem
      Tag = 18
      Caption = 'Button Text'
      OnClick = PopupMenuClick
    end
    object mnuInactiveCaptionText: TMenuItem
      Tag = 19
      Caption = 'Inactive Caption Text'
      OnClick = PopupMenuClick
    end
    object mnuHighlight2: TMenuItem
      Tag = 20
      Caption = 'Highlight'
      OnClick = PopupMenuClick
    end
    object mnu3dDarkShadow: TMenuItem
      Tag = 21
      Caption = '3D Dark Shadow'
      OnClick = PopupMenuClick
    end
    object mnu3DLight: TMenuItem
      Tag = 22
      Caption = '3D Light'
      OnClick = PopupMenuClick
    end
    object mnuInfoTipText: TMenuItem
      Tag = 23
      Caption = 'Info Tip Text'
      OnClick = PopupMenuClick
    end
    object mnuInfoTipBackground: TMenuItem
      Tag = 24
      Caption = 'Info Tip Background'
      OnClick = PopupMenuClick
    end
  end
  object ImageList: TImageList
    Left = 72
    Top = 368
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Options = [fdEffects, fdFixedPitchOnly]
    Left = 104
    Top = 368
  end
end
