object FrmMain: TFrmMain
  Left = 379
  Top = 238
  ActiveControl = BtnStart
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'SynGen'
  ClientHeight = 288
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 8
    Top = 8
    Width = 353
    Height = 241
    ActivePage = TabHighlighter
    TabOrder = 1
    TabWidth = 80
    object TabHighlighter: TTabSheet
      Caption = 'Highlighter'
      object LblAuthor: TLabel
        Left = 8
        Top = 20
        Width = 34
        Height = 13
        Caption = 'Author:'
      end
      object LblDescription: TLabel
        Left = 8
        Top = 52
        Width = 56
        Height = 13
        Caption = 'Description:'
      end
      object LblVersion: TLabel
        Left = 8
        Top = 84
        Width = 38
        Height = 13
        Caption = 'Version:'
      end
      object EditAuthor: TEdit
        Left = 80
        Top = 16
        Width = 257
        Height = 21
        TabOrder = 0
      end
      object EditDescription: TEdit
        Left = 80
        Top = 48
        Width = 257
        Height = 21
        TabOrder = 1
      end
      object EditVersion: TEdit
        Left = 80
        Top = 80
        Width = 257
        Height = 21
        TabOrder = 2
      end
      object ChkGetKeyWords: TCheckBox
        Left = 8
        Top = 188
        Width = 249
        Height = 17
        Caption = 'Include optional GetKeyWords public method'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object ChkGPLHeader: TCheckBox
        Left = 8
        Top = 168
        Width = 249
        Height = 17
        Caption = 'Use SynEdit standard GPL comment header'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object TabLanguage: TTabSheet
      Caption = 'Language'
      object LblFilter: TLabel
        Left = 8
        Top = 20
        Width = 59
        Height = 13
        Caption = 'Default filter:'
      end
      object LblLangName: TLabel
        Left = 8
        Top = 52
        Width = 80
        Height = 13
        Caption = 'Language name:'
      end
      object CboFilter: TComboBox
        Left = 96
        Top = 16
        Width = 241
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = 'All files (*.*)|*.*'
        OnChange = CboLangNameChange
        Items.Strings = (
          'Pascal files (*.pas,*.dpr,*.dpk,*.inc)|*.pas;*.dpr;*.dpk;*.inc'
          'HP48 files (*.s,*.sou,*.a,*.hp)|*.s;*.sou;*.a;*.hp'
          'CA-Clipper files (*.prg, *.ch, *.inc)|*.prg;*.ch;*.inc'
          'C++ files (*.cpp,*.h,*.hpp)|*.cpp;*.h;*.hpp'
          'Java files (*.java)|*.java'
          'Perl files (*.pl,*.pm,*.cgi)|*.pl;*.pm;*.cgi'
          'AWK Script (*.awk)|*.awk'
          'HTML Document (*.htm,*.html)|*.htm;*.html'
          'VBScript files (*.vbs)|*.vbs'
          'Galaxy files (*.gtv,*.galrep,*.txt)|*.gtv;*.galrep;*.txt'
          'Python files (*.py)|*.py'
          'SQL files (*.sql)|*.sql'
          'HP48 files (*.s,*.sou,*.a,*.hp)|*.s;*.sou;*.a;*.hp'
          'Tcl/Tk files (*.tcl)|*.tcl'
          'Rich Text Format (*.rtf)|*.rtf'
          'MS-DOS Batch Files (*.bat)|*.bat'
          'Delphi/C++ Builder Form Files (*.dfm)|*.dfm'
          'x86 Assembly Files (*.asm)|*.asm'
          'GEMBASE files (*.dml,*.gem)|*.dml;*.gem'
          'INI Files (*.ini)|*.ini'
          'Standard ML Files (*.sml)|*.sml'
          'Visual Basic files (*.bas)|*.bas'
          'DSP files (*.dsp,*.inc)|*.dsp;*.inc'
          
            'PHP files (*.php,*.php3,*.phtml,*.inc)|*.php;*.php3;*.phtml;*.in' +
            'c'
          'Cache files (*.mac,*.inc,*.int)|*.mac;*.inc;*.int'
          'Cascading Stylesheets (*.css)|*.css'
          'Javascript files (*.js)|*.js'
          'Kix Scripts (*.kix)|*.kix'
          'Baan 4GL files (*.cln)|*.cln'
          'Foxpro Files (*.prg)|*.prg'
          'Fortran Files (*.for)|*.for'
          '68HC11 Assembler files (*.hc11,*.asm,*.asc)|*.hc11;*.asm;*.asc')
      end
      object CboLangName: TComboBox
        Left = 96
        Top = 48
        Width = 241
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        OnChange = CboLangNameChange
        Items.Strings = (
          'HP48'
          'CA-Clipper'
          'C++'
          'Java'
          'Perl'
          'MS-DOS Batch Language'
          'Delphi/C++ Builder Form Definitions'
          'AWK Script'
          'HTML Document'
          'MS VBScript'
          'Galaxy'
          'General'
          'ObjectPascal'
          'x86 Assembly Language'
          'Python'
          'Tcl/Tk'
          'SQL'
          'Gembase'
          'INI files'
          'Standard ML'
          'Visual Basic'
          'ADSP21xx'
          'PHP'
          'Sybase SQL'
          'General Multi-Highlighter'
          'Cache object script'
          'Cascading Stylesheets'
          'Javascript'
          'KIX32'
          'Baan 4GL'
          'Foxpro'
          'Fortran'
          '68HC11 Assembler')
      end
    end
    object TabAttributes: TTabSheet
      Caption = 'Attributes'
      object LblUnknownTokenAttr: TLabel
        Left = 8
        Top = 120
        Width = 164
        Height = 13
        Caption = 'Assign unknown token to attribute:'
      end
      object GrpAttrNames: TGroupBox
        Left = 8
        Top = 8
        Width = 329
        Height = 96
        Caption = 'Attribute names'
        TabOrder = 0
        object LblIdentifier: TLabel
          Left = 16
          Top = 32
          Width = 43
          Height = 13
          Caption = 'Identifier:'
        end
        object LblReservedWord: TLabel
          Left = 16
          Top = 64
          Width = 75
          Height = 13
          Caption = 'Reserved word:'
        end
        object CboAttrIdentifier: TComboBox
          Left = 104
          Top = 28
          Width = 209
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
        object CboAttrReservedWord: TComboBox
          Left = 104
          Top = 60
          Width = 209
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
      end
      object CboUnknownTokenAttr: TComboBox
        Left = 184
        Top = 116
        Width = 153
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        Items.Strings = (
          'Identifier'
          'Symbol'
          'Miscellaneous')
      end
    end
    object TabFields: TTabSheet
      Caption = 'Private Fields'
      object ListBoxFields: TListBox
        Left = 8
        Top = 40
        Width = 249
        Height = 161
        ItemHeight = 13
        Sorted = True
        TabOrder = 3
        OnClick = ListBoxFieldsClick
      end
      object BtnAdd: TButton
        Left = 264
        Top = 8
        Width = 73
        Height = 23
        Caption = 'Add'
        Enabled = False
        TabOrder = 0
        OnClick = BtnAddClick
      end
      object BtnDelete: TButton
        Left = 264
        Top = 40
        Width = 73
        Height = 23
        Caption = 'Delete'
        Enabled = False
        TabOrder = 1
        OnClick = BtnDeleteClick
      end
      object EditAddField: TEdit
        Left = 8
        Top = 8
        Width = 249
        Height = 21
        TabOrder = 2
        OnChange = EditAddFieldChange
        OnKeyPress = EditAddFieldKeyPress
      end
    end
  end
  object BtnStart: TButton
    Left = 288
    Top = 257
    Width = 75
    Height = 23
    Caption = 'Start!'
    Default = True
    TabOrder = 0
    OnClick = BtnStartClick
  end
  object OpenDialog: TOpenDialog
    Filter = 'Grammar file (*.msg)|*.msg'
    Left = 320
    Top = 184
  end
  object MainMenu: TMainMenu
    Left = 288
    Top = 184
    object MnuFile: TMenuItem
      Caption = '&File'
      object MnuOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = MnuOpenClick
      end
      object MnuExit: TMenuItem
        Caption = 'E&xit'
        ShortCut = 32883
        OnClick = MnuExitClick
      end
    end
    object MnuStart: TMenuItem
      Caption = '&Start!'
      OnClick = BtnStartClick
    end
  end
end
