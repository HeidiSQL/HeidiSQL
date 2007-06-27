object UserManagerForm: TUserManagerForm
  Left = 231
  Top = 131
  Width = 551
  Height = 417
  BorderWidth = 5
  Caption = 'User-Manager'
  Color = clBtnFace
  Constraints.MaxHeight = 800
  Constraints.MaxWidth = 551
  Constraints.MinHeight = 417
  Constraints.MinWidth = 551
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 533
    Height = 343
    ActivePage = TabSheetAddUser
    Align = alClient
    Images = ImageList1
    TabHeight = 22
    TabIndex = 0
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheetAddUser: TTabSheet
      Caption = 'Add User'
      object Label4: TLabel
        Left = 16
        Top = 40
        Width = 51
        Height = 13
        Caption = '&Username:'
        FocusControl = EditUser
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 16
        Top = 88
        Width = 49
        Height = 13
        Caption = '&Password:'
        FocusControl = EditPassword
      end
      object Label6: TLabel
        Left = 16
        Top = 64
        Width = 51
        Height = 13
        Caption = 'From &Host:'
        FocusControl = EditHost
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 256
        Top = 16
        Width = 94
        Height = 13
        Caption = 'Allow a&ccess to:'
        FocusControl = DBUserTree
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label8: TLabel
        Left = 16
        Top = 128
        Width = 60
        Height = 13
        Caption = 'Privileges:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 16
        Top = 16
        Width = 68
        Height = 13
        Caption = 'Credentials:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel1: TBevel
        Left = 16
        Top = 120
        Width = 209
        Height = 9
        Shape = bsTopLine
      end
      object Label1: TLabel
        Left = 274
        Top = 272
        Width = 56
        Height = 13
        Caption = '&Description:'
        Enabled = False
        FocusControl = EditDescription
      end
      object DBUserTree: TTreeView
        Left = 256
        Top = 40
        Width = 257
        Height = 193
        Indent = 19
        ReadOnly = True
        RowSelect = True
        ShowLines = False
        ShowRoot = False
        TabOrder = 3
      end
      object EditUser: TEdit
        Left = 96
        Top = 40
        Width = 129
        Height = 21
        TabOrder = 0
      end
      object EditPassword: TEdit
        Left = 96
        Top = 88
        Width = 129
        Height = 21
        PasswordChar = '*'
        TabOrder = 2
      end
      object EditHost: TEdit
        Left = 96
        Top = 64
        Width = 129
        Height = 21
        TabOrder = 1
        Text = '%'
      end
      object CheckListBoxPrivileges: TCheckListBox
        Left = 96
        Top = 152
        Width = 129
        Height = 113
        ItemHeight = 13
        Items.Strings = (
          'Select'
          'Insert'
          'Update'
          'Delete'
          'Index'
          'Alter'
          'Create'
          'Drop'
          'References'
          'Reload'
          'Shutdown'
          'Process'
          'File')
        TabOrder = 5
      end
      object CheckBoxAllPrivileges: TCheckBox
        Left = 96
        Top = 128
        Width = 105
        Height = 17
        Caption = '&All Privileges'
        TabOrder = 4
        OnClick = CheckBoxAllPrivilegesClick
      end
      object CheckBoxWithGrant: TCheckBox
        Left = 96
        Top = 272
        Width = 105
        Height = 17
        Caption = '&With Grant Option'
        TabOrder = 6
      end
      object CheckBoxCreateAccount: TCheckBox
        Left = 256
        Top = 248
        Width = 249
        Height = 17
        Caption = 'Create Connection-Account for &MySQL-Front'
        TabOrder = 7
        OnClick = CheckBoxCreateAccountClick
      end
      object EditDescription: TEdit
        Left = 336
        Top = 270
        Width = 177
        Height = 21
        Enabled = False
        TabOrder = 8
      end
    end
    object TabSheetEditUsers: TTabSheet
      Caption = 'Edit Users'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 280
        Top = 0
        Width = 245
        Height = 311
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object Label13: TLabel
          Left = 14
          Top = 82
          Width = 38
          Height = 13
          Caption = 'Column:'
          Visible = False
        end
        object LabelColumn: TLabel
          Left = 84
          Top = 82
          Width = 61
          Height = 13
          Caption = 'LabelColumn'
          Visible = False
        end
        object LabelTable: TLabel
          Left = 84
          Top = 66
          Width = 53
          Height = 13
          Caption = 'LabelTable'
          Visible = False
        end
        object LabelDB: TLabel
          Left = 84
          Top = 50
          Width = 41
          Height = 13
          Caption = 'LabelDB'
          Visible = False
        end
        object Label12: TLabel
          Left = 14
          Top = 66
          Width = 30
          Height = 13
          Caption = 'Table:'
          Visible = False
        end
        object Label11: TLabel
          Left = 14
          Top = 50
          Width = 49
          Height = 13
          Caption = 'Database:'
          Visible = False
        end
        object LabelPrivileges: TLabel
          Left = 14
          Top = 8
          Width = 60
          Height = 13
          Caption = 'Privileges:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          Visible = False
          WordWrap = True
        end
        object LabelPleaseSelect: TLabel
          Left = 40
          Top = 96
          Width = 142
          Height = 39
          Caption = 
            'Please select a user or doubleclick on a user to access items be' +
            'low him.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object LabelNoPrivs: TLabel
          Left = 24
          Top = 120
          Width = 195
          Height = 13
          Caption = 'No Privileges are set for the selected item'
          Visible = False
        end
        object LabelUser: TLabel
          Left = 14
          Top = 32
          Width = 3
          Height = 13
        end
        object CheckListBoxPrivs: TCheckListBox
          Left = 14
          Top = 106
          Width = 91
          Height = 193
          OnClickCheck = CheckListBoxPrivsClickCheck
          ItemHeight = 13
          Items.Strings = (
            'Select'
            'Insert'
            'Update'
            'Delete'
            'Create'
            'Drop'
            'Reload'
            'Shutdown'
            'Process'
            'File'
            'Grant'
            'References'
            'Index'
            'Alter')
          TabOrder = 0
          Visible = False
        end
        object ButtonSelectAll: TButton
          Left = 112
          Top = 170
          Width = 100
          Height = 25
          Caption = 'Select all'
          TabOrder = 1
          Visible = False
          OnClick = ButtonSelectAllClick
        end
        object ButtonSelectNone: TButton
          Left = 112
          Top = 202
          Width = 100
          Height = 25
          Caption = 'Select none'
          TabOrder = 2
          Visible = False
          OnClick = ButtonSelectNoneClick
        end
        object ButtonSet: TButton
          Left = 112
          Top = 242
          Width = 100
          Height = 25
          Caption = 'Grant Privileges'
          Enabled = False
          TabOrder = 3
          Visible = False
          OnClick = ButtonSetClick
        end
        object ButtonSelectPrivileges: TButton
          Left = 72
          Top = 136
          Width = 113
          Height = 25
          Caption = 'Specify Privileges'
          TabOrder = 4
          OnClick = ButtonSelectPrivilegesClick
        end
        object ButtonRevoke: TButton
          Left = 112
          Top = 272
          Width = 100
          Height = 25
          Caption = 'Revoke Privileges'
          TabOrder = 5
          OnClick = ButtonRevokeClick
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 280
        Height = 311
        Align = alClient
        BevelOuter = bvNone
        BorderWidth = 5
        Caption = 'Panel2'
        TabOrder = 1
        object Label2: TLabel
          Left = 5
          Top = 5
          Width = 270
          Height = 13
          Align = alTop
          Caption = 'Registered users:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object TreeViewUsers: TTreeView
          Left = 5
          Top = 18
          Width = 270
          Height = 253
          Align = alClient
          ChangeDelay = 50
          HotTrack = True
          Indent = 19
          ReadOnly = True
          TabOrder = 0
          OnChange = TreeViewUsersChange
          OnDblClick = TreeViewUsersDblClick
        end
        object Panel4: TPanel
          Left = 5
          Top = 271
          Width = 270
          Height = 35
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object ButtonEditUser: TButton
            Left = 0
            Top = 8
            Width = 105
            Height = 25
            Caption = 'Edit User...'
            Enabled = False
            TabOrder = 0
            OnClick = ButtonEditUserClick
          end
          object Button1: TButton
            Left = 112
            Top = 8
            Width = 105
            Height = 25
            Caption = 'Refresh'
            TabOrder = 1
            OnClick = Button1Click
          end
        end
      end
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 343
    Width = 533
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object ButtonClose: TButton
      Left = 456
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = ButtonCloseClick
    end
    object ButtonAddUser: TButton
      Left = 368
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add User'
      Default = True
      TabOrder = 1
      OnClick = ButtonAddUserClick
    end
  end
  object ImageList1: TImageList
    Left = 200
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000099000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000099000000000000000000000000000080000000800000008000
      0000800000008000000080000000800000008000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000099
      000000990000009900000099000000990000000000000000000080000000FF00
      0000FF000000C0C0C000C0C0C00000000000FF00000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000099000000FF000000FF000000CCCCCC00CCCCCC0000000000FF0000000000
      000000000000009900000000000000000000000000000000000000000000C0C0
      C000000000000000000000000000000000000000000000800000008000000080
      0000008000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC00CCCCFF00000000000000000000000000000000000000
      0000000000000099000000000000000000000000000000000000000000000000
      00000000000000000000C0C0C000C0C0C000C0C0C0000000000000FF00000000
      000000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000099CCFF00CCCCFF00CCCCCC00CCCCCC00CCCCCC000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000000000000000000000000000000000000000000000000000808080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC0099CCFF0099CCFF00CCCCFF00CCCCFF00CCFFFF000000
      00000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC0099CCFF0099CCFF00CCCCFF00CCCCFF00CCFF
      FF000000000000000000000000000000000000000000C0C0C000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000008080
      8000808080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC00CCCCCC00CCCCCC00CCCCCC000000000099CCFF0099CCFF00CCCCFF00CCCC
      FF00000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CCCCCC00CCCCCC00CCCCCC000000000099CCFF0099CCFF00000000000000
      00000000000000000000000000000000000000000000C0C0C00000000000C0C0
      C000C0C0C0000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000CCCC
      CC0000000000CCCCCC00CCCCCC000000000066CCFF0099CCFF0099CCFF000000
      0000000000000000000000000000000000000000000000000000C0C0C0000000
      0000C0C0C000C0C0C00000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C000C0C0C00000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000CCCCCC0000000000CCCCCC00CCCCCC000000000066CCFF0099CCFF000000
      000000000000000000000000000000000000000000000000000000000000C0C0
      C00000000000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C00000000000C0C0
      C000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000CCCCCC0000000000CCCCCC00CCCCCC00CCCCCC00CCCCCC00CCCC
      CC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000080808000C0C0C000808080008080800000000000C0C0
      C000C0C0C0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000080808000C0C0C00080808000808080008080
      8000808080008080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFBFFFF00000000FFFB807F00000000
      FFE0C07F00000000F01BC80700000000F01B8C0700000000C00F0F8F00000000
      C00F07C700000000C00703C300000000C007430300000000D00F238100000000
      C80F918100000000E40FC80300000000F207E00300000000F80FF80300000000
      FFFFFC0100000000FFFFFE030000000000000000000000000000000000000000
      000000000000}
  end
end
