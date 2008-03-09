object UserManagerForm: TUserManagerForm
  Left = 252
  Top = 131
  BorderWidth = 5
  Caption = 'User-Manager'
  ClientHeight = 380
  ClientWidth = 533
  Color = clBtnFace
  Constraints.MinHeight = 417
  Constraints.MinWidth = 551
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
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
    Images = MainForm.PngImageListMain
    TabHeight = 22
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheetAddUser: TTabSheet
      Caption = 'Add User'
      ImageIndex = 21
      object Label4: TLabel
        Left = 16
        Top = 40
        Width = 52
        Height = 13
        Caption = '&Username:'
        FocusControl = EditUser
      end
      object Label5: TLabel
        Left = 16
        Top = 88
        Width = 50
        Height = 13
        Caption = '&Password:'
        FocusControl = EditPassword
      end
      object Label6: TLabel
        Left = 16
        Top = 64
        Width = 53
        Height = 13
        Caption = 'From &Host:'
        FocusControl = EditHost
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object Label7: TLabel
        Left = 256
        Top = 16
        Width = 89
        Height = 13
        Caption = 'Allow a&ccess to:'
        FocusControl = DBUserTree
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label8: TLabel
        Left = 16
        Top = 128
        Width = 58
        Height = 13
        Caption = 'Privileges:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label9: TLabel
        Left = 16
        Top = 16
        Width = 67
        Height = 13
        Caption = 'Credentials:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
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
        Width = 57
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
        Images = MainForm.PngImageListMain
        Indent = 19
        ReadOnly = True
        RowSelect = True
        ShowLines = False
        ShowRoot = False
        TabOrder = 3
        OnExpanding = DBUserTreeExpanding
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
        Caption = 'Create Connection-Account for appname'
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
      ImageIndex = 12
      object Panel1: TPanel
        Left = 280
        Top = 0
        Width = 245
        Height = 311
        Align = alRight
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          245
          311)
        object Label13: TLabel
          Left = 14
          Top = 82
          Width = 39
          Height = 13
          Caption = 'Column:'
          Visible = False
        end
        object LabelColumn: TLabel
          Left = 84
          Top = 82
          Width = 60
          Height = 13
          Caption = 'LabelColumn'
          Visible = False
        end
        object LabelTable: TLabel
          Left = 84
          Top = 66
          Width = 51
          Height = 13
          Caption = 'LabelTable'
          Visible = False
        end
        object LabelDB: TLabel
          Left = 84
          Top = 50
          Width = 38
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
          Width = 50
          Height = 13
          Caption = 'Database:'
          Visible = False
        end
        object LabelPrivileges: TLabel
          Left = 14
          Top = 8
          Width = 58
          Height = 13
          Caption = 'Privileges:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          Visible = False
          WordWrap = True
        end
        object LabelPleaseSelect: TLabel
          Left = 40
          Top = 96
          Width = 136
          Height = 39
          Caption = 
            'Please select a user or doubleclick on a user to access items be' +
            'low him.'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          WordWrap = True
        end
        object LabelNoPrivs: TLabel
          Left = 24
          Top = 120
          Width = 200
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
          Width = 107
          Height = 193
          OnClickCheck = CheckListBoxPrivsClickCheck
          Anchors = [akLeft, akTop, akRight, akBottom]
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
          Left = 136
          Top = 172
          Width = 100
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Select all'
          TabOrder = 1
          Visible = False
          OnClick = ButtonSelectAllClick
        end
        object ButtonSelectNone: TButton
          Left = 136
          Top = 204
          Width = 100
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'Select none'
          TabOrder = 2
          Visible = False
          OnClick = ButtonSelectNoneClick
        end
        object ButtonSet: TButton
          Left = 136
          Top = 244
          Width = 100
          Height = 25
          Anchors = [akRight, akBottom]
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
          Left = 136
          Top = 274
          Width = 100
          Height = 25
          Anchors = [akRight, akBottom]
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
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object TreeViewUsers: TTreeView
          Left = 5
          Top = 18
          Width = 270
          Height = 253
          Align = alClient
          Anchors = [akLeft, akTop, akBottom]
          ChangeDelay = 50
          HotTrack = True
          Images = MainForm.PngImageListMain
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
    DesignSize = (
      533
      37)
    object ButtonClose: TButton
      Left = 456
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 0
    end
    object ButtonAddUser: TButton
      Left = 368
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Add User'
      Default = True
      TabOrder = 1
      OnClick = ButtonAddUserClick
    end
  end
end
