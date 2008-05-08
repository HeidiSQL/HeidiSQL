object UserManagerForm: TUserManagerForm
  Left = 252
  Top = 131
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'User Manager'
  ClientHeight = 446
  ClientWidth = 352
  Color = clBtnFace
  Constraints.MinHeight = 473
  Constraints.MinWidth = 360
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
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    352
    446)
  PixelsPerInch = 96
  TextHeight = 13
  object lblUser: TLabel
    Left = 8
    Top = 11
    Width = 29
    Height = 13
    Caption = '&User:'
    FocusControl = comboUsers
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object comboUsers: TComboBoxEx
    Left = 56
    Top = 8
    Width = 235
    Height = 22
    ItemsEx = <>
    Style = csExDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 0
    OnChange = comboUsersChange
    Images = MainForm.PngImageListMain
  end
  object btnCancel: TButton
    Left = 269
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 187
    Top = 413
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
  object comboObjects: TComboBoxEx
    Left = 8
    Top = 232
    Width = 283
    Height = 22
    ItemsEx = <>
    Style = csExDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 16
    TabOrder = 3
    OnChange = comboObjectsChange
    Images = MainForm.PngImageListMain
  end
  object boxPrivs: TCheckListBox
    Left = 8
    Top = 260
    Width = 336
    Height = 147
    OnClickCheck = boxPrivsClickCheck
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = 3
    ItemHeight = 13
    TabOrder = 4
  end
  object tlbObjects: TToolBar
    Left = 297
    Top = 232
    Width = 46
    Height = 22
    Align = alNone
    Anchors = [akTop, akRight]
    AutoSize = True
    Caption = 'tlbObjects'
    Images = MainForm.PngImageListMain
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    object btnAddObject: TToolButton
      Left = 0
      Top = 0
      Hint = 'Add object ...'
      ImageIndex = 45
      OnClick = btnAddObjectClick
    end
    object btnDeleteObject: TToolButton
      Left = 23
      Top = 0
      Hint = 'Remove access to object ...'
      ImageIndex = 46
      OnClick = btnDeleteObjectClick
    end
  end
  object tlbUsers: TToolBar
    Left = 297
    Top = 8
    Width = 46
    Height = 22
    Align = alNone
    Anchors = [akTop, akRight]
    AutoSize = True
    Caption = 'tlbUsers'
    Images = MainForm.PngImageListMain
    ParentShowHint = False
    ShowHint = True
    TabOrder = 6
    object btnAddUser: TToolButton
      Left = 0
      Top = 0
      Hint = 'Create user ...'
      ImageIndex = 45
      OnClick = btnAddUserClick
    end
    object btnDeleteUser: TToolButton
      Left = 23
      Top = 0
      Hint = 'Delete user ...'
      ImageIndex = 46
      OnClick = btnDeleteUserClick
    end
  end
  object chkTogglePrivs: TCheckBox
    Left = 8
    Top = 417
    Width = 129
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Select / Deselect all'
    TabOrder = 7
    OnClick = chkTogglePrivsClick
  end
  object PageControlUser: TPageControl
    Left = 8
    Top = 36
    Width = 335
    Height = 190
    ActivePage = tabSettings
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    object tabSettings: TTabSheet
      Caption = 'Settings'
      DesignSize = (
        327
        162)
      object lblFromHost: TLabel
        Left = 10
        Top = 62
        Width = 52
        Height = 13
        Caption = 'From &host:'
        FocusControl = editFromHost
      end
      object lblPassword: TLabel
        Left = 10
        Top = 36
        Width = 50
        Height = 13
        Caption = '&Password:'
        FocusControl = editPassword
      end
      object lblUsername: TLabel
        Left = 10
        Top = 10
        Width = 55
        Height = 13
        Caption = 'User &name:'
      end
      object lblHostHint: TLabel
        Left = 100
        Top = 84
        Width = 164
        Height = 13
        Caption = '(Host: % and _ wildcards allowed)'
      end
      object lblWarning: TLabel
        Left = 10
        Top = 136
        Width = 314
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 'Security warning'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        WordWrap = True
      end
      object editPassword: TEdit
        Left = 100
        Top = 33
        Width = 220
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'editPassword'
        OnChange = editPasswordChange
        OnEnter = editPasswordEnter
        OnExit = editPasswordExit
      end
      object editFromHost: TEdit
        Left = 100
        Top = 59
        Width = 220
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'editFromHost'
        OnChange = editFromHostChange
      end
      object chkDisabled: TCheckBox
        Left = 10
        Top = 105
        Width = 103
        Height = 17
        Alignment = taLeftJustify
        Caption = '&Disable account:'
        TabOrder = 3
        OnClick = chkDisabledClick
      end
      object editUsername: TEdit
        Left = 100
        Top = 7
        Width = 220
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'editUsername'
        OnChange = editUsernameChange
      end
    end
    object tabLimitations: TTabSheet
      Caption = 'Limitations'
      ImageIndex = 1
      DesignSize = (
        327
        162)
      object lblMaxQuestions: TLabel
        Left = 10
        Top = 10
        Width = 182
        Height = 13
        Caption = 'Maximum number of &queries per hour:'
        FocusControl = editMaxQuestions
      end
      object lblMaxUpdates: TLabel
        Left = 10
        Top = 34
        Width = 186
        Height = 13
        Caption = 'Maximum number of &updates per hour:'
        FocusControl = editMaxUpdates
      end
      object lblMaxConnections: TLabel
        Left = 10
        Top = 58
        Width = 204
        Height = 13
        Caption = 'Maximum number of &connections per hour:'
        FocusControl = editMaxConnections
      end
      object lblMaxUserConnections: TLabel
        Left = 10
        Top = 82
        Width = 225
        Height = 13
        Caption = 'Maximum number of &simultaneous connections:'
        FocusControl = editMaxUserConnections
      end
      object lblLimitHint: TLabel
        Left = 184
        Top = 138
        Width = 133
        Height = 13
        Caption = '(Use 0 to indicate unlimited)'
      end
      object editMaxUserConnections: TEdit
        Left = 240
        Top = 79
        Width = 64
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 6
        Text = '0'
      end
      object editMaxConnections: TEdit
        Left = 240
        Top = 55
        Width = 64
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 4
        Text = '0'
      end
      object editMaxUpdates: TEdit
        Left = 240
        Top = 31
        Width = 64
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 2
        Text = '0'
      end
      object editMaxQuestions: TEdit
        Left = 240
        Top = 7
        Width = 64
        Height = 21
        Anchors = [akTop, akRight]
        TabOrder = 0
        Text = '0'
      end
      object udMaxQuestions: TUpDown
        Left = 304
        Top = 7
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editMaxQuestions
        Max = 32767
        TabOrder = 1
        Wrap = True
      end
      object udMaxUpdates: TUpDown
        Left = 304
        Top = 31
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editMaxUpdates
        Max = 32767
        TabOrder = 3
        Wrap = True
      end
      object udMaxConnections: TUpDown
        Left = 304
        Top = 55
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editMaxConnections
        Max = 32767
        TabOrder = 5
        Wrap = True
      end
      object udMaxUserConnections: TUpDown
        Left = 304
        Top = 79
        Width = 16
        Height = 21
        Anchors = [akTop, akRight]
        Associate = editMaxUserConnections
        Max = 32767
        TabOrder = 7
        Wrap = True
      end
    end
    object tabUserInfo: TTabSheet
      Caption = 'User info'
      ImageIndex = 2
      TabVisible = False
      DesignSize = (
        327
        162)
      object lblFullName: TLabel
        Left = 10
        Top = 10
        Width = 49
        Height = 13
        Caption = '&Full name:'
      end
      object lblDescription: TLabel
        Left = 10
        Top = 36
        Width = 57
        Height = 13
        Caption = '&Description:'
      end
      object lblEmail: TLabel
        Left = 10
        Top = 62
        Width = 28
        Height = 13
        Caption = '&Email:'
      end
      object lblContactInfo: TLabel
        Left = 10
        Top = 88
        Width = 63
        Height = 13
        Caption = '&Contact info:'
      end
      object editFullName: TEdit
        Left = 100
        Top = 7
        Width = 220
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 60
        TabOrder = 0
        Text = 'editFullName'
      end
      object editDescription: TEdit
        Left = 100
        Top = 33
        Width = 220
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 255
        TabOrder = 1
        Text = 'editDescription'
      end
      object editEmail: TEdit
        Left = 100
        Top = 59
        Width = 220
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        MaxLength = 80
        TabOrder = 2
        Text = 'editEmail'
      end
      object memoContactInfo: TMemo
        Left = 100
        Top = 86
        Width = 220
        Height = 67
        Anchors = [akLeft, akTop, akRight]
        Lines.Strings = (
          'memoContactInfo')
        MaxLength = 65535
        ScrollBars = ssVertical
        TabOrder = 3
      end
    end
  end
end
