object UserManagerForm: TUserManagerForm
  Left = 252
  Top = 131
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'User Manager'
  ClientHeight = 394
  ClientWidth = 560
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 500
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
  OnShow = FormShow
  DesignSize = (
    560
    394)
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 544
    Height = 346
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 40
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 177
      Top = 0
      Width = 5
      Height = 346
      ResizeStyle = rsUpdate
    end
    object pnlRight: TPanel
      Left = 182
      Top = 0
      Width = 362
      Height = 346
      Align = alClient
      BevelOuter = bvNone
      Constraints.MinWidth = 20
      TabOrder = 1
      object PageControlUser: TPageControl
        Left = 0
        Top = 0
        Width = 362
        Height = 189
        ActivePage = tabSettings
        Align = alTop
        TabOrder = 0
        object tabSettings: TTabSheet
          Caption = 'Authentication'
          DesignSize = (
            354
            161)
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
          object lblWarning: TLabel
            Left = 10
            Top = 140
            Width = 341
            Height = 20
            Anchors = [akLeft, akRight, akBottom]
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
          object lblHostHints: TLabel
            Left = 10
            Top = 86
            Width = 337
            Height = 51
            Alignment = taRightJustify
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 
              '(Host: % and _ wildcards allowed)'#10'(Host: <ip>/<full netmask> syn' +
              'tax ok)'#10'(Host: server skips name resolve?: $SNR)'
            Enabled = False
            WordWrap = True
          end
          object editPassword: TButtonedEdit
            Left = 100
            Top = 33
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            Images = MainForm.ImageListMain
            PasswordChar = '*'
            RightButton.ImageIndex = 0
            RightButton.Visible = True
            TabOrder = 1
            OnExit = editPasswordExit
            OnRightButtonClick = editPasswordRightButtonClick
          end
          object editFromHost: TEdit
            Left = 100
            Top = 59
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            OnChange = editFromHostChange
          end
          object editUsername: TEdit
            Left = 100
            Top = 7
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            OnChange = editUsernameChange
          end
          object chkDisabled: TCheckBox
            Left = 8
            Top = 83
            Width = 105
            Height = 17
            Alignment = taLeftJustify
            Caption = '&Disable login:'
            TabOrder = 3
            OnClick = chkDisabledClick
          end
        end
        object tabLimitations: TTabSheet
          Caption = 'Limitations'
          ImageIndex = 1
          DesignSize = (
            354
            161)
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
            Top = 38
            Width = 186
            Height = 13
            Caption = 'Maximum number of &updates per hour:'
            FocusControl = editMaxUpdates
          end
          object lblMaxConnections: TLabel
            Left = 10
            Top = 66
            Width = 204
            Height = 13
            Caption = 'Maximum number of &connections per hour:'
            FocusControl = editMaxConnections
          end
          object lblMaxUserConnections: TLabel
            Left = 10
            Top = 94
            Width = 225
            Height = 13
            Caption = 'Maximum number of &simultaneous connections:'
            FocusControl = editMaxUserConnections
          end
          object lblLimitHint: TLabel
            Left = 0
            Top = 128
            Width = 133
            Height = 13
            Caption = '(Use 0 to indicate unlimited)'
          end
          object editMaxUserConnections: TEdit
            Left = 267
            Top = 91
            Width = 64
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 6
            Text = '0'
            OnChange = editLimitations
          end
          object editMaxConnections: TEdit
            Left = 267
            Top = 63
            Width = 64
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 4
            Text = '0'
            OnChange = editLimitations
          end
          object editMaxUpdates: TEdit
            Left = 267
            Top = 35
            Width = 64
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 2
            Text = '0'
            OnChange = editLimitations
          end
          object editMaxQuestions: TEdit
            Left = 267
            Top = 7
            Width = 64
            Height = 21
            Anchors = [akTop, akRight]
            TabOrder = 0
            Text = '0'
            OnChange = editLimitations
          end
          object udMaxQuestions: TUpDown
            Left = 331
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
            Left = 331
            Top = 35
            Width = 16
            Height = 21
            Anchors = [akTop, akRight]
            Associate = editMaxUpdates
            Max = 32767
            TabOrder = 3
            Wrap = True
          end
          object udMaxConnections: TUpDown
            Left = 331
            Top = 63
            Width = 16
            Height = 21
            Anchors = [akTop, akRight]
            Associate = editMaxConnections
            Max = 32767
            TabOrder = 5
            Wrap = True
          end
          object udMaxUserConnections: TUpDown
            Left = 331
            Top = 91
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
            354
            161)
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
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 60
            TabOrder = 0
            Text = 'editFullName'
          end
          object editDescription: TEdit
            Left = 100
            Top = 33
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 255
            TabOrder = 1
            Text = 'editDescription'
          end
          object editEmail: TEdit
            Left = 100
            Top = 59
            Width = 247
            Height = 21
            Anchors = [akLeft, akTop, akRight]
            MaxLength = 80
            TabOrder = 2
            Text = 'editEmail'
          end
          object memoContactInfo: TMemo
            Left = 100
            Top = 86
            Width = 247
            Height = 67
            Anchors = [akLeft, akTop, akRight]
            Lines.Strings = (
              'memoContactInfo')
            MaxLength = 65535
            ScrollBars = ssVertical
            TabOrder = 3
          end
        end
        object tabHints: TTabSheet
          Caption = 'Hints'
          ImageIndex = 3
          object lblHints: TLabel
            AlignWithMargins = True
            Left = 5
            Top = 10
            Width = 344
            Height = 146
            Margins.Left = 5
            Margins.Top = 10
            Margins.Right = 5
            Margins.Bottom = 5
            Align = alClient
            AutoSize = False
            Caption = 
              'When no account match during login, the Everybody account is tri' +
              'ed with the user name ignored, if the account exists.'#10#10'If multip' +
              'le accounts match host+user during login, the first account in s' +
              'orted order is tried.'#10#10'After login, privileges are given based o' +
              'n ALL accounts that match host+user, rather than just those priv' +
              'ileges that apply to the authenticated account.'
            WordWrap = True
          end
        end
      end
      object tlbObjects: TToolBar
        Left = 0
        Top = 189
        Width = 362
        Height = 22
        AutoSize = True
        ButtonWidth = 109
        Caption = 'tlbObjects'
        Images = MainForm.ImageListMain
        List = True
        ParentShowHint = False
        ShowCaptions = True
        ShowHint = True
        TabOrder = 1
        object btnAddObject: TToolButton
          Left = 0
          Top = 0
          Hint = 'Add object ...'
          Caption = 'Add privilege'
          Enabled = False
          ImageIndex = 45
          OnClick = btnAddObjectClick
        end
        object btnDeleteObject: TToolButton
          Left = 109
          Top = 0
          Hint = 'Remove access to object ...'
          Caption = 'Remove privilege'
          Enabled = False
          ImageIndex = 46
          OnClick = btnDeleteObjectClick
        end
      end
      object treeObjects: TVirtualStringTree
        Left = 0
        Top = 211
        Width = 362
        Height = 135
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.MainColumn = -1
        Header.ParentFont = True
        Images = MainForm.ImageListMain
        IncrementalSearch = isAll
        TabOrder = 2
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
        OnBeforePaint = treeObjectsBeforePaint
        OnChecked = treeObjectsChecked
        OnExpanded = treeObjectsExpanded
        OnFocusChanged = treeObjectsFocusChanged
        OnGetText = treeObjectsGetText
        OnGetImageIndex = treeObjectsGetImageIndex
        OnInitChildren = treeObjectsInitChildren
        OnInitNode = treeObjectsInitNode
        Columns = <>
      end
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 177
      Height = 346
      Align = alLeft
      BevelOuter = bvNone
      Constraints.MinWidth = 20
      TabOrder = 0
      object listUsers: TVirtualStringTree
        Left = 0
        Top = 22
        Width = 177
        Height = 324
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.DefaultHeight = 17
        Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs]
        Header.ParentFont = True
        Images = MainForm.ImageListMain
        IncrementalSearch = isAll
        NodeDataSize = 0
        TabOrder = 1
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toUseExplorerTheme, toHideTreeLinesIfThemed]
        TreeOptions.SelectionOptions = [toFullRowSelect]
        OnBeforePaint = listUsersBeforePaint
        OnFocusChanged = listUsersFocusChanged
        OnGetText = listUsersGetText
        OnGetImageIndex = listUsersGetImageIndex
        Columns = <
          item
            Position = 0
            Width = 173
          end>
      end
      object tlbUsers: TToolBar
        Left = 0
        Top = 0
        Width = 177
        Height = 22
        AutoSize = True
        ButtonWidth = 82
        Caption = 'tlbUsers'
        Images = MainForm.ImageListMain
        List = True
        ParentShowHint = False
        ShowCaptions = True
        ShowHint = True
        TabOrder = 0
        object btnAddUser: TToolButton
          Left = 0
          Top = 0
          Hint = 'Create user ...'
          Caption = 'Add user'
          ImageIndex = 45
          OnClick = btnAddUserClick
        end
        object btnDeleteUser: TToolButton
          Left = 82
          Top = 0
          Hint = 'Delete user ...'
          Caption = 'Delete user'
          ImageIndex = 46
          OnClick = btnDeleteUserClick
        end
      end
    end
  end
  object btnOK: TButton
    Left = 395
    Top = 361
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 477
    Top = 361
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
