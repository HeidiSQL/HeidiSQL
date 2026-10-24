object RoleManagerMssqlForm: TRoleManagerMssqlForm
  Left = 0
  Top = 0
  Caption = 'MS SQL Server login manager'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Constraints.MinHeight = 520
  Constraints.MinWidth = 800
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    900
    600)
  TextHeight = 14
  object pnlRight: TPanel
    Left = 426
    Top = 0
    Width = 474
    Height = 560
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object pnlMembership: TPanel
      Left = 0
      Top = 250
      Width = 474
      Height = 310
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object treeMemberships: TVirtualStringTree
        Left = 0
        Top = 15
        Width = 474
        Height = 295
        Align = alClient
        Header.AutoSizeIndex = 0
        Header.Options = [hoColumnResize, hoDrag, hoVisible]
        Header.SortColumn = 0
        TabOrder = 0
        TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
        TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
        TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
        OnChecked = treeMembershipsChecked
        OnGetNodeDataSize = treeMembershipsGetNodeDataSize
        OnGetText = treeMembershipsGetText
        Columns = <
          item
            Position = 0
            Text = 'Server role'
            Width = 460
          end>
      end
      object lblMembership: TLabel
        Left = 5
        Top = 0
        Width = 146
        Height = 14
        Align = alTop
        Caption = 'Member of server roles:'
      end
    end
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 474
      Height = 250
      ActivePage = tabAttributes
      Align = alTop
      Images = MainForm.VirtualImageListMain
      TabOrder = 1
      object tabAttributes: TTabSheet
        Caption = 'Attributes'
        ImageIndex = 43
        object lblName: TLabel
          Left = 10
          Top = 15
          Width = 80
          Height = 14
          Caption = 'Login name:'
        end
        object editName: TEdit
          Left = 150
          Top = 12
          Width = 310
          Height = 22
          Anchors = [akTop, akLeft, akRight]
          TabOrder = 0
          OnChange = MarkModified
        end
        object groupAttributes: TGroupBox
          Left = 10
          Top = 45
          Width = 450
          Height = 120
          Anchors = [akTop, akLeft, akRight]
          Caption = 'Login attributes'
          TabOrder = 1
          object chkEnabled: TCheckBox
            Left = 12
            Top = 20
            Width = 200
            Height = 17
            Caption = 'Enabled'
            TabOrder = 0
            OnClick = MarkModified
          end
          object chkPolicy: TCheckBox
            Left = 12
            Top = 44
            Width = 200
            Height = 17
            Caption = 'Enforce password policy'
            TabOrder = 1
            OnClick = chkPolicyClick
          end
          object chkExpiration: TCheckBox
            Left = 12
            Top = 68
            Width = 200
            Height = 17
            Caption = 'Enforce password expiration'
            TabOrder = 2
            OnClick = MarkModified
          end
        end
      end
      object tabLogin: TTabSheet
        Caption = 'Login details'
        ImageIndex = 1
        object lblDefaultDatabase: TLabel
          Left = 10
          Top = 18
          Width = 105
          Height = 14
          Caption = 'Default database:'
        end
        object comboDefaultDatabase: TComboBox
          Left = 150
          Top = 15
          Width = 310
          Height = 22
          Anchors = [akTop, akLeft, akRight]
          TabOrder = 0
          OnChange = MarkModified
        end
        object lblPassword: TLabel
          Left = 10
          Top = 65
          Width = 53
          Height = 14
          Caption = 'Password:'
        end
        object editPassword: TEdit
          Left = 150
          Top = 62
          Width = 310
          Height = 22
          Anchors = [akTop, akLeft, akRight]
          PasswordChar = '*'
          TabOrder = 1
          TextHint = 'Empty = keep current password'
          OnChange = MarkModified
        end
      end
    end
  end
  object splitterLeft: TSplitter
    Left = 420
    Top = 0
    Width = 6
    Height = 560
    Cursor = crSizeWE
    ResizeStyle = rsUpdate
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 0
    Width = 420
    Height = 560
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object treeLogins: TVirtualStringTree
      Left = 0
      Top = 52
      Width = 420
      Height = 508
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Options = [hoColumnResize, hoDrag, hoVisible]
      Header.SortColumn = 0
      Images = MainForm.VirtualImageListMain
      TabOrder = 0
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
      OnFocusChanged = treeLoginsFocusChanged
      OnGetImageIndex = treeLoginsGetImageIndex
      OnGetNodeDataSize = treeLoginsGetNodeDataSize
      OnGetText = treeLoginsGetText
      Columns = <
        item
          Position = 0
          Text = 'Login'
          Width = 130
        end
        item
          Position = 1
          Text = 'Type'
          Width = 80
        end
        item
          Position = 2
          Text = 'Enabled'
          Width = 50
        end
        item
          Position = 3
          Text = 'Default database'
          Width = 110
        end>
    end
    object editFilter: TButtonedEdit
      Left = 0
      Top = 24
      Width = 420
      Height = 22
      Align = alTop
      Images = MainForm.VirtualImageListMain
      RightButton.ImageIndex = 193
      TabOrder = 1
      TextHint = 'Filter ...'
      OnChange = editFilterChange
      OnRightButtonClick = editFilterRightButtonClick
    end
    object toolbarRoles: TToolBar
      Left = 0
      Top = 0
      Width = 420
      Height = 24
      AutoSize = True
      ButtonWidth = 60
      EdgeBorders = []
      Images = MainForm.VirtualImageListMain
      List = True
      ShowCaptions = True
      TabOrder = 2
      Wrapable = False
      object btnAdd: TToolButton
        Left = 0
        Top = 0
        Caption = 'Add'
        ImageIndex = 45
        ImageName = 'icons8-add'
        OnClick = btnAddClick
      end
      object btnClone: TToolButton
        Left = 65
        Top = 0
        Caption = 'Clone'
        ImageIndex = 3
        ImageName = 'icons8-copy-100'
        OnClick = btnCloneClick
      end
      object btnDelete: TToolButton
        Left = 130
        Top = 0
        Caption = 'Delete'
        ImageIndex = 46
        ImageName = 'icons8-delete-button'
        OnClick = btnDeleteClick
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 560
    Width = 900
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnClose: TButton
      Left = 815
      Top = 6
      Width = 80
      Height = 26
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = btnCloseClick
    end
    object btnDiscard: TButton
      Left = 729
      Top = 6
      Width = 80
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Discard'
      TabOrder = 1
      OnClick = btnDiscardClick
    end
    object btnSave: TButton
      Left = 643
      Top = 6
      Width = 80
      Height = 26
      Anchors = [akTop, akRight]
      Caption = 'Save'
      Default = True
      TabOrder = 2
      OnClick = btnSaveClick
    end
  end
end
