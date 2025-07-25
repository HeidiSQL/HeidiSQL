object ProjectManagerPanel: TProjectManagerPanel
  Left = 0
  Top = 0
  Width = 250
  Height = 400
  Caption = 'Project Manager'
  TabOrder = 0
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 30
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblProjectTitle: TLabel
      Left = 8
      Top = 8
      Width = 43
      Height = 13
      Caption = 'Projects'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object ToolBarProjects: TToolBar
      Left = 72
      Top = 0
      Width = 178
      Height = 30
      Align = alRight
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 22
      Caption = 'ToolBarProjects'
      Images = MainForm.VirtualImageListMain
      ShowHint = True
      TabOrder = 0
      object btnAddProject: TToolButton
        Left = 0
        Top = 0
        Hint = 'Add project folder'
        ImageIndex = 45
        ImageName = 'icons8-add'
        OnClick = menuAddProjectClick
      end
      object btnRemoveProject: TToolButton
        Left = 22
        Top = 0
        Hint = 'Remove project folder'
        ImageIndex = 46
        ImageName = 'icons8-delete-button'
        OnClick = menuRemoveProjectClick
      end
      object btnSeparator1: TToolButton
        Left = 44
        Top = 0
        Width = 8
        Style = tbsSeparator
      end
      object btnRefreshProject: TToolButton
        Left = 52
        Top = 0
        Hint = 'Refresh projects'
        ImageIndex = 16
        ImageName = 'icons8-refresh'
        OnClick = menuRefreshProjectClick
      end
    end
  end
  object pnlTree: TPanel
    Left = 0
    Top = 30
    Width = 250
    Height = 370
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object TreeProjects: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 250
      Height = 370
      Align = alClient
      Header.AutoSizeIndex = 0
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      Images = MainForm.VirtualImageListMain
      PopupMenu = PopupProjects
      TabOrder = 0
      TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages, toShowRoot]
      TreeOptions.SelectionOptions = [toFullRowSelect]
      OnBeforeExpansion = TreeProjectsBeforeExpansion
      OnCollapsed = TreeProjectsCollapsed
      OnDblClick = TreeProjectsDblClick
      OnExpanded = TreeProjectsExpanded
      OnGetImageIndex = TreeProjectsGetImageIndex
      OnGetNodeDataSize = TreeProjectsGetNodeDataSize
      OnGetText = TreeProjectsGetText
      OnInitChildren = TreeProjectsInitChildren
      OnInitNode = TreeProjectsInitNode
      Columns = <
        item
          Position = 0
          Text = 'Name'
          Width = 246
        end>
    end
  end
  object PopupProjects: TPopupMenu
    OnPopup = PopupProjectsPopup
    Left = 104
    Top = 168
    object menuAddProject: TMenuItem
      Caption = 'Add Project Folder...'
      ImageIndex = 45
      ImageName = 'icons8-add'
      OnClick = menuAddProjectClick
    end
    object menuRemoveProject: TMenuItem
      Caption = 'Remove Project'
      ImageIndex = 46
      ImageName = 'icons8-delete-button'
      OnClick = menuRemoveProjectClick
    end
    object menuSeparator1: TMenuItem
      Caption = '-'
    end
    object menuOpenFile: TMenuItem
      Caption = 'Open File'
      Default = True
      ImageIndex = 8
      ImageName = 'icons8-opened-folder'
      OnClick = menuOpenFileClick
    end
    object menuOpenInExplorer: TMenuItem
      Caption = 'Open in Explorer'
      ImageIndex = 71
      ImageName = 'icons8-folder'
      OnClick = menuOpenInExplorerClick
    end
    object menuRefreshProject: TMenuItem
      Caption = 'Refresh'
      ImageIndex = 16
      ImageName = 'icons8-refresh'
      OnClick = menuRefreshProjectClick
    end
  end
end
