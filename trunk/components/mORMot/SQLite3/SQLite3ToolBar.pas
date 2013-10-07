/// Database-driven Office 2007 Toolbar
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3ToolBar;

interface

{

    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 3 of the License, or (at
    your option) any later version.

    This library is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this library. If not, see <http://www.gnu.org/licenses/>.



        Handle a Toolbar for Database actions
       ***************************************

      - full Office 2007 Ribbon menus are created directly from code
      - every database table has its own Ribbon tab
      - every Ribbon tab has its own buttons, corresponding to actions, as
        defined in the code in a custom enumeration type

        Initial version: 2008 March, by Arnaud Bouchez

    Version 1.1 - 14 January 2009
    - attempt to reach Delphi 2009/2010 compilation (string=UnicodeString):
      the UNICODE conditional will adapt the framework to these compilers
      (you shouldn't have to set any other conditional define)

    Version 1.4 - February 8, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.8
    - possibility to add a background picture to the ribbon pages
     (creating a TSQLAdvPage class from TAdvPage with a BackgroundPicture
     property, or directly by supplied a CSV resource name to SetToolBarGroups)

    Version 1.9
    - improved Delphi 2009/2010 UnicodeString compatibility
    - add TMS components stylers coherency update
    - added TSQLRibbon to factorize most used Ribbon-related User Interface
      data, functions and events in one class

    Version 1.9.2
    - allow TSQLRibbon.CreateReport() method not to delete the current content
      of the report (useful to specify a custom header/footer before calling
      the default CreateReport implementation)
    - new TSQLRibbon.Refresh method added
    - new AddToReport method to append the specified database Table Content
      to the report

    Version 1.13
    - now uses TSQLRecord.RecordProps instead of lowest level RTTI calls
    - by default, will use only VCL components to create the Ribbon; can use
      proprietary TMS component pack if USETMSPACK global conditional is defined

    Version 1.15
    - TSQLRibbon.AddToReport method can work with self=nil

    Version 1.16
    - includes new TSynAnsiConvert classes for handling Ansi charsets

}

uses
  Windows, Consts, Dialogs, ShellAPI,
  SysUtils, Forms, Classes, Messages, Graphics,
  ImgList, Controls, Grids, ExtCtrls, Menus, 
{$ifdef USETMSPACK}
  AdvOfficePager, AdvToolBar, AdvGlowButton, AdvMenus, AdvShapeButton, AdvPreviewMenu,
  AdvToolBarStylers, AdvPreviewMenuStylers, AdvOfficePagerStylers,
  AdvOfficeStatusBarStylers, AdvPanel,
  TaskDialog, TaskDialogEx, GDIPicture,
{$else}
  StdCtrls, ComCtrls, SynTaskDialog, Buttons, CommCtrl,
{$endif USETMSPACK}
  SynCommons, SQLite3Commons,
  SQLite3UI, SQlite3i18n, SQLite3UILogin,
  SQLite3Pages, SynGdiPlus, SynZip;


type
  /// used to mark which shortcut keys have already been affected
  TFreeShortCutSet = set of ord('A')..ord('Z');

  /// a simple object to get one char shortcuts from caption value
  TFreeShortCut = object
    /// bit set for already used short cut, from 'A' to 'Z'
    Values: TFreeShortCutSet;
    /// attempt to create free shortcut of one char length, from
    // a Caption: try every character of aCaption, from left to right
    // - returns '' if no shortcut calculation was possible
    function FindFreeShortCut(const aCaption: string): string;
  end;


{$ifndef USETMSPACK}
type
  /// a Vista-enabled TForm descendant
  // - this form will have a button in the TaskBar
  // - this form will hide the default Delphi application virtual form
  TSynForm = TVistaForm;

  /// a popup menu to be displayed
  TSynPopupMenu = TPopupMenu;

  /// a button on the Ribbon toolbars, corresponding to one action
  TSynToolButton = class(TToolButton)
  private
  public
    /// this class will have AutoSize set to true
    constructor Create(aOwner: TComponent); override;
    /// display drop down menu
    procedure DoDropDown;
    /// the associated image list, i.e. TToolBar(Owner).Images
    function Images: TCustomImageList;
  end;

  /// a toolbar on a Ribbon page
  TSynToolBar = class(TToolBar)
  public
    /// create a button on the toolbar
    function CreateToolButton(ButtonClick: TNotifyEvent;
      iAction, ImageListFirstIndex: integer; const ActionName, ActionHints: string;
      var ShortCutUsed: TFreeShortCut; ButtonWidth: integer;
      Images: TCustomImageList): TSynToolButton;
  end;

  /// a Ribbon page, which will contain some toolbars for a TSQLRecord class
  TSynPage = class(TTabSheet)
  protected
    fToolBar: array of TSynToolBar;
    fToolBarCaptionsCount: integer;
    function GetToolBarCount: integer;
    function GetToolBar(aIndex: integer): TSynToolBar;
    function GetToolBarNextLeft(var Last: TSynToolBar): integer;
  public
    /// add a TSynToolBar to the page list
    // - then call TSynToolBar.CreateToolButton to add some buttons
    function CreateToolBar(AddToList: boolean=true): TSynToolBar;
    /// call this event when all toolbars have been created
    // - it will create the captions under the toolbars
    // - can be call multiple times, when a toolbar has been added and filled
    // will all its buttons 
    procedure ToolBarCreated;
    /// number of TSynToolBar in the page list
    property ToolBarCount: integer read GetToolBarCount;
    /// access to the TSynToolBar list of this page
    property ToolBars[aIndex: integer]: TSynToolBar read GetToolBar;
  end;

  /// the ribbon pager, which will contain one page per TSQLRecord class
  TSynPager = class(TPageControl)
  private
    function GetActivePageIndex: integer;
    procedure SetActivePageIndex(const Value: integer);
    function GetHelpButton: TSynToolButton;
    function GetCaption: TLabel;
  protected
    fHelpToolBar: TSynToolBar;
    fHelpButton: TSynToolButton;
    fTopMostPanel, fTopPanel: TPanel;
    procedure Change; override;
    function GetSynPage(aIndex: integer): TSynPage; {$ifdef HASINLINE}inline;{$endif}
    procedure GroupLabelClick(Sender: TObject);
  public
    /// create the ribbon pager on a form
    // - reserve some above space for groups, caption and min/max/close buttons,
    // so that FormNoCaption method can be called later
    class function CreatePager(aOwner: TCustomForm; NoTabVisible: boolean=false): TSynPager;
    /// add a page instance
    function AddPage(aPage: TSynPage): integer; overload;
    /// create a new page with the specified caption
    function AddPage(const aCaption: string): integer; overload;
    /// create a group label starting for the given page indexes
    function TabGroupsAdd(TabIndexStart, TabIndexEnd: integer; const aCaption: string): TLabel;
    /// hide TSynForm caption bar and put caption and buttons at groups right
    procedure FormNoCaption;
    /// mimic TTabSheet.Pages property
    property Pages[aIndex: Integer]: TSynPage read GetSynPage;
    /// force OnChange event to be triggered
    property ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex;
    /// the help button to be available on the ribbon
    property HelpButton: TSynToolButton read GetHelpButton;
    /// the label on TopMostPanel, i.e. the TSynForm(Owner).NoCaption
    property Caption: TLabel read GetCaption;
    /// the panel added above the pager, containing groups, caption and buttons 
    property TopMostPanel: TPanel read fTopMostPanel;
    /// the panel containing this TSynPager 
    property TopPanel: TPanel read fTopPanel;
  published
    /// publish this property, e.g. to close a tab by a double click
    property OnDblClick;
  end;

  /// body pager used to display the list and the report on the client screen
  TSynBodyPager = TSynPager;

  /// body page used to display the list and the report on the client screen
  TSynBodyPage = TSynPage;

  
const
  bsCheck = tbsCheck;

  TOOLBAR_HEIGHT = 114;
  TOOLBAR_GROUPS_HEIGHT = 20;
  TOOLBAR_TAB_HEIGHT = 22;


{$else USETMSPACK}
type
  TSynToolButton = TAdvGlowButton;
  TSynPopupMenu = TAdvPopupMenu;
  TSynBodyPage = TAdvOfficePage;
  TSynBodyPager = TAdvOfficePager;

  /// Vista-enabled TAdvToolBarForm descendant
  // - this form will have a button in the TaskBar
  // - this form will hidde the default Delphi application virtual form
  TSynForm = class(TAdvToolBarForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSyscommand(var M: TMessage); message WM_SYSCOMMAND;
  end;

  /// a TMS toolbar
  TSynToolBar = class(TAdvToolBar)
  public
    function CreateToolButton(ButtonClick: TNotifyEvent;
      iAction, ImageListFirstIndex: integer; const ActionName, ActionHints: string;
      var ShortCutUsed: TFreeShortCut; ButtonWidth: integer;
      Images: TCustomImageList): TSynToolButton;
  end;

  /// a TMS ribbon page with an optional background picture
  // - the background picture is right-aligned
  // - if the bitmap width is not wide enough for the page, it's tiled
  TSynPage = class(TAdvPage)
  private
    fBackgroundPicture: TGDIPPicture;
    fBackgroundPictureStored: boolean;
    fBackgroundPictureTiledWidth: integer;
  protected
    procedure Paint; override;
    function GetToolBar(aIndex: integer): TSynToolBar;
  public
    destructor Destroy; override;
    /// add a TSynToolBar to the page list
    // - then call TSynToolBar.CreateToolButton to add some buttons
    function CreateToolBar: TSynToolBar;
    function ToolBarCount: integer;
    property ToolBars[aIndex: integer]: TSynToolBar read GetToolBar;
    /// set a picture to this property to draw it on background
    // - the picture must be available and freed when the page is no longer
    // needed: another possibility is to set the BackgroundPictureStored
    // property to TRUE
    property BackgroundPicture: TGDIPPicture read fBackgroundPicture write fBackgroundPicture;
    /// if this property is TRUE, the corresponding picture will be freed
    // when the instance will be released
    property BackgroundPictureStored: boolean read fBackgroundPictureStored write fBackgroundPictureStored;
    /// the pixel widths used for left-tiling the background bitmap
    // - default value is 200 pixels
    property BackgroundPictureTiledWidth: integer read fBackgroundPictureTiledWidth
      write fBackgroundPictureTiledWidth;
  end;

  /// a TMS pager
  TSynPager = class(TAdvToolBarPager)
  protected
    function GetSynPage(aIndex: integer): TSynPage;
      {$ifdef HASINLINE}inline;{$endif}
    function GetSynPageCount: integer;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// create the ribbon pager on a form
    class function CreatePager(aOwner: TCustomForm; NoTabVisible: boolean=false): TSynPager;
    /// add page instance
    function AddPage(aPage: TSynPage): integer; overload;
    /// create a new page with the specified caption
    function AddPage(const aCaption: string): integer; overload;
    /// mimic TTabSheet.Pages property
    property Pages[aIndex: Integer]: TSynPage read GetSynPage;
    /// mimic TTabSheet.PageCount property
    property PageCount: Integer read GetSynPageCount;
  end;

{$endif USETMSPACK}

type
  TSQLLister = class;

  /// this event is called when a button is pressed
  TSQLListerEvent = procedure(Sender: TObject;
    const RecordClass: TSQLRecordClass;
    ActionValue: integer) of object;

  /// a hidden component, used for handling toolbar buttons of actions
  // to be performed on a TSQLRecordClass list
  TSQLLister = class(TComponent)
  private
    fClient: TSQLRestClient;
    fPager: TSynPager;
    fPage: TSynPage;
    fOnButtonClick: TSQLListerEvent;
    fClass: TSQLRecordClass;
    fActionMax: cardinal;
    fActionHints: string;
    fGrid: TDrawGrid;
    fTableToGrid: TSQLTableToGrid;
    fMenu: TSynPopupMenu;
    fImageList16: TImageList;
    fImageList32: TImageList;
    fReportDetailedIndex: integer;
    fCreateSubMenuLastAction: integer;
    fCreateSubMenuLastButton: TSynToolButton;
    fCreateSubMenuLastMenu: TMenuItem;
  protected
    fShortCutUsed: TFreeShortCut;
    fCurrentSelectedRow: integer;
    function isActionButton(One: TObject): integer;
    procedure ActionButtonClick(Sender: TObject);
    procedure OnRightClickCell(Sender: TSQLTable; ACol, ARow, MouseX, MouseY: Integer);
    procedure OnSelectCell(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
  public
    /// retrieve the page index from a TSQLRecordClass
    // - the TSynPage tag property contains integer(aClass)
    class function FindPage(aOwner: TSynPager; aClass: TSQLRecordClass): integer;
    /// add a page (if not already) for a corresponding TSQLRecordClass
    // - the TSynPage tag property will contain integer(aClass)
    // - the TSynPage caption is expanded and translated from aClass with
    // LoadResStringTranslate(aClass.SQLTableName) or taken directly from
    // CustomCaption if a value is specified (with translation if CustomCaptionTranslate
    // is set)
    class function AddPage(aOwner: TSynPager; aClass: TSQLRecordClass;
      const CustomCaption: string; CustomCaptionTranslate: boolean): TSynPage;

    /// initialize the lister for a specified Client and Class
    // - the possible actions are retrieved from the Client TSQLModel
    // - a single page is used for a list of records, specified by their unique class
    // - a single page can share multiple toolbars
    // - both TImagelist will be used to display some images in Action buttons
    // (32 pixels wide) and Popup Menu (16 pixels wide)
    // - if aGrid has no associated TSQLTableToGrid, a default one is created
    // retrieving a list of records with aGridSelect about the aClass Table
    // from aClient, with the ID column hidden
    // (no TSQLTableToGrid will be created if aGridSelect is '')
    // - aOnButtonClick is called with a specified action if a button is clicked,
    // or with ActionValue=0 each time a row is selected
    constructor Create(aOwner: TComponent; aClient: TSQLRestClientURI;
      aClass: TSQLRecordClass; aGrid: TDrawGrid; aIDColumnHide: boolean;
      aPager: TSynPager; aImageList32,aImageList16: TImageList;
      aOnButtonClick: TSQLListerEvent; aOnValueText: TValueTextEvent;
      const aGridSelect: RawUTF8= '*';
      aHideDisabledButtons: boolean=false); reintroduce; overload;
    /// same as above, but with a specified TSQLTable
    constructor Create(aOwner: TComponent; aClient: TSQLRestClientURI;
      aClass: TSQLRecordClass; aGrid: TDrawGrid; aIDColumnHide: boolean;
      aPager: TSynPager; aImageList32,aImageList16: TImageList;
      aOnButtonClick: TSQLListerEvent; aOnValueText: TValueTextEvent;
      aTable: TSQLTable; aHideDisabledButtons: boolean); reintroduce; overload;

    /// add or update a ToolBar with a specific actions set
    // - a single page can share multiple toolbars, which caption name must be
    // identical between calls for geniune buttons
    // - if the ToolBar is already existing, the status of its Action buttons
    // is enabled or disabled according to the actions set
    // - aActions must point to a set of enumerates, as defined by
    // Client.Model.SetActions(TypeInfo(..))
    // - first call once this procedure to create the toolbar buttons, then
    // call it again to update the enable/disable status of the buttons
    function SetToolBar(const aToolBarName: string; const aActions;
      ActionIsNotButton: pointer): TSynToolBar;
    /// can be used by any TSQLTableToGrid
    // - to draw marked rows with a highlighted color
    // - with respect to the Toolbar theming
    procedure OnDrawCellBackground(Sender: TObject; ACol, ARow: Longint;
      Rect: TRect; State: TGridDrawState);
    /// create a menu item, and add it to a menu
    function NewMenuItem(Menu: TPopupMenu; const aCaption: string;
      ImageIndex: integer=-1; SubMenu: TMenuItem=nil; OnClick: TNotifyEvent=nil;
      itemEnabled: boolean=true): TMenuItem;
    /// find associate Button for an action
    function FindButton(ActionIndex: integer): TSynToolButton;
    /// retrieve a ready to be displayed hint for a specified action
    // - returns the Hint caption of the corresponding button, or '' if not existing
    function ActionHint(const Action): string;
    /// find associate popup Menu item for an action
    function FindMenuItem(ActionIndex: integer): TMenuItem;
    /// create a sub menu item to both button and menu item for an action
    // - if aCaption is '', erase any previous menu
    procedure CreateSubMenuItem(const aCaption: string; ActionIndex: integer;
      OnClick: TNotifyEvent; ImageIndex: integer=-1; Tag: integer=0;
      itemEnabled: boolean=true);

    /// the associated Client
    property Client: TSQLRestClient read fClient;
    /// the associated record class
    property RecordClass: TSQLRecordClass read fClass;
    /// the associated Grid display
    property Grid: TDrawGrid read fGrid;
    /// the associated TSQLTableToGrid hidden component
    property TableToGrid: TSQLTableToGrid read fTableToGrid;
    /// the associated Page on the Office 2007 menu
    property Page: TSynPage read fPage;
    /// TImagelist used to display some images in Action buttons
    property ImageList32: TImageList read fImageList32;
    /// TImagelist used to display some images in Action buttons
    property ImageList16: TImageList read fImageList16;
    /// the Popup Menu, displayed with the Grid
    property Menu: TSynPopupMenu read fMenu;
    /// the Hints captions to be displayed on the screen
    // - must be set before SetToolBar() method call
    // - one action (starting with actMark) each line
    property ActionHints: string read fActionHints write fActionHints;
    /// set to to a "Details" level, according to the bsCheck button pushed
    // - set to the Action index which is currently available
    property ReportDetailedIndex: integer read fReportDetailedIndex;
  end;

  /// create one or more toolbars in a ribbon page, according to an enumeration
  // of actions
  // - use a similar layout and logic as TSQLLister.SetToolBar() method above
  // - to be used for custom forms (e.g. preview or edit) or to add some
  // custom buttons to a previously created one by TSQLLister.SetToolBar()
  // - simply set the associated objects via the Init() method, then call
  // AddToolBar() for every toolbar which need to be created
  TSQLCustomToolBar = object
  public
    Page: TSynPage;
    ActionHints: string;
    ActionsEnum: PEnumType;
    ButtonClick: TNotifyEvent;
    ShortCutUsed: TFreeShortCut;
    ImageList: TImageList;
    Buttons: array of TSynToolButton;
    Toolbars: array of TSynToolBar;
    ImageListFirstIndex: integer;
    /// call this method first to initialize the ribbon
    // - if aToolbarOrPage is a TCustomForm, this form will became a  
    // - if aToolbarOrPage is a TSynPager descendant, a new page is created
    // and added to this TSynPager, and used for toolbars adding
    // - if aToolbarOrPage is a TSynPage descendant, the toolbar is added to
    // this specified Page
    procedure Init(aToolbarOrPage: TControl; aEnum: PTypeInfo;
      aButtonClick: TNotifyEvent; aImageList: TImageList; const aActionHints: string;
      aImageListFirstIndex: integer=0);
    /// call this method for every toobar, with appropriate bits set for its buttons
    function AddToolBar(const ToolBarName: string; ActionsBits: pointer=nil;
      ButtonWidth: integer=60): TSynToolBar;
    /// create a popup menu item for a button
    // - call with aCaption void to clear the menu first
    // - then call it for every menu entry
    function CreateSubMenuItem(aButtonIndex: integer; const aCaption: string;
      aOnClick: TNotifyEvent; aTag: integer=0): TMenuItem;
  end;

  /// this event provide the action values for a specified toolbar
  // - first call is to test the action presence, with TestEnabled=false
  // - a special call is made with ToolBarIndex=-1, in which A should be
  // filled with the marking actions
  // - second call is to test the action enable/disable state, with
  // TestEnabled=true
  // - in all cases, should return any customized toolbar caption name, or ''
  TSQLRibbonSetActionEvent = function(TabIndex, ToolbarIndex: integer;
    TestEnabled: boolean; var A): string of object;

  /// used to store the options status
  TPBooleanDynArray = array of PBoolean;


  /// store the UI elements and data, one per each Table
  TSQLRibbonTab = class
  private
    function GetCurrentID: integer;
  protected
    /// event called by ReportClick(), set by CustomReportPopupMenu()
    FReportPopupClick: TNotifyEvent;
    /// used by ReportPopup() method:
    FReportPopupParams: PEnumType;
    FReportPopupParamsEnabled: pointer;
    FReportPopupValues: TPBooleanDynArray;
    /// trigerred when a report popup menu is displayed
    procedure ReportPopup(Sender: TObject);
    /// used internaly to display the report popup menu via the "View" options
    // - can be used to display the same popup menu from another toolbar
    procedure ReportOptionEvent(Sender: TObject; ClientPoint, ScreenPoint: TPoint);
  public
    /// associated TSQLRecord
    Table: TSQLRecordClass;
    /// associated TSQLRecord index in database Model
    TableIndex: integer;
    /// associated Tab settings used to create this Ribbon Tab
    Parameters: PSQLRibbonTabParameters;
    /// associate Tab in the Ribbon
    Tab: TSynPage;
    /// the "View" toolbar on the associated Ribbon Tab
    ViewToolBar: TSynToolBar;
    /// associate Client Body Page
    Page: TSynBodyPage;
    /// the frame containing associated the List, left side of the Page
    FrameLeft: TFrame;
    /// associated table list
    List: TDrawGrid;
    /// allows List resizing
    FrameSplit: TSplitter;
    /// to provide the List with data from Client
    TableToGrid: TSQLTableToGrid;
    /// to associate Class, Actions, Ribbon and Toolbars
    Lister: TSQLLister;
    /// the frame containing associated Details, right side to the list
    FrameRight: TFrame;
    /// the associated Report, to display the page
    // - exists if aTabParameters.Layout is not llClient, and if
    // aTabParameters.NoReport is false
    Report: TGDIPages;
    /// a current record value
    CurrentRecord: TSQLRecord;
    /// create all the UI elements for a specific Table/Class
    // - create a new page for this Table/Class
    // - populate this page with available Toolbars
    // - populate all Toolbars with action Buttons
    constructor Create(ToolBar: TSynPager; Body: TSynBodyPager;
      aImageList32,aImageList16: TImageList; var aPagesShortCuts: TFreeShortCut;
      const aTabParameters: TSQLRibbonTabParameters;
      Client: TSQLRestClientURI; aUserRights: TSQLFieldBits;
      aOnValueText: TValueTextEvent;
      SetAction: TSQLRibbonSetActionEvent;
      const ActionsTBCaptionCSV, ActionsHintCaption: string; ActionIsNotButton: pointer;
      aOnActionClick: TSQLListerEvent; ViewToolbarIndex: integer;
      aHideDisabledButtons: boolean);
    /// retrieve CurrentRecord from server
    function Retrieve(Client: TSQLRestClient; ARow: integer; ForUpdate: boolean=false): boolean;
    /// ask the User where to perform an Action
    // - return 100 if "Apply to Selected" was choosen
    // - return 101 if "Apply to Marked" was choosen
    // - return any other value if Cancel or No was choosen
    function AskForAction(const ActionCaption, aTitle: string; Client: TSQLRest;
      DontAskIfOneRow, ReturnMarkedIfSomeMarked: boolean): integer;
    /// release associated memory
    destructor Destroy; override;
    /// used to customize the popup menu of the associated Report
    // - this method expect two standard handlers, and a custom enumeration type
    // together with its (bit-oriented) values for the current Ribbon Tab
    // - caller must supply an array of boolean pointers to reflect the
    // checked state of every popup menu item entry
    procedure CustomReportPopupMenu(OnClick: TNotifyEvent;
      ParamsEnum: PTypeInfo; ParamsEnabled: pointer; const Values: array of PBoolean);
    /// add the report options to the specified menu
    procedure AddReportPopupMenuOptions(Menu: TPopupMenu; OnClick: TNotifyEvent);
    /// trigerred when a report popup menu item is clicked
    procedure ReportClick(Sender: TObject);
    {/// retrieve the Hint value for a particular action
    function ActionHint(const Action): string;}
    /// pointers to every popup meu items data
    property ReportPopupValues: TPBooleanDynArray read FReportPopupValues;
    /// pointer to the set of available popup menu parameters for this report
    property ReportPopupParamsEnabled: pointer read FReportPopupParamsEnabled;
    /// retrieve the current selected ID of the grid
    // - returns 0 if no row is selected
    property CurrentID: integer read GetCurrentID;
  end;

  /// Event used to customize screen text of property names
  TOnCaptionName = function(Action: PShortString; Obj: TObject=nil; Index: integer=-1): string of object;

   /// store some variables common to all pages, i.e. for the whole ribbon
  TSQLRibbon = class
  public
    /// the pages array
    Page: array of TSQLRibbonTab;
    /// store the keyboard shortcuts for the whole ribbon
    ShortCuts: TFreeShortCut;
    /// initialize the Pages properties for this ribbon
    // - this constructor must be called in the Owner.OnCreate handler (not in
    // OnShow)
    // - most parameters are sent back to the SQLRibbonTab.Create constructor
    // - if BackgroundPictureResourceNameCSV is set, the corresponding
    // background pictures will be extracted from resources and displayed behind
    // the ribbon toolbar, according to the group
    constructor Create(Owner: TCustomForm; ToolBar: TSynPager; Body: TSynBodyPager;
      aImageList32,aImageList16: TImageList;
      Client: TSQLRestClientURI; aUserRights: TSQLFieldBits;
      aOnValueText: TValueTextEvent; SetAction: TSQLRibbonSetActionEvent;
      const ActionsTBCaptionCSV, ActionsHintCaption: string; ActionIsNotButton: pointer;
      aOnActionClick: TSQLListerEvent; RefreshActionIndex, ViewToolbarIndex: integer;
      aHideDisabledButtons: boolean;
      PagesCount: integer; TabParameters: PSQLRibbonTabParameters; TabParametersSize: integer;
      const GroupCSV: string; const BackgroundPictureResourceNameCSV: string=''); reintroduce; virtual;
    /// release associated memory
    destructor Destroy; override;
    /// retrieve the index of a given Pages[]
    // - returns -1 if this page was not found
    function GetPage(aRecordClass: TSQLRecordClass): integer;
    /// retrieve the current TSQLRibbonTab instance on the screen
    // - returns nil if no page is currently selected
    function GetActivePage: TSQLRibbonTab;
    /// retrieve the TSQLRibbonTabParameters associated to a Ribbon tab, from its index
    // - returns nil if the specified page index is not valid
    function GetParameter(aPageIndex: Integer): PSQLRibbonTabParameters; overload;
    /// get the the TSQLRibbonTabParameters associated to a Ribbon tab, from its table
    // - returns nil if the specified table is not valid
    function GetParameter(aTable: TSQLRecordClass): PSQLRibbonTabParameters; overload;
    /// retrieve the reference of a given button of the ribbon
    // - useful to customize the Ribbon layout, if automatic generation from RTTI
    // don't fit exactly your needs, or even worse marketing's know-how ;)
    // - called by SetButtonHint method
    function FindButton(aTable: TSQLRecordClass; aActionIndex: integer): TSynToolButton;
    /// customize the Hint property of any button
    // - will test the button is available (avoid any GPF error)
    procedure SetButtonHint(aTable: TSQLRecordClass; aActionIndex: integer; const aHint: string);

    /// trigger this event when a page changed on screen
    // - will free GDI resources and unneeded memory
    procedure ToolBarChange(Sender: TObject);
    /// resize the lists according to the body size
    procedure BodyResize(Sender: TObject);
    /// handle a ribbon button press
    // - returns TRUE if a Refresh command has been processed (caller should exit)
    // and a refresh timer command has been set
    // - returns FALSE if the caller must handle the action
    function RefreshClickHandled(Sender: TObject; RecordClass: TSQLRecordClass;
      ActionValue: integer; out Tab: TSQLRibbonTab): boolean;
    /// must be called by the main form to handle any WM_TIMER message
    // - will refresh the screen as necessary
    procedure WMRefreshTimer(var Msg: TWMTimer);
{$ifdef USETMSPACK}
    /// change the style of the ribbon and all associated stylers
    // - TMS style is not coherent between stylers: this method will
    // synchronize the color scheme accross all stylers, at once
    procedure ChangeColorScheme(const ColorScheme: TToolBarStyle;
      PanelStyler: TAdvPanelStyler=nil;
      StatusBarStyler: TAdvOfficeStatusBarOfficeStyler=nil;
      CustomStyle: TMemoryStream=nil);
{$endif USETMSPACK}
    /// create a report for the specified page index
    // - the report must be created from the Page[aPageIndex].CurrentRecord
    // record content
    // - call the CreateReport virtual method
    procedure CreateReport(aPageIndex: Integer); overload;
    /// create a report for the specified page index
    // - this default method create a report with the content of all fields,
    // except those listed in the corrresponding
    // TSQLRibbonTabParameters.EditFieldNameToHideCSV value
    procedure CreateReport(aTable: TSQLRecordClass; aID: integer;
      aReport: TGDIPages; AlreadyBegan: boolean=false); overload; virtual;
    /// add the specified fields content to the report
    // - by default, all main fields are displayed, but caller can specify custom
    // field names as Comma-Separated-Values
    // - retrieve the main Caption of this record (e.g. the "Name" field value)
    function AddToReport(aReport: TGDIPages; aRecord: TSQLRecord; WithTitle: Boolean;
      CSVFieldNames: PUTF8Char=nil; CSVFieldNameToHide: PUTF8Char=nil;
      OnCaptionName: TOnCaptionName=nil;
      ColWidthName: Integer=40; ColWidthValue: integer=60): string; overload;
    /// add the specified database Table Content to the report
    // - if ColWidths are not specified (that is set to []),
    // their values are caculated from the Table content columns
    procedure AddToReport(aReport: TGDIPages; Table: TSQLTable;
      const ColWidths: array of integer); overload;
    /// generic method which print the all marked entries of the supplied table
    function MarkedEntriesToReport(aTable: TSQLRecordClass;
      const ColWidths: array of integer; aRep: TGDIPages=nil): TGDIPages;
    /// make a specified record available to the UI
    // - select tab and record index
    // - if ActionToPerform is set, the corresponding action is launched
    procedure GotoRecord(aTable: TSQLRecordClass; aID: integer;
      ActionToPerform: integer=0); overload;
    /// make a specified record available to the UI
    // - select tab and record index
    // - if ActionToPerform is set, the corresponding action is launched
    procedure GotoRecord(aRecord: TSQLRecord; ActionToPerform: integer=0); overload;
    /// refresh the specified page content
    // - by default, refresh the current page content
    // - calls internaly RefreshClickHandled method
    procedure Refresh(aTable: TSQLRecordClass=nil);
    /// generic method which delete either the current selected entry,
    // either all marked entries
    // - returns TRUE if deletion was successful, or FALSE if any error occured
    function DeleteMarkedEntries(aTable: TSQLRecordClass; const ActionHint: string): Boolean;
    /// generic method which export the supplied record
    // - display the save dialog before
    // - only two formats are available here: Acrobat (.pdf) and plain text (.txt)
    // - returns the exported file name if export was successful, or '' if any error occured
    // - by default, the report is created by using the CreateReport method
    function ExportRecord(aTable: TSQLRecordClass; aID: integer;
      const ActionHint: string; OpenAfterCreation: boolean=true): TFileName;
  protected { private properties set by Init method }
    fTabParameters: PSQLRibbonTabParameters;
    fTabParametersSize: integer;
    fForm: TCustomForm;
    fToolBar: TSynPager;
{$ifdef USETMSPACK}
    fPreviewMenuButton: TAdvShapeButton;
    fPreviewMenu: TAdvPreviewMenu;
{$endif USETMSPACK}
    fBody: TSynBodyPager;
    fOnActionClick: TSQLListerEvent;
    fRefreshActionIndex: integer;
    fLastActiveTab: integer;
    fClient: TSQLRestClientURI;
    fActionType: PTypeInfo;
    fEventType: PTypeInfo;
    fActionsHintCaption: string;
    fReportAutoFocus: boolean;
    procedure RefreshPage(Page: TSQLRibbonTab);
  public
    /// the associated Client connection
    property Client: TSQLRestClientURI read fClient write fClient;
    /// the associated Form on scren
    property Form: TCustomForm read fForm;
    /// the Toolbar component used to display the Ribbon on the Form
    property ToolBar: TSynPager read fToolBar;
{$ifdef USETMSPACK}
    /// the Preview Menu button displayed on the Ribbon
    property PreviewMenuButton: TAdvShapeButton read fPreviewMenuButton;
    /// the Preview Menu to be displayed by pressing the Ribbon Preview Menu button
    property PreviewMenu: TAdvPreviewMenu read fPreviewMenu write fPreviewMenu;
{$endif USETMSPACK}
    /// the main Pager component used to display the main data (i.e. records list
    // and report) on the Form
    property Body: TSynBodyPager read fBody;
    /// if set to TRUE, the right-sided report is focused instead of
    // the left-sided records list
    property ReportAutoFocus: boolean read fReportAutoFocus write fReportAutoFocus;
  end;


/// retrieve the ready to be displayed text of the given property
function CaptionName(OnCaptionName: TOnCaptionName;
  Action: PShortString; Obj: TObject=nil; Index: integer=-1): string;


/// draw the cell of a TDrawGrid according to the current Theming of TabAppearance
procedure NewDrawCellBackground(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState;
  {$ifdef USETMSPACK} TabAppearance: TTabAppearance;{$endif} Marked: boolean);

/// create a report containing all icons for a given action enumeration
// - useful e.g. for marketing or User Interface review purposes
procedure CreateReportWithIcons(ParamsEnum: PTypeInfo; ImgList: TImageList;
  const Title, Hints: string; StartIndexAt: integer);

/// load a bitmap from a .png/.jpg file embedded as a resource to the executable
function LoadBitmapFromResource(const ResName: string): TBitmap;

/// fill a TImageList from the content of another TImageList
// - stretching use GDI+ so is smooth enough for popup menu display
procedure ImageListStretch(ImgListSource, ImgListDest: TImageList;
   BkColor: TColor=clSilver);

/// load TImageList bitmaps from an .zip archive embedded as a ZIP resource
procedure LoadImageListFromEmbeddedZip(ImgList: TCustomImageList; const ZipName: TFileName);

/// load TImageList bitmaps from a TBitmap
// - warning Bmp content can be modified: it could be converted from multi-line
// (e.g. IDE export format) into one-line (as expected by TImageList.AddMasked)
procedure LoadImageListFromBitmap(ImgList: TCustomImageList; Bmp: TBitmap);

/// add an Icon to the supplied TImageList
// - return the newly created index in the image list
// - the HIcon handle is destroyed before returning
function AddIconToImageList(ImgList: TCustomImageList; Icon: HIcon): integer;

resourcestring
{$ifdef USETMSPACK}
  sPerformToSelected = 'Apply to the selected entry:<br><br><font color="clnavy">%s</font> ?'#13;
  sPerformToMarkedOrSelected =
    'Apply to the <b>Selected entry</b>:<br>  <font color="clnavy">%s</font><br><br>'+
    'Or apply to all <b>Marked entries</b>:<font color="clnavy">';
{$else}
  sPerformToSelected = 'Apply to the selected entry:\n\n%s ?\n';
  sPerformToMarkedOrSelected =
    'Apply to the Selected entry:\n  %s\n\nOr apply to all Marked entries:';
{$endif}
  sApplyToSelected = 'Selected entry';
  sApplyToMarked = 'Marked entries';
  sDeleteN = 'About to Delete %d record(s)';
  sTextFile = 'Text File';

implementation

procedure CreateReportWithIcons(ParamsEnum: PTypeInfo; ImgList: TImageList;
  const Title, Hints: string; StartIndexAt: integer);
var Dest: TGDIPages;
    A: integer;
    P: PEnumType;
    PS: PShortString;
    Bmp: TBitmap;
    R: TRect;
    PC: PChar;
begin
  Bmp := TBitmap.Create;
  Dest := TGDIPages.Create(nil);
  try
    Dest.BeginDoc;
    Dest.Caption := Title;
    Dest.DrawTitle(Title+' - Icons list',true);
    Bmp.Width := ImgList.Width;
    Bmp.Height := ImgList.Height;
    P := ParamsEnum^.EnumBaseType;
    PS := @P^.NameList;
    PC := pointer(Hints);
    R.Left := Dest.LeftMargin;
    R.Right := R.Left+12;
    Dest.LeftMargin := R.Right+10;
    for A := StartIndexAt to P^.MaxValue do begin // start at 1 (0=noAction)
      Bmp.Canvas.FillRect(Rect(0,0,Bmp.Width,Bmp.Height));
      if A-StartIndexAt<ImgList.Count then
        ImgList.Draw(Bmp.Canvas,0,0,A-StartIndexAt);
      if not Dest.HasSpaceFor(20) then
        Dest.NewPage;
      R.Top := Dest.CurrentYPos;
      R.Bottom := R.Top+12;
      Dest.DrawBMP(R,Bmp);
      Dest.Font.Style := [fsBold];
      Dest.DrawText(IntToStr(A)+' - '+P^.GetCaption(A));
      Dest.Font.Style := [];
      Dest.DrawText(GetNextItemString(PC,#13));
      while Dest.CurrentYPos<R.Bottom+5 do
        Dest.NewHalfLine;
      inc(PtrUInt(PS),ord(PS^[0])+1);
    end;
    Dest.EndDoc;
    Dest.ShowPreviewForm;
  finally
    Bmp.Free;
    Dest.Free;
  end;
end;


{ TSQLLister }

class function TSQLLister.AddPage(aOwner: TSynPager;
  aClass: TSQLRecordClass; const CustomCaption: string;
  CustomCaptionTranslate: boolean): TSynPage;
var i: integer;
    Cap: string;
begin
  i := FindPage(aOwner,aClass);
  if i<0 then begin
    if CustomCaption='' then
      // get expanded caption
      if aClass=nil then
        Cap := '' else
        Cap := aClass.CaptionName else begin
      // translate specified caption if necessary
      Cap := CustomCaption;
      if CustomCaptionTranslate and Assigned(LoadResStringTranslate) then
        LoadResStringTranslate(Cap);
    end;
    result := TSynPage.Create(aOwner);
    result.Caption := Cap;
    aOwner.AddPage(result);
  end else
    result := aOwner.Pages[i];
  result.Tag := integer(aClass);
end;

class function TSQLLister.FindPage(aOwner: TSynPager;
  aClass: TSQLRecordClass): integer;
begin
  if aClass<>nil then
  for result := 0 to aOwner.PageCount-1 do
    if aOwner.Pages[result].Tag=integer(aClass) then
      exit;
  result := -1;
end;

procedure TSQLLister.ActionButtonClick(Sender: TObject);
var aAction: integer;
    A: TSQLAction absolute aAction;
    Btn: TSynToolButton absolute Sender;
    iTB,iGB: integer;
    TB: TSynToolBar;
    GB: TSynToolButton;
begin
  if fReportDetailedIndex<0 then
    exit; // avoid recursive call after GB.Down := false below
  aAction := isActionButton(Sender);
  case A of
    actNoAction: exit;
    actUnMarkAll:
      // (un)marking are standard actions
      TableToGrid.SetMark(A);
    actMark:
      if Sender.InheritsFrom(TSynToolButton) then
        Btn.DoDropDown else
      if Sender.InheritsFrom(TMenuItem) then
        // actMarkAllEntries..actMarkBeforeOneYear are regrouped in 
        // the only one aAction=actMark button
        TableToGrid.SetMark(TSQLAction(TMenuItem(Sender).Tag));
    else begin
      if Sender.InheritsFrom(TSynToolButton) and
         (Btn.Style=bsCheck) then begin
        if Btn.Down then begin
          fReportDetailedIndex := -1; // avoid recursive call
          for iTB := 0 to fPage.ToolBarCount-1 do begin
            TB := fPage.ToolBars[iTB];
            for iGB := 0 to TB.ComponentCount-1 do begin
              GB := TSynToolButton(TB.Components[iGB]);
              if (GB<>Sender) and
                 GB.InheritsFrom(TSynToolButton) and
                 (GB.Style=bsCheck) then
                 GB.Down := false;
            end;
          end;
          fReportDetailedIndex := aAction;
        end else
          fReportDetailedIndex := 0;
      end;
      if Assigned(fOnButtonClick) then
        fOnButtonClick(Sender,fClass,aAction);
    end;
  end;
end;

procedure TSQLLister.OnSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if Assigned(fOnButtonClick) {and (ARow<>fCurrentSelectedRow)} then begin
    fCurrentSelectedRow := ARow;
    fOnButtonClick(nil,fClass,ARow); // Sender=nil for Cell select
  end;
end;

constructor TSQLLister.Create(aOwner: TComponent; aClient: TSQLRestClientURI;
  aClass: TSQLRecordClass; aGrid: TDrawGrid; aIDColumnHide: boolean;
  aPager: TSynPager; aImageList32,aImageList16: TImageList;
  aOnButtonClick: TSQLListerEvent; aOnValueText: TValueTextEvent;
  const aGridSelect: RawUTF8; aHideDisabledButtons: boolean);
var T: TSQLTable;
begin
  if (aClient=nil) or (aGridSelect='') then
    T := nil else
    T := aClient.List([aClass],aGridSelect);
  Create(aOwner,aClient,aClass,aGrid,aIDColumnHide,aPager,
    aImageList32,aImageList16,aOnButtonClick,aOnValueText,T,aHideDisabledButtons);
end;

constructor TSQLLister.Create(aOwner: TComponent; aClient: TSQLRestClientURI;
  aClass: TSQLRecordClass; aGrid: TDrawGrid; aIDColumnHide: boolean;
  aPager: TSynPager; aImageList32, aImageList16: TImageList;
  aOnButtonClick: TSQLListerEvent; aOnValueText: TValueTextEvent;
  aTable: TSQLTable; aHideDisabledButtons: boolean);
var i: integer;
    C: TSQLRestClientURI;
begin
{$ifdef USETMSPACK}
  if aPager.Parent.InheritsFrom(TSynForm) then
    aPager.MinimizeApp := false; // so WMSyscommand method will be called below
{$endif}
  inherited Create(aOwner);
  if (aClient=nil) or (aPager=nil) or (aClass=nil) then
    raise EComponentError.Create(ClassName);
  fClient := aClient;
  fPager := aPager;
  fClass := aClass;
  fGrid := aGrid;
  if aTable<>nil then begin
    fTableToGrid := TSQLTableToGrid.From(fGrid);
    if fTableToGrid=nil then begin
      // this Grid has no associated TSQLTableToGrid -> create default one
      if fClient.InheritsFrom(TSQLRestClientURI) then
        C := TSQLRestClientURI(fClient) else
        C := nil;
      fTableToGrid := TSQLTableToGrid.Create(fGrid,aTable,C);
      if aIDColumnHide then
        fTableToGrid.IDColumnHide;
    end;
    fTableToGrid.OnRightClickCell := OnRightClickCell;
    TableToGrid.OnValueText := aOnValueText;
    fGrid.DefaultDrawing := false; // we force full redraw
    TableToGrid.OnDrawCellBackground := OnDrawCellBackground;
    TableToGrid.OnSelectCell := OnSelectCell;
  end;
  i := FindPage(fPager,fClass);
  if i>=0 then
     fPage := fPager.Pages[i];
  fImageList32 := aImageList32;
  fImageList16 := aImageList16;
  fOnButtonClick := aOnButtonClick;
  if fClient.Model.Actions<>nil then
    fActionMax := fClient.Model.Actions^.MaxValue;
end;

function TSQLLister.SetToolBar(const aToolBarName: string; const aActions;
  ActionIsNotButton: pointer): TSynToolBar;
var TypeName: PShortString;
    A,iTB,iGB,iM,img: integer;
    GB: TSynToolButton;
    iAction: cardinal;
    M: TMenuItem;
    EN: boolean;
    A2: TSQLAction;
    ActionNames: TStringDynArray;
begin
  result := nil;
  if fPage=nil then
    exit;
  // on existing Toolbar: update its buttons from aActions, and exit
  for iTB := 0 to fPage.ToolBarCount-1 do
    // test exact match, not with SameText(), since Caption can be translated
    if fPage.ToolBars[iTB].Caption=aToolBarName then begin
      result := fPage.ToolBars[iTB];
      for iGB := 0 to result.ComponentCount-1 do begin
        GB := TSynToolButton(result.Components[iGB]);
        if isActionButton(GB)<>0 then begin
          img := GB.ImageIndex;
          EN := GetBit(aActions,img+1);
          GB.Enabled := EN;  // enable or disable buttons
          for iM := 0 to fMenu.Items.Count-1 do
          with fMenu.Items[iM] do
            if ImageIndex=img then
              Enabled := EN; // enable or disable popup menu item
        end;
      end;
      break;
    end;
  if result<>nil then
     exit; // we have found the toolbar
  // no Toolbar: create one with its buttons; also create associated popup menu
  EN := false;
  for A := 0 to fActionMax do
    if GetBit(aActions,A) then begin
      EN := true;
      break;
    end;
  if not EN then
    exit; // aActions=[] -> no toolbar to add
  if fMenu=nil then begin
    fMenu := TSynPopupMenu.Create(fGrid);
    fMenu.Images := ImageList16;
  end;
  result := fPage.CreateToolBar;
  try
{$ifdef USETMSPACK}
    result.BeginUpdate;
    result.AutoPositionControls := true;
    result.ShowOptionIndicator := false;
    result.AutoSize := true;
{$else}
    result.Images := ImageList32;
{$endif}
    result.Caption := aToolBarName;
    SetLength(ActionNames,fActionMax+1);
    TypeName := @fClient.Model.Actions^.NameList;
    for iAction := 0 to fActionMax do begin
      ActionNames[iAction] := fClass.CaptionName(TypeName); // expanded caption
      inc(integer(TypeName),ord(TypeName^[0])+1); // next enumerate value name
{$ifndef USETMSPACK}
    end;
    for iAction := fActionMax downto 0 do begin // TToolBar adds at 1st position
{$endif}
      if GetBit(aActions,iAction) then // is this enumerate value inside aActions?
        with result.CreateToolButton(ActionButtonClick,iAction,1,ActionNames[iAction],
           ActionHints,fShortCutUsed,60,ImageList32) do begin
          if GetBit(ActionIsNotButton,iAction) then
            Style := bsCheck;
          // create associated sub menu entry
          if Style<>bsCheck then begin
            NewMenuItem(fMenu,Caption,iAction-1);
            if TSQLAction(iAction)=actMark then
              // actMarkAllEntries..actMarkBeforeOneYear are regrouped in
              // an only one aAction=actMark
              with PTypeInfo(TypeInfo(TSQLAction))^.EnumBaseType^ do
              for A2 := actMarkAllEntries to actmarkInverse do
                if (A2<actmarkOlderThanOneDay) or (A2>actmarkOlderThanOneYear) or
                 (TableToGrid.FieldIndexTimeLogForMark>=0) then begin
                  if A2=actmarkInverse then
                    CreateSubMenuItem('-',iAction,nil);
                  CreateSubMenuItem(fClass.CaptionName(GetEnumName(A2)),
                    iAction,nil,iAction-1,integer(A2));
                end;
          end;
        end;
    end;
    M := TMenuItem.Create(fMenu);
    M.Caption := '-';
    fMenu.Items.Add(M);
  finally
{$ifdef USETMSPACK}
    result.EndUpdate;
{$endif}
  end;
end;

function TSQLLister.isActionButton(One: TObject): integer;
var GB: TSynToolButton absolute One;
    M: TMenuItem absolute One;
begin
  result := -1; // not an action buton or menu
  if One.InheritsFrom(TSynToolButton) then
    if ((GB.Images=ImageList32) or (GB.Images=ImageList16)) and
      Assigned(GB.OnClick) and (TMethod(GB.OnClick).Data=Self) then
        result := GB.ImageIndex;
  if One.InheritsFrom(TMenuItem) then
    if Assigned(M.OnClick) and (TMethod(M.OnClick).Data=Self) then
      result := M.ImageIndex;
  if cardinal(result)>=fActionMax then
    result := 0 else
    inc(result);
end;

procedure TSQLLister.OnRightClickCell(Sender: TSQLTable;
  ACol, ARow, MouseX, MouseY: Integer);
var P: TPoint;
begin
  if (self=nil) or (fMenu=nil) then
    exit;
  P.X := MouseX;
  P.Y := MouseY;
  P := fGrid.ClientToScreen(P);
  fMenu.Popup(P.X,P.Y);
end;


{.$define MARKEDROWSCOLORED}
// if defined, all Marked[] rows are highligted with a different background color

procedure NewDrawCellBackground(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState;
  {$ifdef USETMSPACK} TabAppearance: TTabAppearance;{$endif} Marked: boolean);
var Grid: TDrawGrid absolute Sender;
begin
  if not Sender.InheritsFrom(TDrawGrid) then
    exit;
{$ifdef USETMSPACK}
  if TabAppearance<>nil then
  with TabAppearance, Grid.Canvas do begin
    Font := Grid.Font;
    if (gdFixed in State) then begin
      Brush.Color := BackGround.Color;
      Font.Color := TextColor;
      Pen.Color := ShadowColor;
      inc(Rect.Bottom,1);
      MoveTo(Rect.Right,Rect.Top);
      LineTo(Rect.Right,Rect.Bottom);
    end else
    if Marked then
      if (gdSelected in State) then begin
        Brush.Color := HighLightColorSelected;
        Font.Color := TextColorHot;
      end else
        Brush.Color := HighLightColor else
      if (gdSelected in State) then begin
        Brush.Color := ColorHot;
        Font.Color := TextColorHot;
      end else
        Brush.Color := ColorTo;
{$else}
  with Grid.Canvas do begin
    Font := Grid.Font;
    if gdFixed in State then begin
      Font.Color := clCaptionText;
      Brush.Color := clGradientInactiveCaption;
      Pen.Color := clGrayText;
      inc(Rect.Bottom,1);
      MoveTo(Rect.Right,Rect.Top);
      LineTo(Rect.Right,Rect.Bottom);
    end else
    if (gdSelected in State) then begin
      Font.Color := clHighlightText;
      Brush.Color := clHighlight;
    end else begin
      Font.Color := clWindowText;
      Brush.Color := clWindow;
    end;
{$endif}
    FillRect(Rect);
  end;
end;

procedure TSQLLister.OnDrawCellBackground(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin // draw the cell of a TDrawGrid according to the current Theming of Page
  if (self=nil) or (Page=nil) then
    exit;
  NewDrawCellBackground(Sender,ACol,ARow,Rect,State,
    {$ifdef USETMSPACK}TAdvToolBarOfficeStyler(Page.AdvToolBarPager.ToolBarStyler).TabAppearance,{$endif}
    {$ifdef MARKEDROWSCOLORED}TableToGrid.Marked[ARow]{$else}false{$endif});
end;

function TSQLLister.NewMenuItem(Menu: TPopupMenu; const aCaption: string;
  ImageIndex: integer=-1; SubMenu: TMenuItem=nil; OnClick: TNotifyEvent=nil;
  itemEnabled: boolean=true): TMenuItem;
begin
  result := TMenuItem.Create(Menu);
  result.Caption := aCaption;
  if not itemEnabled then
    result.Enabled := false;
  if Assigned(OnClick) then
    result.OnClick := OnClick else
    result.OnClick := ActionButtonClick;
  if ImageIndex>=0 then
    result.ImageIndex := ImageIndex;
  if SubMenu=nil then
    Menu.Items.Add(result) else
    SubMenu.Add(result);
end;

function TSQLLister.FindButton(ActionIndex: integer): TSynToolButton;
var iTB, iGB: integer;
begin
  dec(ActionIndex);
  for iTB := 0 to fPage.ToolBarCount-1 do
  with fPage.ToolBars[iTB] do
    for iGB := 0 to ComponentCount-1 do begin
      result := TSynToolButton(Components[iGB]);
      if result.InheritsFrom(TSynToolButton) and (result.ImageIndex=ActionIndex) then
        exit;
    end;
  result := nil;
end;

function TSQLLister.ActionHint(const Action): string;
var B: TSynToolButton;
begin
  B := FindButton(Byte(Action));
  if B=nil then
    result := '' else
    result := B.Hint;
end;

function TSQLLister.FindMenuItem(ActionIndex: integer): TMenuItem;
var i: integer;
begin
  dec(ActionIndex);
  for i := 0 to fMenu.Items.Count-1 do begin
    result := fMenu.Items[i];
    if result.ImageIndex=ActionIndex then
      exit;
  end;
  result := nil;
end;

procedure TSQLLister.CreateSubMenuItem(const aCaption: string;
  ActionIndex: integer; OnClick: TNotifyEvent; ImageIndex, Tag: integer;
  itemEnabled: boolean);
begin
  if (fCreateSubMenuLastMenu=nil) or (fCreateSubMenuLastAction<>ActionIndex) then begin
    fCreateSubMenuLastMenu := nil;
    fCreateSubMenuLastButton := FindButton(ActionIndex);
    if fCreateSubMenuLastButton=nil then exit; // avoid GPF
    if fCreateSubMenuLastButton.DropDownMenu=nil then begin
      fCreateSubMenuLastButton.DropDownMenu := TSynPopupMenu.Create(fGrid);
      fCreateSubMenuLastButton.DropDownMenu.Images := fMenu.Images;
    end;
    fCreateSubMenuLastMenu := FindMenuItem(ActionIndex);
    fCreateSubMenuLastAction := ActionIndex;
  end;
  if (fCreateSubMenuLastMenu=nil) or (fCreateSubMenuLastButton=nil) then exit;
  if aCaption='' then begin // erase any previous menu content
    fCreateSubMenuLastButton.DropDownMenu.Items.Clear;
    fCreateSubMenuLastMenu.Clear; // erase any previous menu content
  end else begin
    NewMenuItem(fMenu,aCaption,ImageIndex,fCreateSubMenuLastMenu,
      OnClick,itemEnabled).Tag := Tag;
    NewMenuItem(fCreateSubMenuLastButton.DropDownMenu,aCaption,ImageIndex,nil,
      OnClick,itemEnabled).Tag := Tag;
  end;
end;


{ TSQLRibbonTab }

function TSQLRibbonTab.AskForAction(const ActionCaption, aTitle: string;
  Client: TSQLRest; DontAskIfOneRow, ReturnMarkedIfSomeMarked: boolean): integer;
var i: integer;
{$ifdef USETMSPACK}
    Capt, Txt: string; // generic VCL string
{$else}
    TaskDialog: TTaskDialog;
    CommonButtons: TCommonButtons;
    DefaultButton: integer;
{$endif}
begin
  if TableToGrid.Table.RowCount<1 then
    result := 0 else
  if DontAskIfOneRow and
     (TableToGrid.Table.RowCount=1) and (TDrawGrid(TableToGrid.Owner).Row=1)  then
    result := 100 else // one Row (only one workstation) -> "apply to selected"
  if ReturnMarkedIfSomeMarked and TableToGrid.MarkAvailable then
    result := 101 else // force "Apply to Marked"
{$ifdef USETMSPACK}
  with CreateAdvTaskDialog do
  try
    if aTitle='' then
      Title := SMsgDlgConfirm else
      Title := aTitle;
    Instruction := ActionCaption;
    Icon := tiQuestion;
    Capt := '';
    with TableToGrid do
    if MarkAvailable and not MarkedIsOnlyCurrrent then begin
      for i := 1 to Table.RowCount do
        if Marked[i] then
          if length(Capt)>500 then begin
            Capt := Capt+'<br>   ...';
            break;
          end else
            Capt := Capt+'<br>   '+ExpandRowAsString(i,Client);
    end;
    if Capt<>'' then begin
      // we have some marked entries -> ask if perform on them, or selected only
      CommonButtons := [cbCancel];
      Custombuttons.Add(sApplyToSelected);
      Custombuttons.Add(sApplyToMarked);
      Capt := sPerformToMarkedOrSelected+Capt;
      DefaultButton := integer(mrCancel);
    end else begin
      // we have no marked entry -> confirm action on selected
      CommonButtons := [cbYes,cbNo];
      DefaultButton := integer(mrNo);
      Capt := sPerformToSelected;
    end;
    Txt := TableToGrid.ExpandRowAsString(TDrawGrid(TableToGrid.Owner).Row,Client);
    Content := format(Capt,[Txt]);
    result := Execute;
    if (DefaultButton=integer(mrNo)) and (result=IDYES) then
       // "Apply to Selected" was choosen
      result := 100;
  finally
    Free;
  end;
{$else}
  with TaskDialog do begin
    if aTitle='' then
      Title := SMsgDlgConfirm else
      Title := aTitle;
    Inst := ActionCaption;
    with TableToGrid do
    if MarkAvailable and not MarkedIsOnlyCurrrent then begin
      for i := 1 to Table.RowCount do
        if Marked[i] then
          if length(Content)>500 then begin
            Content := Content+'\n   ...';
            break;
          end else
            Content := Content+'\n   '+ExpandRowAsString(i,Client);
    end;
    if Content<>'' then begin
      // we have some marked entries -> ask if perform on them, or selected only
      CommonButtons := [cbCancel];
      Buttons := sApplyToSelected+#13#10+sApplyToMarked;
      Content := sPerformToMarkedOrSelected+Content;
      DefaultButton := integer(mrCancel);
    end else begin
      // we have no marked entry -> confirm action on selected
      CommonButtons := [cbYes,cbNo];
      DefaultButton := integer(mrNo);
      Content := sPerformToSelected;
    end;
    Content := format(Content,
      [TableToGrid.ExpandRowAsString(TDrawGrid(TableToGrid.Owner).Row,Client)]);
    result := Execute(CommonButtons,DefaultButton,[],tiQuestion);
    if (DefaultButton=integer(mrNo)) and (result=IDYES) then
       // "Apply to Selected" was choosen
      result := 100;
  end;
{$endif USETMSPACK}
end;

destructor TSQLRibbonTab.Destroy;
begin
  FreeAndNil(CurrentRecord);
  inherited;
end;

{.$define CREATEICONSREPORT}
// if defined, a report is created containing all icons for this toolbar
// -> not working yet

constructor TSQLRibbonTab.Create(ToolBar: TSynPager; Body: TSynBodyPager;
  aImageList32,aImageList16: TImageList; var aPagesShortCuts: TFreeShortCut;
  const aTabParameters: TSQLRibbonTabParameters;
  Client: TSQLRestClientURI; aUserRights: TSQLFieldBits;
  aOnValueText: TValueTextEvent; SetAction: TSQLRibbonSetActionEvent;
  const ActionsTBCaptionCSV, ActionsHintCaption: string; ActionIsNotButton: pointer;
  aOnActionClick: TSQLListerEvent; ViewToolbarIndex: integer;
  aHideDisabledButtons: boolean);
var W: string;
    U: RawUTF8;
    i, LW: integer;
    PC: PChar;
    TestEnabled: boolean;
    A: array[0..31] of byte; // enough space for a set of 256 elements
    TB: TSynToolBar;
    Act: TSQLActions absolute A;
    OK: boolean;
{$ifdef CREATEICONSREPORT}
    IcoRep: TGDIPages;
{$endif}
begin
  Table := aTabParameters.Table;
  TableIndex := Client.Model.GetTableIndex(Table);
  Parameters := @aTabParameters;
  assert(TableIndex>=0);
  // create tab on ribbon
  if aTabParameters.CustomCaption<>nil then
    W := LoadResString(aTabParameters.CustomCaption) else // do the translation
    W := '';
  Tab := TSQLLister.AddPage(ToolBar,Table,W,false);
  assert(TableIndex=Tab.PageIndex); // as expected in SetAction()
  // create Body page
{$ifdef USETMSPACK}
  Page := Body.AdvPages[Body.AddAdvPage(Table.ClassName)]; {$else}
  Page := Body.Pages[Body.AddPage(Table.ClassName)];
{$endif}
  Page.TabVisible := false;
  if not (TableIndex in aUserRights) then begin
    Tab.TabVisible := false;
    exit; // this Table won't be visible in the client area
  end;
{$ifdef USETMSPACK}
  Tab.ShortCutHintPos := shpBottom;
  Tab.ShortCutHint := aPagesShortCuts.FindFreeShortCut(Tab.Caption);
{$endif}
  Tab.ShowHint := true;
  FrameLeft := TFrame.Create(Body);
  FrameLeft.Parent := Page;
  List := TDrawGrid.Create(Body);
  List.Parent := FrameLeft;
  List.Align := alClient;
  Lister := TSQLLister.Create(Page,Client,Table,List,not aTabParameters.ShowID,
    Toolbar,aImageList32,aImageList16,aOnActionClick,aOnValueText,
    'ID,'+aTabParameters.Select,aHideDisabledButtons);
  TableToGrid := Lister.TableToGrid;
  U := aTabParameters.FieldWidth;
  if aTabParameters.ShowID then
    U := 'b'+U; // ID is shown centered in first column
  if assigned(SetAction) then
    SetAction(Tab.PageIndex,-1,false,A) else // -1 -> A := mark toolbar
    fillchar(A,sizeof(A),0);
  if TableToGrid<>nil then begin
    TableToGrid.SetFieldLengthMean(U,actMark in Act); // actMark -> add checkboxes
    if cardinal(aTabParameters.OrderFieldIndex)<cardinal(TableToGrid.Table.FieldCount) then begin
      TableToGrid.SortForce(
        aTabParameters.OrderFieldIndex,not aTabParameters.ReverseOrder);
      List.Row := 1;
    end;
  end;
  if aTabParameters.Layout=llClient then begin
    FrameLeft.Align := alClient;
    // no FrameSplit nor FrameRight necessary
  end else begin // llLeft, llUp, llLeftUp:
    FrameRight := TFrame.Create(Body);
    FrameRight.Parent := Page;
    FrameRight.Align := alClient;
    FrameSplit := TSplitter.Create(Body);
    FrameSplit.Parent := Page;
    LW := aTabParameters.ListWidth;
    if LW=0 then
      LW := 30; // default list width is 30%
    case aTabParameters.Layout of
    llLeft, llLeftUp: begin
      FrameLeft.Width := (Page.ClientWidth*LW)div 100;
      FrameLeft.Align := alLeft;
      FrameSplit.Left:= FrameLeft.Width;
    end;
    llUp: begin
      FrameLeft.Height := (Page.ClientHeight*LW)div 100;
      FrameLeft.Align := alTop;
      FrameSplit.Top := FrameLeft.Height;
    end;
    end;
    FrameSplit.Align := FrameLeft.Align;
    if TableToGrid<>nil then
      FrameRight.OnResize := TableToGrid.Resize;
    if not aTabParameters.NoReport then begin
      Report := TGDIPages.Create(Body);
      Report.ForceScreenResolution := true;
      Report.Parent := FrameRight;
      Report.Align := alClient;
      Report.Zoom := PAGE_WIDTH;
{$ifdef USETMSPACK}
      Report.Color := (Toolbar.ToolBarStyler as TAdvToolBarOfficeStyler).
        DockColor.Color;
{$endif}
    end;
  end;
  // create toolbars on ribbon for this tab
  // TestEnabled := false -> first create all buttons
  // TestEnabled := true -> then enabled or disable the buttons,
  //  according to user rights
  if assigned(SetAction) then begin
{$ifdef CREATEICONSREPORT}
    IcoRep := TGDIPages.Create(nil);
    IcoRep.BeginDoc;
    IcoRep.DrawTitle(Page.Caption,true);
{$endif}
    Lister.ActionHints := ActionsHintCaption;
    for TestEnabled := false to true do begin
      PC := pointer(ActionsTBCaptionCSV);
      i := 0;
      while PC<>nil do begin
        if aHideDisabledButtons then
          OK := true else
          OK := TestEnabled;
        W := SetAction(Tab.PageIndex,i,OK,A); // actions for this toolbar
        if W='' then begin
          W := GetNextItemString(PC);
          if W='%' then  // display the Tab name as Toolbar caption
            W := Tab.Caption else
          if W='%%' then // display the table name as Toolbar caption
            W := Table.CaptionName;
        end else
          GetNextItemString(PC); // customized caption -> just ignore default
{$ifdef CREATEICONSREPORT}
        if TestEnabled then begin
          IcoRep.DrawTitle('"'+W+'" Toolbar');
        end;
{$endif}TB := Lister.SetToolBar(W,A,ActionIsNotButton);
        if i=ViewToolbarIndex then
          ViewToolbar := TB;
        inc(i);
      end;
    end;
{$ifdef CREATEICONSREPORT}
    IcoRep.EndDoc;
    IcoRep.ShowPreviewForm;
    IcoRep.Free;
{$endif}
  end;
  // update Report popup menu
  if (Report<>nil) and (ViewToolbar<>nil) then begin
    Report.PopupMenu := TSynPopupMenu.Create(Report);
{$ifdef USETMSPACK}
    ViewToolBar.ShowOptionIndicator := true;
    ViewToolbar.OnOptionClick := ReportOptionEvent;
{$endif}
  end;
{$ifndef USETMSPACK}
  Lister.Page.ToolBarCreated;
{$endif}
end;

function TSQLRibbonTab.Retrieve(Client: TSQLRestClient; ARow: integer; ForUpdate: boolean=false): boolean;
var ID: integer;
begin
  FreeAndNil(CurrentRecord); // force Destroy: private fields MUST be reset
  if (Client=nil) or (ARow<=0) then
    result := false else begin
    CurrentRecord := Table.Create;
    ID := TableToGrid.Table.IDColumnHiddenValue(ARow);
    if ID<=0 then
      result := false else
      result := Client.Retrieve(ID,CurrentRecord,ForUpdate);
  end;
end;

procedure TSQLRibbonTab.ReportOptionEvent(Sender: TObject; ClientPoint,
  ScreenPoint: TPoint);
begin
  Report.PopupMenu.Popup(ScreenPoint.X,ScreenPoint.Y);
end;

procedure TSQLRibbonTab.CustomReportPopupMenu(OnClick: TNotifyEvent;
  ParamsEnum: PTypeInfo; ParamsEnabled: pointer; const Values: array of PBoolean);
begin
  if (ParamsEnum=nil) or not (ParamsEnum^.Kind=tkEnumeration) then
    exit;
  Report.OnPopupMenuPopup := ReportPopup;
  Report.OnPopupMenuClick := ReportClick;
  FReportPopupClick := OnClick;
  FReportPopupParams := ParamsEnum^.EnumBaseType;
  FReportPopupParamsEnabled := ParamsEnabled;
  SetLength(FReportPopupValues,FReportPopupParams.MaxValue+1);
  move(Values[0],FReportPopupValues[0],length(Values)*sizeof(Values[0]));
end;

procedure TSQLRibbonTab.AddReportPopupMenuOptions(Menu: TPopupMenu; OnClick: TNotifyEvent);
var P: PShortString;
    i: integer;
function AddReportMenu(const Caption: string): TMenuItem;
begin
  result := Lister.NewMenuItem(Menu,Caption,-1,nil,OnClick);
  result.Tag := 1000+i;
end;
begin
  if (self=nil) or (FReportPopupParams=nil) or (FReportPopupParamsEnabled=nil) then
    exit;
  if not Assigned(OnClick) and (Report<>nil) then
    OnClick := Report.PopupMenuItemClick;
  AddReportMenu('-'); // separator
  P := @FReportPopupParams^.NameList;
  for i := 0 to FReportPopupParams^.MaxValue do begin
    if GetBit(FReportPopupParamsEnabled^,i) then
      with AddReportMenu(Table.CaptionName(P)) do
        Checked := (FReportPopupValues[i]<>nil) and FReportPopupValues[i]^;
    inc(integer(P),ord(P^[0])+1); // next enumeration item
  end;
end;

procedure TSQLRibbonTab.ReportPopup(Sender: TObject);
begin
  AddReportPopupMenuOptions(Report.PopupMenu,nil);
end;

procedure TSQLRibbonTab.ReportClick(Sender: TObject);
var M: TMenuItem absolute Sender;
    i: integer;
begin
  if (Sender<>nil) and not Sender.InheritsFrom(TMenuItem) then
    exit;
  if FReportPopupValues=nil then
    exit; // avoid GPF
  i := M.Tag-1000;
  if (cardinal(i)<=cardinal(high(FReportPopupValues))) and
     (FReportPopupValues[i]<>nil) then begin
    FReportPopupValues[i]^ := not FReportPopupValues[i]^;
    M.Checked := FReportPopupValues[i]^;
    if Assigned(FReportPopupClick) then
      FReportPopupClick(Sender);
  end;
end;

function TSQLRibbonTab.GetCurrentID: integer;
begin
  if (self=nil) or (TableToGrid=nil) or (List=nil) then
    result := 0 else
    result := TableToGrid.Table.IDColumnHiddenValue(List.Row);
end;

{ TFreeShortCut }

function TFreeShortCut.FindFreeShortCut(const aCaption: string): string;
var c: AnsiChar;
    i: integer;
begin
  for i := 1 to length(aCaption) do begin
{$ifdef UNICODE}
    if word(aCaption[i])>255 then
      continue;
{$endif}
    c := NormToUpper[AnsiChar(aCaption[i])];
    if (c in ['A'..'Z']) and not (ord(c) in Values) then begin
      Include(Values,ord(c)); // one char shortcut
      result := string(c);
      exit;
    end;
  end;
  result := '';
end;


{ TSQLCustomToolBar }

function TSQLCustomToolBar.AddToolBar(const ToolBarName: string;
  ActionsBits: pointer=nil; ButtonWidth: integer=60): TSynToolBar;
var TypeName: PShortString;
    iAction: integer;
    ActionNames: TStringDynArray;
begin
  if (ActionsEnum=nil) or (ActionsBits=nil) then
    // no enum -> add a void toolbar
    result := Page.CreateToolBar else begin
    result := nil;
    for iAction := 0 to ActionsEnum^.MaxValue do
      if GetBit(ActionsBits^,iAction) then begin
        result := Page.CreateToolBar;
        break;
      end;
  end;
  SetLength(ToolBars,length(ToolBars)+1); // ToolBars[] index is OK
  if result=nil then
    exit; // only create the toolbar if any button to put inside ;)
  ToolBars[high(ToolBars)] := result;
  try
{$ifdef USETMSPACK}
    result.BeginUpdate;
    result.AutoPositionControls := true;
    result.ShowOptionIndicator := false;
    result.AutoSize := true;
{$else}
    result.Images := ImageList;
{$endif}
    result.Caption := ToolBarName;
    if ActionsEnum=nil then
      exit;
    SetLength(ActionNames,ActionsEnum^.MaxValue+1);
    TypeName := @ActionsEnum^.NameList;
    for iAction := 0 to ActionsEnum^.MaxValue do begin
      ActionNames[iAction] := TSQLRecord.CaptionName(TypeName); // expanded caption
      inc(integer(TypeName),ord(TypeName^[0])+1); // next enumerate value name
{$ifndef USETMSPACK}
    end; // TToolBar adds at 1st position -> downto
    for iAction := ActionsEnum^.MaxValue downto 0 do begin 
{$endif}
      if (ActionsBits=nil) or GetBit(ActionsBits^,iAction) then 
        Buttons[iAction] := result.CreateToolButton(ButtonClick,iAction,
         ImageListFirstIndex,ActionNames[iAction],ActionHints,ShortCutUsed,
         ButtonWidth,ImageList);
    end;
  finally
{$ifdef USETMSPACK}
    result.EndUpdate;
{$else}
   Page.ToolBarCreated; // create Caption label under toolbar
{$endif}
  end;
end;

function TSQLCustomToolBar.CreateSubMenuItem(aButtonIndex: integer;
  const aCaption: string; aOnClick: TNotifyEvent; aTag: integer): TMenuItem;
begin
  result := nil;
  if (@self<>nil) and (cardinal(aButtonIndex)<=cardinal(high(Buttons))) then
    with Buttons[aButtonIndex] do begin
      if DropDownMenu=nil then
        DropDownMenu := TSynPopupMenu.Create(Page);
      if aCaption='' then // erase any previous menu content
        DropDownMenu.Items.Clear else begin // or add item
        result := TMenuItem.Create(DropDownMenu);
        result.Caption := aCaption;
        result.OnClick := aOnClick;
        result.Tag := aTag;
        DropDownMenu.Items.Add(result);
      end;
    end;
end;

procedure TSQLCustomToolBar.Init(aToolbarOrPage: TControl; aEnum: PTypeInfo;
  aButtonClick: TNotifyEvent; aImageList: TImageList; const aActionHints: string;
  aImageListFirstIndex: integer);
var ToolBar: TSynPager absolute aToolbarOrPage;
begin
  if aToolbarOrPage.InheritsFrom(TCustomForm) then begin
    ToolBar := TSynPager.CreatePager(TCustomForm(aToolbarOrPage),true);
{$ifndef USETMSPACK}
    ToolBar.FormNoCaption;
{$endif}
  end;
  if aToolbarOrPage.InheritsFrom(TSynPager) then begin
    Page := TSQLLister.AddPage(ToolBar,nil,'',False);
{$ifdef USETMSPACK}
    if ToolBar.Parent.InheritsFrom(TSynForm) then
      ToolBar.MinimizeApp := false; // so WMSyscommand method will be called below
{$endif}
    ToolBar.ActivePageIndex := 0;
  end else
  if aToolbarOrPage.InheritsFrom(TSynPage) then
    Page := pointer(aToolbarOrPage) else
    assert(false);
  if aEnum^.Kind=tkEnumeration then begin
    ActionsEnum := aEnum^.EnumBaseType;
    SetLength(Buttons,ActionsEnum^.MaxValue+1);
  end;
  ButtonClick := aButtonClick;
  ImageList := aImageList;
  ActionHints := aActionHints;
  ImageListFirstIndex := aImageListFirstIndex;
end;


{$ifdef USETMSPACK}

{ TSynForm }

procedure TSynForm.CreateParams(var Params: TCreateParams);
begin
  HideAppFormTaskBarButton; // check if not already hidden
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or
    WS_EX_APPWINDOW;  // this form will appear in the TaskBar
end;

procedure TSynForm.WMSyscommand(var M: TMessage);
begin
  case (M.WParam and $FFF0) of
    SC_MINIMIZE, SC_RESTORE, SC_MAXIMIZE: begin
       M.Result := DefWindowProc(self.Handle, M.Msg, M.WParam, M.LParam);
       ShowWindow(Application.Handle, SW_HIDE);
     end;
  else
    inherited;
  end;
end;

{$endif USETMSPACK}

function LoadBitmapFromResource(const ResName: string): TBitmap;
var Pic: TSynPicture;
begin
  result := TBitmap.Create;
  try
    Pic := TSynPicture.Create;
    try
      Pic.LoadFromResourceName(HInstance,ResName); // *.png
      result.Width := Pic.Width;
      result.Height := Pic.Height;
      result.Canvas.Draw(0,0,Pic);
    finally
      Pic.Free;
    end;
  except
    on Exception do
      FreeAndNil(result);
  end;
end;

procedure ImageListStretch(ImgListSource, ImgListDest: TImageList;
   BkColor: TColor=clSilver);
var BmpSource, BmpDest: TBitmap;
    i: integer;
    Pic: TSynPicture;
    RS,RD: TRect;
begin
  ImgListDest.Clear;
  if Gdip=nil then
    Gdip := TGDIPlusFull.Create;
  Pic := TSynPicture.Create;
  BmpSource := TBitmap.Create;
  BmpDest := TBitmap.Create;
  try
    RS.Left := 0;
    RS.Top := 0;
    RS.Right := ImgListSource.Width;
    RS.Bottom := ImgListSource.Height;
    BmpSource.Width := RS.Right;
    BmpSource.Height := RS.Bottom;
    RD.Left := 0;
    RD.Top := 0;
    RD.Right := ImgListDest.Width;
    RD.Bottom := ImgListDest.Height;
    BmpDest.Width := RD.Right;
    ImgListDest.Masked := false;
    BmpDest.Height := RD.Bottom;
    for i := 0 to ImgListSource.Count-1 do begin
      BmpSource.Canvas.Brush.Color := BkColor;
      BmpSource.Canvas.Brush.Style := bsSolid;
      BmpSource.Canvas.FillRect(RS);
      ImgListSource.Draw(BmpSource.Canvas,0,0,i);
      Pic.Assign(BmpSource);
      Pic.Draw(BmpDest.Canvas,RD); // GDI+ smooth draw
      ImgListDest.Add(BmpDest,nil);
    end;
  finally
    BmpDest.Free;
    BmpSource.Free;
    Pic.Free;
  end;
end;

function AddIconToImageList(ImgList: TCustomImageList; Icon: HIcon): integer;
var Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Transparent := true;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Width := ImgList.Width;
    Bmp.Height := ImgList.Height;
    DrawIconEx(Bmp.Canvas.Handle,0,0,Icon,ImgList.Width,ImgList.Height,0,
      Bmp.Canvas.Brush.Handle,DI_NORMAL);
    DestroyIcon(Icon);
    result := ImgList.AddMasked(Bmp,clWhite);
  finally
    Bmp.Free;
  end;
end;

procedure LoadImageListFromBitmap(ImgList: TCustomImageList; Bmp: TBitmap);
var i: integer;
    BW,BH,W,H: integer;
begin
  // from multi-line (i.e. IDE export) into one-line (for AddMasked)
  BW := Bmp.Width;
  BH := Bmp.Height;
  W := (BW div ImgList.Width);
  H := (BH div ImgList.Height);
  Bmp.Width := W*H*ImgList.Width;
  BH := ImgList.Height;
  for i := 2 to H do
    Bmp.Canvas.CopyRect(Rect((i-1)*BW,0,i*BW,BH),
      Bmp.Canvas,Rect(0,(i-1)*BH,BW,i*BH));
  Bmp.Height := BH;
  // add these images to the image list
  ImgList.AddMasked(Bmp,Bmp.Canvas.Pixels[0,0]);
end;

procedure LoadImageListFromEmbeddedZip(ImgList: TCustomImageList; const ZipName: TFileName);
var i: integer;
    Bmp: TBitmap;
    Stream: TSynMemoryStream;
begin
  with TZipRead.Create(HInstance,'Zip','ZIP') do
  try
    i := NameToIndex(ZipName);
    if i<0 then exit;
    Stream := TSynMemoryStream.Create(UnZip(i)); // uncompress
    try
      Bmp := TBitmap.Create;
      try
        Bmp.LoadFromStream(Stream);
        LoadImageListFromBitmap(ImgList,Bmp);
      finally
        Bmp.Free;
      end;
    finally
      Stream.Free;
    end;
  finally
    Free;
  end;
end;



{$ifndef USETMSPACK}

{ TSynToolButton }

constructor TSynToolButton.Create(aOwner: TComponent);
begin
  inherited;
  AutoSize := true;
end;

procedure TSynToolButton.DoDropDown;
begin
  CheckMenuDropdown;
end;

function TSynToolButton.Images: TCustomImageList;
begin
  if Owner.InheritsFrom(TToolBar) then
    result := TToolBar(Owner).Images else
    result := nil;
end;


{ TSynPage }

const
  TOOLBAR_SPACE = 20;

function TSynPage.GetToolBarNextLeft(var Last: TSynToolBar): integer;
var n, W: integer;
begin
  n := length(fToolBar);
  if n>0 then begin
    Last := fToolBar[n-1];
    with Last.Buttons[Last.ButtonCount-1] do
      W := Left+Width+2;
    Last.Width := W;
    result := Last.Left+W+TOOLBAR_SPACE;
  end else begin
    result := 4;
    Last := nil;
  end;
end;

function TSynPage.CreateToolBar(AddToList: boolean): TSynToolBar;
var n: integer;
    Last: TSynToolBar;
begin
  result := TSynToolBar.Create(self);
  result.Parent := self;
  result.Align := alNone;
  result.AutoSize := false;
  result.ShowCaptions := true;
  result.ButtonHeight := 58;
  result.ButtonWidth := 60;
  result.Height := 57;
  result.Top := 7;
  result.Font := DefaultFont;
  result.Font.Size := result.Font.Size-1;
  result.Left := GetToolBarNextLeft(Last);
  if (Last<>nil) and (Last.Images<>nil) then
    result.Images := Last.Images;
  if AddToList then begin
    n := length(fToolBar);
    SetLength(fToolBar,n+1);
    fToolBar[n] := result;
  end;
end;

function TSynPage.GetToolBar(aIndex: integer): TSynToolBar;
begin
  if cardinal(aIndex)<cardinal(length(fToolBar)) then
    result := fToolBar[aIndex] else
    result := nil;
end;

function TSynPage.GetToolBarCount: integer;
begin
  result := length(fToolBar);
end;

procedure TSynPage.ToolBarCreated;
var n, i: integer;
    Lab: TLabel;
    Bev: TBevel;
    Last: TSynToolBar;
begin
  n := length(fToolBar);
  if fToolBarCaptionsCount=n then
    exit;
  GetToolBarNextLeft(Last);
  for i := fToolBarCaptionsCount to n-1 do begin
     Lab := TLabel.Create(Self);
     Lab.Parent := Self;
     Lab.AutoSize := false;
     Lab.Alignment := Classes.taCenter;
     Lab.Font.Color := clGrayText;
     Lab.Transparent := true;
     Bev := TBevel.Create(Self);
     Bev.Parent := Self;
     with fToolBar[i] do begin
       Lab.Font.Size := Font.Size;
       Lab.SetBounds(Left,Top+Height+3,Width,20);
       Bev.SetBounds(Left,Top+Height+1,Width,2);
       Lab.Caption := Caption;
     end;
     Bev.Shape := bsTopLine;
  end;
  fToolBarCaptionsCount := n;
end;


{ TSynPager }

function TSynPager.AddPage(aPage: TSynPage): integer;
begin
  aPage.PageControl := self;
  result := PageCount-1;
  if TabHeight=0 then
    aPage.TabVisible := false;
end;

function TSynPager.AddPage(const aCaption: string): integer;
var aPage: TSynPage;
begin
  aPage := TSynPage.Create(self);
  aPage.Parent := self;
  result := AddPage(aPage);
end;

function TSynPager.GetSynPage(aIndex: integer): TSynPage;
begin
  result := inherited Pages[aIndex] as TSynPage;
end;

function TSynPager.GetActivePageIndex: integer;
begin
  result := inherited ActivePageIndex;
end;

procedure TSynPager.SetActivePageIndex(const Value: integer);
begin
  inherited ActivePageIndex := Value;
  Change;
  if Assigned(OnChange) then
    OnChange(self);
end;

procedure TSynPager.Change;
var Pag: TSynPage;
    Last: TSynToolBar;
begin
  inherited;
  if fHelpToolBar<>nil then begin
    Pag := ActivePage as TSynPage;
    fHelpToolBar.Parent := Pag;
    fHelpToolBar.Left := Pag.GetToolBarNextLeft(Last);
  end;
end;

function TSynPager.TabGroupsAdd(TabIndexStart, TabIndexEnd: integer; const aCaption: string): TLabel;
begin
  assert(TopMostPanel<>nil,'expect TSynPager.CreatePager');
  result := TLabel.Create(TopMostPanel);
  result.Font := DefaultFont;
  result.Parent := TopMostPanel;
  result.Font.Color := clGrayText;
  result.Font.Size := Font.Size-1;
  result.Alignment := Classes.taCenter;
  result.Transparent := true;
  result.Caption := aCaption;
  result.Left := TabWidth*TabIndexStart+8;
  result.Top := 2;
  result.Tag := TabIndexStart; // for GroupLabelClick to select the page
  result.OnClick := GroupLabelClick;
  result.Width := TabWidth*TabIndexEnd-result.Left;
  with TBevel.Create(TopMostPanel) do begin
    Parent := TopMostPanel;
    SetBounds(result.Left+result.Width+1,2,2,result.Height+2);
    Shape := bsLeftLine;
  end;
end;

procedure TSynPager.FormNoCaption;
begin
  if Owner.InheritsFrom(TSynForm) and (TopMostPanel<>nil) then begin
    TSynForm(Owner).SetNoCaption(TopMostPanel,TabWidth*PageCount);
    TSynForm(Owner).NoCaptionLabel.Font := DefaultFont;
  end;
end;

procedure TSynPager.GroupLabelClick(Sender: TObject);
begin
  if Sender.InheritsFrom(TLabel) then
    ActivePageIndex := TLabel(Sender).Tag;
end;

class function TSynPager.CreatePager(aOwner: TCustomForm; NoTabVisible: boolean=false): TSynPager;
var H: integer;
begin
  if NoTabVisible then
    H := TOOLBAR_HEIGHT-TOOLBAR_TAB_HEIGHT else
    H := TOOLBAR_HEIGHT;
  result := TSynPager.Create(aOwner);
  result.fTopMostPanel := TPanel.Create(aOwner);
  with result.fTopMostPanel do begin
    Parent := aOwner;
    Height := H+TOOLBAR_GROUPS_HEIGHT;
    Align := alTop;
  end;
  result.fTopPanel := TPanel.Create(aOwner);
  with result.fTopPanel do begin
    Parent := result.fTopMostPanel;
    Height := result.fTopMostPanel.ClientHeight-TOOLBAR_GROUPS_HEIGHT;
    Align := alBottom;
  end;
  result.Parent := result.fTopPanel;
  result.Height := H;
  result.HotTrack := true;
  if not NoTabVisible then begin
    result.TabHeight := TOOLBAR_TAB_HEIGHT;
    result.TabWidth := 85;
  end;
  result.Font := DefaultFont;
  result.Align := alTop;
end;

function TSynPager.GetHelpButton: TSynToolButton;
var Pag: TSynPage;
begin
  if fHelpButton=nil then begin
    fHelpButton := TSynToolButton.Create(self);
    Pag := ActivePage as TSynPage;
    fHelpToolBar := Pag.CreateToolBar(false);
    fHelpButton.Parent := fHelpToolBar;
    fHelpButton.Caption := SMsgDlgHelpHelp;
    fHelpButton.ImageIndex :=
      AddIconToImageList(fHelpToolBar.Images,LoadIcon(0,IDI_QUESTION));
    fHelpToolBar.Width := fHelpButton.Width+3;
    Pag.ToolBarCreated;
  end;
  result := fHelpButton;
end;

function TSynPager.GetCaption: TLabel;
begin
  if Owner.InheritsFrom(TSynForm) then
    result := TSynForm(Owner).NoCaptionLabel else
    result := nil;
end;

{$else USETMSPACK}

{ TSynPage }

function TSynPage.CreateToolBar: TSynToolBar;
begin
  result := TSynToolBar(CreateAdvToolBar);
end;

destructor TSynPage.Destroy;
begin
  if BackgroundPictureStored then
    FreeAndNil(fBackgroundPicture);
  inherited;
end;

function TSynPage.GetToolBar(aIndex: integer): TSynToolBar;
begin
  result := TSynToolBar(AdvToolBars[aIndex]);
end;

procedure TSynPage.Paint;
var Bmp: TBitmap;
    W,BW: integer;
begin
  inherited Paint;
  if BackgroundPicture=nil then
    Exit;
  W := Width-BackgroundPicture.Width-4;
  Canvas.Draw(W, 4, BackgroundPicture);
  if W<=4 then
    exit; // no tiling necessary
  Bmp := TBitmap.Create;
  try // need to tile for filling the left side of the page
    if BackgroundPictureTiledWidth<10 then
      BW := 200 else
      BW := BackgroundPictureTiledWidth;
    if BW>BackgroundPicture.Width then
      BW := BackgroundPicture.Width;
    Bmp.Width := BW;
    Bmp.Height := BackgroundPicture.Height;
    Bmp.Canvas.Draw(0,0,BackgroundPicture);
    repeat
      dec(W,Bmp.Width);
      Canvas.Draw(W,4,Bmp); // replicate leftmost of the bitmap
    until W<=4;
  finally
    Bmp.Free;
  end;
end;

function TSynPage.ToolBarCount: integer;
begin
  result := AdvToolBarCount;
end;


{ TSynPager }

class function TSynPager.CreatePager(aOwner: TCustomForm; NoTabVisible: boolean=false): TSynPager;
begin
  result := TSynPager.Create(aOwner);
  result.Parent := aOwner;
  if NoTabVisible then begin
    result.TabSettings.Height := 0;
    result.Height := 119;
    result.HelpButton.Hide;
  end else
    result.Height := 145;
  result.ToolBarStyler := TAdvToolBarOfficeStyler.Create(aOwner);
  TAdvToolBarOfficeStyler(result.ToolBarStyler).Style := bsOffice2007Luna;
end;

function TSynPager.AddPage(aPage: TSynPage): integer;
begin
  result := AddAdvPage(aPage);
end;

function TSynPager.AddPage(const aCaption: string): integer;
begin
  result := AddAdvPage(aCaption);
end;

function TSynPager.GetSynPage(aIndex: integer): TSynPage;
begin
  result := TSynPage(AdvPages[aIndex]);
end;

function TSynPager.GetSynPageCount: integer;
begin
  result := AdvPageCount;
end;

{$endif USETMSPACK}


{ TSynToolBar }

function TSynToolBar.CreateToolButton(ButtonClick: TNotifyEvent;
  iAction, ImageListFirstIndex: integer; const ActionName, ActionHints: string;
  var ShortCutUsed: TFreeShortCut; ButtonWidth: integer; Images: TCustomImageList): TSynToolButton;
begin
  result := TSynToolButton.Create(self);
  result.Caption := ActionName;
{$ifdef USETMSPACK}
  result.Height := (ClientHeight-CaptionHeight)-4;
  result.Width := ButtonWidth;
  result.Layout := blGlyphTop;
  result.ShortCutHint := ShortCutUsed.FindFreeShortCut(result.Caption);
  result.ShortCutHintPos := shpBottom;
  result.Images := Images;
{$endif}
  result.Parent := self;
  result.OnClick := ButtonClick;
  result.ImageIndex := iAction-ImageListFirstIndex; // store the button enumerate value in ImageIndex
  result.Tag := iAction;
  result.Hint := GetCSVItemString(pointer(ActionHints),result.ImageIndex,#13);
  if result.Hint<>'' then
    result.ShowHint := true;
end;


{ TSQLRibbon }

function CaptionName(OnCaptionName: TOnCaptionName;
  Action: PShortString; Obj: TObject=nil; Index: integer=-1): string;
begin
  if Assigned(OnCaptionName) then begin
    result := OnCaptionName(Action,Obj,Index);
    if result<>'' then
      exit;
  end;
  // default implementation use RTTI
  if Obj=nil then
    result := TSQLRecord.CaptionName(Action) else
    if Obj.InheritsFrom(TSQLRecord) then
      result := TSQLRecord(Obj).CaptionName else
      result := TSQLRecord.CaptionName(PPointer(PtrInt(Obj.ClassType)+vmtClassName)^);
  if Index>=0 then
    result := result+' '+IntToStr(Index);
end;


procedure TSQLRibbon.BodyResize(Sender: TObject);
var P: integer;
begin
  for P := 0 to high(Page) do
  if Page[P]<>nil then
    Page[P].TableToGrid.Resize(Sender);
end;


{$ifdef USETMSPACK}

const // bulky TMS styles don't match :(
  TToolBarStyleToPanel: array[TToolBarStyle] of TAdvPanelStyle =
  (psOffice2003Blue, psOffice2003Silver, psOffice2003Olive, psOffice2003Classic,
   psOffice2007Luna, psOffice2007Obsidian, psWindowsXP, psWhidbey,
   psOffice2003Olive {=bsCustom}, psOffice2007Silver, psXP, psWindowsVista,
   psWindows7, psTerminal, psOffice2010Blue, psOffice2010Silver, psOffice2010Black);

procedure TSQLRibbon.ChangeColorScheme(
  const ColorScheme: TToolBarStyle;
  PanelStyler: TAdvPanelStyler;
  StatusBarStyler: TAdvOfficeStatusBarOfficeStyler;
  CustomStyle: TMemoryStream);
var PreviewStyle: TPreviewMenuStyle;
    StatusStyle: TOfficeStatusBarStyle;
    TBStyler: TAdvToolBarOfficeStyler;
    i, curr: Integer;
    C: TComponent;
begin
  if not fToolBar.ToolBarStyler.InheritsFrom(TAdvToolBarOfficeStyler) then
    exit;
  TBStyler := TAdvToolBarOfficeStyler(ToolBar.ToolBarStyler);
  if TBStyler=nil then
    exit;
  TBStyler.Style := ColorScheme;
  case ColorScheme of // bulky TMS styles don't match :(
    bsCustom: begin
      if CustomStyle<>nil then begin
        CustomStyle.Seek(0,soFromBeginning);
        CustomStyle.ReadComponent(TBStyler);
      end;
      PreviewStyle := AdvPreviewMenuStylers.psOffice2003Olive;
      StatusStyle := AdvOfficeStatusBarStylers.psOffice2003Olive;
    end;
    bsOffice2007Silver: begin
      PreviewStyle := AdvPreviewMenuStylers.psOffice2007Silver;
      StatusStyle := AdvOfficeStatusBarStylers.psOffice2007Silver;
    end;
    bsOfficeXP: begin
      PreviewStyle := AdvPreviewMenuStylers.psOfficeXP;
      StatusStyle := AdvOfficeStatusBarStylers.psWindowsXP;
    end;
    bsWindowsVista..high(TToolBarStyle): begin
      PreviewStyle := TPreviewMenuStyle(ColorScheme);
      StatusStyle := TOfficeStatusBarStyle(pred(ColorScheme));
    end;
    else begin
      PreviewStyle := TPreviewMenuStyle(ColorScheme);
      StatusStyle := TOfficeStatusBarStyle(ColorScheme);
    end;
  end;
  if (PreviewMenu<>nil) and
     PreviewMenu.Styler.InheritsFrom(TAdvPreviewMenuOfficeStyler) then
    TAdvPreviewMenuOfficeStyler(PreviewMenu.Styler).Style := PreviewStyle;
  if StatusBarStyler<>nil then
    StatusBarStyler.Style := StatusStyle;
  if PanelStyler<>nil then
    PanelStyler.Style := TToolBarStyleToPanel[ColorScheme];
  TBStyler.CaptionAppearance.Assign(TBStyler.GroupAppearance.CaptionAppearance);
  with TBStyler.GroupAppearance.TabAppearance do begin
    TBStyler.PagerCaption.TextColor := TextColor;
    TBStyler.PagerCaption.TextColorExtended := TextColorSelected;
  end;
  if (Body<>nil) and Body.AdvOfficePagerStyler.InheritsFrom(TAdvOfficePagerOfficeStyler) then
    TAdvOfficePagerOfficeStyler(Body.AdvOfficePagerStyler).Style :=
      TOfficePagerStyle(StatusBarStyler.Style);
  // update colors for windows
  for i := 0 to Application.ComponentCount-1 do begin
    C := Application.Components[i];
    if C.InheritsFrom(TCustomForm) then
      SetStyle(C,TBStyler); // will set style for all embedded components
  end;
  // update report colors on every ribbon page
  curr := fToolBar.ActivePageIndex;
  for i := 0 to high(Page) do
  with Page[i] do begin
    if Report<>nil then
      Report.Color := TBStyler.QATAppearance.ColorTo;
    if i=curr then
      List.Invalidate; // repaint list first row with new colors
  end;
  if Form<>nil then
    Form.Invalidate; // whole form redraw
end;
{$endif USETMSPACK}

constructor TSQLRibbon.Create(Owner: TCustomForm; ToolBar: TSynPager; Body: TSynBodyPager;
      aImageList32,aImageList16: TImageList;
      Client: TSQLRestClientURI; aUserRights: TSQLFieldBits;
      aOnValueText: TValueTextEvent; SetAction: TSQLRibbonSetActionEvent;
      const ActionsTBCaptionCSV, ActionsHintCaption: string; ActionIsNotButton: pointer;
      aOnActionClick: TSQLListerEvent; RefreshActionIndex, ViewToolbarIndex: integer;
      aHideDisabledButtons: boolean;
      PagesCount: integer; TabParameters: PSQLRibbonTabParameters; TabParametersSize: integer;
      const GroupCSV: string; const BackgroundPictureResourceNameCSV: string='');
{$ifdef USETMSPACK}
var Pic: TGDIPPicture;
    PicUsed: boolean;
    ResName: string;
    PS: TSynPage;
    PB: PChar;
{$else}
var aPageFirst: integer;
{$endif}
    aGroup: integer;
    PC: PChar;
    aPage: integer;
    ActionsHints: string;
    TP: PSQLRibbonTabParameters;
begin
  if (Owner=nil) or (TabParameters=nil) or (Client=nil) then
    exit;
  fActionsHintCaption := ActionsHintCaption;
  fTabParameters := TabParameters;
  fTabParametersSize := TabParametersSize;
  fClient := Client;
  fForm := Owner;
  if ToolBar=nil then begin
    ToolBar := TSynPager.CreatePager(Owner);
{$ifdef USETMSPACK}
    fPreviewMenu := TAdvPreviewMenu.Create(Owner);
    fPreviewMenu.Styler := TAdvPreviewMenuOfficeStyler.Create(Owner);
    TAdvPreviewMenuOfficeStyler(fPreviewMenu.Styler).Style :=
      AdvPreviewMenuStylers.psOffice2007Luna;
    fPreviewMenuButton := TAdvShapeButton.Create(Owner);
    fPreviewMenuButton.Appearance.Shape := bsOrb;
    fPreviewMenuButton.Parent := ToolBar;
    fPreviewMenuButton.SetBounds(6,6,45,45);
    fPreviewMenuButton.AdvPreviewMenu := fPreviewMenu;
{$endif}
  end;
  fToolBar := ToolBar;
  if Body=nil then begin
    Body := TSynBodyPager.Create(Owner);
    Body.Parent := Owner;
{$ifdef USETMSPACK}
    Body.AdvOfficePagerStyler := TAdvOfficePagerOfficeStyler.Create(Owner);
    TAdvOfficePagerOfficeStyler(Body.AdvOfficePagerStyler).Style :=
      AdvOfficePagerStylers.psOffice2007Luna;
    Body.TabSettings.Height := 0;
{$endif}
  end;
  fBody := Body;
  fOnActionClick := aOnActionClick;
  fRefreshActionIndex:= RefreshActionIndex;
  Body.Align := alClient;
  SetLength(Page,PagesCount);
  TP := TabParameters;
  for aPage := 0 to PagesCount-1 do begin
    if TP^.CustomHint<>nil then
      ActionsHints := LoadResString(TP^.CustomHint) else
      ActionsHints := TP^.Table.CaptionName(nil,true); // ForHint=true
    ActionsHints := StringReplace(ActionsHintCaption,'%s',ActionsHints,[rfReplaceAll]);
    Page[aPage] := TSQLRibbonTab.Create(ToolBar, Body, aImageList32, aImageList16,
      ShortCuts, TP^, Client, aUserRights, aOnValueText,
      SetAction, ActionsTBCaptionCSV, ActionsHints, ActionIsNotButton,
      aOnActionClick, ViewToolbarIndex, aHideDisabledButtons);
    Inc(PtrInt(TP),TabParametersSize);
  end;
  aPage := 0;
  aGroup := 0;
  PC := pointer(GroupCSV);  // 'Tab 1,Tab 2,Tab 3'
{$ifdef USETMSPACK}
  PB := pointer(BackgroundPictureResourceNameCSV);
  while PC<>nil do
    with ToolBar.TabGroups.Add do begin
      Caption := GetNextItemString(PC);
      CaptionAlignment := Classes.taCenter;
      TabIndexStart := aPage;
      PicUsed := false;
      ResName := GetNextItemString(PB);
      if (ResName='') or
         (FindResource(HInstance,pointer(ResName),RT_RCDATA)=0) then
        Pic := nil else
        Pic := TGDIPPicture.Create;
        if Pic<>nil then
          Pic.LoadFromResourceName(HInstance,ResName);
      try
        while (aPage<PagesCount) and (TabParameters^.Group=aGroup) do begin
          if Pic<>nil then begin
            PS := TSynPage(ToolBar.AdvPages[aPage]);
            if PS.InheritsFrom(TSynPage) then begin
              PS.BackgroundPicture := Pic;
              if not PicUsed then begin
                PS.BackgroundPictureStored := True;
                PicUsed := true;
              end;
            end;
          end;
          inc(PtrInt(TabParameters),TabParametersSize);
          inc(aPage);
        end;
      finally
        if (Pic<>nil) and not PicUsed then
          Pic.Free;
      end;
      TabIndexEnd := aPage-1;
      inc(aGroup);
    end;
{$else}
  while PC<>nil do begin
    aPageFirst := aPage;
    while (aPage<PagesCount) and (TabParameters^.Group=aGroup) do begin
      inc(PtrInt(TabParameters),TabParametersSize);
      inc(aPage);
    end;
    ToolBar.TabGroupsAdd(aPageFirst,aPage,GetNextItemString(PC));
    inc(aGroup);
  end;
  ToolBar.FormNoCaption;
{$endif}
  ToolBar.OnChange := ToolBarChange;
  Body.OnResize := BodyResize;
  BodyResize(nil);
end;


destructor TSQLRibbon.Destroy;
var P: integer;
begin
  if Form<>nil then begin
    KillTimer(Form.Handle,WM_TIMER_REFRESH_SCREEN); // avoid GPF
    KillTimer(Form.Handle,WM_TIMER_REFRESH_REPORT);
  end;
  for P := 0 to high(Page) do
    FreeAndNil(Page[P]);
  inherited;
end;

function TSQLRibbon.GetActivePage: TSQLRibbonTab;
var P: Integer;
begin
  result := nil;
  if (Self=nil) or (ToolBar=nil) then
    exit;
  P := Toolbar.ActivePageIndex;
  if cardinal(P)<=cardinal(high(Page)) then
    result := Page[P];
end;

function TSQLRibbon.GetPage(aRecordClass: TSQLRecordClass): integer;
begin
  if self<>nil then
    for result := 0 to high(Page) do
      if (Page[result]<>nil) and (Page[result].Table=aRecordClass) then
        exit;
  result := -1;
end;

function TSQLRibbon.GetParameter(aPageIndex: Integer): PSQLRibbonTabParameters;
begin
  if (Self=nil) or (fTabParameters=nil) or (fTabParametersSize=0) or
     (cardinal(aPageIndex)>cardinal(high(Page)) )then
    result := nil else
    result := Pointer(PtrInt(fTabParameters)+fTabParametersSize*aPageIndex);
end;

procedure TSQLRibbon.GotoRecord(aTable: TSQLRecordClass; aID, ActionToPerform: integer);
var P,R: integer;
begin
  if (self=nil) or (aTable=nil) or (aID<=0) then
    exit; // no record to jump in
  P := GetPage(aTable);
  if P>=0 then
    with Page[P] do
    if TableToGrid<>nil then begin
      R := TableToGrid.Table.RowFromID(aID);
      if R<0 then exit;
      if ToolBar.ActivePageIndex<>P then begin
        ToolBar.ActivePageIndex := P;
        Application.ProcessMessages;
      end;
      TableToGrid.Refresh;
      R := TableToGrid.Table.RowFromID(aID); // do it now after Grid refresh
      if R<0 then exit;
      List.Row := R;
      Application.ProcessMessages;
      Form.BringToFront;
      Application.ProcessMessages;
      if (ActionToPerform<>0) and Assigned(fOnActionClick) then
        // Sender needs to be <> nil
        fOnActionClick(self,Table,ActionToPerform);
    end;
end;

procedure TSQLRibbon.GotoRecord(aRecord: TSQLRecord; ActionToPerform: integer);
begin
  if (self<>nil) and (aRecord<>nil) then
    GotoRecord(aRecord.RecordClass,aRecord.ID);
end;

function TSQLRibbon.RefreshClickHandled(Sender: TObject;
  RecordClass: TSQLRecordClass; ActionValue: integer;
  out Tab: TSQLRibbonTab): boolean;
var aP: integer;
begin
  result := true; // caller must exit now
  aP := GetPage(RecordClass);
  if aP<0 then
    exit;
  Tab := Page[aP];
  if (Sender<>nil) and (fRefreshActionIndex<>0) and
     (ActionValue=fRefreshActionIndex) and not Tab.TableToGrid.Refresh then begin
    Sender := nil;
    ActionValue := Tab.List.Row;
  end;
  if Sender=nil then begin
    if Tab.FrameRight<>nil then begin
      if Tab.Retrieve(Client,ActionValue) then begin
        Tab.FrameRight.Show;
        if Tab.Report<>nil then begin
          if ReportAutoFocus and
             (Form.Focused or Tab.List.Focused) and Tab.Report.CanFocus then
            Tab.Report.SetFocus;
          SetTimer(Form.Handle,WM_TIMER_REFRESH_REPORT,200,nil);
        end;
      end else
        Tab.FrameRight.Hide;
    end;
    exit;
  end;
  result := false; // caller must handle the action
end;

procedure TSQLRibbon.RefreshPage(Page: TSQLRibbonTab);
var P: TSQLRibbonTab;
begin
  if (self=nil) or (Page=nil) or (fRefreshActionIndex=0) then
    exit;
  if Assigned(fOnActionClick) then
    fOnActionClick(Page.Lister,Page.Table,fRefreshActionIndex) else
    RefreshClickHandled(Page.Lister,Page.Table,fRefreshActionIndex,P);
end;

procedure TSQLRibbon.ToolBarChange(Sender: TObject);
var aPage: integer;
begin
  KillTimer(Form.Handle,WM_TIMER_REFRESH_SCREEN);
  KillTimer(Form.Handle,WM_TIMER_REFRESH_REPORT);
  aPage := fToolBar.ActivePageIndex;
  if cardinal(aPage)>cardinal(high(Page)) then
    exit;
  with Page[fLastActiveTab] do begin
    TableToGrid.PageChanged;
    if Report<>nil then // release some unused GDI resources
      FreeAndNil(Report.PreviewSurfaceBitmap);
  end;
  fLastActiveTab := aPage;
  fBody.ActivePageIndex := aPage;
  if fBody.Visible and (fRefreshActionIndex<>0) then begin
    RefreshPage(Page[aPage]);
    if (Form<>nil) and GetParameter(aPage)^.AutoRefresh then
      // set Timer to refresh the screen every second
      SetTimer(Form.Handle,WM_TIMER_REFRESH_SCREEN,1000,nil);
  end;
end;

procedure TSQLRibbon.WMRefreshTimer(var Msg: TWMTimer);
var i: integer;
begin
  if (self=nil) or (Form=nil) or (csDestroying in Form.ComponentState) then
    exit; // avoid GPF
  i := ToolBar.ActivePageIndex;
  if cardinal(i)>cardinal(high(Page)) then
    exit;
  case Msg.TimerID of // action from Timer ID
  WM_TIMER_REFRESH_SCREEN: // used to send a Refresh command (default every second)
    if (fRefreshActionIndex<>0) and
      GetParameter(i)^.AutoRefresh then // refresh Workstation content
      RefreshPage(Page[i]);
  WM_TIMER_REFRESH_REPORT: with Page[i] do
    if (FrameRight<>nil) and (Report<>nil) and
       Retrieve(Client,List.Row) then begin
      // used to recreate the current selected report
      // use a timer, otherwize marking is buggy and UI experience is poor
      KillTimer(Form.Handle,WM_TIMER_REFRESH_REPORT); // only refresh once
      try
        Report.BeginDoc;
        CreateReport(i);
      finally
        Report.EndDoc; // so we ignore any reporting errors
      end;
    end;
  end;
end;

function TSQLRibbon.AddToReport(aReport: TGDIPages; aRecord: TSQLRecord;
  WithTitle: Boolean; CSVFieldNames, CSVFieldNameToHide: PUTF8Char;
  OnCaptionName: TOnCaptionName;
  ColWidthName, ColWidthValue: integer): string;
var OldWordWrapLeftCols: boolean;
    i: integer;
    aName: RawUTF8;
    RibbonParams: PSQLRibbonTabParameters;
    PHint: PChar; // map FieldHints
    aHint: string;
begin
  result := '';
  if (Self=nil) or (aReport=nil) or (aRecord=nil) or (Client=nil) then
    exit;
  PHint := nil;
  RibbonParams := GetParameter(aRecord.RecordClass);
  if RibbonParams<>nil then
    with RibbonParams^ do
      if EditFieldHintsToReport and (EditFieldHints<>nil) then
        PHint := pointer(LoadResString(EditFieldHints));
  result := U2S(Client.MainFieldValue(aRecord.RecordClass,aRecord.ID,true));
  if WithTitle then begin
    aReport.DrawTitle(aRecord.CaptionName+' : '+result,true);
    aReport.NewHalfLine;
  end;
  OldWordWrapLeftCols := aReport.WordWrapLeftCols;
  aReport.WordWrapLeftCols := true; // automatic word wrap and #13 for next line
  aReport.AddColumns([ColWidthName,ColWidthValue]);
  aReport.SetColumnBold(0);
  aReport.AddColumnHeaders([],true);
  with aRecord.RecordProps do begin
    for i := 0 to high(Fields) do begin
      aHint := GetNextItemString(PHint,'|'); // ALL fields are listed: do it now
      aName := FieldsName[i];
      if ((CSVFieldNameToHide<>nil) and
          (FindCSVIndex(CSVFieldNameToHide,aName,',',false)>=0)) or
         ((CSVFieldNames<>nil) and
          (FindCSVIndex(CSVFieldNames,aName,',',false)<0)) then
        continue; // display properties listed in optional CSVFieldNames parameter
      if aHint<>'' then begin
        aReport.Font.Color := clNavy;
        aReport.DrawText(aHint);
        aReport.Font.Color := clBlack;
      end;
      aReport.DrawTextAcrossCols([CaptionName(OnCaptionName,@Fields[i]^.Name),
       Language.PropToString(Fields[i],aRecord,Client)]); // PropToString do all the magic
      if aHint<>'' then
        aReport.NewLine else
        aReport.NewHalfLine;
    end;
  end;
  aReport.WordWrapLeftCols := oldWordWrapLeftCols;
end;

function TSQLRibbon.DeleteMarkedEntries(aTable: TSQLRecordClass; const ActionHint: string): boolean;
var Tab: TSQLRibbonTab;
    aP, n, i: integer;
begin
  result := True; // success by default
  if (self=nil) or (aTable=nil) or (Client=nil)  then
    exit;
  aP := GetPage(aTable);
  if aP<0 then
    exit;
  Tab := Page[aP];
  with Tab.TableToGrid do begin
     n := MarkedTotalCount;
     if n=0 then
       if (Tab.List.Row<1) or (YesNo(ActionHint,
         U2S(Client.MainFieldValue(aTable,Tab.CurrentID,true)),false)=ID_NO) or
          not Client.Delete(aTable,Tab.CurrentID) then begin
         result := false;
         exit;
       end else
         GotoRecord(aTable,Table.IDColumnHiddenValue(Tab.List.Row+1)) else
     if YesNo(ActionHint,Format(sDeleteN,[n]),false)=ID_NO then
       exit else
     if Client.TransactionBegin(aTable) then
     try
       for i := Table.RowCount downto 1 do
         if Marked[i] then
           if not Client.Delete(aTable,Table.IDColumnHiddenValue(i)) then begin
             Client.RollBack;
             result := false;
             break;
           end;
       SetMark(actUnmarkAll);
       Tab.List.Row := 0;
       Refresh;
     finally
       Client.Commit; // will do nothing if Client.RollBack has been called
     end;
  end;
end;

function TSQLRibbon.ExportRecord(aTable: TSQLRecordClass; aID: integer;
  const ActionHint: string; OpenAfterCreation: boolean): TFileName;
var i: integer;
    aName, ext: TFileName;
    Rep: TGDIPages;
    Content: string;
begin
  result := '';
  if (self=nil) or (aTable=nil) or (Client=nil)  then
    exit;
  if GetPage(aTable)<0 then
    exit; // CreateReport need a known record type
  if aID>0 then
    aName := U2S(Client.MainFieldValue(aTable,aID,true)) else
    aName := aTable.CaptionName;
  with TSaveDialog.Create(Application.MainForm) do
  try
    Title := ActionHint;
    Options := [ofOverwritePrompt,ofHideReadOnly,ofEnableSizing];
    Filter := sPDFFile+' (*.pdf)|*.pdf|'+sTextFile+' (*.txt)|*.txt';
    DefaultExt := '.pdf';
    FileName := aName;
    if not Execute then
      exit;
    aName := FileName;
  finally
    Free;
  end;
  ext := ExtractFileExt(aName);
  Rep := TGDIPages.Create(nil); // use a temp report to create text
  try
    Screen.Cursor := crHourGlass;
    Rep.ForceCopyTextAsWholeContent := true; // headers copied once
    CreateReport(aTable,aID,Rep);
    Rep.EndDoc;
    if SameText(ext,'.PDF') then begin
      Rep.Caption := aName;
      if not Rep.ExportPDF(aName,false) then
        exit;
    end else
    if SameText(ext,'.TXT') then begin
      for i := 0 to Rep.Pages.Count-1 do
        Content := Content+Rep.Pages[i]; // append content of every page
      // export as ANSI text file, in the current code page
        if not FileFromString(
          {$ifdef UNICODE}
          CurrentAnsiConvert.UnicodeBufferToAnsi(pointer(Content),length(Content))
          {$else}
          Content
          {$endif} ,aName) then
          exit;
    end else
      exit; // invalid extension
    if OpenAfterCreation then
      ShellExecute(Application.DialogHandle,nil,pointer(aName),nil,nil,SW_SHOWNORMAL);
    result := aName; // mark success
  finally
    Screen.Cursor := crDefault;
    Rep.Free;
  end;
end;

function TSQLRibbon.GetParameter(aTable: TSQLRecordClass): PSQLRibbonTabParameters;
begin
  result := GetParameter(GetPage(aTable));
end;

function TSQLRibbon.MarkedEntriesToReport(aTable: TSQLRecordClass;
  const ColWidths: array of integer; aRep: TGDIPages): TGDIPages;
var P, R, F: integer;
    ColText: TSynUnicodeDynArray;
    ColWidth: TIntegerDynArray;
begin
  P := GetPage(aTable);
  if (P<0) or (Page[P].TableToGrid.MarkedTotalCount=0) then begin
    result := nil;
    exit;
  end;
  if aRep<>nil then
    result := aRep else
    result := TGDIPages.Create(nil);
  result.WordWrapLeftCols := true; // word wrap so that we won't loose any data
  CreateReport(aTable,-1,result); // create footer
  result.DrawTitle(aTable.CaptionName,true);
  with Page[P].TableToGrid do begin
    if length(ColWidths)=Table.FieldCount then begin
      SetLength(ColWidth,Table.FieldCount);
      move(ColWidths[0],ColWidth[0],Table.FieldCount*4);
    end else
      Table.CalculateFieldLengthMean(ColWidth,true); // FromDisplay=true
    result.AddColumns(ColWidth);
    SetLength(ColText,Table.FieldCount);
    for F := 0 to Table.FieldCount-1 do
      Table.ExpandAsSynUnicode(0,F,Client,ColText[F]);
    result.AddColumnHeaders(ColText,true,true); // true = with gray bottom line
    for R := 1 to Table.RowCount do
    if Marked[R] then begin
      for F := 0 to Table.FieldCount-1 do
        Table.ExpandAsSynUnicode(R,F,Client,ColText[F]);
      result.DrawTextAcrossCols(ColText);
    end;
  end;
end;

procedure TSQLRibbon.CreateReport(aPageIndex: Integer);
var P: TSQLRibbonTab;
begin
  if cardinal(aPageIndex)>cardinal(high(Page)) then
    exit;
  P := Page[aPageIndex];
  if P<>nil then
    CreateReport(P.Table,P.CurrentRecord.ID,P.Report,false);
end;

procedure TSQLRibbon.CreateReport(aTable: TSQLRecordClass; aID: integer;
  aReport: TGDIPages; AlreadyBegan: boolean=false);
var P: integer;
begin
  if (aReport<>nil) and (aTable<>nil) and (self<>nil) then
    if aID=0 then
      MarkedEntriesToReport(aTable,[],aReport) else
    with aReport do begin
      if not AlreadyBegan then begin
        Clear;
        BeginDoc;
      end;
      if (aID>0) and (Caption='') then
        Caption := U2S(Client.MainFieldValue(aTable,aID,true));
      Font.Size := 9;
      AddPagesToFooterAt(
        format('%s - %s %s',[sPageN,aTable.CaptionName,Caption]),LeftMargin);
      Font.Size := 10;
      P := GetPage(aTable);
      if (P<0) or (aID<=0) then
        exit;
      with self.Page[P] do
        if CurrentRecord.ID=aID then
          AddToReport(aReport,CurrentRecord,True,nil,
              pointer(GetParameter(P)^.EditFieldNameToHideCSV));
    end;
end;

function TSQLRibbon.FindButton(aTable: TSQLRecordClass; aActionIndex: integer): TSynToolButton;
var P: integer;
begin
  P := GetPage(aTable);
  if P<0 then
    result := nil else
    result := Page[P].Lister.FindButton(aActionIndex);
end;

procedure TSQLRibbon.SetButtonHint(aTable: TSQLRecordClass;
  aActionIndex: integer; const aHint: string);
var Btn: TSynToolButton;
begin
  Btn := FindButton(aTable,aActionIndex);
  if Btn<>nil then
    Btn.Hint := aHint;
end;

procedure TSQLRibbon.Refresh(aTable: TSQLRecordClass);
var Tab: TSQLRibbonTab;
begin
  if aTable=nil then begin
    Tab := GetActivePage;
    if Tab=nil then
      exit;
    aTable := Tab.Table;
  end;
  RefreshClickHandled(self,aTable,fRefreshActionIndex,Tab);
end;

procedure TSQLRibbon.AddToReport(aReport: TGDIPages; Table: TSQLTable;
  const ColWidths: array of integer);
var R,F: integer;
    ColText: TSynUnicodeDynArray;
    ColWidth: TIntegerDynArray;
    aClient: TSQLRest;
begin
  if (aReport=nil) or (Table=nil) or (Table.FieldCount=0) then
    exit;
  if self=nil then
    aClient := nil else
    aClient := Client;
  if length(ColWidths)=Table.FieldCount then begin
    SetLength(ColWidth,Table.FieldCount);
    move(ColWidths[0],ColWidth[0],Table.FieldCount*4);
  end else
    Table.CalculateFieldLengthMean(ColWidth,true); // FromDisplay=true
  aReport.AddColumns(ColWidth);
  SetLength(ColText,Table.FieldCount);
  for F := 0 to Table.FieldCount-1 do
    Table.ExpandAsSynUnicode(0,F,aClient,ColText[F]);
  aReport.AddColumnHeaders(ColText,true,true); // true = with gray bottom line
  for R := 1 to Table.RowCount do begin
    for F := 0 to Table.FieldCount-1 do
      Table.ExpandAsSynUnicode(R,F,aClient,ColText[F]);
    aReport.DrawTextAcrossCols(ColText);
  end;
end;


end.

