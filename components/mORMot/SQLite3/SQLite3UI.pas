/// Grid to display Database content
// - this unit is a part of the freeware Synopse SQLite3 database framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3UI;

(*
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse SQLite3 database framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****



       Fill a TDrawGrid with data from our database engine
      *****************************************************

      - associate TSQLTable content to a TDrawGrid
      - display UTF-8 values with true unicode characters
      - column size is calculated from data size
      - field sort by left/right arrow keys or clicking on first row
      - incremental key lookup for direct search inside displayed values
      - Ctrl + click on a cell to display its full unicode content
      - ID column can be hidden on demand, but IDs still remaining in memory

    Initial version: 2008 March, by Arnaud Bouchez

    Version 1.4 - February 8, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.5 - February 19, 2010
    - User Interface Query action implementation

    Version 1.8
    - added TSynIntegerLabeledEdit component (e.g. used in SQLite3UIOptions)

    Version 1.9
    - improved Delphi 2009/2010 UnicodeString compatibility
    - fix some issues, and complete implementation of marking from time elapsed

    Version 1.9.2
    - new ForceRefresh parameter to the TSQLTableToGrid.Refresh method

    Version 1.13
    - now compiles with Delphi 6
    - new TSQLTableToGrid.SelectedRecordCreate method
    - added generic TSynLabeledEdit to handle Integer, Int64, Currency and
      double kind of values

    Version 1.14
    - fixed issue with MaxValue if RangeChecking not enabled

    Version 1.15
    - compatibility with Delphi XE2
    - new TSQLTableToGrid.SetFieldFixedWidth method
    - new TSQLTableToGrid.FieldTitleTruncatedNotShownAsHint property
    - fixed issue on TDrawGrid events when TSQLTableToGrid is destroyed

    Version 1.16
    - new FillStringGrid() function, ready to fill a regular TStringGrid
    - includes new TSynAnsiConvert classes for handling Ansi charsets

    Version 1.17
    -  global functions AddApplicationToFirewall() and AddPortToFirewall()
       are now compatible with Windows XP, Vista and Seven - renamed on purpose 

*)


interface

{$I Synopse.inc}

uses
  Windows,
  SynCommons, SQLite3Commons, SQLite3i18n,
  SysUtils, Classes, Messages, Variants,
  {$ifdef WITHUXTHEME}
  Themes,
  {$endif}
  Graphics, StdCtrls, Controls, Grids, Buttons, ExtCtrls, Forms;

type
  /// a THintWindow descendant, with an internal delay to auto-hide
  // - this component can be used directly with the hint text to be displayed
  // (companion to the controls Hint properties and Application.ShowHint)
  // - you can specify a time interval for the popup window to be hidden
  // - this component expects UTF-8 encoded text, and displays it as Unicode
  THintWindowDelayed = class(THintWindow)
  private
    fRow: integer;
    fCol: integer;
  protected
    fTimerEnabled: boolean;
    fFontColor: TColor;
    fUTF8Text: RawUTF8;
    /// called after a Hide call
    procedure VisibleChanging; override;
    /// used to hide the popup hint after a delay
    procedure WMTimer(var Msg: TWMTimer); message WM_TIMER;
    /// overriden method, Unicode ready
    procedure Paint; override;
  public
    /// initializes the component
    constructor Create(aOwner: TComponent); override;
    /// releases component resources and memory
    destructor Destroy; override;
    /// displays the appropriate Hint Text at a specified screen position
    // - Text is decoded from UTF-8 to Unicode before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedUTF8(const Text: RawUTF8; X,Y,Time: integer;
      FontColor: TColor; AlignLeft: boolean=false); overload;
    /// displays the appropriate Hint Text at a position relative to a control
    // - Text is decoded from UTF-8 to Unicode before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedUTF8(const Text: RawUTF8; Origin: TControl;
      X,Y,Time: integer; FontColor: TColor; AlignLeft: boolean=false); overload;
    /// displays the appropriate Hint Text at a specified screen position
    // - if string is AnsiString (i.e. for Delphi 2 to 2007), Text is decoded into
    // Unicode (using the current i18n code page) before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedString(const Text: string; X,Y,Time: integer;
      FontColor: TColor; AlignLeft: boolean=false); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// displays the appropriate Hint Text at a position relative to a control
    // - Text is decoded from Ansi to Unicode (using the current i18n code page) before display
    // - Time is the maximum text display delay, in milliseconds
    procedure ShowDelayedString(const Text: string; Origin: TControl;
      X,Y,Time: integer; FontColor: TColor; AlignLeft: boolean=false); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// overriden method, Unicode ready
    function CalcHintRect(MaxWidth: Integer; const AHint: RawUTF8; AData: Pointer): TRect; reintroduce;
    /// the column number when the hint is displayed
    property Col: integer read fCol;
    /// the row number when the hint is displayed
    property Row: integer read fRow;
  end;

  /// kind of event used to change some text on the fly for grid display
  // - expect generic string Text, i.e. UnicodeString for Delphi 2009/2010,
  // ready to be used with the VCL for all Delphi compiler versions
  // - if the cell at FiieldIndex/RowIndex is to have a custom content,
  // shall set the Text variable content and return TRUE
  // - if returns FALSE, the default content will be displayed 
  TValueTextEvent = function(Sender: TSQLTable; FieldIndex, RowIndex: Integer; var Text: string): boolean of object;

  /// kind of event used to change some text on the fly for popup hint
  // - expect generic string Text, i.e. UnicodeString for Delphi 2009/2010,
  // ready to be used with the VCL for all Delphi compiler versions
  THintTextEvent = function(Sender: TSQLTable; FieldIndex, RowIndex: Integer; var Text: string): boolean of object;

  /// kind of event used to display a menu on a cell right click
  TRightClickCellEvent = procedure(Sender: TSQLTable; ACol, ARow, MouseX, MouseY: Integer) of object;

  /// a hidden component, used for displaying a TSQLTable in a TDrawGrid
  // - just call  TSQLTableToGrid.Create(Grid,Table)  to initiate the association
  // - the Table will be released when no longer necessary
  // - any former association by TSQLTableToGrid.Create() will be overriden
  // - handle unicode, column size, field sort, incremental key lookup, hide ID
  // - Ctrl + click on a cell to display its full unicode content
  TSQLTableToGrid = class(TComponent)
  private
    fOnSelectCell: TSelectCellEvent;
    fOnRightClickCell: TRightClickCellEvent;
    fClient: TSQLRestClientURI;
    fOnDrawCellBackground: TDrawCellEvent;
    fMarked: array of byte;
    fMarkAllowed: boolean;
    fMouseDownMarkedValue: (markNone,markOn,markOff);
    fTruncAsHint: Boolean;
    fOnSelectCellProcessing: boolean;
    fFieldIndexTimeLogForMark: integer;
    function GetMarked(RowIndex: integer): boolean;
    procedure SetMarked(RowIndex: integer; const Value: boolean);
    function GetMarkAvailable: boolean;
    function GetDrawGrid: TDrawGrid;
    function GetMarkedIsOnlyCurrrent: boolean;
    function GetMarkedTotalCount: integer;
    // function because field information may be set manually after Create
    function GetFieldIndexTimeLogForMark: integer;
  protected
    fOnValueText: TValueTextEvent;
    fOnHintText: THintTextEvent;
    /// associated TSQLTable result to be displayed
    fTable: TSQLTable;
    /// true if the specific field is in Ascending order
    fFieldOrder: array of boolean;
    /// current field number used for field
    fCurrentFieldOrder: integer;
    /// avoid resizing columns on height change only
    fLastWidth: cardinal;
    /// contain the key to be searched
    fIncrementalSearch: RawUTF8;
    /// true if row is changed by incremental key lookup
    fIncrementalSearchMove: boolean;
    /// used to display some hint text
    fHint: THintWindowDelayed;
    /// text of this field must be centered
    fCentered: Int64;
    /// text of this column/field name has been truncated
    fFieldNameTruncated: Int64;
    /// used by OnTableUpdate() event
    fOnTableUpdateID: TIntegerDynArray;
    /// return true if a GPF may occur
    function NotDefined: boolean;
  public
    /// fill a TDrawGrid with the results contained in a TSQLTable
    constructor Create(aOwner: TDrawGrid; aTable: TSQLTable; aClient: TSQLRestClientURI); reintroduce;
    /// release the hidden object
    // - will be called by the parent Grid when it is destroyed
    // - will be called by any future TSQLTableToGrid.Create() association
    // - free the associated TSQLTable and its memory content
    // - will reset the Grid overriden events to avoid GPF
    destructor Destroy; override;
    /// called by the owner TDrawGrid to draw a Cell from the TSQLTable data
    // - the cell is drawn using direct Win32 Unicode API
    // - the first row (fixed) is drawn as field name (centered bold text with
    //  sorting order displayed with a triangular arrow)
    procedure DrawCell(Sender: TObject; ACol, ARow: Longint; Rect: TRect; State: TGridDrawState);
    /// called by the owner TDrawGrid when a Cell is selected
    procedure DrawGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    /// called by the owner TDrawGrid when a Cell is clicked by the mouse
    // - check if the first (fixed) row is clicked: then change sort order
    // - Ctrl + click to display its full unicode content (see HintText to customize it)
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    /// called by the owner TDrawGrid when the mouse is unclicked over a Cell
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    /// called by the owner TDrawGrid when the mouse is over a Cell
    procedure DrawGridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    /// called by the owner TDrawGrid when the user presses a key
    // - used for incremental key lookup
    procedure DrawGridKeyPress(Sender: TObject; var Key: Char);
    /// called by the owner TDrawGrid when the user presses a key
    // - used for LEFT/RIGHT ARROW column order change
    procedure DrawGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    /// call this procedure to automaticaly resize the TDrawString columns
    // - can be used as TSQLTableToGrid.From(DrawGrid).Resize();
    procedure Resize(Sender: TObject);
    /// display a popup Hint window at a specified Cell position
    // - expect generic string Text, i.e. UnicodeString for Delphi 2009/2010,
    // ready to be used with the VCL for all Delphi compiler versions
    procedure ShowHintString(const Text: string; ACol, ARow, Time: integer;
      FontColor: TColor=clBlack);
    /// if the ID column is available, hides it from the grid
    procedure IDColumnHide;
    /// toggle the sort order of a specified column
    procedure SortChange(ACol: integer);
    /// set a specified column for sorting
    // - if ACol=-1, then the Marked[] rows are shown first, in current sort
    procedure SortForce(ACol: integer; Ascending: boolean; ARow: integer=-1);
    /// get the ID of the first selected row, 0 on error (no ID field e.g.)
    // - usefull even if ID column was hidden with IDColumnHide
    function SelectedID: integer;
    /// retrieve the record content of the first selected row, nil on error
    // - record type is retrieved via Table.QueryTables[0] (if defined)
    // - warning: it's up to the caller to Free the created instance after use
    // (you should e.g. embedd the process in a try...finally block):
    // ! Rec := Grid.SelectedRecordCreate;
    // ! if Rec<>nil then
    // ! try
    // !   DoSomethingWith(Rec);
    // ! finally
    // !   Rec.Free;
    // ! end;
    // - usefull even if ID column was hidden with IDColumnHide
    function SelectedRecordCreate: TSQLRecord;
    /// set columns number which must be centered
    procedure SetCentered(const Cols: array of cardinal); overload;
    /// set a column number which must be centered
    procedure SetCentered(aCol: cardinal); overload;
    /// force the mean of characters length for every field
    // - supply a string with every character value is proportionate to
    // the corresponding column width
    // - if the character is lowercase, the column is set as centered
    // - if aMarkAllowed is set, a first checkbox column is added, for
    // reflecting and updating the Marked[] field values e.g.
    // - if Lengths='', will set some uniform width, left aligned
    procedure SetFieldLengthMean(const Lengths: RawUTF8; aMarkAllowed: boolean);
    /// force all columns to have a specified width, in pixels
    procedure SetFieldFixedWidth(aColumnWidth: integer);
    /// force refresh paint of Grid from Table data
    // - return true if Table data has been successfully retrieved from Client
    // and if data was refreshed because changed since last time
    // - if ForceRefresh is TRUE, the Client is not used to retrieve the data,
    // which must be already refreshed before this call
    function Refresh(ForceRefresh: Boolean=false): boolean;
    /// call this procedure after a refresh of the data
    // - current Row will be set back to aID
    // - called internal by Refresh function above
    procedure AfterRefresh(aID: integer);
    /// you can call this method when the list is no more on the screen
    // - it will hide any pending popup Hint windows, for example
    procedure PageChanged;
    /// perform the corresponding Mark/Unmark[All] Action
    procedure SetMark(aAction: TSQLAction);
    /// retrieve the Marked[] bits array
    function GetMarkedBits: pointer;
    {{ read-only access to a particular row values, as VCL text
     - Model is one TSQLModel instance (used to display TRecordReference)
     - returns the text as generic string, ready to be displayed via the VCL
       after translation, for sftEnumerate, sftTimeLog, sftRecord and all other
       properties
     - uses OnValueText property Event if defined by caller }
    function ExpandRowAsString(Row: integer; Client: TObject): string;
    /// retrieve the associated TSQLTableToGrid from a specific TDrawGrid
    class function From(Grid: TDrawGrid): TSQLTableToGrid;
    /// used by TSQLRestClientURI.UpdateFromServer() to let the client
    // perform the rows update (for Marked[])
    procedure OnTableUpdate(State: TOnTableUpdateState);

    /// associated TDrawGrid
    // - just typecast the Owner as TDrawGrid
    property DrawGrid: TDrawGrid read GetDrawGrid;
    /// associated TSQLTable to be displayed
    property Table: TSQLTable read fTable;
    /// associated Client used to retrieved the Table data
    property Client: TSQLRestClientURI read fClient;
    /// used to display some hint text
    property Hint: THintWindowDelayed read fHint;
    /// assign an event here to customize the background drawing of a cell
    property OnDrawCellBackground: TDrawCellEvent read fOnDrawCellBackground
      write fOnDrawCellBackground;
    /// individual bits of this field is set to display a column data as centered
    property Centered: Int64 read fCentered;
    /// true if Marked[] is available (add checkboxes at the left side of every row)
    property MarkAllowed: boolean read fMarkAllowed;
    /// true if any Marked[] is checked
    property MarkAvailable: boolean read GetMarkAvailable;
    /// true if only one entry is in Marked[], and it is the current one
    property MarkedIsOnlyCurrrent: boolean read GetMarkedIsOnlyCurrrent;
    /// returns the number of item marked or selected
    // - if no item is marked, it return 0 even if a row is currently selected
    property MarkedTotalCount: integer read GetMarkedTotalCount;
    /// retrieves if a row was previously marked
    // - first data row index is 1
    property Marked[RowIndex: integer]: boolean read GetMarked write SetMarked;
    /// retrieves the index of the sftTimeLog first field
    // - i.e. the field index which can be used for Marked actions
    // - equals -1 if not such field exists
    property FieldIndexTimeLogForMark: integer read GetFieldIndexTimeLogForMark;
    /// current field number used for current table sorting
    property CurrentFieldOrder: integer read fCurrentFieldOrder;
    /// set to FALSE to display the column title as hint when truncated on screen
    property FieldTitleTruncatedNotShownAsHint: boolean read fTruncAsHint write fTruncAsHint;
    /// override this event to customize the text display in the table
    property OnValueText: TValueTextEvent read fOnValueText write fOnValueText;
    /// override this event to customize the Ctrl+Mouse click popup text
    property OnHintText: THintTextEvent read fOnHintText write fOnHintText;
    /// override this event to customize the Mouse click on a data cell
    property OnSelectCell: TSelectCellEvent read fOnSelectCell write fOnSelectCell;
    /// override this event to customize the Mouse right click on a data cell
    property OnRightClickCell: TRightClickCellEvent read fOnRightClickCell write fOnRightClickCell;
  end;

type
  /// exception class raised by TSynIntegerLabeledEdit
  ESynLabeledEdit = class(Exception);

  /// diverse kind of values which may be edited by a TSynLabeledEdit
  TSynLabeledEditKind = (sleInteger, sleInt64, sleCurrency, sleDouble);

  /// TLabeledEdit with optional boundaries check of a Variant value
  TSynLabeledEdit = class(TLabeledEdit)
  protected
    FMaxValue: Variant;
    FMinValue: Variant;
    FAdditionalHint: string;
    FKind: TSynLabeledEditKind;
    FRangeChecking: boolean;
    function IsValid(const Txt: string; var ToValue: Variant): Boolean;
    procedure SetValue(const Value: Variant);
    function GetValue: Variant;
    procedure KeyPress(var Key: char); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
  public
    /// if true, GetValue() will raise an ESynVariantLabeledEdit exception on
    // any Variant value range error, when the Value property is read
    RaiseExceptionOnError: boolean;
    /// convert the entered Variant value into a textual representation
    function ToString(NumberOfDigits: integer): string; reintroduce;
    /// return TRUE if the entered value is inside the boundaries
    function ValidateValue: boolean;
    /// create the component instance
    constructor Create(AOwner: TComponent); override;
  published
    /// the kind of value which is currently edited by this TSynLabeledEdit
    property Kind: TSynLabeledEditKind read fKind write fKind default sleInteger;
    /// the entered value
    // - getting this property will check for in range according to the
    // current MinValue/MaxValue boundaries, if RangeChecking is set
    // - if RangeChecking is not set, could return a NULL variant for no data
    // - it will sound a beep in case of any out of range
    // - it will also raise a ESynVariantLabeledEdit exception if
    // RaiseExceptionOnError is set to TRUE (equals FALSE by default)
    property Value: Variant read GetValue write SetValue;
    /// set to TRUE if MinValue/MaxValue properties must be checked when
    // reading Value property
    property RangeChecking: boolean read fRangeChecking write fRangeChecking;
    /// lowest allowed Variant value
    property MinValue: Variant read FMinValue write FMinValue;
    /// highest allowed Variant value
    property MaxValue: Variant read FMaxValue write FMaxValue;
    /// some additional popup hint to be displayed
    // - by default, the allowed range is displayed: 'Min. Value: #, Max. Value: #'
    // - you can specify here some additional text to be displayed when the mouse
    // is hover the component
    property AdditionalHint: string read FAdditionalHint write FAdditionalHint;
  end;


/// register the TSynIntegerLabeledEdit component in the IDE toolbar
// - not necessary for the SQLite3 framework to run: since all User Interface
// is created from code, and not from the Delphi IDE, you don't have to register
// anything
procedure Register;


resourcestring
  SErrorFieldNotValid = 'Field "%s"'#13'does not contain a valid %s value';
  SErrorFieldTooSmall = 'Field "%s"'#13'is too small, value must be >= %s';
  SErrorFieldTooLarge = 'Field "%s"'#13'is too large, value must be <= %s';
  SMinMaxValue = 'Min. Value: %s, Max. Value: %s';

{$define VISTAFORM}

{$ifdef VISTAFORM}

{$R SQLite3UI.RES}

type
  /// Vista-enabled TForm descendant
  // - this form will have a button in the TaskBar
  // - this form will hide the default Delphi application virtual form
  // - this form can be with no caption bar using SetNoCaption method
  TVistaForm = class(TForm)
  protected
    fNoCaption: TPanel;
    fNoCaptionLabel: TLabel;
    fMinimizeBtn: TSpeedButton;
    fMaximizeBtn: TSpeedButton;
    fCloseBtn: TSpeedButton;
    procedure NoCaptionMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnClick(Sender: TObject);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMSyscommand(var M: TMessage); message WM_SYSCOMMAND;
  public
    /// call this method to hide the Caption bar and replace it with a TPanel
    procedure SetNoCaption(aTopMostPanel: TPanel; aLabelLeft: integer);
    /// the TPanel instance replacing the Caption bar
    property NoCaptionPanel: TPanel read fNoCaption;
    /// the TLabel instance created on NoCaptionPanel to replace the Caption bar
    property NoCaptionLabel: TLabel read fNoCaptionLabel;
  end;

/// low level VCL routine in order to hide the application from Windows task bar
// - don't use it directly: it's called by TVistaForm.CreateParams()
procedure HideAppFormTaskBarButton;
{$endif}

/// draw a CheckBox in the Canvas Handle of the Wwindow hWnd, in the middle
// of the Rect coordinates
// - use theming under XP, Vista and Seven
procedure DrawCheckBox(hWnd: THandle; Handle: HDC; const Rect: TRect; Checked: boolean);

/// test if the ClearType is enabled for font display
// - ClearType is a software technology that improves the readability of text
// on liquid crystal display (LCD) monitors
function IsClearTypeEnabled: boolean;

/// enable the ClearType font display
// - under Windows 2000, standard font smoothing is forced, since Clear Type
// was introduced with XP
procedure ClearTypeEnable;

/// create an Icon
// - return the .lnk file name (i.e. Name+'.lnk')
function CreateAnIcon (const Name, Description, Path, Parameters,
  WorkingDir, IconFilename: TFileName; const IconIndex: Integer;
  const RunMinimized: Boolean = false): TFileName;

/// get the corresponding windows folder, from its ID
function GetShellFolderPath(const FolderID: Integer): string;

const
  CSIDL_PROGRAMS = $0002;
  CSIDL_DOCUMENTS = $0005;
  CSIDL_COMMON_PROGRAMS = $0017;
  CSIDL_COMMON_STARTMENU = $0016;
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019;

/// allow an application to access the network through the Windows firewall
// - works on Windows WP, Vista and Seven
// - caller process must have the administrator rights (this is the case
// for a setup program)
procedure AddApplicationToFirewall(const EntryName, ApplicationPathAndExe: string);

/// open a firewall port on the current computer
// - works on Windows XP, Vista and Seven
// - caller process must have the administrator rights (this is the case
// for a setup program)
procedure AddPortToFirewall(const EntryName: string; PortNumber: cardinal);


/// fill TStringGrid.Cells[] with the supplied data
// - will be slower than the TSQLTableToGrid method, but will work on
// a non standard TDrawGrid component
// - it will display date & time and enumerates as plain text, and handle
// the header properly (using the current SQLite3i18n language settings, if any)
// - the Client optional parameter will be used to display any RecordRef column
// - all data will be stored within the TStringGrid: you can safely release the
// Source data after having called this procedure  
procedure FillStringGrid(Source: TSQLTable; Dest: TStringGrid; Client: TSQLRest=nil);


implementation

uses
  ShellApi, ComObj, Activex, Shlobj, VarUtils;

procedure CreateShellLink (const Filename, Description, ShortcutTo, Parameters,
  WorkingDir, IconFilename: String; const IconIndex: Integer;
  const RunMinimized: Boolean);
{ Creates a lnk file named Filename, with a description of Description, which
  points to ShortcutTo. }
var
  Obj: IUnknown;
  SL: IShellLink;
  PF: IPersistFile;
  WideFilename: WideString;
begin
  Obj := CreateComObject(CLSID_ShellLink);
  SL := Obj as IShellLink;
  SL.SetPath(pointer(ShortcutTo));
  SL.SetArguments(pointer(Parameters));
  if WorkingDir <> '' then
    SL.SetWorkingDirectory(pointer(WorkingDir));
  if IconFilename <> '' then
    SL.SetIconLocation(pointer(IconFilename), IconIndex);
  SL.SetDescription(pointer(Description));
  PF := Obj as IPersistFile;
  WideFilename := Filename;
  PF.Save(PWideChar(WideFilename), True);
  { Delphi 3+ automatically releases COM objects when they go out of scope }
end;

function GetShellFolderPath(const FolderID: Integer): string;
var pidl: PItemIDList;
    Buffer: array[0..MAX_PATH-1] of Char;
    Malloc: IMalloc;
begin
  Result := '';
  if Win32MajorVersion<4 then Exit;
  if SUCCEEDED(SHGetSpecialFolderLocation(0, FolderID, pidl)) then begin
    if SHGetPathFromIDList(pidl, Buffer) then begin
      Result := Buffer;
      if Result[length(Result)]<>'\' then
        Result := Result+'\';
    end;
    if not FAILED(SHGetMalloc(Malloc)) then
      Malloc.Free(pidl);
  end;
end;

function CreateAnIcon (const Name, Description, Path, Parameters,
  WorkingDir, IconFilename: TFileName; const IconIndex: Integer;
  const RunMinimized: Boolean = false): TFileName;
var Dir: TFileName;
begin
  result := Name + '.lnk';
  Dir := ExtractFilePath(result);
  if Dir='' then begin
    if Win32Platform=VER_PLATFORM_WIN32_NT then
      Dir := GetShellFolderPath(CSIDL_COMMON_DESKTOPDIRECTORY) else
      Dir := GetShellFolderPath(CSIDL_DESKTOPDIRECTORY);
    if Dir[length(Dir)]='\' then
      result := Dir+result else
      result := Dir+'\'+result;
  end;
  if not DirectoryExists(Dir) then begin
    Dir := ExpandFilename(Dir);
    if not CreateDirectory(pointer(Dir), nil) then exit;
    SHChangeNotify(SHCNE_MKDIR, SHCNF_PATH, pointer(Dir), nil);
    SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
        pointer(ExtractFilePath(Dir)), nil);
  end;
  CreateShellLink(result, Description, Path, Parameters, WorkingDir,
      IconFilename, IconIndex, RunMinimized);
  SHChangeNotify(SHCNE_CREATE, SHCNF_PATH, pointer(result), nil);
  SHChangeNotify(SHCNE_UPDATEDIR, SHCNF_PATH or SHCNF_FLUSH,
      pointer(ExtractFilePath(result)), nil);
end;

const
  NET_FW_PROFILE_DOMAIN = 0;
  NET_FW_PROFILE_STANDARD = 1;
  NET_FW_PROFILE2_PRIVATE = 2;
  NET_FW_PROFILE2_PUBLIC  = 4;
  NET_FW_IP_VERSION_ANY = 2;
  NET_FW_IP_PROTOCOL_UDP = 17;
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_SCOPE_ALL = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;
  NET_FW_ACTION_ALLOW = 1;

function GetXPFirewall(var fwMgr, profile: OleVariant): boolean;
begin
  result := (Win32Platform=VER_PLATFORM_WIN32_NT) and
    (Win32MajorVersion>5) or ((Win32MajorVersion=5) and (Win32MinorVersion>0));
  if result then // need Windows XP at least
  try
    fwMgr := CreateOleObject('HNetCfg.FwMgr');
    profile := fwMgr.LocalPolicy.CurrentProfile;
  except
    on E: Exception do
      result := false;
  end;
end;

function GetVistaSevenFirewall(var fwMgr, rule: OleVariant; const Description: string): boolean;
begin
  result := (Win32Platform=VER_PLATFORM_WIN32_NT) and (Win32MajorVersion>5);
  if result then // need Windows Vista at least
  try
    fwMgr := CreateOleObject('HNetCfg.FwPolicy2');
    rule := CreateOleObject('HNetCfg.FWRule');
    rule.Name := Description;
    rule.Description := Description;
    rule.Protocol := NET_FW_IP_PROTOCOL_TCP;
    rule.Enabled := true;
    rule.Profiles := NET_FW_PROFILE2_PRIVATE OR NET_FW_PROFILE2_PUBLIC;
    rule.Action := NET_FW_ACTION_ALLOW;
  except
    on E: Exception do
      result := false;
  end;
end;

procedure AddApplicationToFirewall(const EntryName, ApplicationPathAndExe: string);
var fwMgr, profile, app, rule: OleVariant;
begin
  if Win32MajorVersion<6 then begin
    if GetXPFirewall(fwMgr,profile) and profile.FirewallEnabled then begin
      app := CreateOLEObject('HNetCfg.FwAuthorizedApplication');
      app.ProcessImageFileName := ApplicationPathAndExe;
      app.Name := EntryName;
      app.Scope := NET_FW_SCOPE_ALL;
      app.IpVersion := NET_FW_IP_VERSION_ANY;
      app.Enabled := true;
      profile.AuthorizedApplications.Add(app);
    end;
  end else
    if GetVistaSevenFirewall(fwMgr,rule,EntryName) then begin
      rule.ApplicationName := ApplicationPathAndExe;
      fwMgr.Rules.Add(rule);
    end;
end;

procedure AddPortToFirewall(const EntryName: string; PortNumber: cardinal);
var fwMgr, profile, port, rule: OleVariant;
begin
  if Win32MajorVersion<6 then begin
    if GetXPFirewall(fwMgr,profile) and profile.FirewallEnabled then begin
      port := CreateOLEObject('HNetCfg.FWOpenPort');
      port.Name := EntryName;
      port.Protocol := NET_FW_IP_PROTOCOL_TCP;
      port.Port := PortNumber;
      port.Scope := NET_FW_SCOPE_ALL;
      port.Enabled := true;
      profile.GloballyOpenPorts.Add(port);
    end;
  end else
    if GetVistaSevenFirewall(fwMgr,rule,EntryName) then begin
      rule.LocalPorts := PortNumber;
      fwMgr.Rules.Add(rule);
    end;
end;

const // for Delphi 6 compilation
  SPI_GETFONTSMOOTHINGTYPE = $200A;
  SPI_SETFONTSMOOTHINGTYPE = $200B;
  FE_FONTSMOOTHINGSTANDARD = $0001;
  FE_FONTSMOOTHINGCLEARTYPE = $0002;

function IsClearTypeEnabled: boolean;
// see http://blogs.msdn.com/michkap/archive/2008/03/01/7971061.aspx
var MType, SmoothFonts: DWORD;
begin
  SmoothFonts := 0;
  SystemParametersInfo(SPI_GETFONTSMOOTHING, 1, @SmoothFonts, 0);
  SystemParametersInfo(SPI_GETFONTSMOOTHINGTYPE, 0, @MType, 0);
  result := boolean(SmoothFonts) and (MType=FE_FONTSMOOTHINGCLEARTYPE);
end;

procedure ClearTypeEnable;
var MType, SmoothFonts: DWORD;
begin
  if (Win32MajorVersion<5) or IsClearTypeEnabled then
    exit; // no font smoothing before Win2K
  SystemParametersInfo(SPI_GETFONTSMOOTHING, 1, @SmoothFonts, 0);
  if not boolean(SmoothFonts) then
    SystemParametersInfo(SPI_SETFONTSMOOTHING, 1, nil, SPIF_UPDATEINIFILE or
      SPIF_SENDCHANGE);
  if (Win32MajorVersion=5) and (Win32MinorVersion=0) then
    MType := FE_FONTSMOOTHINGSTANDARD else // no Clear Type on Win2K
    MType := FE_FONTSMOOTHINGCLEARTYPE;
  SystemParametersInfo(SPI_SETFONTSMOOTHINGTYPE, 0, Pointer(MType),
    SPIF_UPDATEINIFILE or SPIF_SENDCHANGE);
end;

/// attempt to change the scroll bar size -> need Grids.pas rewrite -> buggy
procedure SetScrollVPage(Handle: HWND; aPage, aMax: Integer);
var ScrollInfo: TScrollInfo;
begin
  if Handle=0 then
    exit;
  with ScrollInfo do begin
    cbSize := SizeOf(ScrollInfo);
    fMask := SIF_ALL;
    GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    if nMin>=nMax then exit;
    if nMax<>127 then exit;
    fMask := SIF_DISABLENOSCROLL or SIF_PAGE or SIF_RANGE;
    if aMax<=0 then
      nPage := 1 else begin
      nPage := 30;
      inc(nMax,nPage);
    end;
  end;
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, true);
end;


{ THintWindowDelayed }

constructor THintWindowDelayed.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Color := $C0FFFF;
  fFontColor := clBlack;
{  if aOwner.InheritsFrom(TSQLTableToGrid) and  // done by ParentFont := true
    not TSQLTableToGrid(aOwner).NotDefined then
     Canvas.Font := TDrawGrid(TSQLTableToGrid(aOwner).Owner).Canvas.Font; }
end;

destructor THintWindowDelayed.Destroy;
begin
  Hide;
  inherited;
end;

procedure THintWindowDelayed.ShowDelayedUTF8(const Text: RawUTF8; X, Y, Time: integer;
  FontColor: TColor; AlignLeft: boolean=false);
var R: TRect;
begin
  if self=nil then
    exit;
  Hide;
  if Text='' then
    exit; // no text to show
  R := CalcHintRect(512, Text, nil);
  if not AlignLeft then
    dec(X,R.Right-R.Left); // align right
  inc(R.Left,X);
  inc(R.Right,X);
  inc(R.Top,Y);
  inc(R.Bottom,Y);
  ActivateHint(R,U2S(Text)); // perform Caption := Text
  fUTF8Text  := Text; // so it will work with Delphi 2009/2010
  if Time<>0 then begin
    if fTimerEnabled then
      KillTimer(Handle,1) else
      fTimerEnabled := true;
    SetTimer(Handle,1,Time,nil);
  end;
  fFontColor := FontColor;
  Show;
end;

procedure THintWindowDelayed.ShowDelayedUTF8(const Text: RawUTF8; Origin: TControl;
  X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean=false);
begin
  with Origin.ClientToScreen(Point(X,Y)) do
    ShowDelayedUTF8(Text,X,Y,Time,FontColor,AlignLeft);
end;

function THintWindowDelayed.CalcHintRect(MaxWidth: Integer;
  const AHint: RawUTF8; AData: Pointer): TRect;
var U: RawUnicode; // faster than a WideString
begin // unicode version
  Result := Rect(0, 0, MaxWidth, 0);
  U := Utf8DecodeToRawUnicode(AHint);
  DrawTextW(Canvas.Handle, pointer(U), length(U) shr 1, Result, DT_CALCRECT or DT_LEFT or
    DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 6);
  Inc(Result.Bottom, 2);
end;

procedure THintWindowDelayed.Paint;
var R: TRect;
    U: RawUnicode; // faster than a WideString
begin // unicode version
  R := ClientRect;
  Inc(R.Left, 2);
  Inc(R.Top, 2);
  Canvas.Font.Color := fFontColor;
  U := Utf8DecodeToRawUnicodeUI(fUTF8Text);
  DrawTextW(Canvas.Handle, pointer(U), -1, R, DT_LEFT or DT_NOPREFIX or
    DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);
end;

procedure THintWindowDelayed.VisibleChanging;
begin
  try
    if fTimerEnabled and Visible then begin // are we in a Hide process?
      KillTimer(Handle,1);
      fTimerEnabled := false;
      fRow := -1;
      if HandleAllocated then
        ReleaseHandle;
    end;
  finally
    inherited VisibleChanging;
  end;
end;

procedure THintWindowDelayed.WMTimer(var Msg: TWMTimer);
begin
  Hide;
  fRow := -1;
end;


procedure THintWindowDelayed.ShowDelayedString(const Text: string; X, Y,
  Time: integer; FontColor: TColor; AlignLeft: boolean=false);
begin
  ShowDelayedUTF8(S2U(Text),X,Y,Time,FontColor,AlignLeft);
end;

procedure THintWindowDelayed.ShowDelayedString(const Text: string;
  Origin: TControl; X, Y, Time: integer; FontColor: TColor; AlignLeft: boolean=false);
begin
  with Origin.ClientToScreen(Point(X,Y)) do
    ShowDelayedUTF8(S2U(Text),X,Y,Time,FontColor,AlignLeft);
end;


{ TSQLTableToGrid }

resourcestring
  sErrorTSQLTableToGridNoData = '%s didn''t receive any data for %s';

constructor TSQLTableToGrid.Create(aOwner: TDrawGrid; aTable: TSQLTable;
  aClient: TSQLRestClientURI);
begin
  if aTable=nil then
    raise Exception.CreateFmt(sErrorTSQLTableToGridNoData,[ClassName,aOwner.Name]);
  From(aOwner).Free; // any old association will be overriden by this instance
  inherited Create(aOwner);
  fTable := aTable;
  fClient := aClient;
  fHint := THintWindowDelayed.Create(self);
  aOwner.RowCount := 2; // first reset row count, to avoid flicking
  aOwner.FixedRows := 1;
  with aOwner.Canvas.TextExtent('jQH°;') do
    aOwner.DefaultRowHeight := cy+4;
  aOwner.Options := [goFixedHorzLine,goFixedVertLine,goVertLine,
    goHorzLine,goColSizing,goRowSelect,goThumbTracking]; // no goRangeSelect
  aOwner.FixedCols := 0;
  aOwner.ColCount := Table.FieldCount;
  SetLength(fFieldOrder,Table.FieldCount);   // auto filled to false
  fCurrentFieldOrder := Table.FieldIndex(
    pointer(SQLGetOrder(Table.QuerySQL)));   // get 'ORDER BY' field index
  if fCurrentFieldOrder>=0 then
    fFieldOrder[fCurrentFieldOrder] := true; // mark 'ORDER BY' field ascending
  fFieldIndexTimeLogForMark := -2; // so GetFieldIndexTimeLogForMark will get it
  aOwner.OnDrawCell := DrawCell;
  aOwner.OnMouseMove := DrawGridMouseMove;
  aOwner.OnMouseDown := DrawGridMouseDown;
  aOwner.OnMouseUp := DrawGridMouseUp;
  aOwner.OnSelectCell := DrawGridSelectCell;
  aOwner.OnKeyPress := DrawGridKeyPress;
  aOwner.OnKeyDown := DrawGridKeyDown;
  if Table.RowCount>0 then
    aOwner.RowCount := Table.RowCount+1;
end;

destructor TSQLTableToGrid.Destroy;
begin
  if (Owner<>nil) and Owner.InheritsFrom(TDrawGrid) then
  with TDrawGrid(Owner) do begin // reset the Grid overriden events to avoid GPF
    OnDrawCell := nil;
    OnMouseMove := nil;
    OnMouseDown := nil;
    OnMouseUp := nil;
    OnSelectCell := nil;
    OnKeyPress := nil;
    OnKeyDown := nil;
  end;
  FreeAndNil(fTable);
  inherited;
end;

class function TSQLTableToGrid.From(Grid: TDrawGrid): TSQLTableToGrid;
var i: integer;
begin
  if Grid<>nil then
  for i := 0 to Grid.ComponentCount-1 do begin
    result := pointer(Grid.Components[i]);
    if result.InheritsFrom(TSQLTableToGrid) and (result.Owner=Grid) then
      exit;
  end;
  result := nil;
end;

const
  CheckBoxWidth = 13;

{$WARN SYMBOL_DEPRECATED OFF} // for ThemeServices

procedure DrawCheckBox(hWnd: THandle; Handle: HDC; const Rect: TRect; Checked: boolean);
const
{$ifdef WITHUXTHEME}
  XPState: array[boolean] of TThemedButton = (
    tbCheckBoxUncheckedNormal, tbCheckBoxCheckedNormal);
{$endif}
  Win32State: array[boolean] of cardinal = (
    DFCS_BUTTONCHECK, DFCS_BUTTONCHECK or DFCS_CHECKED);
var DrawRect: TRect;
begin
  DrawRect.Left := Rect.Left+(Rect.Right-Rect.Left-CheckBoxWidth)shr 1;
  DrawRect.Top:= Rect.Top+2;
  DrawRect.Right := DrawRect.Left + CheckBoxWidth;
  DrawRect.Bottom := DrawRect.Top + CheckBoxWidth;
{$ifdef WITHUXTHEME}
  if ThemeServices.ThemesEnabled then begin // Windows XP and later: use theming
    ThemeServices.DrawElement(handle,
      ThemeServices.GetElementDetails(XPState[Checked]), DrawRect);
  end else
{$endif}
    DrawFrameControl(Handle,DrawRect,DFC_BUTTON,Win32State[Checked]);
end;

procedure TSQLTableToGrid.DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var Options, x,y, L, i, XInc: integer;
    StringValue: string; // generic string type, VCL ready
    Points: array[0..2] of TPoint;
    Centered, WithMark: boolean;
    tmp: array[0..255] of WideChar; // 255 chars is wide enough inside a cell
begin
  // default cell draw
  if NotDefined then
    exit;
  if Assigned(OnDrawCellBackground) then
    OnDrawCellBackground(Owner,ACol,ARow,Rect,State);
  if (cardinal(ARow)>cardinal(Table.RowCount)) or
    (cardinal(ACol)>=cardinal(Table.FieldCount)) then // avoid any possible GPF
    exit;
  with TDrawGrid(Owner).Canvas do begin
    Options := ETO_CLIPPED or TextFlags;
    if Brush.Style <> bsClear then
      Options := Options or ETO_OPAQUE;
    WithMark := fMarkAllowed and (ACol=0);
    if ARow=0 then begin
      // 1. 1st row = field name: bold + centered translated text, with sort indicator
      if not Assigned(OnValueText) or
         not OnValueText(Table,ACol,0,StringValue) then
        StringValue := Table.GetCaption(0,ACol); // auto translated
      if not Assigned(OnDrawCellBackground) then
        Font.Style := [fsBold];
      L := Rect.Right-Rect.Left;
      if WithMark then
        dec(L,CheckBoxWidth+4);
      Centered := TextWidth(StringValue)<L;
      if Centered then begin
        UnSetBit64(fFieldNameTruncated,ACol);
        XInc := L shr 1;
        SetTextAlign(Handle,TA_CENTER);
      end else begin
        SetBit64(fFieldNameTruncated,ACol);
        XInc := 2;
      end;
      if WithMark then
        inc(XInc,CheckBoxWidth+4);
      ExtTextOut(Handle, Rect.Left+XInc,Rect.Top+2, Options, @Rect, pointer(StringValue),
        length(StringValue), nil); // direct translated text centered draw
      if Centered then
        SetTextAlign(Handle,TA_LEFT);
      Font.Style := [];
      if fCurrentFieldOrder=ACol then begin
        // sorted field: draw sort indicator
        x := Rect.Right-8;
        if fFieldOrder[ACol] then begin
          Points[0].X := x+5; // ascending order arrow
          Points[1].X := x;
          Points[2].X := x-5;
        end else begin
          Points[0].X := x-5; // descending order arrow
          Points[1].X := x+5;
          Points[2].X := x;
        end;
        y := Rect.Bottom-9;
        if fFieldOrder[ACol] then begin
          Points[0].Y := y-5; // ascending order arrow
          Points[1].Y := y+5;
          Points[2].Y := y-5
        end else begin
          Points[0].Y := y+5; // descending order arrow
          Points[1].Y := y+5;
          Points[2].Y := y-5
        end;
        Brush.Color := clWhite; // fill the arrow content
        Polygon(Points);
        Pen.Color := clLtGray;  // draw the arrow border
        MoveTo(Points[0].X,Points[0].Y);
        LineTo(Points[1].X,Points[1].Y);
        Pen.Color := clGray;
        LineTo(Points[2].X,Points[2].Y);
        LineTo(Points[0].X,Points[0].Y);
      end;
    end else begin
      // 2. field value rows
      Centered := GetBit64(fCentered,ACol);
      if Centered then begin
        SetTextAlign(Handle,TA_CENTER);
        L := Rect.Right-Rect.Left;
        if WithMark then
          dec(L,CheckBoxWidth+4);
        XInc := L shr 1;
      end else
        XInc := 4;
      if WithMark then
        inc(XInc,CheckBoxWidth+4);
      if Assigned(OnValueText) and OnValueText(Table,ACol,ARow,StringValue) then
        ExtTextOut(Handle, Rect.Left+XInc, Rect.Top+2, Options, @Rect, pointer(StringValue),
          length(StringValue), nil) else // translated text
      case Table.ExpandAsString(ARow,ACol,Client,StringValue) of
      // very fast response (calculated once)
      sftBoolean:
        // display boolean as checkbox
        DrawCheckBox(TDrawGrid(Owner).Handle, Handle, Rect,
          PWord(Table.Get(ARow,ACol))^<>ord('0')); // fast StrComp(,'0')
      sftEnumerate, sftTimeLog, sftRecord, sftDateTime:
        ExtTextOut(Handle, Rect.Left+XInc, Rect.Top+2, Options, @Rect, pointer(StringValue),
          length(StringValue), nil); // translated text
      //sftID: { TODO : display ID as TSQLRecord content? better calculate it in SELECT }
      else begin
        // normal field value: unicode text (even with Delphi 2-2007 VCL), left aligned
        L := Table.GetWP(ARow,ACol,tmp,high(tmp));
        for i := 0 to L-1 do // replace #13,#10 chars in the grid with spaces
          if tmp[i]<' ' then
            tmp[i] := ' ';
        // direct unicode text draw
        ExtTextOutW(Handle, Rect.Left+XInc, Rect.Top+2, Options, @Rect, tmp, L, nil);
      end;
      end;
      if Centered then
        SetTextAlign(Handle,TA_LEFT);
    end;
    if WithMark then begin // draw left side checkbox with Marked[] value
      inc(Rect.Left,2);
      Rect.Right := Rect.Left+CheckBoxWidth;
      DrawCheckBox(TDrawGrid(Owner).Handle, Handle,Rect,Marked[ARow]);
    end;
  end;
end;

procedure TSQLTableToGrid.SortForce(ACol: integer; Ascending: boolean;
  ARow: integer=-1);
var
    MIDs: TIntegerDynArray;
begin
  if NotDefined or (ACol>=Table.FieldCount) then // we allow ACol<0 (see below)
    exit;
  if ARow<0 then
    ARow := TDrawGrid(Owner).Row; // keep current row selected if none specified
  if ACol<0 then begin
    // if ACol=-1, then the Marked[] rows are shown first, in current sort
    if fMarked=nil then
      exit; // no Marked[] rows to put in first place
    ARow := Table.IDColumnHiddenValue(ARow); // current row
    Table.SortBitsFirst(fMarked[0]);
    TDrawGrid(Owner).Row := Table.RowFromID(ARow);
    // no PageChanged here
  end else begin
    if fMarked<>nil then
      Table.IDArrayFromBits(fMarked[0],MIDs); // save current marked entries
    fFieldOrder[ACol] := Ascending;
    fCurrentFieldOrder := ACol;
    Table.SortFields(ACol,fFieldOrder[ACol],@ARow);
    if MIDs<>nil then
      Table.IDArrayToBits(fMarked[0],MIDs); // restore marked entries
    TDrawGrid(Owner).Row := ARow; // reselect row after sort (+ Invalidate)
    PageChanged; // hide any pending popup Hint e.g.
  end;
end;

procedure TSQLTableToGrid.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol, ARow: integer;
    Hint: string; // generic string type, for VCL
begin
  if NotDefined then // avoid any possible GPF
    exit;
  fMouseDownMarkedValue := markNone;
  TDrawGrid(Owner).MouseToCell(X, Y, ACol, ARow);
  if cardinal(ACol)<cardinal(Table.FieldCount) then
  if ARow=0 then
  if (ssCtrl in Shift) or (Button<>mbLeft) then begin
    // Ctrl or right button pressed -> display first row as hint
    ShowHintString(U2S(Table.Get(ARow,ACol)),ACol,ARow,4000);
  end else begin
    // first row -> sort fields
    if fMarkAllowed and (X<CheckBoxWidth+4) then
      // sort Marked[] first
      SortForce(-1,true) else
    if fCurrentFieldOrder=ACol then
      // same column -> toggle sorting order
      SortForce(ACol,not fFieldOrder[ACol]) else
      // column changed -> sort ascending first
      SortForce(ACol,true);
  end else
    // not first row: data 
    if (Button=mbRight) and (ssRight in Shift) and Assigned(OnRightClickCell) then
      OnRightClickCell(Table,ACol,ARow,X,Y) else
    if (ssCtrl in Shift) or (Button<>mbLeft) then begin
      if not Assigned(OnHintText) or
        not OnHintText(Table,ACol,ARow,Hint) then
        Table.ExpandAsString(ARow,ACol,Client,Hint);
//    Hint := IntToStr(SelectedID);
      ShowHintString(Hint,ACol,ARow,4000);
    end else
    if (Button=mbLeft) and (ACol=0) and fMarkAllowed and (X<CheckBoxWidth+4) then begin
      if Marked[ARow] then // on click: invert current Marked[] checkbox state
        fMouseDownMarkedValue := markOff else
        fMouseDownMarkedValue := markOn;
      Marked[ARow] := (fMouseDownMarkedValue=markOn);
    end;
  TDrawGrid(Owner).Invalidate;
end;

resourcestring
  sPutMarkedRowFirst = 'Sort marked rows first';

procedure TSQLTableToGrid.DrawGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var ACol, ARow: integer;
begin
  if NotDefined then // avoid any possible GPF
    exit;
  TDrawGrid(Owner).MouseToCell(X, Y, ACol, ARow);
  if cardinal(ACol)>=cardinal(Table.FieldCount) then
    exit;
  if ARow=0 then begin
    // over the checkbox left of the first row: show appropriate hint
    if (ACol=0) and fMarkAllowed and (fMarked<>nil) and (X<CheckBoxWidth+4) and
      ((Hint=nil) or (Hint.Col<>-1) or (Hint.Row<>0)) then begin
        ShowHintString(sPutMarkedRowFirst,0,0,1000);
        Hint.fCol := -1; // column = -1 for checkbox
     end else
    // over the first row, i.e. column name: show hint if name was truncated
    if (not FieldTitleTruncatedNotShownAsHint) and GetBit64(fFieldNameTruncated,ACol) and
       ((Hint=nil) or (Hint.Col<>ACol) or (Hint.Row<>0)) then
      ShowHintString(Table.GetCaption(0,ACol),ACol,0,1000);
  end else
    // select/unselect checkbox left of data rows
    if (ACol=0) and fMarkAllowed and (fMouseDownMarkedValue<>markNone) and
       (X<=CheckBoxWidth+4) then
      Marked[ARow] := (fMouseDownMarkedValue=markOn);
end;

procedure TSQLTableToGrid.DrawGridMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fMouseDownMarkedValue := markNone; // reset Marked[] checkbox state
end;

procedure TSQLTableToGrid.DrawGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if NotDefined then // avoid any possible GPF
    exit;
  if not fIncrementalSearchMove then
    fIncrementalSearch := ''; // reset incremental key lookup
  if Assigned(OnSelectCell) and not fOnSelectCellProcessing then
  try
    fOnSelectCellProcessing := true; // avoid endless loop or GPF
    OnSelectCell(Sender,ACol,ARow,CanSelect);
  finally
    fOnSelectCellProcessing := false;
  end;
end;

procedure TSQLTableToGrid.Resize(Sender: TObject);
var i: integer;
    width,tot: cardinal;
begin
  if NotDefined then // avoid any possible GPF
    exit;
  width := TDrawGrid(Owner).ClientWidth-GetSystemMetrics(SM_CXBORDER)*4;
  if (width=fLastWidth) then
    exit; // draw if necessary
  fLastWidth := width;
  tot := Table.FieldLengthMeanSum;
  for i := 0 to Table.FieldCount-1 do
    TDrawGrid(Owner).ColWidths[i] := (width*Table.FieldLengthMean(i))div tot;
//  with TDrawGrid(Owner) do SetScrollVPage(Handle,ClientHeight div DefaultRowHeight,RowCount-FixedRows);
end;


function TSQLTableToGrid.NotDefined: boolean;
begin
  result := (self=nil) or (Owner=nil) or (Table=nil) or
    not Owner.InheritsFrom(TDrawGrid);
end;

procedure TSQLTableToGrid.DrawGridKeyPress(Sender: TObject; var Key: Char);
var F,R: integer;
begin // incremental key lookup
  if NotDefined then // avoid any possible GPF
    exit;
  if Key=#27 then       // ESC key reset the lookup string
    fIncrementalSearch := '' else
  if Key=#8 then begin  // BACKDEL key delete last lookup char
    if fIncrementalSearch<>'' then
      SetLength(fIncrementalSearch,length(fIncrementalSearch)-1);
  end else
  if Key>=' ' then
    if (Key=' ') and (fIncrementalSearch='') then begin
      // space with no lookup key -> allow mark/unmark current one
      R := TDrawGrid(Owner).Row;
      if fMarkAllowed and (R>0) then begin
        Marked[R] := not Marked[R];
        inc(R);
        if R<=Table.RowCount then
          TDrawGrid(Owner).Row := R else // and go to next row
          TDrawGrid(Owner).Invalidate;
      end;
      exit;
    end else
      fIncrementalSearch := fIncrementalSearch+RawUTF8(NormToUpper[AnsiChar(Key)]);
  if fIncrementalSearch='' then begin
    if fHint<>nil then
      fHint.Hide;
    exit; // nothing to search
  end;
  // search from the next row
  F := fCurrentFieldOrder;
  R := Table.SearchValue(fIncrementalSearch,TDrawGrid(Owner).Row+1,fCurrentFieldOrder,Client);
  if R=0 then begin // not found: search from the beginning
    R := Table.SearchValue(fIncrementalSearch,1,fCurrentFieldOrder,Client);
    if R=0 then begin // not found in this field: search in all fields
      R := Table.SearchValue(fIncrementalSearch,TDrawGrid(Owner).Row+1,@F,Client);
      if R=0 then // not found: search from the beginning
        R := Table.SearchValue(fIncrementalSearch,1,@F,Client);
    end;
  end;
  if R>0 then begin
    fIncrementalSearchMove := true; // DrawGridSelectCell() won't reset fIncremental
    TDrawGrid(Owner).Row := R;
    fIncrementalSearchMove := false;
    ShowHintString(U2S(fIncrementalSearch),F,R,2000,clNavy);
  end else // not found: display searched string in red
    ShowHintString(U2S(fIncrementalSearch)+'?',fCurrentFieldOrder,
      TDrawGrid(Owner).Row,2000,clRed);
end;

procedure TSQLTableToGrid.ShowHintString(const Text: string; ACol, ARow, Time: integer;
  FontColor: TColor=clBlack);
begin
  if NotDefined then // avoid any possible GPF
    exit;
  if Text='' then begin
    if fHint<>nil then
      fHint.Hide;
    exit;
  end;
  fHint.fCol := ACol;
  fHint.fRow := ARow;
  with TDrawGrid(Owner).CellRect(ACol,ARow) do
    fHint.ShowDelayedString(Text,TDrawGrid(Owner),Right,Top+2,Time,FontColor);
end;

procedure TSQLTableToGrid.DrawGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var F: integer;
begin
  if NotDefined or (Shift<>[]) then // avoid any possible GPF
    exit;
  case Key of
  VK_LEFT: // LEFT ARROW key sort previous column
    if fCurrentFieldOrder>0 then
      F := fCurrentFieldOrder-1 else
      F := Table.FieldCount-1;
  VK_RIGHT: // RIGHT ARROW key sort next column
    if fCurrentFieldOrder>=Table.FieldCount-1 then
      F := 0 else
      F := fCurrentFieldOrder+1;
  else exit;
  end;
  SortChange(F);
  Key := 0; // we proceed this key -> caller will ignore it
end;

procedure TSQLTableToGrid.SortChange(ACol: integer);
begin
  if not NotDefined and (cardinal(ACol)<cardinal(Table.FieldCount)) then
    if fCurrentFieldOrder=ACol then
      // same column -> toggle sorting order
      SortForce(ACol,not fFieldOrder[ACol]) else
      // column changed -> sort ascending first
      SortForce(ACol,true);
end;

procedure TSQLTableToGrid.IDColumnHide;
begin
  if NotDefined or not Table.IDColumnHide then
    exit;
  TDrawGrid(Owner).ColCount := Table.FieldCount; // we loose one column
  fCurrentFieldOrder := -1; // force no previous column -> always ascending order
  SortChange(0);
end;

function TSQLTableToGrid.SelectedID: integer;
begin
  if NotDefined then
    result := 0 else
    result := Table.IDColumnHiddenValue(TDrawGrid(Owner).Row);
end;

function TSQLTableToGrid.SelectedRecordCreate: TSQLRecord;
var aID: integer;
    RecordType: TSQLRecordClass;
begin
  aID := SelectedID;
  if (aID<=0) or (fClient=nil) then
    result := nil else begin
    RecordType := TSQLRecordClass(Table.QueryRecordType);
    if (RecordType=nil) or not RecordType.InheritsFrom(TSQLRecord) then
      result := nil else
      result := RecordType.Create(fClient,aID);
  end;
end;

procedure TSQLTableToGrid.SetCentered(const Cols: array of cardinal);
var i: integer;
begin
  fCentered := 0;
  if Table<>nil then
  for i := 0 to high(Cols) do
    if Cols[i]<cardinal(Table.FieldCount) then
      SetBit64(fCentered,Cols[i]-1);
end;

procedure TSQLTableToGrid.SetCentered(aCol: cardinal);
begin
  if self<>nil then
    if aCol<cardinal(Table.FieldCount) then
      SetBit64(fCentered,aCol);
end;

procedure TSQLTableToGrid.PageChanged;
begin
  if (Self<>nil) and (Hint<>nil) then
    Hint.Hide;
end;

function TSQLTableToGrid.Refresh(ForceRefresh: Boolean=false): boolean;
var Refreshed: boolean;
    aID: integer;
begin
  if self=nil then
    result := false else begin
    aID := Table.IDColumnHiddenValue(TDrawGrid(Owner).Row);
    if ForceRefresh then
      result := true else
      result := Client.UpdateFromServer([Table],Refreshed) and Refreshed;
    if result then
      AfterRefresh(aID);
  end;
end;

procedure TSQLTableToGrid.AfterRefresh(aID: integer);
var CurrentRow: integer;
    Bulk: boolean;
begin
  with TDrawGrid(Owner) do begin
    if Table.RowCount=0 then
      RowCount := 2 else
      RowCount := Table.RowCount+1;
    if Table.FieldCount<>ColCount then begin
      // we get results from a void table for the first time
      ColCount := Table.FieldCount;
      SetLength(fFieldOrder,Table.FieldCount);
    end;
    CurrentRow := Table.RowFromID(aID);
    if CurrentRow=0 then
      CurrentRow := 1;
    Row := CurrentRow;
    TopRow := 1;
    Invalidate;
  end;
  Resize(nil); // auto resize columns
  if Assigned(OnSelectCell) then
     OnSelectCell(Owner,0,CurrentRow,Bulk); // refresh details
end;

procedure TSQLTableToGrid.SetFieldLengthMean(const Lengths: RawUTF8; aMarkAllowed: boolean);
var L, i: integer;
    c: AnsiChar;
    Means: array of cardinal;
begin
  if self=nil then Exit;
  fMarkAllowed := aMarkAllowed;
  L := length(Lengths);
  if L=0 then begin
    SetLength(Means,Table.FieldCount);
    for i := 0 to Table.FieldCount-1 do
      Means[i] := 10; // some fixed width
  end else
  if Table.FieldCount=L then begin
    SetLength(Means,L);
    for i := 0 to L-1 do begin
      c := Lengths[i+1];
      if c in ['a'..'z'] then begin
        SetCentered(i);
        dec(c,32);
      end;
      Means[i] := ord(c)+(-ord('A')+1);
    end;
    Table.SetFieldLengthMean(Means);
  end;
  if aMarkAllowed then
    Table.FieldLengthMeanIncrease(0,2); // space for Marked[] checkbox e.g.
end;

procedure TSQLTableToGrid.SetFieldFixedWidth(aColumnWidth: integer);
var i: integer;
begin
  with TDrawGrid(Owner) do
    for i := 0 to ColCount-1 do
      ColWidths[i] := aColumnWidth;
end;

function TSQLTableToGrid.GetMarked(RowIndex: integer): boolean;
begin
  dec(RowIndex);
  if (self=nil) or (fMarked=nil) or
     (cardinal(RowIndex)>=cardinal(length(fMarked)shl 3)) then
    result := false else
    result := GetBit(fMarked[0],RowIndex);
end;

procedure TSQLTableToGrid.SetMarked(RowIndex: integer;
  const Value: boolean);
var n: integer;
begin
  dec(RowIndex);
  if (self=nil) or (cardinal(RowIndex)>=cardinal(Table.RowCount)) then
    exit;
  n := (Table.RowCount shr 3)+1;
  if length(fMarked)<n then // need to allocate/expand fMarked[] array?
    SetLength(fMarked,n);   // initializes all expanded bytes to 0
  if Value then
    SetBit(fMarked[0],RowIndex) else
    UnSetBit(fMarked[0],RowIndex)
end;

procedure TSQLTableToGrid.SetMark(aAction: TSQLAction);
var i: integer;
    V, Time: Int64;
const
  DIFFTIME: array[actMarkOlderThanOneDay..actMarkOlderThanOneYear] of double =
    (1,7,31,183,365); // 183 = more or less half a year
begin
  if NotDefined then
    exit;
  with TDrawGrid(Owner) do
  case aAction of
    actMarkAllEntries:
      for i := 1 to RowCount do
        Marked[i] := true;
    actUnMarkAll:
      if fMarked<>nil then
        Finalize(fMarked);
    actmarkInverse:
      for i := 1 to RowCount do
        Marked[i] := not Marked[i];
    actMarkOlderThanOneDay..actMarkOlderThanOneYear:
    if FieldIndexTimeLogForMark>=0 then begin
      // use TDateTime calculation because TTimeLog is not duration compatible
      Iso8601(Time).From(Now-DIFFTIME[aAction],true);
      for i := 1 to RowCount do begin
        SetInt64(Table.Get(i,fFieldIndexTimeLogForMark),V);
        if (V>0) and (V<=Time) then
          Marked[i] := true;
      end;
    end;
  else exit;
  end;
  TDrawGrid(Owner).Invalidate; // refresh screen
end;

function TSQLTableToGrid.GetMarkAvailable: boolean;
var i: integer;
begin
  result := fMarkAllowed and (fMarked<>nil);
  if not result then
    exit;
  for i := 0 to Table.RowCount-1 do // very any bit is realy set
    if GetBit(fMarked[0],i) then
      exit;
  result := false;
end;

function TSQLTableToGrid.GetMarkedIsOnlyCurrrent: boolean;
begin
  with TDrawGrid(Owner) do
    result := fMarkAllowed and (fMarked<>nil) and
      (GetBitsCount(fMarked[0],RowCount)=1) and Marked[Row];
end;

function TSQLTableToGrid.GetMarkedTotalCount: integer;
begin
  with TDrawGrid(Owner) do 
    if not fMarkAllowed or (fMarked=nil) then
      result := 0 else
      result := GetBitsCount(fMarked[0],RowCount);
end;

function TSQLTableToGrid.ExpandRowAsString(Row: integer; Client: TObject): string;
var F, i: integer;
    Text: string; // generic VCL-ready string 
begin
  result := '';
  if (self=nil) or (cardinal(Row)>cardinal(Table.RowCount)) or (Table.FieldCount<=0) then
    exit;
  for F := 0 to Table.FieldCount-1 do begin
    if (not Assigned(OnValueText)) or
       (not OnValueText(Table,F,Row,text)) then
      Table.ExpandAsString(Row,F,Client,Text);
    i := pos(#13,Text); // trim multi-line text to first line
    if i>0 then
      SetLength(Text,i-1);
    if (F>0) and (text<>'') then
      text := ' '+text;
    result := result+text;
  end;
end;

procedure TSQLTableToGrid.OnTableUpdate(State: TOnTableUpdateState);
begin
  if (self=nil) or (fMarked=nil) then
    exit; // wrong parameters 
  case State of
    tusPrepare:
      // save current marked entries
      if fMarked<>nil then begin
        Table.IDArrayFromBits(fMarked[0],fOnTableUpdateID);
        exit; // don't Finalize(fOnTableUpdateID)
      end;
    tusChanged:
      // restore marked entries
      if fOnTableUpdateID<>nil then
        Table.IDArrayToBits(fMarked[0],fOnTableUpdateID);
  end;
  // tusNoChange or tusChanged: release IDs memory
  if fOnTableUpdateID<>nil then
    Finalize(fOnTableUpdateID);
end;

function TSQLTableToGrid.GetMarkedBits: pointer;
begin
  result := fMarked;
end;

function TSQLTableToGrid.GetDrawGrid: TDrawGrid;
begin
  if self=nil then
    result := nil else
    result := TDrawGrid(Owner);
end;

function TSQLTableToGrid.GetFieldIndexTimeLogForMark: integer;
var F: integer;
begin
  if fFieldIndexTimeLogForMark=-2 then begin
    fFieldIndexTimeLogForMark := -1;
    for F := 0 to Table.FieldCount-1 do
      if Table.FieldType(F,nil)=sftTimeLog then begin
        fFieldIndexTimeLogForMark := F;
        break;
      end;
  end;
  result := fFieldIndexTimeLogForMark;
end;


{ TVistaForm }

{$ifdef VISTAFORM}

var
  AppFormTaskBarButtonHidden: boolean = false;

procedure HideAppFormTaskBarButton;
{$ifdef ISDELPHI2007ANDUP}
begin
end;
{$else}
var ExtendedStyle: Integer;
begin
  if AppFormTaskBarButtonHidden then
    exit;
  AppFormTaskBarButtonHidden := true;
  ShowWindow(Application.Handle, SW_HIDE);
  ExtendedStyle := GetWindowLong(Application.Handle, GWL_EXSTYLE);
  SetWindowLong(Application.Handle, GWL_EXSTYLE,
      ExtendedStyle and not WS_EX_APPWINDOW or WS_EX_TOOLWINDOW);
  ShowWindow(Application.Handle, SW_SHOW);
  Application.DialogHandle := Application.Handle;
end;
{$endif}

procedure TVistaForm.BtnClick(Sender: TObject);
begin
  if Sender=fCloseBtn then
    Close else
  if Sender=fMinimizeBtn then
    WindowState := wsMinimized else
  if (Sender=fMaximizeBtn) or Sender.InheritsFrom(TLabel) then
    if WindowState=wsMaximized then
      WindowState := wsNormal else
      WindowState := wsMaximized;
end;

procedure TVistaForm.CreateParams(var Params: TCreateParams);
begin
  HideAppFormTaskBarButton; // check if not already hidden
  inherited;
  {$ifndef ISDELPHI2007ANDUP}
  Params.ExStyle := Params.ExStyle and not WS_EX_TOOLWINDOW or
      WS_EX_APPWINDOW;  // this form will appear in the TaskBar
  {$endif}
  if fNoCaption<>nil then begin
    Params.ExStyle := Params.ExStyle or WS_EX_STATICEDGE;
    Params.Style := Params.Style or WS_SIZEBOX;
  end;
end;

procedure TVistaForm.NoCaptionMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  SC_DRAGMOVE = $F012;
begin
  if Button = mbLeft then
  begin
    ReleaseCapture;
    Perform(WM_SYSCOMMAND, SC_DRAGMOVE, 0);
  end;
end;

procedure TVistaForm.SetNoCaption(aTopMostPanel: TPanel; aLabelLeft: integer);
const BUT_SIZE = 16;
procedure SetEvent(Event: TMouseEvent);
var i: integer;
begin
  if fNoCaption<>nil then begin
    fNoCaption.OnMouseDown := Event;
    for i := 0 to fNoCaption.ComponentCount-1 do
      if fNoCaption.Components[i].InheritsFrom(TLabel) then
        with TLabel(fNoCaption.Components[i]) do
          if not Assigned(OnClick) then
            OnMouseDown := Event;
  end;
end;
var R: integer;
function Btn(const ResName: string): TSpeedButton;
begin 
  dec(R,BUT_SIZE+1);
  result := TSpeedButton.Create(aTopMostPanel);
  result.Parent := aTopMostPanel;
  result.SetBounds(R,2,BUT_SIZE+1,BUT_SIZE);
  result.Anchors := [akRight, akTop];
  result.Glyph.LoadFromResourceName(HInstance,ResName);
  result.OnClick := BtnClick;
  result.Flat := true;
end;
begin
  if aTopMostPanel=fNoCaption then
    exit;
  SetEvent(nil);
  fNoCaption := aTopMostPanel;
  if aTopMostPanel<>nil then begin
    fNoCaptionLabel := TLabel.Create(aTopMostPanel);
    R := aTopMostPanel.ClientWidth-4;
    fCloseBtn := Btn('ButClose');
    fMaximizeBtn := Btn('ButMax');
    fMinimizeBtn := Btn('ButMin');
    with fNoCaptionLabel do begin
      Parent := aTopMostPanel;
      Transparent := true;
      AutoSize := false;
      SetBounds(aLabelLeft,2,R-aLabelLeft,20);
      Anchors := [akLeft, akRight, akBottom, akTop];
      Alignment := Classes.taCenter;
      OnDblClick := BtnClick;
    end;
    SetEvent(NoCaptionMouseDown);
    BorderStyle :=  bsNone;
    {$ifdef ISDELPHI2007ANDUP}
    RecreateWnd;
    {$endif}
  end else
    BorderStyle := bsSizeable;
end;

procedure TVistaForm.WMSyscommand(var M: TMessage);
begin
  {$ifdef ISDELPHI2007ANDUP}
  inherited;
  {$else}
  case (M.WParam and $FFF0) of
    SC_MINIMIZE, SC_RESTORE, SC_MAXIMIZE: begin
       M.Result := DefWindowProc(Handle, M.Msg, M.WParam, M.LParam);
       ShowWindow(Application.Handle, SW_HIDE);
     end;
    else
      inherited;
  end;
  {$endif}
end;

{$endif}



{ TSynLabeledEdit }

constructor TSynLabeledEdit.Create(AOwner: TComponent);
begin
  inherited;
  ShowHint := True;
  MaxValue := 100;
  MinValue := 1;
  Text := '';
end;

function TSynLabeledEdit.GetValue: Variant;
var Txt: string;
begin
  Txt := trim(Text);
  if Txt='' then
    if RangeChecking then begin
      result := MinValue;
      Text := MinValue;
    end else
      VarClear(result) else begin
    if not IsValid(Txt,result) then begin
      if RaiseExceptionOnError then
        raise ESynLabeledEdit.CreateFmt(SErrorFieldNotValid, [EditLabel.Caption,
          GetEnumCaption(TypeInfo(TSynLabeledEditKind),Kind)]);
      if RangeChecking then
        result := MinValue else
        VarClear(result);
    end;
  end;
  if RangeChecking and (Result<MinValue) then begin
    Text := MinValue;
    if RaiseExceptionOnError then
      raise ESynLabeledEdit.CreateFmt(SErrorFieldTooSmall,
        [EditLabel.Caption,string(MinValue)]);
  end;
  if RangeChecking and (Result>MaxValue) then begin
    Text := MaxValue;
    if RaiseExceptionOnError then
      raise ESynLabeledEdit.CreateFmt(SErrorFieldTooLarge,
        [EditLabel.Caption,string(MaxValue)]);
  end;
end;

function TSynLabeledEdit.IsValid(const Txt: string; var ToValue: Variant): Boolean;
var err: integer;
    resInt32: integer;
    resInt64: Int64;
    resDouble: Double;
begin
  result := false;
  case Kind of
    sleInteger: begin
      val(Txt,resInt32,err);
      if err<>0 then exit;
      ToValue := resInt32;
    end;
    sleInt64: begin
      val(Txt,resInt64,err);
      if err<>0 then exit;
      ToValue := resInt64;
    end;
    sleCurrency: begin
      val(Txt,resDouble,err);
      if err<>0 then exit;
      ToValue := Currency(resDouble);
    end;
    sleDouble: begin
      val(Txt,resDouble,err);
      if err<>0 then exit;
      ToValue := resDouble;
    end;
  end;
  result := true;
end;

procedure TSynLabeledEdit.KeyPress(var Key: char);
Var Temp: Variant;
    TempString: string;
begin
  inherited;
  if Key=#8 then exit;
  if Key=',' then
    Key := '.';
  if (Kind in [sleInteger,sleInt64]) and (Key='.') then
    Key := #0;
  if ((Key<'0') or (Key>'9')) and (Key<>'.') then begin
    Key := #0;
    Beep;
    exit;
  end;
  TempString := Text;
  if (TempString=#0) or (Self.SelText=TempString) then
    exit;
  Insert(Key, TempString, Self.SelStart + 1);
  if IsValid(TempString,Temp) and
     RangeChecking and (Temp>MaxValue) then begin
    Key := #0;
    Beep;
  end;
end;

procedure TSynLabeledEdit.MouseMove(Shift: TShiftState; X,Y: integer);
var H: string;
begin
  inherited;
  if RangeChecking then
    H := format(SMinMaxValue,[string(FMinValue),string(FMaxValue)]);
  if FAdditionalHint <> '' then
    H := trim(FAdditionalHint + #13#10 + H);
  Hint := H;
end;

procedure TSynLabeledEdit.SetValue(const Value: Variant);
begin
  Text := Value;
end;

function TSynLabeledEdit.ToString(NumberOfDigits: integer): string;
var numberOfMissingDigits: integer;
begin
  Result := Value;
  numberOfMissingDigits := NumberOfDigits - Length(Result);
  if numberOfMissingDigits > 0 then
    Result := StringOfChar('0', numberOfMissingDigits) + Result;
end;

function TSynLabeledEdit.ValidateValue: boolean;
var V: Variant;
begin
  result := IsValid(Text,V);
  if RangeChecking and result then
    result := (V>=MinValue) and (V<=MaxValue);
end;


procedure FillStringGrid(Source: TSQLTable; Dest: TStringGrid; Client: TSQLRest);
var C,R: integer;
    s: string;
begin
  if (Source=nil) or (Dest=nil) then
    exit; // avoid GPF
  Dest.ColCount := Source.FieldCount;
  Dest.RowCount := Source.RowCount+1;
  for R := 0 to Source.RowCount+1 do
    for C := 0 to Source.FieldCount-1 do begin
      Source.ExpandAsString(R,C,Client,s); // will do all the magic
      Dest.Cells[C,R] := s;
    end;
end;

procedure Register;
begin
  RegisterComponents('Synopse',[TSynLabeledEdit]);
end;

end.

