/// Reporting unit
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3Pages;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2012 Arnaud Bouchez
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

  The Original Code is Synopse framework.

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


    Initial Notes and Copyright:
   ******************************

 Component Name:  TGDIPages
 Module:          Pages
 Description:     Report writer and previewer
 Version:         1.6
 Date:            25-MAY-2004 (initial version)
 Target:          Win32, Delphi 3 - Delphi 7
 Author:          Angus Johnson,   angusj-AT-myrealbox-DOT-com
 Copyright        © 2003 Angus Johnson

 Notes:
   * TGDIPages is designed as a simple lightweight report writer. Reports are
     created in code, they are not banded, nor are they directly linked to
     TDatasets. If you're looking for a dataset aware report writer then
     TGDIPages is not for you. TGDIPages is a visual component based on a
     TScrollbox, though it isn't necessary to view reports prior to printing.

   * Main features include:
     + Text can be output either wrapped between page margins, in columns
       or at specified offsets.
     + Multiple alignment options -
         > left, right and justified in non-columned text
         > left, right and currency in columned text
     + Tabs to Assigned tabstops
     + Multi-line page headers, footers and column headers
     + Multiple fonts can be used.
     + Angled text output
     + Single, line & half, and double line spacing
     + Methods for printing bitmaps, lines, boxes and arrows
     + Page numbering can be redefined
     + Text output 'groups' prevent blocks of text spanning across pages
     + Designed around a TScrollbox descendant preview window with:
       mouse click zoom control; keyboard handling of lineup, linedown,
       pageup and pagedown srolling; mouse wheel scrolling.

   * In order to get the best print quality, TGDIPages uses the selected
     printer driver's resolution to prepare reports.
     If a report will be printed to a different printer (eg by using a
     PrintDialog), it's preferable to change to that printer object BEFORE
     preparing the report. Otherwise, the report will be stretch down to the
     printer canvas resulting in a slight degradation in print quality.


  Enhanced for the freeware Synopse framework:
 **********************************************

  - Windows XP, Vista and Seven compatibility adding
  - fix printing metafiles and page groups on some printer (with bad drivers)
  - optionnaly use antialiaised drawing (by using GDI+)
  - popup menu creation, with zoom, print or copy features (and custom entries)
  - direct PDF export (if a PDF printer is installed, e.g. free doPDF)
  - direct page export to clipboard as text
  - optional Black and White / Duplex mode (with out TPrinterNew custom class)
  - new usefull methods for easy text adding (especially column definition)
  - new fast double buffering drawing
  - limited Unicode charset conversion
  - speed up and various bug fixes

  Modifications © 2009-2010 Arnaud Bouchez

  Version 1.4 - February 8, 2010
  - whole Synopse SQLite3 database framework released under the GNU Lesser
    General Public License version 3, instead of generic "Public Domain"

  Version 1.6
  - new version, using our SynGdiPlus unit: if the GDI+ is available,
    it will use it to render the page using its AntiAliased engine;
    under Windows 98 or 2000, no antialiasing will occur, but the program
    will still run (since our SynGdiPlus unit use dynamic linking of the
    gdiplus.dll library);
    if only GDI+ 1.0 is available (i.e. with a Windows XP without any Office
    2003/2007 installed) a pure Delphi version of GDI+ drawing is used, which
    should not be able to convert 100% of page content, but should work on
    most cases

  Version 1.8
  - some fixes for compilation under Delphi 2009/2010

  Version 1.9
  - new AppendRichEdit method to draw RichEdit content
  - new WordWrapLeftCols property used to optionaly word wrap caLeft columns
    into multiple lines, i.e. if the text is wider than the column width, its
    content is wrapped to the next line (set to false by default) - this
    also will handle #13/#10 in column text as a "go to next line" command

  Version 1.9.2
  - fix font color issue in header and footers
  - safety additional code to avoid any division per 0 exception

  Version 1.11
  - fixed issue in TGDIPages.AppendRichEdit - see user feedback from
    http://synopse.info/forum/viewtopic.php?pid=671#p671
  - added Author, Subject and Keywords optional parameters to TGDIPages.ExportPDF

  Version 1.12
  - fixed one issue (in SynGdiPlus) for displaying bitmaps in anti-aliased mode
    and displaying underlined or stroken out text - new ForceInternalAntiAliased
    method (true by default) using SynGdiPlus instead of GDI+ 1.1 native conversion
  - OnStringToUnicodeEvent is now called for all text, whatever the alignment is
  - new property BiDiMode, for formatting the text in right to left order
  - added new DrawBMP overloaded method to add some bitmap as a (centered)
    paragraph, with some optional legend - bitmaps are now cached and reused
    in the exported PDF, if the same one is drawn multiple time in the document
  - added new AddBookMark, AddOutline and AddLink methods (working also with
    the PDF export using SynPdf) :)
  - live navigation via links in the preview screen, and via the new 'Bookmarks'
    popup menu entry
  - additional ExportPDF* properties used during PDF export
  - introducing the new TRenderPages class, for high-quality document rendering
    (used e.g. within SynProject for document preview and PDF generation,
    with basic understanding of the rtf format)

  Version 1.15
  - fixed an endless loop in TGDIPages.DrawTextAcrossCols when wrapping text
  - fixed an issue in TGDIPages.DrawTextAcrossCols when test is exported to pdf
    (wrong clipping region set)
  - if TGDIPages.WordWrapLeftCols=TRUE, won't wrap column headers

  Version 1.16
  - includes new TSynAnsiConvert classes for handling Ansi charsets
  - some minor fixes (e.g. preview landscape or keys for popup menu)
  - fix issue in TGDIPages.AppendRichEdit() when called on a blank page
  - enhanced the print preview screen with a left-sided button bar
  - new TGdiPages.RenderGraphic method (accepting both TBitmap and TMetaFile)

  Version 1.17
  - now whole text process is UNICODE-ready, even on pre-Delphi-2009 versions
  - now implements font fall-back in internal Anti-Aliaised drawing,
    if the new ForceInternalAntiAliasedFontFallBack property is set to TRUE


*)

interface

{.$define MOUSE_CLICK_PERFORM_ZOOM} // old not user-friendly behavior
{.$define RENDERPAGES} // TRenderBox and TRenderPages are not yet finished

{$define GDIPLUSDRAW}
// optionaly (if ForceNoAntiAliased=false) use GDI+ to draw for antialiasing:
// slower but smoother (need the GDI+ library, best with version 1.1)

{.$define USEPDFPRINTER}
// do not use the Synopse PDF engine, in Delphi code, but a PDF virtual printer

{.$define PRINTERNEW}
// if our custom Printer.pas unit is installed, use TPrinterNew class instead
// of TPrinter to allow Black&White and Duplex printing
// -> disabled by default, should be enabled globaly from the Project Options

{$ifndef ENHANCEDRTL}
  {$undef PRINTERNEW}
  // Black&White and Duplex printing are only available with our Enhanced RTL
{$endif}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  SynCommons,
{$ifndef USEPDFPRINTER}
  SynPdf,
{$endif}
  Windows, Messages, SysUtils, Classes, Contnrs,
{$ifdef GDIPLUSDRAW}
  SynGdiPlus,
{$endif}
  Graphics, Controls, Dialogs, Forms, StdCtrls,
  ExtCtrls, WinSpool, Printers, Menus, ShellAPI, RichEdit;

const
  MAXCOLS = 20;
  MAXTABS = 20;

  {{ this constant can be used to be replaced by the page number in
   the middle of any text }
  PAGENUMBER = '<<pagenumber>>';

type

  /// text paragraph alignment
  TTextAlign = (taLeft,taRight,taCenter,taJustified);

  /// text column alignment
  TColAlign = (caLeft,caRight,caCenter, caCurrency);

  /// text line spacing
  TLineSpacing = (lsSingle, lsOneAndHalf, lsDouble);

  /// available zoom mode
  // - zsPercent is used with a zoom percentage (e.g. 100% or 50%)
  // - zsPageFit fits the page to the report
  // - zsPageWidth zooms the page to fit the report width on screen
  TZoomStatus = (zsPercent, zsPageFit, zsPageWidth);

  /// Event trigerred when a new page is added
  TNewPageEvent = procedure(Sender: TObject; PageNumber: integer) of object;

  /// Event trigerred when the Zoom was changed
  TZoomChangedEvent = procedure(Sender: TObject;
    Zoom: integer; ZoomStatus: TZoomStatus) of object;

  /// Event trigerred to allow custom unicode character display on the screen
  // - called for all text, whatever the alignment is
  // - Text content can be modified by this event handler to customize
  // some characters (e.g. '>=' can be converted to the one unicode equivalent)
  TOnStringToUnicodeEvent = function(const Text: SynUnicode): SynUnicode of object;
    
  TGDIPages = class;

  /// a report layout state, as used by SaveLayout/RestoreSavedLayout methods
  TSavedState = record
    FontName: string;
    FontColor: integer;
    Flags: integer;
    LeftMargin: integer;
    RightMargin: integer;
    BiDiMode: TBiDiMode;
  end;

  /// internal format of the header or footer text
  THeaderFooter = class
  public
    Text: SynUnicode;
    State: TSavedState;
    /// initialize the header or footer parameters with current report state
    constructor Create(Report: TGDIPages; doubleline: boolean;
      const aText: SynUnicode=''; IsText: boolean=false);
  end;

  /// internal format of a text column
  TColRec = record
    ColLeft, ColRight: integer;
    ColAlign: TColAlign;
    ColBold: boolean;
  end;

  TPopupMenuClass = class of TPopupMenu;

  /// hack the TPaintBox to allow custom background erase
  TPagePaintBox = class(TPaintBox)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  end;

  /// internal structure used to store bookmarks or links
  TGDIPagereference = class
  public
    /// the associated page number (starting at 1)
    Page: Integer;
    /// graphical coordinates of the hot zone
    // - for bookmarks, Top is the Y position
    // - for links, the TRect will describe the hot region
    // - for Outline, Top is the Y position and Bottom the outline tree level
    Rect: TRect;
    /// coordinates on screen of the hot zone
    Preview: TRect;
    /// initialize the structure with the current page
    constructor Create(PageNumber: integer; Left, Top, Right, Bottom: integer);
    /// compute the coordinates on screen into Preview
    procedure ToPreview(Pages: TGDIPages);
  end;

  /// Report class for generating documents from code
  // - data is drawn in memory, they displayed or printed as desired
  // - allow preview and printing, and direct pdf export
  // - handle bookmark, outlines and links inside the document
  // - page coordinates are in mm's
  TGDIPages = class(TScrollBox)
  protected
    fPreviewSurface: TPagePaintbox;
    fCanvas: TMetafileCanvas;
    fCanvasText: string;
    fBeforeGroupText: string;
    fGroupPage: TMetafile;
    // Strings[] is text (for clipboard), Objects[] is TMetaFile
    fPages: TStringList;
    fHeaderLines: TObjectList;
    fFooterLines: TObjectList;
    fColumns: array of TColRec;
    fColumnHeaderList: TStringList;
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
    fZoomTimer: TTimer;
{$endif}
    fPtrHdl: THandle;

    fTabCount: integer;
    fCurrentPrinter: string;
    fOrientation: TPrinterOrientation;
    fDefaultLineWidth: integer;        //drawing line width (boxes etc)
    fVirtualPageNum: integer;
    fCurrPreviewPage: integer;
    fZoomIn: boolean;
    fLineHeight: integer;              //Text line height
    fLineSpacing: TLineSpacing;
    fCurrentYPos: integer;
    fCurrentTextTop, fCurrentTextPage: integer;
    fHeaderHeight: integer;
    fHangIndent: integer;
    fAlign: TTextAlign;
    fBiDiMode: TBiDiMode;
    fPageMarginsPx: TRect;
    fHasPrinterInstalled: boolean;
{$ifdef USEPDFPRINTER}
    fHasPDFPrinterInstalled: boolean;
    fPDFPrinterIndex: integer;
{$else}
    fForceJPEGCompression: Integer;
    fExportPDFApplication: string;
    fExportPDFAuthor: string;
    fExportPDFSubject: string;
    fExportPDFKeywords: string;
    fExportPDFEmbeddedTTF: boolean;
    fExportPDFA1: boolean;
    {$ifndef NO_USE_UNISCRIBE}
    fExportPDFUseUniscribe: boolean;
    {$endif}
{$endif}
    fPrinterPxPerInch: TPoint;
    fPhysicalSizePx: TPoint;           //size of page in printer pixels
    fPhysicalOffsetPx: TPoint;         //size of non-printing margins in pixels
    fZoom: integer;
    fZoomStatus: TZoomStatus;
    fNegsToParenthesesInCurrCols: boolean;
    fWordWrapLeftCols: boolean;
    fUseOutlines: boolean;
    fForceScreenResolution: boolean;
    fHeaderDone: boolean;
    fFooterHeight: integer;
    fFooterGap: integer;
    fInHeaderOrFooter: boolean;
    fColumnHeaderPrinted: boolean;
    fColumnHeaderPrintedAtLeastOnce: boolean;
    fDrawTextAcrossColsDrawingHeader: boolean;

    fColumnHeaderInGroup: boolean;
    fColumnsUsedInGroup: boolean;
    fGroupVerticalSpace: integer;
    fGroupVerticalPos: integer;

    fZoomChangedEvent: TZoomChangedEvent;
    fPreviewPageChangedEvent: TNotifyEvent;
    fStartNewPage: TNewPageEvent;
    fStartPageHeader: TNotifyEvent;
    fEndPageHeader: TNotifyEvent;
    fStartPageFooter: TNotifyEvent;
    fEndPageFooter: TNotifyEvent;
    fStartColumnHeader: TNotifyEvent;
    fEndColumnHeader: TNotifyEvent;

    fSavedCount: integer;
    fSaved: array of TSavedState;

    fTab: array of integer;
    fColumnsWithBottomGrayLine: boolean;
    fColumnsRowLineHeight: integer;
    fOnDocumentProducedEvent: TNotifyEvent;
    PageRightButton, PageLeftButton: TPoint;
    fPagesToFooterText: string; // not SynUnicode, since calls format()
    fPagesToFooterAt: TPoint;
    fPagesToFooterState: TSavedState;

    procedure GetPrinterParams;
    function  GetPaperSize: TSize;
    function  PrinterPxToScreenPxX(PrinterPx: integer): integer;
    function  PrinterPxToScreenPxY(PrinterPx: integer): integer;
    procedure ResizeAndCenterPaintbox;

    function  GetOrientation: TPrinterOrientation;
    procedure SetOrientation(orientation: TPrinterOrientation);
    procedure SetTextAlign(Value: TTextAlign);
    procedure SetPage(NewPreviewPage: integer);
    function  GetPageCount: integer;
    function  GetLineHeight: integer;
    function  GetLineHeightMm: integer;
    procedure CheckYPos;               //ie: if not vertical room force new page
    function  GetYPos: integer;
    procedure SetYPos(YPos: integer);
    procedure NewPageInternal; virtual;
    function  CreateMetaFile(aWidth, aHeight: integer): TMetaFile;
    function  CreateMetafileCanvas(Page: TMetafile): TMetafileCanvas;
    procedure UpdateMetafileCanvasFont(aCanvas: TMetafileCanvas);
    function  TextFormatsToFlags: integer;
    procedure SetFontWithFlags(flags: integer);
    function  GetPageMargins: TRect;
    procedure SetPageMargins(Rect: TRect);

    procedure DoHeader;
    procedure DoFooter;
    procedure DoHeaderFooterInternal(Lines: TObjectList);
    procedure CalcFooterGap;

    function  GetColumnCount: integer;
    function  GetColumnRec(col: integer): TColRec;
    procedure PrintColumnHeaders;

    procedure SetZoom(zoom: integer);
    procedure ZoomTimerInternal(X,Y: integer; ZoomIn: boolean);
    procedure ZoomTimer(Sender: TObject);

    procedure LineInternal(start,finish: integer; DoubleLine: boolean);
    procedure PrintFormattedLine(s: SynUnicode; flags: integer;
      const aBookmark: string=''; const aLink: string='');
    procedure LeftOrJustifiedWrap(const s: SynUnicode);
    procedure RightOrCenterWrap(const s: SynUnicode);
    procedure GetTextLimitsPx(var LeftOffset, RightOffset: integer);
    procedure HandleTabsAndPrint(const leftstring: SynUnicode;
      var rightstring: SynUnicode; leftOffset, rightOffset: integer);
    procedure PreviewPaint(Sender: TObject);
    procedure PreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetLeftMargin: integer;
    procedure SetLeftMargin(const Value: integer);
    function GetRightMarginPos: integer;
    function GetSavedState: TSavedState;
    procedure SetSavedState(const SavedState: TSavedState);
    /// can be used internaly (for instance by fPagesToFooterState)  
    property SavedState: TSavedState read GetSavedState write SetSavedState;
  protected
    fMousePos: TPoint;
{$ifndef MOUSE_CLICK_PERFORM_ZOOM}
    fButtonDown, fButtonDownScroll: TPoint;
{$endif}
    /// Strings[] are the bookmark names, and Objects[] are TGDIPagereference
    // to get the Y position
    fBookmarks: TStringList;
    /// Strings[] are the bookmark names, and Objects[] are TGDIPagereference to
    // get the hot region
    fLinks: TStringList;
    fLinksCurrent: integer;
    /// Strings[] are the outline titles, and Objects[] are TGDIPagereference
    // to get the Y position of the destination
    fOutline: TStringList;
    fInternalUnicodeString: SynUnicode;
    PreviewForm: TForm;
    PreviewButtons: array of TButton;
    PreviewPageCountLabel: TLabel;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    {$IFNDEF VER100}
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override; //no mousewheel support in Delphi 3
    {$ENDIF}
    procedure PopupMenuPopup(Sender: TObject);
    procedure CheckHeaderDone; virtual;
    // warning: PW buffer is overwritten at the next method call
    procedure InternalUnicodeString(const s: SynUnicode;
      var PW: PWideChar; var PWLen: integer; size: PSize);

  public
    /// Event trigerred when the ReportPopupMenu is displayed
    // - default handling (i.e. leave this field nil) is to add Page naviguation
    // - you can override this method for adding items to the ReportPopupMenu
    OnPopupMenuPopup: TNotifyEvent;
    /// Event trigerred when a ReportPopupMenu item is selected
    // - default handling (i.e. leave this field nil) is for Page navigation
    // - you can override this method for handling additionnal items to the menu
    // - the Tag component of the custom TMenuItem should be 0 or greater than
    // Report pages count: use 1000 as a start for custom TMenuItem.Tag values
    OnPopupMenuClick: TNotifyEvent;
    /// user can customize this class to create an advanced popup menu instance
    PopupMenuClass: TPopupMenuClass;
    /// the title of the report
    // - used for the preview caption form
    // - used for the printing document name
    Caption: string;
    /// if true, the PrintPages() method will use a temporary bitmap for printing
    // - some printer device drivers have problems with printing metafiles
    // which contains other metafiles; should have been fixed
    // - not usefull, since slows the printing a lot and makes huge memory usage
    ForcePrintAsBitmap: boolean;
    /// if true the preview will not use GDI+ library to draw anti-aliaised graphics
    // - this may be slow on old computers, so caller can disable it on demand
    ForceNoAntiAliased: boolean;
    /// if true, drawing will NOT to use native GDI+ 1.1 conversion
    // - we found out that GDI+ 1.1 was not as good as our internal conversion
    // function written in Delphi, e.g. for underlined fonts
    // - so this property is set to true by default for proper display on screen
    // - will only be used if ForceNoAntiAliased is false, of course
    ForceInternalAntiAliased: boolean;
    /// if true, internal text drawing will use a font-fallback mechanism
    // for characters not existing within the current font (just as with GDI)
    // - is disabled by default, but could be set to TRUE to force enabling
    // TGDIPlusFull.ForceUseDrawString property
    ForceInternalAntiAliasedFontFallBack: boolean;

{$ifdef PRINTERNEW}
    // the PrintPages() will use this parameter to force black and white, or
    // color mode, whatever the global printer setting is
    ForcePrintColorMode: (printColorDefault, printBW, printColor);
    // the PrintPages() will use this parameter to force duplex mode,
    // whatever the global printer setting is
    ForcePrintDuplexMode: (printDuplexDefault, printSimplex, printDuplex);
{$endif}
    /// if true, the headers are copied only once to the text
    ForceCopyTextAsWholeContent: boolean;
    /// customize left aligned text conversion from Ansi
    // - to be used before Delphi 2009/2010/XE only, in order to force some
    // character customization (e.g. <= or >=) 
    OnStringToUnicode: TOnStringToUnicodeEvent;
    /// set group page fill method
    // - if set to true, the groups will be forced to be placed on the same page
    // (this was the original default "Pages" component behavior, but this
    // is not usual in page composition, so is disabled by default in TGDIPages) 
    // - if set to false, the groups will force a page feed if there is not
    // enough place for 20 lines on the current page (default behavior)
    GroupsMustBeOnSamePage: boolean;
    /// the bitmap used to draw the page
    PreviewSurfaceBitmap: TBitmap;

    /// creates the reporting component
    constructor Create(AOwner: TComponent); override;
    /// finalize the component, releasing all used memory
    destructor Destroy; override;
    /// customized invalidate
    procedure Invalidate; override;

    /// Begin a Report document
    // - Every report must start with BeginDoc and end with EndDoc
    procedure BeginDoc;
    /// Clear the current Report document
    procedure Clear; virtual;
    /// draw some text as a paragraph, with the current alignment
    // - this method does all word-wrapping and formating if necessary
    // - this method handle multiple paragraphs inside s (separated by newlines -
    // i.e. #13)
    procedure DrawText(const s: string); {$ifdef HASINLINE}inline;{$endif}
    /// draw some UTF-8 text as a paragraph, with the current alignment
    // - this method does all word-wrapping and formating if necessary
    // - this method handle multiple paragraphs inside s (separated by newlines -
    // i.e. #13)
    procedure DrawTextU(const s: RawUTF8); {$ifdef HASINLINE}inline;{$endif}
    /// draw some Unicode text as a paragraph, with the current alignment
    // - this method does all word-wrapping and formating if necessary
    // - this method handle multiple paragraphs inside s (separated by newlines -
    // i.e. #13)
    procedure DrawTextW(const s: SynUnicode);
    /// draw some text as a paragraph, with the current alignment
    // - this method use format() like parameterss
    procedure DrawTextFmt(const s: string; const Args: array of const);
    /// get the formating flags associated to a Title
    function TitleFlags: integer;
    /// draw some text as a paragraph title
    // - the outline level can be specified, if UseOutline property is enabled
    // - if aBookmark is set, a bookmark is created at this position
    // - if aLink is set, a link to the specified bookmark name (in aLink) is made
    procedure DrawTitle(const s: SynUnicode; DrawBottomLine: boolean=false; OutlineLevel: Integer=0;
      const aBookmark: string=''; const aLink: string='');
    /// draw one line of text, with the current alignment
    procedure DrawTextAt(s: SynUnicode; XPos: integer; const aLink: string='';
      CheckPageNumber: boolean=false);
    /// draw one line of text, with a specified Angle and X Position
    procedure DrawAngledTextAt(const s: SynUnicode; XPos, Angle: integer);
    /// draw a square box at the given coordinates
    procedure DrawBox(left,top,right,bottom: integer);
    /// draw a filled square box at the given coordinates
    procedure DrawBoxFilled(left,top,right,bottom: integer; Color: TColor);
    /// Stretch draws a bitmap image at the specified page coordinates in mm's
    procedure DrawBMP(rec: TRect; bmp: TBitmap); overload;
    /// add the bitmap at the specified X position
    // - if there is not enough place to draw the bitmap, go to next page
    // - then the current Y position is updated
    // - bLeft (in mm) is calculated in reference to the LeftMargin position
    // - if bLeft is maxInt, the bitmap is centered to the page width
    // - bitmap is stretched (keeping aspect ratio) for the resulting width to
    // match the bWidth parameter (in mm)
    procedure DrawBMP(bmp: TBitmap; bLeft, bWidth: integer; const Legend: string=''); overload;
    /// Stretch draws a metafile image at the specified page coordinates in mm's
    procedure DrawMeta(rec: TRect; meta: TMetafile);
    /// add the graphic (bitmap or metafile) at the specified X position
    // - handle only TBitmap and TMetafile kind of TGraphic
    // - if there is not enough place to draw the bitmap, go to next page
    // - then the current Y position is updated
    // - bLeft (in mm) is calculated in reference to the LeftMargin position
    // - if bLeft is maxInt, the bitmap is centered to the page width
    // - bitmap is stretched (keeping aspect ratio) for the resulting width to
    // match the bWidth parameter (in mm)
    procedure DrawGraphic(graph: TGraphic; bLeft, bWidth: integer; const Legend: SynUnicode=''); 
    /// draw an Arrow
    procedure DrawArrow(Point1, Point2: TPoint; HeadSize: integer; SolidHead: boolean);
    /// draw a Line, either simple or double, between the left & right margins
    procedure DrawLine(doubleline: boolean=false);
    /// draw a Dashed Line between the left & right margins
    procedure DrawDashedLine;
    /// append a Rich Edit content to the current report
    procedure AppendRichEdit(RichEditHandle: HWnd);
    /// jump some line space between paragraphs
    // - Increments the current Y Position the equivalent of a single line
    // relative to the current font height and line spacing
    procedure NewLine;
    /// jump some half line space between paragraphs
    // - Increments the current Y Position the equivalent of an half single line
    // relative to the current font height and line spacing
    procedure NewHalfLine;
    /// jump some line space between paragraphs
    // - Increments the current Y Position the equivalent of 'count' lines
    // relative to the current font height and line spacing
    procedure NewLines(count: integer);
    /// save the current font and alignment
    procedure SaveLayout; virtual;
    /// restore last saved font and alignment
    procedure RestoreSavedLayout; virtual;
    /// jump to next page, i.e. force a page break
    procedure NewPage(ForceEndGroup: boolean=false);
    /// jump to next page, but only if some content is pending
    procedure NewPageIfAnyContent;
    /// begin a Group: stops the contents from being split across pages
    // - BeginGroup-EndGroup text blocks can't be nested
    procedure BeginGroup;
    /// end a previously defined Group
    // - BeginGroup-EndGroup text blocks can't be nested
    procedure EndGroup;
    /// End the Report document
    // - Every report must start with BeginDoc and end with EndDoc
    procedure EndDoc;
    /// Print the selected pages to the default printer of Printer unit
    // - if PrintFrom=0 and PrintTo=0, then all pages are printed
    // - if PrintFrom=-1 or PrintTo=-1, then a printer dialog is displayed
    function PrintPages(PrintFrom, PrintTo: integer): boolean;
    /// export the current report as PDF
{$ifdef USEPDFPRINTER}
    // - uses an external 'PDF' printer
{$else}
    // - uses internal PDF code, from Synopse PDF engine (handle bookmarks,
    // outline and twin bitmaps) - in this case, a file name can be set 
{$endif}
    function ExportPDF(aPdfFileName: TFileName; ShowErrorOnScreen: boolean;
      LaunchAfter: boolean=true): boolean;
    /// show a form with the preview, allowing the user to browse pages and
    // print the report
    procedure ShowPreviewForm;

    /// set the Tabs stops on every line
    // - if one value is provided, it will set the Tabs as every multiple of it
    // - if more than one value are provided, they will be the exact Tabs positions
    procedure SetTabStops(const tabs: array of integer);
    /// returns true if there is enough space in the current Report for Count lines
    // - Used to check if there's sufficient vertical space remaining on the page
    // for the specified number of lines based on the current Y position
    function  HasSpaceForLines(Count: integer): boolean;
    /// returns true if there is enough space in the current Report for a
    // vertical size, specified in mm
    function  HasSpaceFor(mm: integer): boolean;

    /// Clear all already predefined Headers
    procedure ClearHeaders;
    /// Adds either a single line or a double line (drawn between the left &
    // right page margins) to the page header
    procedure AddLineToHeader(doubleline: boolean);
    /// Adds text using to current font and alignment to the page header
    procedure AddTextToHeader(const s: SynUnicode);
    /// Adds text to the page header at the specified horizontal position and
    // using to current font.
    // - No Line feed will be triggered: this method doesn't increment the YPos,
    // so can be used to add multiple text on the same line
    procedure AddTextToHeaderAt(const s: SynUnicode; XPos: integer);

    /// Clear all already predefined Footers
    procedure ClearFooters;
    /// Adds either a single line or a double line (drawn between the left &
    // right page margins) to the page footer
    procedure AddLineToFooter(doubleline: boolean);
    /// Adds text using to current font and alignment to the page footer
    procedure AddTextToFooter(const s: SynUnicode);
    /// Adds text to the page footer at the specified horizontal position and
    // using to current font. No Line feed will be triggered.
    procedure AddTextToFooterAt(const s: SynUnicode; XPos: integer);
    /// Will add the current 'Page n/n' text at the specified position
    // - PageText must be of format 'Page %d/%d', in the desired language
    procedure AddPagesToFooterAt(const PageText: string; XPos: integer);

    /// register a column, with proper alignment
    procedure AddColumn(left, right: integer; align: TColAlign; bold: boolean);
    /// register same alignement columns, with percentage of page column width
    // - sum of all percent width should be 100, but can be of any value
    // - negative widths are converted into absolute values, but
    // corresponding alignment is set to right
    // - if a column need to be right aligned or currency aligned,
    // use SetColumnAlign() method below
    // - individual column may be printed in bold with SetColumnBold() method
    procedure AddColumns(const PercentWidth: array of integer; align: TColAlign=caLeft);
    /// register some column headers, with the current font formating
    // - Column headers will appear just above the first text output in
    // columns on each page
    // - you can call this method several times in order to have diverse
    // font formats across the column headers
    procedure AddColumnHeaders(const headers: array of SynUnicode;
      WithBottomGrayLine: boolean=false; BoldFont: boolean=false;
      RowLineHeight: integer=0; flags: integer=0);
    /// register some column headers, with the current font formating
    // - Column headers will appear just above the first text output in
    // columns on each page
    // - call this method once with all columns text as CSV
    procedure AddColumnHeadersFromCSV(var CSV: PWideChar;
      WithBottomGrayLine: boolean; BoldFont: boolean=false; RowLineHeight: integer=0);
    /// draw some text, split across every columns
    // - if BackgroundColor is not clNone (i.e. clRed or clNavy or clBlack), the
    // row is printed on white with this background color (e.g. to highlight errors)
    procedure DrawTextAcrossCols(const StringArray: array of SynUnicode;
      BackgroundColor: TColor=clNone);
    /// draw some text, split across every columns
    // - this method expect the text to be separated by commas
    // - if BackgroundColor is not clNone (i.e. clRed or clNavy or clBlack), the
    // row is printed on white with this background color (e.g. to highlight errors)
    procedure DrawTextAcrossColsFromCSV(var CSV: PWideChar; BackgroundColor: TColor=clNone);
    /// draw (double if specified) lines at the bottom of all currency columns
    procedure DrawLinesInCurrencyCols(doublelines: boolean);

    /// retrieve the current Column count
    property ColumnCount: integer read GetColumnCount;
    /// retrieve the attributes of a specified column
    function GetColumnInfo(index: integer): TColRec;
    /// individually set column alignment
    // - usefull after habing used AddColumns([]) method e.g.
    procedure SetColumnAlign(index: integer; align: TColAlign);
    /// individually set column bold state
    // - usefull after habing used AddColumns([]) method e.g.
    procedure SetColumnBold(index: integer);
    /// erase all columns and the associated headers
    procedure ClearColumns;
    /// clear the Headers associated to the Columns
    procedure ClearColumnHeaders;
    /// ColumnHeadersNeeded will force column headers to be drawn again just
    // prior to printing the next row of columned text
    // - Usually column headers are drawn once per page just above the first
    // column. ColumnHeadersNeeded is useful where columns of text have been
    // separated by a number of lines of non-columned text
    procedure ColumnHeadersNeeded;

    /// create a bookmark entry at the current position of the current page
    // - return false if this bookmark name was already existing, true on success
    // - if aYPosition is not 0, the current Y position will be used
    function AddBookMark(const aBookmarkName: string; aYPosition: integer=0): Boolean; virtual;
    /// go to the specified bookmark
    // - returns true if the bookmark name was existing and reached
    function GotoBookmark(const aBookmarkName: string): Boolean; virtual;
    /// create an outline entry at the current position of the current page
    // - if aYPosition is not 0, the current Y position will be used
    procedure AddOutline(const aTitle: string; aLevel: Integer;
      aYPosition: integer=0; aPageNumber: integer=0); virtual;
    /// create a link entry at the specified coordinates of the current page
    // - coordinates are specified in mm
    // - the bookmark name is not checked by this method: a bookmark can be
    // linked before being marked in the document 
    procedure AddLink(const aBookmarkName: string; aRect: TRect;
      aPageNumber: integer=0); virtual;

    /// convert a rect of mm into pixel canvas units
    function MmToPrinter(const R: TRect): TRect;
    /// convert a rect of pixel canvas units into mm
    function PrinterToMM(const R: TRect): TRect;
    /// convert a mm X position into pixel canvas units
    function MmToPrinterPxX(mm: integer): integer;
    /// convert a mm Y position into pixel canvas units
    function MmToPrinterPxY(mm: integer): integer;
    /// convert a pixel canvas X position into mm
    function PrinterPxToMmX(px: integer): integer;
    /// convert a pixel canvas Y position into mm
    function PrinterPxToMmY(px: integer): integer;
    /// return the width of the specified text, in mm
    function TextWidth(const Text: SynUnicode): integer;
    /// the current Text Alignment, during text adding
    property TextAlign: TTextAlign read fAlign write SetTextAlign;
    /// specifies the reading order (bidirectional mode) of the box
    // - only bdLeftToRight and bdRightToLeft are handled
    // - this will be used by DrawText[At], DrawTitle, AddTextToHeader/Footer[At],
    // DrawTextAcrossCols, SaveLayout/RestoreSavedLayout methods
    property BiDiMode: TBiDiMode read fBiDiMode write fBiDiMode;
    /// create a meta file and its associated canvas for displaying a picture
    // - you must release manually both Objects after usage
    function CreatePictureMetaFile(Width, Height: integer;
      out MetaCanvas: TCanvas): TMetaFile;
    /// Distance (in mm's) from the top of the page to the top of the current group
    // - returns CurrentYPos if no group is in use
    function CurrentGroupPosStart: integer;
    /// go to the specified Y position on a given page
    // - used e.g. by GotoBookmark() method
    procedure GotoPosition(aPage: integer; aYPos: integer);
    /// the TMetaFile list containing all pages content
    // - the TMetaFile is stored in the Objects[] array
    // - the text equivalent of the page is stored in the Strings[] array
    // - numerotation begin with Pages[0] for page 1
    // - The Pages property should be rarely needed
    property Pages: TStringList read fPages;
    /// add an item to the popup menu
    // - used mostly internaly to add page browsing
    // - default OnClick event is to go to page set by the Tag property
    function NewPopupMenuItem(const aCaption: string; Tag: integer=0;
      SubMenu: TMenuItem=nil; OnClick: TNotifyEvent=nil; ImageIndex: integer=-1): TMenuItem;
    /// this is the main popup menu item click event
    procedure PopupMenuItemClick(Sender: TObject);
    /// can be used to draw directly using GDI commands
    // - The Canvas property should be rarely needed
    property Canvas: TMetaFileCanvas read fCanvas;
    /// Distance (in mm's) from the top of the page to the top of the next line
    property CurrentYPos: integer read GetYPos write SetYPos;
    /// get current line height (mm)
    property LineHeight: integer read GetLineHeightMm;
    /// the name of the current selected printer
    property PrinterName: string read fCurrentPrinter;
    /// the index of the previewed page
    // - please note that the first page is 1 (not 0)
    property Page: integer read fCurrPreviewPage write SetPage;
    /// total number of pages
    property PageCount: integer read GetPageCount;
    /// Size of each margin relative to its corresponding edge in mm's
    property PageMargins: TRect read GetPageMargins write SetPageMargins;
    /// Size of the left margin relative to its corresponding edge in mm's
    property LeftMargin: integer read GetLeftMargin write SetLeftMargin;
    /// Position of the right margin, in mm
    property RightMarginPos: integer read GetRightMarginPos;
    /// get the current selected paper size
    property PaperSize: TSize read GetPaperSize;
    /// number of pixel per inch, for X and Y directions
    property PrinterPxPerInch: TPoint read fPrinterPxPerInch;
{$ifdef USEPDFPRINTER}
    /// true if any printer appears to be a PDF printer
    property HasPDFPrinterInstalled: boolean read fHasPDFPrinterInstalled;
{$else}
    /// this property can force saving all bitmaps as JPEG in exported PDF
    // - by default, this property is set to 0 by the constructor of this class,
    // meaning that the JPEG compression is not forced, and the engine will use
    // the native resolution of the bitmap - in this case, the resulting
    // PDF file content will be bigger in size (e.g. use this for printing)
    // - 60 is the prefered way e.g. for publishing PDF over the internet
    // - 80/90 is a good ration if you want to have a nice PDF to see on screen
    // - of course, this doesn't affect vectorial (i.e. emf) pictures
    property ExportPDFForceJPEGCompression: integer read fForceJPEGCompression write fForceJPEGCompression;
    /// optional application name used during Export to PDF
    // - if not set, global Application.Title will be used
    property ExportPDFApplication: string read fExportPDFApplication write fExportPDFApplication;
    /// optional Author name used during Export to PDF
    property ExportPDFAuthor: string read fExportPDFAuthor write fExportPDFAuthor;
    /// optional Subject text used during Export to PDF
    property ExportPDFSubject: string read fExportPDFSubject write fExportPDFSubject;
    /// optional Keywords name used during Export to PDF
    property ExportPDFKeywords: string read fExportPDFKeywords write fExportPDFKeywords;
    /// if set to TRUE, the used True Type fonts will be embedded to the exported PDF
    // - not set by default, to save disk space and produce tiny PDF
    property ExportPDFEmbeddedTTF: boolean read fExportPDFEmbeddedTTF write fExportPDFEmbeddedTTF;
    /// if set to TRUE, the exported PDF is made compatible with PDF/A-1 requirements
    property ExportPDFA1: Boolean read fExportPDFA1 write fExportPDFA1;
    {$ifndef NO_USE_UNISCRIBE}
    /// set if the exporting PDF engine must use the Windows Uniscribe API to
    // render Ordering and/or Shaping of the text
    // - usefull for Hebrew, Arabic and some Asiatic languages handling
    // - set to FALSE by default, for faster content generation
    // - the Synopse PDF engine don't handle Font Fallback yet: the font you use
    // must contain ALL glyphs necessary for the supplied unicode text - squares
    // or blanks will be drawn for any missing glyph/character
    property ExportPDFUseUniscribe: boolean read fExportPDFUseUniscribe write fExportPDFUseUniscribe;
    {$endif}
{$endif}
    /// the current page number, during text adding
    // - Page is used during preview, after text adding
    property VirtualPageNum: integer read fVirtualPageNum write fVirtualPageNum;
    /// true if any header as been drawn, that is if something is to be printed
    property HeaderDone: boolean read fHeaderDone;
{    /// used to set if columns must be delimited at their bottom with a gray line
    property ColumnsWithBottomGrayLine: boolean read fColumnsWithBottomGrayLine
      write fColumnsWithBottomGrayLine; }
  published
    /// accounting standard layout for caCurrency columns:
    // - convert all negative sign into parentheses
    // - using parentheses instead of negative numbers is used in financial
    // statement reporting (see e.g. http://en.wikipedia.org/wiki/Income_statement)
    // - align numbers on digits, not parentheses
    property NegsToParenthesesInCurrCols: boolean
      read fNegsToParenthesesInCurrCols write fNegsToParenthesesInCurrCols;
    /// word wrap (caLeft) left-aligned columns into multiple lines
    // - if the text is wider than the column width, its content
    // is wrapped to the next line
    // - if the text contains some #13/#10 characters, it will be splitted into
    // individual lines
    // - this is disabled by default
    property WordWrapLeftCols: boolean read fWordWrapLeftCols write fWordWrapLeftCols;
    /// if set, any DrawTitle() call will create an Outline entry
    // - used e.g. for PDF generation
    // - this is enabled by default
    property UseOutlines: boolean read fUseOutlines write fUseOutlines;
    /// left justification hang indentation  
    property HangIndent: integer read fHangIndent write fHangIndent;
    /// Line spacing: can be lsSingle, lsOneAndHalf or lsDouble
    property LineSpacing: TLineSpacing read fLineSpacing write fLineSpacing;
    /// the paper orientation
    // - nb: it's not possible to change the orientation once a report has started,
    // i.e. after a BeginDoc call
    property Orientation: TPrinterOrientation read GetOrientation write SetOrientation;
    /// the current Zoom value, according to the zoom status
    property Zoom: integer read fZoom write SetZoom;
    /// the current Zoom procedure, i.e. zsPercent, zsPageFit or zsPageWidth
    property ZoomStatus: TZoomStatus read fZoomStatus;
    /// if set to true, we reduce the precision for better screen display
    property ForceScreenResolution: boolean
      read fForceScreenResolution write fForceScreenResolution;

    /// Event triggered when each new page is created
    property OnNewPage: TNewPageEvent
      read fStartNewPage write fStartNewPage;
    /// Event triggered when each new header is about to be drawn
    property OnStartPageHeader: TNotifyEvent
      read fStartPageHeader write fStartPageHeader;
    /// Event triggered when each header was drawn
    property OnEndPageHeader: TNotifyEvent
      read fEndPageHeader write fEndPageHeader;
    /// Event triggered when each new footer is about to be drawn
    property OnStartPageFooter: TNotifyEvent
      read fStartPageFooter write fStartPageFooter;
    /// Event triggered when each footer was drawn
    property OnEndPageFooter: TNotifyEvent
      read fEndPageFooter write fEndPageFooter;
    /// Event triggered when each new column is about to be drawn
    property OnStartColumnHeader: TNotifyEvent
      read fStartColumnHeader write fStartColumnHeader;
    /// Event triggered when each column was drawn
    property OnEndColumnHeader: TNotifyEvent
      read fEndColumnHeader write fEndColumnHeader;

    /// Event triggered whenever the report document generation is done
    // - i.e. when the EndDoc method has just been called
    property OnDocumentProduced: TNotifyEvent
      read fOnDocumentProducedEvent write fOnDocumentProducedEvent;
    /// Event triggered whenever the current preview page is changed
    property OnPreviewPageChanged: TNotifyEvent
      read fPreviewPageChangedEvent write fPreviewPageChangedEvent;
    /// Event triggered whenever the preview page is zoomed in or out
    property OnZoomChanged: TZoomChangedEvent
      read fZoomChangedEvent write fZoomChangedEvent;
  end;

{$ifdef RENDERPAGES}
  TRenderPages = class;

  /// a TRenderPages additional layout state
  // - used by the overriden SaveLayout/RestoreSavedLayout methods
  TSavedStateRender = record
    FirstLineIndent: Integer;
    Before: Integer;
    After: Integer;
    RightIndent: Integer;
    LeftIndent: Integer;
  end;

  PRenderBoxWord = ^TRenderBoxWord;

  /// the internal "Word" box structure used by TRenderBox
  TRenderBoxWord = packed record
    /// offset in the fText[] array
    TextOffset: integer;
    /// PWideChar count starting from fText[TextOffset]
    TextLength: integer;
    /// size on the canvas
    Size: TSize;
    /// used to retrieve associated font attributes
    FontIndex: integer;
    /// space width from current font attribute
    FontSpaceWidth: integer;
    /// number of spaces at the right side of this "Word" box
    SpaceAfterCount: integer;
    /// associated link bookmark name
    // - from fLinksBookMarkName[LinkNumber-1], no link set for 0
    LinkNumber: integer;
  end;

  PRenderBoxLayout = ^TRenderBoxLayout;
  
  /// the internal "drawing" box structure used by TRenderBox
  // - TRenderBox.InternalRender populate fLayout[] with this structures,
  // ready to be drawn to the document Canvas
  TRenderBoxLayout = packed record
    /// pointer of the words in the fText[] array
    Text: PWideChar;
    /// number of PWideChar starting at Text^
    Length: integer;
    /// layout box X coordinate
    Left: integer;
    /// layout box Y coordinate
    Top: integer;
    /// layout box width (in pixels)
    Width: integer;
    /// layout box height (in pixels) - that is, the line height
    Height: integer;
    /// corresponding rendered line index (starting at 0)
    LineIndex: integer;
    /// used to retrieve associated font attributes and links e.g.
    LastBox: PRenderBoxWord;
    /// length of extra space, in pixels - as used by SetTextJustification()
    BreakExtra: integer;
    /// count of space characters in line of text - as used by SetTextJustification()
    BreakCount: integer;
  end;

  /// used to render a "box" of text
  // - will handle word adding, and formatting for a given width
  // - is used by TRenderPage for a whole paragraph, or a column inside a table
  TRenderBox = class
  protected
    fBiDiMode: TBiDiMode;
    fWidth: integer;
    fHeight: integer;
    /// an internal buffer containing the Unicode text of this box
    fText: array of WideChar;
    fTextLen: integer;
    /// word markers of the current text
    fBox: array of TRenderBoxWord;
    fBoxCount: integer;
    /// InternalRender will fill this ready to be rendered layout array
    fLayout: array of TRenderBoxLayout;
    fLayoutCount: integer;
    fOwner: TRenderPages;
    fOwnerFont: TFont;
    /// associated links: none set for 0, otherwise fLinksBookMarkName[number-1]
    fLinksBookMarkNameCurrent: integer;
    fLinksBookMarkName: array of string;
    /// populate fLayout[] from fBox[] and calculate fHeight
    procedure InternalRender;
    function GetHeight: integer;
    procedure Clear;
  public
    /// initialize the rendering "box"
    constructor Create(Owner: TRenderPages);
    /// add some text at the current position
    // - the text is converted to Unicode before adding (calling
    // Owner.OnStringToUnicode if was defined) 
    // - the current Owner Font settings are used for the rendering
    // - warning: this method won't handle control chars (like #13 or #10), but
    // will replace them with a space: it's about the caller to
    procedure AddText(const s: string); overload;
    /// add some text at the current position
    // - the current Owner Font settings are used for the rendering
    // - warning: this method won't handle control chars (like #13 or #10), but
    // will replace them with a space: it's about the caller to
    procedure AddText(PW: PWideChar; PWLen: integer); overload;
    /// format the already inserted text into the TRenderPages owner
    // - this TRenderBox text content will be cleared at the end of this method
    // - you don't have to call it usualy: use Owner.RdrParagraph instead
    // - by default, will render top aligned to the X=Left/Y=Top pixels position
    // - for vertical alignment, specify an height in ForcedHeightBottomCentered
    // then will be centered if ForcedAtBottom=false, or bottom aligned if true
    // - if CurrentPageOnly is true, will only flush the content which will fit on
    // the current page - the fLayout[] array will contain remaining boxes;
    // - if CurrentPageOnly is false, this will flush all content to multiple pages
    procedure Flush(Left, Top: Integer; CurrentPageOnly: boolean;
      ForcedHeightBottomCentered: Integer; ForcedAtBottom: boolean);
    /// render the text paragraph, but go to the next line
    // - similar to the <br /> HTML tag or the \line RTF command
    procedure NewLine;
    /// mark that an hyperlink must begin at the current position
    // - use e.g. RdrAddText method to add some text for the link
    // - will cancel any previous LinkBegin with no LinkEnd: i.e. no nested
    // links are handled yet (how would want it anyway, in the HTML world?)
    procedure LinkBegin(const aBookmarkName: string);
    /// mark that an hyperlink must begin at the current position
    // - use e.g. RdrAddText method to add some text for the link
    // - return false on error (e.g. no hyperlink previously opened via LinkBegin)
    function LinkEnd: boolean;
    /// reset font (character) formatting properties to a default value
    // - default value have been set by RdrSetCurrentStateAsDefault
    // - if no previous call to RdrSetCurrentStateAsDefault has been made,
    // the font is reset to a 12 point, with no bold/italic/underline attributes
    // - similar to the \plain RTF command
    procedure Plain; {$ifdef HASINLINE}inline;{$endif}
    /// reset paragraph formatting properties to a default value
    // - similar to the \pard RTF command
    procedure Pard; {$ifdef HASINLINE}inline;{$endif}
    /// reset both paragraph and font formatting properties to a default value
    // - similar to the \pard\plain RTF command
    procedure PardPlain; {$ifdef HASINLINE}inline;{$endif}
    /// shortcut to the owner TRenderPages
    property Owner: TRenderPages read fOwner;
    /// shortcut to the owner TRenderPages.Font
    property Font: TFont read fOwnerFont;
    /// specifies the reading order (bidirectional mode) of the box
    // - only bdLeftToRight and bdRightToLeft are handled
    property BiDiMode: TBiDiMode read FBiDiMode write FBiDiMode;
    /// current width of the "box", in pixels
    // - must be set before any call to InternalRender
    property Width: integer read fWidth write fWidth;
    /// current resulting height of the "box", in pixels
    // - will be calculated from current text if necessary
    property Height: integer read GetHeight;
  end;

  /// Report class specified in high-quality document rendering
  // - this class add some methods for creating a document at the character
  // level (whereas standard TGDIPages allows reporting at paragraph level)
  // - can be used e.g. to render some RTF-like content
  // - column handling is much more sophisticated than AddColumn*() methods
  // - uses the Windows Uniscribe API to handle right-to-left scripting and
  // process complex scripts (like Arabic)
  // - uses internaly some TeX-like algorithms like widows and orphans, and
  // an optional external hyphenation engine (like our hyphen unit)
  TRenderPages = class(TGDIPages)
  protected
    fParagraphFirstLineIndent: Integer;
    fParagraphBefore: Integer;
    fParagraphAfter: Integer;
    fParagraphRightIndent: Integer;
    fParagraphLeftIndent: Integer;
    fSavedRender: array of TSavedStateRender;
    fDefaultState: TSavedState;
    fDefaultStateRender: TSavedStateRender;
    fRdr: TRenderBox;
    fRdrCol: TObjectList;
    /// an array of TFont, used as cache
    fFontCache: TObjectList;
    fFontCacheSpace: array of TSize;
    procedure RdrPard;
    procedure RdrPardPlain;
    procedure RdrPlain;
    function GetCurrentFontCacheIndex: integer;
    function GetCurrentFontCacheIndexAndSelect: integer;
    function GetSavedRender: TSavedStateRender;
    procedure SetSavedRender(const State: TSavedStateRender);
    /// will close any pending paragraph (\page makes an implicit \par)
    procedure NewPageInternal; override;
  public
    /// will set the current Font and Paragraph properties to be used as default
    // - will be used by RdrPlain and RdrPard methods
    procedure RdrSetCurrentStateAsDefault;
    /// render the text paragraph, and begin a new one
    // - write the paragraph text as specified by all previous calls to the
    // Rdr TRenderBox methods, and begin a new paragraph, using a cleaned
    // TRenderBox instance
    // - will use the current TextAlign property value, and the current value
    // of all Paragraph* properties of this class
    // - similar to the </p> HTML tag or the \par RTF command
    procedure RdrParagraph;
    /// create a new table at the current position
    // - return false on error (e.g. a table was opened but not yet ended)
    function RdrTableBegin(const PercentWidth: array of integer): Boolean;
    /// get a particular column
    // - return the 'box' handling the layout of the column: use its
    // AddText/NewLine/Link*/Plain/Pard methods  methods to add some formatted text
    function RdrTableColumn(aColumnIndex: Integer): TRenderBox; {$ifdef HASINLINE}inline;{$endif}
    /// end a previously opened table
    // - will draw all columns to the documents
    // - return false on error (e.g. a table was not opened)
    function RdrTableEnd: Boolean;
    /// the main paragraph 'box' of the document
    // - its AddText/NewLine/Link*/Plain/Pard methods  methods to add some
    // formatted text
    // - the paragraph will be flushed to the main document with the RdrParagraph
    // method will be called
    property Rdr: TRenderBox read fRdr;
  public { some overriden methods }
    /// creates the reporting component
    constructor Create(AOwner: TComponent); override;
    /// finalize the component, releasing all used memory and associated TRenderBox
    destructor Destroy; override;
    /// Clear the current Report document
    procedure Clear; override;
    /// save the current font and alignment
    // - similar to a { character in some RTF content
    // - this version will save also Paragraph* properties values
    procedure SaveLayout; override;
    /// restore last saved font and alignment
    // - similar to a } character in some RTF content
    // - this version will restore also Paragraph* properties values
    procedure RestoreSavedLayout; override;
  public
    /// current paragraph "space before" spacing (in mm, the default is 0)
    property ParagraphBefore: Integer read fParagraphBefore write fParagraphBefore;
    /// current paragraph "space after" spacing (in mm, the default is 0)
    property ParagraphAfter: Integer read fParagraphAfter write fParagraphAfter;
    /// current paragraph first-line indent (in mm, the default is 0)
    property ParagraphFirstLineIndent: Integer
      read fParagraphFirstLineIndent write fParagraphFirstLineIndent;
    /// current paragraph left indent (in mm, the default is 0)
    property ParagraphLeftIndent: Integer
      read fParagraphLeftIndent write fParagraphLeftIndent;
    /// current paragraph right indent (in mm, the default is 0)
    property ParagraphRightIndent: Integer
      read fParagraphRightIndent write fParagraphRightIndent;
  end;
{$endif RENDERPAGES}

resourcestring
  sPDFFile = 'Acrobat File';
  sPageN = 'Page %d / %d';
  /// used to create the popup menu of the report
  sReportPopupMenu1 = '&Next page,&Previous page,&Go to Page...,&Zoom...,'+
    '&Bookmarks,Copy Page as &Text,P&rint,PDF &Export,&Close,Page fit,Page width';
  /// used to create the pages browsing menu of the report
  sReportPopupMenu2 = 'Pages %d to %d,Page %d';

type
  /// the available menu items
  TReportPopupMenu = (
    rNone, rNextPage, rPreviousPage, rGotoPage, rZoom, rBookmarks,
    rPageAsText, rPrint, rExportPDF, rClose);

const
  /// minimum gray border with around preview page
  GRAY_MARGIN = 10;

  //preview page zoom options...
  PAGE_WIDTH = -1;
  PAGE_FIT   = -2;

  //TEXT FORMAT FLAGS...
  FORMAT_DEFAULT    = $0;
  //fontsize bits 0-7  .'. max = 255
  FORMAT_SIZE_MASK  = $FF;
  //alignment bits 8-9
  FORMAT_ALIGN_MASK = $300;
  FORMAT_LEFT       = $0;
  FORMAT_RIGHT      = $100;
  FORMAT_CENTER     = $200;
  FORMAT_JUSTIFIED  = $300;
  //fontstyle bits 10-12
  FORMAT_BOLD       = $400;
  FORMAT_UNDERLINE  = $800;
  FORMAT_ITALIC     = $1000;
  //undefined bit 13
  FORMAT_UNDEFINED  = $2000;
  //line flags bits 14-15
  FORMAT_SINGLELINE = $8000;
  FORMAT_DOUBLELINE = $4000;
  FORMAT_LINES      = $C000;
  //DrawTextAt XPos 16-30 bits  (max value = ~64000)
  FORMAT_XPOS_MASK  = $FFFF0000;

  PAPERSIZE_A4_WIDTH = 210;
  PAPERSIZE_A4_HEIGHT = 297;

procedure SetCurrentPrinterAsDefault;
function CurrentPrinterName: string;
function CurrentPrinterPaperSize: string;
procedure UseDefaultPrinter;

procedure Register;


implementation

uses
  Types, Clipbrd, Consts;

type
  //TZStrings: used by ColumnHeaderList to store #0 terminated char arrays
  //eg: A column header row might look like - 'Column One'#0'Column Two'#0
  TZStrings = SynUnicode;


// Miscellaneous functions ...

function TextExtent(Canvas: TCanvas; const Text: SynUnicode; Len: integer=0): TSize;
begin
  Result.cX := 0;
  Result.cY := 0;
  if Len=0 then
    Len := length(Text);
  GetTextExtentPoint32W(Canvas.Handle, pointer(Text), Len, Result);
end;

function TextWidthC(Canvas: TCanvas; const Text: SynUnicode): Integer;
begin
  Result := TextExtent(Canvas,Text).cX;
end;

procedure TextOut(Canvas: TCanvas; X,Y: integer; Text: PWideChar; Len: integer); overload;
begin
  ExtTextOutW(Canvas.Handle,X,Y,Canvas.TextFlags,nil,Text,Len,nil);
end;

procedure TextOut(Canvas: TCanvas; X,Y: integer; const Text: SynUnicode); overload;
begin
  ExtTextOutW(Canvas.Handle,X,Y,Canvas.TextFlags,nil,pointer(Text),Length(Text),nil);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TGDIPages]);
end;

function ConvertNegsToParentheses(const ValStr: SynUnicode): SynUnicode;
begin
  result := ValStr;
  if (result = '') or (result[1] <> '-') then
    exit;
  result[1] := '(';
  result := result+')';
end;

function PrinterDriverExists: boolean;
var Flags, Count, NumInfo: dword;
    Level: Byte;
begin
  //avoid using fPrinter.printers.Count as this will raise an
  //exception if no printer driver is installed...
  Count := 0;
  try
    if Win32Platform = VER_PLATFORM_WIN32_NT then begin
      Flags := PRINTER_ENUM_CONNECTIONS or PRINTER_ENUM_LOCAL;
      Level := 4;
    end else begin
      Flags := PRINTER_ENUM_LOCAL;
      Level := 5;
    end;
    EnumPrinters(Flags, nil, Level, nil, 0, Count, NumInfo);
  except
  end;
  result := (count > 0);
end;

function RightTrim(const S: SynUnicode): SynUnicode;
var i: integer;
begin
  i := Length(s);
  while (i > 0) and (ord(S[i])<=32) do dec(i);
  SetString(result,PWideChar(pointer(S)),i);
end;

function LowerCaseU(const S: SynUnicode): SynUnicode;
var i: integer;
begin
  SetString(result,PWideChar(pointer(S)),length(S));
  for i := 0 to length(S)-1 do
    if PWordArray(result)[i] in [ord('A')..ord('Z')] then
      dec(PWordArray(result)[i],32);
end;

function Max(a,b: integer): integer;
begin
  if a > b then
    result := a else
    result := b;
end;

function Min(a,b: integer): integer;
begin
  if a < b then
    result := a else
    result := b;
end;

procedure UseDefaultPrinter;
begin
  Printers.Printer.PrinterIndex := -1;
end;

function GetDefaultPrinterName: string;
var Device : array[byte] of char;
    p,p2: PChar;
begin
  GetProfileString('windows', 'device', '', Device, 255);
  p2 := Device;
  while p2^ = ' ' do inc(p2);
  p := p2;
  while not (ord(p2^) in [0,ord(',')]) do inc(p2);
  SetLength(result, p2 - p);
  if p2 > p then
   move(p^, pointer(result)^, p2 - p);
end;

function GetDriverForPrinter(Device: PChar; Driver: PChar): boolean;
var
  PrintHandle: THandle;
  DriverInfo2: PDriverInfo2;
  cnt: dword;
  DriverPath: string;
begin
  result := false;
  if not OpenPrinter(Device,PrintHandle, nil) then exit;
  try
    getmem(DriverInfo2,1024);
    try
      if GetPrinterDriver(PrintHandle, nil, 2, DriverInfo2, 1024, cnt) then
      begin
        DriverPath :=
          changefileext(extractfilename(DriverInfo2.pDriverPath),'');
        strpcopy(Driver, DriverPath);
        result := true;
      end;
    finally
      freemem(DriverInfo2);
    end;
  finally
    ClosePrinter(PrintHandle);
  end;
end;

procedure SetCurrentPrinterAsDefault;
var Device : array[byte] of char;
  Driver : array[byte] of char;
  Port  : array[byte] of char;
  DefaultPrinter: string;
  hDeviceMode: THandle;
begin
  DefaultPrinter := GetDefaultPrinterName;
  Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
  if DefaultPrinter = Device then exit;
  if (Driver[0] = #0) then
    if not GetDriverForPrinter(Device, Driver) then exit;  //oops !
  DefaultPrinter := format('%s,%s,%s',[Device, Driver, Port]);
  WriteProfileString( 'windows', 'device', pointer(DefaultPrinter) );
  Device := 'windows';
  SendMessage( HWND_BROADCAST, WM_WININICHANGE, 0, integer( @Device ));
end;

function CurrentPrinterName: string;
var Device : array[byte] of char;
    Driver : array[byte] of char;
    Port  : array[byte] of char;
    hDeviceMode: THandle;
begin
  Printer.GetPrinter(Device, Driver, Port, hDeviceMode);
  result := trim(Device);
end;


function CurrentPrinterPaperSize: string;
var PtrHdl: THandle;
    PtrPPI: TPoint;
    size: TSize;
begin
  try
    PtrHdl := Printer.Handle;
    PtrPPI.x := GetDeviceCaps(PtrHdl, LOGPIXELSX);
    PtrPPI.y := GetDeviceCaps(PtrHdl, LOGPIXELSY);
    size.cx := MulDiv(GetDeviceCaps(PtrHdl, PHYSICALWIDTH), 254,PtrPPI.x *10);
    size.cy := MulDiv(GetDeviceCaps(PtrHdl, PHYSICALHEIGHT), 254,PtrPPI.y *10);
  except
  end;
  with size do
  begin
    if cx > cy then
    begin
      //landscape ...
      case cy of
        148: if (cx = 210) then result := 'A5 (210 x 148mm)';
        210: if (cx = 297) then result := 'A4 (297 x 210mm)';
        216: if (cx = 279) then result := 'Letter (11 x 8½")'
             else if (cx = 356) then result := 'Legal (14 x 8½")';
        297: if (cx = 420) then result := 'A3 (420 x 297mm)';
      end;
    end else
    begin
      //portrait ...
      case cx of
        148: if (cy = 210) then result := 'A5 (148 x 210mm)';
        210: if (cy = 297) then result := 'A4 (210 x 297mm)';
        216: if (cy = 279) then result := 'Letter (8½ x 11")'
             else if (cy = 356) then result := 'Legal (8½ x 14")';
        297: if (cy = 420) then result := 'A3 (297 x 420mm)';
      end;
    end;
    if result = '' then result := format('Custom (%d x %dmm)',[cx, cy]);
  end;
end;


//This declaration modifies Delphi's declaration of GetTextExtentExPoint
//so that the variable to receive partial string extents (p6) is ignored ...
function GetTextExtentExPointNoPartialsW(DC: HDC; p2: PChar; p3, p4: Integer;
  var p5: Integer; const p6: integer; var p7: TSize): BOOL; stdcall;
    external gdi32 name 'GetTextExtentExPointW';

//TrimLine: Splits off from LS any characters beyond the allowed width
//breaking at the end of a word if possible. Leftover chars -> RS.
procedure TrimLine(Canvas: TCanvas; var ls: SynUnicode; out rs: SynUnicode;
                           LineWidthInPxls: integer);
var i,len,NumCharWhichFit: integer;
    dummy: TSize;
begin
  len := length(ls);
  if len = 0 then exit;

  // get the number of characters which will fit within LineWidth...
  if not GetTextExtentExPointNoPartialsW(Canvas.Handle,
    pointer(ls),len,LineWidthInPxls,NumCharWhichFit,0,dummy) then
      raise Exception.create('GetTextExtentExPoint WinApi error in TGDIPages');

  if NumCharWhichFit = len then exit; //if everything fits then stop here

  i := NumCharWhichFit;
  //find the end of the last whole word which will fit...
  while (NumCharWhichFit > 0) and (ls[NumCharWhichFit] > ' ') do
    dec(NumCharWhichFit);
  if (NumCharWhichFit = 0) then NumCharWhichFit := i;
  
  i := NumCharWhichFit+1;
  //ignore trailing blanks in LS...
  while (ls[NumCharWhichFit] = ' ') do dec(NumCharWhichFit);
  //ignore beginning blanks in RS...
  while (i < len) and (ls[i] = ' ') do inc(i);
  rs := copy(ls,i,len);
  ls := copy(ls,1,NumCharWhichFit);        //nb: assign ls AFTER rs here
end;


procedure PrintBitmap(Canvas: TCanvas; DestRect: TRect; Bitmap: TBitmap);
var BitmapHeader:  pBitmapInfo;
    BitmapImage :  POINTER;
    HeaderSize  :  dword;
    ImageSize   :  dword;
begin
  // we expect the bitmap to be stored as DIB in the TMetaFile content
  GetDIBSizes(Bitmap.Handle,HeaderSize,ImageSize);
  GetMem(BitmapHeader,HeaderSize);
  GetMem(BitmapImage,ImageSize);
  try
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
    // will create a EMR_STRETCHDIBITS record, ready for SynPdf and SynGdiPlus 
    StretchDIBits(Canvas.Handle,
                  DestRect.Left, DestRect.Top,     // Destination Origin
                  DestRect.Right  - DestRect.Left, // Destination Width
                  DestRect.Bottom - DestRect.Top,  // Destination Height
                  0,0,                             // Source Origin
                  Bitmap.Width, Bitmap.Height,     // Source Width & Height
                  BitmapImage,
                  TBitmapInfo(BitmapHeader^),
                  DIB_RGB_COLORS,
                  SRCCOPY);
  finally
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  end;
end;


//This DrawArrow() function is based on code downloaded from
//http://www.efg2.com/Lab/Library/Delphi/Graphics/Algorithms.htm
//(The original author is unknown)
procedure DrawArrowInternal(Canvas: TCanvas;
  FromPoint, ToPoint: TPoint; HeadSize: integer; SolidArrowHead: boolean);
var
  xbase           :  integer;
  xLineDelta      :  integer;
  xLineUnitDelta  :  Double;
  xNormalDelta    :  integer;
  xNormalUnitDelta:  Double;
  ybase           :  integer;
  yLineDelta      :  integer;
  yLineUnitDelta  :  Double;
  yNormalDelta    :  integer;
  yNormalUnitDelta:  Double;
  SavedBrushColor :  TColor;
begin
  with FromPoint do Canvas.MoveTo(x,y);
  with ToPoint do Canvas.LineTo(x,y);

  xLineDelta := ToPoint.X - FromPoint.X;
  yLineDelta := ToPoint.Y - FromPoint.Y;

  xLineUnitDelta := xLineDelta / SQRT( SQR(xLineDelta) + SQR(yLineDelta) );
  yLineUnitDelta := yLineDelta / SQRt( SQR(xLineDelta) + SQR(yLineDelta) );

  //(xBase,yBase) is where arrow line is perpendicular to base of triangle.
  xBase := ToPoint.X - ROUND(HeadSize * xLineUnitDelta);
  yBase := ToPoint.Y - ROUND(HeadSize * yLineUnitDelta);

  xNormalDelta :=  yLineDelta;
  yNormalDelta := -xLineDelta;
  xNormalUnitDelta := xNormalDelta / SQRT( SQR(xNormalDelta) + SQR(yNormalDelta) );
  yNormalUnitDelta := yNormalDelta / SQRt( SQR(xNormalDelta) + SQR(yNormalDelta) );

  SavedBrushColor := Canvas.Brush.Color;
  if SolidArrowHead then
    Canvas.Brush.Color := Canvas.Pen.Color;
  Canvas.Polygon([ToPoint,
    Point(xBase + ROUND(HeadSize*xNormalUnitDelta),
      yBase + ROUND(HeadSize*yNormalUnitDelta)),
    Point(xBase - ROUND(HeadSize*xNormalUnitDelta),
      yBase - ROUND(HeadSize*yNormalUnitDelta)) ]);
  Canvas.Brush.Color := SavedBrushColor;
end;



{ TPagePaintBox }

procedure TPagePaintBox.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1; // no erasing is necessary after this method call
end;


{ TGDIPages }

procedure TGDIPages.GetPrinterParams;
var i: integer;
begin
  if Self=nil then exit;
  if not fForceScreenResolution and fHasPrinterInstalled then
    try
      fCurrentPrinter := CurrentPrinterName;
      if (Printer.orientation <> fOrientation) then
        Printer.orientation := fOrientation;
      fPtrHdl := Printer.Handle;
      fPrinterPxPerInch.x := GetDeviceCaps(fPtrHdl, LOGPIXELSX);
      fPrinterPxPerInch.y := GetDeviceCaps(fPtrHdl, LOGPIXELSY);
      fPhysicalSizePx.x := GetDeviceCaps(fPtrHdl, PHYSICALWIDTH);
      fPhysicalOffsetPx.x := GetDeviceCaps(fPtrHdl,PHYSICALOFFSETX);
      fPhysicalSizePx.y := GetDeviceCaps(fPtrHdl, PHYSICALHEIGHT);
      fPhysicalOffsetPx.y := GetDeviceCaps(fPtrHdl,PHYSICALOFFSETY);
      fDefaultLineWidth := fPrinterPxPerInch.y div screen.pixelsperinch;
      //fDefaultLineWidth = 0.25 mm
      fDefaultLineWidth := (fPrinterPxPerInch.y*25) div 2540;
      exit; // if a printer was found then that's all that's needed
    except
      fHasPrinterInstalled := false;
    end;

  // ForceScreenResolution or no Printer: use screen resolution
  if fHasPrinterInstalled then begin
    if (Printer.orientation <> fOrientation) then
      Printer.orientation := fOrientation;
    fPtrHdl := printer.Handle;
    fPhysicalSizePx.X := round(GetDeviceCaps(fPtrHdl, PHYSICALWIDTH) *
        screen.pixelsperinch / GetDeviceCaps(fPtrHdl, LOGPIXELSX));
    fPhysicalSizePx.Y := round(GetDeviceCaps(fPtrHdl, PHYSICALHEIGHT) *
        screen.pixelsperinch / GetDeviceCaps(fPtrHdl, LOGPIXELSY));
  end else begin
    // if no printer drivers installed use the screen as device context and
    // assume A4 page size...
    fPtrHdl := 0; //GetDC(0);
    fPhysicalSizePx.X := MulDiv(PAPERSIZE_A4_WIDTH*10,screen.pixelsperinch,254);
    fPhysicalSizePx.Y := MulDiv(PAPERSIZE_A4_HEIGHT*10,screen.pixelsperinch,254);
  end;
  //assume 6mm non-printing offsets...
  fPhysicalOffsetPx.X := MulDiv(60,screen.pixelsperinch,254);
  fPhysicalOffsetPx.Y := MulDiv(60,screen.pixelsperinch,254);
  fPrinterPxPerInch.X := screen.pixelsperinch;
  fPrinterPxPerInch.Y := screen.pixelsperinch;
  //fDefaultLineWidth ==> 0.3 mm
  fDefaultLineWidth := (fPrinterPxPerInch.y*3) div 254;
  if not fHasPrinterInstalled and (fOrientation = poLandscape) then begin
    // no Printer.Orientation -> swap width & height if Landscape page layout
    i := fPhysicalSizePx.x;
    fPhysicalSizePx.x := fPhysicalSizePx.y;
    fPhysicalSizePx.y := i;
  end;
end;

function TGDIPages.PrinterPxToScreenPxX(PrinterPx: integer): integer;
begin
  if (Self=nil) or (fPrinterPxPerInch.x=0) then
    result := 0 else
    result := (PrinterPx*screen.pixelsperinch*fZoom) div (fPrinterPxPerInch.x*100);
end;

function TGDIPages.PrinterPxToScreenPxY(PrinterPx: integer): integer;
begin
  if (Self=nil) or (fPrinterPxPerInch.y=0) then
    result := 0 else
    result := (PrinterPx*screen.pixelsperinch*fZoom) div (fPrinterPxPerInch.y*100);
end;

function TGDIPages.MmToPrinterPxX(mm: integer): integer;
begin
  if Self=nil then
    result := 0 else
    result := ((mm*10) * fPrinterPxPerInch.x) div 254;
end;

function TGDIPages.MmToPrinterPxY(mm: integer): integer;
begin
  if Self=nil then
    result := 0 else
    result := ((mm*10) * fPrinterPxPerInch.y) div 254;
end;

function TGDIPages.PrinterPxToMmX(px: integer): integer;
begin
  if (Self=nil) or (fPrinterPxPerInch.x=0) then
    result := 0 else
    result := (px*254) div (fPrinterPxPerInch.x*10);
end;


function TGDIPages.PrinterPxToMmY(px: integer): integer;
begin
  if (Self=nil) or (fPrinterPxPerInch.y=0) then
    result := 0 else
    result := (px*254) div (fPrinterPxPerInch.y*10);
end;


procedure TGDIPages.ResizeAndCenterPaintbox;
var w,h,l,t, i: integer;
begin
  // center the paintbox according to the new size
  with fPreviewSurface do begin
    w := PrinterPxToScreenPxX(fPhysicalSizePx.x)+GRAY_MARGIN*2;
    h := PrinterPxToScreenPxY(fPhysicalSizePx.y)+GRAY_MARGIN*2;
    l := Max((Self.ClientWidth - w) div 2,0) - HorzScrollbar.Position;
    t := Max((Self.ClientHeight - h) div 2,0) - VertScrollbar.Position;
    SetBounds(l,t,w,h);
  end;
  // resize any hot link
  for i := 0 to fLinks.Count-1 do
    TGDIPagereference(fLinks.Objects[i]).ToPreview(Self);
end;

function TGDIPages.GetOrientation: TPrinterOrientation;
begin
  if (Self=nil) or (fPhysicalSizePx.x > fPhysicalSizePx.y) then
    result := poLandscape else
    result := poPortrait;
end;

procedure TGDIPages.SetTextAlign(Value: TTextAlign);
begin
  if Self<>nil then
    fAlign := Value;
end;

procedure TGDIPages.SetOrientation(orientation: TPrinterOrientation);
begin
  if Self<>nil then
    // nb: it's not possible to change the orientation once a report has started
    fOrientation := orientation;
end;

procedure TGDIPages.SetPage(NewPreviewPage: integer);
begin
  if Self=nil then exit;
  if (NewPreviewPage > Pages.Count) then
    NewPreviewPage := Pages.Count else
  if (NewPreviewPage < 1) then
    NewPreviewPage := 1;
  if (Pages.Count = 0) or (fCurrPreviewPage = NewPreviewPage) then
    exit;
  fCurrPreviewPage := NewPreviewPage;
  fLinksCurrent := -1;
  FreeAndNil(PreviewSurfaceBitmap); // force double buffering Bitmap recreate
  PreviewPaint(Self);
  if Assigned(fPreviewPageChangedEvent) then
    fPreviewPageChangedEvent(Self);
  if PreviewForm<>nil then begin
    PreviewPageCountLabel.Caption := format(sPageN,[Page,PageCount]);
    PreviewButtons[ord(rNextPage)-1].Enabled := Page<PageCount;
    PreviewButtons[ord(rPreviousPage)-1].Enabled := Page>1;
  end;
end;

function TGDIPages.GetPageCount: integer;
begin
  if Self=nil then
    result := 0 else
    result := Pages.Count;
end;

function TGDIPages.GetLineHeight: integer;
var tm: TTextMetric;
    DC: HDC;
begin
  if Self=nil then begin
    result := 0;
    exit;
  end;
  if fLineHeight = 0 then begin
    if not Assigned(fCanvas) then begin
      // if no current fCanvas: use the Screen resolution (very fast)
      DC := GetDC(0);
      GetTextMetrics(DC,tm);
      ReleaseDC(0,DC);
    end else
      GetTextMetrics(fCanvas.Handle,tm);
    fLineHeight := tm.tmHeight+tm.tmInternalLeading+tm.tmExternalLeading;
  end;
  if fInHeaderOrFooter then
    result := fLineHeight else
    case fLineSpacing of
      lsSingle:     result := fLineHeight;
      lsOneAndHalf: result := (fLineHeight*3) shr 1;
      else          result := fLineHeight*2;
    end;
end;

function TGDIPages.GetLineHeightMm: integer;
begin
  if Self=nil then
    result := 0 else
    result := PrinterPxToMmY(GetLineHeight);
end;

procedure TGDIPages.CheckHeaderDone;
begin
  if not fHeaderDone then
    DoHeader;
end;

procedure TGDIPages.CheckYPos;
begin
  if Self=nil then exit;
  if fInHeaderOrFooter then exit;
  CheckHeaderDone;
  if not HasSpaceForLines(1) then begin
    NewPageInternal;
    // nb: header is done inside a group, so we must check for it
    CheckHeaderDone;
  end;
end;

function TGDIPages.GetYPos: integer;
begin
  if (Self=nil) or (fPrinterPxPerInch.y=0) then
    result := 0 else
    result := (fCurrentYPos*254) div (fPrinterPxPerInch.y*10);
end;

procedure TGDIPages.SetYPos(YPos: integer);
begin
  if Self=nil then exit;
  if fCurrentYPos >= fPhysicalSizePx.y then
    NewPageInternal;
  fCurrentYPos := MmToPrinterPxY(YPos);
end;

function TGDIPages.GetSavedState: TSavedState;
begin
  with result do begin
    Flags := TextFormatsToFlags;
    FontName := Font.Name;
    FontColor := Font.Color;
    LeftMargin := fPageMarginsPx.Left;
    RightMargin := fPageMarginsPx.Right;
    BiDiMode := fBiDiMode;
  end;
end;

procedure TGDIPages.SetSavedState(const SavedState: TSavedState);
begin
  with SavedState do begin
    SetFontWithFlags(Flags);
    Font.Name := FontName;
    Font.Color := FontColor;
    fPageMarginsPx.Left := LeftMargin;
    fPageMarginsPx.Right := RightMargin;
    fBiDiMode := BiDiMode;
  end;
end;

procedure TGDIPages.SaveLayout;
begin
  if Self=nil then exit; // avoid GPF
  if fSavedCount>=length(fSaved) then
    SetLength(fSaved,fSavedCount+20);
  fSaved[fSavedCount] := SavedState;
  inc(fSavedCount);
end;

procedure TGDIPages.RestoreSavedLayout;
begin
  if Self=nil then exit; // avoid GPF
  if fSavedCount<=0 then
    exit;
  dec(fSavedCount);
  SavedState := fSaved[fSavedCount];
end;

function TGDIPages.CreateMetaFile(aWidth, aHeight: integer): TMetaFile;
begin
  result := TMetafile.Create;
  if Self=nil then exit; 
  result.Width := aWidth;
  result.Height := aHeight;
end;

procedure TGDIPages.NewPageInternal;
var n: integer;
  UsedGroupSpace: integer;
  NewPage: TMetafile;
  InGroup: boolean;
  GroupText: string;
begin
  if Self=nil then exit;
  UsedGroupSpace := 0; //stops a warning
  InGroup := Assigned(fGroupPage);
  if InGroup then begin // close the Group Canvas
    UsedGroupSpace := fCurrentYPos;
    FreeAndNil(fCanvas); // now recreate/redraw a fresh fCanvas for DoFooter
    fCanvas := CreateMetafileCanvas(TMetaFile(Pages.Objects[Pages.Count-1]));
    fCanvas.Draw(0,0,TMetaFile(Pages.Objects[Pages.Count-1])); // re-draw last page
    GroupText := fCanvasText;
    fCanvasText := fBeforeGroupText;
  end;
  DoFooter;
  //create a new metafile and its canvas ...
  if Assigned(fCanvas) then
     FreeAndNil(fCanvas);
  n := Pages.Count;
  if n>0 then
    Pages[n-1] := fCanvasText; // update page text 
  NewPage := CreateMetaFile(fPhysicalSizePx.x,fPhysicalSizePx.y);
  //NewPage.MMWidth := (fPhysicalSizePx.x*2540) div fPrinterPxPerInch.x;
  //NewPage.MMHeight := (fPhysicalSizePx.y*2540) div fPrinterPxPerInch.y;
  Pages.AddObject('',NewPage);
  fCanvas := CreateMetafileCanvas(NewPage);
  fCanvasText := '';
  inc(fVirtualPageNum);
  fCurrentYPos := fPageMarginsPx.top;
  if Assigned(fStartNewPage) then
    fStartNewPage(Self,Pages.Count);
  fHeaderDone := false;
  fColumnHeaderPrinted := false; // when next col. started add header
  if InGroup then begin // draw the group at the begining of new page + EndGroup
    DoHeader;
    if fColumnsUsedInGroup then begin
      //The next line is a workaround to stop an endless loop. CheckYPos (called
      //via PrintColumnHeaders) thinks we're still drawing on fGroupPage as it's
      //still Assigned so can flag "out of room" and try to create another page.
      fGroupVerticalSpace := fPhysicalSizePx.y;
      if not fColumnHeaderInGroup then
        PrintColumnHeaders else
        fColumnHeaderPrinted := true;
    end;
    fCanvas.Draw(0,fCurrentYPos,fGroupPage);
    FreeAndNil(fGroupPage); // idem as EndGroup
    inc(fCurrentYPos,UsedGroupSpace);
    fCanvasText := fCanvasText+GroupText;
  end;
end;

function TGDIPages.CreateMetafileCanvas(Page: TMetafile): TMetafileCanvas;
begin
  result := TMetafileCanvas.Create(Page,fPtrHdl);
  if Self=nil then exit; 
  UpdateMetafileCanvasFont(result);
  result.Pen.Width := fPrinterPxPerInch.y div screen.PixelsPerInch;
end;

procedure TGDIPages.UpdateMetafileCanvasFont(aCanvas: TMetafileCanvas);
begin
  // next 2 lines are a printer bug workaround - 23Mar2000
  aCanvas.Font.Size := Font.Size+1;
  aCanvas.Font.PixelsPerInch := fPrinterPxPerInch.y;
  aCanvas.Font := Font;
end;

function TGDIPages.TextFormatsToFlags: integer;
begin
  result := min(max(font.size,4),FORMAT_SIZE_MASK); { size between 4 and 255 }
  case fAlign of
    taRight:     result := result or FORMAT_RIGHT;
    taCenter:    result := result or FORMAT_CENTER;
    taJustified: result := result or FORMAT_JUSTIFIED;
  end;
  if fsBold in font.style then
    result := result or FORMAT_BOLD;
  if fsUnderline in font.style then
    result := result or FORMAT_UNDERLINE;
  if fsItalic in font.style then
    result := result or FORMAT_ITALIC;
end;

procedure TGDIPages.SetFontWithFlags(flags: integer);
var fontstyle: TFontStyles;
begin
  if flags and FORMAT_SIZE_MASK<>Font.Size then
    Font.size := flags and FORMAT_SIZE_MASK;
  if (flags and FORMAT_BOLD) <> 0 then
    fontstyle := [fsBold] else
    fontstyle := [];
  if (flags and FORMAT_UNDERLINE) <> 0 then
    include(fontstyle,fsUnderline);
  if (flags and FORMAT_ITALIC) <> 0 then
    include(fontstyle,fsItalic);
  if Font.Style<>fontstyle then
    Font.Style := fontstyle;
  case flags and FORMAT_ALIGN_MASK of
    FORMAT_RIGHT:     falign := taRight;
    FORMAT_CENTER:    falign := taCenter;
    FORMAT_JUSTIFIED: falign := taJustified;
    else              falign := taLeft;
  end;
end;

function TGDIPages.HasSpaceForLines(Count: integer): boolean;
begin
  if Self=nil then
    result := false else // avoid GPF
  if Assigned(fGroupPage) then
    result := fCurrentYPos + GetLineHeight*Count < fGroupVerticalSpace else
    result := fCurrentYPos + GetLineHeight*Count <
      fPhysicalSizePx.y - fPageMarginsPx.bottom - fFooterHeight;
end;

function TGDIPages.HasSpaceFor(mm: integer): boolean;
begin
  if Self=nil then
    result := false else begin // avoid GPF
    mm := fCurrentYPos + MmToPrinterPxY(mm);
    if Assigned(fGroupPage) then
      result := mm < fGroupVerticalSpace else
      result := mm < fPhysicalSizePx.y - fPageMarginsPx.bottom - fFooterHeight;
  end;
end;

procedure TGDIPages.DoHeader;
begin
  fHeaderDone := true;
  if (fHeaderLines.Count = 0) then exit;
  SaveLayout;
  if Assigned(fStartPageHeader) then
    fStartPageHeader(Self);
  Font.Color := clBlack;
  DoHeaderFooterInternal(fHeaderLines);
  if Assigned(fEndPageHeader) then
    fEndPageHeader(Self);
  GetLineHeight;
  inc(fCurrentYPos,fLineHeight shr 2); // add a small header gap
  fHeaderHeight := fCurrentYPos-fPageMarginsPx.Top;
  RestoreSavedLayout;
end;

procedure TGDIPages.DoFooter;
begin
  if (fFooterLines.Count = 0) then exit;
  SaveLayout;
  fCurrentYPos :=
    fPhysicalSizePx.y - fPageMarginsPx.bottom - fFooterHeight + fFooterGap;
  if Assigned(fStartPageFooter) then
    fStartPageFooter(Self);
  DoHeaderFooterInternal(fFooterLines);
  if Assigned(fEndPageFooter) then
    fEndPageFooter(Self);
  RestoreSavedLayout;
end;

procedure TGDIPages.DoHeaderFooterInternal(Lines: TObjectList);
var i: integer;
begin
  SaveLayout;
  fInHeaderOrFooter := true;
  try
    for i := 0 to Lines.Count -1 do
      with THeaderFooter(Lines[i]) do
      begin
        SavedState := State;
        PrintFormattedLine(Text, State.Flags);
      end;
  finally
    fInHeaderOrFooter := false;
    RestoreSavedLayout;
  end;
end;

procedure TGDIPages.CalcFooterGap;
begin
  GetLineHeight;
  // make sure there's a gap of at least 1/4 of a lineheight
  // between the page body and the footer ...
  fFooterGap := fLineHeight shr 2;
  fFooterHeight := fFooterGap;
end;

function TGDIPages.GetColumnRec(col: integer): TColRec;
begin
  result.ColLeft := 0;
  result.ColRight := 0;
  if Cardinal(col)<Cardinal(length(fColumns)) then
    result := fColumns[col];
end;

procedure TGDIPages.PrintColumnHeaders;
var
  i,j,SavedFontSize,FontCol: integer;
  SavedFontStyle: TFontStyles;
  SavedAlign: TTextAlign;
  SavedWordWrapLeftCols: boolean;
  headers: array[0..MAXCOLS-1] of SynUnicode;
  zStr: TZStrings;

  function GetSubstringFromStringArray(var s: TZStrings): SynUnicode;
  begin
    result := PWideChar(pointer(s)); // result := next #0 ended string in s
    delete(s,1,length(result)+1);
  end;

begin
  if (fColumnHeaderList.Count = 0) or (fColumns=nil) then exit;
  CheckYPos;

  fColumnHeaderPrinted := true;   //stops an endless loop
  SavedFontSize := Font.size;
  SavedFontStyle := font.style;
  SavedAlign := fAlign;
  SavedWordWrapLeftCols := WordWrapLeftCols;
  WordWrapLeftCols := false;

  if Assigned(fStartColumnHeader) then
    fStartColumnHeader(Self);
  FontCol := fCanvas.Font.Color;
  for i := 0 to fColumnHeaderList.Count-1 do begin
    SetFontWithFlags(integer(fColumnHeaderList.Objects[i]));
    fCanvas.Font.Color := clBlack;
    j := 0;
    zStr := fColumnHeaderList[i];
    while (j < MAXCOLS) and (zStr<>'') do begin
      headers[j] := GetSubstringFromStringArray(zStr);
      inc(j);
    end;
    fDrawTextAcrossColsDrawingHeader := true;
    DrawTextAcrossCols(slice(headers,j));
    fDrawTextAcrossColsDrawingHeader := false;
  end;
  fCanvas.Font.Color := FontCol;
  if Assigned(fEndColumnHeader) then
    fEndColumnHeader(Self);
  // add a small space below the column headers
  // inc(fCurrentYPos,fLineHeight shr 2);

  Font.Size := SavedFontSize;
  Font.Style := SavedFontStyle;
  fAlign := SavedAlign;
  WordWrapLeftCols := SavedWordWrapLeftCols;
  if Assigned(fGroupPage) then
    fColumnHeaderInGroup := true;
  fColumnHeaderPrintedAtLeastOnce :=
    ForceCopyTextAsWholeContent; // don't reproduce headers every page
end;

procedure TGDIPages.SetZoom(Zoom: integer);
var i, zoomW, zoomH: integer;
begin
  if (Self=nil) or (zoom < PAGE_FIT) or (zoom in [0..9]) or (zoom > 200) then
    exit;
  fLinksCurrent := -1;
  FreeAndNil(PreviewSurfaceBitmap);

  // ZoomStatus required when resizing...
  if zoom = PAGE_FIT then
    fZoomStatus := zsPageFit else
  if zoom = PAGE_WIDTH then
    fZoomStatus := zsPageWidth else
    fZoomStatus := zsPercent;

  if (fZoom = Zoom) or (not handleallocated) then
    exit;
  fZoom := Zoom;

  // calculate the new fZoom ...
  if zoom = PAGE_FIT then begin
    ZoomW := trunc((clientWidth-GRAY_MARGIN*2)*fPrinterPxPerInch.x*
               100/fPhysicalSizePx.x/screen.pixelsperinch);
    ZoomH := trunc((clientHeight-GRAY_MARGIN*2)*fPrinterPxPerInch.y*
               100/fPhysicalSizePx.y/screen.pixelsperinch);
    //choose the smallest of width% and height% to fit on page (but min 10%)
    fZoom := Max(Min(ZoomW,ZoomH),10);
  end else
  if zoom = PAGE_WIDTH then
    fZoom := trunc((clientWidth-GRAY_MARGIN*2)*fPrinterPxPerInch.x*
               100/fPhysicalSizePx.x/screen.pixelsperinch) else
    fZoom := Zoom;

  i := PrinterPxToScreenPxY(GetLineHeight);
  HorzScrollbar.Increment := i;
  VertScrollbar.Increment := i;

  // resize and center preview surface...
  ResizeAndCenterPaintbox;

  if Assigned(fZoomChangedEvent) then
    fZoomChangedEvent(Self, fZoom, fZoomStatus);
end;

const
  ZOOMSTEP = 20;

procedure TGDIPages.ZoomTimerInternal(X,Y: integer; ZoomIn: boolean);
var
  OldZoom: integer;
  pt: TPoint;
begin
  if (Self=nil) or (fPhysicalSizePx.x=0) or (fPhysicalSizePx.y=0) then
    Exit;
  OldZoom := fZoom;
  sendmessage(handle,WM_SETREDRAW,0,0);
  try
    if ZoomIn then begin
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
      if fZoom >= 200 then
        fZoomTimer.enabled := false else      //(maximum 200%)
{$else}if fZoom < 200 then {$endif}
        Zoom := ((fZoom + ZOOMSTEP) div ZOOMSTEP)*ZOOMSTEP;                //to nearest ZOOMSTEP%
    end else begin
      if (fZoom > 20) then
        Zoom := ((fZoom - ZOOMSTEP) div ZOOMSTEP)*ZOOMSTEP else //(minimum 20%)
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
        fZoomTimer.enabled := false;
{$endif}
    end;
    if fZoom = OldZoom then
      exit;
    // work out click pos relative to page (as x & y percentages)
    pt.x := ((X-fPreviewSurface.left-GRAY_MARGIN)*100) div PrinterPxToScreenPxX(fPhysicalSizePx.x);
    pt.x := min(max(pt.x,0),100);
    pt.y := ((Y-fPreviewSurface.top-GRAY_MARGIN)*100) div PrinterPxToScreenPxY(fPhysicalSizePx.y);
    pt.y := min(max(pt.y,0),100);
    // finally, adjust scrollbar positions based on click pos ...
    with HorzScrollbar do position := (pt.x*(range-clientwidth)) div 100;
    with VertScrollbar do position := (pt.y*(range-clientheight)) div 100;
  finally
    SendMessage(handle,WM_SETREDRAW,1,0);
  end;
  Invalidate;
end;

procedure TGDIPages.ZoomTimer(Sender: TObject);
var
  CursorPos: TPoint;
begin
  GetCursorPos(CursorPos);
  CursorPos := ScreenToClient(CursorPos);
  ZoomTimerInternal(CursorPos.x,CursorPos.y, fZoomIn);
end;

procedure TGDIPages.LineInternal(start,finish: integer; DoubleLine: boolean);
var
  Y: integer;
begin
  if (Self<>nil) and (fCanvas<>nil) then
  with fCanvas do begin
    Pen.Width := MulDiv(fDefaultLineWidth,Self.Font.Size,8);
    if fsBold in Self.Font.style then Pen.Width := Pen.Width +1;
    if DoubleLine then begin
      Y := fCurrentYPos + (GetLineHeight shr 1) - (Pen.Width);
      MoveTo(start,Y);
      LineTo(finish,Y);
      MoveTo(start,Y + Pen.Width*2);
      LineTo(finish,Y + Pen.Width*2);
    end else begin
      Y := fCurrentYPos + (GetLineHeight shr 1) - (Pen.Width shr 1);
      MoveTo(start,Y);
      LineTo(finish,Y);
    end;
  end;
end;

procedure TGDIPages.PrintFormattedLine(s: SynUnicode; flags: integer;
  const aBookmark: string; const aLink: string);
var i: integer;
    leftOffset, rightOffset: integer;
begin
  s := RightTrim(s);
  i := pos(PAGENUMBER,LowerCaseU(s));
  if i > 0 then begin
    delete(s,i,14);
    insert(UTF8ToSynUnicode(Int32ToUtf8(fVirtualPageNum)),s,i);
  end;
  if flags <> FORMAT_DEFAULT then
    SetFontWithFlags(flags);
  CheckYPos;
  fCurrentTextTop := fCurrentYPos;
  fCurrentTextPage := PageCount;
  GetTextLimitsPx(leftOffset,rightOffset);
  if flags and (FORMAT_SINGLELINE or FORMAT_DOUBLELINE)<>0 then begin
    LineInternal(leftOffset,rightOffset,flags and FORMAT_DOUBLELINE=FORMAT_DOUBLELINE);
    NewLine;
  end else
  if s = '' then
    NewLine else
  if (flags and FORMAT_XPOS_MASK <> 0) then
    DrawTextAt(s,((flags and FORMAT_XPOS_MASK) shr 16)-1) else
  if (falign in  [taLeft,taJustified]) then
    LeftOrJustifiedWrap(s) else
    RightOrCenterWrap(s);
  if aBookmark<>'' then
    AddBookMark(aBookmark,fCurrentTextTop);
  if aLink<>'' then
    AddLink(aLink,Rect(PrinterPxToMmX(leftOffset),PrinterPxToMmY(fCurrentTextTop),
      PrinterPxToMmX(rightOffset),PrinterPxToMmY(fCurrentTextTop+fLineHeight)),
      fCurrentTextPage);
    // first line of written text is added
end;

procedure TGDIPages.LeftOrJustifiedWrap(const s: SynUnicode);
var indent, leftOffset, rightOffset, LineWidth: integer;
    leftstring, rightstring: SynUnicode;
    firstLoop: boolean;
begin
  leftstring := s;
  Indent := MmToPrinterPxX(fHangIndent);
  firstLoop := true;
  repeat
    CheckYPos;
    GetTextLimitsPx(leftOffset,rightOffset);
    LineWidth := rightOffset-leftOffset;

    // offset leftOffset if hang-indenting...
    if Indent<>0 then
      if firstLoop then begin
        firstLoop := false;
        if (Indent < 0) then begin
          inc(leftOffset,-Indent);
          dec(LineWidth,-Indent);
        end;
      end else
      if (Indent > 0) and (Indent < LineWidth) then begin
        inc(leftOffset,Indent);
        dec(LineWidth,Indent);
      end;

    // dump overrun into rightstring...
    TrimLine(fCanvas,leftstring,rightstring,LineWidth);

    // HandleTabsAndPrint: prints leftstring after adjusting for tabs and
    // prepending any further text overrun into rightstring ...
    HandleTabsAndPrint(leftstring, rightstring, leftOffset, rightOffset);
    if length(rightstring)=0 then
      break;
    leftstring := rightstring;
    NewLine;
  until false;
  NewLine;
end;

procedure TGDIPages.RightOrCenterWrap(const s: SynUnicode);
var i,leftOffset,rightOffset, LineWidth: integer;
    leftstring,rightstring: SynUnicode;
    offset: integer;
begin
  leftstring := s;
  // remove tabs and replace by spaces
  i := pos(#9,leftstring);
  while i > 0 do begin
    delete(leftstring,i,1);
    insert('    ',leftstring,i);
    i := pos(#9,leftstring);
  end;
  // write text
  SetBkMode(fCanvas.Handle,TRANSPARENT);
  repeat
    GetTextLimitsPx(leftOffset,rightOffset);
    LineWidth := rightOffset-leftOffset;
    TrimLine(fCanvas,leftstring,rightstring,LineWidth);
    case falign of
      taRight:  Offset := rightOffset-TextWidthC(fCanvas,leftstring)-1;
      taCenter: Offset := leftOffset+
        (rightOffset-leftOffset-TextWidthC(fCanvas,leftstring))div 2;
      else Offset := 0; // should never happen - ?? add assert
    end;
    CheckYPos;
    TextOut(fCanvas,Offset,fCurrentYPos,leftstring);
    if length(rightstring) = 0 then break;
    leftstring := rightstring;
    NewLine;
  until false;
  NewLine;
end;

procedure TGDIPages.GetTextLimitsPx(var LeftOffset, RightOffset: integer);
begin
  // Offsets (in Printer pixels) based on current page margins
  LeftOffset := fPageMarginsPx.left;
  RightOffset := fPhysicalSizePx.x-fPageMarginsPx.right;
  if RightOffset<=LeftOffset then
    raise Exception.Create('GetTextLimitsPx: wrong margins');
end;

procedure TGDIPages.HandleTabsAndPrint(const leftstring: SynUnicode;
  var rightstring: SynUnicode; leftOffset, rightOffset: integer);
const
    // if a tabstop is very close to the right margin, it may spoil justifying...
    MIN_CHAR_WIDTH_PX = 5;
var i, spacecount, linewidth, tabPos, tabIndex, PWLen: integer;
    ls, rs: SynUnicode;
    size: TSize;
    PW: PWideChar;
begin
  // handles tabs one at a time and prints text into the available space...
  // (unfortunately there's no equivalent GetTextExtentExPoint() for tabbed text
  // and using GetTabbedTextExtent() and TabbedDrawText() instead would appear
  // to be undesirable as there's no efficient way to determine the number of
  // chars that will fit within the specified space)
  ls := leftstring;
  linewidth := rightOffset - leftOffset;
  tabPos := pos(#9,ls);
  SetBkMode(fCanvas.Handle,TRANSPARENT);
  while tabPos > 0 do begin // and still room to print
    // split line at the tab ...
    if rs <> '' then
        rs := copy(ls,tabPos+1,length(ls)) + ' '+ rs else
        rs := copy(ls,tabPos+1,length(ls));
    // add a trailing space so next the tabstop is at least one space away ...
    ls := copy(ls,1,tabPos-1)+' ';
    // get offset of next tabstop ...
    size := TextExtent(fCanvas,ls,tabPos);
    i := leftOffset + size.cx; //minimum pos for next tabstop
    tabIndex := 0;
    while tabIndex < MAXTABS do
      if fTab[tabIndex] > i then
        break else
        inc(tabIndex);
    if (tabIndex = MAXTABS) or
      (fTab[tabIndex] >= rightOffset - MIN_CHAR_WIDTH_PX) then begin
      // no tabstop found to align 'rs' to, so ...
      // rather than left aligning 'ls', remove its appended space and
      // break out ready to print it ? align left&right justified.
      SetLength(ls,length(ls)-1);
      break;
    end;
    // tabstop found so DrawText 'ls' simply left aligned ...
    TextOut(fCanvas,leftOffset,fCurrentYPos,ls);
    leftOffset := fTab[tabIndex];
    linewidth := rightOffset - leftOffset;
    ls := rs;
    TrimLine(fCanvas,ls,rs,linewidth);
    tabPos := pos(#9,ls);
  end;
  if rs <> '' then
    rightstring := rs + ' '+ rightstring;

  // OK, no TABS now in ls...
  InternalUnicodeString(ls,PW,PWLen,@size);
  // print ls into (remaining) linewidth at (leftOffset, fCurrentYPos)
  if (falign = taLeft) or (rightstring = '') then begin // left aligned
    if BiDiMode=bdRightToLeft then
      leftOffset := rightOffset-size.cx;
    TextOut(fCanvas,leftOffset,fCurrentYPos,PW,PWLen);
    // don't care about line width: it should be always equal or smaller,
    // and we are left aligned
  end else begin // justified
    spacecount := 0;
    for i := 1 to length(ls) do
      if ls[i] = ' ' then
        inc(spacecount);
    if spacecount>0 then
      SetTextJustification(fCanvas.Handle, linewidth - size.cx, spacecount);
    TextOut(fCanvas,leftOffset,fCurrentYPos,PW,PWLen);
    SetTextJustification(fCanvas.Handle,0,0);
  end;
end;

procedure TGDIPages.PreviewPaint(Sender: TObject);
var R: TRect;
    MS: TMemoryStream;
    Img: TMetaFile;
    P1,P2: TPoint;
begin
  if not Visible then begin
    FreeAndNil(PreviewSurfaceBitmap);
    exit;
  end;
  if PreviewSurfaceBitmap<>nil then
    fPreviewSurface.Canvas.Draw(0,0,PreviewSurfaceBitmap) else
  with fPreviewSurface do begin
    // paint the page white with a dark gray line around it
    R := ClientRect;
    PreviewSurfaceBitmap := TBitmap.Create;
    PreviewSurfaceBitmap.Width := R.Right;
    PreviewSurfaceBitmap.Height := R.Bottom;
    with PreviewSurfaceBitmap.Canvas do begin
      Brush.Color := Color; // background color
      FillRect(R);
      InflateRect(R,-GRAY_MARGIN,-GRAY_MARGIN);
      Brush.Color := clWhite;
      Pen.Width := 1;
      Pen.Color := clGray;
      Rectangle(R);
      Refresh;
    end;
    // draw the metafile on the page
    if (PageCount>0) and (cardinal(Page-1)<cardinal(Pages.Count)) then begin
{$ifdef GDIPLUSDRAW} // anti aliased drawing:
      if not ForceNoAntiAliased then
        DrawEmfGdip(PreviewSurfaceBitmap.Canvas.Handle,
          TMetaFile(Pages.Objects[Page-1]),R,ForceInternalAntiAliased,
          ForceInternalAntiAliasedFontFallBack) else
{$endif} begin // fast direct GDI painting, with no antialiaising:
        // PreviewSurfaceBitmap.Canvas.StretchDraw(R,TMetaFile(Pages.Objects[Page-1]))
        // is not to be used here:
        // we must use a temporary TMetaFile, otherwize the Pages[] content
        // is changed (screen dpi is changed but not reset in nested emf) and the
        // resulting report is incorrect on most printers, due to a driver bug :(
        MS := TMemoryStream.Create;
        Img := TMetaFile.Create;
        try
          TMetaFile(Pages.Objects[Page-1]).SaveToStream(MS);
          MS.Seek(0,soFromBeginning);
          Img.LoadFromStream(MS);
          PreviewSurfaceBitmap.Canvas.StretchDraw(R,Img);
        finally
          Img.Free;
          MS.Free;
        end;
      end;
      PreviewSurfaceBitmap.Canvas.Refresh;
    end;
    // draw the change page grey "arrow" buttons
    if Page>1 then begin
      P1.X := R.Left+10;
      P2.X := R.Left+1;
      PageLeftButton.X := P2.X;
      P1.Y := R.Top+11;
      P2.Y := P1.Y;
      PageLeftButton.Y := P1.Y-10;
      DrawArrowInternal(PreviewSurfaceBitmap.Canvas,P1,P2,10,true);
    end else
      PageLeftButton.X := 0;
    if Page<PageCount then begin
      P1.X := R.Right-10;
      PageRightButton.X := P1.X;
      P2.X := R.Right-1;
      P1.Y := R.Top+11;
      P2.Y := P1.Y;
      PageRightButton.Y := P1.Y-10;
      DrawArrowInternal(PreviewSurfaceBitmap.Canvas,P1,P2,10,true);
    end else
      PageRightButton.X := 0;
    //draw the page shadows
    R.Top := GRAY_MARGIN+3;
    R.Left := ClientWidth-GRAY_MARGIN;
    R.Bottom := ClientHeight-GRAY_MARGIN+3;
    R.Right := R.Left+3;
    PreviewSurfaceBitmap.Canvas.brush.color := clGray;
    PreviewSurfaceBitmap.Canvas.FillRect(R);
    R.Top := ClientHeight-GRAY_MARGIN;
    R.Left := GRAY_MARGIN+3;
    R.Bottom := R.Top+3;
    R.Right := ClientWidth-GRAY_MARGIN+3;
    PreviewSurfaceBitmap.Canvas.brush.color := clGray;
    PreviewSurfaceBitmap.Canvas.FillRect(R);
    Canvas.Draw(0,0,PreviewSurfaceBitmap)
  end;
  if fLinksCurrent>=0 then
    fPreviewSurface.Canvas.DrawFocusRect(
      TGDIPagereference(fLinks.Objects[fLinksCurrent]).Preview);
end;

procedure TGDIPages.PreviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  if Button=mbRight then begin
    if PopupMenu<>nil then begin
      with fPreviewSurface.ClientToScreen(Point(X,Y)) do
        PopupMenu.Popup(X,Y);
      exit;
    end;
  end else
  if Button=mbLeft then begin
    if fLinksCurrent>=0 then begin
      fPreviewSurface.Canvas.DrawFocusRect(
        TGDIPagereference(fLinks.Objects[fLinksCurrent]).Preview);
      i := fLinksCurrent;
      fLinksCurrent := -1;
      GotoBookmark(fLinks[i]);
    end else
    if (PageLeftButton.X<>0) and
       (cardinal(X-PageLeftButton.X)<10) and
       (cardinal(Y-PageLeftButton.Y)<20) then begin
      Page := Page-1;
      exit;
    end else
    if (PageRightButton.X<>0) and
       (cardinal(X-PageRightButton.X)<10) and
       (cardinal(Y-PageRightButton.Y)<20) then begin
      Page := Page+1;
      exit;
    end;
  end;
  if (Button=mbLeft) and (ssDouble in Shift) then begin
    Zoom := PAGE_WIDTH; // double click on page -> reset zoom to page width
  end else
{$ifndef MOUSE_CLICK_PERFORM_ZOOM}
  if Button=mbLeft then begin
    fButtonDown.X := (X shr 3)shl 3; // move 8 pixels by 8 pixels
    fButtonDown.Y := (Y shr 3)shl 3;
    fButtonDownScroll.X := HorzScrollBar.Position;
    fButtonDownScroll.Y := VertScrollBar.Position;
    Screen.Cursor := crHandPoint;
  end;
{$endif}
  //pass the TPaintbox mouse-down event messages to Self (TScrollBox) ...
  MouseDown(Button,Shift,X+fPreviewSurface.left,Y+fPreviewSurface.Top);
end;

procedure TGDIPages.PreviewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //pass the TPaintbox mouse-up event messages to Self (TScrollBox) ...
  MouseUp(Button,Shift,X+fPreviewSurface.left,Y+fPreviewSurface.Top);
end;

procedure TGDIPages.PreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
{$ifndef MOUSE_CLICK_PERFORM_ZOOM}
var BX, V: integer;
{$endif}
var i: integer;
begin
  fMousePos.X := X+fPreviewSurface.left;
  fMousePos.Y := Y+fPreviewSurface.Top;
  if fLinksCurrent>=0 then begin
    fPreviewSurface.Canvas.DrawFocusRect(
      TGDIPagereference(fLinks.Objects[fLinksCurrent]).Preview);
    fLinksCurrent := -1;
  end;
{$ifndef MOUSE_CLICK_PERFORM_ZOOM}
  if fButtonDown.X>=0 then begin
    X := (X shr 3)shl 3; // move 8 pixels by 8 pixels
    Y := (Y shr 3)shl 3;
{    OutputDebugString(pointer(format(
        'X=%d Y=%d ScrlIni X=%d Y=%d ScrlCurr X=%d Y=%d ',
        [X,Y,fButtonDownScroll.X,fButtonDownScroll.Y,
        HorzScrollBar.Position,VertScrollBar.Position]))); }
    BX := fButtonDown.X;
    fButtonDown.X := -1; // avoid endless recursive call
    V := fButtonDownScroll.X-X+BX;
    if (V>=0) and (HorzScrollBar.Position<>V) and (V<HorzScrollBar.Range) then begin
      HorzScrollBar.Position := V;
      fButtonDownScroll.X := V;
    end;
    V := fButtonDownScroll.Y-Y+fButtonDown.Y;
    if (V>=0) and (VertScrollBar.Position<>V) and (V<VertScrollBar.Range) then begin
      VertScrollBar.Position := V;
      fButtonDownScroll.Y := V;
    end;
    fButtonDown.X := BX;
    exit;
  end else
{$endif}
  for i := 0 to fLinks.Count-1 do
    with TGDIPagereference(fLinks.Objects[i]) do
      if (Page=Self.Page) and (X>=Preview.Left) and (X<Preview.Right) and
         (Y>=Preview.Top) and (Y<Preview.Bottom) then begin
        fLinksCurrent := i;
        fPreviewSurface.Canvas.DrawFocusRect(Preview);
        break;
      end;
end;

procedure TGDIPages.CMFontChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(fCanvas) then
    UpdateMetafileCanvasFont(fCanvas);
  fLineHeight := 0; // force recalculation of lineheight
end;

procedure TGDIPages.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TGDIPages.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SetPageAndPosition(newpage,newpos: integer);
  begin
    perform(WM_SETREDRAW,0,0);
    Page := newpage;
    VertScrollbar.position := newpos;
    perform(WM_SETREDRAW,1,0);
    refresh;
  end;

var
  OldPosition,lh: integer;
begin
  lh := PrinterPxToScreenPxY(GetLineHeight);
  case Key of
  VK_DOWN:
    with VertScrollbar do begin
      OldPosition := Position;
      position := position + lh;
      if (Position = OldPosition) and (Page < PageCount) then
        SetPageAndPosition(Page+1,0);
    end;
  VK_UP:
    with VertScrollbar do begin
      OldPosition := Position;
      position := position - lh;
      if (Position = OldPosition) and (Page > 1) then
        SetPageAndPosition(Page-1,range);
    end;
  VK_RIGHT:
    with HorzScrollbar do
      position := position + max(lh,0);
  VK_LEFT:
    with HorzScrollbar do
      position := position - min(lh,range);
  VK_NEXT:
    with VertScrollbar do
      if (shift = [ssCtrl]) and (Page < PageCount) then
        SetPageAndPosition(PageCount,0)
      else begin
        OldPosition := Position;
        position := position + max(clientheight - lh,0);
        if (Position = OldPosition) and (Page < PageCount) then
          SetPageAndPosition(Page+1,0);
      end;
  VK_PRIOR:
    with VertScrollbar do begin
      if (shift = [ssCtrl]) and (Page > 1) then
          SetPageAndPosition(1,0)
      else begin
        OldPosition := Position;
        position := position - max(clientheight-lh,0);
        if (Position = OldPosition) and (Page > 1) then
          SetPageAndPosition(Page-1,range);
      end;
    end;
  VK_ADD, VK_SUBTRACT, 187, 189:
    if ssCtrl in Shift then begin
      fZoomIn := Key in [VK_ADD,187]; // Ctrl+ Ctrl- are standard zoom IN/OUT
      ZoomTimer(nil);
    end;
  VK_ESCAPE:
    if PreviewForm<>nil then
      PreviewForm.Close; // ESC will close preview form (if any)
  end;
  inherited;
end;

procedure TGDIPages.CreateWnd;
begin
  inherited CreateWnd;
  // force page repositioning  +/-resizing
  case ZoomStatus of
    zsPercent:   ResizeAndCenterPaintbox;
    zsPageWidth: zoom := PAGE_WIDTH;
    else         zoom := PAGE_FIT;
  end;
end;

procedure TGDIPages.Resize;
begin
  // force page repositioning  +/-resizing
  case ZoomStatus of
    zsPercent:   ResizeAndCenterPaintbox;
    zsPageWidth: zoom := PAGE_WIDTH;
    else         zoom := PAGE_FIT;
  end;
  inherited Resize;
end;

procedure TGDIPages.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  // allow overriding of default mouse handling...
  if not Assigned(OnMouseDown) then begin
    fZoomIn := (Button = mbLeft);
    ZoomTimerInternal(X, Y, fZoomIn);
    fZoomTimer.Enabled := true;
  end;
{$endif}
  if Button=mbLeft then begin
    if PopupMenu<>nil then begin
      with fPreviewSurface do
      if (X<Left) or (X>Left+Width) then
      with Self.ClientToScreen(Point(X,Y)) do
        Self.PopupMenu.Popup(X,Y);
    end;
  end;
  if canfocus and not focused then
    Setfocus;
  inherited;
end;

procedure TGDIPages.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  fZoomTimer.enabled := false;
{$else}
  fButtonDown.X := -1; // so MouseMove() won't scroll paintbox
  Screen.Cursor := crDefault;
{$endif}
  inherited;
end;

{$IFNDEF VER100}
function TGDIPages.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean;
var key: word;
begin
  //treat mousewheel events as if a down-arrow or up-arrow event ...
  if Shift=[] then begin
    if WheelDelta < 0 then
      key := VK_DOWN else
      key := VK_UP;
    KeyDown(Key,[]);
  end else
  if Shift=[ssCtrl] then
    ZoomTimerInternal(fMousePos.X,fMousePos.Y,(WheelDelta>0));
  Result := true;
end;
{$ENDIF}

constructor TGDIPages.Create(AOwner: TComponent);
{$ifdef USEPDFPRINTER}
var i: integer;
    aName: string;
{$endif}
begin
  inherited Create(AOwner);
  SetLength(fTab,MAXTABS);
  ForceInternalAntiAliased := True; // GDI+ 1.1 ConvertToEmfPlus is buggy
  PopupMenuClass := TPopupMenu;
  // DoubleBuffered := true; // avoiding flicker is done in Paint method
  Height := 150;
  width := 200;
  ControlStyle := ControlStyle - [csAcceptsControls];
  if (AOWner<>nil) and AOWner.InheritsFrom(TCustomForm) then
    Color := TCustomForm(AOwner).Color else
    Color := clLtGray;
  HorzScrollBar.Tracking := True;
  VertScrollBar.Tracking := True;
  tabstop := true;

  Font.Name := 'Tahoma';
  Font.Size := 12;
  fLineSpacing := lsSingle;
  fOrientation := poPortrait;
  fUseOutlines := true;

  fPages := TStringList.create;
  fHeaderLines := TObjectList.Create;
  fFooterLines := TObjectList.Create;
  fColumnHeaderList := TStringList.create;

{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  fZoomTimer := TTimer.create(Self);
  fZoomTimer.Interval := 200;
  fZoomTimer.OnTimer := ZoomTimer;
  fZoomTimer.enabled := false;
{$else}
  fButtonDown.X := -1; // so MouseMove() won't scroll paintbox
{$endif}

  fHasPrinterInstalled := not (csDesigning in componentState)
    and PrinterDriverExists;
{$ifdef USEPDFPRINTER}
  fPDFPrinterIndex := -1;
  if fHasPrinterInstalled then
    for i := 0 to Printer.Printers.Count-1 do begin
      aName := Printer.Printers[i];
      if pos('doPDF',aName)=1 then begin
        fPDFPrinterIndex := i;
        break;
      end else
      if pos('PDF',aName)>0 then
        fPDFPrinterIndex := i;
    end;
  fHasPDFPrinterInstalled := (fPDFPrinterIndex<>-1);
{$endif}
  GetPrinterParams; // necessary, but will also be updated in BeginDoc().
  fCanvas := nil;
  fPreviewSurface := TPagePaintbox.Create(Self);
  fPreviewSurface.parent := Self;
  fPreviewSurface.OnPaint := PreviewPaint;
  fPreviewSurface.OnMouseDown := PreviewMouseDown;
  fPreviewSurface.OnMouseUp := PreviewMouseUp;
  fPreviewSurface.OnMouseMove := PreviewMouseMove;
  fZoomStatus := zsPercent;
  fZoom := 100;
  fBookmarks := TStringList.Create;
  fLinks := TStringList.Create;
  fOutline := TStringList.Create;
end;

destructor TGDIPages.Destroy;
begin
  Clear;
  fPages.free;
  fHeaderLines.free;
  fFooterLines.free;
  fColumnHeaderList.free;
  fPreviewSurface.free;
  PreviewSurfaceBitmap.Free;
{$ifdef MOUSE_CLICK_PERFORM_ZOOM}
  fZoomTimer.free;
{$endif}
  fOutline.Free;
  fLinks.Free;
  fBookmarks.Free;
  inherited Destroy;
end;

procedure TGDIPages.Invalidate;
begin
  FreeAndNil(PreviewSurfaceBitmap); // invalidate custom double buffering
  inherited;
end;

procedure TGDIPages.BeginDoc;
begin
  if Self=nil then exit; // avoid GPF
  Clear;
  GetPrinterParams; // essential as Printers.printer object may have changed
  fHangIndent := 0;
  fAlign := taLeft;
  SetPageMargins(Rect(10,10,10,10));
  fVirtualPageNum := 0;
  Application.ProcessMessages;
  NewPageInternal;  // create a blank page
  // preview resize in case Printers.printer object has changed
  case ZoomStatus of
    zsPercent:   zoom := fzoom;
    zsPageWidth: zoom := PAGE_WIDTH;
    else zoom := PAGE_FIT;
  end;
  fButtonDown.X := -1; // so MouseMove() won't scroll paintbox
end;

procedure TGDIPages.DrawText(const s: string);
begin
  DrawTextW(StringToSynUnicode(s));
end;

procedure TGDIPages.DrawTextW(const s: SynUnicode);
var P, Start: PWideChar;
    tmpStr: SynUnicode;
begin
  if Self=nil then exit;
  CheckYPos;
  if s = '' then
    NewLine else begin
    // split NewLine characters (#13 or #13#10) into multi lines
    P := pointer(s);
    while P^ <> #0 do begin
      Start := P;
      while not (ord(P^) in [0, 10, 13]) do Inc(P);
      SetString(tmpStr, Start, P-Start);
      if not fInHeaderOrFooter then
        fCanvasText := fCanvasText+SynUnicodeToString(tmpStr)+#13#10;
      PrintFormattedLine(tmpStr, FORMAT_DEFAULT);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
  end;
end;

procedure TGDIPages.DrawTextU(const s: RawUTF8);
begin
  DrawTextW(UTF8ToSynUnicode(s));
end;

procedure TGDIPages.DrawTitle(const s: SynUnicode; DrawBottomLine: boolean=false;
  OutlineLevel: Integer=0; const aBookmark: string=''; const aLink: string='');
var H: integer;
    str: string;
begin
  if Self=nil then exit; // avoid GPF
  CheckYPos;
  SaveLayout;
  try
    str := SynUnicodeToString(s);
    if not fInHeaderOrFooter then
      fCanvasText := fCanvasText+str+#13#10; // copy as text
    PrintFormattedLine(s,TitleFlags,aBookMark,aLink);
    if UseOutlines then
      AddOutline(str,OutlineLevel,fCurrentTextTop,fCurrentTextPage);
    if DrawBottomLine then begin
      H := (GetLineHeight*15) shr 5;
      dec(fCurrentYPos, H);
      LineInternal(fPageMarginsPx.left, fPhysicalSizePx.x-fPageMarginsPx.right, false);
      inc(fCurrentYPos, H*2);
    end;
  finally
    RestoreSavedLayout;
  end;
end;

procedure TGDIPages.DrawTextAt(s: SynUnicode; XPos: integer; const aLink: string='';
  CheckPageNumber: boolean=false);
var i: integer;
    R: TRect;
    Size: TSize;
begin
  if (Self=nil) or (s='') then exit;
  CheckYPos;
  if CheckPageNumber then begin
    i := pos(PAGENUMBER,LowerCaseU(s));
    if i > 0 then begin
      Delete(s,i,14);
      Insert(UTF8ToSynUnicode(Int32ToUtf8(fVirtualPageNum)),s,i);
    end;
  end;
  SetBkMode(fCanvas.Handle,TRANSPARENT);
  Size := TextExtent(fCanvas,s);
  R.Left := MmToPrinterPxX(XPos);
  case falign of
    taRight:  dec(R.Left,Size.cx+1);
    taCenter: dec(R.Left,Size.cx shr 1+1);
  end;
  R.Top := fCurrentYPos;
  TextOut(fCanvas,R.Left,R.Top,s);
  if not fInHeaderOrFooter then // copy as text on a new line
    fCanvasText := fCanvasText+SynUnicodeToString(s)+#13#10;
  if aLink<>'' then begin
    R.Right := R.Left+Size.cx;
    R.Bottom := R.Top+Size.cy;
    AddLink(aLink,PrinterToMM(R));
  end;
end;

procedure TGDIPages.DrawAngledTextAt(const s: SynUnicode; XPos, Angle: integer);
var
  lf: TLogFont;
  OldFontHdl,NewFontHdl: HFont;
begin
  if (s='') or (Self=nil) then exit; // avoid GPF
  CheckYPos;
  XPos := MmToPrinterPxX(XPos);
  SetBkMode(fCanvas.Handle,TRANSPARENT);
  with fCanvas do begin
    if GetObject(Font.Handle, SizeOf(lf), @lf) = 0 then exit;
    lf.lfEscapement := Angle * 10;
    lf.lfOrientation := Angle * 10;
    lf.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    NewFontHdl := CreateFontIndirect(lf);
    OldFontHdl := selectObject(handle,NewFontHdl);
  end;
  TextOut(fCanvas,XPos,fCurrentYPos,s);
  selectObject(fCanvas.handle,OldFontHdl);
  DeleteObject(NewFontHdl);
  if not fInHeaderOrFooter then
    fCanvasText := fCanvasText+s+#13#10; // copy as text on a new line
end;

function TGDIPages.MmToPrinter(const R: TRect): TRect;
begin
  if Self=nil then begin
    FillChar(result,sizeof(result),0);
    exit; // avoid GPF
  end;
  result.left := MmToPrinterPxX(R.left);
  result.top := MmToPrinterPxY(R.top);
  result.right := MmToPrinterPxX(R.right);
  result.bottom := MmToPrinterPxY(R.bottom);
end;

function TGDIPages.PrinterToMM(const R: TRect): TRect;
begin
  if Self=nil then begin
    FillChar(result,sizeof(result),0);
    exit; // avoid GPF
  end;
  result.left := PrinterPxToMmX(R.left);
  result.top := PrinterPxToMmY(R.top);
  result.right := PrinterPxToMmX(R.right);
  result.bottom := PrinterPxToMmY(R.bottom);
end;

procedure TGDIPages.DrawBox(left,top,right,bottom: integer);
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  left := MmToPrinterPxX(left);
  top := MmToPrinterPxY(top);
  right := MmToPrinterPxX(right);
  bottom := MmToPrinterPxY(bottom);
  with fCanvas do begin
    Pen.Width := MulDiv(fDefaultLineWidth,Self.Font.Size,8);
    if fsBold in Self.Font.style then
      Pen.Width := Pen.Width +1;
    MoveTo(left,top);
    LineTo(right,top);
    LineTo(right,bottom);
    LineTo(left,bottom);
    LineTo(left,top);
  end;
end;

procedure TGDIPages.DrawBoxFilled(left,top,right,bottom: integer; Color: TColor);
var SavedBrushColor: TColor;
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  left := MmToPrinterPxX(left);
  top := MmToPrinterPxY(top);
  right := MmToPrinterPxX(right);
  bottom := MmToPrinterPxY(bottom);
  with fCanvas do begin
    Pen.Width := MulDiv(fDefaultLineWidth,Self.Font.Size,8);
    if fsBold in Self.Font.style then
      Pen.Width := Pen.Width +1;
    SavedBrushColor := Brush.Color;
    brush.Color := Color;
    rectangle(left,top,right,bottom);
    Brush.Color := SavedBrushColor;
  end;
end;

procedure TGDIPages.DrawBMP(rec: TRect; bmp: TBitmap);
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  PrintBitmap(fCanvas, MmToPrinter(rec), bmp);
end;

procedure TGDIPages.DrawBMP(bmp: TBitmap; bLeft, bWidth: integer; const Legend: string);
begin
  DrawGraphic(bmp,bLeft,bWidth,Legend);
end;

procedure TGDIPages.DrawGraphic(graph: TGraphic; bLeft, bWidth: integer;
  const Legend: SynUnicode);
var R: TRect;
    H: Integer;
begin
  if (Self=nil) or (graph=nil) then exit; // avoid GPF
  // compute position and draw bitmap
  if bLeft=maxInt then // do center
    bLeft := PrinterPxToMmX(fPageMarginsPx.Left+
      (fPhysicalSizePx.x-fPageMarginsPx.Right-fPageMarginsPx.Left-MmToPrinterPxX(bWidth))shr 1) else
    inc(bLeft,LeftMargin);
  R.Left := bLeft;
  R.Right := bLeft+bWidth;
  R.Bottom := (graph.Height*bWidth) div graph.Width;
  if Legend<>'' then
    H := LineHeight else
    H := 0;
  if not HasSpaceFor(R.Bottom+H) then begin
    NewPage;
    DoHeader;
    NewHalfLine;
  end;
  R.Top := CurrentYPos;
  Inc(R.Bottom,R.Top);
  if graph.InheritsFrom(TBitmap) then
    DrawBMP(R,graph as TBitmap) else
  if graph.InheritsFrom(TMetaFile) then
    DrawMeta(R,graph as TMetaFile);
  CurrentYPos := R.Bottom;
  // draw optional caption bottom
  if Legend<>'' then begin
    SaveLayout;
    TextAlign := taCenter;
    Font.Style := [];
    Font.Size := (Font.Size*3)shr 2; // smaller font for caption text
    DrawTextW(Legend);
    RestoreSavedLayout;
  end else
    NewHalfLine;
end;

procedure TGDIPages.DrawMeta(rec: TRect; meta: TMetafile);
var old: Integer;
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  rec := MmToPrinter(rec);
  old := SaveDC(fCanvas.Handle);   // ensure safe metafile embedding
  fCanvas.StretchDraw(rec, meta);
  RestoreDC(fCanvas.Handle,old);
end;

procedure TGDIPages.DrawArrow(Point1, Point2: TPoint;
      HeadSize: integer; SolidHead: boolean);
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  Point1.X := MmToPrinterPxX(Point1.X);
  Point1.Y := MmToPrinterPxY(Point1.Y);
  Point2.X := MmToPrinterPxX(Point2.X);
  Point2.Y := MmToPrinterPxY(Point2.Y);
  HeadSize := MmToPrinterPxX(max(HeadSize,0));
  fCanvas.Pen.Width := MulDiv(fDefaultLineWidth,Self.Font.Size, 8);
  DrawArrowInternal(fCanvas, Point1, Point2, HeadSize, SolidHead);
end;

procedure TGDIPages.DrawLine(doubleline: boolean);
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  LineInternal(fPageMarginsPx.left, fPhysicalSizePx.x-fPageMarginsPx.right, doubleline);
  NewLine;
end;

procedure TGDIPages.DrawDashedLine;
var
  Y: integer;
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  with fCanvas do
  begin
    Pen.Width := 1;
    Pen.Style := psDash;
    Y := fCurrentYPos + (GetLineHeight shr 1) - (Pen.Width shr 1);
    MoveTo(fPageMarginsPx.left, Y);
    LineTo(fPhysicalSizePx.x-fPageMarginsPx.right, Y);
    Pen.Style := psSolid;
  end;
  NewLine;
end;

procedure TGDIPages.NewLine;
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  inc(fCurrentYPos, GetLineHeight);
//  fCanvasText := fCanvasText+#13#10;
end;

procedure TGDIPages.NewHalfLine;
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  inc(fCurrentYPos, GetLineHeight shr 1);
//  fCanvasText := fCanvasText+#13#10;
end;

procedure TGDIPages.NewLines(count: integer);
begin
  if Self=nil then exit; // avoid GPF
  CheckHeaderDone;
  if count < 1 then exit;
  inc(fCurrentYPos, GetLineHeight* count);
//  fCanvasText := fCanvasText+#13#10;
end;

procedure TGDIPages.NewPage(ForceEndGroup: boolean);
begin
  if Self=nil then exit; // avoid GPF
  if ForceEndGroup then
    EndGroup else
  if Assigned(fGroupPage) then
    raise Exception.Create('Cannot call NewPage within a group block.');
  CheckHeaderDone;
  NewPageInternal;
end;

procedure TGDIPages.NewPageIfAnyContent;
begin
  if Self=nil then exit; // avoid GPF
  if fHeaderDone then
    NewPage; 
end;

procedure TGDIPages.BeginGroup;
begin
  if Self=nil then exit; // avoid GPF
  if not fHeaderDone then exit; // i.e. haven't even started a page yet
  if Assigned(fGroupPage) then
    raise Exception.create('Group already started!');

  if not GroupsMustBeOnSamePage then begin
    // Group "light" implementation
    if fHeaderDone and not HasSpaceForLines(20) then
      NewPageInternal;
    exit;
  end;

  //make sure there's room for at least 2 lines otherwise just start a new page
  //(a group surely contains at least 2 lines )
  if not HasSpaceForLines(2) then begin
    NewPageInternal;
    exit;
  end;
  fGroupVerticalSpace :=
    fPhysicalSizePx.y - fCurrentYPos - fPageMarginsPx.bottom - fFooterHeight;
  fColumnsUsedInGroup := false;
  fColumnHeaderInGroup := false;
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  fGroupPage := CreateMetaFile(fPhysicalSizePx.x,fGroupVerticalSpace + fPhysicalOffsetPx.Y);
  fCanvas := CreateMetafileCanvas(fGroupPage);
  fGroupVerticalPos := fCurrentYPos;
  fCurrentYPos := 0;
  fBeforeGroupText := fCanvasText;
  fCanvasText := '';
end;

procedure TGDIPages.EndGroup;
begin
  if Self=nil then exit; // avoid GPF
  if not Assigned(fGroupPage) then
    exit;
  FreeAndNil(fCanvas); //closes fGroupPage canvas
  fCanvas := CreateMetafileCanvas(TMetaFile(Pages.Objects[Pages.Count-1]));
  fCanvas.Draw(0,0,TMetaFile(Pages.Objects[Pages.Count-1]));     //re-draw the last page
  fCanvas.Draw(0,fGroupVerticalPos,fGroupPage); //add the Group data
  FreeAndNil(fGroupPage);                       //destroy Group metafile
  inc(fCurrentYPos,fGroupVerticalPos);
  fCanvasText := fBeforeGroupText+fCanvasText;
  fBeforeGroupText := '';
end;

function TGDIPages.CurrentGroupPosStart: integer;
begin
  if Self=nil then
    result := 0 else begin
    if Assigned(fGroupPage) then
      result := fGroupVerticalPos else
      result := fPageMarginsPx.top;
    result := PrinterPXtoMmY(result);
  end;
end;

function GetNextItemW(var P: PWideChar): SynUnicode;
var S: PWideChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>',') do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
      P := S+1 else
      P := nil;
  end;
end;

function GetNextItemS(var P: PChar): string;
var S: PChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>',') do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
      P := S+1 else
      P := nil;
  end;
end;

const // zoom percentages for popup menu entries
  MenuZoom: array[0..6] of byte = (25,50,75,100,125,150,200);

procedure TGDIPages.EndDoc;
var PC: PChar;
    i, n, aX: integer;
    Men: TReportPopupMenu;
    M, Root: TMenuItem;
    Page: TMetaFile;
    s: string;
begin
  if Self=nil then exit; // avoid GPF
  fLinksCurrent := -1;
  EndGroup;
  DoFooter;
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  // cancel the last page if it hasn't been started ...
  n := Pages.Count;
  if (n>1) and not HeaderDone then begin
    TMetafile(Pages.Objects[Pages.Count-1]).Free;
    Pages.Delete(Pages.Count-1);
    dec(n);
  end else
  if n>0 then
    Pages.Strings[n-1] := fCanvasText;
  if (n>0) and (fPagesToFooterText<>'') then
    // add 'Page #/#' caption at the specified position
    for i := 0 to n-1 do begin
      Page := CreateMetaFile(fPhysicalSizePx.x,fPhysicalSizePx.y);
      fCanvas := CreateMetafileCanvas(Page);
      fCanvas.Draw(0,0,TMetaFile(Pages.Objects[i])); // re-draw the original page
      s := format(fPagesToFooterText,[i+1,n]); // add 'Page #/#' caption
      aX := fPagesToFooterAt.X;
      SavedState := fPagesToFooterState;
      if TextAlign=taRight then
        dec(aX,fCanvas.TextWidth(s));
      fCanvas.TextOut(aX,fPhysicalSizePx.y-fPageMarginsPx.bottom
        -fFooterHeight+fFooterGap+fPagesToFooterAt.Y,s);
      FreeAndNil(fCanvas);
      Pages.Objects[i].Free;
      Pages.Objects[i] := Page; // replace page content
    end;
  // OK, all Metafile pages have now been created and added to Pages[]
  if Assigned(fOnDocumentProducedEvent) then
    fOnDocumentProducedEvent(Self); // notify report just generated
  fCurrPreviewPage := 1;
  if Assigned(fPreviewPageChangedEvent) then
    fPreviewPageChangedEvent(Self); // notify page changed
  Invalidate;
  // update popup menu content
  if PopupMenu=nil then // caller may have created a TPopupMenu instance
    PopupMenu := PopupMenuClass.Create(Self) else
    PopupMenu.Items.Clear;
  PopupMenu.OnPopup := PopupMenuPopup;
  PC := pointer(string(sReportPopupMenu1));
  // 'Next,Previous,GotoPage,Zoom,Bookmarks,CopyasText,Print,PDF,Close,Pagefit,Pagewidth'
  for Men := rNextPage to rClose do
      NewPopupMenuItem(GetNextItemS(PC),-ord(Men)).Enabled :=
        (Men<rPrint) or (Men=rClose) or
        ( (Men=rPrint) and fHasPrinterInstalled) or
        ( (Men=rExportPDF) {$ifdef USEPDFPRINTER}and fHasPDFPrinterInstalled{$endif});
  PopupMenu.Items[ord(rClose)-1].Visible := false;
  M := PopupMenu.Items[ord(rZoom)-1];
  NewPopupMenuItem(GetNextItemS(PC),-1000-PAGE_FIT,M);
  NewPopupMenuItem(GetNextItemS(PC),-1000-PAGE_WIDTH,M);
  for i := 0 to high(MenuZoom) do
    NewPopupMenuItem(format('%d %%',[MenuZoom[i]]),-1000-MenuZoom[i],M);
  Root := PopupMenu.Items[ord(rBookmarks)-1];
  if UseOutlines and (fOutline.Count>0) then begin
    Root.Enabled := true;
    M := Root;
    for i := 0 to fOutline.Count-1 do
    with TGDIPagereference(fOutline.Objects[i]) do begin
      while (M<>Root) and (cardinal(-2000-M.Tag)<cardinal(fOutline.Count)) and 
         (Rect.Bottom<=TGDIPagereference(fOutline.Objects[-2000-M.Tag]).Rect.Bottom) do
        M := M.Parent;
      M := NewPopupMenuItem(fOutline[i],-2000-i,M);
    end;
  end else
    M.Enabled := false;
end;

function TGDIPages.PrintPages(PrintFrom, PrintTo: integer): boolean;
var i: integer;
    rec: TRect;
    CheckCurrentPtr: string;
    UseStretchDraw: boolean;
    BMP: TBitmap;
begin
  result := false;
  if Self=nil then exit; // avoid GPF
  if not fHasPrinterInstalled then
    raise Exception.Create('No printer driver is currently installed.');
  if PrintFrom<0 then
    with TPrintDialog.Create(nil) do
    try
      Options := [poPageNums];
      MinPage := 1;
      MaxPage := PageCount;
      FromPage := 1;
      ToPage := PageCount;
      if not Execute then
        exit;
      PrintFrom := FromPage;
      PrintTo := ToPage;
    finally
      Free;
    end;
  result := true;
  // ideally, the user has changed printers BEFORE generating a report, but
  // if they want a report sent to a different printer then use StretchDraw ...
  CheckCurrentPtr := CurrentPrinterName;
  if CheckCurrentPtr <> fCurrentPrinter then begin
    GetPrinterParams; //also updates fCurrentPrinter
    UseStretchDraw := true;
  end else
    UseStretchDraw := false;
  PrintFrom := max(PrintFrom-1,0);
  if PrintTo=0 then
    PrintTo := Pages.Count-1 else
    PrintTo := min(PrintTo-1,Pages.Count-1);
{$ifdef PRINTERNEW} // set enhanced TPrinterNew class color/BW or duplex mode
  with PrinterNew do begin
    if ForcePrintColorMode<>printColorDefault then begin
      if (ForcePrintColorMode=printColor) and HasColorMode then
        ColorMode := true else
      if ForcePrintColorMode=printBW then
        ColorMode := false;
    end;
    if ForcePrintDuplexMode<>printDuplexDefault then begin
      if (ForcePrintDuplexMode=printDuplex) and HasDuplexMode then
        DuplexMode := true else
      if ForcePrintDuplexMode=printSimplex then
        DuplexMode := false;
    end;
{$else}
  with Printer do begin
{$endif}
    if Caption='' then
    {$ifndef USEPDFPRINTER}
      if ExportPDFApplication<>'' then
        Title := ExportPDFApplication else
    {$endif}
        Title := Application.Title else
      Title := Caption;
    Orientation := Self.Orientation; // just in case fPrinter changed
    BeginDoc;
    try
      Screen.Cursor := crHourGlass;
      if ForcePrintAsBitmap then begin // very slow printing
        BMP := TBitmap.Create;
        try
          BMP.Width := GetDeviceCaps(handle, PHYSICALWIDTH);
          BMP.Height := GetDeviceCaps(handle, PHYSICALHEIGHT);
          for i := PrintFrom to PrintTo do begin
            BMP.Canvas.StretchDraw(Rect(0,0,Bmp.Width,Bmp.Height),
              TMetaFile(Pages.Objects[i]));
            Canvas.Draw(-fPhysicalOffsetPx.x,-fPhysicalOffsetPx.y,BMP);
            if i<PrintTo then
              NewPage;
          end;
        finally
          BMP.Free;
        end;
      end else
      for i := PrintFrom to PrintTo do begin
        // nb: the printer's page origin is fPhysicalOffsetPx so it's
        //     necessary to offset our rect by -fPhysicalOffsetPx ...
        if ForceScreenResolution then begin
          rec := Rect(0, 0, GetDeviceCaps(handle, PHYSICALWIDTH),
            GetDeviceCaps(handle, PHYSICALHEIGHT));
          OffsetRect(rec, -GetDeviceCaps(handle,PHYSICALOFFSETX),
            -GetDeviceCaps(handle,PHYSICALOFFSETY));
          Canvas.StretchDraw(rec, TMetaFile(Pages.Objects[i]));
        end else
        if UseStretchDraw then
          Canvas.StretchDraw(Rect(-fPhysicalOffsetPx.x,-fPhysicalOffsetPx.y,
            fPhysicalSizePx.x-fPhysicalOffsetPx.x, fPhysicalSizePx.y-fPhysicalOffsetPx.y),
            TMetaFile(Pages.Objects[i])) else
          Canvas.Draw(-fPhysicalOffsetPx.x,-fPhysicalOffsetPx.y,TMetaFile(Pages.Objects[i]));
        if i<PrintTo then
          NewPage;
      end;
      EndDoc;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;


procedure TGDIPages.SetTabStops(const tabs: array of integer);
var i: integer;
begin
  if Self=nil then exit; // avoid GPF
  FillChar(fTab[0],MAXTABS*sizeof(fTab[0]),0);
  fTabCount := min(high(tabs)+1,MAXTABS);
  //ignore trailing 0 tabs in array ...
  if (fTabCount > 0) then
    while (fTabCount > 0) and (tabs[fTabCount-1] = 0) do
      dec(fTabCount);

  if (fTabCount > 1) then begin
    if (tabs[0] <= 0) then
      raise Exception.Create('Tabs stops must be greater than 0.');
    fTab[0] := MmToPrinterPxX(tabs[0]);
    for i := 1 to fTabCount -1 do
      if tabs[i] > tabs[i-1] then
        fTab[i] := MmToPrinterPxX(tabs[i]) else
        raise Exception.Create('Tabs stops must be in ascending order');
  end else
  if fTabCount = 1 then begin
    //if one tab set then use that tab as the interval for subsequent tabs
    for i := 0 to MAXTABS-1 do
      fTab[i] := MmToPrinterPxX((i+1)*tabs[0]);
    fTabCount := MAXTABS;
  end else begin
    //if no tabs set then default to tabs every 20mm
    for i := 0 to MAXTABS-1 do fTab[i] := MmToPrinterPxX((i+1)*20);
    fTabCount := MAXTABS;
  end;
end;


function TGDIPages.GetPageMargins: TRect;
begin
  if Self=nil then
    FillChar(result,sizeof(result),0) else
    with result do begin
      Left := PrinterPxToMmX(fPageMarginsPx.left);
      Top := PrinterPxToMmY(fPageMarginsPx.top);
      Right := PrinterPxToMmX(fPageMarginsPx.right);
      Bottom := PrinterPxToMmY(fPageMarginsPx.bottom);
    end;
end;

procedure TGDIPages.SetPageMargins(Rect: TRect);
begin
  fPageMarginsPx := MmToPrinter(Rect);
  if not fHeaderDone then
    fCurrentYPos := fPageMarginsPx.top;
end;

function TGDIPages.GetLeftMargin: integer;
begin
  if Self=nil then
    result := 0 else
    result := PrinterPxToMmX(fPageMarginsPx.left);
end;

procedure TGDIPages.SetLeftMargin(const Value: integer);
begin
  if Self=nil then exit;
  fPageMarginsPx.Left := MmToPrinterPxX(Value);
end;

function TGDIPages.GetPaperSize: TSize;
begin
  if Self=nil then
    FillChar(result,sizeof(result),0) else begin
    result.cx := PrinterPxToMmX(fPhysicalSizePx.X);
    result.cy := PrinterPxToMmY(fPhysicalSizePx.Y);
  end;
end;

procedure TGDIPages.AddLineToHeader(doubleline: boolean);
begin
  if Self=nil then exit; // avoid GPF
  fHeaderLines.Add(THeaderFooter.Create(Self,doubleline));
end;

procedure TGDIPages.AddLineToFooter(doubleline: boolean);
begin
  if Self=nil then exit; // avoid GPF
  if fFooterLines.Count = 0 then
    CalcFooterGap;
  fFooterLines.Add(THeaderFooter.Create(Self,doubleline));
  inc(fFooterHeight, GetLineHeight);
end;


procedure TGDIPages.AddTextToHeader(const s: SynUnicode);
begin
  if Self<>nil then
    fHeaderLines.Add(THeaderFooter.Create(Self,false,s,true));
end;

procedure TGDIPages.AddTextToHeaderAt(const s: SynUnicode; XPos: integer);
var Head: THeaderFooter;
begin
  if Self=nil then exit; // avoid GPF
  Head := THeaderFooter.Create(Self,false,s,true);
  Head.State.Flags := Head.State.Flags or ((XPos+1) shl 16);
  fHeaderLines.Add(Head);
end;

procedure TGDIPages.AddTextToFooter(const s: SynUnicode);
begin
  if Self=nil then exit; // avoid GPF
  if fFooterLines.Count = 0 then
    CalcFooterGap;
  fFooterLines.Add(THeaderFooter.Create(Self,false,s,true));
  inc(fFooterHeight, GetLineHeight);
end;

procedure TGDIPages.AddTextToFooterAt(const s: SynUnicode; XPos: integer);
var Foot: THeaderFooter;
begin
  if Self=nil then exit; // avoid GPF
  //todo - can't print at 0mm from left edge so raise exception
  if fFooterLines.Count = 0 then
    CalcFooterGap;
  Foot := THeaderFooter.Create(Self,false,s,true);
  Foot.State.Flags := Foot.State.Flags or ((XPos+1) shl 16);
  fFooterLines.Add(Foot);
end;

procedure TGDIPages.AddPagesToFooterAt(const PageText: string; XPos: integer);
begin
  if fPagesToFooterText<>'' then
    exit; // only add once
  fPagesToFooterText := PageText;
  fPagesToFooterAt.X := MmToPrinterPxX(XPos);
  fPagesToFooterAt.Y := fFooterHeight;
  fPagesToFooterState := SavedState;
end;

function TGDIPages.GetColumnCount: integer;
begin
  if Self=nil then
    result := 0 else
    result := length(fColumns);
end;

function TGDIPages.GetColumnInfo(index: integer): TColRec;
begin
  if Self=nil then begin
    FillChar(result,sizeof(result),0);
    exit;
  end;
  if cardinal(index)>=cardinal(Length(fColumns)) then
    raise Exception.create('GetColumnInfo: index out of range');
  with fColumns[index] do begin
    result.ColLeft := PrinterPxToMmX(ColLeft);
    result.ColRight := PrinterPxToMmX(ColRight);
    result.ColAlign := ColAlign;
    result.ColBold := ColBold;
  end;
end;

procedure TGDIPages.SetColumnAlign(index: integer; align: TColAlign);
begin
  if Self=nil then exit; // avoid GPF
  if cardinal(index)>=cardinal(Length(fColumns)) then
    raise Exception.create('SetColumnAlign: index out of range') else
    fColumns[index].ColAlign := align;
end;

procedure TGDIPages.SetColumnBold(index: integer);
begin
  if Self=nil then exit; // avoid GPF
  if cardinal(index)>=cardinal(Length(fColumns)) then
    raise Exception.create('SetColumnAlign: index out of range') else
    fColumns[index].ColBold := true;
end;

procedure TGDIPages.AddColumn(left, right: integer; align: TColAlign; bold: boolean);
var n: integer;
begin
  if Self=nil then exit; // avoid GPF
  left := MmToPrinterPxX(left);
  right := MmToPrinterPxX(right);
  n := length(fColumns);
  if (n>0) and (left<fColumns[n-1].ColRight) then
    raise Exception.create('Columns overlap!');
  SetLength(fColumns,n+1);
  with fColumns[n] do begin
    ColLeft := left;
    ColRight := right;
    ColAlign := align;
    ColBold := bold;
  end;
end;

procedure TGDIPages.AddColumns(const PercentWidth: array of integer; align: TColAlign);
var i, sum, left, right, ww, n: integer;
begin
  if Self=nil then exit; // avoid GPF
  ClearColumns;
  sum := 0;
  for i := 0 to high(PercentWidth) do
    inc(sum,abs(PercentWidth[i]));
  if sum<=0 then
    exit;
  left := fPageMarginsPx.left;
  ww := fPhysicalSizePx.x-left-fPageMarginsPx.right;
  n := length(fColumns);
  SetLength(fColumns,n+length(PercentWidth));
  for i := 0 to high(PercentWidth) do begin
    right := left+(abs(PercentWidth[i])*ww) div sum;
    // manual adding (no mm conversion -> exact width)
    with fColumns[i+n] do begin
      ColLeft := left;
      ColRight := right;
      if PercentWidth[i]<0 then
        ColAlign := caCenter  else
        ColAlign := align;
      ColBold := false;
    end;
    left := right;
  end;
end;

procedure TGDIPages.AddColumnHeaders(const headers: array of SynUnicode;
  WithBottomGrayLine: boolean=false; BoldFont: boolean=false;
  RowLineHeight: integer=0; flags: integer=0);
var i: integer;
    zStr: TZStrings;
begin
  if Self=nil then exit; // avoid GPF
  if flags=0 then begin
    if BoldFont then
      Font.Style := [fsBold];
    flags := TextFormatsToFlags;
  end;
  zStr := '';
  for i := Low(headers) to High(headers) do
    zStr := zStr + headers[i]+#0;
  fColumnHeaderList.AddObject(zStr,pointer(flags));
  fColumnHeaderPrinted := false;
  fColumnHeaderPrintedAtLeastOnce := false;
  fColumnsWithBottomGrayLine := WithBottomGrayLine;
  fColumnsRowLineHeight := RowLineHeight;
  if BoldFont then
    Font.Style := [];
end;

function CSVToArray(var CSV: PWideChar; n: integer): TSynUnicodeDynArray;
var i: integer;
begin
  SetLength(result,n);
  for i := 0 to n-1 do
    result[i] := GetNextItemW(CSV);
end;

procedure TGDIPages.AddColumnHeadersFromCSV(var CSV: PWideChar;
  WithBottomGrayLine, BoldFont: boolean; RowLineHeight: integer);
begin
  if Self<>nil then // avoid GPF
    AddColumnHeaders(CSVToArray(CSV,length(fColumns)),
      WithBottomGrayLine,BoldFont,RowLineHeight);
end;

procedure TGDIPages.DrawTextAcrossColsFromCSV(var CSV: PWideChar; BackgroundColor: TColor=clNone);
begin
  if Self<>nil then // avoid GPF
    DrawTextAcrossCols(CSVToArray(CSV,length(fColumns)),BackgroundColor);
end;

/// round inverted color to white or black
function clAlways(cl: TColor): TColor;
begin
  if ((GetRValue(longword(cl)) * 2) +
      (GetGValue(longword(cl)) * 3) +
      (GetBValue(longword(cl)) * 2)) < 600 then
    result := clWhite else
    result := clBlack;
end;

procedure TGDIPages.DrawTextAcrossCols(const StringArray: array of SynUnicode;
  BackgroundColor: TColor = clNone);
function HasCRLF(const s: SynUnicode): boolean;
var i: integer;
begin
  result := true;
  for i := 0 to length(s)-1 do
    if s[i+1]<' ' then
      exit;
  result := false;
end;
function WrapText(s: SynUnicode; MaxWidth: integer; Lines: PSynUnicodeDynArray): integer;
var j,k,sp: integer;
begin
  result := 0; // returns the line count
  if Lines<>nil then
    SetLength(Lines^,0);
  repeat
    if HasCRLF(s) or (TextWidthC(fCanvas,s)>MaxWidth) then begin
      j := 1;
      k := 1;
      sp := 0;
      while (j<length(s)) and (TextWidthC(fCanvas,copy(s,1,j))<MaxWidth) do begin
        k := j; // store last fitting character index
        if s[j]<=' ' then begin
          sp := j; // mark space (=word delimiter) found
          if s[j]<' ' then
            break; // #13,#10 will force word wrap here = next line
        end;
        inc(j);
      end;
      if sp=0 then
        sp := k; // if no space found, use character wrapping
    end else
      sp := length(s)+1;
    if sp<=1 then
      sp := 2;
    if Lines<>nil then begin
      SetLength(Lines^,length(Lines^)+1);
      Lines^[high(Lines^)] := copy(s,1,sp-1);
    end;
    inc(result); // update lines count
    s := trim(copy(s,sp,maxInt)); // trim ' ',#13,#10 for next line
  until s='';
end;
var RowRect: TRect;
    lh: integer;
var max, i, j, k, c, H, ParenthW, LinesCount, X: integer;
    s: SynUnicode;
    line: string;
    Lines: TSynUnicodeDynArray;
    PW: PWideChar;
    PWLen, Options: integer;
    size: TSize;
begin
  if Self=nil then exit; // avoid GPF
  max := high(fColumns);
  if (max<0) or (length(StringArray)=0) then
    exit; // no column defined
  if High(StringArray)<max then
    max := High(StringArray);
  if max<0 then
    exit; // nothing to draw
  // check enough place for this column content on the page
  lh := GetLineHeight;
  CheckYPos;
  LinesCount := 1; // by default, one line of text will be written
  if WordWrapLeftCols then begin // check if stay on current page after word wrap
    for j := 0 to max do
    with fColumns[j] do
    if (ColAlign=caLeft) and (ColRight>ColLeft) and
       (HasCRLF(StringArray[j]) or
        (TextWidthC(fCanvas,StringArray[j])>ColRight-ColLeft)) then begin
      k := WrapText(StringArray[j],ColRight-ColLeft,nil); // calculate line counts
      if k>LinesCount then
        LinesCount := k; // calculate maximum line count
    end;
    if (LinesCount>1) and not HasSpaceForLines(LinesCount) then begin
      NewPageInternal;
      CheckHeaderDone;
    end;
  end;
  if (fColumnHeaderList.Count > 0) and not fColumnHeaderPrinted then begin
    i := fColumnHeaderList.Count + 2;
    if not HasSpaceForLines(i) then
      NewPageInternal;
    PrintColumnHeaders;
  end;
  // prepare column write
  if Assigned(fGroupPage) then
    fColumnsUsedInGroup := true;
  ParenthW := fCanvas.TextWidth(')');
  RowRect.Top := fCurrentYPos;
  RowRect.Bottom := RowRect.Top+lh*LinesCount;
  RowRect.Right := fColumns[max].ColRight;
  if BackgroundColor<>clNone then
  with fCanvas do begin
    Brush.Style := bsSolid;
    Brush.Color := BackgroundColor;
    RowRect.Left := fColumns[0].ColLeft;
    FillRect(RowRect);
    Brush.Style := bsClear;
    Font.Color := clAlways(BackgroundColor);
  end;
  // main loop, used to write column content
  line := '';
  for i := 0 to max do begin
    s := StringArray[i];
    line := line+SynUnicodeToString(s)+#9; // add column content + tab for report text
    if s<>'' then
    with fColumns[i], fCanvas do
    if ColRight>ColLeft then begin
      if ColBold then
        Font.Style := Font.Style+[fsBold];
      Options := ETO_CLIPPED or TextFlags; // unicode version of TextRect()
      if Brush.Style <> bsClear then
        Options := Options or ETO_OPAQUE;
      InternalUnicodeString(s,PW,PWLen,@size);
      if (ColAlign=caCenter) and (size.cx>ColRight-ColLeft) then
        // overlapping centered -> draw right aligned
        RowRect.Left := ColRight-size.cx-ParenthW else
      case ColAlign of
        caLeft: begin
          RowRect.Left := ColLeft;
          if WordWrapLeftCols and (ColRight>ColLeft) and
             (HasCRLF(s) or (size.cx>ColRight-ColLeft)) then begin
            // handle optional left aligned column content word wrap
            WrapText(s,ColRight-ColLeft,@Lines); // word wrap s into Lines[]
            dec(RowRect.Left,ParenthW);
            for j := 0 to high(Lines) do begin
              InternalUnicodeString(Lines[j],PW,PWLen,@size);
              if BiDiMode=bdRightToLeft then
                X := ColRight-size.cx-ParenthW else
                X := ColLeft;
              RowRect.Top := fCurrentYPos+lh*j;
              ExtTextOutW(Handle,X,RowRect.Top,Options,@RowRect,PW,PWLen,nil);
            end;
            RowRect.Top := fCurrentYPos;
            if ColBold then
              Font.Style := Font.Style-[fsBold];
            Continue; // text was written as word-wrap -> write next column
          end else
          if BiDiMode=bdRightToleft then
            RowRect.Left := ColRight-size.cx-ParenthW;
        end;
        caCenter:
          RowRect.Left := ColLeft+(ColRight-ColLeft-size.cx)shr 1;
        caRight:
          if BiDiMode=bdLeftToRight then
            RowRect.Left := ColRight-size.cx-ParenthW;
        caCurrency:
          begin
            if fNegsToParenthesesInCurrCols then
              InternalUnicodeString(ConvertNegsToParentheses(s),PW,PWLen,@size);
            // ignore parentheses when aligning currency values ...
            if (s<>'') and (s[length(s)] = ')') then
              RowRect.Left := ColRight-size.cx+ParenthW-1 else
              RowRect.Left := ColRight-size.cx-ParenthW;
            // no bdRightToleft handling necessary for caCurrency
          end;
      end;
      dec(RowRect.Left,ParenthW);
      ExtTextOutW(Handle,RowRect.Left+ParenthW,fCurrentYPos,Options,@RowRect,PW,PWLen,nil);
      inc(RowRect.Left,size.cx+ParenthW);
      if ColBold then
        Font.Style := Font.Style-[fsBold];
    end;
  end;
  if not fDrawTextAcrossColsDrawingHeader or
     not fColumnHeaderPrintedAtLeastOnce then begin
    line[length(line)] := #13; // overwrite last #9
    line := line+#10;
    fCanvasText := fCanvasText+line; // append columns content to report text
  end;
  if BackgroundColor<>clNone then
    fCanvas.Font.Color := clBlack;
  if not fDrawTextAcrossColsDrawingHeader and (fColumnsRowLineHeight>LinesCount) then
    // custom space for Row before bottom gray line
    LinesCount := fColumnsRowLineHeight;
  for i := 2 to LinesCount do
    NewLine;
  if fColumnsWithBottomGrayLine and (RowRect.Right<>0) then begin
    c := fCanvas.Pen.Color;
    fCanvas.Pen.Color := clLtGray;
    H := lh shr 1-(lh*15)shr 4;
    dec(fCurrentYPos, H);
    LineInternal(GetColumnRec(0).ColLeft,RowRect.Right,false);
    inc(fCurrentYPos, H);
    fCanvas.Pen.Color := c;
  end;
  NewLine;
end;


procedure TGDIPages.DrawLinesInCurrencyCols(doublelines: boolean);
var i: integer;
begin
  if Self=nil then exit; // avoid GPF
  CheckYPos;
  if (fColumnHeaderList.Count > 0) and not fColumnHeaderPrinted then begin
    i := fColumnHeaderList.Count + 2;
    if not HasSpaceForLines(i) then
      NewPageInternal;
    PrintColumnHeaders;
  end;
  for i := 0 to high(fColumns) do
    with fColumns[i] do
      if ColAlign = caCurrency then
        LineInternal(ColLeft, ColRight, doublelines);
  NewLine;
end;

procedure TGDIPages.ColumnHeadersNeeded;
begin
  if Self=nil then exit; // avoid GPF
  fColumnHeaderPrinted := false;
end;

procedure TGDIPages.Clear;
procedure ClearObjects(List: TStringList);
var i: integer;
begin
  for i := 0 to List.Count-1 do
    List.Objects[i].Free;
  List.Clear;
end;
begin
  if Self=nil then exit; // avoid GPF
  if Assigned(fCanvas) then
    FreeAndNil(fCanvas);
  if Assigned(fGroupPage) then
    FreeAndNil(fGroupPage);
  ClearObjects(Pages);
  ClearObjects(fBookmarks);
  ClearObjects(fLinks);
  ClearObjects(fOutline);
  ClearHeaders;
  ClearFooters;
  ClearColumns;
  SetTabStops([20]);
  fCanvasText := '';
  fLinksCurrent := -1;
  fSavedCount := 0;
end;

procedure TGDIPages.ClearHeaders;
begin
  if Self=nil then exit; // avoid GPF
  fHeaderLines.Clear;
end;

procedure TGDIPages.ClearFooters;
begin
  if Self=nil then exit; // avoid GPF
  fFooterLines.Clear;
  fPagesToFooterText := '';
end;

procedure TGDIPages.ClearColumns;
begin
  if Self=nil then exit; // avoid GPF
  SetLength(fColumns,0);
  ClearColumnHeaders;
end;

procedure TGDIPages.ClearColumnHeaders;
begin
  if Self=nil then exit; // avoid GPF
  fColumnHeaderList.clear;
end;

function TGDIPages.CreatePictureMetaFile(Width, Height: integer;
  out MetaCanvas: TCanvas): TMetaFile;
begin
  if Self=nil then
    result := nil else begin
    result := CreateMetaFile(MmToPrinterPxX(Width),MmToPrinterPxY(Height));
    MetaCanvas := CreateMetafileCanvas(result);
  end;
end;

procedure TGDIPages.DrawTextFmt(const s: string; const Args: array of const);
begin
  DrawText(format(s,Args));
end;

function TGDIPages.TitleFlags: integer;
begin
  result := ((Font.Size*12) div 10) or FORMAT_BOLD or FORMAT_LEFT;
end;

function TGDIPages.TextWidth(const Text: SynUnicode): integer;
begin
  if Self=nil then
    result := 0 else begin
    if fCanvas=nil then
      result := TextWidthC(Canvas,Text) else
      result := TextWidthC(fCanvas,Text);
    result := PrinterPxToMmX(result);
  end;
end;

procedure TGDIPages.ShowPreviewForm;
  procedure CopyMenus(Source,Dest: TMenuItem);
  var i: integer;
      Sub: TMenuItem;
  begin
    for i := 0 to Source.Count-1 do
    with Source.Items[i] do begin
      Sub := TMenuItem.Create(PreviewForm);
      Sub.Tag := Tag;
      Sub.OnClick := OnClick;
      Sub.Caption := Caption;
      Dest.Add(Sub);
      CopyMenus(Source.Items[i],Sub);
    end;
  end;
const PANELWIDTH = 128;
var OldParent: TWinControl;
    i,y,W: integer;
    M: TMenuItem;
    LeftPanel: TPanel;
begin
  if Self=nil then exit; // avoid GPF
  PreviewForm := TForm.Create(nil);
  try
    PreviewForm.Position := poScreenCenter;
    PreviewForm.Height := Screen.Height-64;
    PreviewForm.Caption := Caption;
    PreviewForm.Font.Name := 'Tahoma';
    with PaperSize do begin
      if cy=0 then
        y := 1 else
        y := cy;
      PreviewForm.Width := (cx*PreviewForm.Height) div y+(64+PANELWIDTH);
    end;
    if PreviewForm.Width>Screen.WorkAreaWidth then
      PreviewForm.WindowState := wsMaximized;
    LeftPanel := TPanel.Create(PreviewForm);
    LeftPanel.Parent := PreviewForm;
    LeftPanel.Width := PANELWIDTH;
    LeftPanel.Align := alLeft;
    W := LeftPanel.ClientWidth-8;
    PreviewPageCountLabel := TLabel.Create(PreviewForm);
    PreviewPageCountLabel.Transparent := true;
    PreviewPageCountLabel.Parent := LeftPanel;
    PreviewPageCountLabel.SetBounds(4,24,W-4,24);
    PreviewPageCountLabel.Alignment := Classes.taCenter;
    PreviewPageCountLabel.AutoSize := false;
    PreviewPageCountLabel.Caption := format(sPageN,[Page,PageCount]);
    PopupMenuPopup(nil); // refresh PopupMenu.Items[]
    SetLength(PreviewButtons,PopupMenu.Items.Count);
    y := 48;
    for i := 0 to High(PreviewButtons) do begin
      M := PopupMenu.Items[i];
      PreviewButtons[i] := TButton.Create(PreviewForm);
      with PreviewButtons[i] do begin
        Parent := LeftPanel;
        SetBounds(4,y,W,32);
        Enabled := M.Enabled;
        Caption := M.Caption;
        Tag := M.Tag;
        OnClick := PopupMenuItemClick;
        if M.Count>0 then begin
          PopupMenu := PopupMenuClass.Create(PreviewForm);
          CopyMenus(M,PopupMenu.Items);
        end;
        case TReportPopupMenu(i+1) of
        rPrint: begin
          Height := 60;
          inc(y,64);
          Default := true;
        end;
        rClose, rNextPage, rPreviousPage: begin
          Height := 48;
          inc(y,52);
        end;
        rGotoPage, rZoom, rBookmarks, rExportPDF:
          inc(y,48);
        else
          inc(y,36);
        end;
      end;
    end;
    OldParent := Parent;
    Parent := PreviewForm;
    Align := alClient;
    Zoom := PAGE_FIT;
    try
      PreviewForm.ActiveControl := self;
      PreviewForm.ShowModal;
    finally
      Parent := OldParent;
    end;
  finally
    FreeAndNil(PreviewForm);
    Finalize(PreviewButtons);
  end;
end;

function TGDIPages.GetRightMarginPos: integer;
begin
  result := PrinterPxToMmX(fPhysicalSizePx.x-fPageMarginsPx.right);
end;

function TGDIPages.NewPopupMenuItem(const aCaption: string; Tag: integer;
  SubMenu: TMenuItem; OnClick: TNotifyEvent; ImageIndex: integer): TMenuItem;
begin
  if (Self=nil) or (PopupMenu=nil) then begin
    result := nil;
    exit;
  end;
  result := TMenuItem.Create(PopupMenu);
  result.Caption := aCaption;
  result.Tag := Tag;
  if Assigned(OnClick) then
    result.OnClick := OnClick else
    result.OnClick := PopupMenuItemClick;
  if ImageIndex>=0 then
    result.ImageIndex := ImageIndex;
  if SubMenu=nil then
    PopupMenu.Items.Add(result) else
    SubMenu.Add(result);
end;

procedure TGDIPages.PopupMenuItemClick(Sender: TObject);
var Comp: TComponent absolute Sender;
    i: Integer;
begin
  if not Sender.InheritsFrom(TComponent) then
    exit;
  if Assigned(OnPopupMenuClick) then
  if (Comp.Tag=0) or (Comp.Tag>PageCount) then
     OnPopupMenuClick(Sender); // only notify custom events
  case -Comp.Tag of
    ord(rNone):
      exit;
    ord(rNextPage):
      Page := Page+1;
    ord(rPreviousPage):
      Page := Page-1;
    ord(rPageAsText):
      if Page>0 then
        Clipboard.AsText := Pages.Strings[Page-1];
    ord(rPrint):
      if PrintPages(-1,-1) then
        if PreviewForm<>nil then
          PreviewForm.Close;
    ord(rExportPDF):
      ExportPDF('',true);
    ord(rClose):
      if PreviewForm<>nil then
        PreviewForm.Close;
    ord(rGotoPage), ord(rZoom), ord(rBookmarks):
      if Sender.InheritsFrom(TButton) and (PreviewButtons<>nil) then
      with PreviewButtons[-1-Comp.Tag],
         PreviewForm.ClientToScreen(Point(Left,Top+Height)) do
        PopupMenu.Popup(X,Y);
    991..1999: // allow -1000-PAGE_WIDTH
      Zoom := -1000-Comp.Tag;
    2000..4000: begin // allow -2000-OutlineIndex
      i := -2000-Comp.Tag;
      if cardinal(i)<cardinal(fOutline.Count) then
        with TGDIPagereference(fOutline.Objects[i]) do
          GotoPosition(Page,Rect.Top);
    end;
    else
      if Cardinal(Comp.Tag)<=Cardinal(PageCount) then
        Page := Comp.Tag;
  end;
  if PreviewForm<>nil then
    SetFocus; 
end;

procedure TGDIPages.InternalUnicodeString(const s: SynUnicode;
  var PW: PWideChar; var PWLen: integer; size: PSize);
begin
  if Assigned(OnStringToUnicode) then begin
    fInternalUnicodeString := OnStringToUnicode(s);
    PW := pointer(fInternalUnicodeString);
    PWLen := length(fInternalUnicodeString);
  end else begin
    PW := pointer(s);
    PWLen := length(s);
  end;
  if size<>nil then begin
    size^.cx := 0;
    size^.cy := 0;
    GetTextExtentPoint32W(fCanvas.Handle,PW,PWLen,size^);
  end;
end;

procedure TGDIPages.PopupMenuPopup(Sender: TObject);
var P: PChar;
    PageFromTo, PageN: string;
    M,M2: TMenuItem;
    i,j,k: integer;
procedure AddPage(Menu: TMenuItem);
begin
  NewPopupMenuItem(format(PageN,[i]),i,Menu).Enabled := i<>Page;
end;
begin
  with PopupMenu.Items do
    if Count=0 then
      exit else
    while Count>ord(rClose) do
      Delete(ord(rClose)); // delete after "Close" entry
  PopupMenu.Items[Ord(rNextPage)-1].Enabled := Page<PageCount;
  PopupMenu.Items[Ord(rPreviousPage)-1].Enabled := Page>1;
  M := PopupMenu.Items[Ord(rGoToPage)-1];
  while M.Count>0 do
    M.Delete(0);
  M.Enabled := PageCount>1;
  if PageCount>=1 then begin // add 'Go to Page' sub menus (group by 10 pages) 
    P := pointer(string(sReportPopupMenu2));
    PageFromTo := GetNextItemS(P); // Pages %d to %d
    PageN := GetNextItemS(P);      // Page %d
    if PageCount>10 then begin
      for j := 0 to PageCount div 10 do begin
        k := j*10+1;
        if k>PageCount then
          break;
        M2 := NewPopupMenuItem(format(PageFromTo,[k,k+9]),-800,M);
        // Tag=-800 -> no OnClick event trigerred for this entry
        for i := k to k+9 do
          if i>PageCount then
            break else
            AddPage(M2);
      end;
    end else
      for i := 1 to PageCount do
        AddPage(M);
  end;
  if Assigned(OnPopupMenuPopup) then
    OnPopupMenuPopup(Sender);
end;


function TGDIPages.ExportPDF(aPdfFileName: TFileName; ShowErrorOnScreen: boolean;
  LaunchAfter: boolean): boolean;
{$ifdef USEPDFPRINTER}
var DefaultPrinter: integer;
{$else}
function ValidFileName(const FN: TFileName): TFileName;
var i: integer;
begin
  result := FN;
  for i := length(result) downto 1 do
    if ord(result[i]) in [ord('/'),ord(':'),ord('\'),ord('.')] then
      delete(result,i,1);
  i := length(Result);
  while (i>0) and (ord(result[i]) in [ord(' '),ord('-')]) do dec(i);
  SetLength(Result,i);
  result := trim(result);
end;
var PDF: TPDFDocument;
    PDFFileName: TFileName;
    i: integer;
    Name: string;
    PaperSize: TPDFPaperSize;
    Scale: Extended;
    TempDir: TFileName;
{$endif}
begin
  result := False;
  if Self=nil then
    exit;
  if PageCount>10 then
    Screen.Cursor := crHourGlass;
{$ifdef USEPDFPRINTER}
  if HasPDFPrinterInstalled then begin
    DefaultPrinter := Printer.PrinterIndex;
    Printer.PrinterIndex := fPDFPrinterIndex;
    PrintPages(0,0);
    Printer.PrinterIndex := DefaultPrinter;
  end;
{$else}
  // use the Synopse PDF engine
  if aPdfFileName='' then
  with TSaveDialog.Create(nil) do
  try
    TempDir := GetCurrentDir;
    Filter := sPDFFile+' (*.pdf)|*.pdf';
    Title := Caption;
    FileName := ValidFileName(Caption);
    DefaultExt := 'pdf';
    Options := [ofOverwritePrompt,ofHideReadOnly,ofEnableSizing];
    repeat
      if not Execute then
        exit;
      PDFFileName := FileName;
      i := FileCreate(PDFFileName); // test file create (pdf not already opened)
      if i>0 then break;
      MessageBox(0,pointer(Format(SIniFileWriteError,[PDFFileName])),nil,MB_ICONERROR);
    until false;
    FileClose(i);
  finally
    SetCurrentDir(TempDir); // allow unplug e.g. any USB
    Free;
  end else
    PDFFileName := aPdfFileName;
  PaperSize := SynPDF.CurrentPrinterPaperSize;
  Scale := Screen.PixelsPerInch/fPrinterPxPerInch.x; // resolution trick
  PDF := TPDFDocument.Create(UseOutlines,0,ExportPDFA1);
  try
    try
      //PDF.CompressionMethod := cmNone;
      with PDF.Info do begin
        Title := SysUtils.Trim(Caption);
        if ExportPDFApplication='' then
          Name := Application.Title else
          Name := ExportPDFApplication;
        Creator := Name;
        Author := ExportPDFAuthor;
        Subject := ExportPDFSubject;
        Keywords := ExportPDFKeywords;
      end;
      PDF.EmbeddedTTF := ExportPDFEmbeddedTTF;
      {$ifndef NO_USE_UNISCRIBE}
      PDF.UseUniscribe := ExportPDFUseUniscribe;
      {$endif}
      if PDF.DefaultPaperSize<>PaperSize then  // same page size
        PDF.DefaultPaperSize := PaperSize;
      if Orientation=poLandscape then begin // same orientation
        i := PDF.DefaultPageWidth;
        PDF.DefaultPageWidth :=  PDF.DefaultPageHeight;
        PDF.DefaultPageHeight := i;
      end;
      PDF.ForceJPEGCompression := ExportPDFForceJPEGCompression;
      for i := 0 to PageCount-1 do begin
        // this loop will do all the magic :)
        PDF.AddPage;
        PDF.Canvas.RenderMetaFile(TMetaFile(Pages.Objects[i]),Scale);
      end;
      PDF.SaveToFile(PDFFileName);
    finally
      PDF.Free;
    end;
    if LaunchAfter then
      ShellExecute(Application.MainForm.Handle,'open',Pointer(PDFFileName),
        nil,nil,SW_NORMAL);
  except
    on E: Exception do begin // show any error raised during PDF creation
      if ShowErrorOnScreen then
        MessageBox(0,pointer(E.Message),Pointer(Name),MB_ICONERROR);
      exit;
    end;
  end;
{$endif}
  result := true;
  if PageCount>10 then
    Screen.Cursor := crDefault;
end;

procedure TGDIPages.WMEraseBkgnd(var Message: TWmEraseBkgnd);
var R: TRect;
begin
  Message.Result := 1; // no erasing is necessary after this method call
  if Message.DC=0 then exit;
  // erase outside the preview surface
  R.Left := 0;
  R.Right := fPreviewSurface.left;
  R.Top := 0;
  R.Bottom := Height;
  FillRect(Message.DC,R,Brush.Handle);
  R.Left := R.Right+fPreviewSurface.Width;
  R.Right := Width;
  FillRect(Message.DC,R,Brush.Handle);
  R.Left := 0;
  R.Bottom := fPreviewSurface.Top;
  FillRect(Message.DC,R,Brush.Handle);
  R.Top := fPreviewSurface.Top+fPreviewSurface.Height;
  R.Bottom := Height;
  FillRect(Message.DC,R,Brush.Handle);
end;

procedure TGDIPages.AppendRichEdit(RichEditHandle: HWnd);
var Range: TFormatRange;
    LogX, LogY, LastChar, MaxLen, OldMap: integer;
    TextLenEx: TGetTextLengthEx; // RichEdit 2.0 Window Class
begin
  if (Self<>nil) and (fCanvas<>nil) then
  with Range do begin
    LogX := GetDeviceCaps(fCanvas.Handle, LOGPIXELSX);
    LogY := GetDeviceCaps(fCanvas.Handle, LOGPIXELSY);
    rcPage.Left := (fPageMarginsPx.Left*1440) div LogX;
    rcPage.Right := ((fPhysicalSizePx.x-fPageMarginsPx.Right)*1440) div LogX;
    rcPage.Top := ((fPageMarginsPx.Top+fHeaderHeight)*1440) div LogY;
    rcPage.Bottom := ((fPhysicalSizePx.y-fPageMarginsPx.Bottom-fFooterHeight)*1440) div LogY;
    CheckHeaderDone;
    rc := rcPage;
    rc.Top := (fCurrentYPos*1440) div LogY;
    LastChar := 0;
    with TextLenEx do begin
      flags := GTL_DEFAULT;
      codepage := CP_ACP;
    end;
    MaxLen := SendMessage(RichEditHandle, EM_GETTEXTLENGTHEX, Integer(@TextLenEx), 0);
    chrg.cpMax := -1;
    OldMap := SetMapMode(hdc, MM_TEXT);
    try
      SendMessage(RichEditHandle, EM_FORMATRANGE, 0, 0);
      repeat
        chrg.cpMin := LastChar;
        hdc := fCanvas.Handle;
        hdcTarget := hdc;
        LastChar := SendMessage(RichEditHandle, EM_FORMATRANGE, 1, Integer(@Range));
        if cardinal(LastChar)>=cardinal(MaxLen) then
          break;
        NewPageInternal;
        DoHeader;
        rc := rcPage;
      until false;
      fCurrentYPos := (rc.Bottom*LogY) div 1440;
    finally
      SendMessage(RichEditHandle, EM_FORMATRANGE, 0, 0);
      SetMapMode(hdc, OldMap);
    end;
  end;
end;

function TGDIPages.AddBookMark(const aBookmarkName: string; aYPosition: integer=0): boolean;
begin
  if fBookmarks.IndexOf(aBookmarkName)>=0 then // avoid duplicate bookmarks
    result := false else begin
    if aYPosition=0 then begin
      CheckYPos;
      aYPosition := fCurrentYPos;
    end;
    fBookMarks.AddObject(aBookmarkName,
      TGDIPagereference.Create(PageCount,0,aYPosition,0,0));
    {$ifndef USEPDFPRINTER}
    fCanvas.MoveTo(0,aYPosition);
    GDICommentBookmark(fCanvas.Handle,StringToUTF8(aBookmarkName));
    {$endif}
    result := true;
  end;
end;

procedure TGDIPages.GotoPosition(aPage: integer; aYPos: integer);
begin
  Page := aPage;
  HorzScrollbar.Position := 0;
  VertScrollbar.Position := (aYPos*VertScrollbar.Range) div fPhysicalSizePx.y
end;

function TGDIPages.GotoBookmark(const aBookmarkName: string): Boolean;
var i: integer;
begin
  i := fBookmarks.IndexOf(aBookmarkName);
  result := i>=0;
  if result then
    with TGDIPagereference(fBookmarks.Objects[i]) do
      GotoPosition(Page,Rect.Top);
end;

procedure TGDIPages.AddOutline(const aTitle: string; aLevel: Integer;
  aYPosition: integer=0; aPageNumber: integer=0);
begin
  if aPageNumber=0 then
    aPageNumber := PageCount;
  if aYPosition=0 then begin
    CheckYPos;
    aYPosition := fCurrentYPos;
  end;
  fOutline.AddObject(aTitle,
    TGDIPagereference.Create(aPageNumber,0,aYPosition,0,aLevel));
  {$ifndef USEPDFPRINTER}
  fCanvas.MoveTo(0,aYPosition);
  GDICommentOutline(fCanvas.Handle, StringToUtf8(aTitle),aLevel);
  {$endif}
end;

procedure TGDIPages.AddLink(const aBookmarkName: string; aRect: TRect; aPageNumber: integer=0);
begin
  if aPageNumber=0 then
    aPageNumber := PageCount;
  aRect := MmToPrinter(aRect);
  with aRect do
    fLinks.AddObject(aBookmarkName,
      TGDIPagereference.Create(aPageNumber,Left,Top,Right,Bottom));
  {$ifndef USEPDFPRINTER}
  GDICommentLink(fCanvas.Handle,StringToUtf8(aBookmarkName),aRect);
  {$endif}
end;


{ TGDIPagereference }

constructor TGDIPagereference.Create(PageNumber: integer; Left, Top, Right,
  Bottom: integer);
begin
  inherited Create;
  Page := PageNumber;
  Rect.Left := Left;
  Rect.Top := Top;
  Rect.Right := Right;
  Rect.Bottom := Bottom;
end;

procedure TGDIPagereference.ToPreview(Pages: TGDIPages);
var W,H: integer;
begin // do it for all pages (zoom is not reset between page shift)
  if Page<>0 then
    with Pages.fPreviewSurface do begin
      W := Width-GRAY_MARGIN*2;
      H := Height-GRAY_MARGIN*2;
      Preview.Left := GRAY_MARGIN+(Rect.Left*W) div Pages.fPhysicalSizePx.x;
      Preview.Right := GRAY_MARGIN+(Rect.Right*W) div Pages.fPhysicalSizePx.x;
      Preview.Top := GRAY_MARGIN+(Rect.Top*H) div Pages.fPhysicalSizePx.y;
      Preview.Bottom := GRAY_MARGIN+(Rect.Bottom*H) div Pages.fPhysicalSizePx.y;
    end;
end;


{ THeaderFooter }

constructor THeaderFooter.Create(Report: TGDIPages; doubleline: boolean;
  const aText: SynUnicode=''; IsText: boolean=false);
begin
  Text := aText;
  State := Report.SavedState;
  if not IsText then
    if doubleline then
      State.Flags := State.Flags or FORMAT_DOUBLELINE else
      State.Flags := State.Flags or FORMAT_SINGLELINE;
end;


{$ifdef RENDERPAGES}

{ TRenderPages }

procedure TRenderPages.Clear;
begin
  inherited;
  fRdrCol.Clear;
  fFontCache.Clear;
end;

constructor TRenderPages.Create(AOwner: TComponent);
begin
  inherited;
  fRdr := TRenderBox.Create(self);
  fRdrCol := TObjectList.Create;
  fFontCache := TObjectList.Create;
end;

destructor TRenderPages.Destroy;
begin
  inherited;
  FreeAndNil(fRdrCol);
  FreeAndNil(fRdr);
  FreeAndNil(fFontCache);
end;

function TRenderPages.GetCurrentFontCacheIndex: integer;
var F: TFont;
begin
  for result := 0 to fFontCache.Count-1 do
    with TFont(fFontCache.List[result]) do
      if (Color=Font.Color) and (Height=Font.Height) and (Style=Font.Style) and
         (Name=Font.Name) then
        exit;
  F := TFont.Create;
  F.Assign(Font);
  result := fFontCache.Add(F);
end;

function TRenderPages.GetCurrentFontCacheIndexAndSelect: integer;
var H: HDC;
begin
  result := GetCurrentFontCacheIndex;
  H := Canvas.Handle;
  with TFont(fFontCache[result]) do begin // same as TCanvas.CreateFont
    SelectObject(H,Handle);
    SetTextColor(H,ColorToRGB(Color));
    if length(fFontCacheSpace)<fFontCache.Count then
      SetLength(fFontCacheSpace,fFontCache.Count+20);
    if fFontCacheSpace[result].cx=0 then
      GetTextExtentPoint32W(H,' ',1,fFontCacheSpace[result]);
  end;
end;

function TRenderPages.GetSavedRender: TSavedStateRender;
begin
  with result do begin
    FirstLineIndent := ParagraphFirstLineIndent;
    Before := ParagraphBefore;
    After := ParagraphAfter;
    RightIndent := ParagraphRightIndent;
    LeftIndent := ParagraphLeftIndent;
  end;
end;

procedure TRenderPages.NewPageInternal;
begin
  { TODO : close any pending paragraph }
  inherited;
end;

procedure TRenderPages.RdrParagraph;
begin
  if ParagraphBefore<>0 then
    CurrentYPos := CurrentYPos+ParagraphBefore;
  Rdr.Flush(fPageMarginsPx.left,fCurrentYPos,false,0,False);
  if ParagraphAfter<>0 then
    CurrentYPos := CurrentYPos+ParagraphAfter;
end;

procedure TRenderPages.RdrPard;
var State: TSavedState;
begin
  if self=nil then
    exit;
  fAlign := taLeft;
  SetSavedRender(fDefaultStateRender);
  State := SavedState;
  if State.Flags<>fDefaultState.Flags then begin
    State.Flags := fDefaultState.Flags;
    SavedState := State; // will set FORMAT_RIGHT/CENTER/JUSTIFIED
  end;
end;

procedure TRenderPages.RdrPardPlain;
begin
  if self=nil then
    exit;
  if fDefaultState.FontName='' then
    RdrPlain else
    SavedState := fDefaultState;
  SetSavedRender(fDefaultStateRender);
end;

procedure TRenderPages.RdrPlain;
var State: TSavedState;
begin
  if self=nil then
    exit;
  if (fDefaultState.FontName='') or (fDefaultState.Flags=0) then begin
    Font.Size := 12;
    Font.Style := [];
    Font.Color := clBlack;
  end else begin
    State := fDefaultState;
    State.Flags :=
      // void FORMAT_RIGHT/CENTER/JUSTIFIED
      (State.Flags and not (FORMAT_RIGHT or FORMAT_CENTER or FORMAT_JUSTIFIED)) or
      // keep current FORMAT_RIGHT/CENTER/JUSTIFIED
      (TextFormatsToFlags and (FORMAT_RIGHT or FORMAT_CENTER or FORMAT_JUSTIFIED));
    SavedState := State;
  end;
end;

procedure TRenderPages.RdrSetCurrentStateAsDefault;
begin
  fDefaultState := SavedState;
  fDefaultStateRender := GetSavedRender;
end;

function TRenderPages.RdrTableBegin(const PercentWidth: array of integer): Boolean;
var i, sum, w: integer;
    col: TRenderBox;
begin
  result := (Self<>nil) and (fRdrCol.Count=0);
  if not result then
    exit;
  sum := 0;
  for i := 0 to high(PercentWidth) do
    inc(sum,PercentWidth[i]);
  if sum<=0 then begin
    result := false;
    exit;
  end;
  w := fPhysicalSizePx.x-fPageMarginsPx.Left-fPageMarginsPx.right;
  for i := 0 to high(PercentWidth) do begin
    col := TRenderBox.Create(self);
    col.Width := (w*100)div sum;
    fRdrCol.Add(col);
  end;
end;

function TRenderPages.RdrTableColumn(aColumnIndex: Integer): TRenderBox;
begin
  if (Self=nil) or (cardinal(aColumnIndex)>=cardinal(fRdrCol.Count-1)) then
    result := nil else
    result := TRenderBox(fRdrCol.List[aColumnIndex]);
end;

function TRenderPages.RdrTableEnd: Boolean;
begin
  result := (Self<>nil) and (fRdrCol.Count>0);
  if not result then
    exit;

  fRdrCol.Clear;
end;

procedure TRenderPages.RestoreSavedLayout;
begin
  if Self=nil then exit; // avoid GPF
  if fSavedCount>=length(fSavedRender) then
    Setlength(fSavedRender,fSavedCount+20);
  fSavedRender[fSavedCount] := GetSavedRender;
  inherited;
end;

procedure TRenderPages.SaveLayout;
begin
  if Self=nil then exit; // avoid GPF
  if fSavedCount<=0 then
    exit;
  inherited;
  SetSavedRender(fSavedRender[fSavedCount]);
end;

procedure TRenderPages.SetSavedRender(const State: TSavedStateRender);
begin
 with State do begin
    ParagraphFirstLineIndent := FirstLineIndent;
    ParagraphBefore := Before;
    ParagraphAfter := After;
    ParagraphRightIndent := RightIndent;
    ParagraphLeftIndent := LeftIndent;
  end;
end;


{ TRenderBox }

procedure TRenderBox.AddText(const s: string);
var PW: PWideChar;
    PWLen: integer;
begin
  if (self=nil) or (Owner=nil) then
    exit; // avoid GPF
  // convert text to unicode and add to fText[] internal buffer
  Owner.InternalUnicodeString(s,PW,PWLen,nil);
  AddText(PW,PWLen);
end;

procedure TRenderBox.AddText(PW: PWideChar; PWLen: integer);
var PDBeg, PD: PWideChar;
    aFontIndex, aFontSpaceWidth: integer;
begin
  if (self=nil) or (Owner=nil) or (PWLen=0) then
    exit; // avoid GPF
  if PWLen+fTextLen>length(fText) then
    SetLength(fText,length(fText)+PWLen+1024);
  PD := @fText[fTextLen];
  inc(fTextLen,PWLen);
  // create associated word markers
  aFontIndex := Owner.GetCurrentFontCacheIndexAndSelect;
  aFontSpaceWidth := Owner.fFontCacheSpace[aFontIndex].cx;
  repeat
    PDBeg := PD;
    while true do
      case integer(PW^) of
        0, 32: break;
        1..31: if PD<>PDBeg then break else Inc(PW);
        else begin
          PD^ := PW^;
          inc(PW);
          inc(PD);
        end;
      end;
    if fBoxCount>=Length(fBox) then
      SetLength(fBox,fBoxCount+200);
    with fBox[fBoxCount] do begin
      TextOffset := PD-@fText[0];
      TextLength := PD-PDBeg;
      FontIndex := aFontIndex;
      FontSpaceWidth := aFontSpaceWidth;
      GetTextExtentPoint32W(Owner.Canvas.Handle,PDBeg,TextLength,Size);
      SpaceAfterCount := 0;
      while integer(PW^) in [1..32] do begin
        PD^ := ' ';
        inc(PW);
        inc(PD);
        inc(SpaceAfterCount);
      end;
      LinkNumber := fLinksBookMarkNameCurrent;
    end;
    inc(fBoxCount);
  until PW^=#0;
end;

procedure TRenderBox.Clear;
begin
  if Self=nil then
    exit;
  Finalize(fLinksBookMarkName);
  Finalize(fBox);
  fLayoutCount := 0;
  fBoxCount := 0;
  fTextLen := 0;
  fHeight := 0;
  fLinksBookMarkNameCurrent := 0;
end;

constructor TRenderBox.Create(Owner: TRenderPages);
begin
  fOwner := Owner;
  fBiDiMode := Owner.BiDiMode;
  fOwnerFont := Owner.Font;
end;

/// format the already inserted text into the TRenderPages owner
// - this TRenderBox text content will be cleared at the end of this method
// - you don't have to call it usualy: use Owner.RdrParagraph instead
// - by default, will render top aligned to the X=Left/Y=Top position
// - for vertical alignment, specify an height in ForcedHeightBottomCentered
// then will be centered if ForcedAtBottom=false, or bottom aligned if true
// - if CurrentPageOnly is true, will only flush the content which will fit on
// the current page - then the fLayout[] array will contain remaining boxes; otherwise,
// this will flush all content to multiple pages

procedure TRenderBox.Flush(Left, Top: Integer; CurrentPageOnly: boolean;
  ForcedHeightBottomCentered: Integer; ForcedAtBottom: boolean);
var H, Y, i, fitLayout: integer;
    WillBreak: boolean;
begin
  if (self=nil) or (Owner=nil) then
    exit; // avoid GPF
  H := GetHeight; // will populate fLayout[] from fBox[] if necessary
  { render on document Canvas }
  WillBreak := false;
  fitLayout := fLayoutCount-1;
  for i := 0 to fitLayout do
    if fLayout[i].Top>=H then begin
      fitLayout := i-1;
      WillBreak := true;
      break;
    end;

  { TODO : handle TGDIPagereference creation from fLayout[].LastBox.LinkNumber }
  // reset internal TRenderBox content
  Clear;
end;

function TRenderBox.GetHeight: integer;
begin
  if self=nil then
    result := 0 else begin
    if fHeight=0 then
      // need to recalculate the layout to refresh the resulting Height
      InternalRender;
    result := fHeight;
  end;
end;

procedure TRenderBox.InternalRender;
var ndx, ndxFirst: integer;
    X, Y, H, W, LineW: integer;
    txt: PWideChar;
    Box: PRenderBoxWord;
    LineLayout, LineNdx: integer;
procedure AddLayout(DoLineFeed, LastLine: boolean);
var nspace, Adjust, i, j, aLeft, n: Integer;
    align: TTextAlign;
    TmpLayout: array of TRenderBoxLayout;
begin
  if fLayoutCount>=length(fLayout) then
    SetLength(fLayout,fLayoutCount+50);
  with fLayout[fLayoutCount] do begin
    Text := txt;
    with Box^ do
      txt := @fText[TextOffset+TextLength]; // txt^ points to ' ' after text
    Length := txt-Text;
    Left := X;
    Top := Y;
    Width := W;
    LineIndex := LineNdx;
    LastBox := Box;
    BreakExtra := 0;
    BreakCount := 0;
  end;
  if DoLineFeed then begin
    // we must handle the line feed layout 
    Align := Owner.TextAlign;
    Adjust := LineW-(X+W);
    if (Adjust<=0) or
       // force left align if wider than expected (i.e. overpass right margin)
       (LastLine and (Align=taJustified)) then
       // last line of justified paragraph is never justified
      Align := taLeft;
    if BiDiMode=bdRightToLeft then begin
      case Align of
        taLeft:  Align := taRight;
        taRight: Align := taLeft;
      end;
      n := fLayoutCount-LineLayout+1;
      if n>1 then begin
        // multi layouts: change logical to visual order for RTL languages
        SetLength(TmpLayout,n);
        Move(fLayout[LineLayout],TmpLayOut[0],n*sizeof(TmpLayOut[0]));
        aLeft := fLayout[LineLayout].Left;
        for i := 0 to n-1 do begin
          move(TmpLayout[i],fLayout[fLayoutCount-i],sizeof(TmpLayOut[0]));
          with fLayout[fLayoutCount-i] do begin
            Left := aLeft;
            Inc(aLeft,Width+LastBox^.FontSpaceWidth*LastBox^.SpaceAfterCount);
          end;
        end;
      end;
    end;
    case Align of
      taRight:
        for i := LineLayout to fLayoutCount do
          inc(fLayout[i].Left,Adjust);
      taCenter: begin
        Adjust := Adjust div 2;
        for i := LineLayout to fLayoutCount do
          inc(fLayout[i].Left,Adjust);
      end;
      taJustified:
      if Adjust>0 then begin
        // compute SetTextJustification() values and update Left position
        aLeft := fLayout[LineLayout].Left;
        nspace := 0;
        for i := LineLayout to fLayoutCount do
          with fLayout[i] do begin
            for j := 0 to Length-1 do
              if Text[j]=' ' then
                inc(BreakCount);
            inc(nspace,BreakCount);
          end;
        if nspace>0 then
          for i := LineLayout to fLayoutCount do
          with fLayout[i] do begin
            Left := aLeft;
            BreakExtra := (Adjust*BreakCount) div nspace;
            dec(Width,LastBox^.FontSpaceWidth*BreakCount-BreakExtra);
            inc(aLeft,Width);
          end;
      end;
    end;
    for i := LineLayout to fLayoutCount do
      fLayout[i].Height := H; // same height for all fLayout[] of this line
    with Owner do
      X := MmToPrinterPxX(ParagraphLeftIndent);
    inc(Y,H);
    H := 0; // force recalculate line height
    LineLayout := fLayoutCount;
    inc(LineNdx);
  end else begin
    // just append this "word" box to fLayout[fLayoutCount]
    with Box^ do
      // compute next position
      inc(X,W+FontSpaceWidth*SpaceAfterCount);
  end;
  inc(fLayoutCount);
  ndxFirst := ndx+1;
  W := 0;
end;
begin // compute TRenderBoxWord.X/Y and fHeight
  fHeight := 0;
  fLayoutCount := 0;
  SetLength(fLayout,fBoxCount shr 2);
  if fBoxCount=0 then
    exit; // no text added
  with Owner do begin
    X := MmToPrinterPxX(ParagraphFirstLineIndent);
    LineW := self.fWidth-MmToPrinterPxX(ParagraphRightIndent);
  end;
  LineNdx := 0;
  Y := 0;
  H := 0;
  W := 0;
  LineLayout := 0;
  txt := @fText[0];
  ndxFirst := 0;
  for ndx := 0 to fBoxCount-1 do begin
    Box := @fBox[ndx];
    if Box^.Size.cy>H then
      H := Box^.Size.cy;
    inc(W,Box^.Size.cx);
    if ndx=fBoxCount-1 then
      // reached last box -> flush pending line content
      AddLayout(true,true) else
    with fBox[ndx+1] do
      if X+W+Size.cx>LineW then
        // not enough space in current line -> flush+adjust and go to next line
        AddLayout(true,false) else
      if (FontIndex<>Box^.FontIndex) or (LinkNumber<>Box^.LinkNumber) then
        // text formatting or Link will change -> add a layout box
        AddLayout(false,false);
  end;
  fHeight := Y;
end;

procedure TRenderBox.LinkBegin(const aBookmarkName: string);

begin
  if (self=nil) or (Owner=nil) then
    exit; // avoid GPF
  LinkEnd; // no nested links
  fLinksBookMarkNameCurrent := Length(fLinksBookMarkName)+1;
  SetLength(fLinksBookMarkName,fLinksBookMarkNameCurrent);
  fLinksBookMarkName[fLinksBookMarkNameCurrent-1] := aBookmarkName;
end;

function TRenderBox.LinkEnd: boolean;
begin
  result := false;
  if (self=nil) or (Owner=nil) or (fLinksBookMarkNameCurrent=0) then
    exit; // avoid GPF
  fLinksBookMarkNameCurrent := 0;
  result := true;
end;

procedure TRenderBox.NewLine;
begin
  if (self=nil) or (Owner=nil) then
    exit; // avoid GPF

end;

procedure TRenderBox.Pard;
begin
  if (self<>nil) and (Owner<>nil) then // avoid GPF
    Owner.RdrPard;
end;

procedure TRenderBox.PardPlain;
begin
  if (self<>nil) and (Owner<>nil) then // avoid GPF
    Owner.RdrPardPlain;
end;

procedure TRenderBox.Plain;
begin
  if (self<>nil) and (Owner<>nil) then // avoid GPF
    Owner.RdrPlain;
end;

{$endif RENDERPAGES}

end.

