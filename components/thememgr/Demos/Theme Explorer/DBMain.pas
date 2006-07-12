unit DBMain;

// This unit contains the main form of the Windows XP theme explorer. It demonstrates the abilities of the
// Theme services/manager classes.
//
// Author: Dipl. Ing. Mike Lischke (public@soft-gems.de).
//         www.lischke-online.de
//         www.delphi-unicode.net
//         www.delphi-gems.com

interface

uses 
  Windows, SysUtils, Forms,
  ThemeMgrDB, ThemeSrv, DBGrids, DB, DBTables, Grids, Menus, ImgList, DBCtrls, ComCtrls, ExtCtrls,
  Graphics, StdCtrls, Controls, Classes;

type
  TDBMainForm = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    OSLabel: TLabel;
    ThemeLabel: TLabel;
    StatusBar: TStatusBar;
    Label4: TLabel;
    ThemePartsLabel: TLabel;
    PageControl1: TPageControl;
    ExplorerTabSheet: TTabSheet;
    ThemesEnableGroup: TRadioGroup;
    TabSheet7: TTabSheet;
    ElementsListBox: TListBox;
    DrawListBox: TListBox;
    Label3: TLabel;
    Label12: TLabel;
    CheckBox4: TCheckBox;
    Image1: TImage;
    Label26: TLabel;
    SmallImages: TImageList;
    PopupMenu1: TPopupMenu;
    Exit1: TMenuItem;
    N1: TMenuItem;
    PrintSetup1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    New1: TMenuItem;
    CheckBox7: TCheckBox;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    Table1: TTable;
    Table2: TTable;
    DataSource2: TDataSource;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    DBLookupListBox1: TDBLookupListBox;
    DBLookupComboBox1: TDBLookupComboBox;
    Label8: TLabel;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThemeManagerThemeChange(Sender: TObject);
    procedure ThemesEnableGroupClick(Sender: TObject);
    procedure ElementsListBoxClick(Sender: TObject);
    procedure DrawListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    FCollapsed: Boolean;
    FBackBitmap,
    FThumbnailBackground: TBitmap;
    procedure CreateDefaultBackground;
    procedure FadeBitmap(Bitmap: TBitmap);
    procedure FillBackground(R: TRect; Target: TBitmap);
    procedure FillExplorerListBox;
    procedure ReadComputerProperties;
    procedure UpdateThemeInfo;
  public
  end;

var
  DBMainForm: TDBMainForm;
  ThemeManager: TThemeManagerDB;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}
{$R WinXP.res}

uses
  TypInfo, UxTheme, ShellAPI, ThemeMgr;

const
  ElementName: array[TThemedElement] of string = (
    'Button',       // teButton
    'Clock',        // teClock
    'ComboBox',     // teComboBox
    'Edit',         // teEdit
    'Explorer bar', // teExplorerBar
    'Header',       // teHeader
    'Listview',     // teListView
    'Menu',         // teMenu
    'Page',         // tePage
    'Progress',     // teProgress
    'Rebar',        // teRebar
    'Scrollbar',    // teScrollBar
    'Spin',         // teSpin
    'Start panel',  // teStartPanel
    'Status',       // teStatus
    'Tab',          // teTab
    'Taskband',     // teTaskBand
    'Taskbar',      // teTaskBar
    'Toolbar',      // teToolBar
    'Tooltip',      // teToolTip
    'Trackbar',     // teTrackBar
    'Tray notify',  // teTrayNotify
    'Treeview',     // teTreeview
    'Window'        // teWindow
  );

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.CreateDefaultBackground;

// Creates the checkered default background for an entry.

begin
  FThumbnailBackground := TBitmap.Create;
  with FThumbnailBackground do
  begin
    Width := 16;
    Height := 16;
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    Canvas.Brush.Color := clBtnHighlight;
    Canvas.FillRect(Rect(0, 0, 8, 8));
    Canvas.FillRect(Rect(8, 8, 16, 16));
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.FadeBitmap(Bitmap: TBitmap);

// Very simple (and slow) fade routine (alpha blending against highlight color) for selections in the draw list box.

const
  Alpha = 0.25;
  OneMinusAlpha = 1 - Alpha;
  
type
  PBGRA = ^TBGRA;
  TBGRA = record
    B, G, R, A: Byte;
  end;

var
  X, Y: Integer;
  Run: PBGRA;
  R, G, B: Single;


begin
  R := GetRValue(ColorToRGB(clHighlight)) * Alpha;
  G := GetGValue(ColorToRGB(clHighlight)) * Alpha;
  B := GetBValue(ColorToRGB(clHighlight)) * Alpha;
  for Y := 0 to Bitmap.Height - 1 do
  begin
    Run := Bitmap.ScanLine[Y];
    for X := 0 to Bitmap.Width - 1 do
    begin
      Run.R := Round(OneMinusAlpha * Run.R + R);
      Run.G := Round(OneMinusAlpha * Run.G + G);
      Run.B := Round(OneMinusAlpha * Run.B + B);
      Inc(Run);
    end;
  end;
  with Bitmap do
  begin
    Canvas.Pen.Color := clHighlight;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.FillBackground(R: TRect; Target: TBitmap);

// Tiles the background image over the given target bitmap.

var
  X, Y: Integer;
  dX, dY: Integer;

begin
  with Target.Canvas do
  begin
    dX := FThumbnailBackground.Width;
    dY := FThumbnailBackground.Height;

    Y := 0;
    while Y < R.Bottom - R.Top do
    begin
      X := 0;
      while X < R.Right - R.Left do
      begin
        Draw(X, Y, FThumbnailBackground);
        Inc(X, dX);
      end;
      Inc(Y, dY);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.FillExplorerListBox;

// Fills the explorer listbox with all available element names.

var
  Element: TThemedElement;

begin
  FCollapsed := True;
  for Element := Low(TThemedElement) to High(TThemedElement) do
    ElementsListBox.Items.Add(ElementName[Element]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.ReadComputerProperties;

begin
  case Win32MajorVersion of
    3: // NT 3.51
      OSLabel.Caption := 'Windows NT 3.51';
    4: // Win9x/ME, NT 4
      case Win32MinorVersion of
        0:
          OSLabel.Caption := 'Windows 95';
        10:
          OSLabel.Caption := 'Windows 98';
        90:
          OSLabel.Caption := 'Windows ME';
      else
        if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
          OSLabel.Caption := 'Windows NT 4.0'
        else
          OSLabel.Caption := 'unknown';
      end;
    5: // Win2K, XP
      case Win32MinorVersion of
        0:
          OSLabel.Caption := 'Windows 2000';
        1:
          OSLabel.Caption := 'Windows XP or .NET server';
      else
        OSLabel.Caption := 'unknown';
      end;
  else
    OSLabel.Caption := 'unknown';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.UpdateThemeInfo;

var
  S: string;

begin
  with ThemeManager, ThemeServices do
  begin
    if ThemesAvailable then
    begin
      S := 'Themes are available ';
      if ThemesEnabled then
        S := S + 'and enabled.'
      else
        S := S + 'but not enabled.';
      ThemeLabel.Caption := S;
    end
    else
      ThemeLabel.Caption := 'Themes are not available.';

    S := '';
    if toAllowNonClientArea in Options then
      S := 'non client area';
    if toAllowControls in Options then
    begin
      if Length(S) > 0 then
        S := S +', ';
      S := S + 'controls (client area)';
    end;
    if toAllowWebContent in Options then
    begin
      if Length(S) > 0 then
        S := S +', ';
      S := S + 'web content';
    end;
    if Length(S) > 0 then
      ThemePartsLabel.Caption := S
    else
      ThemePartsLabel.Caption := 'none';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.Button1Click(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.FormCreate(Sender: TObject);

begin
  ThemeManager := TThemeManagerDB.Create(Self);
  ThemeManager.Name := 'DBThemeManager1';
  ThemeManager.OnThemeChange := ThemeManagerThemeChange;

  // If you create the theme manager manually then you have to tell it to collect all controls which must be subclassed.
  // This is not necessary if you create the manager via component palette and normal design time handling.
  ThemeManager.CollectForms;

  PageControl1.ActivePage := ExplorerTabsheet;
  ReadComputerProperties;
  UpdateThemeInfo;
  FillExplorerListBox;
  CreateDefaultBackground;

  FBackBitmap := TBitmap.Create;
  with FBackBitmap do
  begin
    PixelFormat := pf32Bit;
    Width := DrawListBox.ClientWidth;
    Height := DrawListBox.ItemHeight;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.ThemeManagerThemeChange(Sender: TObject);

begin
  UpdateThemeInfo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.ThemesEnableGroupClick(Sender: TObject);

begin
  if ThemesEnableGroup.ItemIndex = 0 then
    ThemeManager.Options := DefaultThemeOptions
  else                                                             
    ThemeManager.Options := DefaultThemeOptions - [toAllowControls, toAllowNonClientArea];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.ElementsListBoxClick(Sender: TObject);

const
  SupportStrings: array[Boolean] of string = (
    ' unsupported', ' supported'
  );

var
  Element: TThemedElement;
  Count: Integer;

begin
  Element := TThemedElement(ElementsListBox.ItemIndex);

  // I cannot use the virtual mode of the list box here because this is not known before Delphi 6.
  DrawListBox.Items.Clear;
  with ThemeManager do
  begin
    case Element of
      teButton:
        Count := Ord(High(TThemedButton));
      teClock:
        Count := Ord(High(TThemedClock));
      teComboBox:
        Count := Ord(High(TThemedComboBox));
      teEdit:
        Count := Ord(High(TThemedEdit));
      teExplorerBar:
        Count := Ord(High(TThemedExplorerBar));
      teHeader:
        Count := Ord(High(TThemedHeader));
      teListView:
        Count := Ord(High(TThemedListview));
      teMenu:
        Count := Ord(High(TThemedMenu));
      tePage:
        Count := Ord(High(TThemedPage));
      teProgress:
        Count := Ord(High(TThemedProgress));
      teRebar:
        Count := Ord(High(TThemedRebar));
      teScrollBar:
        Count := Ord(High(TThemedScrollBar));
      teSpin:
        Count := Ord(High(TThemedSpin));
      teStartPanel:
        Count := Ord(High(TThemedStartPanel));
      teStatus:
        Count := Ord(High(TThemedStatus));
      teTab:
        Count := Ord(High(TThemedTab));
      teTaskBand:
        Count := Ord(High(TThemedTaskBand));
      teTaskBar:
        Count := Ord(High(TThemedTaskBar));
      teToolBar:
        Count := Ord(High(TThemedToolBar));
      teToolTip:
        Count := Ord(High(TThemedToolTip));
      teTrackBar:
        Count := Ord(High(TThemedTrackBar));
      teTrayNotify:
        Count := Ord(High(TThemedTrayNotify));
      teTreeview:
        Count := Ord(High(TThemedTreeview));
      teWindow:
        Count := Ord(High(TThemedWindow));
    else
      Count := -1;
    end;

    StatusBar.Panels[0].Text := ElementsListBox.Items[ElementsListBox.ItemIndex] + SupportStrings[ThemeServices.Theme[Element] <> 0];

    while Count > 0 do
    begin
      DrawListBox.Items.Add('');
      Dec(Count);
    end;
  end;
  
  FBackBitmap.Width := DrawListBox.ClientWidth;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.DrawListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

var
  Element: TThemedElement;
  Text: string;
  R, TextR: TRect;
  Details: TThemedElementDetails;

begin
  Inc(Index);
  Element := TThemedElement(ElementsListBox.ItemIndex);
  with ThemeManager, ThemeServices, FBackBitmap.Canvas do
  begin
    if FBackBitmap.Width <> DrawListBox.ClientWidth then
    begin
      // There is no notification if the width of the list box changed. So we check here and repaint the
      // entire window if its width differs from our back bitmap. 
      FBackBitmap.Width := DrawListBox.ClientWidth;
      DrawlistBox.Invalidate;
      Exit;
    end;

    R := Classes.Rect(0, 0, FBackBitmap.Width, FBackBitmap.Height);
    TextR := R;
    Inc(TextR.Top, 5);
    Inc(TextR.Left, 5);
    if CheckBox4.Checked then
      FillBackground(R, FBackBitmap)
    else
    begin
      Brush.Color := clWhite;
      FillRect(Classes.Rect(0, 0, FBackBitmap.Width, FBackBitmap.Height));
    end;
    if odFocused in State then
      FadeBitmap(FBackBitmap);

    if ThemesAvailable then
    begin
      Font.Color := clMaroon;
      case Element of
        teButton:
          begin
            Text := GetEnumName(TypeInfo(TThemedButton), Index);
            Details := GetElementDetails(TThemedButton(Index));
          end;
        teClock:
          begin
            Text := GetEnumName(TypeInfo(TThemedClock), Index);
            Details := GetElementDetails(TThemedClock(Index));
          end;
        teComboBox:
          begin
            Text := GetEnumName(TypeInfo(TThemedComboBox), Index);
            Details := GetElementDetails(TThemedComboBox(Index));
          end;
        teEdit:
          begin
            Text := GetEnumName(TypeInfo(TThemedEdit), Index);
            Details := GetElementDetails(TThemedEdit(Index));
          end;
        teExplorerBar:
          begin
            Text := GetEnumName(TypeInfo(TThemedExplorerBar), Index);
            Details := GetElementDetails(TThemedExplorerBar(Index));
          end;
        teHeader:
          begin
            Text := GetEnumName(TypeInfo(TThemedHeader), Index);
            Details := GetElementDetails(TThemedHeader(Index));
          end;
        teListView:
          begin
            Text := GetEnumName(TypeInfo(TThemedListview), Index);
            Details := GetElementDetails(TThemedListview(Index));
          end;
        teMenu:
          begin
            Text := GetEnumName(TypeInfo(TThemedMenu), Index);
            Details := GetElementDetails(TThemedMenu(Index));
          end;
        tePage:
          begin
            Text := GetEnumName(TypeInfo(TThemedPage), Index);
            Details := GetElementDetails(TThemedPage(Index));
          end;
        teProgress:
          begin
            Text := GetEnumName(TypeInfo(TThemedProgress), Index);
            Details := GetElementDetails(TThemedProgress(Index));
          end;
        teRebar:
          begin
            Text := GetEnumName(TypeInfo(TThemedRebar), Index);
            Details := GetElementDetails(TThemedRebar(Index));
          end;
        teScrollBar:
          begin
            Text := GetEnumName(TypeInfo(TThemedScrollBar), Index);
            Details := GetElementDetails(TThemedScrollBar(Index));
          end;
        teSpin:
          begin
            Text := GetEnumName(TypeInfo(TThemedSpin), Index);
            Details := GetElementDetails(TThemedSpin(Index));
          end;
        teStartPanel:
          begin
            Text := GetEnumName(TypeInfo(TThemedStartPanel), Index);
            Details := GetElementDetails(TThemedStartPanel(Index));
          end;
        teStatus:
          begin
            Text := GetEnumName(TypeInfo(TThemedStatus), Index);
            Details := GetElementDetails(TThemedStatus(Index));
          end;
        teTab:
          begin
            Text := GetEnumName(TypeInfo(TThemedTab), Index);
            Details := GetElementDetails(TThemedTab(Index));
          end;
        teTaskBand:
          begin
            Text := GetEnumName(TypeInfo(TThemedTaskBand), Index);
            Details := GetElementDetails(TThemedTaskBand(Index));
          end;
        teTaskBar:
          begin
            Text := GetEnumName(TypeInfo(TThemedTaskBar), Index);
            Details := GetElementDetails(TThemedTaskBar(Index));
          end;
        teToolBar:
          begin
            Text := GetEnumName(TypeInfo(TThemedToolBar), Index);
            Details := GetElementDetails(TThemedToolBar(Index));
          end;
        teToolTip:
          begin
            Text := GetEnumName(TypeInfo(TThemedToolTip), Index);
            Details := GetElementDetails(TThemedToolTip(Index));
          end;
        teTrackBar:
          begin
            Text := GetEnumName(TypeInfo(TThemedTrackBar), Index);
            Details := GetElementDetails(TThemedTrackBar(Index));
          end;
        teTrayNotify:
          begin
            Text := GetEnumName(TypeInfo(TThemedTrayNotify), Index);
            Details := GetElementDetails(TThemedTrayNotify(Index));
          end;
        teTreeview:
          begin
            Text := GetEnumName(TypeInfo(TThemedTreeview), Index);
            Details := GetElementDetails(TThemedTreeview(Index));
          end;
        teWindow:
          begin
            Text := GetEnumName(TypeInfo(TThemedWindow), Index);
            Details := GetElementDetails(TThemedWindow(Index));
          end;
      end;

      DrawText(Handle, Details, Text, TextR, DT_SINGLELINE, 0);
      InflateRect(R, -10, -5);
      Inc(R.Top, TextHeight(Text) + 5);
      DrawElement(Handle, Details, R);
    end;
  end;

  DrawListBox.Canvas.Draw(Rect.Left, Rect.Top, FBackBitmap);

  // The listbox will also draw a focus rectangle. Both together will result in no focus rect.
  if odFocused in State then
    DrawListBox.Canvas.DrawFocusRect(Rect);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.FormDestroy(Sender: TObject);

begin
  FThumbnailBackground.Free;
  FBackBitmap.Free;
  ThemeManager.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.CheckBox4Click(Sender: TObject);

begin
  if TListBox(Sender).Tag = 0 then
    ElementsListBox.Invalidate;
  DrawListBox.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.PageControl1Change(Sender: TObject);

begin
  StatusBar.Panels[0].Text := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.Image1Click(Sender: TObject);

begin
  ShellExecute(0, 'open', 'www.soft-gems.net/ThemeManager.php', nil, nil, SW_SHOW);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TDBMainForm.CheckBox1Click(Sender: TObject);

begin
  DBNavigator1.Flat := CheckBox1.Checked;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
