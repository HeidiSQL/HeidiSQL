unit Main;

// This unit contains the main form of the Windows XP theme explorer. It demonstrates the abilities of the
// Theme services/manager classes.
//
// Author: Mike Lischke (public@soft-gems.net).
//         www.soft-gems.net

interface
                                         
{$I ../../../compilerdetection/compilers.inc}
{$R WinXP.res}

uses 
  ThemeMgr, ThemeSrv, Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls,
  ExtCtrls, ComCtrls, Graphics, CheckLst, Buttons, ToolWin,
  Menus, ImgList, TestFrame, Grids;

type
  TMainForm = class(TForm)
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
    CommonControlsTabSheet: TTabSheet;
    ListView1: TListView;
    TabSheet4: TTabSheet;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    Label5: TLabel;
    Edit1: TEdit;
    Label6: TLabel;
    RadioGroup1: TRadioGroup;
    TreeView1: TTreeView;
    ThemesEnableGroup: TRadioGroup;
    RichEdit1: TRichEdit;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    GroupBox2: TGroupBox;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    TabSheet6: TTabSheet;
    Animate1: TAnimate;
    UpDown1: TUpDown;
    HotKey1: THotKey;
    DateTimePicker1: TDateTimePicker;
    CoolBar1: TCoolBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    TabSheet7: TTabSheet;
    LeftPaintBox: TPaintBox;
    ElementsListBox: TListBox;
    DrawListBox: TListBox;
    Label3: TLabel;
    Label12: TLabel;
    CheckBox4: TCheckBox;
    RightPaintBox: TPaintBox;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    GroupBox3: TGroupBox;
    RadioButton2: TRadioButton;
    RadioButton1: TRadioButton;
    CheckBox3: TCheckBox;
    CheckBox2: TCheckBox;
    GroupBox4: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    BitBtn1: TBitBtn;
    Button4: TButton;
    BitBtn2: TBitBtn;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    ComboBox1: TComboBox;
    Label23: TLabel;
    ScrollBar1: TScrollBar;
    Label24: TLabel;
    Button2: TButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    CheckBox1: TCheckBox;
    CheckBox5: TCheckBox;
    Memo2: TMemo;
    Edit2: TEdit;
    Image1: TImage;
    Label26: TLabel;
    TabSheet1: TTabSheet;
    PageControl3: TPageControl;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    RadioGroup2: TRadioGroup;
    Button3: TButton;
    BitBtn3: TBitBtn;
    TabSheet2: TTabSheet;
    StressTestStartButton: TButton;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    HeavyLoadButton: TButton;
    Label31: TLabel;
    SmallImages: TImageList;
    ToolBar2: TToolBar;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
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
    Label34: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Label35: TLabel;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Panel2: TPanel;
    Panel3: TPanel;
    Label32: TLabel;
    MiscTabSheet: TTabSheet;
    Button5: TButton;
    Label33: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Bevel1: TBevel;
    ScrollBox1: TScrollBox;
    StringGrid1: TStringGrid;
    Label40: TLabel;
    TabSheet3: TTabSheet;
    Label36: TLabel;
    Panel4: TPanel;
    SpeedButton1: TSpeedButton;
    Button6: TButton;
    RadioGroup3: TRadioGroup;
    BitBtn4: TBitBtn;
    CheckBox6: TCheckBox;
    RadioButton5: TRadioButton;
    Label37: TLabel;
    Frame11: TFrame1;
    Label41: TLabel;
    CheckBox7: TCheckBox;
    Label25: TLabel;
    CheckListBox1: TCheckListBox;
    Label42: TLabel;
    StaticText1: TStaticText;
    Label43: TLabel;
    DLLFormButton: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ThemeManagerThemeChange(Sender: TObject);
    procedure ThemesEnableGroupClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure LeftPaintBoxPaint(Sender: TObject);
    procedure ElementsListBoxClick(Sender: TObject);
    procedure DrawListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure RightPaintBoxPaint(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure StressTestStartButtonClick(Sender: TObject);
    procedure HeavyLoadButtonClick(Sender: TObject);
    procedure PageControl3Change(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure DLLFormButtonClick(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    FCollapsed: Boolean;
    FBackBitmap,
    FThumbnailBackground: TBitmap;
    FCheckPending: Boolean;
    procedure ControlMessage(Sender: TThemeManager; Control: TControl; var Message: TMessage; var Handled: Boolean);
    procedure CreateDefaultBackground;
    procedure FadeBitmap(Bitmap: TBitmap);
    procedure FillBackground(R: TRect; Target: TBitmap);
    procedure FillExplorerListBox;
    procedure ReadComputerProperties;
    procedure UpdateThemeInfo;
  public
  end;

var
  MainForm: TMainForm;
  ThemeManager: TThemeManager;

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  TypInfo, UxTheme, SubForm1, SubForm2, SubForm3, Dialogs, ShellAPI, MSHTML;

const
  TestDuration = 10000; // Duration of the stress test in milliseconds.

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

procedure TMainForm.ControlMessage(Sender: TThemeManager; Control: TControl; var Message: TMessage;
  var Handled: Boolean);

var
  R: TRect;
  SaveFont: HFONT;

begin
  with ThemeManager, ThemeServices do
  begin
    if (Control is TListBox) and CheckBox7.Checked and ThemesEnabled then
      case Message.Msg of
        CN_CTLCOLORLISTBOX:
          with TWMCtlColorListBox(Message), Control as TListBox do
          begin
            // Draw background transparently.
            SetBkMode(ChildDC, TRANSPARENT);
            R := ItemRect(ItemIndex);
            // DrawThemeParentBackground changes the font in the given DC. This is a bug in my opinion, but
            // we cannot fix it so we need a work-around.
            SaveFont := GetCurrentObject(ChildDC, OBJ_FONT);
            DrawParentBackground(Handle, ChildDC, nil, False, @R);
            SelectObject(ChildDC, SaveFont);
            // Return an empty brush to prevent Windows from overpainting we just have created.
            Result := GetStockObject(NULL_BRUSH);
            Handled := True; // Do not process this message further.
          end;
        WM_ERASEBKGND:
          with TWMEraseBkGnd(Message), Control as TListBox do
          begin
            // Get the parent to draw its background into the control's background.
            DrawParentBackground(Handle, DC, nil, False);
            Result := 1;
            Handled := True; // Do not process this message further.
          end;
      end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CreateDefaultBackground;

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

procedure TMainForm.FadeBitmap(Bitmap: TBitmap);

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

procedure TMainForm.FillBackground(R: TRect; Target: TBitmap);

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

procedure TMainForm.FillExplorerListBox;

// Fills the explorer listbox with all available element names.

var
  Element: TThemedElement;

begin
  FCollapsed := True;
  for Element := Low(TThemedElement) to High(TThemedElement) do
    ElementsListBox.Items.Add(ElementName[Element]);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ReadComputerProperties;

begin
  case Win32MajorVersion of
    3: // NT 3.51
      case Win32MinorVersion of
        51:
          OSLabel.Caption := 'Windows NT 3.51';
      else
        OSLabel.Caption := 'unknown';
      end;
    4: // Win9x/ME, NT 4
      case Win32MinorVersion of
        0:
          if (Win32Platform and VER_PLATFORM_WIN32_NT) <> 0 then
            OSLabel.Caption := 'Windows NT 4.0'
          else
            OSLabel.Caption := 'Windows 95';
        10:
          OSLabel.Caption := 'Windows 98';
        90:
          OSLabel.Caption := 'Windows ME';
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

procedure TMainForm.UpdateThemeInfo;

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

procedure TMainForm.Button1Click(Sender: TObject);

begin
  Close;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);

begin
  Panel4.DoubleBuffered := True;
  Panel1.DoubleBuffered := True;

  ThemeManager := TThemeManager.Create(Self);
  ThemeManager.Name := 'ThemeManager1';
  ThemeManager.OnThemeChange := ThemeManagerThemeChange;
  ThemeManager.RegisterListener(ControlMessage);

  // If you create the theme manager manually then you have to tell it to collect all controls which must be subclassed.
  // This is not necessary if you create the manager via component palette and normal design time handling.
  ThemeManager.CollectForms;

  PageControl1.ActivePage := ExplorerTabsheet;
  ReadComputerProperties;
  UpdateThemeInfo;
  FillExplorerListBox;
  CreateDefaultBackground;

  Animate1.Active := True;
  FBackBitmap := TBitmap.Create;
  with FBackBitmap do
  begin
    PixelFormat := pf32Bit;
    Width := DrawListBox.ClientWidth;
    Height := DrawListBox.ItemHeight;
  end;

  Label29.Caption := 'This test will run for ' + IntToStr(TestDuration div 1000) + ' seconds.';

  {$ifdef COMPILER_6_UP}
    with CheckListBox1 do
    begin
      Header[0] := True;
      HeaderBackgroundColor := clBackground;
      HeaderColor := clWhite;
    end;
  {$endif COMPILER_6_UP}
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ThemeManagerThemeChange(Sender: TObject);

begin
  UpdateThemeInfo;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ThemesEnableGroupClick(Sender: TObject);

begin
  if ThemesEnableGroup.ItemIndex = 0 then
    ThemeManager.Options := DefaultThemeOptions
  else                                                             
    ThemeManager.Options := DefaultThemeOptions - [toAllowControls, toAllowNonClientArea];
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.TrackBar1Change(Sender: TObject);

begin
  ProgressBar1.Position := TrackBar1.Position; 
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.ElementsListBoxClick(Sender: TObject);

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

procedure TMainForm.DrawListBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);

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

    if ThemesEnabled then
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

procedure TMainForm.FormDestroy(Sender: TObject);

begin
  FThumbnailBackground.Free;
  FBackBitmap.Free;
  ThemeManager.Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CheckBox4Click(Sender: TObject);

begin
  if TListBox(Sender).Tag = 0 then
    ElementsListBox.Invalidate;
  DrawListBox.Invalidate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.LeftPaintBoxPaint(Sender: TObject);

// Draws some samples which demonstrate the use of the theme manager.

var
  Offset: Integer;
  R: TRect;
  WorkRect: TRect;
  BackgroundDetails,
  Details: TThemedElementDetails;

begin
  with ThemeManager, ThemeServices, LeftPaintBox, Canvas do
  begin
    if ThemesEnabled and (toAllowControls in ThemeManager.Options) then
    begin
      R := ClientRect;
      Details := GetElementDetails(tebSpecialGroupHead);
      DrawElement(Handle, Details, R);
      // Create 3 visual areas.
      Offset := (R.Bottom - R.Top) div 3;
      R.Bottom := Offset;

      BackgroundDetails := GetElementDetails(tebHeaderBackgroundNormal);

      // 1.
      WorkRect := R;
      InflateRect(WorkRect, -10, -10);
      WorkRect.Bottom := WorkRect.Top + 26;
      DrawElement(Handle, BackgroundDetails, WorkRect);
      DrawText(Handle, BackgroundDetails, 'First area', WorkRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0);
      Details := GetElementDetails(tebHeaderPinNormal);
      DrawElement(Handle, Details, Rect(WorkRect.Right - 32, WorkRect.Top + 4, WorkRect.Right, WorkRect.Bottom - 4));
      WorkRect.Top := WorkRect.Bottom + 5;
      WorkRect.Bottom := R.Bottom - 5;
      Details := GetElementDetails(ttbFlashButton);
      DrawElement(Handle, Details, WorkRect);

      // 2.
      OffsetRect(R, 0, Offset);
      WorkRect := R;
      InflateRect(WorkRect, -10, -10);
      WorkRect.Bottom := WorkRect.Top + 26;
      Details := GetElementDetails(ttbFlashButton);
      DrawElement(Handle, BackgroundDetails, WorkRect);
      DrawText(Handle, BackgroundDetails, 'Second area', WorkRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0);
      Details := GetElementDetails(tebSpecialGroupCollapseSpecial);
      DrawElement(Handle, Details, Rect(WorkRect.Right - 32, WorkRect.Top + 4, WorkRect.Right, WorkRect.Bottom - 4));
      WorkRect.Top := WorkRect.Bottom + 5;
      WorkRect.Bottom := R.Bottom - 5;
      Details := GetElementDetails(teEditTextNormal);
      DrawElement(Handle, Details, WorkRect);

      // 3.
      OffsetRect(R, 0, Offset);
      WorkRect := R;
      InflateRect(WorkRect, -10, -10);
      WorkRect.Bottom := WorkRect.Top + 26;
      DrawElement(Handle, BackgroundDetails, WorkRect);
      DrawText(Handle, BackgroundDetails, 'Third area', WorkRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE, 0);
      Details := GetElementDetails(tebHeaderCloseNormal);
      DrawElement(Handle, Details, Rect(WorkRect.Right - 32, WorkRect.Top + 4, WorkRect.Right, WorkRect.Bottom - 4));
      WorkRect.Top := WorkRect.Bottom + 5;
      WorkRect.Bottom := R.Bottom - 5;
      Details := GetElementDetails(tbUserButton);
      DrawElement(Handle, Details, WorkRect);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.RightPaintBoxPaint(Sender: TObject);

var
  Offset: Integer;
  R: TRect;
  WorkRect: TRect;
  Details: TThemedElementDetails;

begin
  with ThemeManager, ThemeServices, RightPaintBox, Canvas do
  begin
    if ThemesEnabled and (toAllowControls in ThemeManager.Options) then
    begin
      R := ClientRect;
      Details := GetElementDetails(ttPane);
      DrawElement(Handle, Details, R);

      Inc(R.Left, 10);
      Offset := (R.Bottom - R.Top) div 3;
      R.Bottom := Offset;

      Details := GetElementDetails(tspPlacesListSeparator);
      WorkRect := R;
      WorkRect.Top := WorkRect.Bottom - 4;
      WorkRect.Right := (R.Right - R.Left) div 2;
      DrawElement(Handle, Details, WorkRect);

      OffsetRect(R, 0, Offset);

      WorkRect := R;
      WorkRect.Top := WorkRect.Bottom - 4;
      WorkRect.Right := (R.Right - R.Left) div 2;
      DrawElement(Handle, Details, WorkRect);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PageControl1Change(Sender: TObject);

begin
  StatusBar.Panels[0].Text := '';
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.StressTestStartButtonClick(Sender: TObject);

type
  TFormArray = array[0..9] of TForm;

var
  I: Integer;
  Forms1,
  Forms2: TFormArray;

  Start: Cardinal;

begin
  StressTestStartButton.Enabled := False;
  try
    Randomize;
    Start := GetTickCount;
    FillChar(Forms1, SizeOf(Forms1), 0);
    FillChar(Forms2, SizeOf(Forms2), 0);
    I := 0;

    while GetTickCount - Start < TestDuration do
    begin
      // Create or destroy one form of type 1.
      if Assigned(Forms1[I]) then
      begin
        Forms1[I].Release;
        Forms1[I] := nil;
      end
      else
      begin
        Forms1[I] := TForm1.Create(Self);
        with Forms1[I] do
        begin
          Left := I * 20;
          Top := I * 20;
          Show;
        end;
      end;

      // Create or destroy one form of type 2.
      if Assigned(Forms2[I]) then
      begin
        Forms2[I].Free;
        Forms2[I] := nil;
      end
      else
      begin
        Forms2[I] := TForm2.Create(Self);
        with Forms2[I] do
        begin
          Left := I * 20;
          Top := I * 20 + 320;
          Show;
          Update;
        end;
      end;

      Inc(I);
      if I > High(Forms1) then
        I := 0;
      // Give the application some time to display the forms.
      Application.ProcessMessages;
    end;

    for I := 0 to 9 do
    begin
      if Assigned(Forms1[I]) then
        Forms1[I].Free;
      if Assigned(Forms2[I]) then
        Forms2[I].Free;
    end;
  finally
    StressTestStartButton.Enabled := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.HeavyLoadButtonClick(Sender: TObject);

begin
  with TForm3.Create(Self) do
    Show;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.PageControl3Change(Sender: TObject);

begin
  if Panel3.ControlCount = 0 then
  begin
    with TForm1.Create(Application) do
    begin
      Parent := Panel3;
      BorderStyle := bsNone;
      Align := alClient;
      Show;
    end;
  end;
end;
                                                
//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Button5Click(Sender: TObject);

begin
  ShowMessage('A simple message box (ShowMessage).');
  Application.MessageBox('Extended message box (Application.MessageBox)', 'Message box', MB_YESNOCANCEL or
    MB_ICONINFORMATION or MB_DEFBUTTON1);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CheckListBox1Click(Sender: TObject);

begin
  if not FCheckPending and (CheckListBox1.ItemIndex > 0) then
  begin
    Screen.Cursor := crHourGlass;
    try
      ShellExecute(0, 'open', PChar('news://' + CheckListBox1.Items[CheckListBox1.ItemIndex]), nil, nil, SW_SHOW);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  FCheckPending := False;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Image1Click(Sender: TObject);

begin
  ShellExecute(0, 'open', 'www.soft-gems.net/ThemeManager.php', nil, nil, SW_SHOW);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.CheckListBox1ClickCheck(Sender: TObject);

begin
  FCheckPending := True;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ShowDLLForm; external 'ThemedDLL.dll';
procedure HideDLLForm; external 'ThemedDLL.dll';
procedure SetApplicationHandle(Handle: HWnd); external 'ThemedDLL.dll';

procedure TMainForm.DLLFormButtonClick(Sender: TObject);

begin
  SetApplicationHandle(Application.Handle);
  ShowDLLForm;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMainForm.Button7Click(Sender: TObject);

begin
  HideDLLForm;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
