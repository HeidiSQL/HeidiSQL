
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntExtDlgs;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, Windows, TntDialogs, TntExtCtrls, TntStdCtrls, TntButtons;

type
{TNT-WARN TOpenPictureDialog}
  TTntOpenPictureDialog = class(TTntOpenDialog)
  private
    FPicturePanel: TTntPanel;
    FPictureLabel: TTntLabel;
    FPreviewButton: TTntSpeedButton;
    FPaintPanel: TTntPanel;
    FImageCtrl: TTntImage;
    FSavedFilename: WideString;
    function  IsFilterStored: Boolean;
    procedure PreviewKeyPress(Sender: TObject; var Key: Char{TNT-ALLOW Char});
  protected
    procedure PreviewClick(Sender: TObject); virtual;
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
    property ImageCtrl: TTntImage read FImageCtrl;
    property PictureLabel: TTntLabel read FPictureLabel;
  published
    property Filter stored IsFilterStored;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
    {$IFDEF COMPILER_9_UP}
    function Execute(ParentWnd: HWND): Boolean; override;
    {$ENDIF}
  end;

{TNT-WARN TSavePictureDialog}
  TTntSavePictureDialog = class(TTntOpenPictureDialog)
  public
    function Execute: Boolean; override;
    {$IFDEF COMPILER_9_UP}
    function Execute(ParentWnd: HWND): Boolean; override;
    {$ENDIF}
  end;

implementation

uses
  ExtDlgs, {ExtDlgs is needed for a linked resource} Dialogs, Consts, Messages,
  Graphics, Math, Controls, Forms, SysUtils, CommDlg, TntSysUtils, TntForms;

{ TTntSilentPaintPanel }

type
  TTntSilentPaintPanel = class(TTntPanel)
  protected
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  end;

procedure TTntSilentPaintPanel.WMPaint(var Msg: TWMPaint);
begin
  try
    inherited;
  except
    Caption := SInvalidImage;
  end;
end;

{ TTntOpenPictureDialog }

constructor TTntOpenPictureDialog.Create(AOwner: TComponent);
begin
  inherited;
  Filter := GraphicFilter(TGraphic);
  FPicturePanel := TTntPanel.Create(Self);
  with FPicturePanel do
  begin
    Name := 'PicturePanel';
    Caption := '';
    SetBounds(204, 5, 169, 200);
    BevelOuter := bvNone;
    BorderWidth := 6;
    TabOrder := 1;
    FPictureLabel := TTntLabel.Create(Self);
    with FPictureLabel do
    begin
      Name := 'PictureLabel';
      Caption := '';
      SetBounds(6, 6, 157, 23);
      Align := alTop;
      AutoSize := False;
      Parent := FPicturePanel;
    end;
    FPreviewButton := TTntSpeedButton.Create(Self);
    with FPreviewButton do
    begin
      Name := 'PreviewButton';
      SetBounds(77, 1, 23, 22);
      Enabled := False;
      Glyph.LoadFromResourceName(FindClassHInstance(TOpenPictureDialog{TNT-ALLOW TOpenPictureDialog}), 'PREVIEWGLYPH');
      Hint := SPreviewLabel;
      ParentShowHint := False;
      ShowHint := True;
      OnClick := PreviewClick;
      Parent := FPicturePanel;
    end;
    FPaintPanel := TTntSilentPaintPanel.Create(Self);
    with FPaintPanel do
    begin
      Name := 'PaintPanel';
      Caption := '';
      SetBounds(6, 29, 157, 145);
      Align := alClient;
      BevelInner := bvRaised;
      BevelOuter := bvLowered;
      TabOrder := 0;
      FImageCtrl := TTntImage.Create(Self);
      Parent := FPicturePanel;
      with FImageCtrl do
      begin
        Name := 'PaintBox';
        Align := alClient;
        OnDblClick := PreviewClick;
        Parent := FPaintPanel;
        Proportional := True;
        Stretch := True;
        Center := True;
        IncrementalDisplay := True;
      end;
    end;
  end;
end;

procedure TTntOpenPictureDialog.DoClose;
begin
  inherited;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TTntOpenPictureDialog.DoSelectionChange;
var
  FullName: WideString;
  ValidPicture: Boolean;

  function ValidFile(const FileName: WideString): Boolean;
  begin
    Result := WideFileGetAttr(FileName) <> $FFFFFFFF;
  end;

begin
  FullName := FileName;
  if FullName <> FSavedFilename then
  begin
    FSavedFilename := FullName;
    ValidPicture := WideFileExists(FullName) and ValidFile(FullName);
    if ValidPicture then
    try
      FImageCtrl.Picture.LoadFromFile(FullName);
      FPictureLabel.Caption := WideFormat(SPictureDesc,
        [FImageCtrl.Picture.Width, FImageCtrl.Picture.Height]);
      FPreviewButton.Enabled := True;
      FPaintPanel.Caption := '';
    except
      ValidPicture := False;
    end;
    if not ValidPicture then
    begin
      FPictureLabel.Caption := SPictureLabel;
      FPreviewButton.Enabled := False;
      FImageCtrl.Picture := nil;
      FPaintPanel.Caption := srNone;
    end;
  end;
  inherited;
end;

procedure TTntOpenPictureDialog.DoShow;
var
  PreviewRect, StaticRect: TRect;
begin
  { Set preview area to entire dialog }
  GetClientRect(Handle, PreviewRect);
  StaticRect := GetStaticRect;
  { Move preview area to right of static area }
  PreviewRect.Left := StaticRect.Left + (StaticRect.Right - StaticRect.Left);
  Inc(PreviewRect.Top, 4);
  FPicturePanel.BoundsRect := PreviewRect;
  FPreviewButton.Left := FPaintPanel.BoundsRect.Right - FPreviewButton.Width - 2;
  FImageCtrl.Picture := nil;
  FSavedFilename := '';
  FPaintPanel.Caption := srNone;
  FPicturePanel.ParentWindow := Handle;
  inherited;
end;

function TTntOpenPictureDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE' else
    Template := nil;
  Result := inherited Execute;
end;

{$IFDEF COMPILER_9_UP}
function TTntOpenPictureDialog.Execute(ParentWnd: HWND): Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE' else
    Template := nil;
  Result := inherited Execute(ParentWnd);
end;
{$ENDIF}

function TTntOpenPictureDialog.IsFilterStored: Boolean;
begin
  Result := not (Filter = GraphicFilter(TGraphic));
end;

procedure TTntOpenPictureDialog.PreviewClick(Sender: TObject);
var
  PreviewForm: TTntForm;
  Panel: TTntPanel;
begin
  PreviewForm := TTntForm.Create(Self);
  with PreviewForm do
  try
    Name := 'PreviewForm';
    BorderStyle := bsSizeToolWin; // By doing this first, it will work on WINE.
    Visible := False;
    Caption := SPreviewLabel;
    KeyPreview := True;
    Position := poScreenCenter;
    OnKeyPress := PreviewKeyPress;
    Panel := TTntPanel.Create(PreviewForm);
    with Panel do
    begin
      Name := 'Panel';
      Caption := '';
      Align := alClient;
      BevelOuter := bvNone;
      BorderStyle := bsSingle;
      BorderWidth := 5;
      Color := clWindow;
      Parent := PreviewForm;
      DoubleBuffered := True;
      with TTntImage.Create(PreviewForm) do
      begin
        Name := 'Image';
        Align := alClient;
        Stretch := True;
        Proportional := True;
        Center := True;
        Picture.Assign(FImageCtrl.Picture);
        Parent := Panel;
      end;
    end;
    if FImageCtrl.Picture.Width > 0 then
    begin
      ClientWidth := Min(Monitor.Width * 3 div 4,
        FImageCtrl.Picture.Width + (ClientWidth - Panel.ClientWidth)+ 10);
      ClientHeight := Min(Monitor.Height * 3 div 4,
        FImageCtrl.Picture.Height + (ClientHeight - Panel.ClientHeight) + 10);
    end;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TTntOpenPictureDialog.PreviewKeyPress(Sender: TObject; var Key: Char{TNT-ALLOW Char});
begin
  if Key = Char{TNT-ALLOW Char}(VK_ESCAPE) then
    (Sender as TTntForm).Close;
end;

{ TSavePictureDialog }
function TTntSavePictureDialog.Execute: Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE' else
    Template := nil;

  if (not Win32PlatformIsUnicode) then
    Result := DoExecute(@GetSaveFileNameA)
  else
    Result := DoExecuteW(@GetSaveFileNameW);
end;

{$IFDEF COMPILER_9_UP}
function TTntSavePictureDialog.Execute(ParentWnd: HWND): Boolean;
begin
  if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'DLGTEMPLATE' else
    Template := nil;

  if (not Win32PlatformIsUnicode) then
    Result := DoExecute(@GetSaveFileNameA, ParentWnd)
  else
    Result := DoExecuteW(@GetSaveFileNameW, ParentWnd);
end;
{$ENDIF}

end.
