unit progress_dialog;

{$mode delphi}{$H+}

// Cross-platform replacement for the Windows shell IProgressDialog, used for
// long running loops on the main thread, e.g. when running SQL files.
// The form is shown non-modally while all other forms get disabled, the same
// way ShowModal does it internally. HasUserCancelled pumps the message queue
// in a throttled way, so the dialog repaints and the cancel button works
// without slowing down loops with many small steps.
// Extras: a throughput graph (like the Windows file copy dialog), a colored
// state for cancel/error, and progress in the Windows task button.

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, Graphics,
  LCLIntf, LCLType, extra_controls, lazaruscompat;

type

  { TfrmProgress }

  TfrmProgress = class(TExtForm)
    lblLine1: TLabel;
    lblLine2: TLabel;
    lblTime: TLabel;
    progressBar: TProgressBar;
    paintSpeed: TPaintBox;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure paintSpeedPaint(Sender: TObject);
  private
    const SpeedBuckets = 200;
    var
    FCancelled: Boolean;
    FStarted: Boolean;
    FState: TProgressBarState;
    FDisabledForms: TList;
    FStartTime: UInt64;
    FLastPump: UInt64;
    // Throughput graph: one bytes/second value per progress fraction bucket
    FSpeeds: array[0..SpeedBuckets-1] of Double;
    FSpeedFilled: Integer;
    FSpeedCurrent, FSpeedPeak: Double;
    FLastSampleTime: UInt64;
    FLastSamplePos: Int64;
    FBucketStartTime: UInt64;
    FBucketStartPos: Int64;
    procedure UpdateTime(Position, Max: Int64);
    procedure AddSpeedSample(Position, Max: Int64);
    function StateColor: TColor;
    procedure SetTaskbarState(State: TProgressBarState; Visible: Boolean=True);
    procedure SetTaskbarValue(Position: Integer);
  public
    destructor Destroy; override;
    // Show the dialog and disable all other forms
    procedure Start;
    // Re-enable other forms and hide the dialog
    procedure Stop;
    procedure SetTitle(const Title: String);
    // Line 1 or 2. ForcePaint repaints right away, for use before a longer
    // blocking operation
    procedure SetLine(LineNum: Integer; const Text: String; ForcePaint: Boolean=False);
    procedure SetProgress(Position, Max: Int64);
    // Normal, error (red) or paused (yellow), for bar, graph and task button
    procedure SetState(State: TProgressBarState);
    // Processes pending messages, at most every few milliseconds
    function HasUserCancelled: Boolean;
    property State: TProgressBarState read FState;
  end;


implementation

uses apphelpers, main;

{$R *.lfm}

const
  PumpInterval = 150; // milliseconds
  SampleInterval = 250; // milliseconds between throughput samples
  ProgressBarMax = 1000;


destructor TfrmProgress.Destroy;
begin
  Stop;
  inherited;
end;


procedure TfrmProgress.Start;
var
  i: Integer;
begin
  if FStarted then
    Exit;
  FCancelled := False;
  FStartTime := GetTickCount64;
  FLastPump := 0;
  for i:=Low(FSpeeds) to High(FSpeeds) do
    FSpeeds[i] := 0;
  FSpeedFilled := 0;
  FSpeedCurrent := 0;
  FSpeedPeak := 0;
  FLastSampleTime := FStartTime;
  FLastSamplePos := 0;
  FBucketStartTime := FStartTime;
  FBucketStartPos := 0;
  progressBar.Max := ProgressBarMax;
  progressBar.Position := 0;
  lblTime.Caption := '';
  btnCancel.Enabled := True;
  if Owner is TCustomForm then begin
    PopupMode := pmExplicit;
    PopupParent := TCustomForm(Owner);
  end;
  Show;
  SetState(pbsNormal);
  SetTaskbarValue(0);
  // Block input to other forms, especially to the main form, whose actions
  // would otherwise be reachable through ProcessMessages in HasUserCancelled
  FDisabledForms := Screen.DisableForms(Self);
  FStarted := True;
  Application.ProcessMessages;
end;


procedure TfrmProgress.Stop;
begin
  if not FStarted then
    Exit;
  FStarted := False;
  SetTaskbarState(pbsNormal, False);
  // Re-enable forms before hiding, so the OS activates the main form again
  // instead of some other application
  Screen.EnableForms(FDisabledForms);
  Hide;
end;


procedure TfrmProgress.SetTitle(const Title: String);
begin
  Caption := Title;
end;


procedure TfrmProgress.SetLine(LineNum: Integer; const Text: String; ForcePaint: Boolean=False);
begin
  case LineNum of
    1: lblLine1.Caption := Text;
    2: lblLine2.Caption := Text;
  end;
  if ForcePaint and FStarted then begin
    Application.ProcessMessages;
    FLastPump := GetTickCount64;
  end;
end;


procedure TfrmProgress.SetProgress(Position, Max: Int64);
var
  NewPos: Integer;
begin
  if Max <= 0 then
    NewPos := 0
  else if Position >= Max then
    NewPos := ProgressBarMax
  else
    NewPos := Round(Position / Max * ProgressBarMax);
  if progressBar.Position <> NewPos then begin
    progressBar.Position := NewPos;
    SetTaskbarValue(NewPos);
  end;
  AddSpeedSample(Position, Max);
  UpdateTime(Position, Max);
end;


procedure TfrmProgress.SetState(State: TProgressBarState);
{$IFDEF WINDOWS}
const
  // Win32 common control message (Vista+), paints the bar green/red/yellow
  PBM_SETSTATE = $0400 + 16; // WM_USER + 16
  PBST: array[TProgressBarState] of PtrUInt = (1, 2, 3); // PBST_NORMAL, PBST_ERROR, PBST_PAUSED
{$ENDIF}
begin
  FState := State;
  {$IFDEF WINDOWS}
  // The LCL TProgressBar has no State property; send PBM_SETSTATE like VCL does
  LCLIntf.SendMessage(progressBar.Handle, PBM_SETSTATE, PBST[State], 0);
  {$ENDIF}
  // The graph shows the state on all platforms
  paintSpeed.Invalidate;
  SetTaskbarState(State);
end;


procedure TfrmProgress.SetTaskbarState(State: TProgressBarState; Visible: Boolean=True);
{$IFDEF WINDOWS}
var
  Flag: Integer;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if Assigned(MainForm) and Assigned(MainForm.TaskbarList3) then begin
    if not Visible then
      Flag := 0 // TBPF_NOPROGRESS
    else case State of
      pbsError: Flag := 4;  // TBPF_ERROR
      pbsPaused: Flag := 8; // TBPF_PAUSED
      else Flag := 2;       // TBPF_NORMAL
    end;
    MainForm.TaskbarList3.SetProgressState(MainForm.Handle, Flag);
  end;
  {$ENDIF}
end;


procedure TfrmProgress.SetTaskbarValue(Position: Integer);
begin
  {$IFDEF WINDOWS}
  if Assigned(MainForm) and Assigned(MainForm.TaskbarList3) then
    MainForm.TaskbarList3.SetProgressValue(MainForm.Handle, Position, ProgressBarMax);
  {$ENDIF}
end;


procedure TfrmProgress.UpdateTime(Position, Max: Int64);
var
  Elapsed, Remaining: Double;
begin
  // Replacement for PROGDLG_AUTOTIME
  Elapsed := (GetTickCount64 - FStartTime) / 1000;
  if (Position > 0) and (Max > Position) and (Elapsed >= 2) then begin
    Remaining := Elapsed / Position * (Max - Position);
    lblTime.Caption := f_('Elapsed time: %s, remaining: approx. %s',
      [FormatTimeNumber(Elapsed, True, 0), FormatTimeNumber(Remaining, True, 0)]);
  end else
    lblTime.Caption := f_('Elapsed time: %s', [FormatTimeNumber(Elapsed, True, 0)]);
end;


procedure TfrmProgress.AddSpeedSample(Position, Max: Int64);
var
  TickNow: UInt64;
  Speed: Double;
  Bucket, i: Integer;
begin
  TickNow := GetTickCount64;
  if (Max <= 0) or (TickNow - FLastSampleTime < SampleInterval) then
    Exit;
  Speed := (Position - FLastSamplePos) / ((TickNow - FLastSampleTime) / 1000);
  if Speed < 0 then
    Speed := 0;
  // Smoothed value for the text display, against jitter from queries of
  // different size
  if FSpeedFilled = 0 then
    FSpeedCurrent := Speed
  else
    FSpeedCurrent := FSpeedCurrent * 0.6 + Speed * 0.4;

  Bucket := Trunc(Position / Max * SpeedBuckets);
  if Bucket >= SpeedBuckets then
    Bucket := SpeedBuckets-1;
  if Bucket >= FSpeedFilled then begin
    // Entering new bucket(s). Buckets skipped by one sample, e.g. during a
    // long blocking query, get that sample's speed, so they show up as a
    // flat stretch instead of a gap.
    for i:=FSpeedFilled to Bucket do
      FSpeeds[i] := Speed;
    FSpeedFilled := Bucket+1;
    FBucketStartTime := FLastSampleTime;
    FBucketStartPos := FLastSamplePos;
  end else if TickNow > FBucketStartTime then begin
    // Still in the current bucket: its value is the real average throughput
    // since the bucket started, so pauses within the bucket remain visible
    FSpeeds[Bucket] := (Position - FBucketStartPos) / ((TickNow - FBucketStartTime) / 1000);
  end;
  FLastSampleTime := TickNow;
  FLastSamplePos := Position;
  paintSpeed.Invalidate;
end;


function TfrmProgress.StateColor: TColor;
begin
  case FState of
    pbsError: Result := RGBToColor(220, 60, 60);
    pbsPaused: Result := RGBToColor(225, 165, 30);
    else Result := ColorToRGB(clHighlight);
  end;
end;


function BlendColors(Color1, Color2: TColor; Percent2: Integer): TColor;
var
  c1, c2: TColor;
begin
  c1 := ColorToRGB(Color1);
  c2 := ColorToRGB(Color2);
  Result := RGBToColor(
    (Red(c1) * (100-Percent2) + Red(c2) * Percent2) div 100,
    (Green(c1) * (100-Percent2) + Green(c2) * Percent2) div 100,
    (Blue(c1) * (100-Percent2) + Blue(c2) * Percent2) div 100
    );
end;


procedure TfrmProgress.paintSpeedPaint(Sender: TObject);
var
  Cnv: TCanvas;
  W, H, i, x, y, Margin: Integer;
  ScaleMax: Double;
  Points: array of TPoint;
  LineColor, BackColor: TColor;
  S: String;
begin
  Cnv := paintSpeed.Canvas;
  W := paintSpeed.ClientWidth;
  H := paintSpeed.ClientHeight;
  BackColor := ColorToRGB(clWindow);
  LineColor := StateColor;

  // Background and frame
  Cnv.Brush.Style := bsSolid;
  Cnv.Brush.Color := BackColor;
  Cnv.Pen.Color := BlendColors(BackColor, clWindowText, 25);
  Cnv.Pen.Width := 1;
  Cnv.Rectangle(0, 0, W, H);

  FSpeedPeak := 0;
  for i:=0 to FSpeedFilled-1 do begin
    if FSpeeds[i] > FSpeedPeak then
      FSpeedPeak := FSpeeds[i];
  end;

  if (FSpeedFilled > 0) and (FSpeedPeak > 0) then begin
    // Leave some headroom above the peak for the text
    ScaleMax := FSpeedPeak * 1.35;
    SetLength(Points, FSpeedFilled + 2);
    for i:=0 to FSpeedFilled-1 do begin
      x := 1 + Round((i + 1) / SpeedBuckets * (W - 2));
      y := H - 1 - Round(FSpeeds[i] / ScaleMax * (H - 2));
      Points[i] := Point(x, y);
    end;
    Points[0].X := 1;
    Points[FSpeedFilled] := Point(Points[FSpeedFilled-1].X, H - 1);
    Points[FSpeedFilled+1] := Point(1, H - 1);

    // Filled area below the curve
    Cnv.Brush.Color := BlendColors(BackColor, LineColor, 35);
    Cnv.Pen.Color := Cnv.Brush.Color;
    Cnv.Polygon(Points);

    // Curve
    Cnv.Pen.Color := LineColor;
    Cnv.Pen.Width := ScaleFromDesign(2);
    Cnv.Polyline(Points, 0, FSpeedFilled);
    Cnv.Pen.Width := 1;

    // Current and peak throughput
    Margin := ScaleFromDesign(4);
    Cnv.Brush.Style := bsClear;
    Cnv.Font.Assign(Font);
    Cnv.Font.Color := clWindowText;
    Cnv.Font.Style := [fsBold];
    Cnv.TextOut(Margin, Margin, FormatByteNumber(Round(FSpeedCurrent)) + '/s');
    Cnv.Font.Style := [];
    S := f_('Peak: %s', [FormatByteNumber(Round(FSpeedPeak)) + '/s']);
    Cnv.TextOut(W - Margin - Cnv.TextWidth(S), Margin, S);
  end;
end;


function TfrmProgress.HasUserCancelled: Boolean;
var
  TickNow: UInt64;
begin
  if FStarted and (not FCancelled) then begin
    TickNow := GetTickCount64;
    if TickNow - FLastPump >= PumpInterval then begin
      FLastPump := TickNow;
      Application.ProcessMessages;
    end;
  end;
  Result := FCancelled;
end;


procedure TfrmProgress.btnCancelClick(Sender: TObject);
begin
  FCancelled := True;
  btnCancel.Enabled := False;
  SetState(pbsPaused);
end;


procedure TfrmProgress.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Closing via title bar or Alt+F4 means cancel. The caller stops the dialog.
  CanClose := not FStarted;
  if FStarted and (not FCancelled) then
    btnCancelClick(Sender);
end;


end.
