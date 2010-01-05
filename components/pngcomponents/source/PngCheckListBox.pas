unit PngCheckListBox;

{$I compilers.inc}

interface

uses
  Windows, Classes, CheckLst, pngimage, PngFunctions;

type
  TPngCheckListBox = class(TCheckListBox)
  private
    FPngUnchecked: TPngImage;
    FPngChecked: TPngImage;
    FPngOptions: TPngOptions;
    FPngGrayed: TPngImage;
    procedure SetPngChecked(const Value: TPngImage);
    procedure SetPngUnchecked(const Value: TPngImage);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure SetPngGrayed(const Value: TPngImage);
  protected
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    function GetCheckWidth: Integer; reintroduce;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PngChecked: TPngImage read FPngChecked write SetPngChecked;
    property PngUnchecked: TPngImage read FPngUnchecked write SetPngUnchecked;
    property PngGrayed: TPngImage read FPngGrayed write SetPngGrayed;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
  end;

implementation

uses
  Graphics, StdCtrls, Math;

{ TPngCheckListBox }

constructor TPngCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPngChecked := TPngImage.Create;
  FPngUnchecked := TPngImage.Create;
  FPngGrayed := TPngImage.Create;
end;

destructor TPngCheckListBox.Destroy;
begin
  FPngChecked.Free;
  FPngUnchecked.Free;
  FPngGrayed.Free;
  inherited Destroy;
end;

procedure TPngCheckListBox.DrawItem(Index: Integer; ARect: TRect; State:
  TOwnerDrawState);

  procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
  var
    Png: TPngImage;
    OldColor: TColor;
  begin
    //Draws the check image, if it's a PNG, otherwise the inherited would have
    //been called
    OldColor := Canvas.Brush.Color;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
    Canvas.Brush.Color := OldColor;
    case AState of
      cbUnchecked: Png := FPngUnchecked;
      cbChecked: Png := FPngChecked;
    else
      Png := FPngGrayed;
    end;
    DrawPNG(Png, Canvas, Rect(R.Left, R.Top, R.Left + Png.Width, R.Top +
      Png.Height), FPngOptions);
  end;

  procedure DrawText;
  var
    Flags: Integer;
    Data: string;
  begin
    //Draws the text for an item
    if Assigned(OnDrawItem) then
      OnDrawItem(Self, Index, ARect, State)
    else begin
      Canvas.FillRect(ARect);
      if Index < Items.Count then begin
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or
          DT_NOPREFIX);
        if not UseRightToLeftAlignment then
          Inc(ARect.Left, 2)
        else
          Dec(ARect.Right, 2);
        Data := '';
        if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
          Data := DoGetData(Index)
        else
          Data := Items[Index];
        Windows.DrawText(Canvas.Handle, PChar(Data), Length(Data), ARect, Flags);
      end;
    end;
  end;

var
  R: TRect;
  SaveEvent: TDrawItemEvent;
  ACheckWidth: Integer;
  Enable: Boolean;
begin
  if FPngChecked.Empty and FPngUnchecked.Empty and FPngGrayed.Empty then
    inherited DrawItem(Index, ARect, State)
  else begin
    ACheckWidth := GetCheckWidth;
    if Index < Items.Count then begin
      R := ARect;
      Enable := Self.Enabled and ItemEnabled[Index];
      if not Header[Index] then begin
        if not UseRightToLeftAlignment then begin
          R.Right := ARect.Left;
          R.Left := R.Right - ACheckWidth;
        end
        else begin
          R.Left := ARect.Right;
          R.Right := R.Left + ACheckWidth;
        end;
        DrawCheck(R, Self.State[Index], Enable);
      end
      else begin
        Canvas.Font.Color := HeaderColor;
        Canvas.Brush.Color := HeaderBackgroundColor;
      end;
      if not Enable then
        Canvas.Font.Color := clGrayText;
    end;

    if (Style = lbStandard) and Assigned(OnDrawItem) then begin
      //Force lbStandard list to ignore OnDrawItem event.
      SaveEvent := OnDrawItem;
      OnDrawItem := nil;
      try
        DrawText;
      finally
        OnDrawItem := SaveEvent;
      end;
    end
    else
      DrawText;
  end;
end;

function TPngCheckListBox.GetCheckWidth: Integer;
begin
  //CheckWidth is equal to the widest PNG
  if not (FPngChecked.Empty and FPngUnchecked.Empty and FPngGrayed.Empty) then
    Result := Max(FPngChecked.Width, Max(FPngUnchecked.Width, FPngGrayed.Width))
  else
    Result := inherited GetCheckWidth;
end;

procedure TPngCheckListBox.SetPngChecked(const Value: TPngImage);
begin
  //This is all neccesary, because you can't assign a nil to a TPngImage
  if Value = nil then begin
    FPngChecked.Free;
    FPngChecked := TPngImage.Create;
  end
  else
    FPngChecked.Assign(Value);
  Repaint;
end;

procedure TPngCheckListBox.SetPngUnchecked(const Value: TPngImage);
begin
  //This is all neccesary, because you can't assign a nil to a TPngImage
  if Value = nil then begin
    FPngUnchecked.Free;
    FPngUnchecked := TPngImage.Create;
  end
  else
    FPngUnchecked.Assign(Value);
  Repaint;
end;

procedure TPngCheckListBox.SetPngGrayed(const Value: TPngImage);
begin
  //This is all neccesary, because you can't assign a nil to a TPngImage
  if Value = nil then begin
    FPngGrayed.Free;
    FPngGrayed := TPngImage.Create;
  end
  else
    FPngGrayed.Assign(Value);
  Repaint;
end;

procedure TPngCheckListBox.SetPngOptions(const Value: TPngOptions);
begin
  if FPngOptions <> Value then begin
    FPngOptions := Value;
    Repaint;
  end;
end;

end.
