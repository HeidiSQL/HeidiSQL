unit PngCheckListBox;

{$I compilers.inc}

interface

uses
  Windows, Messages, Graphics, Controls, Classes, StdCtrls, CheckLst, pngimage,
  PngFunctions, Math;

type
  TPngCheckListBox = class(TCheckListBox)
  private
    FPngUnchecked: TPNGObject;
    FPngChecked: TPNGObject;
    FPngOptions: TPngOptions;
    FPngGrayed: TPNGObject;
    procedure SetPngChecked(const Value: TPNGObject);
    procedure SetPngUnchecked(const Value: TPNGObject);
    procedure SetPngOptions(const Value: TPngOptions);
    procedure SetPngGrayed(const Value: TPNGObject);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function GetCheckWidth: Integer; reintroduce;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property PngChecked: TPNGObject read FPngChecked write SetPngChecked;
    property PngUnchecked: TPNGObject read FPngUnchecked write SetPngUnchecked;
    property PngGrayed: TPNGObject read FPngGrayed write SetPngGrayed;
    property PngOptions: TPngOptions read FPngOptions write SetPngOptions default [pngBlendOnDisabled];
  end;

implementation

{ TPngCheckListBox }

constructor TPngCheckListBox.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
FPngChecked := TPNGObject.Create;
FPngUnchecked := TPNGObject.Create;
FPngGrayed := TPNGObject.Create;
end;

destructor TPngCheckListBox.Destroy;
begin
FPngChecked.Free;
FPngUnchecked.Free;
FPngGrayed.Free;
inherited Destroy;
end;

procedure TPngCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);

  procedure DrawCheck(R: TRect; AState: TCheckBoxState; AEnabled: Boolean);
  var
     Png: TPNGObject;
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
       cbChecked:   Png := FPngChecked;
       else         Png := FPngGrayed;
       end;
  DrawPNG(Png, Canvas, Classes.Rect(R.Left, R.Top, R.Left + Png.Width, R.Top + Png.Height), FPngOptions);
  end;

  procedure DrawText;
  var
     Flags: Integer;
     Data: string;
  begin
  //Draws the text for an item
  if Assigned(OnDrawItem)
  then OnDrawItem(Self, Index, Rect, State)
  else begin
       Canvas.FillRect(Rect);
       if Index < Items.Count
       then begin
            Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
            if not UseRightToLeftAlignment
            then Inc(Rect.Left, 2)
            else Dec(Rect.Right, 2);
            Data := '';
            {$IFDEF COMPILER_6_UP}
            if (Style in [lbVirtual, lbVirtualOwnerDraw])
            then Data := DoGetData(Index)
            else Data := Items[Index];
            {$ELSE}
            Data := Items[Index];
            {$ENDIF}
            Windows.DrawText(Canvas.Handle, PChar(Data), Length(Data), Rect, Flags);
            end;
       end;
  end;

var
   R: TRect;
   SaveEvent: TDrawItemEvent;
   ACheckWidth: Integer;
   Enable: Boolean;
begin
if FPngChecked.Empty and FPngUnchecked.Empty and FPngGrayed.Empty
then inherited DrawItem(Index, Rect, State)
else begin
     ACheckWidth := GetCheckWidth;
     if Index < Items.Count
     then begin
          R := Rect;
          Enable := Self.Enabled and ItemEnabled[Index];
          if {$IFDEF COMPILER_6_UP} not Header[Index] {$ELSE} True {$ENDIF}
          then begin
               if not UseRightToLeftAlignment
               then begin
                    R.Right := Rect.Left;
                    R.Left := R.Right - ACheckWidth;
                    end
               else begin
                    R.Left := Rect.Right;
                    R.Right := R.Left + ACheckWidth;
                    end;
               DrawCheck(R, Self.State[Index], Enable);
               end
          else begin
               {$IFDEF COMPILER_6_UP}
               Canvas.Font.Color := HeaderColor;
               Canvas.Brush.Color := HeaderBackgroundColor;
               {$ENDIF}
               end;
          if not Enable
          then Canvas.Font.Color := clGrayText;
          end;

     if (Style = lbStandard) and Assigned(OnDrawItem)
     then begin
          //Force lbStandard list to ignore OnDrawItem event.
          SaveEvent := OnDrawItem;
          OnDrawItem := nil;
          try
            DrawText;
          finally
            OnDrawItem := SaveEvent;
           end;
          end
     else DrawText;
     end;
end;

function TPngCheckListBox.GetCheckWidth: Integer;
begin
//CheckWidth is equal to the widest PNG
if not (FPngChecked.Empty and FPngUnchecked.Empty and FPngGrayed.Empty)
then Result := Max(FPngChecked.Width, Max(FPngUnchecked.Width, FPngGrayed.Width))
else Result := inherited GetCheckWidth;
end;

procedure TPngCheckListBox.SetPngChecked(const Value: TPNGObject);
begin
//This is all neccesary, because you can't assign a nil to a TPNGObject
if Value = nil
then begin
     FPngChecked.Free;
     FPngChecked := TPNGObject.Create;
     end
else FPngChecked.Assign(Value);
Repaint;
end;

procedure TPngCheckListBox.SetPngUnchecked(const Value: TPNGObject);
begin
//This is all neccesary, because you can't assign a nil to a TPNGObject
if Value = nil
then begin
     FPngUnchecked.Free;
     FPngUnchecked := TPNGObject.Create;
     end
else FPngUnchecked.Assign(Value);
Repaint;
end;

procedure TPngCheckListBox.SetPngGrayed(const Value: TPNGObject);
begin
//This is all neccesary, because you can't assign a nil to a TPNGObject
if Value = nil
then begin
     FPngGrayed.Free;
     FPngGrayed := TPNGObject.Create;
     end
else FPngGrayed.Assign(Value);
Repaint;
end;

procedure TPngCheckListBox.SetPngOptions(const Value: TPngOptions);
begin
if FPngOptions <> Value
then begin
     FPngOptions := Value;
     Repaint;
     end;
end;

end.
