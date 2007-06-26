unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEditHighlighter, SynHighlighterPython, SynEditPythonBehaviour, SynEdit,
  SynHighlighterJava, StdCtrls, ExtCtrls, SynEditTypes;

type
  TForm1 = class(TForm)
    Editor: TSynEdit;
    SynJavaSyn1: TSynJavaSyn;
    Panel1: TPanel;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    procedure EditorPaintTransient(Sender: TObject; Canvas: TCanvas;
      TransientType: TTransientType);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FBracketFG: TColor;
    FBracketBG: TColor;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.EditorPaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);
const AllBrackets = ['{','[','(','<','}',']',')','>'];
var Editor: TSynEdit;
    OpenChars: array[0..2] of Char;
    CloseChars: array[0..2] of Char;

  function CharToPixels(P: TBufferCoord): TPoint;
  begin
    Result:=Editor.RowColumnToPixels(Editor.BufferToDisplayPos(P));
  end;

var P: TBufferCoord;
    Pix: TPoint;
    D     : TDisplayCoord;
    S: String;
    I: Integer;
    Attri: TSynHighlighterAttributes;
    start: Integer;
    TmpCharA, TmpCharB: Char;
begin
  if TSynEdit(Sender).SelAvail then exit;
  Editor := TSynEdit(Sender);
//if you had a highlighter that used a markup language, like html or xml, then you would want to highlight
//the greater and less than signs as well as illustrated below

//  if (Editor.Highlighter = shHTML) or (Editor.Highlighter = shXML) then
//    inc(ArrayLength);

  for i := 0 to 2 do
    case i of
      0: begin OpenChars[i] := '('; CloseChars[i] := ')'; end;
      1: begin OpenChars[i] := '{'; CloseChars[i] := '}'; end;
      2: begin OpenChars[i] := '['; CloseChars[i] := ']'; end;
      3: begin OpenChars[i] := '<'; CloseChars[i] := '>'; end;
    end;

  P := Editor.CaretXY;
  D := Editor.DisplayXY;

  Start := Editor.SelStart;

  if (Start > 0) and (Start <= length(Editor.Text)) then
    TmpCharA := Editor.Text[Start]
  else TmpCharA := #0;

  if (Start < length(Editor.Text)) then
    TmpCharB := Editor.Text[Start + 1]
  else TmpCharB := #0;

  if not(TmpCharA in AllBrackets) and not(TmpCharB in AllBrackets) then exit;
  S := TmpCharB;
  if not(TmpCharB in AllBrackets) then
  begin
    P.Char := P.Char - 1;
    S := TmpCharA;
  end;
  Editor.GetHighlighterAttriAtRowCol(P, S, Attri);

  if (Editor.Highlighter.SymbolAttribute = Attri) then
  begin
    for i := low(OpenChars) to High(OpenChars) do
    begin
      if (S = OpenChars[i]) or (S = CloseChars[i]) then
      begin
        Pix := CharToPixels(P);

        Editor.Canvas.Brush.Style := bsSolid;//Clear;
        Editor.Canvas.Font.Assign(Editor.Font);
        Editor.Canvas.Font.Style := Attri.Style;

        if (TransientType = ttAfter) then
        begin
          Editor.Canvas.Font.Color := FBracketFG;
          Editor.Canvas.Brush.Color := FBracketBG;
        end else begin
          Editor.Canvas.Font.Color := Attri.Foreground;
          Editor.Canvas.Brush.Color := Attri.Background;
        end;
        if Editor.Canvas.Font.Color = clNone then
          Editor.Canvas.Font.Color := Editor.Font.Color;
        if Editor.Canvas.Brush.Color = clNone then
          Editor.Canvas.Brush.Color := Editor.Color;

        Editor.Canvas.TextOut(Pix.X, Pix.Y, S);
        P := Editor.GetMatchingBracketEx(P);

        if (P.Char > 0) and (P.Line > 0) then
        begin
          Pix := CharToPixels(P);
          if Pix.X > Editor.Gutter.Width then
          begin
            if S = OpenChars[i] then
              Editor.Canvas.TextOut(Pix.X, Pix.Y, CloseChars[i])
            else Editor.Canvas.TextOut(Pix.X, Pix.Y, OpenChars[i]);
          end;
        end;
      end; //if
    end;//for i :=
    Editor.Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Editor.Lines.LoadFromFile(OpenDialog1.Filename);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Editor.Text := SynJavaSyn1.SampleSource;
  FBracketFG := clRed;
  FBracketBG := clNone;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SynJavaSyn1.Enabled := not(SynJavaSyn1.Enabled);
  Editor.Repaint;
end;

end.
