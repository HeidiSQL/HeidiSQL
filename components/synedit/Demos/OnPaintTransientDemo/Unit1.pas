unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynEditHighlighter, SynHighlighterPython, SynEditPythonBehaviour, SynEdit,
  SynHighlighterJava, StdCtrls, ExtCtrls;

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
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.EditorPaintTransient(Sender: TObject; Canvas: TCanvas;
  TransientType: TTransientType);

const
  BracketSet = ['{','[','(','}',']',')'];
  OpenChars:array[0..2] of Char=('{','[','(');
  CloseChars:array[0..2] of Char=('}',']',')');

  function CharToPixels(P: TPoint): TPoint;
  begin
    Result:=P;
    Result:=Editor.RowColumnToPixels(Result);
    Result.Y:=Result.Y-1;
  end;

var P, Pix: TPoint;
    D     : TPoint;
    S: String;
    I: Integer;
    Attri: TSynHighlighterAttributes;
begin
  P := Editor.CaretXY;
  D := Editor.DisplayXY;

  Editor.GetHighlighterAttriAtRowCol(P, S, Attri);

{ //If you want to be able to highlight on either side of the bracket, uncomment
  //this block of text

  //Check to see if we need to go back a char;
  if (s = '') or
     ((length(s) > 0) and not(S[1] in BracketSet)) then
  begin
    P.X := P.x - 1;
    if P.X <=0 then exit;
    Editor.GetHighlighterAttriAtRowCol(P, S, Attri);
  end;
}
  if (Editor.CaretX<=length(Editor.LineText) + 1) and
     (Editor.Highlighter.SymbolAttribute = Attri) then
  begin
    for i := 0 to 2 do
    begin
      if (S = OpenChars[i]) or (S = CloseChars[i]) then
      begin
        Pix := CharToPixels(D);
        Editor.Canvas.Brush.Style := bsClear;
        Editor.Canvas.Font.Assign(Editor.Font);
        Editor.Canvas.Font.Style := Attri.Style;

        if (TransientType = ttAfter) then
          Editor.Canvas.Font.Color:=clRed
        else begin
          if Attri.Foreground <> clNone then
            Editor.Canvas.Font.Color:=Attri.Foreground
          else Editor.Canvas.Font.Color:= Editor.Font.Color;
        end;

        Editor.Canvas.TextOut(Pix.X, Pix.Y, S);
        P := Editor.GetMatchingBracketEx(P, True);

        if (P.X > 0) and (P.Y > 0) then
        begin
          Pix := CharToPixels(P);
          if S = OpenChars[i] then
            Editor.Canvas.TextOut(Pix.X, Pix.Y, CloseChars[i])
          else Editor.Canvas.TextOut(Pix.X, Pix.Y, OpenChars[i]);
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
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SynJavaSyn1.Enabled := not(SynJavaSyn1.Enabled);
  Editor.Repaint;
end;

end.
