unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SQLite3Pages;

type
  TForm1 = class(TForm)
    edt1: TEdit;
    lbl1: TLabel;
    Label1: TLabel;
    mmo1: TMemo;
    btn1: TButton;
    btn2: TButton;
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R Vista.res}

procedure TForm1.btn2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btn1Click(Sender: TObject);
var i: integer;
    Bmp: TBitmap;
    s: string;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width := ClientWidth;
    Bmp.Height := ClientHeight;
    PaintTo(Bmp.Canvas,0,0); // create some bitmap content
    with TGDIPages.Create(self) do
    try
      // the name of the report is taken from main Window's caption
      Caption := self.Caption;
      // now we add some content to the report
      BeginDoc;
      // header and footer
      Font.Name := 'Georgia';
      Font.Size := 11;
      SaveLayout;
      Font.Style := [fsItalic,fsUnderline];
      TextAlign := taRight;
      AddTextToHeaderAt('http://synopse.info',RightMarginPos);
      Font.Style := [];
      AddLineToFooter(false);
      AddPagesToFooterAt('Page %d/%d',RightMarginPos);
      RestoreSavedLayout;
      AddTextToHeader(ExtractFileName(paramstr(0)));
      AddTextToFooter(DateTimeToStr(Now));
      AddLineToHeader(false);
      Font.Size := 12;
      // main content (automaticaly split on next pages)
      NewHalfLine;
      TextAlign := taJustified;
      s := 'This is some big text which must be justified on multiple lines. ';
      DrawText(s+s+s+s);
      NewLine;
      TextAlign := taLeft;
      DrawTitle(edt1.Text,true);
      for i := 1 to 10 do
        DrawText('This is some text '+IntToStr(i));
      NewLine;
      DrawBMP(Bmp,maxInt,50,'Some bitmap in the report');
      AddBookMark('bookmarkname');
      WordWrapLeftCols := true;
      AddColumns([10,20,50]);
      AddColumnHeaders(['#','Two','Three'],true,true);
      for i := 1 to 100 do
        DrawTextAcrossCols([IntToStr(i),'Column '+IntToStr(i),'Some text here. '+s]);
      NewLine;
      DrawBMP(Bmp,maxInt,50,'Some bitmap in the report (twice)');
      DrawTitle('This is your text',false,0,'','bookmarkname');
      DrawText(mmo1.Text);
      EndDoc;
      // set optional PDF export options
      // ExportPDFForceJPEGCompression := 80;
      // ExportPDFEmbeddedTTF := true;
      // ExportPDFUseUniscribe := true;
      // show a preview form, and allow basic actions via the right click menu
      ShowPreviewForm;
    finally
      Free;
    end;
  finally
    Bmp.Free;
  end;
end;

end.
