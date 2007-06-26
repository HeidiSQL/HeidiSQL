unit Main;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QSynEdit, QSynEditHighlighter, QSynHighlighterURI,
  QSynURIOpener;

type
  TForm1 = class(TForm)
    SynURIOpener1: TSynURIOpener;
    SynURISyn1: TSynURISyn;
    SynEdit1: TSynEdit;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

end.
