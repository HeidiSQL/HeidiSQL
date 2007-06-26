unit Main;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynEditHighlighter, SynHighlighterURI, SynURIOpener;

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

{$R *.dfm}


end.

