unit StrEditD4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, dsgnintf;

type
  TStrEditDlg = class(TForm)
    Bevel1: TBevel;
    btnOk: TButton;
    btnCancel: TButton;
    Editor: TRichEdit;
    StatusBar: TStatusBar;
    procedure EditorChange(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  	FModified:	Boolean;
  public
    { Public declarations }
  end;

  TStringListProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;
  
implementation

{$R *.DFM}

procedure TStrEditDlg.EditorChange(Sender: TObject);
begin
	if Sender = Editor then
  	FModified := True;
  StatusBar.SimpleText := Format ('%d lines.', [Editor.Lines.Count]);
end;

procedure TStrEditDlg.EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
  	ModalResult := mrCancel;
end;

{ TStringListProperty }

procedure TStringListProperty.Edit;
begin
  with TStrEditDlg.Create(Application) do
  try
    Editor.Lines := TStrings(GetOrdValue);
    EditorChange (nil);
    FModified := False;
    ActiveControl := Editor;
    if ShowModal = mrOk then
    	SetOrdValue(LongInt(Editor.Lines));
  finally
    Free;
  end;
end;

function TStringListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paSubProperties];
end;

end.
