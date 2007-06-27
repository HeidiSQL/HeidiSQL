unit FormMain_ctParams;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynCompletionProposal, StdCtrls, SynEdit;

type
  TForm1 = class(TForm)
    SynTest: TSynEdit;
    SynEdit1: TSynEdit;
    Button3: TButton;
    scpParams: TSynCompletionProposal;
    FontDialog1: TFontDialog;
    procedure scpParamsExecute(Kind: SynCompletionType; Sender: TObject;
      var AString: String; x, y: Integer; var CanExecute: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    LookupList : TStringList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses SynEditTypes;

{$R *.DFM}

procedure TForm1.scpParamsExecute(Kind: SynCompletionType; Sender: TObject;
  var AString: String; x, y: Integer; var CanExecute: Boolean);
var locline, lookup: String;
    TmpX, savepos, StartX,
    ParenCounter,
    TmpLocation    : Integer;
    FoundMatch     : Boolean;
begin
  //Param Completion is different than Code Completion.  We can't just use
  //the string passed to us we have to figure out what they are looking for,
  //which is language dependant For this demo, I assume that it has to be on the
  //*same* line, then do some paren checking.  For the sake of the demo, the
  //function will be the word directly before the paren.  In other languages you
  //would want to do something like grab everything before the last end of
  //statement char (like in ObjectPascal it's the ';' char).  It *does* support
  //embedded functions (Hense the paren checking).  In this case, commas are the
  //delimiter so they are incremented accordingly.

  //Also everything is hard coded in.  You will want to have some kind of
  //structure that you are using instead of hard coding the parameters in

  with TSynCompletionProposal(Sender).Editor do
  begin
    locLine := LineText;

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    if TmpX > length(locLine) then
      TmpX := length(locLine)
    else dec(TmpX);
    FoundMatch := False;
    TmpLocation := 0;
    while (TmpX > 0) and not(FoundMatch) do
    begin
      if LocLine[TmpX] = ',' then
      begin
        inc(TmpLocation);
        dec(TmpX);
      end else if LocLine[TmpX] = ')' then
      begin
        //We found a close, go till it's opening paren
        ParenCounter := 1;
        dec(TmpX);
        while (TmpX > 0) and (ParenCounter > 0) do
        begin
          if LocLine[TmpX] = ')' then inc(ParenCounter)
          else if LocLine[TmpX] = '(' then dec(ParenCounter);
          dec(TmpX);
        end;
        if TmpX > 0 then dec(TmpX);  //eat the open paren
      end else if locLine[TmpX] = '(' then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        while (TmpX > 0) and not(locLine[TmpX] in TSynValidStringChars) do
          Dec(TmpX);
        if TmpX > 0 then
        begin
          SavePos := TmpX;
          While (TmpX > 0) and (locLine[TmpX] in TSynValidStringChars) do
            dec(TmpX);
          inc(TmpX);
          lookup := Uppercase(Copy(LocLine, TmpX, SavePos - TmpX + 1));
          FoundMatch := LookupList.IndexOf(Lookup) > -1;
          if not(FoundMatch) then
          begin
            TmpX := StartX;
            dec(TmpX);
          end;
        end;
      end else dec(TmpX)
    end;
  end;

  CanExecute := FoundMatch;

  if CanExecute then
  begin
    TSynCompletionProposal(Sender).Form.CurrentIndex := TmpLocation;
    if Lookup <> TSynCompletionProposal(Sender).PreviousWord then
    begin
      TSynCompletionProposal(Sender).ItemList.Clear;

      if Lookup = 'TESTFUNCTION' then
      begin
        TSynCompletionProposal(Sender).ItemList.Add('FirstParam integer');
        TSynCompletionProposal(Sender).ItemList.Add('SecondParam integer');
        TSynCompletionProposal(Sender).ItemList.Add('ThirdParam string');
      end else if Lookup = 'MIN' then
      begin
        TSynCompletionProposal(Sender).ItemList.Add('A integer');
        TSynCompletionProposal(Sender).ItemList.Add('B integer');
      end else if Lookup = 'MAX' then
      begin
        TSynCompletionProposal(Sender).ItemList.Add('A integer');
        TSynCompletionProposal(Sender).ItemList.Add('B integer');
      end;
    end;
  end else TSynCompletionProposal(Sender).ItemList.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LookupList := TStringList.Create;
  LookupList.Add('TESTFUNCTION');
  LookupList.Add('MAX');
  LookupList.Add('MIN');
  scpParams.EndOfTokenChr := '';
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LookupList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SynEdit1.SetFocus;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(scpParams.Font);
  if FontDialog1.Execute then
    scpParams.Font.Assign(FontDialog1.Font);
end;

end.
