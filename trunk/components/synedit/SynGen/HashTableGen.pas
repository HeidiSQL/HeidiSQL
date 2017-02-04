unit HashTableGen;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TFrmHashTableGen = class(TForm)
    LabelParams: TLabel;
    LabelD: TLabel;
    LabelC: TLabel;
    LabelM: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    LabelPercentage: TLabel;
    ProgressBar1: TProgressBar;
    EditD: TMemo;
    EditC: TMemo;
    EditM: TMemo;
    ButtonFindHash: TButton;
    procedure ButtonFindHashClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FKeyList: TList;
  public
    procedure AssignKeyWords(KeyList: TList; CaseSensitive: Boolean);
    function GetHashKeyFunctionSource(ClassName: string): string;
    function GetKeyWordConstantsSource(CaseSensitive: Boolean): string;
    function KeyIndicesCount: Integer;
  end;

const
  MaxTableSize = 100000;

type
  THashKeyList = class
  private
    FMaxHashKey: Integer;
    FHashKeys: array[0..MaxTableSize - 1] of Boolean;
  public
    function Add(HashKey: Integer): Boolean;
    procedure Clear;
  end;

var
  FrmHashTableGen: TFrmHashTableGen;

implementation

{$R *.dfm}

uses
{$IFDEF SYN_COMPILER_6_UP}
  StrUtils,
{$ENDIF}
  SynGenUnit,
  SynUnicode;

{$I primenumbers.inc}

var
  c, d, m: Cardinal;
  FinalC, FinalD, FinalM: Cardinal;
  searching: Boolean;
  KeyWords: array of UnicodeString;
  HashKeyList: THashKeyList;

{$Q-}
function HashKey(const S: UnicodeString): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := Result * c + Ord(S[i]) * d;
  Result := Result mod m;
end;
{$Q+}

{$Q-}
function FinalHashKey(const S: UnicodeString): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    Result := Result * FinalC + Ord(S[i]) * FinalD;
  Result := Result mod FinalM;
end;
{$Q+}

procedure WordWrapAtCol80(Words, Result: TStrings; Indentation: Integer);
var
  WrappedLines: TStringList;
  i: Integer;
  Line: string;
begin
  WrappedLines := TStringList.Create;
  try
    i := 0;

    while i < Words.Count do
    begin
      Line := StringOfChar(' ', Indentation);
      while (i < Words.Count) and (Length(Line) + Length(Words[i]) <= 80) do
      begin
        Line := Line + Words[i] + ' ';
        inc(i);
      end;
      WrappedLines.Add(Line);
    end;

    Result.Assign(WrappedLines);
  finally
    WrappedLines.Free;
  end;
end;

{ TFrmHashTableGen }

procedure TFrmHashTableGen.FormCreate(Sender: TObject);
begin
  HashKeyList := THashKeyList.Create;
  ProgressBar1.Max := 1000;
end;

procedure TFrmHashTableGen.FormDestroy(Sender: TObject);
begin
  HashKeyList.Free;
end;

procedure TFrmHashTableGen.AssignKeyWords(KeyList: TList; CaseSensitive: Boolean);
var
  i: Integer;
  KeyWordsList: TStringList;
begin
  FKeyList := nil;
  SetLength(KeyWords, 0);
  HashKeyList.Clear;

  KeyWordsList := TStringList.Create;
  try
    KeyWordsList.Sorted := True;
    KeyWordsList.Duplicates := dupIgnore;
    for i := 0 to KeyList.Count - 1 do
      KeyWordsList.Add(TLexKeys(KeyList[i]).KeyName);

    SetLength(KeyWords, KeyWordsList.Count);
    if CaseSensitive then
     for i := 0 to KeyWordsList.Count - 1 do
       KeyWords[i] := KeyWordsList[i]
    else
      for i := 0 to KeyWordsList.Count - 1 do
        KeyWords[i] := SynWideLowerCase(KeyWordsList[i]);

    FKeyList := KeyList;
  finally
    KeyWordsList.Free;
  end;
end;

procedure TFrmHashTableGen.FormShow(Sender: TObject);
begin
  c := 0;
  d := 0;
  m := 0;
  FinalC := 0;
  FinalD := 0;
  FinalM := 0;
  ProgressBar1.Position := 0;
  EditD.Lines.Text := '0';
  EditC.Lines.Text := '0';
  EditM.Lines.Text := '0';
  LabelPercentage.Caption := '0%';
  ButtonFindHash.SetFocus;
  ButtonFindHash.Caption := 'Find Hash Params';
end;

procedure TFrmHashTableGen.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  i: Integer;
begin
  if Assigned(FKeyList) then
  begin
    for i := 0 to FKeyList.Count - 1 do
      with TLexKeys(FKeyList[i]) do
      begin
        Key := FinalHashKey(SynWideLowerCase(KeyName));
      end;
  end;
end;

procedure TFrmHashTableGen.ButtonFindHashClick(Sender: TObject);
var
  i, j: Integer;
  collided: Boolean;
  Key, smallestM: Cardinal;

  procedure SearchStop;
  begin
    ButtonFindHash.Caption := 'Find Hash Params';
    searching := False;
    Close;
    if FinalM = 0 then
      raise Exception.Create('Cannot build the hashtable as no working hash parameters were found');
  end;

begin
  collided := False;
  
  if searching then
  begin
    SearchStop;
    Exit;
  end
  else
  begin
    ProgressBar1.Position := 0;
    LabelPercentage.Caption := '0%';
    Application.ProcessMessages;

    if Length(KeyWords) = 0 then exit;

    searching := True;
    ButtonFindHash.Caption := 'Stop Search';
  end;

  smallestM := MaxTableSize + 1;

  for d := 1 to 1000 do
  begin
    for c := 1 to 1000 do
      for j := 0 to High(PrimeNumbers) do
      begin
        m := PrimeNumbers[j];
        if m >= smallestM then
        begin
          m := smallestM;
          Break;
        end;
        for i := Low(KeyWords) to High(KeyWords) do
        begin
          Key := HashKey(KeyWords[i]);
          collided := HashKeyList.Add(Key);
          if collided then
          begin
            HashKeyList.Clear;
            break;
          end;
        end;
        if not collided then
        begin
          smallestM := m;
          EditD.Lines.Text := IntToStr(d);
          EditC.Lines.Text := IntToStr(c);
          EditM.Lines.Text := IntToStr(m);
          FinalD := d;
          FinalC := c;
          FinalM := m;
          if m = Cardinal(Length(KeyWords)) then
          begin
            ProgressBar1.Position := ProgressBar1.Max;
            LabelPercentage.Caption := '100%';
            SearchStop;
            Exit;
          end;
        break; // all the following solutions will only have a bigger array
        end;
        Application.ProcessMessages;
        if not searching then
        begin
          SearchStop;
          Exit;
        end;
      end;
    ProgressBar1.Position := d;
    LabelPercentage.Caption := FloatToStr(d / 10) + '%';
    Application.ProcessMessages;
  end;

  SearchStop;
end;

function TFrmHashTableGen.GetHashKeyFunctionSource(ClassName: string): string;
begin
  Result := '{$Q-}'#13#10;
  Result := Result + Format('function %s.HashKey(Str: PWideChar): Cardinal;', [ClassName]) + #13#10;
  Result := Result + 'begin'#13#10;
  Result := Result + '  Result := 0;'#13#10;
  Result := Result + '  while IsIdentChar(Str^) do'#13#10;
  Result := Result + '  begin'#13#10;
  if (FinalC = 1) and (FinalD = 1) then
    Result := Result + '    Result := Result + Ord(Str^);'#13#10
  else if FinalC = 1 then
    Result := Result + Format('    Result := Result + Ord(Str^) * %d;', [FinalD]) + #13#10
  else if FinalD = 1 then
    Result := Result + Format('    Result := Result * %d + Ord(Str^);', [FinalC]) + #13#10
  else
    Result := Result + Format('    Result := Result * %d + Ord(Str^) * %d;', [FinalC, FinalD]) + #13#10;
  Result := Result + '    inc(Str);'#13#10;
  Result := Result + '  end;'#13#10;
  Result := Result + '  Result := Result mod ' + IntToStr(FinalM) + ';'#13#10;
  Result := Result + '  fStringLen := Str - fToIdent;'#13#10;
  Result := Result + 'end;'#13#10;
  Result := Result + '{$Q+}'#13#10;
end;

{$IFNDEF SYN_COMPILER_6_UP}
function DupeString(const AText: string; ACount: Integer): string;
var
  P: PChar;
  C: Integer;
begin
  C := Length(AText);
  SetLength(Result, C * ACount);
  P := Pointer(Result);
  if P = nil then Exit;
  while ACount > 0 do
  begin
    Move(Pointer(AText)^, P^, C);
    Inc(P, C);
    Dec(ACount);
  end;
end;
{$ENDIF}

function TFrmHashTableGen.GetKeyWordConstantsSource(CaseSensitive: Boolean): string;
var
  i: Integer;
  sl: TStringList;
  LastItem: string;
begin
  // write KeyWords
  if not CaseSensitive then
    Result := Result + '  // as this language is case-insensitive keywords *must* be in lowercase'#13#10;
  Result := Result + Format('  KeyWords: array[0..%d] of UnicodeString = (', [High(KeyWords)]) + #13#10;
  sl := TStringList.Create;
  try
    for i := Low(KeyWords) to High(KeyWords) do
      sl.Add(#39 + KeyWords[i] + #39 + ',');

    if sl.Count > 0 then
    begin
      // remove comma from last line
      LastItem := sl[sl.Count - 1];
      Delete(LastItem, Length(LastItem), 1);
      sl[sl.Count - 1] := LastItem;

      WordWrapAtCol80(sl, sl, 4);

      Result := Result + sl.Text;
    end;
  finally
    sl.Free;
  end;
  Result := Result + '  );'#13#10;

  Result := Result + #13#10;

  // write KeyIndices
  Result := Result + Format('  KeyIndices: array[0..%d] of Integer = (', [FinalM - 1]) + #13#10;
  sl := TStringList.Create;
  try
    sl.Text := DupeString('-1,'#13#10, FinalM);
    for i := Low(KeyWords) to High(KeyWords) do
      sl[FinalHashKey(KeyWords[i])] := IntToStr(i) + ',';

    if sl.Count > 0 then
    begin
      // remove comma from last line
      LastItem := Trim(sl[sl.Count - 1]);
      Delete(LastItem, Length(LastItem), 1);
      sl[sl.Count - 1] := LastItem;

      WordWrapAtCol80(sl, sl, 4);

      Result := Result + sl.Text;
    end;
  finally
    sl.Free;
  end;
  Result := Result + '  );'#13#10;
end;

function TFrmHashTableGen.KeyIndicesCount: Integer;
begin
  Result := FinalM;
end;

{ THashKeyList }

function THashKeyList.Add(HashKey: Integer): Boolean;
begin
  if HashKey > FMaxHashKey then
    FMaxHashKey := HashKey;
  Result := FHashKeys[HashKey];
  FHashKeys[HashKey] := True;
end;

procedure THashKeyList.Clear;
begin
  FillChar(FHashKeys, FMaxHashKey + 1, 0);
  FMaxHashKey := 0;
end;

end.
