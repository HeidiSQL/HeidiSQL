{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynGenUnit.pas, released 2000-04-19.
Description: Generator for skeletons of HighLighters to use in SynEdit,
drived by a simple grammar.

The Original Code is based on SynGenU.pas by Martin Waldenburg, part of
the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
Portions created by Pieter Polak are Copyright (C) 2001 Pieter Polak.
Unicode translation by Maël Hörz.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynGenUnit.pas,v 1.18.2.11 2008/10/25 23:30:31 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Todo:
  - Remember the last opened MSG file
  - Double-click a MSG file opens SynGen
  - Add user-defined default attributes to TSynXXXSyn.Create
  - SynEdit to edit the MSG file (using the highlighter for MSG files)
  - Store language names list and attribute names list in INI file
  - SynEdit with Pascal highlighter to preview the created highlighter source
  - Allow to define different type of keywords in MSG file

Known Issues:
-------------------------------------------------------------------------------}

unit SynGenUnit;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GenLex, ComCtrls, Menus, SynUnicode;

var
  mKeyHashTable: array[#0..#255] of Integer;
  mSKeyHashTable: array[#0..#255] of Integer;

type
  TLexKeys = class
  public
    KeyName: string;
    Key: Cardinal;
    TokenType: string;
  end;

  TLexCharsets = class
  public
    SetName: string;
    Charset: string;
    ProcData: string;
    FuncData: string;
  end;

  TLexEnclosedBy = class
  public
    TokenName: string;
    ProcName: string;
    StartsWith: string;
    EndsWith: string;
    MultiLine: Boolean;
    constructor Create;
  end;

  TLexDefaultAttri = class
  public
    Style: string;
    Foreground: string;
    Background: string;
    constructor Create;
  end;

  TFormMain = class(TForm)
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    ButtonStart: TButton;
    ComboBoxAttrIdentifier: TComboBox;
    ComboBoxAttrReservedWord: TComboBox;
    ComboBoxFilter: TComboBox;
    ComboBoxLangName: TComboBox;
    ComboBoxUnknownTokenAttr: TComboBox;
    CheckBoxGetKeyWords: TCheckBox;
    CheckBoxGPLHeader: TCheckBox;
    EditAddField: TEdit;
    EditAuthor: TEdit;
    EditDescription: TEdit;
    EditVersion: TEdit;
    GrpAttrNames: TGroupBox;
    LabelAuthor: TLabel;
    LabelDescription: TLabel;
    LabelFilter: TLabel;
    LabelIdentifier: TLabel;
    LabelLangName: TLabel;
    LabelReservedWord: TLabel;
    LabelUnknownTokenAttr: TLabel;
    LabelVersion: TLabel;
    ListBoxFields: TListBox;
    MainMenu: TMainMenu;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemStart: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    TabAttributes: TTabSheet;
    TabFields: TTabSheet;
    TabHighlighter: TTabSheet;
    TabLanguage: TTabSheet;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBoxLangNameChange(Sender: TObject);
    procedure ListBoxFieldsClick(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure EditAddFieldChange(Sender: TObject);
    procedure EditAddFieldKeyPress(Sender: TObject; var Key: Char);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLexName: string;
    FIdentPre: string;
    FIdentStart: string;
    FIdentContent: string;
    FFileName: string;
    FIniFile: string;
    FOutFile: TextFile;
    FSensitivity: Boolean;
    FLexFileContents: UnicodeString;
    FLex: TGenLex;
    FKeyList: TList;
    FSetList: TList;
    FEnclosedList: TList;
    FSampleSourceList: TStringList;
    FIdentList: TStringList;
    procedure ClearAll;
    function GetFilterName: string;
    function GetLangName: string;
    function FilterInvalidChars(const Value: string): string;
    procedure MakeHashTable;
    procedure MakeSensitiveHashTable;
    procedure FillKeyList;
    procedure FillTokenTypeList;
    procedure OutFileCreate(InName: string);
    procedure ParseCharsets;
    procedure ParseEnclosedBy;
    procedure ParseSampleSource;
    procedure RetrieveCharset;
    procedure RetrieveEnclosedBy;
    procedure RetrieveSampleSource;
    procedure WriteSettings;
    function PerformFileOpen: Boolean;
    procedure WriteRest;
    function KeywordsAreAllAlphaNumAndDifferent: Boolean;
    function GetFriendlyLangName: string;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

uses
{$IFDEF SYN_COMPILER_6_UP}
  StrUtils,
{$ENDIF}
  Registry, HashTableGen;

const
  BoolStrs: array[Boolean] of string = ('False', 'True'); // Do not localize

function CompareKeys(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareStr(TLexKeys(Item1).KeyName, TLexKeys(Item2).KeyName);
end;

function CompareSets(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if TLexCharsets(Item1).SetName < TLexCharsets(Item2).SetName then
    Result := -1
  else if TLexCharsets(Item1).SetName > TLexCharsets(Item2).SetName then
    Result := 1;
end;

function AddInt(const aValue: Integer): string;
begin
  if (aValue < 0) then
    Result := ' - ' + IntToStr(Abs(aValue))
  else if (aValue > 0) then
    Result := ' + ' + IntToStr(aValue)
  else
    Result := '';
end;

function StuffString(const Value: UnicodeString): UnicodeString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Value) do
  begin
    if (Value[i] = '''') then
      Result := Result + ''''''
    else
      Result := Result + Value[i];
  end;
end;

function FirstLetterCap(S: UnicodeString): UnicodeString;
begin
  Result := SynWideLowerCase(S);
  if Length(Result) > 0 then
    Result[1] := SynWideUpperCase(S[1])[1];
end;

{$IFNDEF SYN_COMPILER_6_UP}
function AnsiReplaceStr(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;
{$ENDIF}

function ToAlphaNum(S: UnicodeString): UnicodeString;
var
  c: Char;
begin
  for c := #33 to #47 do
    S := AnsiReplaceStr(S, c, IntToStr(Ord(c)));

  for c := #58 to #64 do
    S := AnsiReplaceStr(S, c, IntToStr(Ord(c)));

  for c := #91 to #96 do
    S := AnsiReplaceStr(S, c, IntToStr(Ord(c)));

  for c := #123 to #191 do
    S := AnsiReplaceStr(S, c, IntToStr(Ord(c)));

  Result := S;
end;

function IsASCIIAlphaNum(S: UnicodeString): Boolean;
var
  i: Integer;
begin
  Result := True;

  S := ToAlphaNum(S);

  for i := 1 to Length(S) do
    case S[i] of
      'a'..'z', 'A'..'Z', '0'..'9', '_': ;
      else
      begin
        Result := False;
        Exit;
      end;
    end;
end;

constructor TLexEnclosedBy.Create;
begin
  inherited Create;
  TokenName := '';
  ProcName := '';
  StartsWith := '';
  EndsWith := '';
  MultiLine := False;
end;

constructor TLexDefaultAttri.Create;
begin
  inherited Create;
  Style := '';
  Foreground := '';
  Background := '';
end;

procedure TFormMain.MakeSensitiveHashTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case CharInSet(I, ['_', 'A'..'Z', 'a'..'z']) of
      True:
        begin
          if (I > #64) and (I < #91) then
            mSKeyHashTable[I] := Ord(I) - 64
          else if (I > #96) then
            mSKeyHashTable[I] := Ord(I) - 95;
        end;
    else
      mSKeyHashTable[I] := 0;
    end;
  end;
end;

procedure TFormMain.MakeHashTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    J := UpperCase(I)[1];
    case CharInSet(I, ['_', 'A'..'Z', 'a'..'z']) of
      True: mKeyHashTable[I] := Ord(J) - 64;
    else
      mKeyHashTable[I] := 0;
    end;
  end;
end;

procedure TFormMain.WriteSettings;
begin
  with TRegIniFile.Create(FIniFile) do
  try
    WriteString('General', 'OpenDir', OpenDialog.InitialDir);
    WriteBool(FFileName, 'GetKeyWords', CheckBoxGetKeyWords.Checked);
    WriteBool(FFileName, 'CheckBoxGPLHeader', CheckBoxGPLHeader.Checked);
    WriteString(FFileName, 'Author', EditAuthor.Text);
    WriteString(FFileName, 'Description', EditDescription.Text);
    WriteString(FFileName, 'Version', EditVersion.Text);
    WriteString(FFileName, 'Filter', ComboBoxFilter.Text);
    WriteString(FFileName, 'Language', ComboBoxLangName.Text);
    WriteString(FFileName, 'AttrIdentifier', ComboBoxAttrIdentifier.Text);
    WriteString(FFileName, 'AttrReservedWord', ComboBoxAttrReservedWord.Text);
    WriteString(FFileName, 'UnknownTokenAttr', ComboBoxUnknownTokenAttr.Text);
    WriteString(FFileName, 'Fields', ListBoxFields.Items.CommaText);
  finally
    Free;
  end;
end;

function TFormMain.PerformFileOpen: Boolean;
var
  UserName: PChar;
{$IFDEF SYN_COMPILER_5_UP}
  Count: Cardinal;
{$ELSE}
  Count: Integer;
{$ENDIF}
begin
  if OpenDialog.Execute then
  begin
    Count := 0;
    Result := True;
    FFileName := ExtractFileName(OpenDialog.FileName);
    Caption := 'SynGen - ' + FFileName;
    Application.Title := Caption;
    OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
    GetUserName(nil, Count);
    // retrieve the required size of the user name buffer
    UserName := StrAlloc(Count); // allocate memory for the user name
    GetUserName(UserName, Count); // retrieve the user name
    with TRegIniFile.Create(FIniFile) do
    try
      EditAuthor.Text := ReadString(FFileName, 'Author', StrPas(UserName));
      EditDescription.Text := ReadString(FFileName, 'Description',
        'Syntax Parser/Highlighter');
      EditVersion.Text := ReadString(FFileName, 'Version', '0.1');
      ComboBoxFilter.Text := ReadString(FFileName, 'Filter', 'All files (*.*)|*.*');
      ComboBoxLangName.Text := ReadString(FFileName, 'Language', '');
      CheckBoxGetKeyWords.Checked := ReadBool(FFileName, 'GetKeyWords', True);
      CheckBoxGPLHeader.Checked := ReadBool(FFileName, 'CheckBoxGPLHeader', True);
      ComboBoxAttrIdentifier.ItemIndex := ComboBoxAttrIdentifier.Items.IndexOf
        (ReadString(FFileName, 'AttrIdentifier', 'SYNS_AttrIdentifier'));
      ComboBoxAttrReservedWord.ItemIndex := ComboBoxAttrReservedWord.Items.IndexOf
        (ReadString(FFileName, 'AttrReservedWord', 'SYNS_AttrReservedWord'));
      ComboBoxUnknownTokenAttr.ItemIndex := ComboBoxUnknownTokenAttr.Items.IndexOf
        (ReadString(FFileName, 'UnknownTokenAttr', 'Identifier'));
      ListBoxFields.Items.CommaText := ReadString(FFileName, 'Fields', '');
    finally
      Free;
    end;
    StrDispose(UserName);
    ComboBoxLangNameChange(Self);
  end
  else
    Result := False;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: Integer;
  items: TStrings;
begin
  for i := FormMain.ComponentCount - 1 downto 0 do
    if FormMain.Components[i] is TComboBox then
      if TComboBox(FormMain.Components[i]).Parent = GrpAttrNames then
      begin
        items := TComboBox(FormMain.Components[i]).Items;
        items.Clear;
        items.Add('SYNS_AttrAsm');
        items.Add('SYNS_AttrAsmComment');
        items.Add('SYNS_AttrAsmKey');
        items.Add('SYNS_AttrASP');
        items.Add('SYNS_AttrAssembler');
        items.Add('SYNS_AttrBlock');
        items.Add('SYNS_AttrBrackets');
        items.Add('SYNS_AttrCharacter');
        items.Add('SYNS_AttrClass');
        items.Add('SYNS_AttrComment');
        items.Add('SYNS_AttrCondition');
        items.Add('SYNS_AttrDir');
        items.Add('SYNS_AttrDirective');
        items.Add('SYNS_AttrDocumentation');
        items.Add('SYNS_AttrEmbedSQL');
        items.Add('SYNS_AttrEmbedText');
        items.Add('SYNS_AttrEscapeAmpersand');
        items.Add('SYNS_AttrForm');
        items.Add('SYNS_AttrFunction');
        items.Add('SYNS_AttrIcon');
        items.Add('SYNS_AttrIdentifier');
        items.Add('SYNS_AttrIllegalChar');
        items.Add('SYNS_AttrIndirect');
        items.Add('SYNS_AttrInvalidSymbol');
        items.Add('SYNS_AttrInternalFunction');
        items.Add('SYNS_AttrKey');
        items.Add('SYNS_AttrLabel');
        items.Add('SYNS_AttrMacro');
        items.Add('SYNS_AttrMarker');
        items.Add('SYNS_AttrMessage');
        items.Add('SYNS_AttrMiscellaneous');
        items.Add('SYNS_AttrNull');
        items.Add('SYNS_AttrNumber');
        items.Add('SYNS_AttrOperator');
        items.Add('SYNS_AttrPragma');
        items.Add('SYNS_AttrPreprocessor');
        items.Add('SYNS_AttrQualifier');
        items.Add('SYNS_AttrRegister');
        items.Add('SYNS_AttrReservedWord');
        items.Add('SYNS_AttrRpl');
        items.Add('SYNS_AttrRplKey');
        items.Add('SYNS_AttrRplComment');
        items.Add('SYNS_AttrSASM');
        items.Add('SYNS_AttrSASMComment');
        items.Add('SYNS_AttrSASMKey');
        items.Add('SYNS_AttrSecondReservedWord');
        items.Add('SYNS_AttrSection');
        items.Add('SYNS_AttrSpace');
        items.Add('SYNS_AttrSpecialVariable');
        items.Add('SYNS_AttrString');
        items.Add('SYNS_AttrSymbol');
        items.Add('SYNS_AttrSyntaxError');
        items.Add('SYNS_AttrSystem');
        items.Add('SYNS_AttrSystemValue');
        items.Add('SYNS_AttrText');
        items.Add('SYNS_AttrUnknownWord');
        items.Add('SYNS_AttrUser');
        items.Add('SYNS_AttrUserFunction');
        items.Add('SYNS_AttrValue');
        items.Add('SYNS_AttrVariable');
      end;
  PageControl.ActivePage := PageControl.Pages[0];
  FLex := TGenLex.Create;
  FKeyList := TList.Create;
  FSetList := TList.Create;
  FEnclosedList := TList.Create;
  FSampleSourceList := TStringList.Create;
  FIdentList := TStringList.Create;
  // read ini file
  FIniFile := Copy(ExtractFileName(Application.ExeName), 0,
    Length(ExtractFileName(Application.ExeName)) -
    Length(ExtractFileExt(Application.ExeName))) + '.ini';
  with TRegIniFile.Create(FIniFile) do
  try
    OpenDialog.InitialDir := ReadString('General', 'OpenDir',
      ExtractFilePath(Application.ExeName));
  finally
    Free;
  end;

  { Move form off the screen, but show already, to activate it correctly when
    OpenFileDialog is closed with OK. }
  Left := -10000;
  Show;          
  if PerformFileOpen then
  begin
    MakeHashTable;
    MakeSensitiveHashTable;
    Position := poScreenCenter; // move form on the screen ("make visible")
  end
  else
    Application.Terminate
end;

procedure TFormMain.ClearAll;
var
  I: Integer;
begin
  // Clear the contents of FKeyList
  for I := 0 to (FKeyList.Count - 1) do
    TObject(FKeyList[I]).Free;
  FKeyList.Clear;
  // Clear the contents of FSetList
  for I := 0 to (FSetList.Count - 1) do
    TObject(FSetList[I]).Free;
  FSetList.Clear;
  // Clear the contents of FEnclosedList
  for I := 0 to (FEnclosedList.Count - 1) do
    TObject(FEnclosedList[I]).Free;
  FEnclosedList.Clear;
  // Clear the contents of FIdentList
  for I := 0 to (FIdentList.Count - 1) do
  begin
    if Assigned(FIdentList.Objects[I]) then
      TObject(FIdentList.Objects[I]).Free;
  end;
  FIdentList.Clear;
  // Clear the contents of FSampleSourceList
  FSampleSourceList.Clear;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  ClearAll;
  FLex.Free;
  FIdentList.Free;
  FKeyList.Free;
  FSetList.Free;
  FEnclosedList.Free;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);
var
  LexFileLines: TUnicodeStringList;
begin
  ClearAll;

  Screen.Cursor := crHourGlass;

  LexFileLines := TUnicodeStringList.Create;
  try
    LexFileLines.LoadFromFile(OpenDialog.FileName);
    FLexFileContents := LexFileLines.Text;
  finally
    LexFileLines.Free;
  end;
  FLex.Origin := PWideChar(FLexFileContents);
  FLex.Tokenize;

  while FLex.RunId <> IDIdentifier do
    FLex.Next;
  FLexName := FLex.RunToken;

  FLex.Next;
  while FLex.RunId <> IDIdentifier do
    FLex.Next;
  FIdentPre := FLex.RunToken;

  OutFileCreate(OpenDialog.FileName);
  try
    while not (FLex.RunId in [IdSensitive, IdIdentStart]) do
      FLex.Next;

    if FLex.RunId = IdSensitive then
      FSensitivity := True
    else
      FSensitivity := False;
    FLex.Next;

    while FLex.RunId <> IDCharSet do
      FLex.Next;
    FIdentStart := FLex.RunToken;
    FLex.Next;

    while FLex.RunId <> IDNull do
    begin
      case FLex.RunId of
        IDCharSet: FIdentContent := FLex.RunToken;
        IDKeys: FillKeyList;
        IDTokenTypes: FillTokenTypeList;
        IDChars: ParseCharSets;
        IDEnclosedBy: ParseEnclosedBy;
        IDSampleSource: ParseSampleSource;
      end;
      FLex.Next;
    end;

    if (FKeyList.Count = 0) then
      raise Exception.Create('You should specify at least 1 keyword!');
    if (FIdentList.Count = 0) then
      raise Exception.Create('You should specify at least 1 token type');
    if not KeywordsAreAllAlphaNumAndDifferent then
      raise Exception.Create('One or more keywords contain unhandable characters');
      
    FrmHashTableGen.AssignKeyWords(FKeyList, FSensitivity);
    FrmHashTableGen.ShowModal;

    WriteRest;
    while (FLex.RunId <> IdNull) do
    begin
      FLex.Next;
    end;
  finally
    Screen.Cursor := crDefault;
    CloseFile(FOutFile);
  end;
  MessageDlg(FLexName + ' created on ' + DateTimeToStr(Now), mtInformation,
    [mbOk], 0);
end;

procedure TFormMain.FillKeyList;
var
  aLexKey: TLexKeys;
  aString: string;
  aTokenType: string;
begin
  FLex.Next;

  aTokenType := '';
  while FLex.RunId <> IdCRLF do
  begin
    if not (FLex.RunId in [IdSpace, IdBraceOpen]) then
      aTokenType := aTokenType + FLex.RunToken;
    FLex.Next;
  end;

  if (aTokenType = '') then
    aTokenType := 'Key';

  while FLex.RunId <> IdStop do
  begin
    while FLex.RunId in [IdSpace, IdBraceOpen, IdCRLF] do
      FLex.Next;
    if FLex.RunId <> IdStop then
    begin
      aString := '';
      while not (FLex.RunId in [IdSpace, IdBraceOpen, IdCRLF]) do
      begin
        aString := aString + FLex.RunToken;
        FLex.Next;
      end;
      aLexKey := TLexKeys.Create;
      aLexKey.TokenType := aTokenType;
      aLexKey.KeyName := aString;
      FKeyList.Add(aLexKey);
    end
    else
      Break;
    FLex.Next;
  end;
  FKeyList.Sort(CompareKeys);
end;

procedure TFormMain.FillTokenTypeList;
var
  i: Integer;
  List: TStringList;
  sIdent: string;
  sLine: string;
  DefAttri: TLexDefaultAttri;
begin
  FLex.Next;
  FIdentList.Add(FIdentPre + 'Unknown');
  FIdentList.Add(FIdentPre + 'Null');
  while (FLex.RunId <> IdStop) do
  begin
    while FLex.RunId in [IdSpace, IdBraceOpen, IdCRLF, IDUnknown] do
      FLex.Next;
    if (FLex.RunId <> IdStop) then
    begin
      sIdent := FIdentPre + FLex.RunToken;
      if not IsValidIdent(sIdent) then
        raise Exception.Create('Invalid identifier for token type: ' + sIdent);

      if (FIdentList.IndexOf(sIdent) < 0) then
        FIdentList.Add(sIdent);
      FLex.Next;

      sLine := '';
      while (FLex.RunId = IdSpace) do
        FLex.Next;
      while not (FLex.RunId in [IdStop, IdCRLF]) do
      begin { is there more data on this line? }
        sLine := sLine + FLex.RunToken;
        FLex.Next;
      end;

      if (sLine <> '') then { The Msg file specifies default attributes }
      begin
        List := TStringList.Create;
        try
          while (sLine <> '') do
          begin
            i := Pos('|', sLine);
            if (i > 0) then
            begin
              List.Add(Copy(sLine, 1, i - 1));
              Delete(sLine, 1, i);
            end
            else
            begin
              List.Add(sLine);
              sLine := '';
            end;
          end;

          i := FIdentList.IndexOf(sIdent);
          if (i >= 0) then
          begin
            DefAttri := TLexDefaultAttri.Create;
            DefAttri.Style := List.Values['Style'];
            DefAttri.Foreground := List.Values['Foreground'];
            DefAttri.Background := List.Values['Background'];
            FIdentList.Objects[i] := DefAttri;
          end;
        finally
          List.Free;
        end;
      end;
    end
    else
      Break;
  end;
end;

procedure TFormMain.OutFileCreate(InName: string);
var
  OutName, UName: string;
  sysTime: TSystemTime;
  ISODate: string;
begin
  OutName := ChangeFileExt(InName, '.pas');
  Uname := ExtractFileName(ChangeFileExt(InName, ''));
  AssignFile(FOutFile, OutName);
  rewrite(FOutFile);
  GetSystemTime(sysTime);
  ISODate := Format('%.4d-%.2d-%.2d', [sysTime.wYear, sysTime.wMonth,
    sysTime.wDay]);
  if CheckBoxGPLHeader.Checked then
  begin
    Writeln(FOutFile,
      '{-------------------------------------------------------------------------------');
    Writeln(FOutFile,
      'The contents of this file are subject to the Mozilla Public License');
    Writeln(FOutFile,
      'Version 1.1 (the "License"); you may not use this file except in compliance');
    Writeln(FOutFile,
      'with the License. You may obtain a copy of the License at');
    Writeln(FOutFile, 'http://www.mozilla.org/MPL/');
    Writeln(FOutFile);
    Writeln(FOutFile,
      'Software distributed under the License is distributed on an "AS IS" basis,');
    Writeln(FOutFile,
      'WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for');
    Writeln(FOutFile,
      'the specific language governing rights and limitations under the License.');
    Writeln(FOutFile);
    Writeln(FOutFile, 'Code template generated with SynGen.');
    Writeln(FOutFile, 'The original code is: ' + OutName + ', released ' + ISODate
      + '.');
    Writeln(FOutFile, 'Description: ' + EditDescription.Text);
    Writeln(FOutFile, 'The initial author of this file is ' + EditAuthor.Text +
      '.');
    Writeln(FOutFile, 'Copyright (c) ' + Format('%d', [sysTime.wYear]) +
      ', all rights reserved.');
    Writeln(FOutFile);
    Writeln(FOutFile,
      'Contributors to the SynEdit and mwEdit projects are listed in the');
    Writeln(FOutFile, 'Contributors.txt file.');
    Writeln(FOutFile);
    Writeln(FOutFile,
      'Alternatively, the contents of this file may be used under the terms of the');
    Writeln(FOutFile,
      'GNU General Public License Version 2 or later (the "GPL"), in which case');
    Writeln(FOutFile,
      'the provisions of the GPL are applicable instead of those above.');
    Writeln(FOutFile,
      'If you wish to allow use of your version of this file only under the terms');
    Writeln(FOutFile,
      'of the GPL and not to allow others to use your version of this file');
    Writeln(FOutFile,
      'under the MPL, indicate your decision by deleting the provisions above and');
    Writeln(FOutFile,
      'replace them with the notice and other provisions required by the GPL.');
    Writeln(FOutFile,
      'If you do not delete the provisions above, a recipient may use your version');
    Writeln(FOutFile, 'of this file under either the MPL or the GPL.');
    Writeln(FOutFile);
    Writeln(FOutFile, '$' + 'Id: ' + '$');
    Writeln(FOutFile);
    Writeln(FOutFile,
      'You may retrieve the latest version of this file at the SynEdit home page,');
    Writeln(FOutFile, 'located at http://SynEdit.SourceForge.net');
    Writeln(FOutFile);
    Writeln(FOutFile,
      '-------------------------------------------------------------------------------}');
  end
  else
  begin
    Writeln(FOutFile,
      '{+-----------------------------------------------------------------------------+');
    Writeln(FOutFile, ' | Class:       ' + FLexName);
    Writeln(FOutFile, ' | Created:     ' + ISODate);
    Writeln(FOutFile, ' | Last change: ' + ISODate);
    Writeln(FOutFile, ' | Author:      ' + EditAuthor.Text);
    Writeln(FOutFile, ' | Description: ' + EditDescription.Text);
    Writeln(FOutFile, ' | Version:     ' + EditVersion.Text);
    Writeln(FOutFile, ' |');
    Writeln(FOutFile, ' | Copyright (c) ' + Format('%d', [sysTime.wYear]) + #32 +
      EditAuthor.Text + '. All rights reserved.');
    Writeln(FOutFile, ' |');
    Writeln(FOutFile, ' | Generated with SynGen.');
    Writeln(FOutFile,
      ' +----------------------------------------------------------------------------+}');
  end;
  Writeln(FOutFile);
  Writeln(FOutFile, 'unit ' + Uname + ';');
  Writeln(FOutFile);
  Writeln(FOutFile, '{$I SynEdit.inc}');
  Writeln(FOutFile);
  Writeln(FOutFile, 'interface');
  Writeln(FOutFile);
  Writeln(FOutFile, 'uses');
  Writeln(FOutFile, '  Graphics,');
  Writeln(FOutFile, '  SynEditTypes,');
  Writeln(FOutFile, '  SynEditHighlighter,');
  Writeln(FOutFile, '  SynUnicode,');
  Writeln(FOutFile, '  SysUtils,');
  Writeln(FOutFile, '  Classes;');
  Writeln(FOutFile);
  Writeln(FOutFile, 'type');
  Writeln(FOutFile, '  T' + FIdentPre + 'TokenKind = (');
end;

procedure TFormMain.ParseCharsets;
begin
  FLex.Next;
  while FLex.RunId <> IdStop do
  begin
    case FLex.RunId of
      IdCharset: RetrieveCharset;
    else
      FLex.Next;
    end;
  end;
end;

procedure TFormMain.ParseEnclosedBy;
begin
  FLex.Next;
  while not (FLex.RunId in [IdStop, IdNull]) do
    RetrieveEnclosedBy;
end;

procedure TFormMain.ParseSampleSource;
begin
  FLex.Next;
  if (FLex.RunId = IdCRLF) then
    FLex.Next;

  while not (FLex.RunId in [IdStop, IdNull]) do
    RetrieveSampleSource;
end;

procedure TFormMain.RetrieveCharset;
var
  aSet: TLexCharsets;
begin
  aSet := TLexCharsets.Create;
  aSet.Charset := FLex.RunToken;
  while FLex.RunId <> IDIdentifier do
    FLex.Next;
  aSet.SetName := FLex.RunToken;
  while FLex.RunId <> IDBeginProc do
    FLex.Next;
  FLex.Next;
  while FLex.RunId in [IdCRLF, IdSpace] do
    FLex.Next;
  while not (FLex.RunId = IdEndProc) do
  begin
    aSet.ProcData := aSet.ProcData + FLex.RunToken;
    FLex.Next;
  end;
  FSetList.Add(aSet);
  FLex.Next;
end;

procedure TFormMain.RetrieveSampleSource;
var
  sLine: string;
begin
  sLine := '';
  while not (FLex.RunId in [IdCRLF, IdNull, IdStop]) do
  begin
    sLine := sLine + FLex.RunToken;
    FLex.Next;
  end;
  if (FLex.RunId = IdCRLF) then
    FLex.Next;

  FSampleSourceList.Add(sLine);
end;

procedure TFormMain.RetrieveEnclosedBy;
var
  aThing: TLexEnclosedBy;
  sLine: string;
  iPos: Integer;
begin
  while FLex.RunId in [IdCRLF, IdSpace] do
    FLex.Next;

  sLine := '';
  while not (FLex.RunId in [IdCRLF, IdNull, IdStop]) do
  begin
    sLine := sLine + FLex.RunToken;
    FLex.Next;
  end;

  if (sLine <> '') then
  begin
    aThing := TLexEnclosedBy.Create;

    iPos := Pos(',', sLine);
    aThing.TokenName := Copy(sLine, 1, iPos - 1);
    Delete(sLine, 1, iPos);

    iPos := Pos(',', sLine);
    aThing.ProcName := Copy(sLine, 1, iPos - 1);
    Delete(sLine, 1, iPos);

    iPos := Pos(',', sLine);
    aThing.StartsWith := Copy(sLine, 1, iPos - 1);
    Delete(sLine, 1, iPos);

    iPos := Pos(',', sLine);
    if (iPos > 0) then
    begin
      aThing.EndsWith := Copy(sLine, 1, iPos - 1);
      Delete(sLine, 1, iPos);
      if (Pos('MULTILINE', UpperCase(sLine)) = 1) then
        aThing.MultiLine := True;
    end
    else
      aThing.EndsWith := sLine;

    FEnclosedList.Add(aThing);
  end
  else if (FLex.RunId <> IdStop) then
    FLex.Next;
end; { RetrieveEnclosedBy }

function TFormMain.FilterInvalidChars(const Value: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Value) do
  begin
    if IsValidIdent(Result + Value[i]) then
      Result := Result + Value[i];
  end;
end; { FilterInvalidChars }

function TFormMain.GetFilterName: string;
var
  FilterName: string;
begin
  FilterName := '';
  case ComboBoxFilter.ItemIndex of
    -1: FilterName := 'SYNS_Filter' + FilterInvalidChars(ComboBoxLangName.Text);
    0: FilterName := 'SYNS_FilterPascal';
    1: FilterName := 'SYNS_FilterHP48';
    2: FilterName := 'SYNS_FilterCAClipper';
    3: FilterName := 'SYNS_FilterCPP';
    4: FilterName := 'SYNS_FilterJava';
    5: FilterName := 'SYNS_FilterPerl';
    6: FilterName := 'SYNS_FilterAWK';
    7: FilterName := 'SYNS_FilterHTML';
    8: FilterName := 'SYNS_FilterVBScript';
    9: FilterName := 'SYNS_FilterGalaxy';
    10: FilterName := 'SYNS_FilterPython';
    11: FilterName := 'SYNS_FilterSQL';
    12: FilterName := 'SYNS_FilterTclTk';
    13: FilterName := 'SYNS_FilterRTF';
    14: FilterName := 'SYNS_FilterBatch';
    15: FilterName := 'SYNS_FilterDFM';
    16: FilterName := 'SYNS_FilterX86Asm';
    17: FilterName := 'SYNS_FilterGembase';
    18: FilterName := 'SYNS_FilterINI';
    19: FilterName := 'SYNS_FilterML';
    20: FilterName := 'SYNS_FilterVisualBASIC';
    21: FilterName := 'SYNS_FilterADSP21xx';
    22: FilterName := 'SYNS_FilterPHP';
    23: FilterName := 'SYNS_FilterCache';
    24: FilterName := 'SYNS_FilterCSS';
    25: FilterName := 'SYNS_FilterJScript';
    26: FilterName := 'SYNS_FilterKIX';
    27: FilterName := 'SYNS_FilterBaan';
    28: FilterName := 'SYNS_FilterFoxpro';
    29: FilterName := 'SYNS_FilterFortran';
    30: FilterName := 'SYNS_FilterAsm68HC11';
  end;
  Result := FilterName;
end;

function TFormMain.GetFriendlyLangName: string;
var
  LangName: string;
begin
  case ComboBoxLangName.ItemIndex of
    -1: LangName := 'SYNS_FriendlyLang' + FilterInvalidChars(ComboBoxLangName.Text);
    0: LangName := 'SYNS_FriendlyLangHP48';
    1: LangName := 'SYNS_FriendlyLangCAClipper';
    2: LangName := 'SYNS_FriendlyLangCPP';
    3: LangName := 'SYNS_FriendlyLangJava';
    4: LangName := 'SYNS_FriendlyLangPerl';
    5: LangName := 'SYNS_FriendlyLangBatch';
    6: LangName := 'SYNS_FriendlyLangDfm';
    7: LangName := 'SYNS_FriendlyLangAWK';
    8: LangName := 'SYNS_FriendlyLangHTML';
    9: LangName := 'SYNS_FriendlyLangVBSScript';
    10: LangName := 'SYNS_FriendlyLangGalaxy';
    11: LangName := 'SYNS_FriendlyLangGeneral';
    12: LangName := 'SYNS_FriendlyLangPascal';
    13: LangName := 'SYNS_FriendlyLangX86Asm';
    14: LangName := 'SYNS_FriendlyLangPython';
    15: LangName := 'SYNS_FriendlyLangTclTk';
    16: LangName := 'SYNS_FriendlyLangSQL';
    17: LangName := 'SYNS_FriendlyLangGembase';
    18: LangName := 'SYNS_FriendlyLangINI';
    19: LangName := 'SYNS_FriendlyLangML';
    20: LangName := 'SYNS_FriendlyLangVisualBASIC';
    21: LangName := 'SYNS_FriendlyLangADSP21xx';
    22: LangName := 'SYNS_FriendlyLangPHP';
    23: LangName := 'SYNS_FriendlyLangSybaseSQL';
    24: LangName := 'SYNS_FriendlyLangGeneralMulti';
    25: LangName := 'SYNS_FriendlyLangCache';
    26: LangName := 'SYNS_FriendlyLangCSS';
    27: LangName := 'SYNS_FriendlyLangJScript';
    28: LangName := 'SYNS_FriendlyLangKIX';
    29: LangName := 'SYNS_FriendlyLangBaan';
    30: LangName := 'SYNS_FriendlyLangFoxpro';
    31: LangName := 'SYNS_FriendlyLangFortran';
    32: LangName := 'SYNS_FriendlyLang68HC11';
  end;
  Result := LangName;
end;

function TFormMain.GetLangName: string;
var
  LangName: string;
begin
  case ComboBoxLangName.ItemIndex of
    -1: LangName := 'SYNS_Lang' + FilterInvalidChars(ComboBoxLangName.Text);
    0: LangName := 'SYNS_LangHP48';
    1: LangName := 'SYNS_LangCAClipper';
    2: LangName := 'SYNS_LangCPP';
    3: LangName := 'SYNS_LangJava';
    4: LangName := 'SYNS_LangPerl';
    5: LangName := 'SYNS_LangBatch';
    6: LangName := 'SYNS_LangDfm';
    7: LangName := 'SYNS_LangAWK';
    8: LangName := 'SYNS_LangHTML';
    9: LangName := 'SYNS_LangVBSScript';
    10: LangName := 'SYNS_LangGalaxy';
    11: LangName := 'SYNS_LangGeneral';
    12: LangName := 'SYNS_LangPascal';
    13: LangName := 'SYNS_LangX86Asm';
    14: LangName := 'SYNS_LangPython';
    15: LangName := 'SYNS_LangTclTk';
    16: LangName := 'SYNS_LangSQL';
    17: LangName := 'SYNS_LangGembase';
    18: LangName := 'SYNS_LangINI';
    19: LangName := 'SYNS_LangML';
    20: LangName := 'SYNS_LangVisualBASIC';
    21: LangName := 'SYNS_LangADSP21xx';
    22: LangName := 'SYNS_LangPHP';
    23: LangName := 'SYNS_LangSybaseSQL';
    24: LangName := 'SYNS_LangGeneralMulti';
    25: LangName := 'SYNS_LangCache';
    26: LangName := 'SYNS_LangCSS';
    27: LangName := 'SYNS_LangJScript';
    28: LangName := 'SYNS_LangKIX';
    29: LangName := 'SYNS_LangBaan';
    30: LangName := 'SYNS_LangFoxpro';
    31: LangName := 'SYNS_LangFortran';
    32: LangName := 'SYNS_Lang68HC11';
  end;
  Result := LangName;
end;

procedure TFormMain.WriteRest;
var
  I, J: Integer;
  LineLength: Integer;
  KeyString: string;
  AttrName: string;
  FriendlyAttrName: string;
  AttrTemp: string;
  TempStringList: TStringList;
  sPrefix: string;
  DefAttri: TLexDefaultAttri;
begin
  FIdentList.Sort;
  FSetList.Sort(CompareSets);
  I := 0;
  while I < FIdentList.Count - 1 do
  begin
    Writeln(FOutFile, '    ' + FIdentList[I] + ',');
    inc(I);
  end;
  Writeln(FOutFile, '    ' + FIdentList[I] + ');');
  Writeln(FOutFile);
  Write(FOutFile, '  TRangeState = (rsUnknown');
  for I := 0 to (FEnclosedList.Count - 1) do
    Write(FOutFile, ', rs' + TLexEnclosedBy(FEnclosedList[I]).ProcName);
  Writeln(FOutFile, ');');
  Writeln(FOutFile);
  Writeln(FOutFile, '  TProcTableProc = procedure of object;');
  Writeln(FOutFile);
  Writeln(FOutFile, '  PIdentFuncTableFunc = ^TIdentFuncTableFunc;');
  Writeln(FOutFile, '  TIdentFuncTableFunc = function (Index: Integer): T' + FIdentPre +
    'TokenKind of object;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'type');
  Writeln(FOutFile, '  ' + FLexName + ' = class(TSynCustomHighlighter)');
  Writeln(FOutFile, '  private');
  Writeln(FOutFile, '    FRange: TRangeState;');

  if ListBoxFields.Items.Count > 0 then
    for i := 0 to ListBoxFields.Items.Count - 1 do
      Writeln(FOutFile, '    ' + ListBoxFields.Items[i] + ';');

  Writeln(FOutFile, '    FTokenId: TtkTokenKind;');
  Writeln(FOutFile,
    '    fIdentFuncTable: array[0..' +
    IntToStr(FrmHashTableGen.KeyIndicesCount - 1) + ']' +
    ' of TIdentFuncTableFunc;');

  I := 0;
  while I < FIdentList.Count do
  begin
    if (FIdentList[I] <> FIdentPre + 'Null') and (FIdentList[I] <> FIdentPre +
      'Unknown') then
      Writeln(FOutFile, '    f' + Copy(FIdentList[I], Length(FIdentPre) + 1,
        Length(FIdentList[I])) + 'Attri: TSynHighlighterAttributes;');
    inc(I);
  end;

  Writeln(FOutFile, '    function HashKey(Str: PWideChar): Cardinal;');

  I := 0;
  while I < FKeyList.Count do
  begin
    Writeln(FOutFile, AnsiString('    function Func' +
      ToAlphaNum(FirstLetterCap(TLexKeys(FKeyList[I]).KeyName)) +
      '(Index: Integer): T' + FIdentPre + 'TokenKind;'));
    inc(I);
  end;

  I := 0;
  while I < FSetList.Count do
  begin
    Writeln(FOutFile, '    procedure ' + TLexCharsets(FSetList[I]).SetName +
      'Proc;');
    inc(I);
  end;

  Writeln(FOutFile, '    procedure UnknownProc;');
  Writeln(FOutFile, '    function AltFunc(Index: Integer): T' + FIdentPre + 'TokenKind;');
  Writeln(FOutFile, '    procedure InitIdent;');
  Writeln(FOutFile, '    function IdentKind(MayBe: PWideChar): T' + FIdentPre +
    'TokenKind;');
  Writeln(FOutFile, '    procedure NullProc;');
  if (FIdentList.IndexOf(FIdentPre + 'Space') >= 0) then
    Writeln(FOutFile, '    procedure SpaceProc;');
  Writeln(FOutFile, '    procedure CRProc;');
  Writeln(FOutFile, '    procedure LFProc;');
  for I := 0 to (FEnclosedList.Count - 1) do
  begin
    Writeln(FOutFile, '    procedure ' + TLexEnclosedBy(FEnclosedList[I]).ProcName
      + 'OpenProc;');
    Writeln(FOutFile, '    procedure ' + TLexEnclosedBy(FEnclosedList[I]).ProcName
      + 'Proc;');
  end;
  Writeln(FOutFile, '  protected');
  Writeln(FOutFile, '    function GetSampleSource: UnicodeString; override;');
  Writeln(FOutFile, '    function IsFilterStored: Boolean; override;');
  Writeln(FOutFile, '  public');
  Writeln(FOutFile, '    constructor Create(AOwner: TComponent); override;');
  Writeln(FOutFile, '    class function GetFriendlyLanguageName: UnicodeString; override;');
  Writeln(FOutFile, '    class function GetLanguageName: string; override;');
  Writeln(FOutFile, '    function GetRange: Pointer; override;');
  Writeln(FOutFile, '    procedure ResetRange; override;');
  Writeln(FOutFile, '    procedure SetRange(Value: Pointer); override;');
  Writeln(FOutFile,
    '    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;');
  Writeln(FOutFile, '    function GetEol: Boolean; override;');
  if CheckBoxGetKeyWords.Checked then
    Writeln(FOutFile, '    function GetKeyWords(TokenKind: Integer): UnicodeString; override;');
  Writeln(FOutFile, '    function GetTokenID: TtkTokenKind;');
  Writeln(FOutFile,
    '    function GetTokenAttribute: TSynHighlighterAttributes; override;');
  Writeln(FOutFile, '    function GetTokenKind: Integer; override;');
  Writeln(FOutFile, '    function IsIdentChar(AChar: WideChar): Boolean; override;');
  Writeln(FOutFile, '    procedure Next; override;');
  Writeln(FOutFile, '  published');

  I := 0;
  while I < FIdentList.Count do
  begin
    if (FIdentList[I] <> FIdentPre + 'Null') and (FIdentList[I] <> FIdentPre +
      'Unknown') then
      Writeln(FOutFile, '    property ' + Copy(FIdentList[I], Length(FIdentPre) +
        1, Length(FIdentList[I]))
        + 'Attri: TSynHighlighterAttributes read f' + Copy(FIdentList[I],
        Length(FIdentPre) + 1, Length(FIdentList[I])) +
        'Attri write f' + Copy(FIdentList[I], Length(FIdentPre) + 1,
        Length(FIdentList[I])) + 'Attri;');
    inc(I);
  end;

  Writeln(FOutFile, '  end;');
  Writeln(FOutFile);
  Writeln(FOutFile, 'implementation');
  Writeln(FOutFile);
  Writeln(FOutFile, 'uses');
  Writeln(FOutFile, '  SynEditStrConst;');
  Writeln(FOutFile);
  if (ComboBoxFilter.ItemIndex = -1) or (ComboBoxLangName.ItemIndex = -1) then
  begin
    Writeln(FOutFile, 'resourcestring');
    if (ComboBoxFilter.ItemIndex = -1) then
      Writeln(FOutFile, '  SYNS_Filter' + FilterInvalidChars(ComboBoxLangName.Text) +
        ' = ''' + ComboBoxFilter.Text + ''';');
    if (ComboBoxLangName.ItemIndex = -1) then
    begin
      Writeln(FOutFile, '  SYNS_Lang' + FilterInvalidChars(ComboBoxLangName.Text) +
        ' = ''' + ComboBoxLangName.Text + ''';');

      Writeln(FOutFile, '  SYNS_FriendlyLang' + FilterInvalidChars(ComboBoxLangName.Text) +
        ' = ''' + ComboBoxLangName.Text + ''';');
    end;

    I := 0;
    while I < FIdentList.Count do
    begin
      AttrTemp := Copy(FIdentList[I], Length(FIdentPre) + 1,
        Length(FIdentList[I]));
      if (ComboBoxAttrIdentifier.Items.IndexOf('SYNS_Attr' + AttrTemp) < 0) and
        (AttrTemp <> 'Unknown') then
      begin
        Writeln(FOutFile, '  SYNS_Attr' + FilterInvalidChars(AttrTemp) + ' = '''
          + AttrTemp + ''';');
        Writeln(FOutFile, '  SYNS_FriendlyAttr' + FilterInvalidChars(AttrTemp) + ' = '''
          + AttrTemp + ''';');
      end;
      Inc(i);
    end;
    Writeln(FOutFile);
  end;

  Writeln(FOutFile, 'const');
  Write(FOutFile, FrmHashTableGen.GetKeyWordConstantsSource(FSensitivity));
  Writeln(FOutFile);

  Writeln(FOutFile, 'procedure ' + FLexName + '.InitIdent;');
  Writeln(FOutFile, 'var');
  Writeln(FOutFile, '  i: Integer;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do');
  Writeln(FOutFile, '    if KeyIndices[i] = -1 then');
  Writeln(FOutFile, '      fIdentFuncTable[i] := AltFunc;');
  Writeln(FOutFile, '');

  I := 0;
  while I < FKeyList.Count do
  begin
    if I < FKeyList.Count - 1 then
      while TLexKeys(FKeyList[I]).Key = TLexKeys(FKeyList[I + 1]).Key do
      begin
        inc(I);
        if I >= FKeyList.Count - 1 then
          break;
      end;
    KeyString := IntToStr(TLexKeys(FKeyList[I]).Key);
    Writeln(FOutFile, '  fIdentFuncTable[' + KeyString + '] := Func' +
      ToAlphaNum(FirstLetterCap(TLexKeys(FKeyList[I]).KeyName)) + ';');
    inc(I);
  end;

  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Write(FOutFile, FrmHashTableGen.GetHashKeyFunctionSource(FLexName));
  Writeln(FOutFile);

  I := 0;
  while I < FKeyList.Count do
  begin
    KeyString := ToAlphaNum(FirstLetterCap(TLexKeys(FKeyList[I]).KeyName));
    Writeln(FOutFile, 'function ' + FLexName + '.Func' + KeyString + '(Index: Integer): T' +
      FIdentPre + 'TokenKind;');
    Writeln(FOutFile, 'begin');
    if I < FKeyList.Count - 1 then
      while TLexKeys(FKeyList[I]).Key = TLexKeys(FKeyList[I + 1]).Key do
      begin
        Writeln(FOutFile, '  if IsCurrentToken(KeyWords[Index]) then');
        Writeln(FOutFile, '    Result := ' + FIdentPre + TLexKeys(FKeyList[I]).TokenType);
        Writeln(FOutFile, '  else');
        inc(I);
        if I >= FKeyList.Count - 1 then
          break;
      end;
      Writeln(FOutFile, '  if IsCurrentToken(KeyWords[Index]) then');
      Writeln(FOutFile, '    Result := ' + FIdentPre + TLexKeys(FKeyList[I]).TokenType);
      Writeln(FOutFile, '  else');
      Writeln(FOutFile, '    Result := ' + FIdentPre + 'Identifier;');
    Writeln(FOutFile, 'end;');
    Writeln(FOutFile);
    inc(I);
  end;

  Writeln(FOutFile, 'function ' + FLexName + '.AltFunc(Index: Integer): T' + FIdentPre +
    'TokenKind;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := ' + FIdentPre + 'Identifier;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName + '.IdentKind(MayBe: PWideChar): T' +
    FIdentPre + 'TokenKind;');
  Writeln(FOutFile, 'var');
  Writeln(FOutFile, '  Key: Cardinal;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  fToIdent := MayBe;');
  Writeln(FOutFile, '  Key := HashKey(MayBe);');
  Writeln(FOutFile, '  if Key <= High(fIdentFuncTable) then');
  Writeln(FOutFile, '    Result := FIdentFuncTable[Key](KeyIndices[Key])');
  Writeln(FOutFile, '  else');
  Writeln(FOutFile, '    Result := ' + FIdentPre + 'Identifier;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  if (FIdentList.IndexOf(FIdentPre + 'Space') >= 0) then
  begin
    Writeln(FOutFile, 'procedure ' + FLexName + '.SpaceProc;');
    Writeln(FOutFile, 'begin');
    Writeln(FOutFile, '  inc(Run);');
    Writeln(FOutFile, '  FTokenId := ' + FIdentPre + 'Space;');
    Writeln(FOutFile, '  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);');
    Writeln(FOutFile, 'end;');
    Writeln(FOutFile);
  end;

  Writeln(FOutFile, 'procedure ' + FLexName + '.NullProc;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  FTokenId := ' + FIdentPre + 'Null;');
  Writeln(FOutFile, '  inc(Run);');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'procedure ' + FLexName + '.CRProc;');
  Writeln(FOutFile, 'begin');
  if (FIdentList.IndexOf(FIdentPre + 'Space') >= 0) then
    Writeln(FOutFile, '  FTokenId := ' + FIdentPre + 'Space;')
  else
    Writeln(FOutFile, '  FTokenId := ' + FIdentPre + 'Unknown;');
  Writeln(FOutFile, '  inc(Run);');
  Writeln(FOutFile, '  if FLine[Run] = #10 then');
  Writeln(FOutFile, '    inc(Run);');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'procedure ' + FLexName + '.LFProc;');
  Writeln(FOutFile, 'begin');
  if (FIdentList.IndexOf(FIdentPre + 'Space') >= 0) then
    Writeln(FOutFile, '  FTokenId := ' + FIdentPre + 'Space;')
  else
    Writeln(FOutFile, '  FTokenId := ' + FIdentPre + 'Unknown;');
  Writeln(FOutFile, '  inc(Run);');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  for I := 0 to (FEnclosedList.Count - 1) do
  begin
    Writeln(FOutFile, 'procedure ' + FLexName + '.' +
      TLexEnclosedBy(FEnclosedList[I]).ProcName + 'OpenProc;');
    Writeln(FOutFile, 'begin');
    Writeln(FOutFile, '  Inc(Run);');
    if (Length(TLexEnclosedBy(FEnclosedList[I]).StartsWith) > 1) then
    begin
      Write(FOutFile, '  if ');
      for J := 2 to Length(TLexEnclosedBy(FEnclosedList[I]).StartsWith) do
      begin
        if (J > 2) then
        begin
          Writeln(FOutFile, ' and');
          Write(FOutFile, '     ');
        end;
        Write(FOutFile, '(FLine[Run' + AddInt(J - 2) + '] = ''' +
          StuffString(TLexEnclosedBy(FEnclosedList[I]).StartsWith[J]) + ''')');
      end;
      Writeln(FOutFile, ' then');
      Writeln(FOutFile, '  begin');
      Writeln(FOutFile, '    Inc(Run, ' +
        IntToStr(Length(TLexEnclosedBy(FEnclosedList[I]).StartsWith)-1) + ');');
      Writeln(FOutFile, '    FRange := rs' +
        TLexEnclosedBy(FEnclosedList[I]).ProcName + ';');
      if not TLexEnclosedBy(FEnclosedList[I]).MultiLine then
      begin
        Writeln(FOutFile, '    ' + TLexEnclosedBy(FEnclosedList[I]).ProcName +
          'Proc;');
      end;
      Writeln(FOutFile, '    FTokenId := ' + FIdentPre +
        TLexEnclosedBy(FEnclosedList[I]).TokenName + ';');
      Writeln(FOutFile, '  end');
      Writeln(FOutFile, '  else');
      if (FIdentList.IndexOf(FIdentPre + 'Symbol') >= 0) then
        Writeln(FOutFile, '    FTokenId := ' + FIdentPre + 'Symbol;')
      else
        Writeln(FOutFile, '    FTokenId := ' + FIdentPre + 'Identifier;');
    end
    else
    begin
      Writeln(FOutFile, '  FRange := rs' +
        TLexEnclosedBy(FEnclosedList[I]).ProcName + ';');
      if not TLexEnclosedBy(FEnclosedList[I]).MultiLine then
      begin
        Writeln(FOutFile, '  ' + TLexEnclosedBy(FEnclosedList[I]).ProcName +
          'Proc;');
      end;
      Writeln(FOutFile, '  FTokenId := ' + FIdentPre +
        TLexEnclosedBy(FEnclosedList[I]).TokenName + ';');
    end;
    Writeln(FOutFile, 'end;');
    Writeln(FOutFile);
    Writeln(FOutFile, 'procedure ' + FLexName + '.' +
      TLexEnclosedBy(FEnclosedList[I]).ProcName + 'Proc;');
    Writeln(FOutFile, 'begin');
    if TLexEnclosedBy(FEnclosedList[I]).MultiLine then
    begin
      Writeln(FOutFile, '  case FLine[Run] of');
      Writeln(FOutFile, '     #0: NullProc;');
      Writeln(FOutFile, '    #10: LFProc;');
      Writeln(FOutFile, '    #13: CRProc;');
      Writeln(FOutFile, '  else');
      Writeln(FOutFile, '    begin');
      sPrefix := '    ';
    end
    else
      sPrefix := '';
    Writeln(FOutFile, sPrefix, '  FTokenId := ' + FIdentPre +
      TLexEnclosedBy(FEnclosedList[I]).TokenName + ';');
    Writeln(FOutFile, sPrefix, '  repeat');
    Write(FOutFile, sPrefix, '    if ');
    for J := 1 to Length(TLexEnclosedBy(FEnclosedList[I]).EndsWith) do
    begin
      if (J > 1) then
      begin
        Writeln(FOutFile, ' and');
        Write(FOutFile, sPrefix, '       ');
      end;
      Write(FOutFile, '(FLine[Run' + AddInt(J - 1) + '] = ''' +
        StuffString(TLexEnclosedBy(FEnclosedList[I]).EndsWith[J]) + ''')');
    end;
    Writeln(FOutFile, ' then');
    Writeln(FOutFile, sPrefix, '    begin');
    Writeln(FOutFile, sPrefix, '      Inc(Run, ' +
      IntToStr(Length(TLexEnclosedBy(FEnclosedList[I]).EndsWith)) + ');');
    Writeln(FOutFile, sPrefix, '      FRange := rsUnknown;');
    Writeln(FOutFile, sPrefix, '      Break;');
    Writeln(FOutFile, sPrefix, '    end;');
    Writeln(FOutFile, sPrefix, '    if not IsLineEnd(Run) then');
    Writeln(FOutFile, sPrefix, '      Inc(Run);');
    Writeln(FOutFile, sPrefix, '  until IsLineEnd(Run);');
    Writeln(FOutFile, sPrefix, 'end;');
    if TLexEnclosedBy(FEnclosedList[I]).MultiLine then
    begin
      Writeln(FOutFile, '  end;');
      Writeln(FOutFile, 'end;');
    end;
    Writeln(FOutFile);
  end;

  Writeln(FOutFile, 'constructor ' + FLexName + '.Create(AOwner: TComponent);');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  inherited Create(AOwner);');
  Writeln(FOutFile, '  fCaseSensitive := ' + BoolStrs[FSensitivity] + ';');
  Writeln(FOutFile);
  
  I := 0;
  while I < FIdentList.Count do
  begin
    AttrTemp := Copy(FIdentList[I], Length(FIdentPre) + 1, Length(FIdentList[I]));
    if AttrTemp = 'Key' then
      AttrName := ComboBoxAttrReservedWord.Text
    else if AttrTemp = 'Identifier' then
      AttrName := ComboBoxAttrIdentifier.Text
    else
      AttrName := 'SYNS_Attr' + FilterInvalidChars(AttrTemp);

    if Pos('SYNS_', AttrName) = 1 then
    begin
      FriendlyAttrName := AttrName;
      Insert('Friendly', FriendlyAttrName, Length('SYNS_') + 1)
    end
    else
      FriendlyAttrName := 'Friendly' + AttrName;

    if (FIdentList[I] <> FIdentPre + 'Null') and (FIdentList[I] <> FIdentPre +
      'Unknown') then
    begin
      AttrTemp := 'f' + AttrTemp + 'Attri';
      Writeln(FOutFile, '  ' + AttrTemp + ' := TSynHighLighterAttributes.Create('
        + AttrName + ', ' + FriendlyAttrName + ');');
      if Assigned(FIdentList.Objects[i]) then
      begin
        DefAttri := TLexDefaultAttri(FIdentList.Objects[i]);
        if (DefAttri.Style <> '') then
          Writeln(FOutFile, '  ' + AttrTemp + '.Style := ' + DefAttri.Style +
            ';');
        if (DefAttri.Foreground <> '') then
          Writeln(FOutFile, '  ' + AttrTemp + '.Foreground := ' +
            DefAttri.Foreground + ';');
        if (DefAttri.Background <> '') then
          Writeln(FOutFile, '  ' + AttrTemp + '.Background := ' +
            DefAttri.Background + ';');
      end
      else if (FIdentList[I] = FIdentPre + 'Key') then
        Writeln(FOutFile, '  ' + AttrTemp + '.Style := [fsBold];')
      else if (FIdentList[I] = FIdentPre + 'Comment') then
      begin
        Writeln(FOutFile, '  ' + AttrTemp + '.Style := [fsItalic];');
        Writeln(FOutFile, '  ' + AttrTemp + '.Foreground := clNavy;');
      end;
      Writeln(FOutFile, '  AddAttribute(' + AttrTemp + ');');
      Writeln(FOutFile);
    end;
    Inc(I);
  end;

  Writeln(FOutFile, '  SetAttributesOnChange(DefHighlightChange);');
  Writeln(FOutFile, '  InitIdent;');

  Writeln(FOutFile, '  fDefaultFilter := ' + GetFilterName + ';');
  Writeln(FOutFile, '  FRange := rsUnknown;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  I := 0;
  while I < FSetList.Count do
  begin
    Writeln(FOutFile, 'procedure ' + FLexName + '.' +
      TLexCharsets(FSetList[I]).SetName + 'Proc;');
    Writeln(FOutFile, 'begin');
    Write(FOutFile, '  ' + TLexCharsets(FSetList[I]).ProcData);
    Writeln(FOutFile, 'end;');
    Writeln(FOutFile);
    inc(I);
  end;

  Writeln(FOutFile, 'procedure ' + FLexName + '.UnknownProc;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  inc(Run);');
  Writeln(FOutFile, '  FTokenId := ' + FIdentPre + 'Unknown;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'procedure ' + FLexName + '.Next;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  fTokenPos := Run;');
  if (FEnclosedList.Count > 0) then
  begin
    Writeln(FOutFile, '  case FRange of');
    for I := 0 to (FEnclosedList.Count - 1) do
    begin
      if TLexEnclosedBy(FEnclosedList[I]).MultiLine then
      begin
        Writeln(FOutFile, '    rs' + TLexEnclosedBy(FEnclosedList[I]).ProcName +
          ': ' + TLexEnclosedBy(FEnclosedList[I]).ProcName + 'Proc;');
      end;
    end;
    Writeln(FOutFile, '  else');
    Writeln(FOutFile, '    case FLine[Run] of');
    Writeln(FOutFile, '      #0: NullProc;');
    Writeln(FOutFile, '      #10: LFProc;');
    Writeln(FOutFile, '      #13: CRProc;');

    for I := 0 to (FEnclosedList.Count - 1) do
    begin
      if (TLexEnclosedBy(FEnclosedList[I]).StartsWith <> '') then
      begin
        Writeln(FOutFile, '      ''' +
          StuffString(TLexEnclosedBy(FEnclosedList[I]).StartsWith[1]) +
          ''': ' + TLexEnclosedBy(FEnclosedList[I]).ProcName + 'OpenProc;');
      end;
    end;
    if (FIdentList.IndexOf(FIdentPre + 'Space') >= 0) then
      Writeln(FOutFile, '      #1..#9, #11, #12, #14..#32: SpaceProc;');
    I := 0;
    while I < FSetList.Count do
    begin
      Writeln(FOutFile, '      ' + TLexCharsets(FSetList[I]).Charset +
        ': ' + TLexCharsets(FSetList[I]).SetName + 'Proc;');
      Inc(I);
    end;

    Writeln(FOutFile, '    else');
    Writeln(FOutFile, '      UnknownProc;');
    Writeln(FOutFile, '    end;');
    Writeln(FOutFile, '  end;');
  end
  else
  begin
    Writeln(FOutFile, '  case FLine[Run] of');
    Writeln(FOutFile, '    #0: NullProc;');
    Writeln(FOutFile, '    #10: LFProc;');
    Writeln(FOutFile, '    #13: CRProc;');

    if (FIdentList.IndexOf(FIdentPre + 'Space') >= 0) then
      Writeln(FOutFile, '    #1..#9, #11, #12, #14..#32: SpaceProc;');
    I := 0;
    while I < FSetList.Count do
    begin
      Writeln(FOutFile, '    ' + TLexCharsets(FSetList[I]).Charset +
        ': ' + TLexCharsets(FSetList[I]).SetName + 'Proc;');
      Inc(I);
    end;

    Writeln(FOutFile, '  else');
    Writeln(FOutFile, '    UnknownProc;');
    Writeln(FOutFile, '  end;');
  end;
  Writeln(FOutFile, '  inherited;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName +
    '.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  case Index of');
  if (FIdentList.IndexOf(FIdentPre + 'Comment') >= 0) then
    Writeln(FOutFile, '    SYN_ATTR_COMMENT: Result := FCommentAttri;');
  if (FIdentList.IndexOf(FIdentPre + 'Identifier') >= 0) then
    Writeln(FOutFile, '    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;');
  if (FIdentList.IndexOf(FIdentPre + 'Key') >= 0) then
    Writeln(FOutFile, '    SYN_ATTR_KEYWORD: Result := FKeyAttri;');
  if (FIdentList.IndexOf(FIdentPre + 'String') >= 0) then
    Writeln(FOutFile, '    SYN_ATTR_STRING: Result := FStringAttri;');
  if (FIdentList.IndexOf(FIdentPre + 'Space') >= 0) then
    Writeln(FOutFile, '    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;');
  if (FIdentList.IndexOf(FIdentPre + 'Symbol') >= 0) then
    Writeln(FOutFile, '    SYN_ATTR_SYMBOL: Result := FSymbolAttri;');
  Writeln(FOutFile, '  else');
  Writeln(FOutFile, '    Result := nil;');
  Writeln(FOutFile, '  end;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName + '.GetEol: Boolean;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := Run = FLineLen + 1;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  if CheckBoxGetKeyWords.Checked then
  begin
    Writeln(FOutFile, 'function ' + FLexName + '.GetKeyWords(TokenKind: Integer): UnicodeString;');
    Writeln(FOutFile, 'begin');
    TempStringList := TStringList.Create;
    try
      TempStringList.Sorted := True;
      for I := 0 to FKeyList.Count - 1 do
        TempStringList.Add(TLexKeys(FKeyList[I]).KeyName);
      if TempStringList.Count > 0 then
      begin
        Writeln(FOutFile, '  Result := ');
        for I := 0 to Trunc(Int(Length(TempStringList.CommaText) div 70)) - 1 do
        begin
          if I = 0 then
            LineLength := 69
          else
            LineLength := 70;
          Writeln(FOutFile, '    ' + #39 + Copy(TempStringList.CommaText,
            I * 70, LineLength) + #39 + #32 + #43);
        end;
        I := Trunc(Int(Length(TempStringList.CommaText) div 70));
        Writeln(FOutFile, '    ' + #39 + Copy(TempStringList.CommaText,
          I * 70, Length(TempStringList.CommaText)) + #39 + ';')
      end
      else
        Writeln(FOutFile, '  Result := ' + #39 + #39 + ';');
    finally
      TempStringList.Free;
    end;
    Writeln(FOutFile, 'end;');
    Writeln(FOutFile);
  end;

  Writeln(FOutFile, 'function ' + FLexName + '.GetTokenID: TtkTokenKind;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := FTokenId;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName +
    '.GetTokenAttribute: TSynHighLighterAttributes;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  case GetTokenID of');

  I := 0;
  while I < FIdentList.Count do
  begin
    if (FIdentList[I] <> FIdentPre + 'Null') and (FIdentList[I] <> FIdentPre +
      'Unknown') then
      Writeln(FOutFile, '    ' + FIdentList[I] + ': Result := F' +
        Copy(FIdentList[I], Length(FIdentPre) + 1, Length(FIdentList[I])) +
        'Attri;');
    inc(I);
  end;
  Writeln(FOutFile, '    ' + FIdentPre + 'Unknown: Result := F' +
    ComboBoxUnknownTokenAttr.Text + 'Attri;');

  Writeln(FOutFile, '  else');
  Writeln(FOutFile, '    Result := nil;');
  Writeln(FOutFile, '  end;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName + '.GetTokenKind: Integer;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := Ord(FTokenId);');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName + '.IsIdentChar(AChar: WideChar): Boolean;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  case AChar of');
  Writeln(FOutFile, '    ' + FIdentContent + ':');
  Writeln(FOutFile, '      Result := True;');
  Writeln(FOutFile, '    else');
  Writeln(FOutFile, '      Result := False;');
  Writeln(FOutFile, '  end;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName + '.GetSampleSource: UnicodeString;');
  Writeln(FOutFile, 'begin');
  if (FSampleSourceList.Count = 0) then
  begin
    Writeln(FOutFile, '  Result := ');
    Writeln(FOutFile, '    ''Sample source for: ''#13#10 +');
    Writeln(FOutFile, '    ''' + EditDescription.Text + ''';');
  end
  else
  begin
    Writeln(FOutFile, '  Result := ');
    for i := 0 to FSampleSourceList.Count - 1 do
    begin
      if (i > 0) and (i < FSampleSourceList.Count - 1) then
        Writeln(FOutFile, '#13#10 +');
      if (i < FSampleSourceList.Count - 1) then
        Write(FOutFile, '    ');
      if FSampleSourceList[i] <> '' then
        Write(FOutFile, '''', StuffString(FSampleSourceList[i]), '''');
    end;
    Writeln(FOutFile, ';');
  end;
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName + '.IsFilterStored: Boolean;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := FDefaultFilter <> ' + GetFilterName + ';');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'class function ' + FLexName + '.GetFriendlyLanguageName: UnicodeString;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := ' + GetFriendlyLangName + ';');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'class function ' + FLexName + '.GetLanguageName: string;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := ' + GetLangName + ';');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'procedure ' + FLexName + '.ResetRange;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  FRange := rsUnknown;');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'procedure ' + FLexName + '.SetRange(Value: Pointer);');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  FRange := TRangeState(Value);');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'function ' + FLexName + '.GetRange: Pointer;');
  Writeln(FOutFile, 'begin');
  Writeln(FOutFile, '  Result := Pointer(FRange);');
  Writeln(FOutFile, 'end;');
  Writeln(FOutFile);

  Writeln(FOutFile, 'initialization');
  Writeln(FOutFile, '{$IFNDEF SYN_CPPB_1}');
  Writeln(FOutFile, '  RegisterPlaceableHighlighter(' + FLexName + ');');
  Writeln(FOutFile, '{$ENDIF}');
  Writeln(FOutFile, 'end.');
end;

procedure TFormMain.ComboBoxLangNameChange(Sender: TObject);
begin
  if (ComboBoxLangName.Text <> '') and (ComboBoxFilter.Text <> '') then
    ButtonStart.Enabled := True
  else
    ButtonStart.Enabled := False;
end;

procedure TFormMain.ListBoxFieldsClick(Sender: TObject);
begin
  ButtonDelete.Enabled := True;
end;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  ListBoxFields.Items.Add(EditAddField.Text);
  EditAddField.Clear;
end;

procedure TFormMain.ButtonDeleteClick(Sender: TObject);
begin
  ButtonDelete.Enabled := False;
  ListBoxFields.Items.Delete(ListBoxFields.ItemIndex);
end;

procedure TFormMain.EditAddFieldChange(Sender: TObject);
begin
  ButtonAdd.Enabled := EditAddField.Text <> '';
end;

procedure TFormMain.EditAddFieldKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ';') or (Key = #32) then
    Key := #0;
end;

procedure TFormMain.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuItemOpenClick(Sender: TObject);
begin
  WriteSettings;
  PerformFileOpen;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteSettings;
end;

function TFormMain.KeywordsAreAllAlphaNumAndDifferent: Boolean;
var
  i: Integer;
  KeyWordList: TStringList;
begin
  Result := True;

  KeyWordList := TStringList.Create;
  try
    KeyWordList.Sorted := True;
    KeyWordList.Duplicates := dupError;

    try
      for i := 0 to FKeyList.Count - 1 do
        KeyWordList.Add(TLexKeys(FKeyList[i]).KeyName);
    except
      Result := False;
      Exit;
    end;
  finally
    KeyWordList.Free;
  end;

  for i := 0 to FKeyList.Count - 1 do
    if not IsASCIIAlphaNum(TLexKeys(FKeyList[i]).KeyName) then
    begin
      Result := False;
      Exit;
    end;
end;

end.
