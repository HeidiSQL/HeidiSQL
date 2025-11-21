unit lazaruscompat;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynEditKeyCmds, SynEditHighlighter, laz.VirtualTrees;

type

  // Delphi type aliases
  TSynMemo = TSynEdit;
  TVirtualStringTree = TLazVirtualStringTree;
  TProgressBarState = (pbsNormal, pbsError, pbsPaused);

  // Add methods which exist in Delphi but not in Lazarus
  TSynEditHelper = class helper for TSynEdit
    public
      function GetTextLen: Integer;
      function ConvertCodeStringToCommand(AString: string): TSynEditorCommand;
      function IndexToEditorCommand(const AIndex: Integer): Integer;
  end;
  TSynHighlighterAttributesHelper = class helper for TSynHighlighterAttributes
    public
      procedure AssignColorAndStyle(Source: TSynHighlighterAttributes);
  end;

  TStringsHelper = class helper for TStrings
    public
      function Contains(const S: String): Boolean;
  end;

const
{$IFDEF SYN_CodeFolding}
  EditorCommandStrs: array[0..109] of TIdentMapEntry = (
{$ELSE}
  EditorCommandStrs: array[0..97] of TIdentMapEntry = (
{$ENDIF}
    (Value: ecNone; Name: 'ecNone'),
    (Value: ecLeft; Name: 'ecLeft'),
    (Value: ecRight; Name: 'ecRight'),
    (Value: ecUp; Name: 'ecUp'),
    (Value: ecDown; Name: 'ecDown'),
    (Value: ecWordLeft; Name: 'ecWordLeft'),
    (Value: ecWordRight; Name: 'ecWordRight'),
    (Value: ecLineStart; Name: 'ecLineStart'),
    (Value: ecLineEnd; Name: 'ecLineEnd'),
    (Value: ecPageUp; Name: 'ecPageUp'),
    (Value: ecPageDown; Name: 'ecPageDown'),
    (Value: ecPageLeft; Name: 'ecPageLeft'),
    (Value: ecPageRight; Name: 'ecPageRight'),
    (Value: ecPageTop; Name: 'ecPageTop'),
    (Value: ecPageBottom; Name: 'ecPageBottom'),
    (Value: ecEditorTop; Name: 'ecEditorTop'),
    (Value: ecEditorBottom; Name: 'ecEditorBottom'),
    (Value: ecGotoXY; Name: 'ecGotoXY'),
    (Value: ecSelLeft; Name: 'ecSelLeft'),
    (Value: ecSelRight; Name: 'ecSelRight'),
    (Value: ecSelUp; Name: 'ecSelUp'),
    (Value: ecSelDown; Name: 'ecSelDown'),
    (Value: ecSelWordLeft; Name: 'ecSelWordLeft'),
    (Value: ecSelWordRight; Name: 'ecSelWordRight'),
    (Value: ecSelLineStart; Name: 'ecSelLineStart'),
    (Value: ecSelLineEnd; Name: 'ecSelLineEnd'),
    (Value: ecSelPageUp; Name: 'ecSelPageUp'),
    (Value: ecSelPageDown; Name: 'ecSelPageDown'),
    (Value: ecSelPageLeft; Name: 'ecSelPageLeft'),
    (Value: ecSelPageRight; Name: 'ecSelPageRight'),
    (Value: ecSelPageTop; Name: 'ecSelPageTop'),
    (Value: ecSelPageBottom; Name: 'ecSelPageBottom'),
    (Value: ecSelEditorTop; Name: 'ecSelEditorTop'),
    (Value: ecSelEditorBottom; Name: 'ecSelEditorBottom'),
    (Value: ecSelGotoXY; Name: 'ecSelGotoXY'),
    //(Value: ecSelWord; Name: 'ecSelWord'),
    (Value: ecSelectAll; Name: 'ecSelectAll'),
    (Value: ecDeleteLastChar; Name: 'ecDeleteLastChar'),
    (Value: ecDeleteChar; Name: 'ecDeleteChar'),
    (Value: ecDeleteWord; Name: 'ecDeleteWord'),
    (Value: ecDeleteLastWord; Name: 'ecDeleteLastWord'),
    (Value: ecDeleteBOL; Name: 'ecDeleteBOL'),
    (Value: ecDeleteEOL; Name: 'ecDeleteEOL'),
    (Value: ecDeleteLine; Name: 'ecDeleteLine'),
    (Value: ecClearAll; Name: 'ecClearAll'),
    (Value: ecLineBreak; Name: 'ecLineBreak'),
    (Value: ecInsertLine; Name: 'ecInsertLine'),
    (Value: ecChar; Name: 'ecChar'),
    (Value: ecImeStr; Name: 'ecImeStr'),
    (Value: ecUndo; Name: 'ecUndo'),
    (Value: ecRedo; Name: 'ecRedo'),
    (Value: ecCut; Name: 'ecCut'),
    (Value: ecCopy; Name: 'ecCopy'),
    (Value: ecPaste; Name: 'ecPaste'),
    (Value: ecScrollUp; Name: 'ecScrollUp'),
    (Value: ecScrollDown; Name: 'ecScrollDown'),
    (Value: ecScrollLeft; Name: 'ecScrollLeft'),
    (Value: ecScrollRight; Name: 'ecScrollRight'),
    (Value: ecInsertMode; Name: 'ecInsertMode'),
    (Value: ecOverwriteMode; Name: 'ecOverwriteMode'),
    (Value: ecToggleMode; Name: 'ecToggleMode'),
    (Value: ecBlockIndent; Name: 'ecBlockIndent'),
    (Value: ecBlockUnindent; Name: 'ecBlockUnindent'),
    (Value: ecTab; Name: 'ecTab'),
    (Value: ecShiftTab; Name: 'ecShiftTab'),
    (Value: ecMatchBracket; Name: 'ecMatchBracket'),
    ///(Value: ecCommentBlock; Name: 'ecCommentBlock'),
    (Value: ecNormalSelect; Name: 'ecNormalSelect'),
    (Value: ecColumnSelect; Name: 'ecColumnSelect'),
    (Value: ecLineSelect; Name: 'ecLineSelect'),
    (Value: ecAutoCompletion; Name: 'ecAutoCompletion'),
    (Value: ecUserFirst; Name: 'ecUserFirst'),
    //(Value: ecContextHelp; Name: 'ecContextHelp'),
    (Value: ecGotoMarker0; Name: 'ecGotoMarker0'),
    (Value: ecGotoMarker1; Name: 'ecGotoMarker1'),
    (Value: ecGotoMarker2; Name: 'ecGotoMarker2'),
    (Value: ecGotoMarker3; Name: 'ecGotoMarker3'),
    (Value: ecGotoMarker4; Name: 'ecGotoMarker4'),
    (Value: ecGotoMarker5; Name: 'ecGotoMarker5'),
    (Value: ecGotoMarker6; Name: 'ecGotoMarker6'),
    (Value: ecGotoMarker7; Name: 'ecGotoMarker7'),
    (Value: ecGotoMarker8; Name: 'ecGotoMarker8'),
    (Value: ecGotoMarker9; Name: 'ecGotoMarker9'),
    (Value: ecSetMarker0; Name: 'ecSetMarker0'),
    (Value: ecSetMarker1; Name: 'ecSetMarker1'),
    (Value: ecSetMarker2; Name: 'ecSetMarker2'),
    (Value: ecSetMarker3; Name: 'ecSetMarker3'),
    (Value: ecSetMarker4; Name: 'ecSetMarker4'),
    (Value: ecSetMarker5; Name: 'ecSetMarker5'),
    (Value: ecSetMarker6; Name: 'ecSetMarker6'),
    (Value: ecSetMarker7; Name: 'ecSetMarker7'),
    (Value: ecSetMarker8; Name: 'ecSetMarker8'),
    (Value: ecSetMarker9; Name: 'ecSetMarker9'),
    (Value: {%H-}ecUpperCase; Name: 'ecUpperCase'),
    (Value: {%H-}ecLowerCase; Name: 'ecLowerCase'),
    (Value: {%H-}ecToggleCase; Name: 'ecToggleCase'),
    (Value: {%H-}ecTitleCase; Name: 'ecTitleCase'),
    (Value: {%H-}ecUpperCaseBlock; Name: 'ecUpperCaseBlock'),
    (Value: {%H-}ecLowerCaseBlock; Name: 'ecLowerCaseBlock'),
    (Value: {%H-}ecToggleCaseBlock; Name: 'ecToggleCaseBlock'),
    //(Value: ecTitleCaseBlock; Name: 'ecTitleCaseBlock'),
{$IFDEF SYN_CodeFolding}
    (Value: ecString; Name:'ecString'),
    (Value: ecFoldAll; Name:'ecFoldAll'),
    (Value: ecUnfoldAll; Name:'ecUnfoldAll'),
    (Value: ecFoldNearest; Name:'ecFoldNearest'),
    (Value: ecUnfoldNearest; Name:'ecUnfoldNearest'),
    (Value: ecFoldLevel1; Name:'ecFoldLevel1'),
    (Value: ecFoldLevel2; Name:'ecFoldLevel2'),
    (Value: ecFoldLevel3; Name:'ecFoldLevel3'),
    (Value: ecUnfoldLevel1; Name:'ecUnfoldLevel1'),
    (Value: ecUnfoldLevel2; Name:'ecUnfoldLevel2'),
    (Value: ecUnfoldLevel3; Name:'ecUnfoldLevel3'),
    (Value: ecFoldRegions; Name:'ecFoldRanges'),
    (Value: ecUnfoldRegions; Name:'ecUnfoldRanges'));
{$ELSE}
    (Value: ecString; Name:'ecString'));
{$ENDIF}


implementation


function TSynEditHelper.GetTextLen: Integer;
begin
  Result := Self.Text.Length;
end;

function TSynEditHelper.ConvertCodeStringToCommand(AString: string): TSynEditorCommand;
var
  I: Integer;
begin
  Result := ecNone;

  AString := Uppercase(AString);
  for i := Low(EditorCommandStrs) to High(EditorCommandStrs) do
    if Uppercase(EditorCommandStrs[i].Name) = AString then
    begin
      Result := EditorCommandStrs[i].Value;
      Break;
    end;
end;


function TSynEditHelper.IndexToEditorCommand(const AIndex: Integer): Integer;
begin
  Result := EditorCommandStrs[AIndex].Value;
end;


procedure TSynHighlighterAttributesHelper.AssignColorAndStyle(Source: TSynHighlighterAttributes);
var
  bChanged: Boolean;
begin
  bChanged := False;
  if Background <> Source.Background then
  begin
    Background := Source.Background;
    bChanged := True;
  end;
  if Foreground <> Source.Foreground then
  begin
    Foreground := Source.Foreground;
    bChanged := True;
  end;
  if Style <> Source.Style then
  begin
    Style := Source.Style;
    bChanged := True;
  end;
  if bChanged then
    Changed;
end;

function TStringsHelper.Contains(const S: String): Boolean;
begin
  Result := IndexOf(S) >= 0;
end;

end.

