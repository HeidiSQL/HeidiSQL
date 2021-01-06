unit extra_controls;

interface

uses
  Classes, SysUtils, Forms, Windows, Messages, System.Types, StdCtrls, Clipbrd,
  SizeGrip, apphelpers, Vcl.Graphics, Vcl.Dialogs, gnugettext, Vcl.ImgList, Vcl.ComCtrls,
  ShLwApi, Vcl.ExtCtrls, VirtualTrees, SynRegExpr;

type
  // Form with a sizegrip in the lower right corner, without the need for a statusbar
  TExtForm = class(TForm)
    private
      FSizeGrip: TSizeGripXP;
      function GetHasSizeGrip: Boolean;
      procedure SetHasSizeGrip(Value: Boolean);
    protected
      procedure DoShow; override;
      procedure FilterNodesByEdit(Edit: TButtonedEdit; Tree: TVirtualStringTree);
    public
      constructor Create(AOwner: TComponent); override;
      class procedure InheritFont(AFont: TFont);
      property HasSizeGrip: Boolean read GetHasSizeGrip write SetHasSizeGrip default False;
      class procedure FixControls(ParentComp: TComponent);
  end;
  // Memo replacement which accepts any line break format
  TLineNormalizingMemo = class(TMemo)
    private
      procedure WMSetText(var msg: TWMSettext); message WM_SETTEXT;
      procedure WMPaste(var msg: TWMPaste); message WM_PASTE;
  end;


implementation


{ TExtForm }

constructor TExtForm.Create(AOwner: TComponent);
var
  OldImageList: TCustomImageList;
begin
  inherited;

  InheritFont(Font);
  HasSizeGrip := False;

  // Reduce flicker on Windows 10
  // See https://www.heidisql.com/forum.php?t=19141
  if CheckWin32Version(6, 2) then begin
    DoubleBuffered := True;
  end;

  // Translation and related fixes
  // Issue #557: Apply images *after* translating main menu, so top items don't get unused
  // space left besides them.
  if (Menu <> nil) and (Menu.Images <> nil) then begin
    OldImageList := Menu.Images;
    Menu.Images := nil;
    TranslateComponent(Self);
    Menu.Images := OldImageList;
  end else begin
    TranslateComponent(Self);
  end;

end;


procedure TExtForm.DoShow;
begin
  FixControls(Self);
  inherited;
end;


class procedure TExtForm.FixControls(ParentComp: TComponent);
var
  i: Integer;

  procedure ProcessSingleComponent(Cmp: TComponent);
  begin
    if (Cmp is TButton) and (TButton(Cmp).Style = bsSplitButton) then begin
      // Work around broken dropdown (tool)button on Wine after translation:
      // https://sourceforge.net/p/dxgettext/bugs/80/
      TButton(Cmp).Style := bsPushButton;
      TButton(Cmp).Style := bsSplitButton;
    end;
    if (Cmp is TToolButton) and (TToolButton(Cmp).Style = tbsDropDown) then begin
      // similar fix as above
      TToolButton(Cmp).Style := tbsButton;
      TToolButton(Cmp).Style := tbsDropDown;
    end;
  end;
begin
  // Passed component itself may also be some control to be fixed
  // e.g. TInplaceEditorLink.MainControl
  ProcessSingleComponent(ParentComp);
  for i:=0 to ParentComp.ComponentCount-1 do begin
    ProcessSingleComponent(ParentComp.Components[i]);
  end;
end;


function TExtForm.GetHasSizeGrip: Boolean;
begin
  Result := FSizeGrip <> nil;
end;


procedure TExtForm.SetHasSizeGrip(Value: Boolean);
begin
  if Value then begin
    FSizeGrip := TSizeGripXP.Create(Self);
    FSizeGrip.Enabled := True;
  end else begin
    if FSizeGrip <> nil then
      FreeAndNil(FSizeGrip);
  end;
end;


class procedure TExtForm.InheritFont(AFont: TFont);
var
  LogFont: TLogFont;
  GUIFontName: String;
begin
  // Set custom font if set, or default system font.
  // In high-dpi mode, the font *size* is increased automatically somewhere in the VCL,
  // caused by a form's .Scaled property. So we don't increase it here again.
  // To test this, you really need to log off/on Windows!
  GUIFontName := AppSettings.ReadString(asGUIFontName);
  if not GUIFontName.IsEmpty then begin
    // Apply user specified font
    AFont.Name := GUIFontName;
    // Set size on top of automatic dpi-increased size
    AFont.Size := AppSettings.ReadInt(asGUIFontSize);
  end else begin
    // Apply system font. See issue #3204.
    // Code taken from http://www.gerixsoft.com/blog/delphi/system-font
    if SystemParametersInfo(SPI_GETICONTITLELOGFONT, SizeOf(TLogFont), @LogFont, 0) then begin
      AFont.Height := LogFont.lfHeight;
      AFont.Orientation := LogFont.lfOrientation;
      AFont.Charset := TFontCharset(LogFont.lfCharSet);
      AFont.Name := PChar(@LogFont.lfFaceName);
      case LogFont.lfPitchAndFamily and $F of
        VARIABLE_PITCH: AFont.Pitch := fpVariable;
        FIXED_PITCH: AFont.Pitch := fpFixed;
        else AFont.Pitch := fpDefault;
      end;
    end else begin
      ErrorDialog('Could not detect system font, using SystemParametersInfo.');
    end;
  end;
end;


procedure TExtForm.FilterNodesByEdit(Edit: TButtonedEdit; Tree: TVirtualStringTree);
var
  rx: TRegExpr;
  Node: PVirtualNode;
  i: Integer;
  match: Boolean;
  CellText: String;
begin
  // Loop through all tree nodes and hide non matching
  Node := Tree.GetFirst;
  rx := TRegExpr.Create;
  rx.ModifierI := True;
  rx.Expression := Edit.Text;
  try
    rx.Exec('abc');
  except
    on E:ERegExpr do begin
      if rx.Expression <> '' then begin
        //LogSQL('Filter text is not a valid regular expression: "'+rx.Expression+'"', lcError);
        rx.Expression := '';
      end;
    end;
  end;

  Tree.BeginUpdate;
  while Assigned(Node) do begin
    if not Tree.HasChildren[Node] then begin
      // Don't filter anything if the filter text is empty
      match := rx.Expression = '';
      // Search for given text in node's captions
      if not match then for i := 0 to Tree.Header.Columns.Count - 1 do begin
        CellText := Tree.Text[Node, i];
        match := rx.Exec(CellText);
        if match then
          break;
      end;
      Tree.IsVisible[Node] := match;
    end;
    Node := Tree.GetNext(Node);
  end;
  Tree.EndUpdate;
  Tree.Invalidate;
  rx.Free;

  Edit.RightButton.Visible := IsNotEmpty(Edit.Text);
end;





{ TLineNormalizingMemo }

procedure TLineNormalizingMemo.WMSetText(var msg: TWMSettext);
var
  s: string;
begin
  s := msg.Text;
  s := AdjustLineBreaks(s);
  msg.Text := PChar(s);
  inherited;
end;


procedure TLineNormalizingMemo.WMPaste(var msg: TWMPaste);
var
  s: string;
begin
  if Clipboard.HasFormat(cf_Text) then begin
    s := Clipboard.AsText;
    s := AdjustLineBreaks(s);
    SelText := s;
  end;
end;

end.
