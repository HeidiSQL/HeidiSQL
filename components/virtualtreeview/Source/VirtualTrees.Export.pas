unit VirtualTrees.Export;

{$WARN UNSAFE_CODE OFF}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

interface

uses Winapi.Windows,
     VirtualTrees,
     VirtualTrees.Classes;

function ContentToHTML(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType; const Caption: string = ''): String;
function ContentToRTF(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType): RawByteString;
function ContentToUnicodeString(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType; const Separator: string): string;
function ContentToClipboard(Tree: TCustomVirtualStringTree; Format: Word; Source: TVSTTextSourceType): HGLOBAL;
procedure ContentToCustom(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType);

implementation

uses
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  VirtualTrees.Types,
  VirtualTrees.ClipBoard,
  VirtualTrees.Header,
  VirtualTrees.BaseTree;

type
  TCustomVirtualStringTreeCracker = class(TCustomVirtualStringTree)
  end;

const
  WideCR = Char(#13);
  WideLF = Char(#10);



function ContentToHTML(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType; const Caption: string): String;

// Renders the current tree content (depending on Source) as HTML text encoded in UTF-8.
// If Caption is not empty then it is used to create and fill the header for the table built here.
// Based on ideas and code from Frank van den Bergh and Andreas H�rstemeier.

var
  Buffer: TBufferedString;

  //--------------- local functions -------------------------------------------

  procedure WriteColorAsHex(Color: TColor);

  var
    WinColor: COLORREF;
    I: Integer;
    Component,
    Value: Byte;

  begin
    Buffer.Add('#');
    WinColor := ColorToRGB(Color);
    I := 1;
    while I <= 6 do
    begin
      Component := WinColor and $FF;

      Value := 48 + (Component shr 4);
      if Value > $39 then
        System.Inc(Value, 7);
      Buffer.Add(AnsiChar(Value));
      System.Inc(I);

      Value := 48 + (Component and $F);
      if Value > $39 then
        System.Inc(Value, 7);
      Buffer.Add(AnsiChar(Value));
      System.Inc(I);

      WinColor := WinColor shr 8;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure WriteStyle(const Name: String; Font: TFont);

  // Creates a CSS style entry with the given name for the given font.
  // If Name is empty then the entry is created as inline style.

  begin
    if Length(Name) = 0 then
      Buffer.Add(' style="')
    else
    begin
      Buffer.Add('.');
      Buffer.Add(Name);
      Buffer.Add('{');
    end;

    Buffer.Add(Format('font-family: ''%s''; ', [Font.Name]));
    if Font.Size < 0 then
      Buffer.Add(Format('font-size: %dpx; ', [Font.Height]))
    else
      Buffer.Add(Format('font-size: %dpt; ', [Font.Size]));

    Buffer.Add(Format('font-style: %s; ', [IfThen(TFontStyle.fsItalic in Font.Style, 'italic', 'normal')]));
    Buffer.Add(Format('font-weight: %s; ', [IfThen(TFontStyle.fsBold in Font.Style, 'bold', 'normal')]));
    Buffer.Add(Format('text-decoration: %s; ', [IfThen(TFontStyle.fsUnderline in Font.Style, 'underline', 'none')]));

    Buffer.Add('color: ');
    WriteColorAsHex(Font.Color);
    Buffer.Add(';');
    if Length(Name) = 0 then
      Buffer.Add('"')
    else
      Buffer.Add('}');
  end;

  //--------------- end local functions ---------------------------------------

var
  I, J : Integer;
  Level, MaxLevel: Cardinal;
  AddHeader: String;
  Save, Run: PVirtualNode;
  GetNextNode: TGetNextNodeProc;

  RenderColumns: Boolean;
  Columns: TColumnsArray;
  ColumnColors: array of String;
  Index: Integer;
  IndentWidth,
  LineStyleText: String;
  Alignment: TAlignment;
  BidiMode: TBidiMode;

  CellPadding: String;
  CrackTree: TCustomVirtualStringTreeCracker;
  lGetCellTextEventArgs: TVSTGetCellTextEventArgs;
begin
  CrackTree := TCustomVirtualStringTreeCracker(Tree);

  CrackTree.StartOperation(TVTOperationKind.okExport);
  Buffer := TBufferedString.Create;
  lGetCellTextEventArgs.ExportType := TVTExportType.etHtml;
  try
    // For customization by the application or descendants we use again the redirected font change event.
    CrackTree.RedirectFontChangeEvent(CrackTree.Canvas);

    CellPadding := Format('padding-left: %dpx; padding-right: %0:dpx;', [CrackTree.Margin]);

    IndentWidth := IntToStr(CrackTree.Indent);
    AddHeader := ' ';
    // Add title if adviced so by giving a caption.
    if Length(Caption) > 0 then
      AddHeader := AddHeader + 'caption="' + Caption + '"';
    if CrackTree.Borderstyle <> TFormBorderStyle.bsNone then
      AddHeader := AddHeader + Format(' border="%d" frame=box', [CrackTree.BorderWidth + 1]);

    Buffer.Add('<META http-equiv="Content-Type" content="text/html; charset=utf-8">');

    // Create HTML table based on the CrackTree structure. To simplify formatting we use styles defined in a small CSS area.
    Buffer.Add('<style type="text/css">');
    Buffer.AddnewLine;
    WriteStyle('default', CrackTree.Font);
    Buffer.AddNewLine;
    WriteStyle('header', CrackTree.Header.Font);
    Buffer.AddNewLine;

    // Determine grid/table lines and create CSS for it.
    // Vertical and/or horizontal border to show.
    if CrackTree.LineStyle = lsSolid then
      LineStyleText := 'solid;'
    else
      LineStyleText := 'dotted;';
    if toShowHorzGridLines in CrackTree.TreeOptions.PaintOptions then
    begin
      Buffer.Add('.noborder{');
      Buffer.Add(' border-bottom:1px; border-left: 0px; border-right: 0px; border-top: 1px;');
      Buffer.Add('border-style:');
      Buffer.Add(LineStyleText);
      Buffer.Add(CellPadding);
      Buffer.Add('}');
    end
    else
    begin
      Buffer.Add('.noborder{border-style: none;');
      Buffer.Add(CellPadding);
      Buffer.Add('}');
    end;
    Buffer.AddNewLine;

    Buffer.Add('.normalborder {vertical-align: top; ');
    if toShowVertGridLines in CrackTree.TreeOptions.PaintOptions then
      Buffer.Add('border-right: 1px; border-left: 1px; ')
    else
      Buffer.Add('border-right: none; border-left:none; ');
    if toShowHorzGridLines in CrackTree.TreeOptions.PaintOptions then
      Buffer.Add('border-top: 1px; border-bottom: 1px; ')
    else
      Buffer.Add('border-top:none; border-bottom: none;');
    Buffer.Add('border-width: thin; border-style: ');
    Buffer.Add(LineStyleText);
    Buffer.Add(CellPadding);
    Buffer.Add('}');
    Buffer.Add('</style>');
    Buffer.AddNewLine;

    // General table properties.
    Buffer.Add('<table class="default" style="border-collapse: collapse;" bgcolor=');
    WriteColorAsHex(CrackTree.Color);
    Buffer.Add(AddHeader);
    Buffer.Add(' cellspacing="0">');
    Buffer.AddNewLine;

    Columns := nil;
    ColumnColors := nil;
    RenderColumns := CrackTree.Header.UseColumns;
    if RenderColumns then
    begin
      Columns := CrackTree.Header.Columns.GetVisibleColumns;
      SetLength(ColumnColors, Length(Columns));
    end;

    CrackTree.GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    MaxLevel := 0;
    // The table consists of visible columns and rows as used in the CrackTree, but the main CrackTree column is splitted
    // into several HTML columns to accomodate the indentation.
    while Assigned(Run) and not CrackTree.OperationCanceled do
    begin
      if (CrackTree.CanExportNode(Run)) then
      begin
        Level := CrackTree.GetNodeLevel(Run);
        if Level > MaxLevel then
          MaxLevel := Level;
      end;
      Run := GetNextNode(Run);
    end;

    if RenderColumns then
    begin
      if Assigned(CrackTree.OnBeforeHeaderExport) then
        CrackTree.OnBeforeHeaderExport(CrackTree, etHTML);
      Buffer.Add('<tr class="header" style="');
      Buffer.Add(CellPadding);
      Buffer.Add('">');
      Buffer.AddNewLine;
      // Make the first row in the HTML table an image of the CrackTree header.
      for I := 0 to High(Columns) do
      begin
        if Assigned(CrackTree.OnBeforeColumnExport) then
          CrackTree.OnBeforeColumnExport(CrackTree, etHTML, Columns[I]);
        Buffer.Add('<th height="');
        Buffer.Add(IntToStr(CrackTree.Header.Height));
        Buffer.Add('px"');
        Alignment := Columns[I].CaptionAlignment;
        // Consider directionality.
        if Columns[I].BiDiMode <> bdLeftToRight then
        begin
          ChangeBidiModeAlignment(Alignment);
          Buffer.Add(' dir="rtl"');
        end;

          // Consider aligment.
        case Alignment of
          taRightJustify:
            Buffer.Add(' align=right');
          taCenter:
            Buffer.Add(' align=center');
        else
          Buffer.Add(' align=left');
        end;

        Index := Columns[I].Index;
        // Merge cells of the header emulation in the main column.
        if (MaxLevel > 0) and (Index = CrackTree.Header.MainColumn) then
        begin
          Buffer.Add(' colspan="');
          Buffer.Add(IntToStr(MaxLevel + 1));
          Buffer.Add('"');
        end;

        // The color of the header is usually clBtnFace.
        Buffer.Add(' bgcolor=');
        WriteColorAsHex(clBtnFace);

        // Set column width in pixels.
        Buffer.Add(' width="');
        Buffer.Add(IntToStr(Columns[I].Width));
        Buffer.Add('px">');

        if Length(Columns[I].Text) > 0 then
          Buffer.Add(Columns[I].Text);
        Buffer.Add('</th>');
        if Assigned(CrackTree.OnAfterColumnExport) then
          CrackTree.OnAfterColumnExport(CrackTree, etHTML, Columns[I]);
      end;
      Buffer.Add('</tr>');
      Buffer.AddNewLine;
      if Assigned(CrackTree.OnAfterHeaderExport) then
        CrackTree.OnAfterHeaderExport(CrackTree, etHTML);
    end;

    // Now go through the CrackTree.
    Run := Save;
    while Assigned(Run) and not CrackTree.OperationCanceled do
    begin
      if (not CrackTree.CanExportNode(Run)) then
      begin
        Run := GetNextNode(Run);
        Continue;
      end;
      if Assigned(CrackTree.OnBeforeNodeExport) then
        CrackTree.OnBeforeNodeExport(CrackTree, etHTML, Run);
      Level := CrackTree.GetNodeLevel(Run);
      Buffer.Add(' <tr class="default">');
      Buffer.AddNewLine;

      I := 0;
      while (I < Length(Columns)) or not RenderColumns do
      begin
        if RenderColumns then
          Index := Columns[I].Index
        else
          Index := NoColumn;

        if not RenderColumns or (coVisible in Columns[I].Options) then
        begin
          // Call back the application to know about font customization.
          CrackTree.Canvas.Font := CrackTree.Font;
          CrackTree.FFontChanged := False;
          CrackTree.DoPaintText(Run, CrackTree.Canvas, Index, ttNormal);

          if Index = CrackTree.Header.MainColumn then
          begin
            // Create a cell for each indentation level.
            if RenderColumns and not (coParentColor in Columns[I].Options) then
            begin
              for J := 1 to Level do
              begin
                Buffer.Add('<td class="noborder" width="');
                Buffer.Add(IndentWidth);
                Buffer.Add('" height="');
                Buffer.Add(IntToStr(CrackTree.NodeHeight[Run]));
                Buffer.Add('px"');
                if not (coParentColor in Columns[I].Options) then
                begin
                  Buffer.Add(' bgcolor=');
                  WriteColorAsHex(Columns[I].Color);
                end;
                Buffer.Add('>&nbsp;</td>');
              end;
            end
            else
            begin
              for J := 1 to Level do
                if J = 1 then
                begin
                  Buffer.Add(' <td height="');
                  Buffer.Add(IntToStr(CrackTree.NodeHeight[Run]));
                  Buffer.Add('px" class="normalborder">&nbsp;</td>');
                end
                else
                  Buffer.Add(' <td>&nbsp;</td>');
            end;
          end;

          if CrackTree.FFontChanged then
          begin
            Buffer.Add(' <td class="normalborder" ');
            WriteStyle('', CrackTree.Canvas.Font);
            Buffer.Add(' height="');
            Buffer.Add(IntToStr(CrackTree.NodeHeight[Run]));
            Buffer.Add('px"');
          end
          else
          begin
            Buffer.Add(' <td class="normalborder"  height="');
            Buffer.Add(IntToStr(CrackTree.NodeHeight[Run]));
            Buffer.Add('px"');
          end;

          if RenderColumns then
          begin
            Alignment := Columns[I].Alignment;
            BidiMode := Columns[I].BidiMode;
          end
          else
          begin
            Alignment := CrackTree.Alignment;
            BidiMode := CrackTree.BidiMode;
          end;
          // Consider directionality.
          if BiDiMode <> bdLeftToRight then
          begin
            ChangeBidiModeAlignment(Alignment);
            Buffer.Add(' dir="rtl"');
          end;

          // Consider aligment.
          case Alignment of
            taRightJustify:
              Buffer.Add(' align=right');
            taCenter:
              Buffer.Add(' align=center');
          else
            Buffer.Add(' align=left');
          end;
          // Merge cells in the main column.
          if (MaxLevel > 0) and (Index = CrackTree.Header.MainColumn) and (Level < MaxLevel) then
          begin
            Buffer.Add(' colspan="');
            Buffer.Add(IntToStr(MaxLevel - Level + 1));
            Buffer.Add('"');
          end;
          if RenderColumns and not (coParentColor in Columns[I].Options) then
          begin
            Buffer.Add(' bgcolor=');
            WriteColorAsHex(Columns[I].Color);
          end;
          Buffer.Add('>');
          // Get the text
          lGetCellTextEventArgs.Node := Run;
          lGetCellTextEventArgs.Column := Index;
          CrackTree.DoGetText(lGetCellTextEventArgs);
          Buffer.Add(lGetCellTextEventArgs.CellText);
          if not lGetCellTextEventArgs.StaticText.IsEmpty and (toShowStaticText in TStringTreeOptions(CrackTree.TreeOptions).StringOptions) then
            Buffer.Add(' ' + lGetCellTextEventArgs.StaticText);
          Buffer.Add('</td>');
        end;

        if not RenderColumns then
          Break;
        System.Inc(I);
      end;
      if Assigned(CrackTree.OnAfterNodeExport) then
        CrackTree.OnAfterNodeExport(CrackTree, etHTML, Run);
      Run := GetNextNode(Run);
      Buffer.Add(' </tr>');
      Buffer.AddNewLine;
    end;
    Buffer.Add('</table>');

    CrackTree.RestoreFontChangeEvent(CrackTree.Canvas);

    Result := Buffer.AsString;
  finally
    CrackTree.EndOperation(TVTOperationKind.okExport);
    Buffer.Free;
  end;
end;

function ContentToRTF(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType): RawByteString;

// Renders the current tree content (depending on Source) as RTF (rich text).
// Based on ideas and code from Frank van den Bergh and Andreas Hoerstemeier.

var
  Fonts: TStringList;
  Colors: TList<TColor>;
  CurrentFontIndex,
  CurrentFontColor,
  CurrentFontSize: Integer;
  Buffer: TBufferedRawByteString;

  //--------------- local functions -------------------------------------------

  procedure SelectFont(const Font: string);

  var
    I: Integer;

  begin
    I := Fonts.IndexOf(Font);
    if I > -1 then
    begin
      // Font has already been used
      if I <> CurrentFontIndex then
      begin
        Buffer.Add('\f');
        Buffer.Add(IntToStr(I));
        CurrentFontIndex := I;
      end;
    end
    else
    begin
      I := Fonts.Add(Font);
      Buffer.Add('\f');
      Buffer.Add(IntToStr(I));
      CurrentFontIndex := I;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure SelectColor(Color: TColor);

  var
    I: Integer;

  begin
    I := Colors.IndexOf(Color);
    if I > -1 then
    begin
      // Color has already been used
      if I <> CurrentFontColor then
      begin
        Buffer.Add('\cf');
        Buffer.Add(IntToStr(I + 1));
        CurrentFontColor := I;
      end;
    end
    else
    begin
      I := Colors.Add(Color);
      Buffer.Add('\cf');
      Buffer.Add(IntToStr(I + 1));
      CurrentFontColor := I;
    end;
  end;

  //---------------------------------------------------------------------------

  procedure TextPlusFont(const Text: string; Font: TFont);

  var
    UseUnderline,
    UseItalic,
    UseBold: Boolean;
    I: Integer;

  begin
    if Length(Text) > 0 then
    begin
      UseUnderline := TFontStyle.fsUnderline in Font.Style;
      if UseUnderline then
        Buffer.Add('\ul');
      UseItalic := TFontStyle.fsItalic in Font.Style;
      if UseItalic then
        Buffer.Add('\i');
      UseBold := TFontStyle.fsBold in Font.Style;
      if UseBold then
        Buffer.Add('\b');
      SelectFont(Font.Name);
      SelectColor(Font.Color);
      if Font.Size <> CurrentFontSize then
      begin
        // Font size must be given in half points.
        Buffer.Add('\fs');
        Buffer.Add(IntToStr(2 * Font.Size));
        CurrentFontSize := Font.Size;
      end;
      // Use escape sequences to note Unicode text.
      Buffer.Add(' ');
      // Note: Unicode values > 32767 must be expressed as negative numbers. This is implicitly done
      //       by interpreting the wide chars (word values) as small integers.
      for I := 1 to Length(Text) do
      begin
        if (Text[I] = WideLF) then
          Buffer.Add( '{\par}' )
        else
          if (Text[I] <> WideCR) then
          begin
            Buffer.Add(Format('\u%d\''3f', [SmallInt(Text[I])]));
            Continue;
          end;
      end;
      if UseUnderline then
        Buffer.Add('\ul0');
      if UseItalic then
        Buffer.Add('\i0');
      if UseBold then
        Buffer.Add('\b0');
    end;
  end;

  //--------------- end local functions ---------------------------------------

var
  Level, LastLevel: Integer;
  I, J: Integer;
  Save, Run: PVirtualNode;
  GetNextNode: TGetNextNodeProc;
  S, Tabs : RawByteString;
  Twips: Integer;

  RenderColumns: Boolean;
  Columns: TColumnsArray;
  Index: Integer;
  Alignment: TAlignment;
  BidiMode: TBidiMode;
  LocaleBuffer: array [0..1] of Char;
  CrackTree: TCustomVirtualStringTreeCracker;
  lGetCellTextEventArgs: TVSTGetCellTextEventArgs;
begin
  CrackTree := TCustomVirtualStringTreeCracker(Tree);

  Buffer := TBufferedRawByteString.Create;
  lGetCellTextEventArgs.ExportType := TVTExportType.etRtf;
  CrackTree.StartOperation(TVTOperationKind.okExport);
  try
    // For customization by the application or descendants we use again the redirected font change event.
    CrackTree.RedirectFontChangeEvent(CrackTree.Canvas);

    Fonts := TStringList.Create;
    Colors := TList<TColor>.Create;
    CurrentFontIndex := -1;
    CurrentFontColor := -1;
    CurrentFontSize := -1;

    Columns := nil;
    Tabs := '';
    LastLevel := 0;

    RenderColumns := CrackTree.Header.UseColumns;
    if RenderColumns then
      Columns := CrackTree.Header.Columns.GetVisibleColumns;

    CrackTree.GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    // First make a table structure. The \rtf and other header stuff is included
    // when the font and color tables are created.
    Buffer.Add('\uc1\trowd\trgaph70');
    J := 0;
    if RenderColumns then
    begin
      for I := 0 to High(Columns) do
      begin
        System.Inc(J, Columns[I].Width);
        // This value must be expressed in twips (1 inch = 1440 twips).
        Twips := Round(1440 * J / Screen.PixelsPerInch);
        Buffer.Add('\cellx');
        Buffer.Add(IntToStr(Twips));
      end;
    end
    else
    begin
      Twips := Round(1440 * CrackTree.ClientWidth / Screen.PixelsPerInch);
      Buffer.Add('\cellx');
      Buffer.Add(IntToStr(Twips));
    end;

    // Fill table header.
    if RenderColumns then
    begin
      if Assigned(CrackTree.OnBeforeHeaderExport) then
        CrackTree.OnBeforeHeaderExport(CrackTree, etRTF);
      Buffer.Add('\pard\intbl');
      for I := 0 to High(Columns) do
      begin
        if Assigned(CrackTree.OnBeforeColumnExport) then
          CrackTree.OnBeforeColumnExport(CrackTree, etRTF, Columns[I]);
        Alignment := Columns[I].CaptionAlignment;
        BidiMode := Columns[I].BidiMode;

        // Alignment is not supported with older RTF formats, however it will be ignored.
        if BidiMode <> bdLeftToRight then
          ChangeBidiModeAlignment(Alignment);
        case Alignment of
          taLeftJustify:
            Buffer.Add('\ql');
          taRightJustify:
            Buffer.Add('\qr');
          taCenter:
            Buffer.Add('\qc');
        end;

        TextPlusFont(Columns[I].Text, CrackTree.Header.Font);
        Buffer.Add('\cell');
        if Assigned(CrackTree.OnAfterColumnExport) then
          CrackTree.OnAfterColumnExport(CrackTree, etRTF, Columns[I]);
      end;
      Buffer.Add('\row');
      if Assigned(CrackTree.OnAfterHeaderExport) then
        CrackTree.OnAfterHeaderExport(CrackTree, etRTF);
    end;

    // Now write the contents.
    Run := Save;
    while Assigned(Run) and not CrackTree.OperationCanceled do
    begin
      if (not CrackTree.CanExportNode(Run)) then
      begin
        Run := GetNextNode(Run);
        Continue;
      end;
      if Assigned(CrackTree.OnBeforeNodeExport) then
        CrackTree.OnBeforeNodeExport(CrackTree, etRTF, Run);
      I := 0;
      while not RenderColumns or (I < Length(Columns)) do
      begin
        if RenderColumns then
        begin
          Index := Columns[I].Index;
          Alignment := Columns[I].Alignment;
          BidiMode := Columns[I].BidiMode;
        end
        else
        begin
          Index := NoColumn;
          Alignment := CrackTree.Alignment;
          BidiMode := CrackTree.BidiMode;
        end;

        if not RenderColumns or (coVisible in Columns[I].Options) then
        begin
          // Get the text
          lGetCellTextEventArgs.Node := Run;
          lGetCellTextEventArgs.Column := Index;
          CrackTree.DoGetText(lGetCellTextEventArgs);
          Buffer.Add('\pard\intbl');

          // Alignment is not supported with older RTF formats, however it will be ignored.
          if BidiMode <> bdLeftToRight then
            ChangeBidiModeAlignment(Alignment);
          case Alignment of
            taRightJustify:
              Buffer.Add('\qr');
            taCenter:
              Buffer.Add('\qc');
          end;

          // Call back the application to know about font customization.
          CrackTree.Canvas.Font.Assign(CrackTree.Font);
          CrackTree.FFontChanged := False;
          CrackTree.DoPaintText(Run, CrackTree.Canvas, Index, ttNormal);

          if Index = CrackTree.Header.MainColumn then
          begin
            Level := CrackTree.GetNodeLevel(Run);
            if Level <> LastLevel then
            begin
              LastLevel := Level;
              Tabs := '';
              for J := 0 to Level - 1 do
                Tabs := Tabs + '\tab';
            end;
            if Level > 0 then
            begin
              Buffer.Add(Tabs);
              Buffer.Add(' ');
              TextPlusFont(lGetCellTextEventArgs.CellText, CrackTree.Canvas.Font);
            end
            else
            begin
              TextPlusFont(lGetCellTextEventArgs.CellText, CrackTree.Canvas.Font);
            end;
          end
          else
          begin
            TextPlusFont(lGetCellTextEventArgs.CellText, CrackTree.Canvas.Font);
          end;
          if not lGetCellTextEventArgs.StaticText.IsEmpty and (toShowStaticText in TStringTreeOptions(CrackTree.TreeOptions).StringOptions) then
          begin
            CrackTree.DoPaintText(Run, CrackTree.Canvas, Index, ttStatic);
            TextPlusFont(' ' + lGetCellTextEventArgs.StaticText, CrackTree.Canvas.Font);
          end;//if static text
          Buffer.Add('\cell');
        end;

        if not RenderColumns then
          Break;
        System.Inc(I);
      end;
      Buffer.Add('\row');
      Buffer.AddNewLine;
      if (Assigned(CrackTree.OnAfterNodeExport)) then
        CrackTree.OnAfterNodeExport(CrackTree, etRTF, Run);
      Run := GetNextNode(Run);
    end;

    Buffer.Add('\pard\par');

    // Build lists with fonts and colors. They have to be at the start of the document.
    S := '{\rtf1\ansi\ansicpg1252\deff0\deflang1043{\fonttbl';
    for I := 0 to Fonts.Count - 1 do
      S := S + Format('{\f%d %s;}', [I, Fonts[I]]);
    S := S + '}';

    S := S + '{\colortbl;';
    for I := 0 to Colors.Count - 1 do
    begin
      J := ColorToRGB(TColor(Colors[I]));
      S := S + Format('\red%d\green%d\blue%d;', [J and $FF, (J shr 8) and $FF, (J shr 16) and $FF]);
    end;
    S := S + '}';
    if (GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IMEASURE, @LocaleBuffer[0], Length(LocaleBuffer)) <> 0) and (LocaleBuffer[0] = '0'{metric}) then
      S := S + '\paperw16840\paperh11907'// This sets A4 landscape format
    else
      S := S + '\paperw15840\paperh12240';//[JAM:marder]  This sets US Letter landscape format
    // Make sure a small margin is used so that a lot of the table fits on a paper. This defines a margin of 0.5"
    S := S + '\margl720\margr720\margt720\margb720';
    Result := S + Buffer.AsString + '}';
    Fonts.Free;
    Colors.Free;

    CrackTree.RestoreFontChangeEvent(CrackTree.Canvas);
  finally
    CrackTree.EndOperation(TVTOperationKind.okExport);
    Buffer.Free;
  end;
end;


function ContentToUnicodeString(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType; const Separator: string): string;

// Renders the current tree content (depending on Source) as Unicode text.
// If an entry contains the separator char then it is wrapped with double quotation marks.
var
  Buffer: TBufferedString;

  procedure CheckQuotingAndAppend(const pText: string);
  begin
    // Wrap the text with quotation marks if it contains the separator character.
    if Pos(Separator, pText) > 0 then
      Buffer.Add(AnsiQuotedStr(pText, '"'))
    else
      Buffer.Add(pText);
  end;

var
  RenderColumns: Boolean;
  Tabs: string;
  GetNextNode: TGetNextNodeProc;
  Run, Save: PVirtualNode;

  Columns: TColumnsArray;
  LastColumn: TVirtualTreeColumn;
  Level, MaxLevel: Cardinal;
  Index,
  I: Integer;
  CrackTree: TCustomVirtualStringTreeCracker;
  lGetCellTextEventArgs: TVSTGetCellTextEventArgs;
begin
  CrackTree := TCustomVirtualStringTreeCracker(Tree);

  Buffer := TBufferedString.Create;
  lGetCellTextEventArgs.ExportType := TVTExportType.etText;
  CrackTree.StartOperation(TVTOperationKind.okExport);
  try
    Columns := nil;
    RenderColumns := CrackTree.Header.UseColumns;
    if RenderColumns then
      Columns := CrackTree.Header.Columns.GetVisibleColumns;

    CrackTree.GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    // The text consists of visible groups representing the columns, which are separated by one or more separator
    // characters. There are always MaxLevel separator chars in a line (main column only). Either before the caption
    // to ident it or after the caption to make the following column aligned.
    MaxLevel := 0;
    while Assigned(Run) and not CrackTree.OperationCanceled do
    begin
      Level := CrackTree.GetNodeLevel(Run);
      if Level > MaxLevel then
        MaxLevel := Level;
      Run := GetNextNode(Run);
    end;

    Tabs := DupeString(Separator, MaxLevel);

    // First line is always the header if used.
    if RenderColumns then
    begin
      LastColumn := Columns[High(Columns)];
      for I := 0 to High(Columns) do
      begin
        Buffer.Add(Columns[I].Text);
        if Columns[I] <> LastColumn then
        begin
          if Columns[I].Index = CrackTree.Header.MainColumn then
          begin
            Buffer.Add(Tabs);
            Buffer.Add(Separator);
          end
          else
            Buffer.Add(Separator);
        end;
      end;
      Buffer.AddNewLine;
    end
    else
      LastColumn := nil;

    Run := Save;
    if RenderColumns then
    begin
      while Assigned(Run)  and not CrackTree.OperationCanceled do
      begin
        for I := 0 to High(Columns) do
        begin
          if coVisible in Columns[I].Options then
          begin
            Index := Columns[I].Index;
            lGetCellTextEventArgs.Node := Run;
            lGetCellTextEventArgs.Column := Index;
            CrackTree.DoGetText(lGetCellTextEventArgs);
            if Index = CrackTree.Header.MainColumn then
              Buffer.Add(Copy(Tabs, 1, Integer(CrackTree.GetNodeLevel(Run)) * Length(Separator)));
            if not lGetCellTextEventArgs.StaticText.IsEmpty and (toShowStaticText in TStringTreeOptions(CrackTree.TreeOptions).StringOptions) then
              CheckQuotingAndAppend(lGetCellTextEventArgs.CellText + ' ' + lGetCellTextEventArgs.StaticText)
            else
              CheckQuotingAndAppend(lGetCellTextEventArgs.CellText);
            if Index = CrackTree.Header.MainColumn then
              Buffer.Add(Copy(Tabs, 1, Integer(MaxLevel - CrackTree.GetNodeLevel(Run)) * Length(Separator)));

            if Columns[I] <> LastColumn then
              Buffer.Add(Separator);
          end;
        end;
        Run := GetNextNode(Run);
        Buffer.AddNewLine;
      end;
    end
    else
    begin
      lGetCellTextEventArgs.Column := NoColumn;
      while Assigned(Run) and not CrackTree.OperationCanceled do
      begin
        lGetCellTextEventArgs.Node := Run;
        CrackTree.DoGetText(lGetCellTextEventArgs);
        Level := CrackTree.GetNodeLevel(Run);
        Buffer.Add(Copy(Tabs, 1, Integer(Level) * Length(Separator)));
        Buffer.Add(lGetCellTextEventArgs.CellText);
        Buffer.AddNewLine;

        Run := GetNextNode(Run);
      end;
    end;
    Result := Buffer.AsString;
  finally
    CrackTree.EndOperation(TVTOperationKind.okExport);
    Buffer.Free;
  end;
end;

function ContentToClipboard(Tree: TCustomVirtualStringTree; Format: Word; Source: TVSTTextSourceType): HGLOBAL;

// This method constructs a shareable memory object filled with string data in the required format. Supported are:
// CF_TEXT - plain ANSI text (Unicode text is converted using the user's current locale)
// CF_UNICODETEXT - plain Unicode text
// CF_CSV - comma separated plain ANSI text
// CF_VRTF + CF_RTFNOOBS - rich text (plain ANSI)
// CF_HTML - HTML text encoded using UTF-8
//
// Result is the handle to a globally allocated memory block which can directly be used for clipboard and drag'n drop
// transfers. The caller is responsible for freeing the memory. If for some reason the content could not be rendered
// the Result is 0.

  //--------------- local function --------------------------------------------

  procedure MakeFragment(var HTML: Utf8String);

  // Helper routine to build a properly-formatted HTML fragment.

  const
    Version = 'Version:1.0'#13#10;
    StartHTML = 'StartHTML:';
    EndHTML = 'EndHTML:';
    StartFragment = 'StartFragment:';
    EndFragment = 'EndFragment:';
    DocType = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">';
    HTMLIntro = '<html><head><META http-equiv=Content-Type content="text/html; charset=utf-8">' +
      '</head><body><!--StartFragment-->';
    HTMLExtro = '<!--EndFragment--></body></html>';
    NumberLengthAndCR = 10;

    // Let the compiler determine the description length.
    DescriptionLength = Length(Version) + Length(StartHTML) + Length(EndHTML) + Length(StartFragment) +
      Length(EndFragment) + 4 * NumberLengthAndCR;

  var
    Description: Utf8String;
    StartHTMLIndex,
    EndHTMLIndex,
    StartFragmentIndex,
    EndFragmentIndex: Integer;

  begin
    // The HTML clipboard format is defined by using byte positions in the entire block where HTML text and
    // fragments start and end. These positions are written in a description. Unfortunately the positions depend on the
    // length of the description but the description may change with varying positions.
    // To solve this dilemma the offsets are converted into fixed length strings which makes it possible to know
    // the description length in advance.
    StartHTMLIndex := DescriptionLength;              // position 0 after the description
    StartFragmentIndex := StartHTMLIndex + Length(DocType) + Length(HTMLIntro);
    EndFragmentIndex := StartFragmentIndex + Length(HTML);
    EndHTMLIndex := EndFragmentIndex + Length(HTMLExtro);

    Description := Version +
    System.SysUtils.Format('%s%.8d', [StartHTML, StartHTMLIndex]) + #13#10 +
    System.SysUtils.Format('%s%.8d', [EndHTML, EndHTMLIndex]) + #13#10 +
    System.SysUtils.Format('%s%.8d', [StartFragment, StartFragmentIndex]) + #13#10 +
    System.SysUtils.Format('%s%.8d', [EndFragment, EndFragmentIndex]) + #13#10;
    HTML := Description + DocType + HTMLIntro + HTML + HTMLExtro;
  end;

  //--------------- end local function ----------------------------------------

var
  Data: Pointer;
  DataSize: Cardinal;
  S: AnsiString;
  WS: string;
  lUtf8String: Utf8string;
  P: Pointer;
  CrackTree: TCustomVirtualStringTreeCracker;
begin
  CrackTree := TCustomVirtualStringTreeCracker(Tree);

  Result := 0;
  DataSize := 0;
  Data := nil;
  case Format of
    CF_TEXT:
      begin
        S := AnsiString(ContentToUnicodeString(CrackTree, Source, #9) + #0);
        Data := PAnsiChar(S);
        DataSize := Length(S);
      end;
    CF_UNICODETEXT:
      begin
        WS := ContentToUnicodeString(CrackTree, Source, #9) + #0;
        Data := PWideChar(WS);
        DataSize := 2 * Length(WS);
      end;
  else
    if Format = CF_CSV then
    begin
      S := AnsiString(ContentToUnicodeString(CrackTree, Source, FormatSettings.ListSeparator) + #0);
      Data := PAnsiChar(S);
      DataSize := Length(S);
    end// CF_CSV
    else if (Format = CF_VRTF) or (Format = CF_VRTFNOOBJS) then
    begin
      S := ContentToRTF(CrackTree, Source) + #0;
      Data := PAnsiChar(S);
      DataSize := Length(S);
    end
    else if Format = CF_HTML then
    begin
      lUtf8String := ContentToHTML(CrackTree, Source);
      // Build a valid HTML clipboard fragment.
      MakeFragment(lUtf8String);
      lUtf8String := lUtf8String + #0;
      Data := PAnsiChar(lUtf8String);
      DataSize := Length(lUtf8String);
    end;
  end;

  if DataSize > 0 then
  begin
    Result := GlobalAlloc(GHND or GMEM_SHARE, DataSize);
    P := GlobalLock(Result);
    Move(Data^, P^, DataSize);
    GlobalUnlock(Result);
  end;
end;

procedure ContentToCustom(Tree: TCustomVirtualStringTree; Source: TVSTTextSourceType);

// Generic export procedure which polls the application at every stage of the export.

var
  I: Integer;
  Save, Run: PVirtualNode;
  GetNextNode: TGetNextNodeProc;
  RenderColumns: Boolean;
  Columns: TColumnsArray;
  CrackTree: TCustomVirtualStringTreeCracker;
begin
  CrackTree := TCustomVirtualStringTreeCracker(Tree);

  CrackTree.StartOperation(TVTOperationKind.okExport);
  try
    Columns := nil;
    CrackTree.GetRenderStartValues(Source, Run, GetNextNode);
    Save := Run;

    RenderColumns := CrackTree.Header.UseColumns and ( hoVisible in CrackTree.Header.Options );

    if Assigned(CrackTree.OnBeforeTreeExport) then
      CrackTree.OnBeforeTreeExport(CrackTree, etCustom);

    // Fill table header.
    if RenderColumns then
    begin
      if Assigned(CrackTree.OnBeforeHeaderExport) then
        CrackTree.OnBeforeHeaderExport(CrackTree, etCustom);

      Columns := CrackTree.Header.Columns.GetVisibleColumns;
      for I := 0 to High(Columns) do
      begin
        if Assigned(CrackTree.OnBeforeColumnExport) then
          CrackTree.OnBeforeColumnExport(CrackTree, etCustom, Columns[I]);

        if Assigned(CrackTree.OnColumnExport) then
          CrackTree.OnColumnExport(CrackTree, etCustom, Columns[I]);

        if Assigned(CrackTree.OnAfterColumnExport) then
          CrackTree.OnAfterColumnExport(CrackTree, etCustom, Columns[I]);
      end;

      if Assigned(CrackTree.OnAfterHeaderExport) then
        CrackTree.OnAfterHeaderExport(CrackTree, etCustom);
    end;

    // Now write the content.
    Run := Save;
    while Assigned(Run) and not CrackTree.OperationCanceled do
    begin
      if CrackTree.CanExportNode(Run) then
      begin
        if Assigned(CrackTree.OnBeforeNodeExport) then
          CrackTree.OnBeforeNodeExport(CrackTree, etCustom, Run);

        if Assigned(CrackTree.OnNodeExport) then
          CrackTree.OnNodeExport(CrackTree, etCustom, Run);

        if Assigned(CrackTree.OnAfterNodeExport) then
          CrackTree.OnAfterNodeExport(CrackTree, etCustom, Run);
      end;

      Run := GetNextNode(Run);
    end;

    if Assigned(CrackTree.OnAfterTreeExport) then
      CrackTree.OnAfterTreeExport(CrackTree, etCustom);
  finally
    CrackTree.EndOperation(TVTOperationKind.okExport);
  end;
end;

end.
