{==============================================================================|
| Project : Ararat Synapse                                       | 002.009.000 |
|==============================================================================|
| Content: MIME support procedures and functions                               |
|==============================================================================|
| Copyright (c)1999-200812                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2000-2012.                |
| Portions created by Petr Fejfar are Copyright (c)2011-2012.                  |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(MIME part handling)
Handling with MIME parts.

Used RFC: RFC-2045
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$Q-}
{$R-}
{$M+}

{$IFDEF UNICODE}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$ENDIF}

unit mimepart;

interface

uses
  SysUtils, Classes,
  synafpc,
  synachar, synacode, synautil, mimeinln;

type

  TMimePart = class;

  {:@abstract(Procedural type for @link(TMimepart.Walkpart) hook). This hook is used for
   easy walking through MIME subparts.}
  THookWalkPart = procedure(const Sender: TMimePart) of object;

  {:The four types of MIME parts. (textual, multipart, message or any other
   binary data.)}
  TMimePrimary = (MP_TEXT, MP_MULTIPART, MP_MESSAGE, MP_BINARY);

  {:The various types of possible part encodings.}
  TMimeEncoding = (ME_7BIT, ME_8BIT, ME_QUOTED_PRINTABLE,
    ME_BASE64, ME_UU, ME_XX);

  {:@abstract(Object for working with parts of MIME e-mail.)
   Each TMimePart object can handle any number of nested subparts as new
   TMimepart objects. It can handle any tree hierarchy structure of nested MIME
   subparts itself.

   Basic tasks are:

   Decoding of MIME message:
   - store message into Lines property
   - call DecomposeParts. Now you have decomposed MIME parts in all nested levels!
   - now you can explore all properties and subparts. (You can use WalkPart method)
   - if you need decode part, call DecodePart.

   Encoding of MIME message:

   - if you need multipart message, you must create subpart by AddSubPart.
   - set all properties of all parts.
   - set content of part into DecodedLines stream
   - encode this stream by EncodePart.
   - compose full message by ComposeParts. (it build full MIME message from all subparts. Do not call this method for each subpart! It is needed on root part!)
   - encoded MIME message is stored in Lines property.
  }
  TMimePart = class(TObject)
  private
    FPrimary: string;
    FPrimaryCode: TMimePrimary;
    FSecondary: string;
    FEncoding: string;
    FEncodingCode: TMimeEncoding;
    FDefaultCharset: string;
    FCharset: string;
    FCharsetCode: TMimeChar;
    FTargetCharset: TMimeChar;
    FDescription: string;
    FDisposition: string;
    FContentID: string;
    FBoundary: string;
    FFileName: string;
    FLines: TStringList;
    FPartBody: TStringList;
    FHeaders: TStringList;
    FPrePart: TStringList;
    FPostPart: TStringList;
    FDecodedLines: TMemoryStream;
    FSubParts: TList;
    FOnWalkPart: THookWalkPart;
    FMaxLineLength: integer;
    FSubLevel: integer;
    FMaxSubLevel: integer;
    FAttachInside: boolean;
    FConvertCharset: Boolean;
    FForcedHTMLConvert: Boolean;
    FBinaryDecomposer: boolean;
    procedure SetPrimary(Value: string);
    procedure SetEncoding(Value: string);
    procedure SetCharset(Value: string);
    function IsUUcode(Value: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    {:Assign content of another object to this object. (Only this part,
     not subparts!)}
    procedure Assign(Value: TMimePart);

    {:Assign content of another object to this object. (With all subparts!)}
    procedure AssignSubParts(Value: TMimePart);

    {:Clear all data values to default values. It also call @link(ClearSubparts).}
    procedure Clear;

    {:Decode Mime part from @link(Lines) to @link(DecodedLines).}
    procedure DecodePart;

    {:Parse header lines from Headers property into another properties.}
    procedure DecodePartHeader;

    {:Encode mime part from @link(DecodedLines) to @link(Lines) and build mime
     headers.}
    procedure EncodePart;

    {:Build header lines in Headers property from another properties.}
    procedure EncodePartHeader;

    {:generate primary and secondary mime type from filename extension in value.
     If type not recognised, it return 'Application/octet-string' type.}
    procedure MimeTypeFromExt(Value: string);

    {:Return number of decomposed subparts. (On this level! Each of this
     subparts can hold any number of their own nested subparts!)}
    function GetSubPartCount: integer;

    {:Get nested subpart object as new TMimePart. For getting maximum possible
     index you can use @link(GetSubPartCount) method.}
    function GetSubPart(index: integer): TMimePart;

    {:delete subpart on given index.}
    procedure DeleteSubPart(index: integer);

    {:Clear and destroy all subpart TMimePart objects.}
    procedure ClearSubParts;

    {:Add and create new subpart.}
    function AddSubPart: TMimePart;

    {:E-mail message in @link(Lines) property is parsed into this object.
     E-mail headers are stored in @link(Headers) property and is parsed into
     another properties automaticly. Not need call @link(DecodePartHeader)!
     Content of message (part) is stored into @link(PartBody) property. This
     part is in undecoded form! If you need decode it, then you must call
     @link(DecodePart) method by your hands. Lot of another properties is filled
     also.

     Decoding of parts you must call separately due performance reasons. (Not
     needed to decode all parts in all reasons.)

     For each MIME subpart is created new TMimepart object (accessible via
     method @link(GetSubPart)).}
    procedure DecomposeParts;

    {pf}
    {: HTTP message is received by @link(THTTPSend) component in two parts:
     headers are stored in @link(THTTPSend.Headers) and a body in memory stream
     @link(THTTPSend.Document).

     On the top of it, HTTP connections are always 8-bit, hence data are
     transferred in native format i.e. no transfer encoding is applied.

     This method operates the similiar way and produces the same
     result as @link(DecomposeParts).
    }
    procedure DecomposePartsBinary(AHeader:TStrings; AStx,AEtx:PANSIChar);
    {/pf}

    {:This part and all subparts is composed into one MIME message stored in
     @link(Lines) property.}
    procedure ComposeParts;

    {:By calling this method is called @link(OnWalkPart) event for each part
     and their subparts. It is very good for calling some code for each part in
     MIME message}
    procedure WalkPart;

    {:Return @true when is possible create next subpart. (@link(maxSublevel)
     is still not reached)}
    function CanSubPart: boolean;
  published
    {:Primary Mime type of part. (i.e. 'application') Writing to this property
     automaticly generate value of @link(PrimaryCode).}
    property Primary: string read FPrimary write SetPrimary;

    {:String representation of used Mime encoding in part. (i.e. 'base64')
     Writing to this property automaticly generate value of @link(EncodingCode).}
    property Encoding: string read FEncoding write SetEncoding;

    {:String representation of used Mime charset in part. (i.e. 'iso-8859-1')
     Writing to this property automaticly generate value of @link(CharsetCode).
     Charset is used only for text parts.}
    property Charset: string read FCharset write SetCharset;

    {:Define default charset for decoding text MIME parts without charset
     specification. Default value is 'ISO-8859-1' by RCF documents.
     But Microsoft Outlook use windows codings as default. This property allows
     properly decode textual parts from some broken versions of Microsoft
     Outlook. (this is bad software!)}
    property DefaultCharset: string read FDefaultCharset write FDefaultCharset;

    {:Decoded primary type. Possible values are: MP_TEXT, MP_MULTIPART,
     MP_MESSAGE and MP_BINARY. If type not recognised, result is MP_BINARY.}
    property PrimaryCode: TMimePrimary read FPrimaryCode Write FPrimaryCode;

    {:Decoded encoding type. Possible values are: ME_7BIT, ME_8BIT,
     ME_QUOTED_PRINTABLE and ME_BASE64. If type not recognised, result is
     ME_7BIT.}
    property EncodingCode: TMimeEncoding read FEncodingCode Write FEncodingCode;

    {:Decoded charset type. Possible values are defined in @link(SynaChar) unit.}
    property CharsetCode: TMimeChar read FCharsetCode Write FCharsetCode;

    {:System charset type. Default value is charset used by default in your
     operating system.}
    property TargetCharset: TMimeChar read FTargetCharset Write FTargetCharset;

    {:If @true, then do internal charset translation of part content between @link(CharsetCode)
     and @link(TargetCharset)}
    property ConvertCharset: Boolean read FConvertCharset Write FConvertCharset;

    {:If @true, then allways do internal charset translation of HTML parts
     by MIME even it have their own charset in META tag. Default is @false.}
    property ForcedHTMLConvert: Boolean read FForcedHTMLConvert Write FForcedHTMLConvert;

    {:Secondary Mime type of part. (i.e. 'mixed')}
    property Secondary: string read FSecondary Write FSecondary;

    {:Description of Mime part.}
    property Description: string read FDescription Write FDescription;

    {:Value of content disposition field. (i.e. 'inline' or 'attachment')}
    property Disposition: string read FDisposition Write FDisposition;

    {:Content ID.}
    property ContentID: string read FContentID Write FContentID;

    {:Boundary delimiter of multipart Mime part. Used only in multipart part.}
    property Boundary: string read FBoundary Write FBoundary;

    {:Filename of file in binary part.}
    property FileName: string read FFileName Write FFileName;

    {:String list with lines contains mime part (It can be a full message).}
    property Lines: TStringList read FLines;

    {:Encoded form of MIME part data.}
    property PartBody: TStringList read FPartBody;

    {:All header lines of MIME part.}
    property Headers: TStringList read FHeaders;

    {:On multipart this contains part of message between first line of message
     and first boundary.}
    property PrePart: TStringList read FPrePart;

    {:On multipart this contains part of message between last boundary and end
     of message.}
    property PostPart: TStringList read FPostPart;

    {:Stream with decoded form of budy part.}
    property DecodedLines: TMemoryStream read FDecodedLines;

    {:Show nested level in subpart tree. Value 0 means root part. 1 means
     subpart from this root. etc.}
    property SubLevel: integer read FSubLevel write FSubLevel;

    {:Specify maximum sublevel value for decomposing.}
    property MaxSubLevel: integer read FMaxSubLevel write FMaxSubLevel;

    {:When is @true, then this part maybe(!) have included some uuencoded binary
    data.}
    property AttachInside: boolean read FAttachInside;

    {:Here you can assign hook procedure for walking through all part and their
     subparts.}
    property OnWalkPart: THookWalkPart read FOnWalkPart write FOnWalkPart;

    {:Here you can specify maximum line length for encoding of MIME part.
     If line is longer, then is splitted by standard of MIME. Correct MIME
     mailers can de-split this line into original length.}
    property MaxLineLength: integer read FMaxLineLength Write FMaxLineLength;
  end;

const
  MaxMimeType = 25;
  MimeType: array[0..MaxMimeType, 0..2] of string =
  (
    ('AU', 'audio', 'basic'),
    ('AVI', 'video', 'x-msvideo'),
    ('BMP', 'image', 'BMP'),
    ('DOC', 'application', 'MSWord'),
    ('EPS', 'application', 'Postscript'),
    ('GIF', 'image', 'GIF'),
    ('JPEG', 'image', 'JPEG'),
    ('JPG', 'image', 'JPEG'),
    ('MID', 'audio', 'midi'),
    ('MOV', 'video', 'quicktime'),
    ('MPEG', 'video', 'MPEG'),
    ('MPG', 'video', 'MPEG'),
    ('MP2', 'audio', 'mpeg'),
    ('MP3', 'audio', 'mpeg'),
    ('PDF', 'application', 'PDF'),
    ('PNG', 'image', 'PNG'),
    ('PS', 'application', 'Postscript'),
    ('QT', 'video', 'quicktime'),
    ('RA', 'audio', 'x-realaudio'),
    ('RTF', 'application', 'RTF'),
    ('SND', 'audio', 'basic'),
    ('TIF', 'image', 'TIFF'),
    ('TIFF', 'image', 'TIFF'),
    ('WAV', 'audio', 'x-wav'),
    ('WPD', 'application', 'Wordperfect5.1'),
    ('ZIP', 'application', 'ZIP')
    );

{:Generates a unique boundary string.}
function GenerateBoundary: string;

implementation

{==============================================================================}

constructor TMIMEPart.Create;
begin
  inherited Create;
  FOnWalkPart := nil;
  FLines := TStringList.Create;
  FPartBody := TStringList.Create;
  FHeaders := TStringList.Create;
  FPrePart := TStringList.Create;
  FPostPart := TStringList.Create;
  FDecodedLines := TMemoryStream.Create;
  FSubParts := TList.Create;
  FTargetCharset := GetCurCP;
  //was 'US-ASCII' before, but RFC-ignorant Outlook sometimes using default
  //system charset instead.
  FDefaultCharset := GetIDFromCP(GetCurCP);
  FMaxLineLength := 78;
  FSubLevel := 0;
  FMaxSubLevel := -1;
  FAttachInside := false;
  FConvertCharset := true;
  FForcedHTMLConvert := false;
end;

destructor TMIMEPart.Destroy;
begin
  ClearSubParts;
  FSubParts.Free;
  FDecodedLines.Free;
  FPartBody.Free;
  FLines.Free;
  FHeaders.Free;
  FPrePart.Free;
  FPostPart.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMIMEPart.Clear;
begin
  FPrimary := '';
  FEncoding := '';
  FCharset := '';
  FPrimaryCode := MP_TEXT;
  FEncodingCode := ME_7BIT;
  FCharsetCode := ISO_8859_1;
  FTargetCharset := GetCurCP;
  FSecondary := '';
  FDisposition := '';
  FContentID := '';
  FDescription := '';
  FBoundary := '';
  FFileName := '';
  FAttachInside := False;
  FPartBody.Clear;
  FHeaders.Clear;
  FPrePart.Clear;
  FPostPart.Clear;
  FDecodedLines.Clear;
  FConvertCharset := true;
  FForcedHTMLConvert := false;
  ClearSubParts;
end;

{==============================================================================}

procedure TMIMEPart.Assign(Value: TMimePart);
begin
  Primary := Value.Primary;
  Encoding := Value.Encoding;
  Charset := Value.Charset;
  DefaultCharset := Value.DefaultCharset;
  PrimaryCode := Value.PrimaryCode;
  EncodingCode := Value.EncodingCode;
  CharsetCode := Value.CharsetCode;
  TargetCharset := Value.TargetCharset;
  Secondary := Value.Secondary;
  Description := Value.Description;
  Disposition := Value.Disposition;
  ContentID := Value.ContentID;
  Boundary := Value.Boundary;
  FileName := Value.FileName;
  Lines.Assign(Value.Lines);
  PartBody.Assign(Value.PartBody);
  Headers.Assign(Value.Headers);
  PrePart.Assign(Value.PrePart);
  PostPart.Assign(Value.PostPart);
  MaxLineLength := Value.MaxLineLength;
  FAttachInside := Value.AttachInside;
  FConvertCharset := Value.ConvertCharset;
end;

{==============================================================================}

procedure TMIMEPart.AssignSubParts(Value: TMimePart);
var
  n: integer;
  p: TMimePart;
begin
  Assign(Value);
  for n := 0 to Value.GetSubPartCount - 1 do
  begin
    p := AddSubPart;
    p.AssignSubParts(Value.GetSubPart(n));
  end;
end;

{==============================================================================}

function TMIMEPart.GetSubPartCount: integer;
begin
  Result :=  FSubParts.Count;
end;

{==============================================================================}

function TMIMEPart.GetSubPart(index: integer): TMimePart;
begin
  Result := nil;
  if Index < GetSubPartCount then
    Result := TMimePart(FSubParts[Index]);
end;

{==============================================================================}

procedure TMIMEPart.DeleteSubPart(index: integer);
begin
  if Index < GetSubPartCount then
  begin
    GetSubPart(Index).Free;
    FSubParts.Delete(Index);
  end;
end;

{==============================================================================}

procedure TMIMEPart.ClearSubParts;
var
  n: integer;
begin
  for n := 0 to GetSubPartCount - 1 do
    TMimePart(FSubParts[n]).Free;
  FSubParts.Clear;
end;

{==============================================================================}

function TMIMEPart.AddSubPart: TMimePart;
begin
  Result := TMimePart.Create;
  Result.DefaultCharset := FDefaultCharset;
  FSubParts.Add(Result);
  Result.SubLevel := FSubLevel + 1;
  Result.MaxSubLevel := FMaxSubLevel;  
end;

{==============================================================================}

procedure TMIMEPart.DecomposeParts;
var
  x: integer;
  s: string;
  Mime: TMimePart;

  procedure SkipEmpty;
  begin
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      if s <> '' then
        Break;
      Inc(x);
    end;
  end;

begin
  FBinaryDecomposer := false;
  x := 0;
  Clear;
  //extract headers
  while FLines.Count > x do
  begin
    s := NormalizeHeader(FLines, x);
    if s = '' then
      Break;
    FHeaders.Add(s);
  end;
  DecodePartHeader;
  //extract prepart
  if FPrimaryCode = MP_MULTIPART then
  begin
    while FLines.Count > x do
    begin
      s := FLines[x];
      Inc(x);
      if TrimRight(s) = '--' + FBoundary then
        Break;
      FPrePart.Add(s);
      if not FAttachInside then
        FAttachInside := IsUUcode(s);
    end;
  end;
  //extract body part
  if FPrimaryCode = MP_MULTIPART then
  begin
    repeat
      if CanSubPart then
      begin
        Mime := AddSubPart;
        while FLines.Count > x do
        begin
          s := FLines[x];
          Inc(x);
          if Pos('--' + FBoundary, s) = 1 then
            Break;
          Mime.Lines.Add(s);
        end;
        Mime.DecomposeParts;
      end
      else
      begin
        s := FLines[x];
        Inc(x);
        FPartBody.Add(s);
      end;
      if x >= FLines.Count then
        break;
    until s = '--' + FBoundary + '--';
  end;
  if (FPrimaryCode = MP_MESSAGE) and CanSubPart then
  begin
    Mime := AddSubPart;
    SkipEmpty;
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      Inc(x);
      Mime.Lines.Add(s);
    end;
    Mime.DecomposeParts;
  end
  else
  begin
    while FLines.Count > x do
    begin
      s := FLines[x];
      Inc(x);
      FPartBody.Add(s);
      if not FAttachInside then
        FAttachInside := IsUUcode(s);
    end;
  end;
  //extract postpart
  if FPrimaryCode = MP_MULTIPART then
  begin
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      Inc(x);
      FPostPart.Add(s);
      if not FAttachInside then
        FAttachInside := IsUUcode(s);
    end;
  end;
end;

procedure TMIMEPart.DecomposePartsBinary(AHeader:TStrings; AStx,AEtx:PANSIChar);
var
  x:    integer;
  s:    ANSIString;
  Mime: TMimePart;
  BOP:  PANSIChar; // Beginning of Part
  EOP:  PANSIChar; // End of Part

  function ___HasUUCode(ALines:TStrings): boolean;
  var
    x: integer;
  begin
    Result := FALSE;
    for x:=0 to ALines.Count-1 do
      if IsUUcode(ALInes[x]) then
      begin
        Result := TRUE;
        exit;
      end;
  end;

begin
  FBinaryDecomposer := true;
  Clear;
  // Parse passed headers (THTTPSend returns HTTP headers and body separately)
  x := 0;
  while x<AHeader.Count do
    begin
      s := NormalizeHeader(AHeader,x);
      if s = '' then
        Break;
      FHeaders.Add(s);
    end;
  DecodePartHeader;
  // Extract prepart
  if FPrimaryCode=MP_MULTIPART then
    begin
      CopyLinesFromStreamUntilBoundary(AStx,AEtx,FPrePart,FBoundary);
      FAttachInside := FAttachInside or ___HasUUCode(FPrePart);
    end;
  // Extract body part
  if FPrimaryCode=MP_MULTIPART then
    begin
      repeat
        if CanSubPart then
          begin
            Mime := AddSubPart;
            BOP  := AStx;
            EOP  := SearchForBoundary(AStx,AEtx,FBoundary);
            CopyLinesFromStreamUntilNullLine(BOP,EOP,Mime.Lines);
            Mime.DecomposePartsBinary(Mime.Lines,BOP,EOP);
          end
        else
          begin
            EOP := SearchForBoundary(AStx,AEtx,FBoundary);
            FPartBody.Add(BuildStringFromBuffer(AStx,EOP));
          end;
        //
        BOP := MatchLastBoundary(EOP,AEtx,FBoundary);
        if Assigned(BOP) then
          begin
            AStx := BOP;
            Break;
          end;
      until FALSE;
    end;
  // Extract nested MIME message
  if (FPrimaryCode=MP_MESSAGE) and CanSubPart then
    begin
      Mime := AddSubPart;
      SkipNullLines(AStx,AEtx);
      CopyLinesFromStreamUntilNullLine(AStx,AEtx,Mime.Lines);
      Mime.DecomposePartsBinary(Mime.Lines,AStx,AEtx);
    end
  // Extract body of single part
  else
    begin
      FPartBody.Add(BuildStringFromBuffer(AStx,AEtx));
      FAttachInside := FAttachInside or ___HasUUCode(FPartBody);
    end;
  // Extract postpart
  if FPrimaryCode=MP_MULTIPART then
    begin
      CopyLinesFromStreamUntilBoundary(AStx,AEtx,FPostPart,'');
      FAttachInside := FAttachInside or ___HasUUCode(FPostPart);
    end;
end;
{/pf}

{==============================================================================}

procedure TMIMEPart.ComposeParts;
var
  n: integer;
  mime: TMimePart;
  s, t: string;
  d1, d2, d3: integer;
  x: integer;
begin
  FLines.Clear;
  //add headers
  for n := 0 to FHeaders.Count -1 do
  begin
    s := FHeaders[n];
    repeat
      if Length(s) < FMaxLineLength then
      begin
        t := s;
        s := '';
      end
      else
      begin
        d1 := RPosEx('; ', s, FMaxLineLength);
        d2 := RPosEx(' ', s, FMaxLineLength);
        d3 := RPosEx(', ', s, FMaxLineLength);
        if (d1 <= 1) and (d2 <= 1) and (d3 <= 1) then
        begin
          x := Pos(' ', Copy(s, 2, Length(s) - 1));
          if x < 1 then
            x := Length(s);
        end
        else
          if d1 > 0 then
            x := d1
          else
            if d3 > 0 then
              x := d3
            else
              x := d2 - 1;
        t := Copy(s, 1, x);
        Delete(s, 1, x);
      end;
      Flines.Add(t);
    until s = '';
  end;

  Flines.Add('');
  //add body
  //if multipart
  if FPrimaryCode = MP_MULTIPART then
  begin
    Flines.AddStrings(FPrePart);
    for n := 0 to GetSubPartCount - 1 do
    begin
      Flines.Add('--' + FBoundary);
      mime := GetSubPart(n);
      mime.ComposeParts;
      FLines.AddStrings(mime.Lines);
    end;
    Flines.Add('--' + FBoundary + '--');
    Flines.AddStrings(FPostPart);
  end;
  //if message
  if FPrimaryCode = MP_MESSAGE then
  begin
    if GetSubPartCount > 0 then
    begin
      mime := GetSubPart(0);
      mime.ComposeParts;
      FLines.AddStrings(mime.Lines);
    end;
  end
  else
  //if normal part
  begin
    FLines.AddStrings(FPartBody);
  end;
end;

{==============================================================================}

procedure TMIMEPart.DecodePart;
var
  n: Integer;
  s, t, t2: string;
  b: Boolean;
begin
  FDecodedLines.Clear;
  {pf}
  // The part decomposer passes data via TStringList which appends trailing line
  // break inherently. But in a case of native 8-bit data transferred withouth
  // encoding (default e.g. for HTTP protocol), the redundant line terminators
  // has to be removed
  if FBinaryDecomposer and (FPartBody.Count=1) then
    begin
      case FEncodingCode of
          ME_QUOTED_PRINTABLE:
            s := DecodeQuotedPrintable(FPartBody[0]);
          ME_BASE64:
            s := DecodeBase64(FPartBody[0]);
          ME_UU, ME_XX:
            begin
              s := '';
              for n := 0 to FPartBody.Count - 1 do
                if FEncodingCode = ME_UU then
                  s := s + DecodeUU(FPartBody[n])
                else
                  s := s + DecodeXX(FPartBody[n]);
            end;
        else
          s := FPartBody[0];
        end;
    end
  else
  {/pf}
  case FEncodingCode of
    ME_QUOTED_PRINTABLE:
      s := DecodeQuotedPrintable(FPartBody.Text);
    ME_BASE64:
      s := DecodeBase64(FPartBody.Text);
    ME_UU, ME_XX:
      begin
        s := '';
        for n := 0 to FPartBody.Count - 1 do
          if FEncodingCode = ME_UU then
            s := s + DecodeUU(FPartBody[n])
          else
            s := s + DecodeXX(FPartBody[n]);
      end;
  else
    s := FPartBody.Text;
  end;
  if FConvertCharset and (FPrimaryCode = MP_TEXT) then
    if (not FForcedHTMLConvert) and (uppercase(FSecondary) = 'HTML') then
    begin
      b := false;
      t2 := uppercase(s);
      t := SeparateLeft(t2, '</HEAD>');
      if length(t) <> length(s) then
      begin
        t := SeparateRight(t, '<HEAD>');
        t := ReplaceString(t, '"', '');
        t := ReplaceString(t, ' ', '');
        b := Pos('HTTP-EQUIV=CONTENT-TYPE', t) > 0;
      end;
      //workaround for shitty M$ Outlook 11 which is placing this information
      //outside <head> section
      if not b then
      begin
        t := Copy(t2, 1, 2048);
        t := ReplaceString(t, '"', '');
        t := ReplaceString(t, ' ', '');
        b := Pos('HTTP-EQUIV=CONTENT-TYPE', t) > 0;
      end;
      if not b then
        s := CharsetConversion(s, FCharsetCode, FTargetCharset);
    end
    else
      s := CharsetConversion(s, FCharsetCode, FTargetCharset);
  WriteStrToStream(FDecodedLines, s);
  FDecodedLines.Seek(0, soFromBeginning);
end;

{==============================================================================}

procedure TMIMEPart.DecodePartHeader;
var
  n: integer;
  s, su, fn: string;
  st, st2: string;
begin
  Primary := 'text';
  FSecondary := 'plain';
  FDescription := '';
  Charset := FDefaultCharset;
  FFileName := '';
  //was 7bit before, but this is more compatible with RFC-ignorant outlook
  Encoding := '8BIT';
  FDisposition := '';
  FContentID := '';
  fn := '';
  for n := 0 to FHeaders.Count - 1 do
    if FHeaders[n] <> '' then
    begin
      s := FHeaders[n];
      su := UpperCase(s);
      if Pos('CONTENT-TYPE:', su) = 1 then
      begin
        st := Trim(SeparateRight(su, ':'));
        st2 := Trim(SeparateLeft(st, ';'));
        Primary := Trim(SeparateLeft(st2, '/'));
        FSecondary := Trim(SeparateRight(st2, '/'));
        if (FSecondary = Primary) and (Pos('/', st2) < 1) then
          FSecondary := '';
        case FPrimaryCode of
          MP_TEXT:
            begin
              Charset := UpperCase(GetParameter(s, 'charset'));
              FFileName := GetParameter(s, 'name');
            end;
          MP_MULTIPART:
            FBoundary := GetParameter(s, 'Boundary');
          MP_MESSAGE:
            begin
            end;
          MP_BINARY:
            FFileName := GetParameter(s, 'name');
        end;
      end;
      if Pos('CONTENT-TRANSFER-ENCODING:', su) = 1 then
        Encoding := Trim(SeparateRight(su, ':'));
      if Pos('CONTENT-DESCRIPTION:', su) = 1 then
        FDescription := Trim(SeparateRight(s, ':'));
      if Pos('CONTENT-DISPOSITION:', su) = 1 then
      begin
        FDisposition := SeparateRight(su, ':');
        FDisposition := Trim(SeparateLeft(FDisposition, ';'));
        fn := GetParameter(s, 'FileName');
      end;
      if Pos('CONTENT-ID:', su) = 1 then
        FContentID := Trim(SeparateRight(s, ':'));
    end;
  if fn <> '' then
    FFileName := fn;
  FFileName := InlineDecode(FFileName, FTargetCharset);
  FFileName := ExtractFileName(FFileName);
end;

{==============================================================================}

procedure TMIMEPart.EncodePart;
var
  l: TStringList;
  s, t: string;
  n, x: Integer;
  d1, d2: integer;
begin
  if (FEncodingCode = ME_UU) or (FEncodingCode = ME_XX) then
    Encoding := 'base64';
  l := TStringList.Create;
  FPartBody.Clear;
  FDecodedLines.Seek(0, soFromBeginning);
  try
    case FPrimaryCode of
      MP_MULTIPART, MP_MESSAGE:
        FPartBody.LoadFromStream(FDecodedLines);
      MP_TEXT, MP_BINARY:
        begin
          s := ReadStrFromStream(FDecodedLines, FDecodedLines.Size);
          if FConvertCharset and (FPrimaryCode = MP_TEXT) and (FEncodingCode <> ME_7BIT) then
            s := GetBOM(FCharSetCode) + CharsetConversion(s, FTargetCharset, FCharsetCode);
          if FEncodingCode = ME_BASE64 then
          begin
            x := 1;
            while x <= length(s) do
            begin
              t := copy(s, x, 54);
              x := x + length(t);
              t := EncodeBase64(t);
              FPartBody.Add(t);
            end;
          end
          else
          begin
            if FPrimaryCode = MP_BINARY then
              l.Add(s)
            else
              l.Text := s;
            for n := 0 to l.Count - 1 do
            begin
              s := l[n];
              if FEncodingCode = ME_QUOTED_PRINTABLE then
              begin
                s := EncodeQuotedPrintable(s);
                repeat
                  if Length(s) < FMaxLineLength then
                  begin
                    t := s;
                    s := '';
                  end
                  else
                  begin
                    d1 := RPosEx('=', s, FMaxLineLength);
                    d2 := RPosEx(' ', s, FMaxLineLength);
                    if (d1 = 0) and (d2 = 0) then
                      x := FMaxLineLength
                    else
                      if d1 > d2 then
                        x := d1 - 1
                      else
                        x := d2 - 1;
                    if x = 0 then
                      x := FMaxLineLength;
                    t := Copy(s, 1, x);
                    Delete(s, 1, x);
                    if s <> '' then
                      t := t + '=';
                  end;
                  FPartBody.Add(t);
                until s = '';
              end
              else
                FPartBody.Add(s);
            end;
            if (FPrimaryCode = MP_BINARY)
              and (FEncodingCode = ME_QUOTED_PRINTABLE) then
              FPartBody[FPartBody.Count - 1] := FPartBody[FPartBody.Count - 1] + '=';
          end;
        end;
    end;
  finally
    l.Free;
  end;
end;

{==============================================================================}

procedure TMIMEPart.EncodePartHeader;
var
  s: string;
begin
  FHeaders.Clear;
  if FSecondary = '' then
    case FPrimaryCode of
      MP_TEXT:
        FSecondary := 'plain';
      MP_MULTIPART:
        FSecondary := 'mixed';
      MP_MESSAGE:
        FSecondary := 'rfc822';
      MP_BINARY:
        FSecondary := 'octet-stream';
    end;
  if FDescription <> '' then
    FHeaders.Insert(0, 'Content-Description: ' + FDescription);
  if FDisposition <> '' then
  begin
    s := '';
    if FFileName <> '' then
      s := '; FileName=' + QuoteStr(InlineCodeEx(FileName, FTargetCharset), '"');
    FHeaders.Insert(0, 'Content-Disposition: ' + LowerCase(FDisposition) + s);
  end;
  if FContentID <> '' then
    FHeaders.Insert(0, 'Content-ID: ' + FContentID);

  case FEncodingCode of
    ME_7BIT:
      s := '7bit';
    ME_8BIT:
      s := '8bit';
    ME_QUOTED_PRINTABLE:
      s := 'Quoted-printable';
    ME_BASE64:
      s := 'Base64';
  end;
  case FPrimaryCode of
    MP_TEXT,
      MP_BINARY: FHeaders.Insert(0, 'Content-Transfer-Encoding: ' + s);
  end;
  case FPrimaryCode of
    MP_TEXT:
      s := FPrimary + '/' + FSecondary + '; charset=' + GetIDfromCP(FCharsetCode);
    MP_MULTIPART:
      s := FPrimary + '/' + FSecondary + '; boundary="' + FBoundary + '"';
    MP_MESSAGE, MP_BINARY:
      s := FPrimary + '/' + FSecondary;
  end;
  if FFileName <> '' then
    s := s + '; name=' + QuoteStr(InlineCodeEx(FileName, FTargetCharset), '"');
  FHeaders.Insert(0, 'Content-type: ' + s);
end;

{==============================================================================}

procedure TMIMEPart.MimeTypeFromExt(Value: string);
var
  s: string;
  n: Integer;
begin
  Primary := '';
  FSecondary := '';
  s := UpperCase(ExtractFileExt(Value));
  if s = '' then
    s := UpperCase(Value);
  s := SeparateRight(s, '.');
  for n := 0 to MaxMimeType do
    if MimeType[n, 0] = s then
    begin
      Primary := MimeType[n, 1];
      FSecondary := MimeType[n, 2];
      Break;
    end;
  if Primary = '' then
    Primary := 'application';
  if FSecondary = '' then
    FSecondary := 'octet-stream';
end;

{==============================================================================}

procedure TMIMEPart.WalkPart;
var
  n: integer;
  m: TMimepart;
begin
  if assigned(OnWalkPart) then
  begin
    OnWalkPart(self);
    for n := 0 to GetSubPartCount - 1 do
    begin
      m := GetSubPart(n);
      m.OnWalkPart := OnWalkPart;
      m.WalkPart;
    end;
  end;
end;

{==============================================================================}

procedure TMIMEPart.SetPrimary(Value: string);
var
  s: string;
begin
  FPrimary := Value;
  s := UpperCase(Value);
  FPrimaryCode := MP_BINARY;
  if Pos('TEXT', s) = 1 then
    FPrimaryCode := MP_TEXT;
  if Pos('MULTIPART', s) = 1 then
    FPrimaryCode := MP_MULTIPART;
  if Pos('MESSAGE', s) = 1 then
    FPrimaryCode := MP_MESSAGE;
end;

procedure TMIMEPart.SetEncoding(Value: string);
var
  s: string;
begin
  FEncoding := Value;
  s := UpperCase(Value);
  FEncodingCode := ME_7BIT;
  if Pos('8BIT', s) = 1 then
    FEncodingCode := ME_8BIT;
  if Pos('QUOTED-PRINTABLE', s) = 1 then
    FEncodingCode := ME_QUOTED_PRINTABLE;
  if Pos('BASE64', s) = 1 then
    FEncodingCode := ME_BASE64;
  if Pos('X-UU', s) = 1 then
    FEncodingCode := ME_UU;
  if Pos('X-XX', s) = 1 then
    FEncodingCode := ME_XX;
end;

procedure TMIMEPart.SetCharset(Value: string);
begin
  if value <> '' then
  begin
    FCharset := Value;
    FCharsetCode := GetCPFromID(Value);
  end;
end;

function TMIMEPart.CanSubPart: boolean;
begin
  Result := True;
  if FMaxSubLevel <> -1 then
    Result := FMaxSubLevel > FSubLevel;
end;

function TMIMEPart.IsUUcode(Value: string): boolean;
begin
  Value := UpperCase(Value);
  Result := (pos('BEGIN ', Value) = 1) and (Trim(SeparateRight(Value, ' ')) <> '');
end;

{==============================================================================}

function GenerateBoundary: string;
var
  x, y: Integer;
begin
  y := GetTick;
  x := y;
  while TickDelta(y, x) = 0 do
  begin
    Sleep(1);
    x := GetTick;
  end;
  Randomize;
  y := Random(MaxInt);
  Result := IntToHex(x, 8) + '_' + IntToHex(y, 8) + '_Synapse_boundary';
end;

end.
