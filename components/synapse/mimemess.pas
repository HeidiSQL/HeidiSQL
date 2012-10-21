{==============================================================================|
| Project : Ararat Synapse                                       | 002.006.000 |
|==============================================================================|
| Content: MIME message object                                                 |
|==============================================================================|
| Copyright (c)1999-2012, Lukas Gebauer                                        |
| All rights reserved.                                                         |
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
| History: see HISTORY.HTM From distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(MIME message handling)
Classes for easy handling with e-mail message.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$M+}

unit mimemess;

interface

uses
  Classes, SysUtils,
  mimepart, synachar, synautil, mimeinln;

type

  {:Possible values for message priority}
  TMessPriority = (MP_unknown, MP_low, MP_normal, MP_high);

  {:@abstract(Object for basic e-mail header fields.)}
  TMessHeader = class(TObject)
  private
    FFrom: string;
    FToList: TStringList;
    FCCList: TStringList;
    FSubject: string;
    FOrganization: string;
    FCustomHeaders: TStringList;
    FDate: TDateTime;
    FXMailer: string;
    FCharsetCode: TMimeChar;
    FReplyTo: string;
    FMessageID: string;
    FPriority: TMessPriority;
    Fpri: TMessPriority;
    Fxpri: TMessPriority;
    Fxmspri: TMessPriority;
  protected
    function ParsePriority(value: string): TMessPriority;
    function DecodeHeader(value: string): boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    {:Clears all data fields.}
    procedure Clear; virtual;

    {Add headers from from this object to Value.}
    procedure EncodeHeaders(const Value: TStrings); virtual;

    {:Parse header from Value to this object.}
    procedure DecodeHeaders(const Value: TStrings);

    {:Try find specific header in CustomHeader. Search is case insensitive.
     This is good for reading any non-parsed header.}
    function FindHeader(Value: string): string;

    {:Try find specific headers in CustomHeader. This metod is for repeatly used
     headers like 'received' header, etc. Search is case insensitive.
     This is good for reading ano non-parsed header.}
    procedure FindHeaderList(Value: string; const HeaderList: TStrings);
  published
    {:Sender of message.}
    property From: string read FFrom Write FFrom;

    {:Stringlist with receivers of message. (one per line)}
    property ToList: TStringList read FToList;

    {:Stringlist with Carbon Copy receivers of message. (one per line)}
    property CCList: TStringList read FCCList;

    {:Subject of message.}
    property Subject: string read FSubject Write FSubject;

    {:Organization string.}
    property Organization: string read FOrganization Write FOrganization;

    {:After decoding contains all headers lines witch not have parsed to any
     other structures in this object. It mean: this conatins all other headers
     except:

     X-MAILER, FROM, SUBJECT, ORGANIZATION, TO, CC, DATE, MIME-VERSION,
     CONTENT-TYPE, CONTENT-DESCRIPTION, CONTENT-DISPOSITION, CONTENT-ID,
     CONTENT-TRANSFER-ENCODING, REPLY-TO, MESSAGE-ID, X-MSMAIL-PRIORITY,
     X-PRIORITY, PRIORITY

     When you encode headers, all this lines is added as headers. Be carefull
     for duplicites!}
    property CustomHeaders: TStringList read FCustomHeaders;

    {:Date and time of message.}
    property Date: TDateTime read FDate Write FDate;

    {:Mailer identification.}
    property XMailer: string read FXMailer Write FXMailer;

    {:Address for replies}
    property ReplyTo: string read FReplyTo Write FReplyTo;

    {:message indetifier}
    property MessageID: string read FMessageID Write FMessageID;

    {:message priority}
    property Priority: TMessPriority read FPriority Write FPriority;

    {:Specify base charset. By default is used system charset.}
    property CharsetCode: TMimeChar read FCharsetCode Write FCharsetCode;
  end;

  TMessHeaderClass = class of TMessHeader;

  {:@abstract(Object for handling of e-mail message.)}
  TMimeMess = class(TObject)
  private
    FMessagePart: TMimePart;
    FLines: TStringList;
    FHeader: TMessHeader;
  public
    constructor Create;
    {:create this object and assign your own descendant of @link(TMessHeader)
     object to @link(header) property. So, you can create your own message
     headers parser and use it by this object.}
    constructor CreateAltHeaders(HeadClass: TMessHeaderClass);
    destructor Destroy; override;

    {:Reset component to default state.}
    procedure Clear; virtual;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then one subpart,
     you must have PartParent of multipart type!}
    function AddPart(const PartParent: TMimePart): TMimePart;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then 1 subpart, you
     must have PartParent of multipart type!

     This part is marked as multipart with secondary MIME type specified by
     MultipartType parameter. (typical value is 'mixed')

     This part can be used as PartParent for another parts (include next
     multipart). If you need only one part, then you not need Multipart part.}
    function AddPartMultipart(const MultipartType: String; const PartParent: TMimePart): TMimePart;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then 1 subpart, you
     must have PartParent of multipart type!

     After creation of part set type to text part and set all necessary
     properties. Content of part is readed from value stringlist.}
    function AddPartText(const Value: TStrings; const PartParent: TMimePart): TMimepart;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then 1 subpart, you
     must have PartParent of multipart type!

     After creation of part set type to text part and set all necessary
     properties. Content of part is readed from value stringlist. You can select
     your charset and your encoding type. If Raw is @true, then it not doing
     charset conversion!}
    function AddPartTextEx(const Value: TStrings; const PartParent: TMimePart;
      PartCharset: TMimeChar; Raw: Boolean; PartEncoding: TMimeEncoding): TMimepart;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then 1 subpart, you
     must have PartParent of multipart type!

     After creation of part set type to text part to HTML type and set all
     necessary properties. Content of HTML part is readed from Value stringlist.}
    function AddPartHTML(const Value: TStrings; const PartParent: TMimePart): TMimepart;

    {:Same as @link(AddPartText), but content is readed from file}
    function AddPartTextFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;

    {:Same as @link(AddPartHTML), but content is readed from file}
    function AddPartHTMLFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then 1 subpart,
     you must have PartParent of multipart type!

     After creation of part set type to binary and set all necessary properties.
     MIME primary and secondary types defined automaticly by filename extension.
     Content of binary part is readed from Stream. This binary part is encoded
     as file attachment.}
    function AddPartBinary(const Stream: TStream; const FileName: string; const PartParent: TMimePart): TMimepart;

    {:Same as @link(AddPartBinary), but content is readed from file}
    function AddPartBinaryFromFile(const FileName: string; const PartParent: TMimePart): TMimepart;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then 1 subpart, you
     must have PartParent of multipart type!

     After creation of part set type to binary and set all necessary properties.
     MIME primary and secondary types defined automaticly by filename extension.
     Content of binary part is readed from Stream.

     This binary part is encoded as inline data with given Conten ID (cid).
     Content ID can be used as reference ID in HTML source in HTML part.}
    function AddPartHTMLBinary(const Stream: TStream; const FileName, Cid: string; const PartParent: TMimePart): TMimepart;

    {:Same as @link(AddPartHTMLBinary), but content is readed from file}
    function AddPartHTMLBinaryFromFile(const FileName, Cid: string; const PartParent: TMimePart): TMimepart;

    {:Add MIME part as subpart of PartParent. If you need set root MIME part,
     then set as PartParent @NIL value. If you need set more then 1 subpart, you
     must have PartParent of multipart type!

     After creation of part set type to message and set all necessary properties.
     MIME primary and secondary types are setted to 'message/rfc822'.
     Content of raw RFC-822 message is readed from Stream.}
    function AddPartMess(const Value: TStrings; const PartParent: TMimePart): TMimepart;

    {:Same as @link(AddPartMess), but content is readed from file}
    function AddPartMessFromFile(const FileName: string; const PartParent: TMimePart): TMimepart;

    {:Compose message from @link(MessagePart) to @link(Lines). Headers from
     @link(Header) object is added also.}
    procedure EncodeMessage;

    {:Decode message from @link(Lines) to @link(MessagePart). Massage headers
     are parsed into @link(Header) object.}
    procedure DecodeMessage;

    {pf}
    {: HTTP message is received by @link(THTTPSend) component in two parts:
     headers are stored in @link(THTTPSend.Headers) and a body in memory stream
     @link(THTTPSend.Document).

     On the top of it, HTTP connections are always 8-bit, hence data are
     transferred in native format i.e. no transfer encoding is applied.

     This method operates the similiar way and produces the same
     result as @link(DecodeMessage).
    }
    procedure DecodeMessageBinary(AHeader:TStrings; AData:TMemoryStream);
    {/pf}
  published
    {:@link(TMimePart) object with decoded MIME message. This object can handle
     any number of nested @link(TMimePart) objects itself. It is used for handle
     any tree of MIME subparts.}
    property MessagePart: TMimePart read FMessagePart;

    {:Raw MIME encoded message.}
    property Lines: TStringList read FLines;

    {:Object for e-mail header fields. This object is created automaticly.
     Do not free this object!}
    property Header: TMessHeader read FHeader;
  end;

implementation

{==============================================================================}

constructor TMessHeader.Create;
begin
  inherited Create;
  FToList := TStringList.Create;
  FCCList := TStringList.Create;
  FCustomHeaders := TStringList.Create;
  FCharsetCode := GetCurCP;
end;

destructor TMessHeader.Destroy;
begin
  FCustomHeaders.Free;
  FCCList.Free;
  FToList.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMessHeader.Clear;
begin
  FFrom := '';
  FToList.Clear;
  FCCList.Clear;
  FSubject := '';
  FOrganization := '';
  FCustomHeaders.Clear;
  FDate := 0;
  FXMailer := '';
  FReplyTo := '';
  FMessageID := '';
  FPriority := MP_unknown;
end;

procedure TMessHeader.EncodeHeaders(const Value: TStrings);
var
  n: Integer;
  s: string;
begin
  if FDate = 0 then
    FDate := Now;
  for n := FCustomHeaders.Count - 1 downto 0 do
    if FCustomHeaders[n] <> '' then
      Value.Insert(0, FCustomHeaders[n]);
  if FPriority <> MP_unknown then
    case FPriority of
      MP_high:
        begin
          Value.Insert(0, 'X-MSMAIL-Priority: High');
          Value.Insert(0, 'X-Priority: 1');
          Value.Insert(0, 'Priority: urgent');
        end;
      MP_low:
        begin
          Value.Insert(0, 'X-MSMAIL-Priority: low');
          Value.Insert(0, 'X-Priority: 5');
          Value.Insert(0, 'Priority: non-urgent');
        end;
    end;
  if FReplyTo <> '' then
    Value.Insert(0, 'Reply-To: ' + GetEmailAddr(FReplyTo));
  if FMessageID <> '' then
    Value.Insert(0, 'Message-ID: <' + trim(FMessageID) + '>');
  if FXMailer = '' then
    Value.Insert(0, 'X-mailer: Synapse - Pascal TCP/IP library by Lukas Gebauer')
  else
    Value.Insert(0, 'X-mailer: ' + FXMailer);
  Value.Insert(0, 'MIME-Version: 1.0 (produced by Synapse)');
  if FOrganization <> '' then
    Value.Insert(0, 'Organization: ' + InlineCodeEx(FOrganization, FCharsetCode));
  s := '';
  for n := 0 to FCCList.Count - 1 do
    if s = '' then
      s := InlineEmailEx(FCCList[n], FCharsetCode)
    else
      s := s + ', ' + InlineEmailEx(FCCList[n], FCharsetCode);
  if s <> '' then
    Value.Insert(0, 'CC: ' + s);
  Value.Insert(0, 'Date: ' + Rfc822DateTime(FDate));
  if FSubject <> '' then
    Value.Insert(0, 'Subject: ' + InlineCodeEx(FSubject, FCharsetCode));
  s := '';
  for n := 0 to FToList.Count - 1 do
    if s = '' then
      s := InlineEmailEx(FToList[n], FCharsetCode)
    else
      s := s + ', ' + InlineEmailEx(FToList[n], FCharsetCode);
  if s <> '' then
    Value.Insert(0, 'To: ' + s);
  Value.Insert(0, 'From: ' + InlineEmailEx(FFrom, FCharsetCode));
end;

function TMessHeader.ParsePriority(value: string): TMessPriority;
var
  s: string;
  x: integer;
begin
  Result := MP_unknown;
  s := Trim(separateright(value, ':'));
  s := Separateleft(s, ' ');
  x := StrToIntDef(s, -1);
  if x >= 0 then
    case x of
      1, 2:
        Result := MP_High;
      3:
        Result := MP_Normal;
      4, 5:
        Result := MP_Low;
    end
  else
  begin
    s := lowercase(s);
    if (s = 'urgent') or (s = 'high') or (s = 'highest') then
      Result := MP_High;
    if (s = 'normal') or (s = 'medium') then
      Result := MP_Normal;
    if (s = 'low') or (s = 'lowest')
      or (s = 'no-priority')  or (s = 'non-urgent') then
      Result := MP_Low;
  end;
end;

function TMessHeader.DecodeHeader(value: string): boolean;
var
  s, t: string;
  cp: TMimeChar;
begin
  Result := True;
  cp := FCharsetCode;
  s := uppercase(value);
  if Pos('X-MAILER:', s) = 1 then
  begin
    FXMailer := Trim(SeparateRight(Value, ':'));
    Exit;
  end;
  if Pos('FROM:', s) = 1 then
  begin
    FFrom := InlineDecode(Trim(SeparateRight(Value, ':')), cp);
    Exit;
  end;
  if Pos('SUBJECT:', s) = 1 then
  begin
    FSubject := InlineDecode(Trim(SeparateRight(Value, ':')), cp);
    Exit;
  end;
  if Pos('ORGANIZATION:', s) = 1 then
  begin
    FOrganization := InlineDecode(Trim(SeparateRight(Value, ':')), cp);
    Exit;
  end;
  if Pos('TO:', s) = 1 then
  begin
    s := Trim(SeparateRight(Value, ':'));
    repeat
      t := InlineDecode(Trim(FetchEx(s, ',', '"')), cp);
      if t <> '' then
        FToList.Add(t);
    until s = '';
    Exit;
  end;
  if Pos('CC:', s) = 1 then
  begin
    s := Trim(SeparateRight(Value, ':'));
    repeat
      t := InlineDecode(Trim(FetchEx(s, ',', '"')), cp);
      if t <> '' then
        FCCList.Add(t);
    until s = '';
    Exit;
  end;
  if Pos('DATE:', s) = 1 then
  begin
    FDate := DecodeRfcDateTime(Trim(SeparateRight(Value, ':')));
    Exit;
  end;
  if Pos('REPLY-TO:', s) = 1 then
  begin
    FReplyTo := InlineDecode(Trim(SeparateRight(Value, ':')), cp);
    Exit;
  end;
  if Pos('MESSAGE-ID:', s) = 1 then
  begin
    FMessageID := GetEmailAddr(Trim(SeparateRight(Value, ':')));
    Exit;
  end;
  if Pos('PRIORITY:', s) = 1 then
  begin
    FPri := ParsePriority(value);
    Exit;
  end;
  if Pos('X-PRIORITY:', s) = 1 then
  begin
    FXPri := ParsePriority(value);
    Exit;
  end;
  if Pos('X-MSMAIL-PRIORITY:', s) = 1 then
  begin
    FXmsPri := ParsePriority(value);
    Exit;
  end;
  if Pos('MIME-VERSION:', s) = 1 then
    Exit;
  if Pos('CONTENT-TYPE:', s) = 1 then
    Exit;
  if Pos('CONTENT-DESCRIPTION:', s) = 1 then
    Exit;
  if Pos('CONTENT-DISPOSITION:', s) = 1 then
    Exit;
  if Pos('CONTENT-ID:', s) = 1 then
    Exit;
  if Pos('CONTENT-TRANSFER-ENCODING:', s) = 1 then
    Exit;
  Result := False;
end;

procedure TMessHeader.DecodeHeaders(const Value: TStrings);
var
  s: string;
  x: Integer;
begin
  Clear;
  Fpri := MP_unknown;
  Fxpri := MP_unknown;
  Fxmspri := MP_unknown;
  x := 0;
  while Value.Count > x do
  begin
    s := NormalizeHeader(Value, x);
    if s = '' then
      Break;
    if not DecodeHeader(s) then
      FCustomHeaders.Add(s);
  end;
  if Fpri <> MP_unknown then
    FPriority := Fpri
  else
    if Fxpri <> MP_unknown then
      FPriority := Fxpri
    else
      if Fxmspri <> MP_unknown then
        FPriority := Fxmspri
end;

function TMessHeader.FindHeader(Value: string): string;
var
  n: integer;
begin
  Result := '';
  for n := 0 to FCustomHeaders.Count - 1 do
    if Pos(UpperCase(Value), UpperCase(FCustomHeaders[n])) = 1 then
    begin
      Result := Trim(SeparateRight(FCustomHeaders[n], ':'));
      break;
    end;
end;

procedure TMessHeader.FindHeaderList(Value: string; const HeaderList: TStrings);
var
  n: integer;
begin
  HeaderList.Clear;
  for n := 0 to FCustomHeaders.Count - 1 do
    if Pos(UpperCase(Value), UpperCase(FCustomHeaders[n])) = 1 then
    begin
      HeaderList.Add(Trim(SeparateRight(FCustomHeaders[n], ':')));
    end;
end;

{==============================================================================}

constructor TMimeMess.Create;
begin
  CreateAltHeaders(TMessHeader);
end;

constructor TMimeMess.CreateAltHeaders(HeadClass: TMessHeaderClass);
begin
  inherited Create;
  FMessagePart := TMimePart.Create;
  FLines := TStringList.Create;
  FHeader := HeadClass.Create;
end;

destructor TMimeMess.Destroy;
begin
  FMessagePart.Free;
  FHeader.Free;
  FLines.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMimeMess.Clear;
begin
  FMessagePart.Clear;
  FLines.Clear;
  FHeader.Clear;
end;

{==============================================================================}

function TMimeMess.AddPart(const PartParent: TMimePart): TMimePart;
begin
  if PartParent = nil then
    Result := FMessagePart
  else
    Result := PartParent.AddSubPart;
  Result.Clear;
end;

{==============================================================================}

function TMimeMess.AddPartMultipart(const MultipartType: String; const PartParent: TMimePart): TMimePart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Primary := 'Multipart';
    Secondary := MultipartType;
    Description := 'Multipart message';
    Boundary := GenerateBoundary;
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartText(const Value: TStrings; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'plain';
    Description := 'Message text';
    Disposition := 'inline';
    CharsetCode := IdealCharsetCoding(Value.Text, TargetCharset, IdealCharsets);
    EncodingCode := ME_QUOTED_PRINTABLE;
    EncodePart;
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartTextEx(const Value: TStrings; const PartParent: TMimePart;
  PartCharset: TMimeChar; Raw: Boolean; PartEncoding: TMimeEncoding): TMimepart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'plain';
    Description := 'Message text';
    Disposition := 'inline';
    CharsetCode := PartCharset;
    EncodingCode := PartEncoding;
    ConvertCharset := not Raw;
    EncodePart;
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartHTML(const Value: TStrings; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'html';
    Description := 'HTML text';
    Disposition := 'inline';
    CharsetCode := UTF_8;
    EncodingCode := ME_QUOTED_PRINTABLE;
    EncodePart;
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartTextFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;
var
  tmp: TStrings;
begin
  tmp := TStringList.Create;
  try
    tmp.LoadFromFile(FileName);
    Result := AddPartText(tmp, PartParent);
  Finally
    tmp.Free;
  end;
end;

function TMimeMess.AddPartHTMLFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;
var
  tmp: TStrings;
begin
  tmp := TStringList.Create;
  try
    tmp.LoadFromFile(FileName);
    Result := AddPartHTML(tmp, PartParent);
  Finally
    tmp.Free;
  end;
end;

function TMimeMess.AddPartBinary(const Stream: TStream; const FileName: string; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  Result.DecodedLines.LoadFromStream(Stream);
  Result.MimeTypeFromExt(FileName);
  Result.Description := 'Attached file: ' + FileName;
  Result.Disposition := 'attachment';
  Result.FileName := FileName;
  Result.EncodingCode := ME_BASE64;
  Result.EncodePart;
  Result.EncodePartHeader;
end;

function TMimeMess.AddPartBinaryFromFile(const FileName: string; const PartParent: TMimePart): TMimepart;
var
  tmp: TMemoryStream;
begin
  tmp := TMemoryStream.Create;
  try
    tmp.LoadFromFile(FileName);
    Result := AddPartBinary(tmp, ExtractFileName(FileName), PartParent);
  finally
    tmp.Free;
  end;
end;

function TMimeMess.AddPartHTMLBinary(const Stream: TStream; const FileName, Cid: string; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  Result.DecodedLines.LoadFromStream(Stream);
  Result.MimeTypeFromExt(FileName);
  Result.Description := 'Included file: ' + FileName;
  Result.Disposition := 'inline';
  Result.ContentID := Cid;
  Result.FileName := FileName;
  Result.EncodingCode := ME_BASE64;
  Result.EncodePart;
  Result.EncodePartHeader;
end;

function TMimeMess.AddPartHTMLBinaryFromFile(const FileName, Cid: string; const PartParent: TMimePart): TMimepart;
var
  tmp: TMemoryStream;
begin
  tmp := TMemoryStream.Create;
  try
    tmp.LoadFromFile(FileName);
    Result :=AddPartHTMLBinary(tmp, ExtractFileName(FileName), Cid, PartParent);
  finally
    tmp.Free;
  end;
end;

function TMimeMess.AddPartMess(const Value: TStrings; const PartParent: TMimePart): TMimepart;
var
  part: Tmimepart;
begin
  Result := AddPart(PartParent);
  part := AddPart(result);
  part.lines.addstrings(Value);
  part.DecomposeParts;
  with Result do
  begin
    Primary := 'message';
    Secondary := 'rfc822';
    Description := 'E-mail Message';
    EncodePart;
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartMessFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;
var
  tmp: TStrings;
begin
  tmp := TStringList.Create;
  try
    tmp.LoadFromFile(FileName);
    Result := AddPartMess(tmp, PartParent);
  Finally
    tmp.Free;
  end;
end;

{==============================================================================}

procedure TMimeMess.EncodeMessage;
var
  l: TStringList;
  x: integer;
begin
  //merge headers from THeaders and header field from MessagePart
  l := TStringList.Create;
  try
    FHeader.EncodeHeaders(l);
    x := IndexByBegin('CONTENT-TYPE', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-DESCRIPTION', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-DISPOSITION', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-ID', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-TRANSFER-ENCODING', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    FMessagePart.Headers.Assign(l);
  finally
    l.Free;
  end;
  FMessagePart.ComposeParts;
  FLines.Assign(FMessagePart.Lines);
end;

{==============================================================================}

procedure TMimeMess.DecodeMessage;
begin
  FHeader.Clear;
  FHeader.DecodeHeaders(FLines);
  FMessagePart.Lines.Assign(FLines);
  FMessagePart.DecomposeParts;
end;

{pf}
procedure TMimeMess.DecodeMessageBinary(AHeader:TStrings; AData:TMemoryStream);
begin
  FHeader.Clear;
  FLines.Clear;
  FLines.Assign(AHeader);
  FHeader.DecodeHeaders(FLines);
  FMessagePart.DecomposePartsBinary(AHeader,PANSIChar(AData.Memory),PANSIChar(AData.Memory)+AData.Size);
end;
{/pf}

end.
