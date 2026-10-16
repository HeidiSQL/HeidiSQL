{ VerySimpleXML v3.0.0 - a lightweight, one-unit, cross-platform XML reader/writer
  for Delphi 10.4+ by Dennis Spreen
  http://blog.spreendigital.de/2014/09/13/verysimplexml-3-0/

  (c) Copyrights 2011-2020 Dennis D. Spreen <dennis@spreendigital.de>
  This unit is free and can be used for any needs. The introduction of
  any changes and the use of those changed library is permitted without
  limitations. Only requirement:
  This text must be present without changes in all modifications of library.

  * The contents of this file are used with permission, subject to
  * the Mozilla Public License Version 1.1 (the "License"); you may   *
  * not use this file except in compliance with the License. You may  *
  * obtain a copy of the License at                                   *
  * http:  www.mozilla.org/MPL/MPL-1.1.html                           *
  *                                                                   *
  * Software distributed under the License is distributed on an       *
  * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
  * implied. See the License for the specific language governing      *
  * rights and limitations under the License.                         *
}
unit Xml.VerySimple;

interface

uses
  System.Classes, System.SysUtils, Generics.Defaults, Generics.Collections, System.Rtti;

const
  TXmlSpaces = #$20 + #$0A + #$0D + #9;

type
  TXmlVerySimple = class;
  TXmlNode = class;
  TXmlNodeType = (ntElement, ntText, ntCData, ntProcessingInstr, ntComment, ntDocument, ntDocType, ntXmlDecl);
  TXmlNodeTypes = set of TXmlNodeType;
  TXmlNodeList = class;
  TXmlAttributeType = (atValue, atSingle);
  TXmlOptions = set of (doNodeAutoIndent, doCompact, doParseProcessingInstr, doPreserveWhiteSpace, doCaseInsensitive,
    doWriteBOM, doSimplifyTextNodes);
  TExtractTextOptions = set of (etoDeleteStopChar, etoStopString);

  {$IFNDEF AUTOREFCOUNT}
  WeakAttribute = class(TCustomAttribute);
  {$ENDIF}

  TStreamReaderFillBuffer = procedure(var Encoding: TEncoding) of object;

  TXmlStreamReader = class(TStreamReader)
  protected
    ///	<summary> Call to FillBuffer method of TStreamreader </summary>
    procedure FillBuffer; overload;
  public
    ///	<summary> Assures the read buffer holds at least Value characters </summary>
    function PrepareBuffer(Value: Integer): Boolean;
    ///	<summary> Extract text until chars found in StopChars </summary>
    function ReadText(const StopChars: String; Options: TExtractTextOptions): String; virtual;
    ///	<summary> Returns fist char but does not removes it from the buffer </summary>
    function FirstChar: String;
    ///	<summary> Proceed with the next character(s) (value optional, default 1) </summary>
    procedure IncCharPos(Value: Integer = 1); virtual;
    ///	<summary> Returns True if the first uppercased characters at the current position match Value </summary>
    function IsUppercaseText(const Value: String): Boolean; virtual;
  end;


  TXmlAttribute = class(TObject)
  private
    FValue: String;
  protected
    procedure SetValue(const Value: String); virtual;
  public
    ///	<summary> Attribute name </summary>
    Name: String;
    ///	<summary> Attributes without values are set to atSingle, else to atValue </summary>
    AttributeType: TXmlAttributeType;
    ///	<summary> Create a new attribute </summary>
    constructor Create; virtual;
    ///	<summary> Return the attribute as a String </summary>
    function AsString: String;
    /// <summary> Escapes XML control characters </summar>
    class function Escape(const Value: String): String; virtual;
    ///	<summary> Assign attribute values from source attribute </summary>
    procedure Assign(Source: TXmlAttribute); virtual;
    ///	<summary> Attribute value (always a String) </summary>
    property Value: String read FValue write SetValue;
  end;

  TXmlAttributeList = class(TObjectList<TXmlAttribute>)
  public
    ///	<summary> The xml document of the attribute list of the node</summary>
    [Weak] Document: TXmlVerySimple;
    ///	<summary> Add a name only attribute </summary>
    function Add(const Name: String): TXmlAttribute; overload; virtual;
    ///	<summary> Returns the attribute given by name (case insensitive), NIL if no attribute found </summary>
    function Find(const Name: String): TXmlAttribute; virtual;
    ///	<summary> Deletes an attribute given by name (case insensitive) </summary>
    procedure Delete(const Name: String); overload; virtual;
    ///	<summary> Returns True if an attribute with the given name is found (case insensitive) </summary>
    function HasAttribute(const AttrName: String): Boolean; virtual;
    ///	<summary> Returns the attributes in string representation </summary>
    function AsString: String; virtual;
    ///	<summary> Clears current attributes and assigns all attributes from source attributes </summary>
    procedure Assign(Source: TXmlAttributeList); virtual;
  end;

  TXmlNode = class(TObject)
  protected
    [Weak] FDocument: TXmlVerySimple;
    procedure SetDocument(Value: TXmlVerySimple);
    function GetAttr(const AttrName: String): String; virtual;
    procedure SetAttr(const AttrName: String; const AttrValue: String); virtual;
  public
    ///	<summary> All attributes of the node </summary>
    AttributeList: TXmlAttributeList;
    ///	<summary> List of child nodes, never NIL </summary>
    ChildNodes: TXmlNodeList;
    ///	<summary> Name of the node </summary>
    Name: String; // Node name
    ///	<summary> The node type, see TXmlNodeType </summary>
    NodeType: TXmlNodeType;
    ///	<summary> Parent node, may be NIL </summary>
    [Weak] Parent: TXmlNode;
    ///	<summary> Text value of the node </summary>
    Text: String;
    /// <summary> Creates a new XML node </summary>
    constructor Create(ANodeType: TXmlNodeType = ntElement); virtual;
    ///	<summary> Removes the node from its parent and frees all of its childs </summary>
    destructor Destroy; override;
    ///	<summary> Clears the attributes, the text and all of its child nodes (but not the name) </summary>
    procedure Clear;
    ///	<summary> Find a child node by its name </summary>
    function Find(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode; overload; virtual;
    ///	<summary> Find a child node by name and attribute name </summary>
    function Find(const Name, AttrName: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode; overload; virtual;
    ///	<summary> Find a child node by name, attribute name and attribute value </summary>
    function Find(const Name, AttrName, AttrValue: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode; overload; virtual;
    ///	<summary> Return a list of child nodes with the given name and (optional) node types </summary>
    function FindNodes(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNodeList; virtual;
    ///	<summary> Return a child node by NodePath </summary>
    function SelectNode(const NodePath: String): TXmlNode; overload; virtual;
    ///	<summary> Returns True if the attribute exists </summary>
    function HasAttribute(const AttrName: String): Boolean; virtual;
    ///	<summary> Returns True if a child node with that name exits </summary>
    function HasChild(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): Boolean; virtual;
    ///	<summary> Add a child node with an optional NodeType (default: ntElement)</summary>
    function AddChild(const AName: String; ANodeType: TXmlNodeType = ntElement): TXmlNode; virtual;
    ///	<summary> Insert a child node at a specific position with a (optional) NodeType (default: ntElement)</summary>
    function InsertChild(const Name: String; Position: Integer; NodeType: TXmlNodeType = ntElement): TXmlNode; virtual;
    ///	<summary> Fluent interface for setting the text of the node </summary>
    function SetText(const Value: String): TXmlNode; virtual;
    ///	<summary> Fluent interface for setting the node attribute given by attribute name and attribute value </summary>
    function SetAttribute(const AttrName, AttrValue: String): TXmlNode; virtual;
    ///	<summary> Returns first child or NIL if there aren't any child nodes </summary>
    function FirstChild: TXmlNode; virtual;
    ///	<summary> Returns last child node or NIL if there aren't any child nodes </summary>
    function LastChild: TXmlNode; virtual;
    ///	<summary> Returns next sibling </summary>
    function NextSibling: TXmlNode; overload; virtual;
    ///	<summary> Returns previous sibling </summary>
    function PreviousSibling: TXmlNode; overload; virtual;
    ///	<summary> Returns True if the node has at least one child node </summary>
    function HasChildNodes: Boolean; virtual;
    ///	<summary> Returns True if the node has a text content and no child nodes </summary>
    function IsTextElement: Boolean; virtual;
    ///	<summary> Fluent interface for setting the node type </summary>
    function SetNodeType(Value: TXmlNodeType): TXmlNode; virtual;
    ///	<summary> Attributes of a node, accessible by attribute name (case insensitive) </summary>
    property Attributes[const AttrName: String]: String read GetAttr write SetAttr;
    ///	<summary> The xml document of the node </summary>
    property Document: TXmlVerySimple read FDocument write SetDocument;
    ///	<summary> The node name, same as property Name </summary>
    property NodeName: String read Name write Name;
    ///	<summary> The node text, same as property Text </summary>
    property NodeValue: String read Text write Text;
  end;

  TXmlNodeList = class(TObjectList<TXmlNode>)
  protected
    function IsSame(const Value1, Value2: String): Boolean; virtual;
  public
    ///	<summary> The xml document of the node list </summary>
    [Weak] Document: TXmlVerySimple;
    ///	<summary> The parent node of the node list </summary>
    [Weak] Parent: TXmlNode;
    ///	<summary> Adds a node and sets the parent of the node to the parent of the list </summary>
    function Add(Value: TXmlNode): Integer; overload; virtual;
    ///	<summary> Creates a new node of type NodeType (default ntElement) and adds it to the list </summary>
    function Add(NodeType: TXmlNodeType = ntElement): TXmlNode; overload; virtual;
    ///	<summary> Add a child node with an optional NodeType (default: ntElement)</summary>
    function Add(const Name: String; NodeType: TXmlNodeType = ntElement): TXmlNode; overload; virtual;
    ///	<summary> Find a node by its name (case sensitive), returns NIL if no node is found </summary>
    function Find(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode; overload; virtual;
    ///	<summary> Same as Find(), returnsa a node by its name (case sensitive) </summary>
    function FindNode(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode; virtual;
    ///	<summary> Find a node that has the the given attribute, returns NIL if no node is found </summary>
    function Find(const Name, AttrName: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode; overload; virtual;
    ///	<summary> Find a node that as the given attribute name and value, returns NIL otherwise </summary>
    function Find(const Name, AttrName, AttrValue: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode; overload; virtual;
    ///	<summary> Return a list of child nodes with the given name and (optional) node types </summary>
    function FindNodes(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNodeList; virtual;
    ///	<summary> Returns True if the list contains a node with the given name </summary>
    function HasNode(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): Boolean; virtual;
    ///	<summary> Inserts a node at the given position </summary>
    function Insert(const Name: String; Position: Integer; NodeType: TXmlNodeType = ntElement): TXmlNode; overload; virtual;
    ///	<summary> Returns the first child node, same as .First </summary>
    function FirstChild: TXmlNode; virtual;
    ///	<summary> Returns next sibling node </summary>
    function NextSibling(Node: TXmlNode): TXmlNode; virtual;
    ///	<summary> Returns previous sibling node </summary>
    function PreviousSibling(Node: TXmlNode): TXmlNode; virtual;
    ///	<summary> Returns the node at the given position </summary>
    function Get(Index: Integer): TXmlNode; virtual;
  end;

  TXmlVerySimple = class(TObject)
  protected
    FRoot: TXmlNode;
    [Weak] FHeader: TXmlNode;
    [Weak] FDocumentElement: TXmlNode;
    SkipIndent: Boolean;
    procedure Parse(Reader: TXmlStreamReader); virtual;
    procedure ParseComment(Reader: TXmlStreamReader; var Parent: TXmlNode); virtual;
    procedure ParseDocType(Reader: TXmlStreamReader; var Parent: TXmlNode); virtual;
    procedure ParseProcessingInstr(Reader: TXmlStreamReader; var Parent: TXmlNode); virtual;
    procedure ParseCData(Reader: TXmlStreamReader; var Parent: TXmlNode); virtual;
    procedure ParseText(const Line: String; Parent: TXmlNode; ReplaceText: Boolean = False); virtual;
    function ParseTag(Reader: TXmlStreamReader; FindText: Boolean; var Parent: TXmlNode): TXmlNode; overload; virtual;
    function ParseTag(const TagStr: String; var Parent: TXmlNode): TXmlNode; overload; virtual;
    procedure Walk(Writer: TStreamWriter; const PrefixNode: String; Node: TXmlNode); virtual;
    procedure SetText(const Value: String); virtual;
    function GetText: String; virtual;
    procedure SetEncoding(const Value: String); virtual;
    function GetEncoding: String; virtual;
    procedure SetVersion(const Value: String); virtual;
    function GetVersion: String; virtual;
    procedure Compose(Writer: TStreamWriter); virtual;
    procedure SetStandAlone(const Value: String); virtual;
    function GetStandAlone: String; virtual;
    function GetChildNodes: TXmlNodeList; virtual;
    procedure CreateHeaderNode; virtual;
    function ExtractText(var Line: String; const StopChars: String; Options: TExtractTextOptions): String; virtual;
    procedure SetDocumentElement(Value: TXMlNode); virtual;
    procedure SetPreserveWhitespace(Value: Boolean);
    function GetPreserveWhitespace: Boolean;
    function IsSame(const Value1, Value2: String): Boolean;
  public
    ///	<summary> Indent used for the xml output </summary>
    NodeIndentStr: String;
    ///	<summary> LineBreak used for the xml output, default set to sLineBreak which is OS dependent </summary>
    LineBreak: String;
    ///	<summary> Options for xml output like indentation type </summary>
    Options: TXmlOptions;
    ///	<summary> Creates a new XML document parser </summary>
    constructor Create; virtual;
    ///	<summary> Destroys the XML document parser </summary>
    destructor Destroy; override;
    ///	<summary> Deletes all nodes </summary>
    procedure Clear; virtual;
    ///	<summary> Adds a new node to the document, if it's the first ntElement then sets it as .DocumentElement </summary>
    function AddChild(const Name: String; NodeType: TXmlNodeType = ntElement): TXmlNode; virtual;
    ///	<summary> Creates a new node but doesn't adds it to the document nodes </summary>
    function CreateNode(const Name: String; NodeType: TXmlNodeType = ntElement): TXmlNode; virtual;
    /// <summary> Escapes XML control characters </summar>
    class function Escape(const Value: String): String; virtual;
    /// <summary> Translates escaped characters back into XML control characters </summar>
    class function Unescape(const Value: String): String; virtual;
    ///	<summary> Loads the XML from a file </summary>
    function LoadFromFile(const FileName: String; BufferSize: Integer = 4096): TXmlVerySimple; virtual;
    ///	<summary> Loads the XML from a stream </summary>
    function LoadFromStream(const Stream: TStream; BufferSize: Integer = 4096): TXmlVerySimple; virtual;
    ///	<summary> Parse attributes into the attribute list for a given string </summary>
    procedure ParseAttributes(const AttribStr: String; AttributeList: TXmlAttributeList); virtual;
    ///	<summary> Saves the XML to a file </summary>
    function SaveToFile(const FileName: String): TXmlVerySimple; virtual;
    ///	<summary> Saves the XML to a stream, the encoding is specified in the .Encoding property </summary>
    function SaveToStream(const Stream: TStream): TXmlVerySimple; virtual;
    ///	<summary> A list of all root nodes of the document </summary>
    property ChildNodes: TXmlNodeList read GetChildNodes;
    ///	<summary> Returns the first element node </summary>
    property DocumentElement: TXmlNode read FDocumentElement write SetDocumentElement;
    ///	<summary> Specifies the encoding of the XML file, anything else then 'utf-8' is considered as ANSI </summary>
    property Encoding: String read GetEncoding write SetEncoding;
    ///	<summary> XML declarations are stored in here as Attributes </summary>
    property Header: TXmlNode read FHeader;
    ///	<summary> Set to True if all spaces and linebreaks should be included as a text node, same as doPreserve option </summary>
    property PreserveWhitespace: Boolean read GetPreserveWhitespace write SetPreserveWhitespace;
    ///	<summary> The root node of the document</summary>
    property Root: TXmlNode read FRoot;
    ///	<summary> Defines the xml declaration property "StandAlone", set it to "yes" or "no" </summary>
    property StandAlone: String read GetStandAlone write SetStandAlone;
    ///	<summary> The XML as a string representation </summary>
    property Text: String read GetText write SetText;
    ///	<summary> Defines the xml declaration property "Version", default set to "1.0" </summary>
    property Version: String read GetVersion write SetVersion;
    ///	<summary> The XML as a string representation, same as .Text </summary>
    property Xml: String read GetText write SetText;
  end;

implementation

uses
  System.StrUtils;

{ TVerySimpleXml }

function TXmlVerySimple.AddChild(const Name: String; NodeType: TXmlNodeType = ntElement): TXmlNode;
begin
  Result := CreateNode(Name, NodeType);
  if (NodeType = ntElement) and (not Assigned(FDocumentElement)) then
    FDocumentElement := Result;
  try
    FRoot.ChildNodes.Add(Result);
  except
    Result.Free;
    raise;
  end;
  Result.Document := Self;
end;

procedure TXmlVerySimple.Clear;
begin
  FDocumentElement := NIL;
  FHeader := NIL;
  FRoot.Clear;
end;

constructor TXmlVerySimple.Create;
begin
  inherited;
  FRoot := TXmlNode.Create;
  FRoot.NodeType := ntDocument;
  FRoot.Parent := FRoot;
  FRoot.Document := Self;
  NodeIndentStr := '  ';
  Options := [doNodeAutoIndent, doWriteBOM, doSimplifyTextNodes];
  LineBreak := sLineBreak;
  CreateHeaderNode;
end;

procedure TXmlVerySimple.CreateHeaderNode;
begin
  if Assigned(FHeader) then
    Exit;
  FHeader := FRoot.ChildNodes.Insert('xml', 0, ntXmlDecl);
  FHeader.Attributes['version'] := '1.0';  // Default XML version
  FHeader.Attributes['encoding'] := 'utf-8';
end;

function TXmlVerySimple.CreateNode(const Name: String; NodeType: TXmlNodeType): TXmlNode;
begin
  Result := TXmlNode.Create(NodeType);
  Result.Name := Name;
  Result.Document := Self;
end;

destructor TXmlVerySimple.Destroy;
begin
  FRoot.Parent := NIL;
  FRoot.Clear;
  FRoot.Free;
  inherited;
end;

function TXmlVerySimple.GetChildNodes: TXmlNodeList;
begin
  Result := FRoot.ChildNodes;
end;

function TXmlVerySimple.GetEncoding: String;
begin
  if Assigned(FHeader) then
    Result := FHeader.Attributes['encoding']
  else
    Result := '';
end;

function TXmlVerySimple.GetPreserveWhitespace: Boolean;
begin
  Result := doPreserveWhitespace in Options;
end;

function TXmlVerySimple.GetStandAlone: String;
begin
  if Assigned(FHeader) then
    Result := FHeader.Attributes['standalone']
  else
    Result := '';
end;

function TXmlVerySimple.GetVersion: String;
begin
  if Assigned(FHeader) then
    Result := FHeader.Attributes['version']
  else
    Result := '';
end;

function TXmlVerySimple.IsSame(const Value1, Value2: String): Boolean;
begin
  if doCaseInsensitive in Options then
    Result := AnsiSameText(Value1, Value2)
  else
    Result := (Value1 = Value2);
end;

function TXmlVerySimple.GetText: String;
var
  Stream: TStringStream;
begin
  if AnsiSameText(Encoding, 'utf-8') then
    Stream := TStringStream.Create('', TEncoding.UTF8)
  else
    Stream := TStringStream.Create('', TEncoding.ANSI);
  try
    SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TXmlVerySimple.Compose(Writer: TStreamWriter);
var
  Child: TXmlNode;
begin
  if doCompact in Options then
  begin
    Writer.NewLine := '';
    LineBreak := '';
  end
  else
    Writer.NewLine := LineBreak;

  SkipIndent := False;
  for Child in FRoot.ChildNodes do
    Walk(Writer, '', Child);
end;

function TXmlVerySimple.LoadFromFile(const FileName: String; BufferSize: Integer = 4096): TXmlVerySimple;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead + fmShareDenyWrite);
  try
    LoadFromStream(Stream, BufferSize);
  finally
    Stream.Free;
  end;
  Result := Self;
end;

function TXmlVerySimple.LoadFromStream(const Stream: TStream; BufferSize: Integer = 4096): TXmlVerySimple;
var
  Reader: TXmlStreamReader;
begin
  if Encoding.IsEmpty then // none specified then use UTF8 with DetectBom
    Reader := TXmlStreamReader.Create(Stream, TEncoding.UTF8, True, BufferSize)
  else
  if AnsiSameText(Encoding, 'utf-8') then
    Reader := TXmlStreamReader.Create(Stream, TEncoding.UTF8, False, BufferSize)
  else
    Reader := TXmlStreamReader.Create(Stream, TEncoding.ANSI, False, BufferSize);
  try
    Parse(Reader);
  finally
    Reader.Free;
  end;
  Result := Self;
end;

procedure TXmlVerySimple.Parse(Reader: TXmlStreamReader);
var
  Parent, Node: TXmlNode;
  FirstChar: String;
  ALine: String;
begin
  Clear;
  Parent := FRoot;

  while not Reader.EndOfStream do
  begin
    ALine := Reader.ReadText('<', [etoDeleteStopChar]);
    if not ALine.IsEmpty then  // Check for text nodes
    begin
      ParseText(Aline, Parent);
      if Reader.EndOfStream then  // if no chars available then exit
        Break;
    end;
    FirstChar := Reader.FirstChar;
    if FirstChar = '!' then
      if Reader.IsUppercaseText('!--') then  // check for a comment node
        ParseComment(Reader, Parent)
      else
      if Reader.IsUppercaseText('!DOCTYPE') then // check for a doctype node
        ParseDocType(Reader, Parent)
      else
      if Reader.IsUppercaseText('![CDATA[') then // check for a cdata node
        ParseCData(Reader, Parent)
      else
        ParseTag(Reader, False, Parent) // try to parse as tag
    else // Check for XML header / processing instructions
    if FirstChar = '?' then // could be header or processing instruction
      ParseProcessingInstr(Reader, Parent)
    else
    if not FirstChar.IsEmpty then
    begin // Parse a tag, the first tag in a document is the DocumentElement
      Node := ParseTag(Reader, True, Parent);
      if (not Assigned(FDocumentElement)) and (Parent = FRoot) then
        FDocumentElement := Node;
    end;
  end;
end;

procedure TXmlVerySimple.ParseAttributes(const AttribStr: String; AttributeList: TXmlAttributeList);
var
  Attribute: TXmlAttribute;
  AttrName, AttrText: String;
  Quote: String;
  Value: String;
begin
  Value := TrimLeft(AttribStr);
  while not Value.IsEmpty do
  begin
    AttrName := ExtractText(Value, ' =', []);
    Value := TrimLeft(Value);

    Attribute := AttributeList.Add(AttrName);
    if (Value.IsEmpty) or (Value[1] <> '=') then
      Continue;

    Delete(Value, 1, 1);
    Attribute.AttributeType := atValue;
    ExtractText(Value, '''' + '"', []);
    Value := TrimLeft(Value);
    if not Value.IsEmpty then
    begin
      Quote := Value[1];
      Delete(Value, 1, 1);
      AttrText := ExtractText(Value, Quote, [etoDeleteStopChar]); // Get Attribute Value
      Attribute.Value := Unescape(AttrText);
      Value := TrimLeft(Value);
    end;
  end;
end;


procedure TXmlVerySimple.ParseText(const Line: String; Parent: TXmlNode; ReplaceText: Boolean = False);
var
  SingleChar: Char;
  Node: TXmlNode;
  TextNode: Boolean;
begin
  if PreserveWhiteSpace then
    TextNode := True
  else
  begin
    TextNode := False;
    for SingleChar in Line do
      if not Assigned(AnsiStrScan(TXmlSpaces, SingleChar)) then
      begin
        TextNode := True;
        Break;
      end;
  end;

  if TextNode then
    if ReplaceText then
      Parent.Text := Line
    else
    begin
      Node := Parent.ChildNodes.Add(ntText);
      Node.Text := Line;
    end;
end;

procedure TXmlVerySimple.ParseCData(Reader: TXmlStreamReader; var Parent: TXmlNode);
var
  Node: TXmlNode;
begin
  Node := Parent.ChildNodes.Add(ntCData);
  Node.Text := Reader.ReadText(']]>', [etoDeleteStopChar, etoStopString]);
end;

procedure TXmlVerySimple.ParseComment(Reader: TXmlStreamReader; var Parent: TXmlNode);
var
  Node: TXmlNode;
begin
  Node := Parent.ChildNodes.Add(ntComment);
  Node.Text := Reader.ReadText('-->', [etoDeleteStopChar, etoStopString]);
end;

procedure TXmlVerySimple.ParseDocType(Reader: TXmlStreamReader; var Parent: TXmlNode);
var
  Node: TXmlNode;
  Quote: String;
begin
  Node := Parent.ChildNodes.Add(ntDocType);
  Node.Text := Reader.ReadText('>[', []);
  if not Reader.EndOfStream then
  begin
    Quote := Reader.FirstChar;
    Reader.IncCharPos;
    if Quote = '[' then
      Node.Text := Node.Text + Quote + Reader.ReadText(']',[etoDeleteStopChar]) + ']' +
        Reader.ReadText('>', [etoDeleteStopChar]);
  end;
end;

procedure TXmlVerySimple.ParseProcessingInstr(Reader: TXmlStreamReader; var Parent: TXmlNode);
var
  Node: TXmlNode;
  Tag: String;
begin
  Reader.IncCharPos; // omit the '?'
  Tag := Reader.ReadText('?>', [etoDeleteStopChar, etoStopString]);
  Node := ParseTag(Tag, Parent);
  if lowercase(Node.Name) = 'xml' then
  begin
    FHeader := Node;
    FHeader.NodeType := ntXmlDecl;
  end
  else
  begin
    Node.NodeType := ntProcessingInstr;
    if not (doParseProcessingInstr in Options) then
    begin
      Node.Text := Tag;
      Node.AttributeList.Clear;
    end;
  end;
  Parent := Node.Parent;
end;

function TXmlVerySimple.ParseTag(Reader: TXmlStreamReader; FindText: Boolean; var Parent: TXmlNode): TXmlNode;
var
  Tag: String;
  ALine: String;
begin
  Tag := Reader.ReadText('>', [etoDeleteStopChar]);
  Result := ParseTag(Tag, Parent);
  if (Result = Parent) and (FindText) then // only non-self closing nodes may have a text
  begin
    ALine := Reader.ReadText('<', []);
    ALine := Unescape(ALine);

    // if a node consists of text only then replace text, else parse as separate text node
    if not Aline.IsEmpty then
      ParseText(ALine, Result, doSimplifyTextNodes in Options);
  end;
end;

function TXmlVerySimple.ParseTag(const TagStr: String; var Parent: TXmlNode): TXmlNode;
var
  Node: TXmlNode;
  ALine: String;
  CharPos: Integer;
  Tag: String;
begin
  // A closing tag does not have any attributes nor text
  if (not TagStr.IsEmpty) and (TagStr[1] = '/') then
  begin
    Result := Parent;
    Parent := Parent.Parent;
    Exit;
  end;

  // Creat a new new ntElement node
  Node := Parent.ChildNodes.Add;
  Result := Node;
  Tag := TagStr;

  // Check for a self-closing Tag (does not have any text)
  if (not Tag.IsEmpty) and (Tag[High(Tag)] = '/') then
    Delete(Tag, Length(Tag), 1)
  else
    Parent := Node;

  CharPos := Pos(' ', Tag);
  if CharPos <> 0 then // Tag may have attributes
  begin
    ALine := Tag;
    Delete(Tag, CharPos, Length(Tag));
    Delete(ALine, 1, CharPos);
    if not ALine.IsEmpty then
      ParseAttributes(ALine, Node.AttributeList);
  end;

  Node.Name := Tag;
end;


function TXmlVerySimple.SaveToFile(const FileName: String): TXmlVerySimple;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
  Result := Self;
end;

function TXmlVerySimple.SaveToStream(const Stream: TStream): TXmlVerySimple;
var
  Writer: TStreamWriter;
begin
  if AnsiSameText(Encoding, 'utf-8') then
    if doWriteBOM in Options then
      Writer := TStreamWriter.Create(Stream, TEncoding.UTF8)
    else
      Writer := TStreamWriter.Create(Stream)
  else
    Writer := TStreamWriter.Create(Stream, TEncoding.ANSI);
  try
    Compose(Writer);
  finally
    Writer.Free;
  end;
  Result := Self;
end;

procedure TXmlVerySimple.SetDocumentElement(Value: TXMlNode);
begin
  FDocumentElement := Value;
  if not Assigned(Value.Parent) then
    FRoot.ChildNodes.Add(Value);
end;

procedure TXmlVerySimple.SetEncoding(const Value: String);
begin
  CreateHeaderNode;
  FHeader.Attributes['encoding'] := Value;
end;

procedure TXmlVerySimple.SetPreserveWhitespace(Value: Boolean);
begin
  if Value then
    Options := Options + [doPreserveWhitespace]
  else
    Options := Options - [doPreserveWhitespace]
end;

procedure TXmlVerySimple.SetStandAlone(const Value: String);
begin
  CreateHeaderNode;
  FHeader.Attributes['standalone'] := Value;
end;

procedure TXmlVerySimple.SetVersion(const Value: String);
begin
  CreateHeaderNode;
  FHeader.Attributes['version'] := Value;
end;


class function TXmlVerySimple.Unescape(const Value: String): String;
begin
  Result := ReplaceStr(Value, '&lt;', '<');
  Result := ReplaceStr(Result, '&gt;', '>');
  Result := ReplaceStr(Result, '&quot;', '"');
  Result := ReplaceStr(Result, '&apos;', '''');
  Result := ReplaceStr(Result, '&amp;', '&');
end;

procedure TXmlVerySimple.SetText(const Value: String);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('', TEncoding.UTF8);
  try
    Stream.WriteString(Value);
    Stream.Position := 0;
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TXmlVerySimple.Walk(Writer: TStreamWriter; const PrefixNode: String; Node: TXmlNode);
var
  Child: TXmlNode;
  Line: String;
  Indent: String;
begin
  if (Node = FRoot.ChildNodes.First) or (SkipIndent) then
  begin
    Line := '<';
    SkipIndent := False;
  end
  else
    Line := LineBreak + PrefixNode + '<';

  case Node.NodeType of
    ntComment:
      begin
        Writer.Write(Line + '!--' + Node.Text + '-->');
        Exit;
      end;
    ntDocType:
      begin
        Writer.Write(Line + '!DOCTYPE ' + Node.Text + '>');
        Exit;
      end;
    ntCData:
      begin
        Writer.Write('<![CDATA[' + Node.Text + ']]>');
        Exit;
      end;
    ntText:
      begin
        Writer.Write(Node.Text);
        SkipIndent := True;
        Exit;
      end;
    ntProcessingInstr:
      begin
        if Node.AttributeList.Count > 0 then
          Writer.Write(Line + '?' + Node.Name + Node.AttributeList.AsString + '?>')
        else
          Writer.Write(Line + '?' + Node.Text + '?>');
        Exit;
      end;
    ntXmlDecl:
      begin
        Writer.Write(Line + '?' + Node.Name + Node.AttributeList.AsString + '?>');
        Exit;
      end;
  end;

  Line := Line + Node.Name + Node.AttributeList.AsString;

  // Self closing tags
  if (Node.Text.IsEmpty) and (not Node.HasChildNodes) then
   begin
    Writer.Write(Line + '/>');
    Exit;
  end;

  Line := Line + '>';
  if not Node.Text.IsEmpty then
  begin
    Line := Line + Escape(Node.Text);
    if Node.HasChildNodes then
      SkipIndent := True;
  end;

  Writer.Write(Line);

  // Set indent for child nodes
  if doCompact in Options then
    Indent := ''
  else
    Indent := PrefixNode + NodeIndentStr;

  // Process child nodes
  for Child in Node.ChildNodes do
    Walk(Writer, Indent, Child);

  // If node has child nodes and last child node is not a text node then set indent for closing tag
  if (Node.HasChildNodes) and (not SkipIndent) then
    Indent := LineBreak + PrefixNode
  else
    Indent := '';

  Writer.Write(Indent + '</' + Node.Name + '>');
end;


class function TXmlVerySimple.Escape(const Value: String): String;
begin
  Result := TXmlAttribute.Escape(Value);
  Result := ReplaceStr(Result, '''', '&apos;');
end;

function TXmlVerySimple.ExtractText(var Line: String; const StopChars: String;
  Options: TExtractTextOptions): String;
var
  CharPos, FoundPos: Integer;
  TestChar: Char;
begin
  FoundPos := 0;
  for TestChar in StopChars do
  begin
    CharPos := Pos(TestChar, Line);
    if (CharPos <> 0) and ((FoundPos = 0) or (CharPos < FoundPos)) then
      FoundPos := CharPos;
  end;

  if FoundPos <> 0 then
  begin
    Dec(FoundPos);
    Result := Copy(Line, 1, FoundPos);
    if etoDeleteStopChar in Options then
      Inc(FoundPos);
    Delete(Line, 1, FoundPos);
  end
  else
  begin
    Result := Line;
    Line := '';
  end;
end;

{ TXmlNode }

function TXmlNode.AddChild(const AName: String; ANodeType: TXmlNodeType = ntElement): TXmlNode;
begin
  Result := ChildNodes.Add(AName, ANodeType);
end;

procedure TXmlNode.Clear;
begin
  Text := '';
  AttributeList.Clear;
  ChildNodes.Clear;
end;

constructor TXmlNode.Create(ANodeType: TXmlNodeType = ntElement);
begin
  ChildNodes := TXmlNodeList.Create;
  ChildNodes.Parent := Self;
  AttributeList := TXmlAttributeList.Create;
  NodeType := ANodeType;
end;

destructor TXmlNode.Destroy;
begin
  Clear;
  ChildNodes.Free;
  AttributeList.Free;
  inherited;
end;

function TXmlNode.Find(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode;
begin
  Result := ChildNodes.Find(Name, NodeTypes);
end;

function TXmlNode.Find(const Name, AttrName, AttrValue: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode;
begin
  Result := ChildNodes.Find(Name, AttrName, AttrValue, NodeTypes);
end;

function TXmlNode.Find(const Name, AttrName: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode;
begin
  Result := ChildNodes.Find(Name, AttrName, NodeTypes);
end;

function TXmlNode.FindNodes(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNodeList;
begin
  Result := ChildNodes.FindNodes(Name, NodeTypes);
end;

function TXmlNode.FirstChild: TXmlNode;
begin
  Result := ChildNodes.First;
end;

function TXmlNode.GetAttr(const AttrName: String): String;
var
  Attribute: TXmlAttribute;
begin
  Attribute := AttributeList.Find(AttrName);
  if Assigned(Attribute) then
    Result := Attribute.Value
  else
    Result := '';
end;

function TXmlNode.HasAttribute(const AttrName: String): Boolean;
begin
  Result := AttributeList.HasAttribute(AttrName);
end;

function TXmlNode.HasChild(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): Boolean;
begin
  Result := ChildNodes.HasNode(Name, NodeTypes);
end;

function TXmlNode.HasChildNodes: Boolean;
begin
  Result := (ChildNodes.Count > 0);
end;

function TXmlNode.InsertChild(const Name: String; Position: Integer; NodeType: TXmlNodeType = ntElement): TXmlNode;
begin
  Result := ChildNodes.Insert(Name, Position, NodeType);
  if Assigned(Result) then
    Result.Parent := Self;
end;

function TXmlNode.IsTextElement: Boolean;
begin
  Result := (not Text.IsEmpty) and (not HasChildNodes);
end;

function TXmlNode.LastChild: TXmlNode;
begin
  if ChildNodes.Count > 0 then
    Result := ChildNodes.Last
  else
    Result := NIL;
end;

function TXmlNode.NextSibling: TXmlNode;
begin
  if not Assigned(Parent) then
    Result := NIL
  else
    Result := Parent.ChildNodes.NextSibling(Self);
end;

function TXmlNode.PreviousSibling: TXmlNode;
begin
  if not Assigned(Parent) then
    Result := NIL
  else
    Result := Parent.ChildNodes.PreviousSibling(Self);
end;

function TXmlNode.SelectNode(const NodePath: String): TXmlNode;
var
  Elements: TArray<String>;
  Element: String;
  SubNode, Node: TXmlNode;
begin
  Result := NIL;

  // SplitElements by '/' delimiter
  Elements := NodePath.Split(['/']);
  if not Assigned(Elements) then
    Exit;

  // Start from the root if the path is prefixed with '/'
  if Elements[0].IsEmpty then
  begin
    Node := FDocument.Root;
    Delete(Elements, 0, 1);
  end
  else
    Node := Self;

  // Traverse all elements
  SubNode := NIL;
  for Element in Elements do
  begin
    if Element.IsEmpty then
      Continue;

    SubNode := Node.Find(Element, []);
    if not Assigned(SubNode) then
      Break;

    Node := SubNode;
  end;

  Result := SubNode;
end;

procedure TXmlNode.SetAttr(const AttrName, AttrValue: String);
begin
  SetAttribute(AttrName, AttrValue);
end;

function TXmlNode.SetAttribute(const AttrName, AttrValue: String): TXmlNode;
var
  Attribute: TXmlAttribute;
begin
  Attribute := AttributeList.Find(AttrName); // Search for given name
  if not Assigned(Attribute) then // If attribute is not found, create one
    Attribute := AttributeList.Add(AttrName);
  Attribute.AttributeType := atValue;
  Attribute.Name := AttrName; // this allows rewriting of the attribute name (lower/upper case)
  Attribute.Value := AttrValue;
  Result := Self;
end;

procedure TXmlNode.SetDocument(Value: TXmlVerySimple);
begin
  FDocument := Value;
  AttributeList.Document := Value;
  ChildNodes.Document := Value;
end;

function TXmlNode.SetNodeType(Value: TXmlNodeType): TXmlNode;
begin
  NodeType := Value;
  Result := Self;
end;

function TXmlNode.SetText(const Value: String): TXmlNode;
begin
  Text := Value;
  Result := Self;
end;

{ TXmlAttributeList }

function TXmlAttributeList.Add(const Name: String): TXmlAttribute;
begin
  Result := TXmlAttribute.Create;
  Result.Name := Name;
  try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TXmlAttributeList.Assign(Source: TXmlAttributeList);
var
  Attribute: TXmlAttribute;
  SourceAttribute: TXmlAttribute;
begin
  Clear;
  for SourceAttribute in Source do
  begin
    Attribute := Add('');
    Attribute.Assign(SourceAttribute);
  end;
end;

function TXmlAttributeList.AsString: String;
var
  Attribute: TXmlAttribute;
begin
  Result := '';
  for Attribute in Self do
    Result := Result + ' ' + Attribute.AsString;
end;

procedure TXmlAttributeList.Delete(const Name: String);
var
  Attribute: TXmlAttribute;
begin
  Attribute := Find(Name);
  if Assigned(Attribute) then
    Remove(Attribute);
end;

function TXmlAttributeList.Find(const Name: String): TXmlAttribute;
var
  Attribute: TXmlAttribute;
begin
  Result := NIL;
  for Attribute in Self do
    if ((Assigned(Document) and Document.IsSame(Attribute.Name, Name)) or // use the documents text comparison
      ((not Assigned(Document)) and (Attribute.Name = Name))) then // or if not assigned then compare names case sensitive
    begin
      Result := Attribute;
      Break;
    end;
end;

function TXmlAttributeList.HasAttribute(const AttrName: String): Boolean;
begin
  Result := Assigned(Find(AttrName));
end;

{ TXmlNodeList }

function TXmlNodeList.Find(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  for Node in Self do
    if ((NodeTypes = []) or (Node.NodeType in NodeTypes)) and (IsSame(Node.Name, Name)) then
    begin
      Result := Node;
      Break;
    end;

end;

function TXmlNodeList.Add(Value: TXmlNode): Integer;
begin
  Result := inherited Add(Value);
  Value.Parent := Parent;
end;

function TXmlNodeList.Add(NodeType: TXmlNodeType = ntElement): TXmlNode;
begin
  Result := TXmlNode.Create(NodeType);
  try
    Add(Result);
  except
    Result.Free;
    raise;
  end;
  Result.Document := Document;
end;

function TXmlNodeList.Add(const Name: String; NodeType: TXmlNodeType): TXmlNode;
begin
  Result := Add(NodeType);
  Result.Name := Name;
end;

function TXmlNodeList.Find(const Name, AttrName, AttrValue: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  for Node in Self do
    if ((NodeTypes = []) or (Node.NodeType in NodeTypes)) and // if no type specified or node type in types
      IsSame(Node.Name, Name) and Node.HasAttribute(AttrName) and IsSame(Node.Attributes[AttrName], AttrValue) then
    begin
      Result := Node;
      Break;
    end;
end;

function TXmlNodeList.Find(const Name, AttrName: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode;
var
  Node: TXmlNode;
begin
  Result := NIL;
  for Node in Self do
    if ((NodeTypes = []) or (Node.NodeType in NodeTypes)) and IsSame(Node.Name, Name) and
      Node.HasAttribute(AttrName) then
    begin
      Result := Node;
      Break;
    end;
end;


function TXmlNodeList.FindNode(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNode;
begin
  Result := Find(Name, NodeTypes);
end;

function TXmlNodeList.FindNodes(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): TXmlNodeList;
var
  Node: TXmlNode;
begin
  Result := TXmlNodeList.Create(False);
  Result.Document := Document;
  try
    for Node in Self do
      if ((NodeTypes = []) or (Node.NodeType in NodeTypes)) and IsSame(Node.Name, Name) then
        begin
          Result.Parent := Node.Parent;
          Result.Add(Node);
        end;
    Result.Parent := NIL;
  except
    Result.Free;
    raise;
  end;
end;

function TXmlNodeList.FirstChild: TXmlNode;
begin
  Result := First;
end;


function TXmlNodeList.Get(Index: Integer): TXmlNode;
begin
  Result := Items[Index];
end;

function TXmlNodeList.HasNode(const Name: String; NodeTypes: TXmlNodeTypes = [ntElement]): Boolean;
begin
  Result := Assigned(Find(Name, NodeTypes));
end;

function TXmlNodeList.Insert(const Name: String; Position: Integer; NodeType: TXmlNodeType = ntElement): TXmlNode;
begin
  Result := TXmlNode.Create;
  Result.Document := Document;
  try
    Result.Name := Name;
    Result.NodeType := NodeType;
    Insert(Position, Result);
  except
    Result.Free;
    raise;
  end;
end;

function TXmlNodeList.IsSame(const Value1, Value2: String): Boolean;
begin
  Result := ((Assigned(Document) and Document.IsSame(Value1, Value2)) or // use the documents text comparison
    ((not Assigned(Document)) and (Value1 = Value2))); // or if not assigned then compare names case sensitive
end;

function TXmlNodeList.NextSibling(Node: TXmlNode): TXmlNode;
var
  Index: Integer;
begin
  if (not Assigned(Node)) and (Count > 0) then
    Result := First
  else
  begin
    Index := IndexOf(Node);
    if (Index >= 0) and (Index + 1 < Count) then
      Result := Self[Index + 1]
    else
      Result := NIL;
  end;
end;

function TXmlNodeList.PreviousSibling(Node: TXmlNode): TXmlNode;
var
  Index: Integer;
begin
  Index := IndexOf(Node);
  if Index - 1 >= 0 then
    Result := Self[Index - 1]
  else
    Result := NIL;
end;

{ TXmlAttribute }

procedure TXmlAttribute.Assign(Source: TXmlAttribute);
begin
  FValue := Source.Value;
  Name := Source.Name;
  AttributeType := Source.AttributeType;
end;

function TXmlAttribute.AsString: String;
begin
  Result := Name;
  if AttributeType = atSingle then
    Exit;
  Result := Result + '="' + Escape(Value) + '"';
end;

constructor TXmlAttribute.Create;
begin
  AttributeType := atSingle;
end;

class function TXmlAttribute.Escape(const Value: String): String;
begin
  Result := ReplaceStr(Value, '&', '&amp;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
  Result := ReplaceStr(Result, '"', '&quot;');
end;

procedure TXmlAttribute.SetValue(const Value: String);
begin
  FValue := Value;
  AttributeType := atValue;
end;

{ TXmlStreamReader }

procedure TXmlStreamReader.FillBuffer;
var
  TempEncoding: TEncoding;
begin
  TempEncoding := CurrentEncoding;
  FillBuffer(TempEncoding);
  if TempEncoding <> CurrentEncoding then
    TRttiContext.Create.GetType(TStreamReader).GetField('FEncoding').SetValue(Self, TempEncoding)
end;

function TXmlStreamReader.FirstChar: String;
begin
  if PrepareBuffer(1) then
    Result := FBufferedData.Chars[0]
  else
    Result := '';
end;

procedure TXmlStreamReader.IncCharPos(Value: Integer);
begin
  if PrepareBuffer(Value) then
    FBufferedData.Remove(0, Value);
end;

function TXmlStreamReader.IsUppercaseText(const Value: String): Boolean;
var
  ValueLength: Integer;
  Text: String;
begin
  Result := False;
  ValueLength := Length(Value);

  if PrepareBuffer(ValueLength) then
  begin
    Text := FBufferedData.ToString(0, ValueLength);
    if Text = Value then
    begin
      FBufferedData.Remove(0, ValueLength);
      Result := True;
    end;
  end;
end;

function TXmlStreamReader.PrepareBuffer(Value: Integer): Boolean;
begin
  Result := False;

  if not Assigned(FBufferedData) then
    Exit;

  if (FBufferedData.Length < Value) and (not FNoDataInStream) then
    FillBuffer;

  Result := (FBufferedData.Length >= Value);
end;

function TXmlStreamReader.ReadText(const StopChars: String; Options: TExtractTextOptions): String;
var
  NewLineIndex: Integer;
  PostNewLineIndex: Integer;
  StopChar: Char;
  Found: Boolean;
  TempIndex: Integer;
  StopCharLength: Integer;
  PrevLength: Integer;
begin
  Result := '';
  if not Assigned(FBufferedData) then
    Exit;
  NewLineIndex := 0;
  PostNewLineIndex := 0;
  StopCharLength := Length(StopChars);

  while True do
  begin
    // if we're searching for a string then assure the buffer is wide enough
    if (etoStopString in Options) and (NewLineIndex + StopCharLength > FBufferedData.Length) and
      (not FNoDataInStream) then
      FillBuffer;

    if NewLineIndex >= FBufferedData.Length then
    begin
      if FNoDataInStream then
      begin
        PostNewLineIndex := NewLineIndex;
        Break;
      end
      else
      begin
        PrevLength := FBufferedData.Length;
        FillBuffer;
        // Break if no more data
        if (FBufferedData.Length = 0) or (FBufferedData.Length = PrevLength) then
          Break;
      end;
    end;

    if etoStopString in Options then
    begin
      if NewLineIndex + StopCharLength - 1 < FBufferedData.Length then
      begin
        Found := True;
        TempIndex := NewLineIndex;
        for StopChar in StopChars do
          if FBufferedData[TempIndex] <> StopChar then
          begin
            Found := False;
            Break;
          end
          else
            Inc(TempIndex);

        if Found then
        begin
          if etoDeleteStopChar in Options then
            PostNewLineIndex := NewLineIndex + StopCharLength
          else
            PostNewLineIndex := NewLineIndex;
          Break;
        end;
      end;
    end
    else
    begin
      Found := False;
      for StopChar in StopChars do
        if FBufferedData[NewLineIndex] = StopChar then
        begin
            if etoDeleteStopChar in Options then
              PostNewLineIndex := NewLineIndex + 1
            else
              PostNewLineIndex := NewLineIndex;
          Found := True;
          Break;
        end;
      if Found then
        Break;
    end;

    Inc(NewLineIndex);
  end;

  if NewLineIndex > 0 then
    Result := FBufferedData.ToString(0, NewLineIndex);
  FBufferedData.Remove(0, PostNewLineIndex);
end;

end.
