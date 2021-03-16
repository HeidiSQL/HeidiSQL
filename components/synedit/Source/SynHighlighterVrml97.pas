{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterVrml.pas, released 2002-10-21.
The Original Code is based on: SynHighlighterJScript.pas, released 2000-04-14.
The Original Code is based on the mwJScript.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Tony de Buys.
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

$Id: SynHighlighterVrml97.pas,v 1.6.2.8 2008/09/14 16:25:03 maelh Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides a Vrml97/X3D/JavaScript highlighter for SynEdit)
@author(Massimo Maria Ghisalberti (nissl@mammuth.it)
@created(november 2002 [December 1999, converted to SynEdit April 14, 2000])
@lastmod(2002-11-03)
The SynHighlighterVrml97 unit provides SynEdit with a Vrml97/X3D/JavaScript (.wrl;*.x3d) highlighter.
The highlighter formats Vrml97/X3D source code highlighting keywords, strings, numbers and characters.
}

{ TODO: The Ansi version kept unclear to the status of following tokens:

  Token       Ambiguity
  =====       =========
  bottom      tkVrmlAttribute or tkNonReservedKey
  description tkVrmlAttribute or tkNonReservedKey
  height      tkVrmlAttribute or tkNonReservedKey
  location    tkVrmlAttribute or tkNonReservedKey
  style       tkVrmlAttribute or tkNonReservedKey
  type        tkVrmlAttribute or tkNonReservedKey

  NULL        tkVrmlParameter or tkVrmlProto
  FALSE       tkVrmlParameter or tkVrmlProto
  
  Text        tkVrmlShape or tkNonReservedKey

  I took always the first one as this produces the same results as in the
  Ansi-version, because the other cases were never reached (due to the way
  the if construct was used).
}

unit SynHighlighterVrml97;

{$I SynEdit.inc}

interface

uses
  Windows,
  Messages,
  Registry,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKey,
    tkNull,
    tkNumber,
    tkSpace,
    tkString,
    tkSymbol,
    tkUnknown,
    tkNonReservedKey,
    tkEvent,
    tkVrmlAppearance,
    tkVrmlAttribute,
    tkVrmlDefinition,
    tkVrmlEvent,
    tkVrmlGrouping,
    tkVrmlInterpolator,
    tkVrmlLight,
    tkVrmlNode,
    tkVrmlParameter,
    tkVrmlproto,
    tkVrmlSensor,
    tkVrmlShape,
    tkVrmlShape_Hint,
    tkVrmlTime_dependent,
    tkVrmlViewpoint,
    tkVrmlWorldInfo,
    tkX3DDocType,
    tkX3DHeader);

  TRangeState = (rsNormalText, rsComment, rsX3DHeader, rsX3DDocType);

type
  TSynVrml97Syn = class(TSynCustomHighLighter)
  private
    FRange: TRangeState;
    FIsDoctype: Boolean;
    FTokenID: TtkTokenKind;
    FCommentAttri: TSynHighlighterAttributes;
    FIdentifierAttri: TSynHighlighterAttributes;
    FKeyAttri: TSynHighlighterAttributes;
    FNonReservedKeyAttri: TSynHighlighterAttributes;
    FEventAttri: TSynHighlighterAttributes;
    FNumberAttri: TSynHighlighterAttributes;
    FSpaceAttri: TSynHighlighterAttributes;
    FStringAttri: TSynHighlighterAttributes;
    FSymbolAttri: TSynHighlighterAttributes;

    FVrmlAppearanceAttri: TSynHighlighterAttributes;
    FVrmlAttributeAttri: TSynHighlighterAttributes;
    FVrmlDefinitionAttri: TSynHighlighterAttributes;
    FVrmlEventAttri: TSynHighlighterAttributes;
    FVrmlGroupingAttri: TSynHighlighterAttributes;
    FVrmlInterpolatorAttri: TSynHighlighterAttributes;
    FVrmlLightAttri: TSynHighlighterAttributes;
    FVrmlNodeAttri: TSynHighlighterAttributes;
    FVrmlParameterAttri: TSynHighlighterAttributes;
    FVrmlprotoAttri: TSynHighlighterAttributes;
    FVrmlSensorAttri: TSynHighlighterAttributes;
    FVrmlShapeAttri: TSynHighlighterAttributes;
    FVrmlShape_HintAttri: TSynHighlighterAttributes;
    FVrmlTime_dependentAttri: TSynHighlighterAttributes;
    FVrmlViewpointAttri: TSynHighlighterAttributes;
    FVrmlWorldInfoAttri: TSynHighlighterAttributes;
    FX3DDocTypeAttri: TSynHighlighterAttributes;
    FX3DHeaderAttri: TSynHighlighterAttributes;

    FKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
    function HashKey(Str: PWideChar): Integer;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure AndSymbolProc;
    procedure CommentProc;
    procedure DiesisCommentProc;
    procedure X3DDocTypeOpenProc;
    procedure X3DDocTypeProc;
    procedure X3DHeaderOpenProc;
    procedure X3DHeaderProc;
    procedure InCommentProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function NextTokenIs(T: UnicodeString): Boolean;
  protected
    function GetSampleSource: UnicodeString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: UnicodeString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property NonReservedKeyAttri: TSynHighlighterAttributes read FNonReservedKeyAttri write FNonReservedKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read FNumberAttri write FNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read FSpaceAttri write FSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read FStringAttri write FStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read FSymbolAttri write FSymbolAttri;
    property CommentAttri: TSynHighlighterAttributes read FCommentAttri write FCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read FIdentifierAttri write FIdentifierAttri;
    property EcmaScriptKeyAttri: TSynHighlighterAttributes read FKeyAttri write FKeyAttri;
    property EcmaScriptEventAttri: TSynHighlighterAttributes read FEventAttri write FEventAttri;

    property VrmlAppearanceAttri: TSynHighlighterAttributes read FVrmlAppearanceAttri write FVrmlAppearanceAttri;
    property VrmlAttributeAttri: TSynHighlighterAttributes read FVrmlAttributeAttri write FVrmlAttributeAttri;
    property VrmlDefinitionAttri: TSynHighlighterAttributes read FVrmlDefinitionAttri write FVrmlDefinitionAttri;
    property VrmlEventAttri: TSynHighlighterAttributes read FVrmlEventAttri write FVrmlEventAttri;
    property VrmlGroupingAttri: TSynHighlighterAttributes read FVrmlGroupingAttri write FVrmlGroupingAttri;
    property VrmlInterpolatorAttri: TSynHighlighterAttributes read FVrmlInterpolatorAttri write FVrmlInterpolatorAttri;
    property VrmlLightAttri: TSynHighlighterAttributes read FVrmlLightAttri write FVrmlLightAttri;
    property VrmlNodeAttri: TSynHighlighterAttributes read FVrmlNodeAttri write FVrmlNodeAttri;
    property VrmlParameterAttri: TSynHighlighterAttributes read FVrmlParameterAttri write FVrmlParameterAttri;
    property VrmlprotoAttri: TSynHighlighterAttributes read FVrmlprotoAttri write FVrmlprotoAttri;
    property VrmlSensorAttri: TSynHighlighterAttributes read FVrmlSensorAttri write FVrmlSensorAttri;
    property VrmlShapeAttri: TSynHighlighterAttributes read FVrmlShapeAttri write FVrmlShapeAttri;
    property VrmlShape_HintAttri: TSynHighlighterAttributes read FVrmlShape_HintAttri write FVrmlShape_HintAttri;
    property VrmlTime_dependentAttri: TSynHighlighterAttributes read FVrmlTime_dependentAttri write FVrmlTime_dependentAttri;
    property VrmlViewpointAttri: TSynHighlighterAttributes read FVrmlViewpointAttri write FVrmlViewpointAttri;
    property VrmlWorldInfoAttri: TSynHighlighterAttributes read FVrmlWorldInfoAttri write FVrmlWorldInfoAttri;
    property X3DDocTypeAttri: TSynHighlighterAttributes read FX3DDocTypeAttri write FX3DDocTypeAttri;
    property X3DHeaderAttri: TSynHighlighterAttributes read FX3DHeaderAttri write FX3DHeaderAttri;
  end;

implementation

uses
  SynEditStrConst;

const
  Events: UnicodeString =
    'onAbort, onBlur, onChange, onClick, onDblClick, onError, onFocus, ' +
    'onKeyDown, onKeyPress, onKeyUp, onLoad, onMouseDown, onMouseMove, ' +
    'onMouseOut, onMouseOver, onMouseUp, onReset, onSelect, onSubmit, ' +
    'onUnload';

  KeyWords: UnicodeString =
    'abstract, boolean, break, byte, callee, case, catch, char, class, ' +
    'const, constructor, continue, debugger, default, delete, do, DOCTYPE, ' +
    'double, else, enum, export, extends, false, final, finally, float, for, ' +
    'function, goto, head, if, implements, import, in, instanceof, int, ' +
    'interface, long, meta, NaN, native, new, null, package, private, ' +
    'protected, prototype, public, PUBLIC, return, short, start, static, ' +
    'super, switch, synchronized, this, throw, throws, transient, true, try, ' +
    'typeof, var, void, while, with, xml';

  NonReservedKeys: UnicodeString =
    'abs, acos, action, alert, align, alinkColor, all, All, anchor, anchors, ' +
    'appCodeName, Applet, applets, appName, appVersion, Area, arguments, ' +
    'Arguments, Array, asin, atan, atan2, back, background, bgColor, big, ' +
    'blink, blur, body, bold, Boolean, border, Button, call, caller, ' +
    'captureEvents, ceil, charAt, charCodeAt, Checkbox, checked, clear, ' +
    'clearInterval, clearTimeout, click, close, closed, complete, concat, ' +
    'confirm, content, cookie, cos, current, Date, defaultChecked, ' +
    'defaultSelected, defaultStatus, defaultValue, display, document, ' +
    'domain, E, elements, Embed, embeds, enabledPlugin, encoding, escape, ' +
    'eval, event, exp, fgColor, filename, FileUpload, find, fixed, Float, ' +
    'floor, focus, fontcolor, fontsize, form, Form, forms, forward, Frame, ' +
    'frames, fromCharCode, Function, getDate, getDay, getElementById, ' +
    'getFullYear, getHours, getMilliseconds, getMinutes, getMonth, ' +
    'getSeconds, getTime, getTimezoneOffset, getUTCDate, getUTCDay, ' +
    'getUTCFullYear, getUTCHours, getUTCMilliseconds, getUTCMinutes, ' +
    'getUTCMonth, getUTCSeconds, getYear, Global, go, handleEvent, hash, ' +
    'Hidden, history, History, home, host, hostname, href, hspace, Image, ' +
    'images, index, indexOf, Infinity, innerHeight, innerWidth, input, ' +
    'isFinite, isNaN, italics, java, javaEnabled, join, lastIndexOf, ' +
    'lastModified, Layer, layers, left, link, Link, linkColor, links, LN10, ' +
    'LN2, Location, locationbar, log, LOG10E, LOG2E, logon, lowsrc, match, ' +
    'Math, max, MAX_VALUE, menubar, method, MimeType, mimeTypes, min, ' +
    'MIN_VALUE, moveBy, moveTo, name, navigator, Navigator, ' +
    'NEGATIVE_INFINITY, netscape, next, Null, Number, Object, open, opener, ' +
    'Option, options, outerHeight, outerWidth, Packages, pageX, pageXOffset, ' +
    'pageY, pageYOffset, parent, parse, parseFloat, parseInt, Password, ' +
    'pathname, personalbar, PI, platform, Plugin, plugins, port, ' +
    'POSITIVE_INFINITY, pow, previous, print, prompt, protocol, Radio, ' +
    'random, referrer, refresh, RegExp, releaseEvents, reload, replace, ' +
    'reset, Reset, resizeBy, resizeTo, reverse, right, round, routeEvent, ' +
    'screen, scroll, scrollbars, scrollBy, scrollTo, search, select, Select, ' +
    'selected, selectedIndex, self, setDate, setFullYear, setHours, ' +
    'setInterval, setMilliseconds, setMinutes, setMonth, setSeconds, ' +
    'setTime, setTimeout, setUTCDate, setUTCFullYear, setUTCHours, ' +
    'setUTCMilliseconds, setUTCMinutes, setUTCMonth, setUTCSeconds, setYear, ' +
    'sin, slice, small, sort, split, sqrt, SQRT1_2, SQRT2, src, status, ' +
    'statusbar, stop, strike, String, sub, submit, Submit, substr, ' +
    'substring, suffixes, sup, tags, taint, taintEnabled, tan, target, text, ' +
    'Textarea, title, toGMTString, toLocaleString, toLowerCase, toolbar, ' +
    'toSource, toString, toUpperCase, toUTCString, undefined, Undefined, ' +
    'unescape, untaint, unwatch, URL, userAgent, UTC, value, valueOf, ' +
    'version, visibility, vlinkColor, vspace, watch, width, window, Window, ' +
    'write, writeln, zIndex';

  VrmlAppearances: UnicodeString =
    'Appearance, ImageTexture, Material, NurbsTextureSurface, PixelTexture, ' +
    'TextureBackground, TextureCoordinate, TextureCoordinateGenerator, ' +
    'TextureTransform';

  VrmlAttributes: UnicodeString =
    'addChildren, ambientIntensity, appearance, attenuation, autoOffset, ' +
    'avatarSize, axisOfRotation, backUrl, bboxCenter, bboxSize, beamWidth, ' +
    'beginCap, bindTime, bottom, bottomRadius, bottomUrl, ccw, center, children, ' +
    'choice, collide, collideTime, color, colorIndex, colorPerVertex, ' +
    'ColorRGBA, convex, coord, coordIndex, creaseAngle, crossSection, ' +
    'cutOffAngle, cycleInterval, cycleTime, description, diffuseColor, direction, ' +
    'directOutput, diskAngle, duration_changed, emissiveColor, enabled, ' +
    'endCap, enterTime, eventName, eventType, exitTime, family, fieldName, ' +
    'fieldOfView, fieldType, FillProperties, fogType, fontStyle, ' +
    'fraction_changed, frontUrl, GeoCoordinate, GeoElevationGrid, ' +
    'GeoLocation, GeoLOD, GeoMetadata, geometry, GeoOrigin, groundAngle, ' +
    'groundColor, headlight, height, hitNormal_changed, hitPoint_changed, ' +
    'hitTexCoord_changed, horizontal, image, info, intensity, isActive, ' +
    'isBound, isOver, jump, justify, key, keyValue, language, leftToRight, ' +
    'leftUrl, length, level, LineProperties, location, loop, material, maxAngle, ' +
    'maxBack, maxExtent, maxFront, maxPosition, minAngle, minBack, minFront, ' +
    'minPosition, MultiTexture, MultiTextureCoordinate, mustEvaluate, ' +
    'normal, normalIndex, normalPerVertex, offset, on, orientation, ' +
    'orientation_changed, parameter, pitch, point, position, ' +
    'position_changed, priority, proxy, radius, range, removeChildren, ' +
    'repeatS, repeatT, rightUrl, rotation, rotation_changed, scale, ' +
    'scaleOrientation, set_bind, set_colorIndex, set_coordIndex, ' +
    'set_crossSection, set_fraction, set_height, set_normalIndex, ' +
    'set_orientation, set_scale, set_spine, set_texCoordIndex, shininess, ' +
    'side, size, skyAngle, skyColor, solid, source, spacing, spatialize, ' +
    'specularColor, speed, spine, startTime, stopTime, string, style, texCoord, ' +
    'texCoordIndex, texture, textureTransform, time, top, topToBottom, ' +
    'topUrl, touchTime, trackPoint_changed, translation, ' +
    'translation_changed, transparency, type, url, value_changed, vector, ' +
    'visibilityLimit, visibilityRange, whichChoice, xDimension, xSpacing, ' +
    'zDimension, zSpacing';

  VrmlDefinitions: UnicodeString =
    'MFColor, MFFloat, MFInt32, MFNode, MFRotation, MFString, MFTime, ' +
    'MFVec2f, MFVec3f, SFBool, SFColor, SFFloat, SFImage, SFInt32, SFNode, ' +
    'SFRotation, SFString, SFTime, SFVec2f, SFVec3f';

  VrmlEvents: UnicodeString =
    'eventIn, eventOut, exposedField, field';

  VrmlGroupings: UnicodeString =
    'Anchor, Billboard, Collision, ESPDUTransform, Group, Inline, LOD, ' +
    'NurbsGroup, ReceiverPdu, SignalPdu, StaticGroup, Switch, Transform, ' +
    'Transform2D, TransmitterPdu';

  VrmlInterpolators: UnicodeString =
    'ColorInterpolator, CoordinateInterpolator, CoordinateInterpolator2D, ' +
    'GeoPositionInterpolator, NormalInterpolator, NurbsPositionInterpolator, ' +
    'OrientationInterpolator, PositionInterpolator, PositionInterpolator2D, ' +
    'ScalarInterpolator';

  VrmlLights: UnicodeString =
    'DirectionalLight, PointLight, SpotLight';

  VrmlNodes: UnicodeString =
    'Background, Color, Coordinate, CoordinateDeformer, Fog, FontStyle, ' +
    'Joint, NavigationInfo, Normal, Script, Site, Sound';

  VrmlParameters: UnicodeString =
    'ALL, AUTO, BINDINGS, BOLD, BOTTOM, CENTER, CLAMP, CLOCKWISE, CONVEX, ' +
    'COUNTERCLOCKWISE, CULLING, DEFAULT, DEFAULTS, Displacer, ENUMS, FACE, FALSE, ' +
    'FAMILY, FILE, FORMAT, ITALIC, JUSTIFICATION, LEFT, NONE, NULL, OFF, ON, ' +
    'OVERALL, PARTS, PER_FACE, PER_FACE_INDEXED, PER_PART, PER_PART_INDEXED, ' +
    'PER_VERTEX, PER_VERTEX_INDEXED, REPEAT, RIGHT, SHAPE, SIDES, SOLID, ' +
    'STYLE, TRUE, TYPE, UNKNOWN_FACE_TYPE, UNKNOWN_ORDERING, ' +
    'UNKNOWN_SHAPE_TYPE, WRAP';

  VrmlProtos: UnicodeString =
    'DEF, EXTERNPROTO, IS, PROTO, ROUTE, Scene, TO, USE, VRML, X3D, ' +
    'X3DAppearanceNode, X3DAppearanceChildNode, X3DBackgroundNode, X3DBindableNode, ' +
    'X3DBoundedObject, X3DChildNode, X3DColorNode, X3DComposedGeometryNode, ' +
    'X3DCoordinateNode, X3DDragSensorNode, X3DEnvironmentalSensorNode, ' +
    'X3DFontStyleNode, X3DGeometry2DNode, X3DGeometry3DNode, ' +
    'X3DGeometryNode, X3DGeometryPropertyNode, X3DGroupingNode, ' +
    'X3DInterpolatorNode, X3DKeyDeviceSensorNode, X3DLightNode, ' +
    'X3DMaterialNode, X3DNetworkSensorNode, X3DNode, X3DNormalNode, ' +
    'X3DParametricGeometryNode, X3DPointingDeviceSensorNode, ' +
    'X3DPrototypeInstance, X3DScriptNode, X3DSensorNode, X3DSequencerNode, ' +
    'X3DShapeNode, X3DSoundNode, X3DSoundSourceNode, X3DTexture2DNode, ' +
    'X3DTextureCoordinateNode, X3DTextureNode, X3DTextureTransform2DNode, ' +
    'X3DTextureTransformNode, X3DTimeDependentNode, X3DTouchSensorNode, ' +
    'X3DTriggerNode, X3DUrlObject';

  VrmlSensors: UnicodeString =
    'BooleanFilter, BooleanSequencer, BooleanToggle, BooleanTrigger, ' +
    'CylinderSensor, GeoTouchSensor, IntegerTrigger, KeySensor, LoadSensor, ' +
    'PlaneSensor, ProximitySensor, SphereSensor, StringSensor, TimeSensor, ' +
    'TouchSensor, VisibilitySensor';

  VrmlShapes: UnicodeString =
    'Arc2D, ArcClose2D, Box, Circle2D, Cone, Contour2D, ContourPolyline2D, ' +
    'Cylinder, Disk2D, ElevationGrid, Humanoid, NurbsCurve, NurbsCurve2D, ' +
    'NurbsSurface, PointSet, Polyline2D, Polypoint2D, Rectangle2D, Segment, ' +
    'Shape, Shape2D, Sphere, Text, TriangleFanSet, TriangleSet, TriangleSet2D, ' +
    'TriangleStripSet, TrimmedSurface';

  VrmlShape_Hints: UnicodeString =
    'Extrusion, IndexedFaceSet, IndexedLineSet';

  VrmlTime_dependents: UnicodeString =
    'AudioClip, IntegerSequencer, MovieTexture, TimeTrigger';

  VrmlViewpoints: UnicodeString =
    'GeoViewpoint, Viewpoint';

  VrmlWorldInfos: UnicodeString =
    'WorldInfo';


procedure TSynVrml97Syn.DoAddKeyword(AKeyword: UnicodeString; AKind: Integer);
var
  HashValue: Integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  FKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
end;

function TSynVrml97Syn.HashKey(Str: PWideChar): Integer;

  function GetOrd: Integer;
  begin
    case Str^ of
      'a'..'z': Result := 1 + Ord(Str^) - Ord('a');
      'A'..'Z': Result := 27 + Ord(Str^) - Ord('A');
      '0'..'9': Result := 54 + Ord(Str^) - Ord('0');
      '_': Result := 53;
      else Result := 0;
    end
  end;

begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
{$IFOPT Q-}
    Result := 7 * Result + GetOrd;
{$ELSE}
    Result := (7 * Result + GetOrd) and $FFFFFF;
{$ENDIF}
    Inc(Str);
  end;
  Result := Result and $FF; // 255
  FStringLen := Str - FToIdent;
end;

function TSynVrml97Syn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  FToIdent := MayBe;
  Entry := FKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > FStringLen then
      Break
    else if Entry.KeywordLen = FStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        Exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSynVrml97Syn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCaseSensitive := True;

  FKeywords := TSynHashEntryList.Create;
  FIsDoctype := False;
  FCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  FCommentAttri.Style := [fsItalic];
  FCommentAttri.Foreground := clNavy;
  FCommentAttri.Background := clGray;
  AddAttribute(FCommentAttri);

  FIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  FIdentifierAttri.Style := [];
  FIdentifierAttri.Foreground := clNavy;
  FIdentifierAttri.Background := clWhite;
  AddAttribute(FIdentifierAttri);

  FKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  FKeyAttri.Style := [fsBold];
  FKeyAttri.Foreground := clRed;
  FKeyAttri.Background := clWhite;
  AddAttribute(FKeyAttri);

  FNonReservedKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  FNonReservedKeyAttri.Style := [fsItalic];
  FNonReservedKeyAttri.Foreground := clBlack;
  FNonReservedKeyAttri.Background := clWhite;
  AddAttribute(FNonReservedKeyAttri);

  FEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrEvent, SYNS_FriendlyAttrEvent);
  FEventAttri.Style := [fsItalic];
  FEventAttri.Foreground := clNavy;
  FEventAttri.Background := clWhite;
  AddAttribute(FEventAttri);

  FNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  FEventAttri.Style := [fsItalic];
  FEventAttri.Foreground := clNavy;
  FEventAttri.Background := clWhite;
  AddAttribute(FNumberAttri);

  FSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  FSpaceAttri.Style := [fsItalic];
  FSpaceAttri.Foreground := clNavy;
  AddAttribute(FSpaceAttri);

  FStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  FStringAttri.Style := [fsItalic];
  FStringAttri.Foreground := clNavy;
  FStringAttri.Background := clWhite;
  AddAttribute(FStringAttri);

  FSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  FSymbolAttri.Style := [fsItalic];
  FSymbolAttri.Foreground := clNavy;
  FSymbolAttri.Background := clWhite;
  AddAttribute(FSymbolAttri);
  //-- vrml
  FVrmlAppearanceAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlAppearance, SYNS_FriendlyAttrVrmlAppearance);
  FVrmlAppearanceAttri.Style := [fsItalic];
  FVrmlAppearanceAttri.Foreground := clNavy;
  FVrmlAppearanceAttri.Background := clWhite;
  AddAttribute(FVrmlAppearanceAttri);

  FVrmlAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlAttribute, SYNS_FriendlyAttrVrmlAttribute);
  FVrmlAttributeAttri.Style := [fsItalic];
  FVrmlAttributeAttri.Foreground := clNavy;
  FVrmlAttributeAttri.Background := clGray;
  AddAttribute(FVrmlAttributeAttri);

  FVrmlDefinitionAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlDefinition, SYNS_FriendlyAttrVrmlDefinition);
  FVrmlDefinitionAttri.Style := [fsItalic];
  FVrmlDefinitionAttri.Foreground := clNavy;
  FVrmlDefinitionAttri.Background := clRed;
  AddAttribute(FVrmlDefinitionAttri);

  FVrmlEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlEvent, SYNS_FriendlyAttrVrmlEvent);
  FVrmlEventAttri.Style := [fsBold];
  FVrmlEventAttri.Foreground := clRed;
  FVrmlEventAttri.Background := clWhite;
  AddAttribute(FVrmlEventAttri);

  FVrmlGroupingAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlGrouping, SYNS_FriendlyAttrVrmlGrouping);
  FVrmlGroupingAttri.Style := [fsBold];
  FVrmlGroupingAttri.Foreground := clNavy;
  FVrmlGroupingAttri.Background := clWhite;
  AddAttribute(FVrmlGroupingAttri);

  FVrmlInterpolatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlInterpolator, SYNS_FriendlyAttrVrmlInterpolator);
  FVrmlInterpolatorAttri.Style := [fsItalic];
  FVrmlInterpolatorAttri.Foreground := clLime;
  FVrmlInterpolatorAttri.Background := clWhite;
  AddAttribute(FVrmlInterpolatorAttri);

  FVrmlLightAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlLight, SYNS_FriendlyAttrVrmlLight);
  FVrmlLightAttri.Style := [fsItalic];
  FVrmlLightAttri.Foreground := clTeal;
  FVrmlLightAttri.Background := clWhite;
  AddAttribute(FVrmlLightAttri);

  FVrmlNodeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlNode, SYNS_FriendlyAttrVrmlNode);
  FVrmlNodeAttri.Style := [fsItalic, fsBold];
  FVrmlNodeAttri.Foreground := clGreen;
  FVrmlNodeAttri.Background := clWhite;
  AddAttribute(FVrmlNodeAttri);

  FVrmlParameterAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlParameter, SYNS_FriendlyAttrVrmlParameter);
  FVrmlParameterAttri.Style := [fsBold];
  FVrmlParameterAttri.Foreground := $F0CAA6; //clSkyBlue
  FVrmlParameterAttri.Background := clWhite;
  AddAttribute(FVrmlParameterAttri);

  FVrmlprotoAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlProto, SYNS_FriendlyAttrVrmlProto);
  FVrmlprotoAttri.Style := [fsBold];
  FVrmlprotoAttri.Foreground := clRed;
  FVrmlprotoAttri.Background := clWhite;
  AddAttribute(FVrmlprotoAttri);

  FVrmlSensorAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlSensor, SYNS_FriendlyAttrVrmlSensor);
  FVrmlSensorAttri.Style := [fsBold];
  FVrmlSensorAttri.Foreground := clOlive;
  FVrmlSensorAttri.Background := clWhite;
  AddAttribute(FVrmlSensorAttri);

  FVrmlShapeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlShape, SYNS_FriendlyAttrVrmlShape);
  FVrmlShapeAttri.Style := [fsBold];
  FVrmlShapeAttri.Foreground := clPurple;
  FVrmlShapeAttri.Background := clWhite;
  AddAttribute(FVrmlShapeAttri);

  FVrmlShape_HintAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlShape_Hint, SYNS_FriendlyAttrVrmlShape_Hint);
  FVrmlShape_HintAttri.Style := [fsItalic];
  FVrmlShape_HintAttri.Foreground := clPurple;
  FVrmlShape_HintAttri.Background := clWhite;
  AddAttribute(FVrmlShape_HintAttri);

  FVrmlTime_dependentAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlTime_dependent, SYNS_FriendlyAttrVrmlTime_dependent);
  FVrmlTime_dependentAttri.Style := [fsItalic];
  FVrmlTime_dependentAttri.Foreground := clOlive;
  FVrmlTime_dependentAttri.Background := clWhite;
  AddAttribute(FVrmlTime_dependentAttri);

  FVrmlViewpointAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlViewpoint, SYNS_FriendlyAttrVrmlViewpoint);
  FVrmlViewpointAttri.Style := [fsItalic];
  FVrmlViewpointAttri.Foreground := clGreen;
  FVrmlViewpointAttri.Background := clWhite;
  AddAttribute(FVrmlViewpointAttri);

  FVrmlWorldInfoAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlWorldInfo, SYNS_FriendlyAttrVrmlWorldInfo);
  FVrmlWorldInfoAttri.Style := [fsItalic];
  FVrmlWorldInfoAttri.Foreground := clMaroon;
  FVrmlWorldInfoAttri.Background := clWhite;
  AddAttribute(FVrmlWorldInfoAttri);

  FX3DDocTypeAttri := TSynHighLighterAttributes.Create(SYNS_AttrX3DDocType, SYNS_FriendlyAttrX3DDocType);
  FX3DDocTypeAttri.Style := [fsItalic];
  FX3DDocTypeAttri.Foreground := clMaroon;
  FX3DDocTypeAttri.Background := clWhite;
  AddAttribute(FX3DDocTypeAttri);

  FX3DHeaderAttri := TSynHighLighterAttributes.Create(SYNS_AttrX3DHeader, SYNS_FriendlyAttrX3DHeader);
  FX3DHeaderAttri.Style := [fsItalic];
  FX3DHeaderAttri.Foreground := clMaroon;
  FX3DHeaderAttri.Background := clWhite;
  AddAttribute(FX3DHeaderAttri);
  SetAttributesOnChange(DefHighlightChange);

  EnumerateKeywords(Ord(tkEvent), Events, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkKey), KeyWords, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkNonReservedKey), NonReservedKeys, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlAppearance), VrmlAppearances, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlAttribute), VrmlAttributes, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlDefinition), VrmlDefinitions, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlEvent), VrmlEvents, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlGrouping), VrmlGroupings, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlInterpolator), VrmlInterpolators, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlLight), VrmlLights, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlNode), VrmlNodes, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlParameter), VrmlParameters, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlproto), VrmlProtos, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlSensor), VrmlSensors, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlShape), VrmlShapes, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlShape_Hint), VrmlShape_Hints, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlTime_dependent), VrmlTime_dependents, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlViewpoint), VrmlViewpoints, IsIdentChar, DoAddKeyword);
  EnumerateKeywords(Ord(tkVrmlWorldInfo), VrmlWorldInfos, IsIdentChar, DoAddKeyword);

  FDefaultFilter := SYNS_FilterVrml97;
  FRange := rsNormalText;
end;

destructor TSynVrml97Syn.Destroy;
begin
  FKeywords.Free;
  inherited Destroy;
end;

procedure TSynVrml97Syn.AndSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '&']) then Inc(Run);
end;

function TSynVrml97Syn.NextTokenIs(T: UnicodeString): Boolean;
var
  I, Len: Integer;
begin
  Result := True;
  Len := Length(T);
  for I := 1 to Len do
    if (FLine[Run + I] <> T[I]) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TSynVrml97Syn.InCommentProc;
begin
  if (FLine[Run + 1] = '-') and (FLine[Run + 2] = '-') then
    begin
      Inc(Run);
      FTokenID := tkComment;
      FRange := rsComment;
      Inc(Run, 2);
      repeat
        Inc(Run);
        if (FLine[Run] = '-') and (FLine[Run + 1] = '-') then
          begin
            FRange := rsNormalText;
            Inc(Run, 2);
            Break;
          end;
      until IsLineEnd(Run);
      Exit;
    end;
end;

procedure TSynVrml97Syn.DiesisCommentProc;
begin
  if FLine[Run] = #0 then
    NullProc
  else
    begin
      FTokenID := tkComment;
      repeat
        Inc(Run);
      until IsLineEnd(Run);
    end;
end;

procedure TSynVrml97Syn.X3DHeaderOpenProc;
begin
  Inc(Run);
  FRange := rsX3DHeader;
  X3DHeaderProc;
  FTokenID := tkX3DHeader;
end;

procedure TSynVrml97Syn.X3DHeaderProc;
begin
  case FLine[Run] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
      begin
        FTokenID := tkX3DHeader;
        repeat
          if (FLine[Run] = '?') then
            begin
              Inc(Run, 1);
              FRange := rsNormalText;
              Break;
            end;
          if not IsLineEnd(Run) then
            Inc(Run);
        until IsLineEnd(Run);
      end;
  end;
end;

procedure TSynVrml97Syn.X3DDocTypeOpenProc;
begin
  if NextTokenIs('DOCTYPE') then
    begin
      FRange := rsX3DDocType;
      X3DDocTypeProc;
      FTokenID := tkX3DDocType;
    end
  else
    if NextTokenIs('--') then
      begin
        FRange := rsComment;
        InCommentProc;
        FTokenID := tkComment;
      end
    else
    begin
      FTokenID := tkSymbol;
      Inc(Run);
    end;
end;

procedure TSynVrml97Syn.X3DDocTypeProc;
begin
  case FLine[Run] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
      begin
        FTokenID := tkX3DDocType;
        repeat
          if (FLine[Run + 1] = '>') then
            begin
              Inc(Run, 1);
              FRange := rsNormalText;
              Break;
            end;
          if not IsLineEnd(Run) then
            Inc(Run);
        until IsLineEnd(Run);
      end;
  end;
end;

procedure TSynVrml97Syn.CommentProc;
begin
  if FLine[Run] = #0 then
    NullProc
  else
    begin
      FTokenID := tkComment;
      repeat
        if ((FLine[Run] = '*') and (FLine[Run + 1] = '/'))
          or
          ((FLine[Run] = '-') and (FLine[Run + 1] = '-')) then
          begin
            FRange := rsNormalText;
            Inc(Run, 2);
            Break;
          end;
        Inc(Run);
      until IsLineEnd(Run);
    end;
end;

procedure TSynVrml97Syn.CRProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
  if FLine[Run] = #10 then Inc(Run);
end;

procedure TSynVrml97Syn.IdentProc;
begin
  FTokenID := IdentKind(FLine + Run);
  Inc(Run, FStringLen);
  while IsIdentChar(FLine[Run]) do
    Inc(Run);
end;

procedure TSynVrml97Syn.LFProc;
begin
  FTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynVrml97Syn.MinusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '-', '>']) then Inc(Run);
end;

procedure TSynVrml97Syn.ModSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynVrml97Syn.NullProc;
begin
  FTokenID := tkNull;
  Inc(Run);
end;

procedure TSynVrml97Syn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case FLine[Run] of
      '0'..'9', '.', 'a'..'f', 'A'..'F', 'x', 'X':
        Result := True;
      else
        Result := False;
    end;
  end;

var
  idx1: Integer; // token[1]
  isHex: Boolean;
begin
  FTokenID := tkNumber;
  isHex := False;
  idx1 := Run;
  Inc(Run);
  while IsNumberChar do
  begin
    case FLine[Run] of
      '.' :
        if FLine[Succ(Run)] = '.' then
          Break;
      'a'..'f', 'A'..'F' :
        if not isHex then
          Break;
      'x', 'X' :
        begin
          if (FLine[idx1] <> '0') or (Run > Succ(idx1)) then
            Break;
          if not CharInSet(FLine[Succ(Run)], ['0'..'9', 'a'..'f', 'A'..'F']) then
          begin
            Break;
          end;
          isHex := True;
        end;
    end;
    Inc(Run);
  end;
end;

procedure TSynVrml97Syn.OrSymbolProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '|']) then Inc(Run);
end;

procedure TSynVrml97Syn.PlusProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if CharInSet(FLine[Run], ['=', '+']) then Inc(Run);
end;

procedure TSynVrml97Syn.PointProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if (FLine[Run] = '.') and (FLine[Run + 1] = '.') then Inc(Run, 2);
end;

procedure TSynVrml97Syn.SlashProc;
begin
  Inc(Run);
  case FLine[Run] of
    '/' :
      begin
        FTokenID := tkComment;
        repeat
          Inc(Run);
        until IsLineEnd(Run);
      end;
    '*' :
      begin
        FTokenID := tkComment;
        FRange := rsComment;
        repeat
          Inc(Run);
          if (FLine[Run] = '*') and (FLine[Run + 1] = '/') then
            begin
              FRange := rsNormalText;
              Inc(Run, 2);
              Break;
            end;
        until IsLineEnd(Run);
      end;
    '=' :
      begin
        Inc(Run);
        FTokenID := tkSymbol;
      end;
    else
      FTokenID := tkSymbol;
  end;
end;

procedure TSynVrml97Syn.SpaceProc;
begin
  Inc(Run);
  FTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do Inc(Run);
end;

procedure TSynVrml97Syn.StarProc;
begin
  FTokenID := tkSymbol;
  Inc(Run);
  if FLine[Run] = '=' then Inc(Run);
end;

procedure TSynVrml97Syn.StringProc;
var
  l_strChar: UnicodeString;
begin
  FTokenID := tkString;
  l_strChar := FLine[Run]; // We could have '"' or #39
  if (FLine[Run + 1] = l_strChar) and (FLine[Run + 2] = l_strChar) then Inc(Run, 2);
  repeat
    if IsLineEnd(Run) then
      Break;
    Inc(Run);
  until (FLine[Run] = l_strChar) and (FLine[Pred(Run)] <> '\');
  if not IsLineEnd(Run) then
    Inc(Run);
end;

procedure TSynVrml97Syn.SymbolProc;
begin
  Inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynVrml97Syn.UnknownProc;
begin
  Inc(Run);
  FTokenID := tkUnknown;
end;

procedure TSynVrml97Syn.Next;
begin
  FTokenPos := Run;
  case FRange of
    rsX3DHeader: X3DHeaderProc;
    rsX3DDocType: X3DDocTypeProc;
    rsComment: CommentProc;
    else
      case FLine[Run] of
        '&': AndSymbolProc;
        #13: CRProc;
        '#': DiesisCommentProc;
        'A'..'Z', 'a'..'z', '_': IdentProc;
        #10: LFProc;
        '-': MinusProc;
        '%': ModSymbolProc;
        #0: NullProc;
        '0'..'9': NumberProc;
        '|': OrSymbolProc;
        '+': PlusProc;
        '.': PointProc;
        '/': SlashProc;
        #1..#9, #11, #12, #14..#32: SpaceProc;
        '*': StarProc;
        '"', #39: StringProc;
        '?': X3DHeaderOpenProc;
        '!': X3DDocTypeOpenProc;
        '~', '{', '}', ',', '(', ')', '[', ']', ':', ';', '=', '<', '>': SymbolProc;
        else UnknownProc;
      end;
  end;
  inherited;
end;

function TSynVrml97Syn.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := FCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := FIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := FKeyAttri;
    SYN_ATTR_STRING: Result := FStringAttri;
    SYN_ATTR_WHITESPACE: Result := FSpaceAttri;
    SYN_ATTR_SYMBOL: Result := FSymbolAttri;
    else
      Result := nil;
  end;
end;

function TSynVrml97Syn.GetEol: Boolean;
begin
  Result := FTokenID = tkNull;
end;

function TSynVrml97Syn.GetRange: Pointer;
begin
  Result := Pointer(FRange);
end;

function TSynVrml97Syn.GetTokenID: TtkTokenKind;
begin
  Result := FTokenID;
end;

function TSynVrml97Syn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := FCommentAttri;
    tkIdentifier: Result := FIdentifierAttri;
    tkKey: Result := FKeyAttri;
    tkNonReservedKey: Result := FNonReservedKeyAttri;
    tkEvent: Result := FEventAttri;
    tkNumber: Result := FNumberAttri;
    tkSpace: Result := FSpaceAttri;
    tkString: Result := FStringAttri;
    tkSymbol: Result := FSymbolAttri;
    tkUnknown: Result := FIdentifierAttri;
    // vrml
    tkVrmlAppearance: Result := FVrmlAppearanceAttri;
    tkVrmlAttribute: Result := FVrmlAttributeAttri;
    tkVrmlDefinition: Result := FVrmlDefinitionAttri;
    tkVrmlEvent: Result := FVrmlEventAttri;
    tkVrmlGrouping: Result := FVrmlGroupingAttri;
    tkVrmlInterpolator: Result := FVrmlInterpolatorAttri;
    tkVrmlLight: Result := FVrmlLightAttri;
    tkVrmlNode: Result := FVrmlNodeAttri;
    tkVrmlParameter: Result := FVrmlParameterAttri;
    tkVrmlproto: Result := FVrmlprotoAttri;
    tkVrmlSensor: Result := FVrmlSensorAttri;
    tkVrmlShape: Result := FVrmlShapeAttri;
    tkVrmlShape_Hint: Result := FVrmlShape_HintAttri;
    tkVrmlTime_dependent: Result := FVrmlTime_dependentAttri;
    tkVrmlViewpoint: Result := FVrmlViewpointAttri;
    tkVrmlWorldInfo: Result := FVrmlWorldInfoAttri;
    tkX3DDocType: Result := FX3DDocTypeAttri;
    tkX3DHeader: Result := FX3DHeaderAttri;
    //--
    else
      Result := nil;
  end;
end;

function TSynVrml97Syn.GetTokenKind: Integer;
begin
  Result := Ord(FTokenID);
end;

procedure TSynVrml97Syn.ResetRange;
begin
  FRange := rsNormalText;
end;

procedure TSynVrml97Syn.SetRange(Value: Pointer);
begin
  FRange := TRangeState(Value);
end;

function TSynVrml97Syn.IsFilterStored: Boolean;
begin
  Result := FDefaultFilter <> SYNS_FilterVrml97;
end;

class function TSynVrml97Syn.GetLanguageName: string;
begin
  Result := SYNS_LangVrml97;
end;

function TSynVrml97Syn.GetSampleSource: UnicodeString;
begin
  Result :=
    '#VRML V2.0 utf8'#13#10 +
    'Transform {'#13#10 +
    '  children ['#13#10 +
    '    NavigationInfo { headlight FALSE } # We''ll add our own light'#13#10 +
    ''#13#10 +
    '    DirectionalLight {        # First child'#13#10 +
    '        direction 0 0 -1      # Light illuminating the scene'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    Transform {               # Second child - a red sphere'#13#10 +
    '      translation 3 0 1'#13#10 +
    '      children ['#13#10 +
    '        Shape {'#13#10 +
    '          geometry Sphere { radius 2.3 }'#13#10 +
    '          appearance Appearance {'#13#10 +
    '            material Material { diffuseColor 1 0 0 }   # Red'#13#10 +
    '         }'#13#10 +
    '        }'#13#10 +
    '      ]'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '    Transform {               # Third child - a blue box '#13#10 +
    '      translation -2.4 .2 1'#13#10 +
    '      rotation     0 1 1  .9'#13#10 +
    '      children ['#13#10 +
    '        Shape {'#13#10 +
    '          geometry Box {}'#13#10 +
    '          appearance Appearance {'#13#10 +
    '            material Material { diffuseColor 0 0 1 }  # Blue'#13#10 +
    '         }'#13#10 +
    '        }'#13#10 +
    '      ]'#13#10 +
    '    }'#13#10 +
    ''#13#10 +
    '  ] # end of children for world'#13#10 +
    '}'#13#10 +
    'DEF Example_2 Script {'#13#10 +
    '    field   SFNode myself USE Example_2'#13#10 +
    '    field   SFNode root USE ROOT_TRANSFORM'#13#10 +
    '    field   MFString url "foo.wrl"'#13#10 +
    '    eventIn MFNode   nodesLoaded'#13#10 +
    '    eventIn SFBool   trigger_event'#13#10 +
    ''#13#10 +
    '    url "javascript:'#13#10 +
    '        function trigger_event(value, ts){'#13#10 +
    '            // do something and then fetch values'#13#10 +
    '            Browser.createVRMLFromURL(url, myself, ''nodesLoaded'');'#13#10 +
    '        }'#13#10 +
    ''#13#10 +
    '        function nodesLoaded(value, timestamp){'#13#10 +
    '            if (value.length > 5) {'#13#10 +
    '                 // do something more than 5 nodes in this MFNode...'#13#10 +
    '            }'#13#10 +
    '            root.addChildren = value;'#13#10 +
    '        }"'#13#10 +
    '}';
end;

class function TSynVrml97Syn.GetFriendlyLanguageName: UnicodeString;
begin
  Result := SYNS_FriendlyLangVrml97;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynVrml97Syn);
{$ENDIF}
end.
