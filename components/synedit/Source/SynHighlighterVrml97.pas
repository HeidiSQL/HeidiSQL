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

$Id: SynHighlighterVrml97.pas,v 1.6.2.7 2005/12/16 20:09:37 maelh Exp $

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

{$IFNDEF QSYNHIGHLIGHTERVRML97}
unit SynHighlighterVrml97;
{$ENDIF}

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  Qt,
  QControls,
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  QSynHighlighterHashEntries,
{$ELSE}
  Windows,
  Messages,
  Registry,
  Controls,
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynHighlighterHashEntries,  
{$ENDIF}
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
    fRange :TRangeState;
    isDoctype :boolean;
    FTokenID :TtkTokenKind;
    fCommentAttri :TSynHighlighterAttributes;
    fIdentifierAttri :TSynHighlighterAttributes;
    fKeyAttri :TSynHighlighterAttributes;
    fNonReservedKeyAttri :TSynHighlighterAttributes;
    fEventAttri :TSynHighlighterAttributes;
    fNumberAttri :TSynHighlighterAttributes;
    fSpaceAttri :TSynHighlighterAttributes;
    fStringAttri :TSynHighlighterAttributes;
    fSymbolAttri :TSynHighlighterAttributes;

    fVrmlAppearanceAttri :TSynHighlighterAttributes;
    fVrmlAttributeAttri :TSynHighlighterAttributes;
    fVrmlDefinitionAttri :TSynHighlighterAttributes;
    fVrmlEventAttri :TSynHighlighterAttributes;
    fVrmlGroupingAttri :TSynHighlighterAttributes;
    fVrmlInterpolatorAttri :TSynHighlighterAttributes;
    fVrmlLightAttri :TSynHighlighterAttributes;
    fVrmlNodeAttri :TSynHighlighterAttributes;
    fVrmlParameterAttri :TSynHighlighterAttributes;
    fVrmlprotoAttri :TSynHighlighterAttributes;
    fVrmlSensorAttri :TSynHighlighterAttributes;
    fVrmlShapeAttri :TSynHighlighterAttributes;
    fVrmlShape_HintAttri :TSynHighlighterAttributes;
    fVrmlTime_dependentAttri :TSynHighlighterAttributes;
    fVrmlViewpointAttri :TSynHighlighterAttributes;
    fVrmlWorldInfoAttri :TSynHighlighterAttributes;
    fX3DDocTypeAttri :TSynHighlighterAttributes;
    fX3DHeaderAttri :TSynHighlighterAttributes;

    fKeywords: TSynHashEntryList;
    procedure DoAddKeyword(AKeyword: WideString; AKind: integer);
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
    function NextTokenIs(T: WideString) :Boolean;
  protected
    function GetSampleSource: WideString; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
    class function GetFriendlyLanguageName: WideString; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index :integer) :TSynHighlighterAttributes;
      override;
    function GetEol :Boolean; override;
    function GetRange :Pointer; override;
    function GetTokenID :TtkTokenKind;
    function GetTokenAttribute :TSynHighlighterAttributes; override;
    function GetTokenKind :integer; override;
    procedure Next; override;
    procedure SetRange(Value :Pointer); override;
    procedure ResetRange; override;
  published
    property NonReservedKeyAttri :TSynHighlighterAttributes read fNonReservedKeyAttri write fNonReservedKeyAttri;
    property NumberAttri :TSynHighlighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri :TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri :TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri :TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property CommentAttri :TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri :TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property EcmaScriptKeyAttri :TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property EcmaScriptEventAttri :TSynHighlighterAttributes read fEventAttri write fEventAttri;

    property VrmlAppearanceAttri :TSynHighlighterAttributes read fVrmlAppearanceAttri write fVrmlAppearanceAttri;
    property VrmlAttributeAttri :TSynHighlighterAttributes read fVrmlAttributeAttri write fVrmlAttributeAttri;
    property VrmlDefinitionAttri :TSynHighlighterAttributes read fVrmlDefinitionAttri write fVrmlDefinitionAttri;
    property VrmlEventAttri :TSynHighlighterAttributes read fVrmlEventAttri write fVrmlEventAttri;
    property VrmlGroupingAttri :TSynHighlighterAttributes read fVrmlGroupingAttri write fVrmlGroupingAttri;
    property VrmlInterpolatorAttri :TSynHighlighterAttributes read fVrmlInterpolatorAttri write fVrmlInterpolatorAttri;
    property VrmlLightAttri :TSynHighlighterAttributes read fVrmlLightAttri write fVrmlLightAttri;
    property VrmlNodeAttri :TSynHighlighterAttributes read fVrmlNodeAttri write fVrmlNodeAttri;
    property VrmlParameterAttri :TSynHighlighterAttributes read fVrmlParameterAttri write fVrmlParameterAttri;
    property VrmlprotoAttri :TSynHighlighterAttributes read fVrmlprotoAttri write fVrmlprotoAttri;
    property VrmlSensorAttri :TSynHighlighterAttributes read fVrmlSensorAttri write fVrmlSensorAttri;
    property VrmlShapeAttri :TSynHighlighterAttributes read fVrmlShapeAttri write fVrmlShapeAttri;
    property VrmlShape_HintAttri :TSynHighlighterAttributes read fVrmlShape_HintAttri write fVrmlShape_HintAttri;
    property VrmlTime_dependentAttri :TSynHighlighterAttributes read fVrmlTime_dependentAttri write fVrmlTime_dependentAttri;
    property VrmlViewpointAttri :TSynHighlighterAttributes read fVrmlViewpointAttri write fVrmlViewpointAttri;
    property VrmlWorldInfoAttri :TSynHighlighterAttributes read fVrmlWorldInfoAttri write fVrmlWorldInfoAttri;
    property X3DDocTypeAttri :TSynHighlighterAttributes read fX3DDocTypeAttri write fX3DDocTypeAttri;
    property X3DHeaderAttri :TSynHighlighterAttributes read fX3DHeaderAttri write fX3DHeaderAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  Events: WideString =
    'onAbort, onBlur, onChange, onClick, onDblClick, onError, onFocus, ' +
    'onKeyDown, onKeyPress, onKeyUp, onLoad, onMouseDown, onMouseMove, ' +
    'onMouseOut, onMouseOver, onMouseUp, onReset, onSelect, onSubmit, ' +
    'onUnload';

  KeyWords: WideString =
    'abstract, boolean, break, byte, callee, case, catch, char, class, ' +
    'const, constructor, continue, debugger, default, delete, do, DOCTYPE, ' +
    'double, else, enum, export, extends, false, final, finally, float, for, ' +
    'function, goto, head, if, implements, import, in, instanceof, int, ' +
    'interface, long, meta, NaN, native, new, null, package, private, ' +
    'protected, prototype, public, PUBLIC, return, short, start, static, ' +
    'super, switch, synchronized, this, throw, throws, transient, true, try, ' +
    'typeof, var, void, while, with, xml';

  NonReservedKeys: WideString =
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

  VrmlAppearances: WideString =
    'Appearance, ImageTexture, Material, NurbsTextureSurface, PixelTexture, ' +
    'TextureBackground, TextureCoordinate, TextureCoordinateGenerator, ' +
    'TextureTransform';

  VrmlAttributes: WideString =
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

  VrmlDefinitions: WideString =
    'MFColor, MFFloat, MFInt32, MFNode, MFRotation, MFString, MFTime, ' +
    'MFVec2f, MFVec3f, SFBool, SFColor, SFFloat, SFImage, SFInt32, SFNode, ' +
    'SFRotation, SFString, SFTime, SFVec2f, SFVec3f';

  VrmlEvents: WideString =
    'eventIn, eventOut, exposedField, field';

  VrmlGroupings: WideString =
    'Anchor, Billboard, Collision, ESPDUTransform, Group, Inline, LOD, ' +
    'NurbsGroup, ReceiverPdu, SignalPdu, StaticGroup, Switch, Transform, ' +
    'Transform2D, TransmitterPdu';

  VrmlInterpolators: WideString =
    'ColorInterpolator, CoordinateInterpolator, CoordinateInterpolator2D, ' +
    'GeoPositionInterpolator, NormalInterpolator, NurbsPositionInterpolator, ' +
    'OrientationInterpolator, PositionInterpolator, PositionInterpolator2D, ' +
    'ScalarInterpolator';

  VrmlLights: WideString =
    'DirectionalLight, PointLight, SpotLight';

  VrmlNodes: WideString =
    'Background, Color, Coordinate, CoordinateDeformer, Fog, FontStyle, ' +
    'Joint, NavigationInfo, Normal, Script, Site, Sound';

  VrmlParameters: WideString =
    'ALL, AUTO, BINDINGS, BOLD, BOTTOM, CENTER, CLAMP, CLOCKWISE, CONVEX, ' +
    'COUNTERCLOCKWISE, CULLING, DEFAULT, DEFAULTS, Displacer, ENUMS, FACE, FALSE, ' +
    'FAMILY, FILE, FORMAT, ITALIC, JUSTIFICATION, LEFT, NONE, NULL, OFF, ON, ' +
    'OVERALL, PARTS, PER_FACE, PER_FACE_INDEXED, PER_PART, PER_PART_INDEXED, ' +
    'PER_VERTEX, PER_VERTEX_INDEXED, REPEAT, RIGHT, SHAPE, SIDES, SOLID, ' +
    'STYLE, TRUE, TYPE, UNKNOWN_FACE_TYPE, UNKNOWN_ORDERING, ' +
    'UNKNOWN_SHAPE_TYPE, WRAP';

  VrmlProtos: WideString =
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

  VrmlSensors: WideString =
    'BooleanFilter, BooleanSequencer, BooleanToggle, BooleanTrigger, ' +
    'CylinderSensor, GeoTouchSensor, IntegerTrigger, KeySensor, LoadSensor, ' +
    'PlaneSensor, ProximitySensor, SphereSensor, StringSensor, TimeSensor, ' +
    'TouchSensor, VisibilitySensor';

  VrmlShapes: WideString =
    'Arc2D, ArcClose2D, Box, Circle2D, Cone, Contour2D, ContourPolyline2D, ' +
    'Cylinder, Disk2D, ElevationGrid, Humanoid, NurbsCurve, NurbsCurve2D, ' +
    'NurbsSurface, PointSet, Polyline2D, Polypoint2D, Rectangle2D, Segment, ' +
    'Shape, Shape2D, Sphere, Text, TriangleFanSet, TriangleSet, TriangleSet2D, ' +
    'TriangleStripSet, TrimmedSurface';

  VrmlShape_Hints: WideString =
    'Extrusion, IndexedFaceSet, IndexedLineSet';

  VrmlTime_dependents: WideString =
    'AudioClip, IntegerSequencer, MovieTexture, TimeTrigger';

  VrmlViewpoints: WideString =
    'GeoViewpoint, Viewpoint';

  VrmlWorldInfos: WideString =
    'WorldInfo';


procedure TSynVrml97Syn.DoAddKeyword(AKeyword: WideString; AKind: integer);
var
  HashValue: integer;
begin
  HashValue := HashKey(PWideChar(AKeyword));
  fKeywords[HashValue] := TSynHashEntry.Create(AKeyword, AKind);
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
  fStringLen := Str - fToIdent;
end;

function TSynVrml97Syn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Entry: TSynHashEntry;
begin
  fToIdent := MayBe;
  Entry := fKeywords[HashKey(MayBe)];
  while Assigned(Entry) do
  begin
    if Entry.KeywordLen > fStringLen then
      break
    else if Entry.KeywordLen = fStringLen then
      if IsCurrentToken(Entry.Keyword) then
      begin
        Result := TtkTokenKind(Entry.Kind);
        exit;
      end;
    Entry := Entry.Next;
  end;
  Result := tkIdentifier;
end;

constructor TSynVrml97Syn.Create(AOwner :TComponent);
begin
  inherited Create(AOwner);

  fCaseSensitive := True;

  fKeywords := TSynHashEntryList.Create;
  isDoctype := False;
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  fCommentAttri.Background := clGray;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  fIdentifierAttri.Style := [];
  fIdentifierAttri.Foreground := clNavy;
  fIdentifierAttri.Background := clWhite;
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_FriendlyAttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clRed;
  fKeyAttri.Background := clWhite;
  AddAttribute(fKeyAttri);

  fNonReservedKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrNonReservedKeyword, SYNS_FriendlyAttrNonReservedKeyword);
  fNonReservedKeyAttri.Style := [fsItalic];
  fNonReservedKeyAttri.Foreground := clBlack;
  fNonReservedKeyAttri.Background := clWhite;
  AddAttribute(fNonReservedKeyAttri);

  fEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrEvent, SYNS_FriendlyAttrEvent);
  fEventAttri.Style := [fsItalic];
  fEventAttri.Foreground := clNavy;
  fEventAttri.Background := clWhite;
  AddAttribute(fEventAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_FriendlyAttrNumber);
  fEventAttri.Style := [fsItalic];
  fEventAttri.Foreground := clNavy;
  fEventAttri.Background := clWhite;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  fSpaceAttri.Style := [fsItalic];
  fSpaceAttri.Foreground := clNavy;
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_FriendlyAttrString);
  fStringAttri.Style := [fsItalic];
  fStringAttri.Foreground := clNavy;
  fStringAttri.Background := clWhite;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Style := [fsItalic];
  fSymbolAttri.Foreground := clNavy;
  fSymbolAttri.Background := clWhite;
  AddAttribute(fSymbolAttri);
  //-- vrml
  fVrmlAppearanceAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlAppearance, SYNS_FriendlyAttrVrmlAppearance);
  fVrmlAppearanceAttri.Style := [fsItalic];
  fVrmlAppearanceAttri.Foreground := clNavy;
  fVrmlAppearanceAttri.Background := clWhite;
  AddAttribute(fVrmlAppearanceAttri);

  fVrmlAttributeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlAttribute, SYNS_FriendlyAttrVrmlAttribute);
  fVrmlAttributeAttri.Style := [fsItalic];
  fVrmlAttributeAttri.Foreground := clNavy;
  fVrmlAttributeAttri.Background := clGray;
  AddAttribute(fVrmlAttributeAttri);

  fVrmlDefinitionAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlDefinition, SYNS_FriendlyAttrVrmlDefinition);
  fVrmlDefinitionAttri.Style := [fsItalic];
  fVrmlDefinitionAttri.Foreground := clNavy;
  fVrmlDefinitionAttri.Background := clRed;
  AddAttribute(fVrmlDefinitionAttri);

  fVrmlEventAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlEvent, SYNS_FriendlyAttrVrmlEvent);
  fVrmlEventAttri.Style := [fsBold];
  fVrmlEventAttri.Foreground := clRed;
  fVrmlEventAttri.Background := clWhite;
  AddAttribute(fVrmlEventAttri);

  fVrmlGroupingAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlGrouping, SYNS_FriendlyAttrVrmlGrouping);
  fVrmlGroupingAttri.Style := [fsBold];
  fVrmlGroupingAttri.Foreground := clNavy;
  fVrmlGroupingAttri.Background := clWhite;
  AddAttribute(fVrmlGroupingAttri);

  fVrmlInterpolatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlInterpolator, SYNS_FriendlyAttrVrmlInterpolator);
  fVrmlInterpolatorAttri.Style := [fsItalic];
  fVrmlInterpolatorAttri.Foreground := clLime;
  fVrmlInterpolatorAttri.Background := clWhite;
  AddAttribute(fVrmlInterpolatorAttri);

  fVrmlLightAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlLight, SYNS_FriendlyAttrVrmlLight);
  fVrmlLightAttri.Style := [fsItalic];
  fVrmlLightAttri.Foreground := clTeal;
  fVrmlLightAttri.Background := clWhite;
  AddAttribute(fVrmlLightAttri);

  fVrmlNodeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlNode, SYNS_FriendlyAttrVrmlNode);
  fVrmlNodeAttri.Style := [fsItalic, fsBold];
  fVrmlNodeAttri.Foreground := clGreen;
  fVrmlNodeAttri.Background := clWhite;
  AddAttribute(fVrmlNodeAttri);

  fVrmlParameterAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlParameter, SYNS_FriendlyAttrVrmlParameter);
  fVrmlParameterAttri.Style := [fsBold];
  fVrmlParameterAttri.Foreground := $F0CAA6; //clSkyBlue
  fVrmlParameterAttri.Background := clWhite;
  AddAttribute(fVrmlParameterAttri);

  fVrmlprotoAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlProto, SYNS_FriendlyAttrVrmlProto);
  fVrmlprotoAttri.Style := [fsBold];
  fVrmlprotoAttri.Foreground := clRed;
  fVrmlprotoAttri.Background := clWhite;
  AddAttribute(fVrmlprotoAttri);

  fVrmlSensorAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlSensor, SYNS_FriendlyAttrVrmlSensor);
  fVrmlSensorAttri.Style := [fsBold];
  fVrmlSensorAttri.Foreground := clOlive;
  fVrmlSensorAttri.Background := clWhite;
  AddAttribute(fVrmlSensorAttri);

  fVrmlShapeAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlShape, SYNS_FriendlyAttrVrmlShape);
  fVrmlShapeAttri.Style := [fsBold];
  fVrmlShapeAttri.Foreground := clPurple;
  fVrmlShapeAttri.Background := clWhite;
  AddAttribute(fVrmlShapeAttri);

  fVrmlShape_HintAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlShape_Hint, SYNS_FriendlyAttrVrmlShape_Hint);
  fVrmlShape_HintAttri.Style := [fsItalic];
  fVrmlShape_HintAttri.Foreground := clPurple;
  fVrmlShape_HintAttri.Background := clWhite;
  AddAttribute(fVrmlShape_HintAttri);

  fVrmlTime_dependentAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlTime_dependent, SYNS_FriendlyAttrVrmlTime_dependent);
  fVrmlTime_dependentAttri.Style := [fsItalic];
  fVrmlTime_dependentAttri.Foreground := clOlive;
  fVrmlTime_dependentAttri.Background := clWhite;
  AddAttribute(fVrmlTime_dependentAttri);

  fVrmlViewpointAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlViewpoint, SYNS_FriendlyAttrVrmlViewpoint);
  fVrmlViewpointAttri.Style := [fsItalic];
  fVrmlViewpointAttri.Foreground := clGreen;
  fVrmlViewpointAttri.Background := clWhite;
  AddAttribute(fVrmlViewpointAttri);

  fVrmlWorldInfoAttri := TSynHighlighterAttributes.Create(SYNS_AttrVrmlWorldInfo, SYNS_FriendlyAttrVrmlWorldInfo);
  fVrmlWorldInfoAttri.Style := [fsItalic];
  fVrmlWorldInfoAttri.Foreground := clMaroon;
  fVrmlWorldInfoAttri.Background := clWhite;
  AddAttribute(fVrmlWorldInfoAttri);

  fX3DDocTypeAttri := TSynHighLighterAttributes.Create(SYNS_AttrX3DDocType, SYNS_FriendlyAttrX3DDocType);
  fX3DDocTypeAttri.Style := [fsItalic];
  fX3DDocTypeAttri.Foreground := clMaroon;
  fX3DDocTypeAttri.Background := clWhite;
  AddAttribute(fX3DDocTypeAttri);

  fX3DHeaderAttri := TSynHighLighterAttributes.Create(SYNS_AttrX3DHeader, SYNS_FriendlyAttrX3DHeader);
  fX3DHeaderAttri.Style := [fsItalic];
  fX3DHeaderAttri.Foreground := clMaroon;
  fX3DHeaderAttri.Background := clWhite;
  AddAttribute(fX3DHeaderAttri);
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

  fDefaultFilter := SYNS_FilterVrml97;
  fRange := rsNormalText;
end;

destructor TSynVrml97Syn.Destroy;
begin
  fKeywords.Free;
  inherited Destroy;
end;

procedure TSynVrml97Syn.AndSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('&')] then inc(Run);
end;

function TSynVrml97Syn.NextTokenIs(T: WideString): Boolean;
var
  I, Len: Integer;
begin
  Result := True;
  Len := Length(T);
  for I := 1 to Len do
    if (fLine[Run + I] <> T[I]) then
    begin
      Result := False;
      Break;
    end;
end;

procedure TSynVrml97Syn.InCommentProc;
begin
  if (fLine[Run + 1] = '-') and (fLine[Run + 2] = '-') then
    begin
      Inc(Run);
      fTokenID := tkComment;
      fRange := rsComment;
      Inc(Run, 2);
      repeat
        Inc(Run);
        if (fLine[Run] = '-') and (fLine[Run + 1] = '-') then
          begin
            fRange := rsNormalText;
            Inc(Run, 2);
            break;
          end;
      until IsLineEnd(Run);
      Exit;
    end;
end;

procedure TSynVrml97Syn.DiesisCommentProc;
begin
  if fLine[Run] = #0 then
    NullProc
  else
    begin
      fTokenID := tkComment;
      repeat
        inc(Run);
      until IsLineEnd(Run);
    end;
end;

procedure TSynVrml97Syn.X3DHeaderOpenProc;
begin
  Inc(Run);
  fRange := rsX3DHeader;
  X3DHeaderProc;
  fTokenID := tkX3DHeader;
end;

procedure TSynVrml97Syn.X3DHeaderProc;
begin
  case fLine[Run] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
      begin
        fTokenID := tkX3DHeader;
        repeat
          if (fLine[Run] = '?') then
            begin
              Inc(Run, 1);
              fRange := rsNormalText;
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
      fRange := rsX3DDocType;
      X3DDocTypeProc;
      fTokenID := tkX3DDocType;
    end
  else
    if NextTokenIs('--') then
      begin
        fRange := rsComment;
        InCommentProc;
        fTokenID := tkComment;
      end
    else
    begin
      fTokenID := tkSymbol;
      inc(Run);
    end;
end;

procedure TSynVrml97Syn.X3DDocTypeProc;
begin
  case fLine[Run] of
    #0 :NullProc;
    #10 :LFProc;
    #13 :CRProc;
    else
      begin
        fTokenID := tkX3DDocType;
        repeat
          if (fLine[Run + 1] = '>') then
            begin
              Inc(Run, 1);
              fRange := rsNormalText;
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
  if fLine[Run] = #0 then
    NullProc
  else
    begin
      fTokenID := tkComment;
      repeat
        if ((fLine[Run] = '*') and (fLine[Run + 1] = '/'))
          or
          ((fLine[Run] = '-') and (fLine[Run + 1] = '-')) then
          begin
            fRange := rsNormalText;
            inc(Run, 2);
            break;
          end;
        inc(Run);
      until IsLineEnd(Run);
    end;
end;

procedure TSynVrml97Syn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then inc(Run);
end;

procedure TSynVrml97Syn.IdentProc;
begin
  fTokenID := IdentKind(fLine + Run);
  inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
    inc(Run);
end;

procedure TSynVrml97Syn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVrml97Syn.MinusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('-'), WideChar('>')] then inc(Run);
end;

procedure TSynVrml97Syn.ModSymbolProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynVrml97Syn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynVrml97Syn.NumberProc;

  function IsNumberChar: Boolean;
  begin
    case fLine[Run] of
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
  fTokenID := tkNumber;
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
          if not (FLine[Succ(Run)] in [WideChar('0')..WideChar('9'),
            WideChar('a')..WideChar('f'), WideChar('A')..WideChar('F')]) then
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
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('|')] then inc(Run);
end;

procedure TSynVrml97Syn.PlusProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] in [WideChar('='), WideChar('+')] then inc(Run);
end;

procedure TSynVrml97Syn.PointProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if (fLine[Run] = '.') and (fLine[Run + 1] = '.') then inc(Run, 2);
end;

procedure TSynVrml97Syn.SlashProc;
begin
  Inc(Run);
  case fLine[Run] of
    '/' :
      begin
        fTokenID := tkComment;
        repeat
          Inc(Run);
        until IsLineEnd(Run);
      end;
    '*' :
      begin
        fTokenID := tkComment;
        fRange := rsComment;
        repeat
          Inc(Run);
          if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
            begin
              fRange := rsNormalText;
              Inc(Run, 2);
              break;
            end;
        until IsLineEnd(Run);
      end;
    '=' :
      begin
        Inc(Run);
        fTokenID := tkSymbol;
      end;
    else
      fTokenID := tkSymbol;
  end;
end;

procedure TSynVrml97Syn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynVrml97Syn.StarProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
  if fLine[Run] = '=' then inc(Run);
end;

procedure TSynVrml97Syn.StringProc;
var
  l_strChar: WideString;
begin
  fTokenID := tkString;
  l_strChar := FLine[Run]; // We could have '"' or #39
  if (FLine[Run + 1] = l_strChar) and (FLine[Run + 2] = l_strChar) then inc(Run, 2);
  repeat
    if IsLineEnd(Run) then break;
    inc(Run);
  until (FLine[Run] = l_strChar) and (FLine[Pred(Run)] <> '\');
  if not IsLineEnd(Run) then
    Inc(Run);
end;

procedure TSynVrml97Syn.SymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVrml97Syn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynVrml97Syn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsX3DHeader: X3DHeaderProc;
    rsX3DDocType: X3DDocTypeProc;
    rsComment: CommentProc;
    else
      case fLine[Run] of
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

function TSynVrml97Syn.GetDefaultAttribute(Index :integer) :TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
    else
      Result := nil;
  end;
end;

function TSynVrml97Syn.GetEol :Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynVrml97Syn.GetRange :Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynVrml97Syn.GetTokenID :TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVrml97Syn.GetTokenAttribute :TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNonReservedKey: Result := fNonReservedKeyAttri;
    tkEvent: Result := fEventAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkUnknown: Result := fIdentifierAttri;
    // vrml
    tkVrmlAppearance: Result := fVrmlAppearanceAttri;
    tkVrmlAttribute: Result := fVrmlAttributeAttri;
    tkVrmlDefinition: Result := fVrmlDefinitionAttri;
    tkVrmlEvent: Result := fVrmlEventAttri;
    tkVrmlGrouping: Result := fVrmlGroupingAttri;
    tkVrmlInterpolator: Result := fVrmlInterpolatorAttri;
    tkVrmlLight: Result := fVrmlLightAttri;
    tkVrmlNode: Result := fVrmlNodeAttri;
    tkVrmlParameter: Result := fVrmlParameterAttri;
    tkVrmlproto: Result := fVrmlprotoAttri;
    tkVrmlSensor: Result := fVrmlSensorAttri;
    tkVrmlShape: Result := fVrmlShapeAttri;
    tkVrmlShape_Hint: Result := fVrmlShape_HintAttri;
    tkVrmlTime_dependent: Result := fVrmlTime_dependentAttri;
    tkVrmlViewpoint: Result := fVrmlViewpointAttri;
    tkVrmlWorldInfo: Result := fVrmlWorldInfoAttri;
    tkX3DDocType: Result := fX3DDocTypeAttri;
    tkX3DHeader: Result := fX3DHeaderAttri;
    //--
    else
      Result := nil;
  end;
end;

function TSynVrml97Syn.GetTokenKind :integer;
begin
  Result := Ord(fTokenId);
end;

procedure TSynVrml97Syn.ResetRange;
begin
  fRange := rsNormalText;
end;

procedure TSynVrml97Syn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynVrml97Syn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterVrml97;
end;

class function TSynVrml97Syn.GetLanguageName: string;
begin
  Result := SYNS_LangVrml97;
end;

function TSynVrml97Syn.GetSampleSource: WideString;
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

class function TSynVrml97Syn.GetFriendlyLanguageName: WideString;
begin
  Result := SYNS_FriendlyLangVrml97;
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynVrml97Syn);
{$ENDIF}
end.
