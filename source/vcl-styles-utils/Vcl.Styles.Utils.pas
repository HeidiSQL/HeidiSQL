//**************************************************************************************************
//
// Unit Vcl.Styles.Utils
// unit for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.Utils.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2012-2016 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Vcl.Styles.Utils;

interface
uses
  System.Classes,
  System.Generics.Collections,
  Vcl.Styles.Utils.Graphics,
  Vcl.Themes,
  Vcl.Styles.Ext;

type
  TVCLStylesElement  = (vseBitmaps, vseSysColors, vseStyleColors, vseStyleFontColors);
  TVCLStylesElements = set of TVCLStylesElement;
  TVCLStylesFilter   = (vsfHSL, vsfRGB, vsfBlend, vsfTextureBlend);

  TVclStylesUtils = class
  private
     FClone     : Boolean;
     FStream    : TStream;
     FStyleExt  : TCustomStyleExt;
     FElements  : TVCLStylesElements;
     //FSourceInfo: TSourceInfo;
  public
     procedure SetFilters(Filters : TObjectList<TBitmapFilter>);
     procedure ApplyChanges;
     procedure SaveToFile(const FileName: string);
     //property  SourceInfo: TSourceInfo read FSourceInfo;
     property  StyleExt  :  TCustomStyleExt read FStyleExt;
     property  Elements  : TVCLStylesElements read FElements write FElements;
     constructor Create(const  StyleName : string;Clone:Boolean=False);
     destructor Destroy;override;
     class procedure SaveSettings(const FileName:String;Elements :TVCLStylesElements; FilterType : TVCLStylesFilter;Filters : TObjectList<TBitmapFilter>);
     class procedure LoadSettings(const FileName:String;var Elements :TVCLStylesElements; var FilterType : TVCLStylesFilter;Filters : TObjectList<TBitmapFilter>);
     //class procedure LoadAndApplySettings(const FileName:String);
  end;

const
  VCLStylesFilterNames : Array[TVCLStylesFilter] of string = ('HSL','RGB','Blend','Texture Blend');

implementation

uses
  System.Rtti,
  System.IOUtils,
  System.SysUtils,
  Xml.XMLDoc,
  Xml.XMLIntf,
  Vcl.Graphics;


{ TVclStylesUtils }
constructor TVclStylesUtils.Create(const  StyleName : string;Clone:Boolean=False);
var
  FSourceInfo: TSourceInfo;
begin
  TStyleManager.StyleNames;//call DiscoverStyleResources
  FElements :=[vseBitmaps];
  FClone   :=Clone;
  FStyleExt:=nil;
  FStream  :=nil;

  if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
  begin
   if FClone then
   begin
     FStream:=TMemoryStream.Create;
     FSourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
     TStream(FSourceInfo.Data).Position:=0;
     FStream.CopyFrom(TStream(FSourceInfo.Data),TStream(FSourceInfo.Data).Size);
     //restore original index
     TStream(FSourceInfo.Data).Position:=0;
     FStream.Position:=0;
   end
   else
   FStream:=TStream(TStyleManager.StyleSourceInfo[StyleName].Data);
   FStyleExt:=TCustomStyleExt.Create(FStream);
  end;
end;

destructor TVclStylesUtils.Destroy;
begin
  if Assigned(StyleExt) then
    StyleExt.Free;
  if FClone and Assigned(FStream) then
    FStream.Free;
  inherited;
end;

procedure TVclStylesUtils.ApplyChanges;
begin
  if Assigned(StyleExt) then
  begin
    FStream.Size:=0;
    StyleExt.CopyToStream(FStream);
    FStream.Seek(0,soFromBeginning);
  end;
end;

class procedure TVclStylesUtils.SaveSettings(const FileName:String;Elements :TVCLStylesElements; FilterType : TVCLStylesFilter;Filters : TObjectList<TBitmapFilter>);
var
  Doc       : IXMLDocument;
  RootNode, ChildNode, oNode : IXMLNode;
  LFilter   : TBitmapFilter;
begin
  Doc   :=TXMLDocument.Create(nil);
  try
    Doc.Active  := True;
    Doc.Version :='1.0';
    Doc.Encoding:='utf-8';
    Doc.Options := [doNodeAutoIndent];
    RootNode    := Doc.AddChild('VCLStylesEQ');
    RootNode.Attributes['created']    := FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
    RootNode.Attributes['vseBitmaps'] := BoolToStr({vseBitmaps in Elements}True, True);
    RootNode.Attributes['vseSysColors'] := BoolToStr(vseSysColors in Elements, True);
    RootNode.Attributes['vseStyleColors'] := BoolToStr(vseStyleColors in Elements, True);
    RootNode.Attributes['vseStyleFontColors'] := BoolToStr(vseStyleFontColors in Elements, True);
    ChildNode := RootNode.AddChild('FilterType');
    ChildNode.Attributes['Name'] := VCLStylesFilterNames[FilterType];

    for LFilter in Filters do
    begin
     oNode  := ChildNode.AddChild(LFilter.ClassName);
     oNode.Text:=IntToStr(LFilter.ColorValue);
    end;
    Doc.SaveToFile(FileName);
  finally
   Doc:=nil;
  end;
end;

class procedure TVclStylesUtils.LoadSettings(const FileName:String;var Elements :TVCLStylesElements; var FilterType : TVCLStylesFilter;Filters : TObjectList<TBitmapFilter>);
var
  Doc       : IXMLDocument;
  RootNode, ChildNode, oNode : IXMLNode;
  LFilterType  :TVCLStylesFilter;
  i : Integer;
  LClassName : string;
  Ctx : TRttiContext;
  RttiInstanceType : TRttiInstanceType;
  Value : TValue;
begin
  Doc   :=LoadXMLDocument(FileName);
  try
    RootNode :=Doc.DocumentElement;

    Elements:=[];
    if SameText(RootNode.Attributes['vseBitmaps'],'True') then
     Elements:=Elements+[vseBitmaps];

    if SameText(RootNode.Attributes['vseSysColors'],'True') then
     Elements:=Elements+[vseSysColors];

    if SameText(RootNode.Attributes['vseStyleColors'],'True') then
     Elements:=Elements+[vseStyleColors];

    if SameText(RootNode.Attributes['vseStyleFontColors'],'True') then
     Elements:=Elements+[vseStyleFontColors];

    ChildNode:=RootNode.ChildNodes[0];
    for LFilterType:=Low(TVCLStylesFilter) to High(TVCLStylesFilter) do
     if SameText(VCLStylesFilterNames[LFilterType], ChildNode.Attributes['Name']) then
     begin
       FilterType:=LFilterType;
       break;
     end;

     for i:=0 to ChildNode.ChildNodes.Count-1 do
     begin
      oNode:= ChildNode.ChildNodes[i];
      LClassName:='uHSLUtils.'+oNode.NodeName;
      RttiInstanceType := (Ctx.FindType(LClassName) as TRttiInstanceType);
      Value := RttiInstanceType.GetMethod('Create').Invoke(RttiInstanceType.MetaclassType,[StrToInt(oNode.Text)]);
      Filters.Add((Value.AsObject as TBitmapFilter));
     end;
  finally
   Doc:=nil;
  end;
end;

procedure TVclStylesUtils.SaveToFile(const FileName: string);
var
  FileStream: TFileStream;
begin
   if FileName<>'' then
   begin
     FileStream:=TFile.Create(FileName);
     try
       StyleExt.CopyToStream(FileStream);
     finally
       FileStream.Free;
     end;
   end;
end;

procedure TVclStylesUtils.SetFilters(Filters: TObjectList<TBitmapFilter>);
var
  LBitmap   : TBitmap;
  BitmapList: TObjectList<TBitmap>;
  Index     : Integer;
  Filter    : TBitmapFilter;
  Element   : TIdentMapEntry;
  LColor    : TColor;
  StyleColor: TStyleColor;
  StyleFont : TStyleFont;
begin
   if vseBitmaps in FElements then
   begin
     BitmapList:=StyleExt.BitmapList;
     try
       Index:=0;
       for LBitmap in BitmapList do
       begin
         for Filter in Filters do
           Filter.ProcessBitmap(LBitmap);
          StyleExt.ReplaceBitmap(Index, LBitmap);
          Inc(Index);
       end;
     finally
       BitmapList.Free;
     end;
   end;

   if vseSysColors in FElements then
   begin
     for Element in VclStyles_SysColors do
     begin
       LColor:=StyleExt.GetSystemColor(Element.Value);
        for Filter in Filters do
         LColor:=Filter.ProcessColor(LColor);

       StyleExt.SetSystemColor(Element.Value,LColor);
     end;
   end;

   if vseStyleColors in FElements then
   begin
     for StyleColor  := Low(TStyleColor) to High(TStyleColor) do
     begin
       LColor:=StyleExt.GetStyleColor(StyleColor);
        for Filter in Filters do
         LColor:=Filter.ProcessColor(LColor);

       StyleExt.SetStyleColor(StyleColor, LColor);
     end;
   end;

   if vseStyleFontColors in FElements then
   begin
     for StyleFont  := Low(TStyleFont) to High(TStyleFont) do
     begin
       LColor:=StyleExt.GetStyleFontColor(StyleFont);
        for Filter in Filters do
         LColor:=Filter.ProcessColor(LColor);

       StyleExt.SetStyleFontColor(StyleFont, LColor);
     end;
   end;
end;

end.
