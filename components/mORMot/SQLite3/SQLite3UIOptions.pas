/// General Options setting dialog
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3UIOptions;

(*
    This file is part of Synopse SQLite3 database framework.

    Synopse SQLite3 database framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse SQLite3 database framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


    Version 1.4 - February 8, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.5 - February 17, 2010
    - allow to hide some nodes/pages, depending of User level e.g.
    - add toolbar buttons per user customization

    Version 1.9
    - some code refactoring to share code with the new SQLite3UIEdit unit
    - minor fixes and enhancements

    Version 1.13
    - Delphi 2009/2010/XE compatibility fixes

    Version 1.15
    - Get rid of TMS components dependency

*)

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  SynCommons, SQLite3Commons,
  SQLite3UILogin, SQLite3UI, SQLite3UIEdit, SQLite3ToolBar, SQLite3i18n,
{$ifdef USETMSPACK}
  TaskDialog,
{$endif}
  StdCtrls, ExtCtrls, ComCtrls, SynTaskDialog;

type
  /// Options setting dialog
  // - the settings parameters are taken from the RTTI of supplied objects:
  // all the user interface is created from the code definition of classes;
  // a visual tree node will reflect the properties recursion, and published
  // properties are displayed as editing components
  // - published textual properties may be defined as generic RawUTF8 or
  // as generic string (with some possible encoding issue prior to Delphi 2009)
  // - caller must initialize some events, OnComponentCreate at least,
  // in order to supply the objects to be added on the form
  // - components creation is fully customizable by some events
  TOptionsForm = class(TRTTIForm)
    List: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  protected
    /// every char is a bit index for a tkInt64 TCheckBox
    fAddToolbar: RawByteString;
    procedure SubButtonClick(Sender: TObject);
    // avoid Windows Vista and Seven screen refresh bug (at least with Delphi 7)
    procedure WMUser(var Msg: TMessage); message WM_USER;
  public
    BtnSave: TSynButton;
    BtnCancel: TSynButton;
    /// creator may define this property to force a particular node to
    // be selected at form showing
    SelectedNodeObjectOnShow: TObject;
    /// create corresponding nodes and components for updating Obj
    // - to be used by OnComponentCreate(nil,nil,OptionsForm) in order
    // to populate the object tree of this Form
    // - properties which name starts by '_' are not added to the UI window
    // - published properties of parents of Obj are also added
    function AddEditors(Node: TTreeNode; Obj: TObject;
      const aCustomCaption: string=''; const aTitle: string=''): TTreeNode;
    /// create corresponding checkboxes lists for a given action toolbar
    // - aEnum points to the Action RTTI
    // - aActionHints is a multi line value containing the Hint captions
    // for all available Actions
    // - if aActionsBits is not nil, its bits indicates the Buttons to
    // appear in the list
    procedure AddToolbars(Scroll: TScrollBox; const aToolbarName: string;
      aEnum: PTypeInfo; const aActionHints: string; aActionsBits: pointer;
      aProp: PPropInfo; Obj: TObject);
  end;


implementation

{$R *.dfm}

procedure TOptionsForm.FormShow(Sender: TObject);
var i, n: integer;
    Name: string;
begin
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;
  try
    if not Assigned(OnComponentCreate) then begin // nothing to modify
      ModalResult := mrCancel; // code implementation error -> cancel
      exit;
    end;
    OnComponentCreate(nil,nil,self); // will call AddEditors()
    List.FullCollapse;
    List.TopItem.Expand(false); // show main sub nodes
    if List.Items.Count<>0 then begin
      n := List.Items.Count-1; // select the last entered item by default
      if SelectedNodeObjectOnShow<>nil then begin
        Name := CaptionName(OnCaptionName,nil,SelectedNodeObjectOnShow);
        for i := 0 to n do
          if SameText(List.Items[i].Text,Name) then begin
            n := i;
            break;
          end;
      end;
      List.Selected := List.Items[n];
    end;
    SetStyle(self);
  finally
    Screen.Cursor := crDefault;
  end;
  PostMessage(Handle,WM_USER,0,0); // avoid Vista and Seven screen refresh bug
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  BtnSave := TSynButton.CreateKind(Self,cbRetry,312,432,100,41);
  BtnSave.SetBitmap(BitmapOK);
  BtnSave.Font.Style := [fsBold];
  BtnSave.Caption := sSave;
  BtnSave.OnClick := BtnSaveClick;
  BtnSave.Anchors := [akRight, akBottom];
  BtnCancel := TSynButton.CreateKind(Self,cbCancel,424,432,100,41);
  BtnCancel.Anchors := [akRight, akBottom];
end;

procedure TOptionsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  for i := 0 to List.Items.Count-1 do
    TScrollBox(List.Items[i].Data).Free;
  List.Items.Clear;
end;

const
  /// X coordinates of the field content (left side must be used for caption)
  AddEditorsX=200;

function TOptionsForm.AddEditors(Node: TTreeNode; Obj: TObject;
  const aCustomCaption, aTitle: string): TTreeNode;
var i, j, CW: integer;
    P: PPropInfo;
    E: PEnumType;
    EP: PShortString;
    C: TWinControl;
    CLE: TLabeledEdit absolute C;
    CNE: TSynLabeledEdit absolute C;
    CC: TCheckbox absolute C;
    CB: TCombobox absolute C;
    aCaption, SubCaption, CustomCaption: string;
    Scroll: TScrollBox;
    O: TObject;
    aClassType: TClass;
procedure AddEditor(Obj: TObject; Index: integer);
begin
  if OnComponentCreate(Obj,nil,nil)<>nil then
    exit; // ignore this Object
  with TSynButton.Create(Scroll) do begin
    Parent := Scroll;
    Scroll.Tag := Scroll.Tag+4;
    SetBounds(AddEditorsX,Scroll.Tag,140,20);
    Caption := CaptionName(OnCaptionName,nil,Obj,Index);
    Tag := PtrInt(AddEditors(result,Obj,Caption,SubCaption));
    OnClick := SubButtonClick;
    Scroll.Tag := Scroll.Tag+24;
  end;
end;
begin
  if (self=nil) or (Obj=nil) or not Assigned(OnComponentCreate) then
    exit;
  Scroll := TScrollBox.Create(self);
  Scroll.Parent := self;
  Scroll.Visible := false;
  Scroll.SetBounds(List.Width,0,ClientWidth-List.Width,List.Height);
  Scroll.Anchors := [akLeft,akTop,akRight,akBottom];
  CW := Scroll.ClientWidth;
  if aCustomCaption='' then
    CustomCaption := CaptionName(OnCaptionName,nil,Obj) else
    CustomCaption := aCustomCaption;
  if Node=nil then
    SubCaption := '' else
    if aTitle='' then
      SubCaption := CustomCaption else
      SubCaption := aTitle+' - '+CustomCaption;
  result := List.Items.AddChild(Node,CustomCaption);
  result.Data := pointer(Scroll);
  with TLabel.Create(Scroll) do begin
    Parent := Scroll;
    Font.Style := [fsBold];
    Font.Size := 12;
    Left := 32;
    Top := 8;
    if SubCaption='' then
      Caption := CustomCaption else
      Caption := SubCaption;
  end;
  with TBevel.Create(Scroll) do begin
    Parent := Scroll;
    SetBounds(8,32,CW-32,4);
    Shape := bsTopLine;
  end;
  Scroll.Tag := 48;
  aClassType := PPointer(Obj)^;
  while (aClassType<>nil) and
    (aClassType<>TComponent) and // TComponent have Name and Tag to be ignored
    (aClassType<>TObject) do // TObject don't have any published properties
  with InternalClassProp(aClassType)^ do begin
    P := @PropList;
    for i := 1 to PropCount do
    if P^.Name[1]<>'_' then begin // ignore properties which name starts by _
      aCaption := CaptionName(OnCaptionName,@P^.Name);
      if not Assigned(OnComponentCreate) then
        C := nil else 
        C := OnComponentCreate(Obj,P,Scroll);
      if C=nil then // default creation if not handled by OnComponentCreate()
      case P^.PropType^^.Kind of
        tkInteger: begin
          CNE := TSynLabeledEdit.Create(Scroll);
          CNE.Kind := sleInteger;
          CNE.Value := P^.GetOrdValue(Obj);
          CNE.RaiseExceptionOnError := true; // force show errors on screen
        end;
        tkEnumeration:
          if (P^.PropType^=TypeInfo(boolean)) then begin
            CC := TCheckBox.Create(Scroll);
            CC.Checked := boolean(P^.GetOrdValue(Obj));
          end else begin
            E := P^.PropType^^.EnumBaseType;
            CB := TComboBox.Create(Scroll);
            CB.Parent := Scroll; // need parent now for CB.Items access
            CB.Style := csDropDownList;
            EP := @E^.NameList;
            for j := 0 to E^.MaxValue do begin
              CB.Items.Add(CaptionName(OnCaptionName,EP));
              inc(PtrInt(EP),ord(EP^[0])+1); // next enumeration item
            end;
            CB.ItemIndex := P^.GetOrdValue(Obj);
          end;
        tkLString: begin
            CLE := TLabeledEdit.Create(Scroll);
            if P^.PropType^=TypeInfo(RawUTF8) then
              CLE.Text := U2S(P^.GetLongStrValue(Obj)) else
              CLE.Text := P^.GetGenericStringValue(Obj);
          end;
        {$ifdef UNICODE}
        tkUString: begin
            CLE := TLabeledEdit.Create(Scroll);
            CLE.Text := P^.GetUnicodeStrValue(Obj);
          end;
        {$endif}
        tkClass: begin
          O := pointer(P^.GetOrdValue(Obj));
          if (O<>nil) and (PtrInt(O)<>-1) then
            if O.InheritsFrom(TCollection) then
            with TCollection(O) do
              for j := 0 to Count-1 do
                AddEditor(Items[j],j)
            else
              AddEditor(O,-1);
        end;
      end;
      if (C<>nil) and (C<>self) and (C<>Obj) then begin
        C.Parent := Scroll;
        C.Tag := PtrInt(P);  // for BtnSaveClick() event
        if Assigned(OnComponentCreated) then
          OnComponentCreated(Obj,P,C);
        if C.InheritsFrom(TLabeledEdit) then begin
          CLE.EditLabel.Caption := aCaption;
          CLE.LabelPosition := lpLeft;
        end else
        with TLabel.Create(Scroll) do begin
          Parent := Scroll;
          Caption := aCaption;
          SetBounds(8,Scroll.Tag+4,AddEditorsX-12,Height);
          Alignment := taRightJustify;
          if not C.Enabled then
            Enabled := false;
        end;
        if C.InheritsFrom(TCheckBox) then // trick to avoid black around box
          CC.SetBounds(AddEditorsX,Scroll.Tag+5,13,13) else
          C.SetBounds(AddEditorsX,Scroll.Tag,160,22);
        Scroll.Tag := Scroll.Tag+24;
      end;
      P := P^.Next;
    end;
    aClassType := aClassType.ClassParent; // also add parents properties
  end;
  with TBevel.Create(Scroll) do begin // draw a line at the bottom 
    Parent := Scroll;
    SetBounds(8,Scroll.Tag+8,CW-32,16);
    Shape := bsTopLine;
  end;
  Scroll.Tag := PtrInt(Obj); // for BtnSaveClick()
end;

procedure TOptionsForm.AddToolbars(Scroll: TScrollBox; const aToolbarName: string;
  aEnum: PTypeInfo; const aActionHints: string; aActionsBits: pointer;
  aProp: PPropInfo; Obj: TObject);
var W: integer;
    A: integer;
    E: PEnumType;
    V: Int64;
begin
  if (Self=nil) or (Scroll=nil) or (aEnum=nil) or (aProp=nil) then
    exit;
  if aProp^.PropType^^.Kind<>tkInt64 then
    exit;
  W := Scroll.Width-128;
  Scroll.Tag := Scroll.Tag+12;
  with TLabel.Create(Scroll) do begin
    Parent := Scroll;
    Font.Style := [fsBold];
    Caption := aToolbarName;
    SetBounds(32,Scroll.Tag,W,22);   // Scroll.Tag = current Y in this scrollbox
  end;
  Scroll.Tag := Scroll.Tag+24;
  E := aEnum^.EnumBaseType;
  assert(E^.MaxValue<64); // Value is a Int64 (i.e. max 64 actions)
  for A := 0 to E^.MaxValue do
  if (aActionsBits=nil) or GetBit64(aActionsBits^,A) then begin
    with TCheckBox.Create(Scroll) do begin
      Parent := Scroll;
      Hint := GetCSVItemString(pointer(aActionHints),A,#13);
      ShowHint := true;
      Caption := E^.GetCaption(A);
      SetBounds(64,Scroll.Tag,W,16);
      V := aProp^.GetInt64Value(Obj);
      fAddToolbar := fAddToolbar+AnsiChar(A); // every char is a bit index
      Checked := GetBit64(V,A);
      Tag := PtrInt(aProp);  // for BtnSaveClick() event
    end;
    Scroll.Tag := Scroll.Tag+16;
  end;
end;

procedure TOptionsForm.ListClick(Sender: TObject);
var i: integer;
    S,N: TTreeNode;
begin
  S := List.Selected;
  if S=nil then
    exit;
  for i := 0 to List.Items.Count-1 do begin
    N := List.Items[i];
    if N<>S then
      TScrollBox(N.Data).Hide;
  end;
  with TScrollBox(S.Data) do begin
    Show;
    for i := 0 to ControlCount-1 do
      Controls[i].Repaint; // avoid Vista and Seven screen refresh bug
  end;
end;

procedure TOptionsForm.SubButtonClick(Sender: TObject);
begin
  if TSynButton(Sender).Tag=0 then
    exit;
  List.Select(TTreeNode(TSynButton(Sender).Tag));
  ListClick(nil);
end;

procedure TOptionsForm.BtnSaveClick(Sender: TObject);
var i, j, Index, IndexSorted, ToolbarIndex: integer;
    P: PPropInfo;
    Scroll: TScrollBox;
    Obj: TObject;
    C: TControl;
    CLE: TLabeledEdit absolute C;
    CNE: TSynLabeledEdit absolute C;
    CC: TCheckbox absolute C;
    CB: TCombobox absolute C;
    ToolbarValue: Int64;
begin // update the properties of the settings object from screen
  ToolbarIndex := 0;
  for i := 0 to List.Items.Count-1 do begin
    Scroll := TScrollBox(List.Items[i].Data);
    if Scroll=nil then continue;
    Obj := pointer(Scroll.Tag); // get corresponding Object to update properties
    if Obj=nil then continue;
    for j := 0 to Scroll.ControlCount-1 do begin
      C := Scroll.Controls[j];
      if not C.Enabled then
        continue; // disabled components didn't modify their value
      P := pointer(C.Tag); // get corresponding PPropInfo
      if P=nil then
        continue; // not a value component (label or button)
      if C.InheritsFrom(TSynLabeledEdit) then
      try
        P^.SetOrdValue(Obj,CNE.Value); // call CNE.GetValue for range checking
      except
        on E: ESynLabeledEdit do begin // trigerred by CNE.GetValue
          List.Selected := List.Items[i]; // focus corresponding scroll
          ListClick(nil);
          Application.ProcessMessages;
          CNE.SetFocus;                   // focus corresponding field
          ShowMessage(CNE.EditLabel.Caption+':'#13+E.Message,true);
          exit;
        end;
      end else
      if C.InheritsFrom(TLabeledEdit) then
        {$ifdef UNICODE}
        if P^.PropType^^.Kind=tkUString then
          P^.SetUnicodeStrValue(Obj,CLE.Text) else
        {$endif}
          if P^.PropType^=TypeInfo(RawUTF8) then
            P^.SetLongStrValue(Obj,S2U(CLE.Text)) else
            P^.SetGenericStringValue(Obj,CLE.Text) else
      if C.InheritsFrom(TCheckBox) then
        if P^.PropType^^.Kind=tkInt64 then begin
          // created by AddToolbars() method -> set bit if checked
          inc(ToolbarIndex); // follows the same order as in AddToolbars()
          if ToolbarIndex<=length(fAddToolbar) then begin
            ToolbarValue := P^.GetInt64Value(Obj);
            if CC.Checked then
              SetBit64(ToolbarValue,ord(fAddToolbar[ToolbarIndex])) else
              UnsetBit64(ToolbarValue,ord(fAddToolbar[ToolbarIndex]));
            P^.SetInt64Value(Obj,ToolbarValue);
          end;
        end else
          P^.SetOrdValue(Obj,integer(CC.Checked)) else
      if C.InheritsFrom(TComboBox) then
      if P^.PropType^^.Kind=tkLString then begin
        // don't store index but string value
        P^.SetLongStrValue(Obj,S2U(CB.Text));
      end else begin
        Index := CB.ItemIndex; // store index value
        if Index>=0 then begin
          IndexSorted := PtrInt(CB.Items.Objects[Index]);
          if IndexSorted<>0 then // Objects[] = original index+1 (if sorted)
            Index := IndexSorted-1; // store raw index, before sort
          P^.SetOrdValue(Obj,Index);
        end;
      end;
    end;
  end;
  ModalResult := mrOk; // close window if all OK
end;

procedure TOptionsForm.WMUser(var Msg: TMessage);
begin // avoid Vista and Seven screen refresh bug
  ListClick(nil);
end;

end.
