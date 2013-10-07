/// Form handling queries to a User Interface Grid
// - this unit is a part of the freeware Synopse SQLite3 database framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.15
unit SQLite3UIQuery;

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


  Version 1.5
  - first implementation of the TQueryForm class: handle default and
    custom queries

  Version 1.9
  - custom queries can now handle standard operators, following a specified set
  - TSQLQueryEvent() usage has therefore been modified for custom queries

  Version 1.15
  - now use TMS component pack only if USETMSPACK global conditional is defined:
    by default, will use only VCL components (i.e. TSynButton) for the form
  - handle TModTime published property / sftModTime SQL field
  - handle TCreateTime published property / sftCreateTime SQL field

*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  SynCommons, SynTaskDialog,
  SQLite3Commons, SQLite3UI, SQLite3UILogin, SQLite3i18n,
  StdCtrls, ComCtrls;

type
  /// this Form perform simple Visual queries to a Grid
  // - mark or unmark items, depending of the input of the User on this form
  // - use TSQLRest.QueryIsTrue() method for standard fields and parameters
  // - use TSQLQueryCustom records previously registered to the TSQLRest class,
  // by the TSQLRest.QueryAddCustom() method, to add some custom field search
  // (e.g. to search into fields not available on the grid, or some data
  // embedded inside a field - like .INI-like section entries)
  // - in practice, the query is very fast (immediate for standard fields and
  // parameters), but can demand some bandwith for custom field search (since
  // data has to be retrieved from the server to search within)
  TQueryForm = class(TVistaForm)
    Label1: TLabel;
    Where: TComboBox;
    Label2: TLabel;
    Operation: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    MarkedOnly: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure WhereChange(Sender: TObject);
    procedure BtnMarkClick(Sender: TObject);
  protected
    Table: TSQLTable;
    TableClass: TSQLRecordClass;
    TableToGrid: TSQLTableToGrid;
    Client: TSQLRest;
    FieldType: TSQLFieldType;
    FieldIndex: integer;
    Reference: TWinControl;
    BtnMark:   TSynButton;
    BtnUnmark: TSynButton;
    BtnCancel: TSynButton;
    QueryOpEnumType: PEnumType;
    /// override this if you want to select which enumerates values must
    // appear in the "What to search" criteria
    // - return nil by default, i.e. all enumerates must be added
    // - can return a pointer to a set of enumerates to be shown
    function EnumerateUsedBits(EnumType: PEnumType): pointer; virtual;
  public
    /// create the window instance
    // - all parameters (especially TSQLRest instance to use for custom search)
    // are retrieved via the supplied TSQLTableToGrid
    // - caller must have used TSQLRest.QueryAddCustom() method to register
    // some custom queries, if necessary
    constructor Create(aOwner: TComponent; aTableToGrid: TSQLTableToGrid); reintroduce;
  end;
                      


implementation

{$R *.dfm}

resourcestring
  sBtnMark = 'MARK if match';
  sBtnUnMark = 'UNMARK if match';

constructor TQueryForm.Create(aOwner: TComponent; aTableToGrid: TSQLTableToGrid);
var i: integer;
begin
  inherited Create(aOwner);
  BtnMark := TSynButton.Create(Self);
  with BtnMark do begin
    Parent := self;
    SetBounds(48,264,145,33);
    Caption := sBtnMark;
    ModalResult := mrOk;
    TabOrder := 3;
    OnClick := BtnMarkClick
  end;
  BtnUnMark := TSynButton.Create(Self);
  with BtnUnMark do begin
    Parent := self;
    SetBounds(48,304,145,33);
    Caption := sBtnUnMark;
    ModalResult := mrOk;
    TabOrder := 4;
    OnClick := BtnMarkClick
  end;
  BtnCancel := TSynButton.CreateKind(self,cbCancel,200,304,89,33);
  if aTableToGrid=nil then exit;
  TableToGrid := aTableToGrid;
  Table := aTableToGrid.Table;
  Client := aTableToGrid.Client;
  for i := 0 to Table.FieldCount-1 do
    Where.Items.Add(Table.GetCaption(0,i));
  if Assigned(Table.QueryTables) then begin
    TableClass := TSQLRecordClass(Table.QueryTables[0]);
    for i := 0 to high(Client.QueryCustom) do
    with Client.QueryCustom[i] do
      if Event(TableClass,0,sftUnknown,nil,i,nil) then
        // if this custom query is available to this table
        Where.Items.Add(EnumType^.GetCaption(EnumIndex));
  end;
  MarkedOnly.Enabled := TableToGrid.MarkAvailable;
  QueryOpEnumType := PTypeInfo(TypeInfo(TSQLQueryOperator))^.EnumBaseType;
end;

procedure TQueryForm.FormShow(Sender: TObject);
begin
  if Where.Items.Count>0 then begin
    Where.ItemIndex := 0;
    WhereChange(nil);
  end else begin
    BtnMark.Enabled := false;
    BtnUnMark.Enabled := false;
  end;
  SetStyle(self);
end;

procedure TQueryForm.WhereChange(Sender: TObject);
var i, max: integer;
    EnumType: PEnumType;
    none: TSQLQueryOperator;
begin
  Operation.Clear;
  FieldIndex := Where.ItemIndex;
  if (Table=nil) or (cardinal(FieldIndex)>=cardinal(Table.FieldCount)) then begin
    // QueryCustom[] or no Field -> add specified Operator(s)
    FieldType := sftUTF8Text;
    Dec(FieldIndex,Table.FieldCount);
    if cardinal(FieldIndex)<=cardinal(high(Client.QueryCustom)) then
      for i := Ord(qoEqualTo) to QueryOpEnumType^.MaxValue do
        if TSQLQueryOperator(i) in Client.QueryCustom[FieldIndex].Operators then
          Operation.Items.AddObject(QueryOpEnumType^.GetCaption(i),pointer(i));
    Operation.Enabled := Operation.Items.Count>0;
    if not Operation.Enabled then begin
      none := qoNone;
      Operation.Items.Add(QueryOpEnumType^.GetCaption(none));
    end;
    Label2.Enabled := Operation.Enabled;
    FieldIndex := -1; // mark custom field
  end else begin
    // add "basic" operations from standard TSQLTable Field
    FieldType := Table.FieldType(FieldIndex,@EnumType);
    Operation.Enabled := true;
    Label2.Enabled := true;
    if FieldType in TEXT_FIELDS then
      max := QueryOpEnumType^.MaxValue else  // allow textual operations
      max := ord(qoGreaterThanOrEqualTo);    // numeric operations only
    for i := ord(qoEqualTo) to max do
      Operation.Items.AddObject(QueryOpEnumType^.GetCaption(i),pointer(i));
  end;
  Operation.ItemIndex := 0;
  // create corresponding Reference field
  FreeAndNil(Reference);
  case FieldType of
    sftDateTime, sftTimeLog, sftModTime, sftCreateTime: begin
      Reference := TDateTimePicker.Create(self);
      Reference.Parent := self;
    end;
    sftEnumerate:
    if EnumType<>nil then begin
      Reference := TComboBox.Create(self);
      with TComboBox(Reference) do begin
        Parent := self;
        Style := csDropDownList;
        EnumType.AddCaptionStrings(Items,EnumerateUsedBits(EnumType));
        ItemIndex := 0;
        DropDownCount := 32;
      end;
    end;
  end;
  if Reference=nil then begin
    // default reference
    Reference := TEdit.Create(self);
    Reference.Parent := self;
  end;
  Reference.SetBounds(48,200,169,21);
end;

procedure TQueryForm.BtnMarkClick(Sender: TObject);
var Row, i: integer;
    SoundEx: TSynSoundEx;
    Ope: integer;
    Ref: RawUTF8;
    R, V: PUTF8Char;
    aEvent: TSQLQueryEvent;
    AllRows, OK, ToBeMarked: boolean;
const CONVERT: array[qoSoundsLikeEnglish..qoSoundsLikeSpanish] of
  TSynSoundExPronunciation = (sndxEnglish,sndxFrench,sndxSpanish);
begin
  if Table=nil then exit;
  ToBeMarked := (Sender=BtnMark); // other possibility is BtnUnmark -> false
  AllRows := not (MarkedOnly.Enabled and MarkedOnly.Checked);
  if Operation.Enabled then begin
    // normal operator search
    Ope := integer(Operation.Items.Objects[Operation.ItemIndex]);
    if Reference.InheritsFrom(TEdit) then
      Ref := Language.StringToUTF8(TEdit(Reference).Text) else
    if Reference.InheritsFrom(TComboBox) then begin
      i := TComboBox(Reference).ItemIndex;
      if i<0 then
        Exit; // avoid out of range error
      Ref := IntToStr(Integer(TComboBox(Reference).Items.Objects[i]));
    end else
    if Reference.InheritsFrom(TDateTimePicker) then
      with TDateTimePicker(Reference) do
      case FieldType of
        sftDateTime:
          Ref := DateTimeToIso8601(DateTime,false);
        sftTimeLog, sftModTime, sftCreateTime:
          Ref := IntToStr(Iso8601FromDateTime(DateTime));
      end;
    Ref := Trim(Ref);
    if Ref='' then begin
      ModalResult := mrNone;
      exit;
    end;
    R := pointer(Ref);
    case TSQLQueryOperator(Ope) of
      qoContains, qoBeginWith: begin // expected to be already uppercase
        Ref := UpperCase(Ref);
        R := pointer(Ref);
      end;
      qoSoundsLikeEnglish..qoSoundsLikeSpanish: begin
        Ref := UpperCase(Ref);
        SoundEx.Prepare(pointer(Ref),CONVERT[TSQLQueryOperator(Ope)]);
        R := pointer(@Soundex); // typecast of the prepared TSynSoundEx object
      end;
    end;
  end else begin
    // custom query with no Operator
    if not Reference.InheritsFrom(TEdit) then
      exit;
    Ope := 0;
    Ref := Language.StringToUTF8(Trim(TEdit(Reference).Text));
    R := pointer(Ref);
    assert(FieldIndex<0);
  end;
  if FieldIndex<0 then begin
    i := Where.ItemIndex-Table.FieldCount;
    if cardinal(i)>cardinal(high(Client.QueryCustom)) then
      exit;
    with Client.QueryCustom[i] do begin
      FieldType := TSQLFieldType(EnumIndex+64);
      aEvent := Event;
    end;
  end else begin
    aEvent := Client.QueryIsTrue;
  end;
  V := R; // we need Value<>nil
  Screen.Cursor := crHourGlass; // immediate for standard query, but how knows?
  try
    for Row := 1 to Table.RowCount do
    if AllRows or TableToGrid.Marked[Row] then begin
      if FieldIndex>=0 then
        V := Table.Get(Row,FieldIndex);
      OK := aEvent(TableClass,Table.IDColumnHiddenValue(Row), FieldType, V, Ope, R);
      if not AllRows and ToBeMarked then    // BtnMark + MarkOnly.Checked -> reset
        TableToGrid.Marked[Row] := OK  else
      if OK then
        TableToGrid.Marked[Row] := ToBeMarked; // set/reset from BtnMark/BtnUnmark
    end;
    TableToGrid.DrawGrid.Invalidate; // refresh screen
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TQueryForm.EnumerateUsedBits(EnumType: PEnumType): pointer;
begin
  result := nil; // no custom selection by default
end;

end.
