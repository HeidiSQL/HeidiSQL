/// Record edition dialog, used to edit record content on the screen
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SQLite3UIEdit;

(*
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
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


  Version 1.9
  - Initial Release, handling most common kind of SQL field (sftRecord,
    sftTimeLog, sftCurrency, sftDateTime, sftFloat and sftBlob) are not handled
    yet, because is not needed; perhaps sftTimeLog, sftCurrency, sftDateTime and
    sftFloat should be handled in the future (using TDateTimePicker or a
    to be written TSynExtendedLabeledEdit components)
  - all user interface (fields, layout, etc...) is created from RTTI data and
    some custom parameters

  Version 1.9.2
  - handle an optional caption for the window (by default, the caption is
    guessed from the record type)
  - handle display without any associated Client/Model/Ribbon, that is allow
    direct edition of any TSQLRecord child
  - guess the better TGroupBox width on screen for set of enumerates properties

  Version 1.13
  - new OnComponentValidate property to allow custom field content validation
  - now handle TSQLRecord automated filtering (using TSynFilter classes) and
    validation (using TSynValidate classes)
  - unique field validation is now in TSQLRecord.Validate (better multi-tier
    architecture)
  - hanle sftTimeLog and sftDateTime with a TDateTimePicker
  - handle sftInteger (including Int64 fields), sftCurrency and sftFloat with
    a TSynLabeledEdit field
  - now use TMS component pack only if USETMSPACK global conditional is defined:
    by default, will use only VCL components (i.e. TSynButton) for the form

  Version 1.14
  - fixed issue with sftCurrency kind of property
  - fixed issue with sftSet kind of property

  Version 1.15
  - first editable component is now focused by default
  - handle TModTime published property / sftModTime SQL field
  - handle TCreateTime published property / sftCreateTime SQL field

  Version 1.16
  - moved OnComponentCreated event call after all component initialization
    (allow adding paired components on purpose)


*)


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
{$ifdef USETMSPACK}
  TaskDialog,
{$endif}
  SynCommons, SQLite3Commons,
  SQLite3UILogin, SQLite3UI, SQLite3i18n, SQLite3ToolBar,
  SynTaskDialog, StdCtrls, ExtCtrls, ImgList, ComCtrls;

type
  /// Event used to customize the input component after creation
  TOnComponentCreated = procedure(Obj: TObject; Prop: PPropInfo; Comp: TWinControl) of object;
  /// Event used for the window creation
  TOnComponentCreate = function(Obj: TObject; Prop: PPropInfo; Parent: TWinControl): TWinControl of object;

  /// Event used for individual field validation
  // - must return TRUE if the specified field is correct, FALSE if the content
  // is to be modified
  // - it's up to the handler to inform the user that this field is not correct,
  // via a popup message for instance
  // - you should better use the TSQLRecord.AddFilterOrValidate() mechanism,
  // which is separated from the UI (better multi-tier architecture)
  TOnComponentValidate = function(EditControl: TWinControl; Prop: PPropInfo): boolean of object;


  /// a common ancestor, used by both TRecordEditForm and TOptionsForm
  TRTTIForm = class(TVistaForm)
  public
    /// this event is used to customize screen text of property names
    OnCaptionName: TOnCaptionName;
    /// this event is used to customize the input components creation
    // - this event is also triggerred once at the creation of the Option window,
    // with Obj=Prop=nil and Parent=TOptionsForm: the event must
    // call method Parent.AddEditors() / Parent.SetRecord() to add fields to the
    // Option (this is not mandatory to the Record Edit window)
    // - this event is triggered once for every object, with Prop=nil,
    // and should return nil if the object is to be added to the dialog,
    // and something not nil if the object is to be ignored
    // (same as a runtime-level _Name object)
    // - this is the only mandatory event of this component, for TOptionsForm
    // - this event is not mandatory for TRecordEditForm (you can call
    // its SetRecord method directly)
    OnComponentCreate: TOnComponentCreate;
    /// this event is used to customize the input components after creation
    // - triggered when the component has been created
    // - can be used to disabled the component if user don't have the right
    // to modify its value; but he/she will still be able to view it
    OnComponentCreated: TOnComponentCreated;
  end;

  {{ Record edition dialog, used to edit record content on the screen
   - the window content is taken from the RTTI of the supplied record;
     all the User Interface (fields, etc...) is created from the class definition
     using RTTI: published properties are displayed as editing components
   - caller must initialize some events, OnComponentCreate at least,
    in order to supply the objects to be added on the form
   - components creation is fully customizable by some events }
  TRecordEditForm = class(TRTTIForm)
    BottomPanel: TPanel;
    Scroll: TScrollBox;
    procedure FormShow(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    fRec: TSQLRecord;
    fClient: TSQLRestClient;
    fOnComponentValidate: TOnComponentValidate;
    /// as created by SetRecord()
    fFieldComponents, fFieldComponentsTwin: array of TWinControl;
    fFieldCaption: array of string;
    BtnSave: TSynButton;
    BtnCancel: TSynButton;
    // avoid Windows Vista and Seven screen refresh bug (at least with Delphi 7)
    procedure WMUser(var Msg: TMessage); message WM_USER;
  public
    /// create the corresponding components on the dialog for editing a Record
    // - to be used by OnComponentCreate(nil,nil,EditForm) in order
    // to populate the object tree of this Form
    // - create field on the window for all published properties of the
    // supplied TSQLRecord instance
    // - properties which name starts by '_' are not added to the UI window
    // - user can customize the component creation by setting the
    // OnComponentCreate / OnComponentCreated events
    // - the supplied aRecord instance must be available during all the
    // dialog window modal apparition on screen
    // - by default, all published fields are displayed, but you can specify
    // a CSV list in the optional CSVFieldNames parameter
    // - editor parameters are taken from the optional Ribbon parameter,
    // and its EditFieldHints/EditExpandFieldHints/EditFieldNameWidth properties
    // - if Ribbon is nil, FieldHints may contain the hints to be displayed on
    // screen (useful if your record is not stored in any TSQLRestClient, but
    // only exists in memory); you can set FieldNamesWidth by hand in this case
    procedure SetRecord(aClient: TSQLRestClient; aRecord: TSQLRecord;
      CSVFieldNames: PUTF8Char=nil; Ribbon: TSQLRibbon=nil;
      FieldHints: string=''; FieldNamesWidth: integer=0; aCaption: string='');

    /// the associated Record to be edited
    property Rec: TSQLRecord read fRec;
    /// the associated database Client, used to access remote data
    property Client: TSQLRestClient read fClient;
    /// event called to check if the content of a field on form is correct
    // - is checked when the user press the "Save" Button
    // - if returns false, component is focused and window is not closed
    property OnComponentValidate: TOnComponentValidate read fOnComponentValidate write fOnComponentValidate;
  end;


resourcestring
  sSave = 'Save';
  sEdit = 'Edit';
  sVerb = '%s %s';
  sInvalidFieldN = 'Invalid "%s" Field';


implementation

{$R *.dfm}

{ TRecordEditForm }

procedure TRecordEditForm.SetRecord(aClient: TSQLRestClient;
  aRecord: TSQLRecord; CSVFieldNames: PUTF8Char=nil; Ribbon: TSQLRibbon=nil;
  FieldHints: string=''; FieldNamesWidth: integer=0; aCaption: string='');
var i,j, aID, Y, aHeight, aWidth, CW: integer;
    RibbonParams: PSQLRibbonTabParameters;
    ExpandFieldHints: boolean;
    E: PEnumType;
    EP: PShortString;
    Group: TGroupBox;
    C: TWinControl;
    CLE: TLabeledEdit absolute C;
    CNE: TSynLabeledEdit absolute C;
    CC: TCheckbox absolute C;
    CB: TCombobox absolute C;
    CD: TDateTimePicker absolute C;
    CD2: TDateTimePicker;
    TimeLog: Iso8601;
    aClassType: TSQLRecordClass;
    SetMax: cardinal;
    Sets: Cardinal;
    IDClass: TSQLRecordClass;
    aHint: string;
    aName: RawUTF8;
    FieldNameToHideCSV: PUTF8Char;
    P: PPropInfo;
    PHint: PChar; // map FieldHints
begin
  if (self=nil) or (aRecord=nil) then
    exit; // avoid GPF
  RibbonParams := Ribbon.GetParameter(aRecord.RecordClass);
  if RibbonParams=nil  then begin
    ExpandFieldHints := (FieldHints<>'');
    FieldNameToHideCSV := nil;
    if FieldNamesWidth=0 then
      FieldNamesWidth := 200; // default value
  end else
  with RibbonParams^ do begin
    FieldNamesWidth := EditFieldNameWidth;
    if FieldNamesWidth=0 then
      FieldNamesWidth := 200; // default value
    if EditFieldHints<>nil then
      FieldHints := LoadResString(EditFieldHints);
    ExpandFieldHints := EditExpandFieldHints;
    FieldNameToHideCSV := pointer(EditFieldNameToHideCSV);
  end;
  fRec := aRecord;
  fClient := aClient;
  CW := Scroll.ClientWidth;
  aName := aClient.MainFieldValue(aRecord.RecordClass,aRecord.ID,true);
  if aCaption='' then begin
    if Caption='' then
      aCaption := sEdit else
      aCaption := Caption;
    aCaption := format(sVerb,[aCaption,aRecord.CaptionName]);
    if aName<>'' then
      aCaption := aCaption+' - '+U2S(aName); // add current record name
  end;
  Caption := '  '+aCaption;
  with TStaticText.Create(Scroll) do begin
    Parent := Scroll;
    Alignment := taCenter;
    Font.Style := [fsBold];
    Font.Size := 12;
    Font.Color := clTeal;
    Caption := aCaption;
    SetBounds(8,16,CW-48,Height);
    Y := Top+Height+16;
  end;
  with TBevel.Create(Scroll) do begin
    Parent := Scroll;
    SetBounds(8,Y-12,CW-32,4);
    Shape := bsTopLine;
  end;
  aClassType := PPointer(aRecord)^;
  dec(CW,FieldNamesWidth+32);
  PHint := pointer(FieldHints);
  with aClassType.RecordProps do begin
    SetLength(fFieldComponents,length(Fields));
    SetLength(fFieldCaption,length(Fields));
    for i := 0 to High(Fields) do begin
      aHint := GetNextItemString(PHint,'|'); // ALL fields are listed: do it now
      P := Fields[i];
      aName := FieldsName[i];
      if ((FieldType[i] in [ // must match "case FieldType[i] of" below
          sftRecord, sftBlob, sftBlobDynArray, sftMany]) and
           not Assigned(OnComponentCreate)) or
         ((FieldNameToHideCSV<>nil) and
          (FindCSVIndex(FieldNameToHideCSV,aName,',',false)>=0)) or
         ((CSVFieldNames<>nil) and
          (FindCSVIndex(CSVFieldNames,aName,',',false)<0)) then
        continue; // display properties listed in optional CSVFieldNames parameter
      aCaption := CaptionName(OnCaptionName,@P^.Name);
      fFieldCaption[i] := aCaption;
      if (aHint<>'') and ExpandFieldHints then
        with TLabel.Create(Scroll) do begin // show hint above field
          Parent := Scroll;
          Font.Color := clNavy;
          Font.Size := 8;
          AutoSize := True;
          WordWrap := true;
          SetBounds(FieldNamesWidth,Y+8,CW-32,24);
          Caption:= aHint;
          inc(Y,Height+10);
          aHint := ''; // mark hint displayed on window -> no popup needed
        end else
        inc(Y,10);
      aHeight := 24;
      // try custom component creation
      if not Assigned(OnComponentCreate) then
        C := nil else
        C := OnComponentCreate(aRecord,P,Scroll);
      if C=nil then begin
        // default creation from RTTI, if not handled by OnComponentCreate()
        case FieldType[i] of
          sftTimeLog, sftModTime, sftDateTime: begin
            CD := TDateTimePicker.Create(Scroll);
            CD.Kind := dtkDate;
            if FieldType[i]=sftDateTime then
              CD.DateTime := P^.GetExtendedValue(aRecord) else begin
              TimeLog.Value := P^.GetInt64Value(aRecord);
              CD.DateTime := TimeLog.ToDateTime;
            end;
          end;
          sftCreateTime:
            ; // is low-level read/only field by design
          sftBlob, sftMany:
            ; // not implemented yet
          sftRecord:
            ; // should be handled as a TRecordReference to another record
          sftBlobDynArray:
            ; // array of TSQLRecord should be handled as a list of IDs
              // array of RawUTF8/Integer/Int64 as a list of text or integers
              // array of RegisterCustomJSONSerializer as a list of JSON fields
          sftInteger, sftCurrency, sftFloat: begin
              // numerical fields are handled via a TSynLabeledEdit component
              CNE := TSynLabeledEdit.Create(Scroll);
              case P^.PropType^^.Kind of
                tkInteger: begin
                  CNE.Kind := sleInteger;
                  CNE.Value := P^.GetOrdValue(aRecord);
                end;
                tkInt64: begin
                  CNE.Kind := sleInt64;
                  CNE.Value := P^.GetInt64Value(aRecord)
                end;
                tkFloat:
                case FieldType[i] of
                  sftCurrency: begin
                    CNE.Kind := sleCurrency;
                    CNE.Value := P^.GetCurrencyValue(aRecord);
                  end;
                  sftFloat: begin
                    CNE.Kind := sleDouble;
                    CNE.Value := P^.GetExtendedValue(aRecord);
                  end;
                end;
              end;
              CNE.RaiseExceptionOnError := true; // force show errors on screen
            end;
          sftEnumerate: begin
            // enumeration is handled by a TComboBox with all possible values
            E := P^.PropType^^.EnumBaseType;
            CB := TComboBox.Create(Scroll);
            CB.Parent := Scroll; // need parent now for CB.Items access
            CB.Style := csDropDownList;
            EP := @E^.NameList;
            for j := 0 to E^.MaxValue do begin
              CB.Items.Add(CaptionName(OnCaptionName,EP));
              inc(PtrInt(EP),ord(EP^[0])+1); // next enumeration item
            end;
            CB.ItemIndex := P^.GetOrdValue(aRecord);
          end;
          sftID:
          if aClient<>nil then begin
            // ID field (TSQLRecord descendant) is handled by a TComboBox component
            // with all possible values of the corresponding TSQLRecord descendant
            IDClass := TSQLRecordClass(P^.PropType^^.ClassType^.ClassType);
            CB := TComboBox.Create(Scroll);
            CB.Parent := Scroll; // need parent now for CB.Items access
            CB.Style := csDropDownList;
            aID := P^.GetOrdValue(aRecord);
            with IDClass.RecordProps do
            if MainField[true]>=0 then begin
              aClient.OneFieldValues(IDClass,FieldsName[MainField[true]],'',CB.Items,@aID);
              CB.ItemIndex := aID; // @aID now contains the found index of aID
            end;
          end;
          sftSet: begin
            // enumeration set is handled by a TGroupBox component contaning one
            // TCheckBox for each enumeration value
            Group := TGroupBox.Create(Scroll); // add left-sided label
            Group.Parent := Scroll;
            Group.Font.Style := [fsBold];
            Group.Caption := ' '+aCaption+' ';
            Group.Tag := i+1; // for BtnSaveClick() event
            E := P^.PropType^^.SetEnumType;
            if E^.MaxValue>31 then // up to 32 elements in tkSet (GetOrdValue)
              SetMax := 31 else
              SetMax := E^.MaxValue;
            aWidth := 200;
            EP := @E^.NameList;
            for j := 0 to SetMax do begin
              if EP^[0]>#25 then begin
                aWidth := 250; // wider group box for large enumeration caption
                break;
              end;
              inc(PtrInt(EP),ord(EP^[0])+1); // next enumeration item
            end;
            Group.SetBounds(FieldNamesWidth,Y+4,aWidth,40+20*SetMax);
            dec(aWidth,20);
            Sets := P^.GetOrdValue(aRecord);
            EP := @E^.NameList;
            for j := 0 to SetMax do
              with TCheckBox.Create(Scroll) do begin // add set checkboxes
                Parent := Group;
                Font.Style := [];
                Caption := CaptionName(OnCaptionName,EP);
                SetBounds(16,16+20*j,aWidth,20);
                if aHint<>'' then begin
                  Hint := aHint;
                  ShowHint := True;
                end;
                Checked := GetBit(Sets,j);
                Enabled := Group.Enabled;
                Tag := i+1+(j+1) shl 8;  // for BtnSaveClick() event
                inc(PtrInt(EP),ord(EP^[0])+1); // next enumeration item
              end;
            fFieldComponents[i] := Group;
            if Assigned(OnComponentCreated) then // allow component customization
              OnComponentCreated(aRecord,P,Group); // e.g. set Group.Enabled := false
            inc(Y,Group.Height+12);
            continue;
          end;
          sftBoolean: begin
            // boolean is handled by a TCheckBox component
            CC := TCheckBox.Create(Scroll);
            CC.Parent := Scroll; // initialize font
            CC.Font.Style := [fsBold];
            CC.Checked := boolean(P^.GetOrdValue(aRecord));
            CC.Caption := aCaption;
          end;
          sftUTF8Text, sftAnsiText: begin
            // text field is handled by a  TLabeledEdit component
            CLE := TLabeledEdit.Create(Scroll);
            {$ifdef UNICODE}
            if P^.PropType^^.Kind=tkUString then
              CLE.Text := P^.GetUnicodeStrValue(aRecord) else
            {$endif}
              CLE.Text := U2S(P^.GetValue(aRecord,False)); // convert in GetValue()
          end;
        end;
      end;
      if (C<>nil) and (C<>self) and (C<>Scroll) then begin
        // we reached here if a component was added on screen for this field
        C.Parent := Scroll;
        C.Tag := i+1;  // for BtnSaveClick() event
        if aHint<>'' then begin
          C.Hint := aHint; // show hint text as popup
          C.ShowHint := true;
        end;
        if not C.InheritsFrom(TCheckBox) then
        if C.InheritsFrom(TLabeledEdit) then begin
          CLE.EditLabel.Font.Style := [fsBold];
          CLE.EditLabel.Caption := aCaption;
          CLE.LabelPosition := lpLeft;
        end else
        with TLabel.Create(Scroll) do begin // add label left-sided to the field
          Parent := Scroll;
          Font.Style := [fsBold];
          Caption := aCaption;
          SetBounds(8,Y+4,FieldNamesWidth-12,Height);
          Alignment := taRightJustify;
          if not C.Enabled then
            Enabled := false;
        end;
        if C.InheritsFrom(TCheckBox) then // trick to avoid black around box
          CC.SetBounds(FieldNamesWidth,Y,CW,CC.Height) else
        if C.InheritsFrom(TDateTimePicker) then begin
          CD.SetBounds(FieldNamesWidth,Y,96,22);
          if fFieldComponentsTwin=nil then
            SetLength(fFieldComponentsTwin,length(Fields));
          CD2 := TDateTimePicker.Create(Scroll);
          CD2.Parent := Scroll;
          CD2.Kind := dtkTime;
          CD2.DateTime := CD.DateTime;
          fFieldComponentsTwin[i] := CD2;
          CD2.SetBounds(FieldNamesWidth+100,Y,100,22);
        end else
          C.SetBounds(FieldNamesWidth,Y,200,22);
        fFieldComponents[i] := C;
        if Assigned(OnComponentCreated) then // allow component customization
          OnComponentCreated(aRecord,P,C); // e.g. set C.Enabled := false
        inc(Y,aHeight);
      end;
    end;
  end;
  // draw a line at the bottom of the scroll box
  with TBevel.Create(Scroll) do begin
    Parent := Scroll;
    SetBounds(8,Y+8,CW+FieldNamesWidth,16);
    Shape := bsTopLine;
  end;
  Inc(Y,BottomPanel.Height+32);
  // resize height to fit the fields (avoid bottom gap)
  if ClientHeight>Y then
    ClientHeight := Y;
end;

procedure TRecordEditForm.FormShow(Sender: TObject);
begin
  Application.ProcessMessages;
  Screen.Cursor := crHourGlass;
  try
    if Assigned(OnComponentCreate) then
      OnComponentCreate(nil,nil,self); // will call AddEditors() to create nodes
    SetStyle(self);
  finally
    Screen.Cursor := crDefault;
  end;
  PostMessage(Handle,WM_USER,0,0); // avoid Vista and Seven screen refresh bug
end;

procedure TRecordEditForm.WMUser(var Msg: TMessage);
var i: integer;
begin
  for i := 0 to Scroll.ControlCount-1 do
    Scroll.Controls[i].Repaint;
  if fFieldComponents<>nil then
    fFieldComponents[0].SetFocus;
end;

procedure TRecordEditForm.BtnSaveClick(Sender: TObject);
var j, FieldIndex, SetIndex, aID: integer;
    SetValue: set of 0..31;
    U: RawUTF8;
    C: TWinControl;
    CLE: TLabeledEdit absolute C;
    CNE: TSynLabeledEdit absolute C;
    CC: TCheckbox absolute C;
    CB: TCombobox absolute C;
    CG: TGroupBox absolute C;
    CD: TDateTimePicker absolute C;
    Props: TSQLRecordProperties;
    P: PPropInfo;
    ModifiedFields: TSQLFieldBits;
    D: TDateTime;
    TimeLog: Iso8601;
    ErrMsg: string;
begin
  if Rec=nil then
    exit;
  Props := Rec.RecordProps;
  fillchar(ModifiedFields,sizeof(ModifiedFields),0);
  for FieldIndex := 0 to high(fFieldComponents) do begin
    C := fFieldComponents[FieldIndex];
    if (C=nil) or not C.Enabled then
      continue; // disabled components didn't modify their value
    assert(FieldIndex=(C.Tag and 255)-1);
    P := Props.Fields[FieldIndex];
    if Assigned(OnComponentValidate) and not OnComponentValidate(C,P) then begin
      // invalid field content -> abort saving
      C.SetFocus;
      exit;
    end;
    if C.InheritsFrom(TSynLabeledEdit) then
    try
      case P^.PropType^^.Kind of // use variant CNE.GetValue for range checking
      tkInteger:
        P^.SetOrdValue(Rec,CNE.Value);
      tkInt64:
        P^.SetInt64Value(Rec,CNE.Value);
      tkFloat:
        P^.SetExtendedValue(Rec,CNE.Value);
      end;
      Include(ModifiedFields,FieldIndex);
    except
      on E: ESynLabeledEdit do begin // trigerred by CNE.GetValue
        CNE.SetFocus;
        ShowMessage(CNE.EditLabel.Caption+':'#13+E.Message,true);
        exit;
      end;
    end else
    if C.InheritsFrom(TLabeledEdit) then begin
      U := S2U(CLE.Text);
      P^.SetValue(Rec,pointer(U)); // do conversion for every string type
      Include(ModifiedFields,FieldIndex);
    end else
    if C.InheritsFrom(TGroupBox) then begin
      integer(SetValue) := P^.GetOrdValue(Rec);
      for j := 0 to CG.ControlCount-1 do
        if CG.Controls[j].InheritsFrom(TCheckBox) then
        with TCheckBox(CG.Controls[j]) do begin
          SetIndex := (Tag shr 8)-1;
          if cardinal(SetIndex)<32 then begin // up to 32 elements in tkSet
            if Checked then
              include(SetValue,SetIndex) else
              exclude(SetValue,SetIndex);
            Include(ModifiedFields,FieldIndex);
          end;
      end;
      P^.SetOrdValue(Rec,integer(SetValue));
    end else
    if C.InheritsFrom(TCheckBox) then begin
      if CC.Tag<255 then begin
        P^.SetOrdValue(Rec,integer(CC.Checked));
        Include(ModifiedFields,FieldIndex);
      end;
    end else
    if C.InheritsFrom(TComboBox) then begin
      SetIndex := CB.ItemIndex;
      case Props.FieldType[FieldIndex] of
        sftEnumerate:
          if SetIndex>=0 then begin
            P^.SetOrdValue(Rec,SetIndex);
            Include(ModifiedFields,FieldIndex);
          end;
        sftID: begin
          if SetIndex<0 then
            aID := 0 else
            aID := PtrInt(CB.Items.Objects[SetIndex]);
          P^.SetOrdValue(Rec,aID);
          Include(ModifiedFields,FieldIndex);
        end;
      end;
    end else
    if C.InheritsFrom(TDateTimePicker) then begin
      D := trunc(CD.Date);
      if (fFieldComponentsTwin<>nil) and
         fFieldComponentsTwin[FieldIndex].InheritsFrom(TDateTimePicker) then
          D := D+frac(TDateTimePicker(fFieldComponentsTwin[FieldIndex]).Time);
      case Props.FieldType[FieldIndex] of
        sftDateTime:
          P^.SetExtendedValue(Rec,D);
        sftTimeLog, sftModTime: begin
          TimeLog.From(D);
          P^.SetInt64Value(Rec,TimeLog.Value);
        end;
      end; 
    end;
  end;
  // perform all registered filtering
  Rec.Filter(ModifiedFields);
  // perform content validation
  FieldIndex := -1;
  ErrMsg := Rec.Validate(Client,ModifiedFields,@FieldIndex);
  if ErrMsg<>'' then begin
    // invalid field content -> show message, focus component and abort saving
    if cardinal(FieldIndex)<cardinal(length(fFieldComponents)) then begin
      C := fFieldComponents[FieldIndex];
      C.SetFocus;
      Application.ProcessMessages;
      ShowMessage(ErrMsg,format(sInvalidFieldN,[fFieldCaption[FieldIndex]]),true);
    end else
      ShowMessage(ErrMsg,format(sInvalidFieldN,['?']),true);
  end else
    // close window on success
    ModalResult := mrOk;
end;

procedure TRecordEditForm.FormCreate(Sender: TObject);
begin
  Font := DefaultFont;
  BtnSave := TSynButton.Create(self);
  BtnSave.Parent := BottomPanel;
  BtnSave.SetBounds(251,8,100,41);
  BtnSave.Caption := sSave;
  BtnSave.OnClick := BtnSaveClick;
  BtnSave.SetBitmap(BitmapOK);
  BtnSave.Anchors := [akRight, akBottom];
  BtnCancel := TSynButton.CreateKind(BottomPanel,cbCancel,363,8,100,41);
  BtnCancel.Anchors := [akRight, akBottom];
end;

end.
