unit SynDBExplorerQueryBuilder;

interface

uses
  Windows, Messages, SysUtils, CheckLst, Controls, StdCtrls, Classes, Graphics, Forms,
  SynDB, SynCommons;

type
  TDBQueryTable = object
    Name: string;
    NameWithoutSchema: RawUTF8;
    Fields: TSQLDBColumnDefineDynArray;
    FieldsAlias: TRawUTF8DynArray;
    SelectedAll: boolean;
    Selected: set of byte;
    function FieldAlias(FieldIndex: integer): RawUTF8;
    function FieldIndexMatch(const aTable: RawUTF8; const aField: TSQLDBColumnDefine): integer;
    function AsIniRow: RawUTF8;
    function FromIniSection(P: PUTF8Char): PUTF8Char;
  end;
  TDBQueryJoin = record
    SourceTable: integer;
    SourceField: integer;
    DestTable: integer;
    DestField: integer;
  end;
  TDBQueryTableDynArray = array of TDBQueryTable;
  TDBQueryJoinDynArray = array of TDBQueryJoin;
  TDBQueryObject = object
    Name: string;
    Tables: TDBQueryTableDynArray;
    JOIN: TDBQueryJoinDynArray;
    procedure InitJOIN(Size: integer);
    function ComputeSQLSelect: RawUTF8;
    function AsIniSection: string;
    function FromIniSection(P: PUTF8Char): Boolean; overload;
    function FromIniSection(const IniContent: RawUTF8): Boolean; overload;
  end;
  PDBQueryTable = ^TDBQueryTable;
  
  TDBQueryBuilderForm = class(TForm)
    GroupJoin: TGroupBox;
    GroupFields: TGroupBox;
    MemoSQL: TMemo;
    FieldsTable: TListBox;
    FieldsColumn: TCheckListBox;
    FieldsAll: TCheckBox;
    BtnUseSQL: TButton;
    BtnCancel: TButton;
    BtnExecSQL: TButton;
    BtnToObject: TButton;
    procedure FieldsTableClick(Sender: TObject);
    procedure FieldsAllClick(Sender: TObject);
    procedure FieldsColumnClick(Sender: TObject);
  private
    fProps: TSQLDBConnectionProperties;
    fObject: TDBQueryObject;
    fJOINUI: array of record
      SourceTable: TComboBox;
      SourceField: TComboBox;
      DestTable: TComboBox;
      DestField: TComboBox;
    end;
    procedure JoinTableClick(Sender: TObject);
    procedure SetTableNames(const Value: TStrings);
    function FieldsTableCurrent: PDBQueryTable;
    procedure ComputeSQL(Sender: TObject);
  public
    class function BuildQuery(aTableNames: TStrings; aProps: TSQLDBConnectionProperties;
      out SQL: string): integer; overload;
    class function BuildQuery(aListBox: TListBox; aProps: TSQLDBConnectionProperties;
      out SQL: string): integer; overload;
    property Props: TSQLDBConnectionProperties read fProps write fProps;
  end;

resourcestring
  sMissingJOIN = 'Missing JOIN';


implementation

{$R *.dfm}

{ TDBQueryTable }

function TDBQueryTable.AsIniRow: RawUTF8;
var f: integer;
begin
  result := FormatUTF8('%,%,%,',[Name,length(Fields),SelectedAll]);
  for f := 0 to high(Fields) do
  with Fields[f] do
    result := FormatUTF8('%%,%,%,%,',
      [result,ColumnName,Ord(ColumnType),FieldAlias(f),f in Selected]);
end;

function TDBQueryTable.FieldAlias(FieldIndex: integer): RawUTF8;
begin
  if Cardinal(FieldIndex)>=cardinal(length(FieldsAlias)) then
    result := '' else
    result := FieldsAlias[FieldIndex];
end;

function TDBQueryTable.FieldIndexMatch(const aTable: RawUTF8;
  const aField: TSQLDBColumnDefine): integer;
begin
  for result := 0 to High(Fields) do
    with Fields[result] do
    if (aField.ColumnType=ColumnType) then
      if IsRowID(pointer(ColumnName)) then begin
        if IdemPropNameU(aField.ColumnName,aTable) then
          exit; // Toto.Tata=Tata.ID (e.g. for SQLite3 tables)
      end else
      if IdemPropNameU(aField.ColumnName,ColumnName) then
        exit; // Toto.ID_TRN=Tata.ID_TRN (most common RDRMS layouts)
  result := -1;
end;

function TDBQueryTable.FromIniSection(P: PUTF8Char): PUTF8Char;
var i: integer;
begin
  result := P;
  Name := Ansi7ToString(GetNextItem(result));
  SetLength(Fields,GetNextItemCardinal(result));
  SelectedAll := GetNextItemCardinal(result)=1;
  if result=nil then exit;
  Selected := [];
  SetLength(FieldsAlias,length(Fields));
  for i := 0 to high(Fields) do
  with Fields[i] do begin
    ColumnName := GetNextItem(result);
    ColumnType := TSQLDBFieldType(GetNextItemCardinal(result));
    FieldsAlias[i] := GetNextItem(result);
    if GetNextItemCardinal(result)=1 then
      include(Selected,i);
  end;
end;


{ TDBQueryObject }

function TDBQueryObject.AsIniSection: string;
var ini: RawUTF8;
    t: integer;
begin
  ini := FormatUTF8('[%]'#13#10'TableCount=%'#13#10,
    [Name,length(Tables)]);
  for t := 0 to high(Tables) do
    ini := FormatUTF8('%Table%=%'#13#10,[ini,t,Tables[t].AsIniRow]);
  for t := 0 to high(JOIN) do
    with JOIN[t] do
      ini := FormatUTF8('%Join%=%,%,%,%'#13#10,
        [ini,t,SourceTable,SourceField,DestTable,DestField]);
  result := UTF8ToString(ini);
end;

function TDBQueryObject.ComputeSQLSelect: RawUTF8;
var select, NameWithSchema: RawUTF8;
    t,f: integer;
begin
  result := '';
  for t := 0 to high(Tables) do
  with Tables[t] do begin
    if SelectedAll then
      select := NameWithoutSchema+'.*' else begin
      select := '';
      for f := 0 to High(Fields) do
      if f in Selected then begin
        if select<>'' then
          select := select+',';
        select := select+NameWithoutSchema+'.'+Fields[f].ColumnName;
        if (FieldsAlias<>nil) and (FieldsAlias[f]<>'') then
          select := select+' '+FieldsAlias[f];
      end;
    end;
    if  select<>'' then begin
      if (result<>'') and (result[length(result)]<>',') then
        result := result+',';
      result := result+' '+select;
    end;
  end;
  if result<>'' then begin // need at least one field retrieved
    result := 'select'#13#10' '+result+#13#10'from'#13#10' ';
    for t := 0 to high(Tables) do
    with Tables[t] do begin
      if t>0 then
        result := result+',';
     NameWithSchema := StringToAnsi7(Name);
      result := result+NameWithSchema;
      if NameWithSchema<>NameWithoutSchema then
        result := result+' '+NameWithoutSchema;
    end;
    select := '';
    for t := 0 to high(JOIN) do
    with JOIN[t] do
      if (SourceField<0) or (SourceTable<0) or
         (DestField<0) or (DestTable<0) then begin
        result := StringToUTF8(sMissingJOIN);
        exit;
      end else begin
        if select<>'' then
          select := select+' and ' else
          select := #13#10'where'#13#10' ';
        select := select+Tables[SourceTable].NameWithoutSchema+'.'+
          Tables[SourceTable].Fields[SourceField].ColumnName+'=';
        select := select+Tables[DestTable].NameWithoutSchema+'.'+
          Tables[DestTable].Fields[DestField].ColumnName;
      end;
    result := result+select;
  end;
end;

function TDBQueryObject.FromIniSection(P: PUTF8Char): Boolean;
var t: integer;
begin
  result := false;
  if P=nil then exit;
  while P^=' ' do inc(P);
  if P^<>'[' then exit;
  inc(P);
  Name := Ansi7ToString(GetNextItem(P,']'));
  inc(P); while P^<=' ' do if P^=#0 then exit else inc(P);
  if not IdemPChar(P,'TABLECOUNT=') then exit else inc(P,11);
  SetLength(Tables,GetNextItemCardinal(P,#13));
  if P=nil then exit;
  for t := 0 to high(Tables) do begin
    while P^<=' ' do if P^=#0 then exit else inc(P);
    if not IdemPChar(P,'TABLE') then
      exit else begin
      inc(P,5);
      if (GetNextItemCardinal(P,'=')<>cardinal(t)) or (P=nil) then exit;
      P := Tables[t].FromIniSection(P);
      if P=nil then exit;
    end;
  end;
  SetLength(JOIN,length(Tables)-1);
  for t := 0 to high(JOIN) do begin
    if P=nil then exit;
    while P^<=' ' do if P^=#0 then exit else inc(P);
    if not IdemPChar(P,'JOIN') then
      exit else begin
      inc(P,4);
      if (GetNextItemCardinal(P,'=')<>cardinal(t)) or (P=nil) then exit;
      with JOIN[t] do begin
        SourceTable := GetNextItemCardinal(P);
        SourceField := GetNextItemCardinal(P);
        DestTable := GetNextItemCardinal(P);
        DestField := GetNextItemCardinal(P,#13);
      end;
    end;
  end;
  result := True;
end;

function TDBQueryObject.FromIniSection(const IniContent: RawUTF8): Boolean;
var P: PUTF8Char;
begin
  P := pointer(IniContent);
  result := FromIniSection(P);
end;

procedure TDBQueryObject.InitJOIN(Size: integer);
begin
  SetLength(JOIN,Size);
  fillchar(JOIN[0],Size*sizeof(JOIN[0]),255); // fill all to -1
end;


{ TDBQueryBuilderForm }

class function TDBQueryBuilderForm.BuildQuery(aTableNames: TStrings;
  aProps: TSQLDBConnectionProperties; out SQL: string): integer;
begin
  result := mrCancel;
  if (aProps<>nil) and (aTableNames<>nil) then
  with TDBQueryBuilderForm.Create(Application) do
  try
    Props := aProps;
    SetTableNames(aTableNames);
    result := ShowModal;
    case result of
    mrOk,mrYes:
      SQL := MemoSQL.Text;
    mrRetry:
      SQL := fObject.AsIniSection; 
    end;
  finally
    Free;
  end;
end;

class function TDBQueryBuilderForm.BuildQuery(aListBox: TListBox;
  aProps: TSQLDBConnectionProperties; out SQL: string): integer;
var T: TStringList;
    i: integer;
begin
  result := mrCancel;
  if (aListBox=nil) or (aListBox.SelCount=0) then
    exit;
  T := TStringList.Create;
  try
    for i := 0 to aListBox.Count-1 do
      if aListBox.Selected[i] then
        T.Add(aListBox.Items[i]);
    result := BuildQuery(T,aProps,SQL);
  finally
    T.Free;
  end;
end;

procedure TDBQueryBuilderForm.SetTableNames(const Value: TStrings);
var t,j,f,k,Y: Integer;
  function CreateCombo(X: integer; Table: integer=-1): TComboBox;
  var k: Integer;
  begin
    result := TComboBox.Create(self);
    result.Parent := GroupJoin;
    result.SetBounds(X,Y,100,20);
    result.Style := csDropDownList;
    result.Font.Size := 7;
    if Table>=0 then begin
      for k := 0 to High(fObject.Tables) do
        result.Items.Add(Ansi7ToString(fObject.Tables[k].NameWithoutSchema));
      result.OnClick := JoinTableClick;
    end else
      result.OnClick := ComputeSQL;
  end;
begin
  FieldsTable.Clear;
  Screen.Cursor := crHourGlass;
  try
    // fill fObject.Tables[]
    SetLength(fObject.Tables,Value.Count);
    for t := 0 to high(fObject.Tables) do
    with fObject.Tables[t] do begin
      Name := Value[t];
      NameWithoutSchema := StringToAnsi7(Name);
      Props.GetFields(NameWithoutSchema,Fields);
      NameWithoutSchema := TrimLeftSchema(NameWithoutSchema);
      FieldsTable.Items.Add(Name);
    end;
    // create JOIN controls on form
    SetLength(fJOINUI,length(fObject.Tables)-1);
    Y := 20;
    for t := 0 to high(fJOINUI) do
    with fJOINUI[t] do begin
      SourceTable := CreateCombo(8,t);
      SourceField := CreateCombo(112);
      SourceTable.Tag := PtrInt(SourceField);
      JoinTableClick(SourceTable);
      DestTable := CreateCombo(224,t+1);
      DestField := CreateCombo(328);
      DestTable.Tag := PtrInt(DestField);
      JoinTableClick(DestTable);
      with TLabel.Create(self) do begin
        Parent := GroupJoin;
        SetBounds(214,Y+3,8,20);
        Font.Style := [fsBold];
        Caption := '=';
      end;
      inc(Y,24);
    end;
    inc(Y,8);
    if Y>GroupJoin.ClientHeight then begin
      ClientHeight := ClientHeight+Y-GroupJoin.ClientHeight;
      GroupJoin.ClientHeight := Y;
      GroupFields.ClientHeight := Y;
    end;
    // guess column JOIN criteria
    fObject.InitJOIN(length(fJOINUI));
    for t := 0 to high(fJOINUI) do
    with fObject.JOIN[t], fObject.Tables[t] do begin
      for f := 0 to high(Fields) do begin
        for j := 0 to high(fObject.Tables) do
        if j<>t then begin
          k := fObject.Tables[j].FieldIndexMatch(NameWithoutSchema,Fields[f]);
          if k>=0 then begin // get first matching field (name+type) in tables
            SourceTable := t;
            SourceField := f;
            DestTable := j;
            DestField := k;
            break;
          end;
        end;
        if DestTable>=0 then
          break;
      end;
    end;
    // refresh UI
    for f := 0 to high(fJOINUI) do
    with fJOINUI[f] do begin
      SourceTable.ItemIndex := fObject.JOIN[f].SourceTable;
      JoinTableClick(SourceTable);
      SourceField.ItemIndex := fObject.JOIN[f].SourceField;
      DestTable.ItemIndex := fObject.JOIN[f].DestTable;
      JoinTableClick(DestTable);
      DestField.ItemIndex := fObject.JOIN[f].DestField;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  if Value.Count>0 then begin
    FieldsTable.ItemIndex := 0;
    FieldsTableClick(nil);
  end;
end;

procedure TDBQueryBuilderForm.FieldsTableClick(Sender: TObject);
var i: integer;
    T: PDBQueryTable;
begin
  FieldsColumn.Clear;
  T := FieldsTableCurrent;
  if T=nil then
    exit;
  for i := 0 to High(T^.Fields) do
    FieldsColumn.Items.Add(Ansi7ToString(
      TSQLDBConnectionProperties.GetFieldDefinition(T^.Fields[i])));
  FieldsAll.Checked := T^.SelectedAll;
  FieldsAllClick(nil);
end;

procedure TDBQueryBuilderForm.FieldsAllClick(Sender: TObject);
var T: PDBQueryTable;
    i: integer;
begin
  FieldsColumn.Enabled := not FieldsAll.Checked;
  T := FieldsTableCurrent;
  if T=nil then
    exit;
  T^.SelectedAll := FieldsAll.Checked;
  for i := 0 to High(T^.Fields) do
    FieldsColumn.Checked[i] := i in T^.Selected;
  ComputeSQL(nil);
end;

function TDBQueryBuilderForm.FieldsTableCurrent: PDBQueryTable;
var i: cardinal;
begin
  i := FieldsTable.ItemIndex;
  if i<cardinal(length(fObject.Tables)) then
    result := @fObject.Tables[i] else
    result := nil;
end;

procedure TDBQueryBuilderForm.FieldsColumnClick(Sender: TObject);
var T: PDBQueryTable;
    i: integer;
begin
  T := FieldsTableCurrent;
  if T=nil then
    exit;
  FillChar(T^.Selected,sizeof(T^.Selected),0);
  for i := 0 to High(T^.Fields) do
    if FieldsColumn.Checked[i] then
      Include(T^.Selected,i);
  ComputeSQL(nil);
end;

procedure TDBQueryBuilderForm.ComputeSQL(Sender: TObject);
var f: integer;
begin
  for f := 0 to high(fJOINUI) do
  with fJOINUI[f] do begin
    fObject.JOIN[f].SourceTable := SourceTable.ItemIndex;
    fObject.JOIN[f].SourceField := SourceField.ItemIndex;
    fObject.JOIN[f].DestTable := DestTable.ItemIndex;
    fObject.JOIN[f].DestField := DestField.ItemIndex;
  end;
  MemoSQL.Text := Ansi7ToString(fObject.ComputeSQLSelect);
end;

procedure TDBQueryBuilderForm.JoinTableClick(Sender: TObject);
var Table, Field: TComboBox;
    t,f: integer;
begin
  Table := Sender as TComboBox;
  Field := TComboBox(Table.Tag);
  Field.Items.Clear;
  t := Table.ItemIndex;
  if t>=0 then
    with fObject.Tables[t] do
    for f := 0 to high(Fields) do
      Field.Items.Add(Ansi7ToString(Fields[f].ColumnName));
end;


end.
