unit odbcimport;


// -------------------------------------
// HeidiSQL
// Import through ODBC
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, CheckLst, Db, ADODB, ExtCtrls, comctrls,
  DBTables;

type
  Todbcimportform = class(TForm)
    GroupBox1: TGroupBox;
    ComboBoxDSN: TComboBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    EditFileName: TEdit;
    ButtonOpenFile: TBitBtn;
    ODBCConnection: TADOConnection;
    CheckListBoxTables: TCheckListBox;
    Label1: TLabel;
    ListBoxFields: TListBox;
    Label2: TLabel;
    ADOTable1: TADOTable;
    Bevel1: TBevel;
    ButtonImport: TButton;
    ButtonCancel: TButton;
    Label3: TLabel;
    ComboBoxType: TComboBox;
    Label4: TLabel;
    EditLengthSet: TEdit;
    Label5: TLabel;
    EditDefault: TEdit;
    CheckListBoxOptions: TCheckListBox;
    Label6: TLabel;
    ComboBoxTargetDB: TComboBox;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    Bevel2: TBevel;
    Image1: TImage;
    Image2: TImage;
    ADOQuery1: TADOQuery;
    Table1: TTable;
    procedure FormShow(Sender: TObject);
    procedure SourceSpecified(Sender: TObject);
    procedure CheckListBoxTablesClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ListBoxFieldsClick(Sender: TObject);
    procedure emptycontrols(Sender: TObject);
    procedure ToggleSource(Sender: TObject);
    procedure ButtonOpenFileClick(Sender: TObject);
    procedure ButtonImportClick(Sender: TObject);
    procedure ComboBoxTypeChange(Sender: TObject);
    procedure EditLengthSetChange(Sender: TObject);
    procedure EditDefaultChange(Sender: TObject);
    procedure CheckListBoxOptionsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


type TMysqlField = record
    Name : String[64];
    Typ : Byte;
    LengthSet : String;
    Default : String;
    Primary : Boolean;
    Index : Boolean;
    Unique : Boolean;
    Binary : Boolean;
    Unsigned : Boolean;
    Zerofill : Boolean;
    NotNull : Boolean;
    AutoIncrement : Boolean;
  end;

var
  odbcimportform: Todbcimportform;
  felder : array of array of TMysqlField;

implementation

uses ODBCUtil, Childwin, helpers, Main;

{$R *.DFM}

procedure Todbcimportform.FormShow(Sender: TObject);
var
  tn : TTreeNode;
  i  : Integer;
begin
  GetODBCDataSourceNames(ComboBoxDSN.Items);
  with TMDIChild(Mainform.ActiveMDIChild) do begin
    self.Caption := ZConn.HostName + ' - ODBC Import';
    self.ComboBoxTargetDB.Items.Clear;
    for i:=0 to tnodehost.count-1 do begin
      tn := tnodehost.Item[i];
      self.ComboBoxTargetDB.Items.Add(tn.text);
      if tn.text = ActualDatabase then
        ComboBoxTargetDB.ItemIndex := i;
    end;
  end;
  if ComboBoxTargetDB.ItemIndex < 0 then
    ComboBoxTargetDB.ItemIndex := 0;
end;


procedure Todbcimportform.emptycontrols(Sender: TObject);
begin
  CheckListBoxTables.Items.Clear;
  ListBoxFields.Items.Clear;
  ComboBoxType.Enabled := false;
  EditLengthSet.Enabled := false;
  EditDefault.Enabled := false;
  CheckListBoxOptions.Enabled := false;
end;

procedure Todbcimportform.SourceSpecified(Sender: TObject);
var
  i,j : Integer;
  MyFieldList : TStringList;
begin
  emptycontrols(self);
  setlength(felder, 0);
  // show tables from dsn...
//  if ComboBoxDSN.ItemIndex > -1 then begin
    ODBCConnection.Connected := false;
    if (sender as TControl) = ComboBoxDSN then
      ODBCConnection.ConnectionString :=
        'Provider=MSDASQL.1;' +
        'Persist Security Info=False;' +
        'Data Source=' + ComboBoxDSN.Text
    else begin
      ODBCConnection.ConnectionString :=
        'Provider=Microsoft.Jet.OLEDB.4.0;' +
        'Data Source=' + EditFileName.Text + ';' +
        'Persist Security Info=False';
        end;
    ODBCConnection.Connected := true;
    ODBCConnection.GetTableNames(CheckListBoxTables.Items);


    // fill felder-array
    setlength(felder, CheckListBoxTables.Items.Count);
    ADOTable1.Active := false;
    for i:=0 to CheckListBoxTables.Items.Count-1 do begin
      CheckListBoxTables.Checked[i] := true;
      ADOTable1.TableName := CheckListBoxTables.Items[i];
      ADOTable1.Active := true;
      MyFieldList := TStringList.Create;
      ADOTable1.getFieldNames(MyFieldList);
      setlength(felder[i], MyFieldList.Count);

      for j:= 0 to MyFieldList.Count-1 do begin
        with felder[i][j] do begin
          Name := stringreplace(MyFieldList[j], '-', '_', [rfReplaceAll]);
          Name := stringreplace(Name, ' ', '_', [rfReplaceAll]);
          LengthSet := inttostr(AdoTable1.Fields[j].Size);
          Default := AdoTable1.Fields[j].DefaultExpression;
          Index := AdoTable1.Fields[j].IsIndexField;
          case AdoTable1.Fields[j].Datatype of
            ftUnknown   : begin Typ:=14; LengthSet:='255' end;
            ftString    : begin Typ:=14; end;
            ftSmallint  : begin Typ:=1;  LengthSet:=''; end;
            ftInteger   : begin Typ:=3;  LengthSet:=''; end;
            ftWord      : begin Typ:=2;  LengthSet:=''; Unsigned := true; end;
            ftBoolean   : begin Typ:=23; LengthSet:='''True'',''False'''; end;
            ftFloat     : begin Typ:=5;  LengthSet:=''; end;
            ftCurrency  : begin Typ:=7;  LengthSet:=''; end;
            ftBCD       : begin Typ:=7;  LengthSet:=''; end;
            ftDate      : begin Typ:=8;  LengthSet:=''; end;
            ftTime      : begin Typ:=11; LengthSet:=''; end;
            ftDateTime  : begin Typ:=9;  LengthSet:=''; end;
            ftBytes     : begin Typ:=13; LengthSet:='255'; end;
            ftVarBytes  : begin Typ:=14; LengthSet:='255'; end;
            ftAutoInc   : begin Typ:=3;  LengthSet:='';
                                Primary := true;
                                Unsigned := true;
                                NotNull := true;
                                AutoIncrement := true;
                                end;
            ftBlob      : begin Typ:=18; LengthSet:=''; end;
            ftMemo      : begin Typ:=17; LengthSet:=''; end;
            ftGraphic   : begin Typ:=19; LengthSet:=''; end;
            ftFmtMemo   : begin Typ:=17; LengthSet:=''; end;
            ftParadoxOle: begin Typ:=14; LengthSet:='255'; end;
            ftDBaseOle  : begin Typ:=14; LengthSet:='255'; end;
            ftTypedBinary : begin Typ:=18; LengthSet:=''; end;
            ftCursor    : begin Typ:=14; LengthSet:='255'; end;
            ftFixedChar : begin Typ:=13; end;
            ftWideString: begin Typ:=14; end;
            ftLargeint  : begin Typ:=4;  LengthSet:=''; end;
            ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
            ftVariant, ftInterface, ftIDispatch, ftGuid : begin Typ:=14; LengthSet:='255'; end;
          end;
        end;

      end;
      ADOTable1.Active := false;
      ButtonImport.Enabled := true;
    end;

//  end;
end;

procedure Todbcimportform.CheckListBoxTablesClick(Sender: TObject);
begin
  ADOTable1.Active := false;
  ListBoxFields.Items.Clear;
  ListBoxFieldsClick(self);
  if CheckListBoxTables.ItemIndex > -1 then begin
    ADOTable1.TableName := CheckListBoxTables.Items[CheckListBoxTables.ItemIndex];
    ADOTable1.Active := true;
    ADOTable1.GetFieldNames(ListBoxFields.Items);
  end;
end;

procedure Todbcimportform.ButtonCancelClick(Sender: TObject);
begin
  close;
end;

procedure Todbcimportform.ListBoxFieldsClick(Sender: TObject);
var yesno : Boolean;
begin
  yesno := ListBoxFields.ItemIndex > -1;
  ComboboxType.Enabled := yesno;
  EditLengthSet.Enabled := yesno;
  EditDefault.Enabled := yesno;
  CheckListBoxOptions.Enabled := yesno;
  if yesno then
  with felder[CheckListBoxTables.ItemIndex][ListBoxFields.ItemIndex] do begin
    ComboBoxType.ItemIndex := Typ;
    EditDefault.Text := Default;
    EditLengthSet.Text := Lengthset;
    CheckListBoxOptions.Checked[0] := Primary;
    CheckListBoxOptions.Checked[1] := Index;
    CheckListBoxOptions.Checked[2] := Unique;
    CheckListBoxOptions.Checked[3] := Binary;
    CheckListBoxOptions.Checked[4] := Unsigned;
    CheckListBoxOptions.Checked[5] := Zerofill;
    CheckListBoxOptions.Checked[6] := NotNull;
    CheckListBoxOptions.Checked[7] := AutoIncrement;
  end;
end;

procedure Todbcimportform.ToggleSource(Sender: TObject);
begin
  ComboBoxDSN.Enabled := RadioButton1.Checked;
  EditFilename.Enabled := RadioButton2.Checked;
  ButtonOpenFile.Enabled := RadioButton2.Checked;
  if RadioButton2.Checked then
    comboboxDSN.Color := clBtnFace else
    comboboxDSN.Color := clWindow;
end;

procedure Todbcimportform.ButtonOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    EditFilename.Text := OpenDialog1.Filename;
    SourceSpecified(self);
  end;
end;

procedure Todbcimportform.ButtonImportClick(Sender: TObject);
var
  i,j,k : Integer;
  ctquery, pkstr, unstr, instr : String;
  insquery, value : String;
  year, month, day, Hour, Min, Sec, MSec : Word;
begin
  // Import!!
  Screen.Cursor := crHourglass;
  for i:=0 to length(felder)-1 do if CheckListBoxTables.Checked[i] then begin

    // Query vorbereiten:
    ctquery := 'CREATE TABLE ' + mainform.mask(ComboBoxTargetDB.Text) + '.' + mainform.mask(CheckListBoxTables.Items[i]) + ' (';

    // felder
    for j := 0 to length(felder[i]) - 1 do
    begin
      ctquery := ctquery + mainform.mask(felder[i][j].Name) + ' ' +
        comboboxtype.items[felder[i][j].Typ];                              // Typ
      if felder[i][j].LengthSet <> '' then
        ctquery := ctquery  + ' (' + felder[i][j].LengthSet + ')';         // Length/Set
      if felder[i][j].Binary then
        ctquery := ctquery  + ' BINARY';                                // Binary
      if felder[i][j].Unsigned then
        ctquery := ctquery  + ' UNSIGNED';                              // Unsigned
      if felder[i][j].Zerofill then
        ctquery := ctquery  + ' ZEROFILL';                              // Zerofill
      if felder[i][j].Default <> '' then
        ctquery := ctquery  + ' DEFAULT ''' + felder[i][j].Default + '''';   // Default
      if felder[i][j].NotNull then
        ctquery := ctquery  + ' NOT NULL';                              // Not null
      if felder[i][j].AutoIncrement then
        ctquery := ctquery  + ' AUTO_INCREMENT';                         // AutoIncrement

      if j < length(felder[i])-1 then
        ctquery := ctquery + ', '
    end;

    // Indexes:
    pkstr := '';
    unstr := '';
    instr := '';
    for j := 0 to length(felder[i]) - 1 do
    begin
      if felder[i][j].Primary then
      begin
        if pkstr <> '' then pkstr := pkstr + ',';
        pkstr := pkstr + felder[i][j].Name;
      end;
      if felder[i][j].Unique then
      begin
        if unstr <> '' then unstr := unstr + ',';
        unstr := unstr + felder[i][j].Name;
      end;
      if felder[i][j].Index then
      begin
        if instr <> '' then instr := instr + ',';
        instr := instr + felder[i][j].Name;
      end;
    end;
    if pkstr <> '' then
      ctquery := ctquery + ', PRIMARY KEY(' + pkstr + ')';
    if unstr <> '' then
      ctquery := ctquery + ', UNIQUE(' + unstr + ')';
    if instr <> '' then
      ctquery := ctquery + ', INDEX(' + instr + ')';

    // Abschluss-Klammer:
    ctquery := ctquery + ') ';
    with TMDIChild(Application.Mainform.ActiveMDIChild) do begin
      ExecQuery( 'USE ' + CheckListBoxTables.Items[i] );
      ExecQuery(ctquery);

      if RadioButton2.Checked then // access file - safety-brackets
        AdoQuery1.SQL.Add('SELECT * FROM [' + CheckListBoxTables.Items[i] + ']')
      else                         // ODBC - universal syntax
        AdoQuery1.SQL.Add('SELECT * FROM "' + CheckListBoxTables.Items[i]+'"');
      AdoQuery1.Active := true;
      AdoQuery1.Open;
      insquery := '';
      for j:=0 to AdoQuery1.RecordCount-1 do begin
        insquery := '';
        for k:=0 to AdoQuery1.FieldCount-1 do begin
          value := escape_string(AdoQuery1.Fields[k].AsString);
          if felder[i][k].Typ in [8,9] then begin // Date or DateTime
            decodedate(AdoQuery1.Fields[k].AsDateTime, year, month, day);
            value := inttostr(year)+'-'+inttostr(month)+'-'+inttostr(day);
          end;
          if felder[i][k].Typ = 9 then begin // DateTime
            decodetime(AdoQuery1.Fields[k].AsDateTime, Hour, Min, Sec, MSec);
            value := value +' '+ inttostr(hour)+':'+inttostr(min)+':'+inttostr(sec);
          end;
          insquery := insquery + '''' + value + '''';
          if k < AdoQuery1.FieldCount-1 then
            insquery := insquery + ',';
        end;
        ExecQuery('INSERT INTO ' + mainform.mask(ComboBoxTargetDB.Text) + '.' + mainform.mask(CheckListBoxTables.Items[i]) + ' VALUES(' + insquery + ')');
        AdoQuery1.Next;
      end;
    end;

    AdoQuery1.Active := false;
    AdoQuery1.SQL.Clear;

    CheckListBoxTables.Checked[i] := false;
  end;
  Screen.Cursor := crdefault;
end;

procedure Todbcimportform.ComboBoxTypeChange(Sender: TObject);
begin
  felder[checklistboxtables.ItemIndex][listboxfields.ItemIndex].Typ := ComboBoxType.ItemIndex;
end;

procedure Todbcimportform.EditLengthSetChange(Sender: TObject);
begin
  felder[checklistboxtables.ItemIndex][listboxfields.ItemIndex].LengthSet := EditLengthSet.Text;
end;

procedure Todbcimportform.EditDefaultChange(Sender: TObject);
begin
  felder[checklistboxtables.ItemIndex][listboxfields.ItemIndex].Default := EditDefault.Text;
end;

procedure Todbcimportform.CheckListBoxOptionsClick(Sender: TObject);
begin
  with felder[checklistboxtables.ItemIndex][listboxfields.ItemIndex] do begin
    Primary := CheckListBoxOptions.Checked[0];
    Index := CheckListBoxOptions.Checked[1];
    Unique := CheckListBoxOptions.Checked[2];
    Binary := CheckListBoxOptions.Checked[3];
    Unsigned := CheckListBoxOptions.Checked[4];
    Zerofill := CheckListBoxOptions.Checked[5];
    NotNull := CheckListBoxOptions.Checked[6];
    AutoIncrement := CheckListBoxOptions.Checked[7];
  end;
end;

end.
