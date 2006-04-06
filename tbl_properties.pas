unit tbl_properties;


// -------------------------------------
// HeidiSQL
// Advanced table-properties
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, SortListView, ExtCtrls;

type
  Ttbl_properties_form = class(TForm)
    PageControl1: TPageControl;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Bevel1: TBevel;
    Panel2: TPanel;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  tbl_properties_form: Ttbl_properties_form;

implementation

uses Childwin, mysql, Main;

{$R *.DFM}

procedure Ttbl_properties_form.FormShow(Sender: TObject);
var
  i,t : Integer;
  ts : TTabSheet;
  list, tabellenliste : TSortListView;
  datasize, indexsize : Integer;
  isSelected : Boolean;
  row        : PMYSQL_ROW;
  MyResult   : PMYSQL_RES;
  Field      : PMYSQL_FIELD;
  FieldList  : TStringList;
begin
  tabellenliste := TMDIChild(Mainform.ActiveMDIChild).TabellenListe;
  for i:=PageControl1.PageCount-1 downto 0 do
    PageControl1.Pages[i].Free;

  datasize := 0;
  indexsize := 0;

  MyResult := TMDIChild(Mainform.ActiveMDIChild).q('SHOW TABLE STATUS');
  FieldList := TStringList.Create;
  for i:=0 to myResult.field_count-1 do begin
    field := mysql_fetch_field(MyResult);
    FieldList.add(field.name);
  end;

  for t:=0 to mysql_num_rows(MyResult)-1 do begin
    row := mysql_fetch_row(MyResult);
    isSelected := false;
    for i:=0 to TabellenListe.Items.Count-1 do begin
      isSelected := (TabellenListe.Items[i].caption = row[0]) and TabellenListe.Items[i].Selected;
      if isSelected then
        break;
    end;
    if not isSelected then
      continue;
    ts := TTabSheet.Create(PageControl1);
    ts.Caption := TabellenListe.Items[t].Caption;
    ts.PageControl := PageControl1;

    list := TSortListView.Create(self);
    list.Parent := ts;
    list.Align := alClient;
    list.ViewStyle := vsReport;
    list.ReadOnly := true;
    list.GridLines := true;
    list.RowSelect := true;

    with list.Columns.Add do begin
      Caption := 'Variable';
      Width := 100;
    end;
    with list.Columns.Add do begin
      Caption := 'Value';
      Width := -1;
    end;

    for i:=0 to FieldList.count-1 do
    begin
      if i=5 then
        inc(datasize, StrToIntDef(row[i], 0));
      if i=7 then
        inc(indexsize, StrToIntDef(row[i], 0));
      with List.Items.add do begin
        Caption := FieldList[i];
        SubItems.Add(row[i]);
      end;
    end;

  end;

  Label3.Caption := IntToStr(datasize div 1024) + ' KB';
  Label4.Caption := IntToStr(indexsize div 1024) + ' KB';
  Label6.Caption := IntToStr(datasize div 1024 + indexsize div 1024) + ' KB';

end;


procedure Ttbl_properties_form.Button1Click(Sender: TObject);
begin
  close;
end;

procedure Ttbl_properties_form.FormResize(Sender: TObject);
begin
  button1.Left := Panel2.width div 2 - (button1.width div 2);
  Label3.left := panel1.width - Label3.width - 8;
  Label4.left := panel1.width - Label4.width - 8;
  Label6.left := panel1.width - Label6.width - 8;
end;

end.
