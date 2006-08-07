unit tbl_properties;


// -------------------------------------
// HeidiSQL
// Advanced table-properties
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, SortListView, ExtCtrls, ZDataset;

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

uses Childwin, Main;

{$R *.DFM}

procedure Ttbl_properties_form.FormShow(Sender: TObject);
var
  i,t : Integer;
  ts : TTabSheet;
  list, ListTables : TSortListView;
  datasize, indexsize : Integer;
  isSelected : Boolean;
  FieldList  : TStringList;
  zq : TZReadOnlyQuery;
begin
  ListTables := TMDIChild(Mainform.ActiveMDIChild).ListTables;
  for i:=PageControl1.PageCount-1 downto 0 do
    PageControl1.Pages[i].Free;

  datasize := 0;
  indexsize := 0;

  zq := TMDIChild(Mainform.ActiveMDIChild).ZQuery3;
  TMDIChild(Mainform.ActiveMDIChild).GetResults( 'SHOW TABLE STATUS', zq );
  FieldList := TStringList.Create;
  for i:=0 to zq.FieldCount-1 do
  begin
    FieldList.add( zq.Fields[i].Fieldname );
  end;

  for t:=0 to zq.RecordCount-1 do
  begin
    isSelected := false;
    for i:=0 to ListTables.Items.Count-1 do
    begin
      isSelected := (ListTables.Items[i].caption = zq.Fields[0].AsString) and ListTables.Items[i].Selected;
      if isSelected then
        break;
    end;
    if not isSelected then
    begin
      zq.Next;
      continue;
    end;
    ts := TTabSheet.Create(PageControl1);
    ts.Caption := ListTables.Items[t].Caption;
    ts.PageControl := PageControl1;

    list := TSortListView.Create(self);
    list.Parent := ts;
    list.Align := alClient;
    list.ViewStyle := vsReport;
    list.ReadOnly := true;
    list.GridLines := true;
    list.RowSelect := true;

    with list.Columns.Add do
    begin
      Caption := 'Variable';
      Width := 100;
    end;
    with list.Columns.Add do
    begin
      Caption := 'Value';
      Width := -1;
    end;

    inc( datasize, zq.FieldByName('Data_length').AsInteger );
    inc(indexsize, zq.FieldByName('Index_length').AsInteger);
    for i:=0 to FieldList.count-1 do
    begin
      with List.Items.add do
      begin
        Caption := FieldList[i];
        SubItems.Add( zq.Fields[i].AsString);
      end;
    end;
    zq.Next;
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
