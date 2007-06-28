unit tbl_properties;


// -------------------------------------
// Advanced table-properties
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, HeidiComp, ExtCtrls, ZDataset, SynMemo, Synedit;

type
  Ttbl_properties_form = class(TForm)
    PageControl: TPageControl;
    PanelSummary: TPanel;
    LabelSizeOfTableData: TLabel;
    LabelIndexes: TLabel;
    LabelSizeOfTableDataValue: TLabel;
    LabelIndexesValue: TLabel;
    LabelSum: TLabel;
    LabelSumValue: TLabel;
    Bevel: TBevel;
    PanelControl: TPanel;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  end;

var
  tbl_properties_form: Ttbl_properties_form;

implementation

uses
  Childwin, Main, helpers, Db;

{$R *.DFM}


{***


}
procedure Ttbl_properties_form.FormShow(Sender: TObject);
var
  i: Integer;
  t: Integer;
  ts: TTabSheet;
  list: TSortListView;
  ListTables: TSortListView;
  datasize: Int64;
  indexsize: Int64;
  isSelected: Boolean;
  FieldList: TStringList;
  splitter: TSplitter;
  synmemo: TSynMemo;
  query: TDataSet;
begin
  ListTables  := Mainform.Childwin.ListTables;

  for i := (PageControl.PageCount - 1) downto 0 do
  begin
    PageControl.Pages[i].Free();
  end;

  datasize  := 0;
  indexsize := 0;

  query := Mainform.Childwin.GetResults( 'SHOW TABLE STATUS');

  FieldList := TStringList.Create;
  for i := 0 to (query.FieldCount - 1) do
  begin
    FieldList.add( query.Fields[i].Fieldname );
  end;

  // for tables found
  for t := 0 to (query.RecordCount - 1) do
  begin
    isSelected := false;

    for i := 0 to (ListTables.Items.Count - 1) do
    begin
      isSelected := (ListTables.Items[i].caption = query.Fields[0].AsString) and ListTables.Items[i].Selected;
      if (isSelected) then
      begin
        Break;
      end;
    end;

    if (not(isSelected)) then
    begin
      query.Next();
      Continue;
    end;

    // if a table is selected

    // creates a tab
    ts             := TTabSheet.Create(PageControl);
    ts.Caption     := ListTables.Items[t].Caption;
    ts.PageControl := PageControl;

    // creates a splitter
    splitter        := TSplitter.Create(self);
    splitter.Parent := ts;

    // create a detailed properties list
    list           := TSortListView.Create(self);
    list.Parent    := ts;
    list.Height    := 150;
    list.ViewStyle := vsReport;
    list.ReadOnly  := true;
    list.GridLines := true;
    list.RowSelect := true;
    list.BringToFront();

    with (list.Columns.Add) do
    begin
      Caption := 'Variable';
      Width := 100;
    end;
    with (list.Columns.Add) do
    begin
      Caption := 'Value';
      Width := -1;
    end;

    inc( datasize, StrToInt64Def(query.FieldByName('Data_length').AsString, 0) );
    inc( indexsize, StrToInt64Def(query.FieldByName('Index_length').AsString, 0) );
    for i:=0 to FieldList.count-1 do
    begin
      with (list.Items.add) do
      begin
        Caption := FieldList[i];
        SubItems.Add( query.Fields[i].AsString);
      end;
    end;

    // create a souce viewer
    synmemo                  := TSynMemo.Create(self);
    synmemo.Parent           := ts;
    synmemo.Highlighter      := Mainform.Childwin.SynSQLSyn1;
    synmemo.ReadOnly         := true;
    synmemo.Options          := synmemo.options - [eoScrollPastEol];
    synmemo.Gutter.Visible   := false;
    synmemo.Margins.Top      := 0;
    synmemo.Margins.Left     := 0;
    synmemo.Margins.Right    := 0;
    synmemo.Margins.Bottom   := 0;
    synmemo.AlignWithMargins := true;
    synmemo.Font.Name        := Mainform.Childwin.SynMemoQuery.Font.Name;
    synmemo.Font.Size        := Mainform.Childwin.SynMemoQuery.Font.Size;
    synmemo.Lines.Text       := Mainform.Childwin.GetVar( 'SHOW CREATE TABLE ' + Mainform.Childwin.mask(ListTables.Items[t].Caption), 1 );


    // realign the components to correct position
    list.Align     := alTop;
    splitter.Align := alTop;
    synmemo.Align  := alClient;

    // go to next table
    query.Next();
  end;

  LabelSizeOfTableDataValue.Caption := FormatNumber(datasize div 1024) + ' KB';
  LabelIndexesValue.Caption         := FormatNumber(indexsize div 1024) + ' KB';
  LabelSumValue.Caption             := FormatNumber(datasize div 1024 + indexsize div 1024) + ' KB';
end;


procedure Ttbl_properties_form.btnCloseClick(Sender: TObject);
begin
  Close();
end;

procedure Ttbl_properties_form.FormResize(Sender: TObject);
begin
  btnClose.Left := PanelControl.Width div 2 - (btnClose.Width div 2);

  LabelSizeOfTableDataValue.Left := PanelSummary.Width - LabelSizeOfTableDataValue.Width - 8;
  LabelIndexesValue.Left         := PanelSummary.Width - LabelIndexesValue.Width - 8;
  LabelSumValue.Left             := PanelSummary.Width - LabelSumValue.Width - 8;
end;

end.
