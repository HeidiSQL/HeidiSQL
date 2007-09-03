unit tbl_properties;


// -------------------------------------
// Advanced table-properties
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, HeidiComp, ExtCtrls, ZDataset, SynMemo, Synedit,
  VirtualTrees, Registry, Menus;

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
    StatusBar1: TStatusBar;
    btnClose: TButton;
    popupSynMemo: TPopupMenu;
    Copy1: TMenuItem;
    menuSelectAll: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure menuSelectAllClick(Sender: TObject);
    procedure SplitterMove(Sender: TObject);
  end;

  function tbl_properties_Window(AOwner: TComponent): Boolean;

implementation

uses
  Childwin, Main, helpers, Db;

{$R *.DFM}

const
  DEFAULT_LISTHEIGHT = 150;


{**
  Create form on demand
  @param TComponent Owner of form (should be calling form)
  @return Boolean Form closed using modalresult mrOK
}
function tbl_properties_Window(AOwner: TComponent): Boolean;
var
  f : Ttbl_properties_form;
begin
  f := Ttbl_properties_form.Create(AOwner);
  Result := (f.ShowModal=mrOK);
  FreeAndNil(f);
end;


{**
  Form gets displayed.
  - Setup dimensions of components
  - create one page + listview + synmemo for each selected table.
  - display data and CREATE TABLE statement
  - calculate the sum of their size
}
procedure Ttbl_properties_form.FormShow(Sender: TObject);
var
  i: Integer;
  t: Integer;
  ts: TTabSheet;
  list: TSortListView;
  datasize: Int64;
  indexsize: Int64;
  isSelected: Boolean;
  splitter: TSplitter;
  synmemo: TSynMemo;
  query: TDataSet;
  Selected: TStringList;
  reg : TRegistry;
  listheight : Integer;
begin

  datasize  := 0;
  indexsize := 0;
  listheight := DEFAULT_LISTHEIGHT;

  reg := TRegistry.Create;
  reg.OpenKey( REGPATH, true );
  try
    // Read values
    if reg.ValueExists( REGNAME_TBLPROP_LISTHEIGHT ) then
      listheight := reg.ReadInteger( REGNAME_TBLPROP_LISTHEIGHT );
    if reg.ValueExists( REGNAME_TBLPROP_FORMHEIGHT ) then
      Height := reg.ReadInteger( REGNAME_TBLPROP_FORMHEIGHT );
    if reg.ValueExists( REGNAME_TBLPROP_FORMWIDTH ) then
      Width := reg.ReadInteger( REGNAME_TBLPROP_FORMWIDTH );
  finally
    reg.CloseKey;
    reg.Free;
  end;

  // Fetch selected nodes
  Selected := GetVTCaptions(Mainform.Childwin.ListTables, True);

  query := Mainform.Childwin.GetResults( 'SHOW TABLE STATUS');

  // for tables found
  for t := 0 to (query.RecordCount - 1) do
  begin
    isSelected := false;

    for i := 0 to Selected.Count-1 do
    begin
      isSelected := Selected[i] = query.Fields[0].AsString;
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
    ts.Caption     := query.Fields[0].AsString;
    ts.PageControl := PageControl;

    // creates a splitter
    splitter        := TSplitter.Create(self);
    splitter.Parent := ts;
    splitter.ResizeStyle := rsUpdate;
    splitter.OnMoved := SplitterMove;

    // create a detailed properties list
    list           := TSortListView.Create(self);
    list.Parent    := ts;
    list.Height    := listheight;
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
    for i:=0 to query.FieldCount - 1 do
    begin
      with (list.Items.add) do
      begin
        Caption := query.Fields[i].FieldName;
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
    synmemo.Lines.Text       := Mainform.Childwin.GetVar( 'SHOW CREATE TABLE ' + Mainform.Childwin.mask(query.Fields[0].AsString), 1 );
    synmemo.PopupMenu        := popupSynMemo;

    // realign the components to correct position
    list.Align     := alTop;
    splitter.Align := alTop;
    synmemo.Align  := alClient;

    // go to next table
    query.Next();
  end;

  LabelSizeOfTableDataValue.Caption := FormatByteNumber(datasize);
  LabelIndexesValue.Caption         := FormatByteNumber(indexsize);
  LabelSumValue.Caption             := FormatByteNumber(datasize + indexsize);
end;


{**
  Form closes. Store dimensions of relevant components.
}
procedure Ttbl_properties_form.FormClose(Sender: TObject; var Action:
    TCloseAction);
var
  reg: TRegistry;
  i, listheight: Integer;
begin
  // Find height of dynamically created listviews
  listheight := DEFAULT_LISTHEIGHT;
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i] is TListView then
    begin
      listheight := TListView(Components[i]).Height;
      break;
    end;
  end;

  reg := TRegistry.Create;
  reg.OpenKey( REGPATH, true );
  try
    // Save values
    reg.WriteInteger( REGNAME_TBLPROP_LISTHEIGHT, listheight );
    reg.WriteInteger( REGNAME_TBLPROP_FORMHEIGHT, Height );
    reg.WriteInteger( REGNAME_TBLPROP_FORMWIDTH, Width );
  finally
    reg.CloseKey;
    reg.Free;
  end;
end;


{**
  Form has been resized. Adjust position of Close-button.
}
procedure Ttbl_properties_form.FormResize(Sender: TObject);
begin
  btnClose.Left := PanelSummary.Width div 2 - (btnClose.Width div 2);
end;


{**
  Select all text in a synmemo
}
procedure Ttbl_properties_form.menuSelectAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
  begin
    if (Components[i] is TSynMemo) and (TSynMemo(Components[i]).Parent = PageControl.ActivePage) then
    begin
      TSynMemo(Components[i]).SelectAll;
      break;
    end;
  end;
end;


{**
  Splitter between list and synmemo was moved.
  Move all other splitters to the same position
}
procedure Ttbl_properties_form.SplitterMove(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i] is TListView then
      TSortListView(Components[i]).Height := TSplitter(Sender).Top;
  end;
end;


end.
