unit dataviewsave;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, childwin;

type
  TFrmDataViewSave = class(TForm)
    comboSave: TComboBox;
    lblSave: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    chkDefault: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure comboSaveChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    cwin: TMDIChild;
  public
    { Public declarations }
  end;

{$I const.inc}

implementation

{$R *.dfm}

uses main;


procedure TFrmDataViewSave.FormCreate(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  InheritFont(Font);
  cwin := Mainform.Childwin;
  // Load all view names into combobox
  cwin.GetDataViews(comboSave.Items);
  comboSaveChange(comboSave);
  Screen.Cursor := crDefault;
end;


procedure TFrmDataViewSave.comboSaveChange(Sender: TObject);
begin
  // OK button is only enabled if name is not empty
  btnOK.Enabled := (Sender as TComboBox).Text <> '';
end;


procedure TFrmDataViewSave.btnOKClick(Sender: TObject);
var
  viewName, basekey, Sort: String;
  reg: TRegistry;
  i: Integer;
begin
  // Save current view stuff to registry
  Screen.Cursor := crHourglass;
  viewName := comboSave.Text;
  reg := TRegistry.Create;
  basekey := cwin.GetRegKeyTable + '\' + REGPREFIX_DATAVIEW + viewName;
  if reg.OpenKey(basekey, True) then begin
    reg.WriteString(REGNAME_DISPLAYEDCOLUMNS, Utf8Encode(cwin.FDataGridSelect.DelimitedText));
    reg.WriteString(REGNAME_FILTER, Utf8Encode(cwin.SynMemoFilter.Text));
    for i := 0 to High(cwin.FDataGridSort) do
      Sort := Sort + IntToStr(cwin.FDataGridSort[i].SortDirection) + '_' + cwin.FDataGridSort[i].ColumnName + REGDELIM;
    reg.WriteString(REGNAME_SORT, Utf8Encode(Sort));
    if chkDefault.Checked then begin
      reg.OpenKey(cwin.GetRegKeyTable, False);
      reg.WriteString(REGNAME_DEFAULTVIEW, viewName);
    end;
    reg.CloseKey;
  end;
  FreeAndNil(reg);
  Screen.Cursor := crDefault;
end;


end.
