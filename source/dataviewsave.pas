unit dataviewsave;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

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
  public
    { Public declarations }
  end;

{$I const.inc}

implementation

{$R *.dfm}

uses main, helpers;


procedure TFrmDataViewSave.FormCreate(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  InheritFont(Font);
  // Load all view names into combobox
  Mainform.GetDataViews(comboSave.Items);
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
  viewName, basekey: String;
  Sort: WideString;
  i: Integer;
begin
  // Save current view stuff to registry
  Screen.Cursor := crHourglass;
  viewName := comboSave.Text;
  basekey := Mainform.GetRegKeyTable + '\' + REGPREFIX_DATAVIEW + viewName;
  if MainReg.OpenKey(basekey, True) then begin
    MainReg.WriteString(REGNAME_DISPLAYEDCOLUMNS, Utf8Encode(Mainform.FDataGridSelect.DelimitedText));
    MainReg.WriteString(REGNAME_FILTER, Utf8Encode(Mainform.SynMemoFilter.Text));
    for i := 0 to High(Mainform.FDataGridSort) do
      Sort := Sort + IntToStr(Mainform.FDataGridSort[i].SortDirection) + '_' + Mainform.FDataGridSort[i].ColumnName + REGDELIM;
    MainReg.WriteString(REGNAME_SORT, Utf8Encode(Sort));
    if chkDefault.Checked then begin
      MainReg.OpenKey(Mainform.GetRegKeyTable, False);
      MainReg.WriteString(REGNAME_DEFAULTVIEW, viewName);
    end;
  end;
  Screen.Cursor := crDefault;
end;


end.
