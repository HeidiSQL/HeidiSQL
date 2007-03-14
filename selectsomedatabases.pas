unit selectsomedatabases;


// -------------------------------------
// Select some or all databases to view
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls, Registry;

type
  TSelectFromManyDatabases = class(TForm)
    lblSelectDB: TLabel;
    btnShowAll: TButton;
    lblWarning: TLabel;
    CheckListBoxDBs: TCheckListBox;
    imgWarning: TImage;
    btnSave: TButton;
    procedure btnShowAllClick(Sender: TObject);
    procedure CheckListBoxDBsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FDbList : TStringList;
    procedure SetDbList(const Value: TStringList);
  public
    property DbList : TStringList read FDbList write SetDbList;
  end;

  function SelectFromManyDatabasesWindow (AOwner : TComponent; ADbList : TStringList; Flags : String = '') : Boolean;


implementation

uses main, childwin, connections;

{$R *.DFM}


function SelectFromManyDatabasesWindow (AOwner : TComponent; ADbList : TStringList; Flags : String = '') : Boolean;
var
  f : TSelectFromManyDatabases;
begin
  f := TSelectFromManyDatabases.Create(AOwner);
  f.DbList := ADbList;
  Result := (f.ShowModal=mrOK);
  FreeAndNil (f);
end;


procedure TSelectFromManyDatabases.btnShowAllClick(Sender: TObject);
var
  i : Integer;
  someselected : Boolean;
begin
  someselected := false;

  for i:=0 to CheckListBoxDBs.Items.Count -1 do
  begin
    if CheckListBoxDBs.Checked[i] then
    begin
      someselected := true;
      break;
    end;
  end;

  if someselected then
    with TMDIChild(Application.Mainform.ActiveMDIChild) do
    begin
      //OnlyDBs2.clear;
      FDbList.Clear;
      with CheckListBoxDBs do
      for i:=0 to Items.Count -1 do
      begin
        if Checked[i] then
          FDbList.Add(Items[i]);
      end;
    end;

  close;
end;

procedure TSelectFromManyDatabases.CheckListBoxDBsClickCheck(
  Sender: TObject);
var
  i : Integer;
  someselected : Boolean;
begin
  someselected := false;
  for i:=0 to CheckListBoxDBs.Items.Count -1 do
  begin
    if CheckListBoxDBs.Checked[i] then
    begin
      someselected := true;
      break;
    end;
  end;
  if someselected then
  begin
    btnShowAll.Caption := 'Show selected';
    btnSave.Enabled := true;
  end
  else
  begin
    btnShowAll.Caption := 'Show all';
    btnSave.Enabled := false;
  end
end;

procedure TSelectFromManyDatabases.FormShow(Sender: TObject);
begin
  btnShowAll.Caption := 'Show all';
  btnSave.Enabled := false;
end;

procedure TSelectFromManyDatabases.SetDbList(const Value: TStringList);
begin
  FDbList := Value;

  //CheckListBoxDBs.Items.Clear;

  if Value<>nil then
    CheckListBoxDBs.Items := Value;
end;

procedure TSelectFromManyDatabases.btnSaveClick(Sender: TObject);
var
  i : Integer;
  odbs : String;
begin
  for i:=0 to CheckListBoxDBs.Items.Count -1 do
  if CheckListBoxDBs.Checked[i] then
  begin
    if odbs <> '' then
      odbs := odbs + ';';
    odbs := odbs + CheckListBoxDBs.Items[i];
  end;

  // save settings:
  with TRegistry.Create do
  begin
    openkey(REGPATH + '\Servers\' + TMDIChild(Mainform.ActiveMDIChild).ConnParams.Description, true);
    WriteString('OnlyDBs', odbs);
    closekey();
  end;
  btnSave.Enabled := false;
  MessageDlg('Saved selected Databases to your local settings.', mtInformation, [mbOK], 0);
end;

end.
