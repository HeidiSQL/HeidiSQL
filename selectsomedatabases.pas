unit selectsomedatabases;


// -------------------------------------
// HeidiSQL
// Select some or all databases to view
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, ExtCtrls, Registry;

type
  TSelectFromManyDatabases = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    CheckListBoxDBs: TCheckListBox;
    Image1: TImage;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure CheckListBoxDBsClickCheck(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectFromManyDatabases: TSelectFromManyDatabases;

implementation

uses main, childwin, connections;

{$R *.DFM}




procedure TSelectFromManyDatabases.Button1Click(Sender: TObject);
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
      OnlyDBs2.clear;
      with CheckListBoxDBs do
      for i:=0 to Items.Count -1 do
      begin
        if Checked[i] then
          OnlyDBs2.Add(Items[i]);
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
    Button1.Caption := 'Show selected';
    Button2.Enabled := true;
  end
  else
  begin
    Button1.Caption := 'Show all';
    Button2.Enabled := false;
  end
end;

procedure TSelectFromManyDatabases.FormShow(Sender: TObject);
begin
  Button1.Caption := 'Show all';
  Button2.Enabled := false;
end;

procedure TSelectFromManyDatabases.Button2Click(Sender: TObject);
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
    openkey(regpath + '\Servers\' + connform.ComboBoxDescription.Text, true);
    WriteString('OnlyDBs', odbs);
    closekey();
  end;
  Button2.Enabled := false;
  MessageDlg('Saved selected Databases to your local settings.', mtInformation, [mbOK], 0);
end;

end.
