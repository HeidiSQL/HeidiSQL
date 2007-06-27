unit tblcomment;


// -------------------------------------
// HeidiSQL
// Edit table-comment
// -------------------------------------


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  Ttablecomment = class(TForm)
    Label1: TLabel;
    EditComment: TEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    Bevel1: TBevel;
    ComboBoxTableName: TComboBox;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxTableNameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  tablecomment: Ttablecomment;

implementation

uses Childwin, helpers, Main;

{$R *.DFM}

procedure Ttablecomment.ButtonCancelClick(Sender: TObject);
begin
  close;
end;

procedure Ttablecomment.ButtonOKClick(Sender: TObject);
begin
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    screen.Cursor := crHourGlass;
    ExecQuery('ALTER TABLE ' + mainform.mask(ComboBoxTableName.Text) + ' COMMENT = ''' + escape_string(EditComment.Text) + '''');
    ShowDBProperties(self);
  end;
  close;
end;

procedure Ttablecomment.FormShow(Sender: TObject);
var i : integer;
begin
  // read tables
//  messagedlg('sdf',mtwarning, [], 0);
  ComboBoxTableName.Items.Clear;
  with TMDIChild(Application.Mainform.ActiveMDIChild) do
  begin
    for i:=0 to Tabellenliste.Items.Count-1 do
      self.ComboBoxTableName.Items.Add( Tabellenliste.Items[i].Caption );
    self.ComboBoxTableName.ItemIndex := Tabellenliste.Selected.Index;
  end;
  ComboBoxTableNameChange( self );
end;

procedure Ttablecomment.ComboBoxTableNameChange(Sender: TObject);
begin
  with TMDIChild(Mainform.ActiveMDIChild) do
  begin
    GetResults( 'SHOW TABLE STATUS LIKE ''' + ComboBoxTableName.Text + '''', ZQuery3 );
    self.EditComment.Text := ZQuery3.FieldByName( 'Comment' ).AsString;
  end;
end;

end.
