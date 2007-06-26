unit tblcomment;


// -------------------------------------
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
  screen.Cursor := crHourGlass;
  Mainform.Childwin.ExecUpdateQuery('ALTER TABLE ' + mainform.mask(ComboBoxTableName.Text) + ' COMMENT = ' + esc(EditComment.Text));
  Mainform.Childwin.ShowDBProperties(self);
  close;
end;

procedure Ttablecomment.FormShow(Sender: TObject);
var i : integer;
begin
  // read tables
//  messagedlg('sdf',mtwarning, [], 0);
  ComboBoxTableName.Items.Clear;
  for i:=0 to Mainform.Childwin.ListTables.Items.Count-1 do
    ComboBoxTableName.Items.Add( Mainform.Childwin.ListTables.Items[i].Caption );
  ComboBoxTableName.ItemIndex := Mainform.Childwin.ListTables.Selected.Index;
  ComboBoxTableNameChange( self );
end;

procedure Ttablecomment.ComboBoxTableNameChange(Sender: TObject);
begin
  Mainform.Childwin.GetResults( 'SHOW TABLE STATUS LIKE ''' + ComboBoxTableName.Text + '''', Mainform.Childwin.ZQuery3 );
  EditComment.Text := Mainform.Childwin.ZQuery3.FieldByName( 'Comment' ).AsString;
end;

end.
