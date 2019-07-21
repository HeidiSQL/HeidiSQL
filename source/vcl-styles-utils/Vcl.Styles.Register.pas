unit Vcl.Styles.Register;

interface

procedure Register;

implementation

uses
  System.Classes, Vcl.Styles.Preview;

procedure Register;
begin
  RegisterComponents('VisualStyles', [TVisualStylePreview]);
end;

end.
