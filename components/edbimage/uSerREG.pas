unit uSerREG;

interface

{$R CompsSer.dcr}
{$I Definitions.inc}
procedure Register;

implementation
uses Classes , {RegisterComponent}

     {$IFDEF DELPHI6}
      DesignIntf, DesignEditors,
     {$ELSE}
      DsgnIntf, { RegisterComponentEditor}
     {$ENDIF}

      TypInfo, {propiedad Dataset del QREDBimage}
      DB, {para acceder al Dataset del reporte}

      {     PictEdit, {TGraphicEditor }
     // qrEDBimage,
     edbimage;  { mis componentes }

type
  TQREDBImageFieldProperty = Class (TStringProperty)
  public
    function GetAttributes : TPropertyAttributes; override;
    procedure GetValues(Proc : TGetStrProc); override;
    procedure GetValueList(List : TStrings);
  end;


{ TQREDBImageFieldProperty }

function TQREDBImageFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TQREDBImageFieldProperty.GetValueList(List: TStrings);
var
  Instance: TComponent;
  PropInfo: PPropInfo;
  DataSet: TDataSet;
begin
  Instance := TComponent(GetComponent(0));
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, 'DataSet');
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
    begin
    DataSet := TObject(GetOrdProp(Instance, PropInfo)) as TDataSet;
    if (DataSet <> nil) then
      DataSet.GetFieldNames(List);
    end;
end;

procedure TQREDBImageFieldProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('Data Controls', [TEDBImage]);
  //RegisterComponents('QReport',[TQREDBImage]);
  //RegisterPropertyEditor(TypeInfo(string), TQREDBImage, 'DataField', TQREDBImageFieldProperty); {<-- do not resource }
end;


end.

