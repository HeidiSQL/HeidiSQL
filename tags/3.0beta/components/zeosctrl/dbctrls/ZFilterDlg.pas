{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 Dataset filter dialog                  }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZFilterDlg;

interface

{$R *.DCR}

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, Db, Menus, Dialogs, Mask, DBCtrls, Math, ZConnect, ZQuery, ZSQLExtra;

{$INCLUDE ..\Zeos.inc}

type
  { Dataset filter dialog}
  TZFilterDialog = class(TComponent)
  private
    FDataSet: TZDataset;
  protected
    function  GetTextFilter(const AFilter: string; Direct: Boolean): string;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function  Execute: Boolean;
    procedure Sort(FieldName: string);
    procedure SortDesc(FieldName: string);
    procedure ClearFilter;
    procedure SetFilter(const Filter: string);
    function  GetFilter: string;
    procedure SetIncludeFilter(FieldName: string);
    procedure SetExcludeFilter(FieldName: string);
  published
    property DataSet: TZDataset read FDataSet write FDataSet;
  end;

  TfrmFilterDlg = class(TForm)
    mmFilter: TMemo;
    gbField:  TGroupBox;
    btnEqual: TSpeedButton;
    btnAnd: TSpeedButton;
    btnNotEqual: TSpeedButton;
    btnLess: TSpeedButton;
    btnOr: TSpeedButton;
    btnNot: TSpeedButton;
    btnMore: TSpeedButton;
    btnLessEqual: TSpeedButton;
    btnEqualMore: TSpeedButton;
    btnXor: TSpeedButton;
    btnAdd: TSpeedButton;
    btnSub: TSpeedButton;
    btnDiv: TSpeedButton;
    btnMul: TSpeedButton;
    btnPower: TSpeedButton;
    btnMod: TSpeedButton;
    btnField: TSpeedButton;
    btnBraketLeft: TSpeedButton;
    btnBraketRight: TSpeedButton;
    lsbFields: TListBox;
    btnDelFilter: TSpeedButton;
    btnSetFilter: TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure lsbFieldsDblClick(Sender: TObject);
    procedure btnSetFilterClick(Sender: TObject);
    procedure btnDelFilterClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFilterDlg: TZFilterDialog;
    function  GetFilter: string;
    function  GetDataSet: TZDataset;
  public
    property Filter: string read GetFilter;
    function Execute: Boolean;
    property DataSet: TZDataset read GetDataSet;
  end;

var
  frmFilterDlg: TfrmFilterDlg;

implementation

{$R *.DFM}

uses ZToken, ZDbCtrlsConst;

{**********************************************************}

{ Format field value }
function ConvertValField(Field: TField): string;
var
  Temp: Char;
begin
  Result := '';
  Temp := DecimalSeparator;
  DecimalSeparator := '.';
  if not Assigned(Field) then Exit;
  case Field.DataType of
    ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency
    {$IFNDEF VER100}, ftLargeInt {$ENDIF}, ftAutoInc :
      Result := Field.AsString;
    else
      Result := '"' + Field.AsString + '"';
  end;
  DecimalSeparator := Temp;
end;

{***************** TZFilterDialog implementation *****************}

{ Process notification method }
procedure TZFilterDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDataset) and (Operation = opRemove) then
    FDataset := nil;
end;

{ Show form and set filter equation }
function TZFilterDialog.Execute: Boolean;
var
  frmFilter: TfrmFilterDlg;
begin
  Result := False;
  if not Assigned(FDataSet) then Exit;

  Application.CreateForm(TfrmFilterDlg, frmFilter);
  try
    frmFilter.FFilterDlg := Self;
    Result := frmFilter.Execute;
  finally
    frmFilter.Destroy;
  end;
end;

{ Clear filter equation }
procedure TZFilterDialog.ClearFilter;
begin
  with DataSet do
  begin
    Filtered := False;
    Filter := '';
  end;
end;

{ Ascending sort by field }
procedure TZFilterDialog.Sort(FieldName: string);
begin
  if Assigned(FDataset) then
    FDataset.SortByField(FieldName);
end;

{ Descending sort by field }
procedure TZFilterDialog.SortDesc(FieldName: string);
begin
  if Assigned(FDataset) then
    FDataset.SortDescByField(FieldName);
end;

{ Filter only include the current field value }
procedure TZFilterDialog.SetIncludeFilter(FieldName: string);
var
  FilterStr, AFieldName: string;
  Field: TField;
begin
  if not Assigned(FDataset) then Exit;
  Field := FDataset.FieldByName(FieldName);
  if not Assigned(Field) then Exit;

  with Field.DataSet do
  begin
    if not (Field.FieldKind in [fkData, fkLookup]) then Exit;
    if Field.FieldKind = fkLookup then
      AFieldName := Field.KeyFields
    else
      AFieldName := Field.FieldName;
    FilterStr := Filter;
    if FilterStr <> '' then
      FilterStr := '('+FilterStr+') AND ';
    if Pos(' ', AFieldName) > 0 then
      AFieldName := '['+AFieldName+']';
    FilterStr := FilterStr + AFieldName + '='
      + ConvertValField(FDataset.FieldByName(AFieldName));
    try
      Filter := FilterStr;
      Filtered := True;
    except on E : Exception do begin
        MessageDlg(SFilterError,mtError,[mbOk], 0);
        Filtered := False;
        Filter := '';
      end;
    end;
  end;
end;

{ Get dataset value }
function TfrmFilterDlg.GetDataSet: TZDataset;
begin
  if Assigned(FFilterDlg) then
    Result := FFilterDlg.DataSet
  else
    Result := nil;
end;

{ On form show event }
procedure TfrmFilterDlg.FormShow(Sender: TObject);
var
  I: Integer;
begin
  mmFilter.Lines.Text := FFilterDlg.GetFilter;
  lsbFields.Items.Clear;
  with DataSet do
    for I := 0 to FieldCount - 1 do
      if Fields[I].Visible then
        lsbFields.Items.Add(Fields[I].DisplayName);
  lsbFields.ItemIndex := 0;

  Caption := SFilterCaption;
  gbField.Caption   := SFilterFields;
  btnField.Caption  := SFieldCaption;
  btnField.Hint     := SFieldHint;
  btnSetFilter.Hint := SSetFilterHint;
  btnDelFilter.Hint := SClearFilterHint;
end;

{ Set a filter }
function TfrmFilterDlg.Execute: Boolean;
begin
  Position := poScreenCenter;
  Result   := (ShowModal = mrOk);
end;

{ Decode filter string }
function TZFilterDialog.GetTextFilter(const AFilter: string; Direct: Boolean): string;

{ Convert field desc to field name }
  function ConvertNameField(AFieldName: string): string;
  var
    Field: TField;
    I: Integer;
  begin
    Result := AFieldName;
    if (AFieldName[1] = '[') and (AFieldName[Length(AFieldName)] = ']') then
      AFieldName := Copy(AFieldName, 2, Length(AFieldName) - 2);

    if Direct then with DataSet do
    begin
      for I := 0 to FieldCount - 1 do
        if Fields[I].DisplayName = AFieldName then
        begin
          Result := Fields[I].FieldName;
          if Pos(' ',Result) > 0 then
            Result := '['+ Result +']';
          Break;
        end
    end
    else
    begin
      Field := DataSet.FindField(AFieldName);
      if not Assigned(Field) then Exit;
      Result := Field.DisplayName;
      if Pos(' ',Result) > 0 then
        Result := '['+ Result +']';
    end;
  end;

const
  SkipToken = ' OR NOT XOR AND LIKE ';
var
  Sub, Token: string;
begin
  Result := '';
  Sub := AFilter;
  while Sub <> '' do
  begin
    if ExtractTokenEx(Sub, Token) = ttAlpha then
      if Pos(UpperCase(Token), SkipToken) = 0 then
        Result := Result + ConvertNameField(Token)
      else
        Result := Result + ' ' + Token + ' '
    else
      Result := Result + Token;
  end;
end;

{ Setting filter equation }
procedure TZFilterDialog.SetFilter(const Filter: string);
begin
  DataSet.Filter   := GetTextFilter(Filter, True);
  DataSet.Filtered := True;
end;

{ Get filter equation }
function TZFilterDialog.GetFilter: string;
begin
  if DataSet.Filtered then
    Result := GetTextFilter(DataSet.Filter, False)
  else
    Result := '';
end;

{ Filter only exclude the current field value }
procedure TZFilterDialog.SetExcludeFilter(FieldName: string);
var
  FilterStr, AFieldName: string;
  Field: TField;
begin
  if not Assigned(FDataset) then Exit;
  Field := FDataset.FieldByName(FieldName);
  if not Assigned(Field) then Exit;
  with Field.DataSet do
  begin
    if not (Field.FieldKind in [fkData, fkLookup]) then Exit;
    if Field.FieldKind = fkLookup then
      AFieldName := Field.KeyFields
    else
      AFieldName := Field.FieldName;
    FilterStr := Filter;
    if FilterStr <> '' then
      FilterStr := '('+FilterStr+') AND ';
    if Pos(' ', AFieldName) > 0 then
      AFieldName := '['+AFieldName+']';
    FilterStr := FilterStr+AFieldName+'<>'
      +ConvertValField(FDataset.FieldByName(AFieldName));
    Filter    := FilterStr;
    Filtered  := True;
  end;
end;

{ On operation button click }
procedure TfrmFilterDlg.btnFilterClick(Sender: TObject);
var
  EnterStr: string;
begin
  with Sender as TSpeedButton do
  if Name <> 'btnField' then
  begin
    EnterStr := Caption;
    if Length(EnterStr) = 3 then
      EnterStr := ' ' + EnterStr + ' ';
    mmFilter.SetSelTextBuf(PChar(EnterStr));
  end else
    lsbFieldsDblClick(lsbFields)
end;

{ On field click }
procedure TfrmFilterDlg.lsbFieldsDblClick(Sender: TObject);
var
  EnterStr: string;
begin
  EnterStr := lsbFields.Items[lsbFields.ItemIndex];
  if Pos(' ', EnterStr) > 0 then
    EnterStr := '[' + EnterStr + ']'
  else
    EnterStr := ' ' + EnterStr + ' ';
  mmFilter.SetSelTextBuf(PChar(EnterStr));
end;

{ Get filter equation string }
function TfrmFilterDlg.GetFilter: string;
begin
  Result := mmFilter.Lines.Text;
end;

{ On set filter button click }
procedure TfrmFilterDlg.btnSetFilterClick(Sender: TObject);
begin
  FFilterDlg.SetFilter(Filter);
  ModalResult := mrOk;
end;

{ On clear filter button click }
procedure TfrmFilterDlg.btnDelFilterClick(Sender: TObject);
begin
  FFilterDlg.ClearFilter;
  ModalResult := mrOk;
end;

{ On key down event }
procedure TfrmFilterDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    ModalResult := mrCancel;
  end;
  if (ssCtrl in Shift) and (Key = VK_RETURN) then
  begin
    Key := 0;
    btnSetFilterClick(Sender);
  end;
end;

end.

