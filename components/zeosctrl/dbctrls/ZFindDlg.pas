{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{                 Dataset find dialog                    }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZFindDlg;

interface

{$R *.DCR}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, ZSqlExtra, ZQuery, ExtCtrls;

{$INCLUDE ..\Zeos.inc}

type
  TZLocateSimile = (lsPartialAny, lsExact, lsPartialStart);
  TZStartPos = (spUp, spDown, spAll);

  { Dataset find dialog }
  TZFindDialog = class(TComponent)
  private
    FDataSet:       TZDataset;
    FDataField:     string;
    FSimile:        TZLocateSimile;
    FCaseSensitive: Boolean;
    FIsPattern:     Boolean;
    FStartPos:      TZStartPos;
    FIncrSearch:    Boolean;
    FSelectFields:  Boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute;
  published
    property DataSet: TZDataset read FDataset write FDataset;
    property DataField: string read FDataField write FDataField;
    property Simile: TZLocateSimile read FSimile write FSimile default lsPartialStart;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property IsPattern: Boolean read FIsPattern write FIsPattern default False;
    property StartPos: TZStartPos read FStartPos write FStartPos default spAll;
    property IncrSearch: Boolean read FIncrSearch write FIncrSearch default False;
    property SelectFields: Boolean read FSelectFields write FSelectFields default False;
  end;

  TfrmFindDlg = class(TForm)
    pnTop: TPanel;
    cmbFields: TComboBox;
    lblField: TLabel;
    pnClient: TPanel;
    lblSample: TLabel;
    lblStartPos: TLabel;
    lblSimile: TLabel;
    edtSample: TEdit;
    btnFind: TButton;
    btnFindNext: TButton;
    btnClose: TButton;
    cmbStartPos: TComboBox;
    cmbSimile: TComboBox;
    cbxCaseSensitive: TCheckBox;
    cbxPattern: TCheckBox;
    cbxIncSearch: TCheckBox;
    procedure acCloseExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acFindExecute(Sender: TObject);
    procedure acFindNextExecute(Sender: TObject);
    procedure edtSampleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbFieldsChange(Sender: TObject);
  private
    FDataSet: TZDataset;
    FFieldName: string;

    function GetSimile: TZLocateSimile;
    procedure SetSimile(Value: TZLocateSimile);
    function GetCaseSensetive: Boolean;
    procedure SetCaseSensetive(Value: Boolean);
    function GetPattern: Boolean;
    procedure SetPattern(Value: Boolean);
    function GetStartPos: TZStartPos;
    procedure SetStartPos(Value: TZStartPos);
    function GetSample: string;
    procedure SetSample(Value: string);
    function GetIncrSearch: Boolean;
    procedure SetIncrSearch(Value: Boolean);
    function GetSelectFields: Boolean;
    procedure SetSelectFields(Value: Boolean);
    procedure EnableFieldProp(FieldSearch: TField);
    function FindField(DisplayLabel: string): string;
  public
    procedure Execute;
    property DataSet: TZDataset read FDataSet write FDataSet;
    property Sample: string read GetSample write SetSample;
    property FieldName: string read FFieldName write FFieldName;
    property Simile: TZLocateSimile read GetSimile write SetSimile;
    property CaseSensitive: Boolean read GetCaseSensetive write SetCaseSensetive;
    property IsPattern: Boolean read GetPattern write SetPattern;
    property StartPos: TZStartPos read GetStartPos write SetStartPos;
    property IncrSearch: Boolean read GetIncrSearch write SetIncrSearch default False;
    property SelectFields: Boolean read GetSelectFields write SetSelectFields default False;
  end;

var
  frmFindDlg: TfrmFindDlg;

implementation

{$R *.DFM}

uses ZMatch, ZDbCtrlsConst;

{**********************************************************}

{ Extended find (step by step) }
function FindAdvanced(DataSet: TZDataset; FieldName, Value: string;
  IsNext: Boolean; CaseSensitive: Boolean; IsPattern: Boolean;
  Simile: TZLocateSimile; StartPos: TZStartPos): Boolean;

  function IsNxt: Boolean;
  begin
    if StartPos = spUp then
      Result := not DataSet.Bof
    else
      Result := not DataSet.Eof;
  end;

  function CompString(Pattern, Value: string; Simile: TZLocateSimile;
    IsPattern: Boolean): Boolean;
  begin
    case Simile of
      lsExact:
        if IsPattern then
          Result := IsMatch(Pattern, Value)
        else
          Result := (Pattern = Value);
      lsPartialAny:
        if IsPattern then
          Result := IsMatch('*'+Pattern+'*', Value)
        else if (Length(Pattern) < Length(Value)) then
          Result := (Pos(Pattern, Value) <> 0)
        else
          Result := (Pos(Value, Pattern) <> 0);
      lsPartialStart:
        if IsPattern then
          Result := IsMatch(Pattern+'*',Value)
        else if Length(Pattern) <= Length(Value) then
          Result := (Pattern = Copy(Value, 1, Length(Pattern)))
        else
          Result := False; //(Value = Copy(Pattern, 1, Length(Value)));
     else
       Result := False;
    end;
  end;

var
  FieldFnd: TField;
  Bm: TBookMark;
  Opt: TLocateOptions;
  Search: string;
begin
  Result := False;
  FieldFnd := DataSet.FindField(FieldName);
  if FieldFnd = nil then
    Exit;
  { Find by Locate }
  if (StartPos = spAll) and not IsNext and (Simile <> lsPartialAny)
    and (FieldFnd.FieldKind=fkData) then
  begin
     if Simile = lsPartialStart then
       Opt := [loPartialKey]
     else
       Opt := [];
     if not CaseSensitive then
       Opt := Opt + [loCaseInsensitive];
     Result := DataSet.Locate('['+FieldName+']', Value, Opt);
     Exit;
  end;
  { Find step by step }
  Bm := DataSet.GetBookmark;
  DataSet.DisableControls;
  try
    if not CaseSensitive then
      Value := AnsiUpperCase(Value);
    if (StartPos = spAll) and not IsNext then
      DataSet.First;
    if IsNext then
      if StartPos = spUp then DataSet.Prior
      else DataSet.Next;
    while IsNxt do with DataSet do
    begin
      if not CaseSensitive then
        Search := AnsiUpperCase(FieldFnd.AsString)
      else
        Search := FieldFnd.AsString;
      if CompString(Value, Search, Simile, IsPattern) then
      begin
        Result := True;
        Break;
      end;
      if StartPos = spUp then Prior
      else Next;
    end;
  finally
    if not Result then DataSet.GotoBookmark(Bm);
    DataSet.FreeBookmark(Bm);
    DataSet.EnableControls;
  end;
end;

{ Enable/disable properties according selected field }
procedure TfrmFindDlg.EnableFieldProp(FieldSearch: TField);
begin
  case FieldSearch.DataType of
    ftString, ftMemo:
      begin
        cbxCaseSensitive.Enabled := True;
        cbxPattern.Enabled       := True;
        cmbSimile.Enabled        := True;
      end;
    ftDate, ftTime, ftDateTime:
      begin
        cbxCaseSensitive.Enabled := False;
        cbxPattern.Enabled       := True;
        cmbSimile.Enabled        := True;
      end;
    ftBoolean, ftInteger, ftFloat, ftSmallInt, ftCurrency:
      begin
        Simile := lsExact;
        cbxCaseSensitive.Enabled := False;
        cbxPattern.Enabled       := False;
        cmbSimile.Enabled        := False;
      end;
  end;
end;

{ Find field by display label }
function TfrmFindDlg.FindField(DisplayLabel: string): string;
var
  I: Integer;
begin
  Result := '';
  if not Assigned(DataSet) then Exit;
  for I := 0 to Dataset.FieldCount-1 do
    if Dataset.Fields[I].DisplayName = DisplayLabel then
    begin
      Result := Dataset.Fields[I].FieldName;
      Break;
    end;
end;

{ Change searched field }
procedure TfrmFindDlg.cmbFieldsChange(Sender: TObject);
var
  FieldSearch: TField;
begin
  FieldName := FindField(cmbFields.Items[cmbFields.ItemIndex]);
  if not Assigned(DataSet) then Exit;
  FieldSearch := DataSet.FindField(FieldName);
  EnableFieldProp(FieldSearch);
end;

{ Execute a find dialog }
procedure TfrmFindDlg.Execute;
var
  FieldSearch: TField;
  I: Integer;
begin
  if not Assigned(DataSet) then Exit;
  FieldSearch := DataSet.FindField(FieldName);
  if FieldSearch = nil then Exit;

  EnableFieldProp(FieldSearch);

  if SelectFields then
    Caption := SFindCaption
  else
    Caption := SFindBy + FieldSearch.DisplayName;

  cmbFields.Clear;
  for I := 0 to DataSet.FieldCount-1 do
    cmbFields.Items.Add(DataSet.Fields[I].DisplayName);
  cmbFields.ItemIndex := cmbFields.Items.IndexOf(FieldSearch.DisplayName);

  ShowModal;
end;

{***************** TZFindDialog implementation *****************}

{ Class constructor }
constructor TZFindDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSimile        := lsPartialStart;
  FCaseSensitive := False;
  FIsPattern     := False;
  FStartPos      := spAll;
  FDataSet       := nil;
  FDataField     := '';
end;

{ Process notification method }
procedure TZFindDialog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDataset ) and (Operation = opRemove) then
    FDataset := nil;
end;

{ Execute find dialog }
procedure TZFindDialog.Execute;
var
  FindDialog: TfrmFindDlg;
begin
  if not Assigned(FDataSet) or (FDataField = '') then Exit;

  Application.CreateForm(TfrmFindDlg, FindDialog);
  try
    FindDialog.DataSet    := FDataSet;
    FindDialog.FieldName  := FDataField;
    FindDialog.Simile     := Simile;
    FindDialog.CaseSensitive := CaseSensitive;
    FindDialog.IsPattern  := IsPattern;
    FindDialog.StartPos   := StartPos;
    FindDialog.IncrSearch := IncrSearch;
    FindDialog.SelectFields := SelectFields;
    FindDialog.Execute;
    Simile        := FindDialog.Simile;
    CaseSensitive := FindDialog.CaseSensitive;
    IsPattern     := FindDialog.IsPattern;
    StartPos      := FindDialog.StartPos;
    IncrSearch    := FindDialog.IncrSearch;
  finally
    FindDialog.Free;
  end;
end;

{ Get pattern }
function TfrmFindDlg.GetSample: string;
begin
  Result := edtSample.Text;
end;

{ Set pattern }
procedure TfrmFindDlg.SetSample(Value: string);
begin
  edtSample.Text := Value;
end;

{ Get similar }
function TfrmFindDlg.GetSimile: TZLocateSimile;
begin
  if cmbSimile.Enabled then
    Result := TZLocateSimile(cmbSimile.ItemIndex)
  else
    Result := lsExact;
end;

{ Set similar }
procedure TfrmFindDlg.SetSimile(Value: TZLocateSimile);
begin
  cmbSimile.ItemIndex := Ord(Value);
end;

{ Get case sensetive }
function TfrmFindDlg.GetCaseSensetive: Boolean;
begin
  Result := cbxCaseSensitive.Checked;
end;

{ Set case sensitive }
procedure TfrmFindDlg.SetCaseSensetive(Value: Boolean);
begin
  cbxCaseSensitive.Checked := Value;
end;

{ Get incremental search }
function TfrmFindDlg.GetIncrSearch: Boolean;
begin
  Result := cbxIncSearch.Checked;
end;

{ Set incremental search }
procedure TfrmFindDlg.SetIncrSearch(Value: Boolean);
begin
  cbxIncSearch.Checked := Value;
end;

{ Get search pattern }
function TfrmFindDlg.GetPattern: Boolean;
begin
  Result := cbxPattern.Checked;
end;

{ Set search pattern }
procedure TfrmFindDlg.SetPattern(Value: Boolean);
begin
  cbxPattern.Checked := Value;
end;

{ Get start position }
function TfrmFindDlg.GetStartPos: TZStartPos;
begin
  Result := TZStartPos(cmbStartPos.ItemIndex);
end;

{ Get select fields }
function TfrmFindDlg.GetSelectFields: Boolean;
begin
  Result := pnTop.Visible;
end;

{ Set select fields }
procedure TfrmFindDlg.SetSelectFields(Value: Boolean);
begin
  pnTop.Visible := Value;
  if Value then Height := 156
  else Height := 128;
end;

{ Set start position }
procedure TfrmFindDlg.SetStartPos(Value: TZStartPos);
begin
  cmbStartPos.ItemIndex := Ord(Value);
end;

{ On close button click }
procedure TfrmFindDlg.acCloseExecute(Sender: TObject);
begin
  Close;
end;

{ On form show event }
procedure TfrmFindDlg.FormShow(Sender: TObject);
begin
  edtSampleChange(edtSample);
end;

{ Find button click event }
procedure TfrmFindDlg.acFindExecute(Sender: TObject);
begin
  if not FindAdvanced(DataSet, FieldName, Sample,
    False, CaseSensitive, IsPattern, Simile, StartPos) then
  begin
    Hide;
    MessageDlg(SNoMoreRecords,mtInformation,[mbOk],0);
  end;
  Close;
end;

{ Find Next button click event }
procedure TfrmFindDlg.acFindNextExecute(Sender: TObject);
begin
  if not FindAdvanced(DataSet, FieldName, Sample,
    True, CaseSensitive, IsPattern, Simile, StartPos) then
    MessageDlg(SNoMoreRecords,mtInformation,[mbOk],0);
end;

{ On sample change event }
procedure TfrmFindDlg.edtSampleChange(Sender: TObject);
begin
  if edtSample.Text = '' then
  begin
    btnFindNext.Enabled := False;
    btnFind.Enabled     := False;
  end
  else
  begin
    btnFindNext.Enabled := True;
    btnFind.Enabled     := True;
  end;
  if cbxIncSearch.Checked and (edtSample.Text <> '') then
  begin
    FindAdvanced(DataSet, FieldName, Sample, False,
      CaseSensitive, IsPattern, lsPartialStart, spAll{StartPos});
  end;
end;

{ On form create event }
procedure TfrmFindDlg.FormCreate(Sender: TObject);
begin
  Caption              := SFindCaption;
  btnFind.Caption      := SFindItem;
  btnFindNext.Caption  := SFindNextItem;
  btnClose.Caption     := SCancelItem;
  lblSample.Caption    := SSampleItem;
  lblStartPos.Caption  := SStartItem;
  cmbStartPos.Items[0] := SBackwardItem;
  cmbStartPos.Items[1] := SForwardItem;
  cmbStartPos.Items[2] := SAllItem;
  lblSimile.Caption    := SSimilarItem;
  cmbSimile.Items[0]   := SAnyPartItem;
  cmbSimile.Items[1]   := SWholeFieldItem;
  cmbSimile.Items[2]   := SFromBeginItem;
  cbxCaseSensitive.Caption := SCaseCaption;
  cbxPattern.Caption   := SRegularCaption;
  cbxIncSearch.Caption := SIncSearch;

  cmbStartPos.ItemIndex := 2;
  cmbSimile.ItemIndex   := 1;
end;

end.


