unit heidicomp;

interface

uses
  Classes, ZDataset;

{$I const.inc}

type
  TDeferDataSet = class;
  TAsyncPostRunner = procedure(ds: TDeferDataSet) of object;

  TDeferDataSet = class(TZQuery)
  private
    callback: TAsyncPostRunner;
    kind: Integer;
 protected
    procedure InternalPost; override;
    procedure InternalRefresh; override;
 public
    constructor Create(AOwner: TComponent; PostCallback: TAsyncPostRunner); reintroduce;
    procedure ExecSQL; override;
    procedure DoAsync;
    procedure DoAsyncExecSql;
 end;


implementation

procedure TDeferDataSet.InternalPost;
begin
  kind := 1;
  if @callback = nil then DoAsync
  else callback(self);
end;

procedure TDeferDataSet.InternalRefresh;
begin
  kind := 3;
  if @callback = nil then DoAsync
  else callback(self);
end;

procedure TDeferDataSet.ExecSql;
begin
  kind := 2;
  if @callback = nil then DoAsync
  else callback(self);
end;

constructor TDeferDataSet.Create(AOwner: TComponent; PostCallback: TAsyncPostRunner);
begin
  callback := PostCallback;
  inherited Create(AOwner);
end;

procedure TDeferDataSet.DoAsync;
begin
  case kind of
    1: inherited InternalPost;
    2: inherited ExecSQL;
    3: inherited InternalRefresh;
  end;
end;

procedure TDeferDataSet.DoAsyncExecSql;
begin
  inherited ExecSql;
end;


end.
