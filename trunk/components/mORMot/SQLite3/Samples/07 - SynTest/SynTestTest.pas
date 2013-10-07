unit SynTestTest;

interface

uses
  SynCommons;

function Adding(A,B: double): Double; overload;
function Adding(A,B: integer): integer; overload;

function Multiply(A,B: double): Double; overload;
function Multiply(A,B: integer): integer; overload;


type
  TTestNumbersAdding = class(TSynTestCase)
  published
    procedure TestIntegerAdd;
    procedure TestDoubleAdd;
  end;

  TTestNumbersMultiplying = class(TSynTestCase)
  published
    procedure TestIntegerMultiply;
    procedure TestDoubleMultiply;
  end;

  TTestSuit = class(TSynTestsLogged)
  published
    procedure MyTestSuit;
  end;


implementation

function Adding(A,B: double): Double; overload;
begin
  result := A+B;
end;

function Adding(A,B: integer): integer; overload;
begin
  result := A+B;
end;

function Multiply(A,B: double): Double; overload;
begin
  result := A*B;
end;

function Multiply(A,B: integer): integer; overload;
begin
  result := A*B;
end;


{ TTestSuit }

procedure TTestSuit.MyTestSuit;
begin
  AddCase([TTestNumbersAdding,TTestNumbersMultiplying]);
end;

{ TTestNumbersAdding }

procedure TTestNumbersAdding.TestDoubleAdd;
var A,B: double;
    i: integer;
begin
  for i := 1 to 1000 do
  begin
    A := Random;
    B := Random;
    Check(SameValue(A+B,Adding(A,B)));
  end;
end;

procedure TTestNumbersAdding.TestIntegerAdd;
var A,B: integer;
    i: integer;
begin
  for i := 1 to 1000 do
  begin
    A := Random(maxInt);
    B := Random(maxInt);
    Check(A+B=Adding(A,B));
  end;
end;

{ TTestNumbersMultiplying }

procedure TTestNumbersMultiplying.TestDoubleMultiply;
var A,B: double;
    i: integer;
begin
  for i := 1 to 1000 do
  begin
    A := Random;
    B := Random;
    Check(SameValue(A*B,Multiply(A,B)));
  end;
end;

procedure TTestNumbersMultiplying.TestIntegerMultiply;
var A,B: integer;
    i: integer;
begin
  for i := 1 to 1000 do
  begin
    A := Random(maxInt);
    B := Random(maxInt);
    Check(A*B=Multiply(A,B));
  end;
end;

end.
