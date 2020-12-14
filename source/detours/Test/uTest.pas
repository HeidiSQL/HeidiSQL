unit uTest;

interface

uses
  DUnitX.TestFramework, DDetours;

type

  [TestFixture]
  TDDetours = class(TObject)
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    [Test]
    procedure Test2;
    [Test]
    procedure Test3;
    [Test]
    procedure Test4;
  end;

implementation

const
  Pi = 3.14;

const
  NewPi = 3.14159265359;

type
  TAdd = function(a, b: Integer): Integer;
  TSub = function(a, b: Integer): Integer;
  TGetPi = function(): Extended;

var
  TrampolineAdd: TAdd = nil;
  TrampolineSub: TSub = nil;
  TrampolineGetPi: TGetPi = nil;

  FInterceptSub: TIntercept<TSub, Integer>;

function Add(a, b: Integer): Integer;
begin
  Result := a + b;
end;

function Sub(a, b: Integer): Integer;
begin
  Result := a - b;
end;

function GetPi(): Extended;
begin
  Result := Pi;
end;

function InterceptAdd(a, b: Integer): Integer;
begin
  Result := TrampolineAdd(a, b);
end;

function InterceptSub(a, b: Integer): Integer;
var
  Param: Integer;
begin
  Param := Integer(GetTrampolineParam(TrampolineSub));
  Result := TrampolineSub(a, Param);
end;

function InterceptSub2(a, b: Integer): Integer;
begin
  Result := FInterceptSub.TrampoLine(a, FInterceptSub.Param);
end;

function InterceptGetPi(): Extended;
begin
  Result := NewPi;
end;

procedure TDDetours.Setup;
begin

end;

procedure TDDetours.TearDown;
begin
end;

procedure TDDetours.Test1;
var
  a, b, c: Integer;
begin
  a := 3;
  b := 2;
  c := a + b;
  TrampolineAdd := InterceptCreate(@Add, @InterceptAdd);
  Assert.AreEqual(Add(a, b), c);
  InterceptRemove(@TrampolineAdd);
  Assert.AreEqual(Add(a, b), c);
end;

procedure TDDetours.Test2;
var
  a, b, c: Integer;
  Param: Pointer;
begin
  a := 3;
  b := 2;
  c := a - b;
  Param := Pointer(0);
  TrampolineSub := InterceptCreate(@Sub, @InterceptSub, Param, DefaultInterceptOptions);
  Assert.AreEqual(Sub(a, b), a);
  InterceptRemove(@TrampolineSub);
  Assert.AreEqual(Sub(a, b), c);
end;

procedure TDDetours.Test3;
begin
  TrampolineAdd := InterceptCreate(@GetPi, @InterceptGetPi);
  Assert.AreEqual(GetPi(), NewPi);
  InterceptRemove(@TrampolineAdd);
  Assert.AreEqual(GetPi(), Pi);
end;

procedure TDDetours.Test4;
var
  a, b, c: Integer;
begin
  a := 3;
  b := 2;
  c := a - b;
  FInterceptSub := TIntercept<TSub, Integer>.Create(Sub, InterceptSub2, 0);
  Assert.AreEqual(Sub(a, b), a);
  FInterceptSub.Free();
  Assert.AreEqual(Sub(a, b), c);
end;

initialization

TDUnitX.RegisterTestFixture(TDDetours);

end.
