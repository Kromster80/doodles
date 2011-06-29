unit Unit_Aiming;
interface
uses Classes, Controls, KromUtils, Math, Windows, SysUtils;

type
  TPointF = record
    X,Y:single;
  end;

  TAiming = class
  private
    fArrowSpeed:single;
    fTargetPosition:TPointF;
    fTargetVector:TPointF;
    fPerformance:string;
    procedure CalculateTarget;
  public
    constructor Create;
    property ArrowSpeed:single read fArrowSpeed write fArrowSpeed;
    property TargetPosition:TPointF read fTargetPosition write fTargetPosition;
    property TargetVector:TPointF read fTargetVector write fTargetVector;
    property Performance:string read fPerformance;
    function GetHit:boolean;
    function GetTarget(aTime:single):TPointF;
    function GetTime:single;
    procedure UpdatePerformance;
  end;


  function PointF(X,Y:single):TPointF;

var
  fAiming:TAiming;


implementation
uses Unit1;


function PointF(X,Y:single):TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;


{ TAiming }
constructor TAiming.Create;
begin
  fArrowSpeed := 50;
  fTargetPosition.X := 155;
  fTargetPosition.Y := -120;
  fTargetVector.X := -60;
  fTargetVector.Y := -15;
  fPerformance := '0';
end;


//Check if target can be hit
function TAiming.GetHit: boolean;
begin
  Result := GetTime > 0;
end;


procedure TAiming.CalculateTarget;
begin
  GetTarget(GetTime);
end;


procedure TAiming.UpdatePerformance;
begin
  fPerformance := inttostr(PerformanceCounter(CalculateTarget));
end;


function TAiming.GetTarget(aTime:single): TPointF;
begin
  if aTime > 0 then
  begin
    Result.X := TargetPosition.X + TargetVector.X * aTime;
    Result.Y := TargetPosition.Y + TargetVector.Y * aTime;
  end else
  begin
    Result.X := 0;
    Result.Y := 0;
  end;
end;


function TAiming.GetTime: single;
var A,B,C,D:single; Time1,Time2:single;
begin
{ Target.X := TargetPosition.X + TargetVector.X * Time;
  Target.Y := TargetPosition.Y + TargetVector.Y * Time;

  FlightDistance := ArrowSpeed * Time;

  sqr(Target.X)+sqr(Target.Y) = sqr(FlightDistance);

  sqr(TargetPosition.X + TargetVector.X * Time) + sqr(TargetPosition.Y + TargetVector.Y * Time) = sqr(ArrowSpeed * Time)

  sqr(TargetPosition.X) + 2 * Time * TargetPosition.X * TargetVector.X + sqr(Time) * sqr(TargetVector.X) +
  sqr(TargetPosition.Y) + 2 * Time * TargetPosition.Y * TargetVector.Y + sqr(Time) * sqr(TargetVector.Y) =
  sqr(ArrowSpeed) * sqr(Time)

  sqr(Time) * (sqr(TargetVector.X) + sqr(TargetVector.Y) - sqr(ArrowSpeed)) +
  2 * Time * (TargetPosition.X * TargetVector.X + TargetPosition.Y * TargetVector.Y) +
  sqr(TargetPosition.X) + sqr(TargetPosition.Y) = 0

  //Lets try to solve this quadratic equation
  //ATT + BT + C = 0
  //by using formulae X = (-B +- sqrt(B*B - 4*A*C)) / 2*A
  A = sqr(TargetVector.X) + sqr(TargetVector.Y) - sqr(ArrowSpeed)
  B = 2 * (TargetPosition.X * TargetVector.X + TargetPosition.Y * TargetVector.Y)
  C = sqr(TargetPosition.X) + sqr(TargetPosition.Y) }

  A := sqr(TargetVector.X) + sqr(TargetVector.Y) - sqr(fArrowSpeed);
  B := 2 * (TargetPosition.X * TargetVector.X + TargetPosition.Y * TargetVector.Y);
  C := sqr(TargetPosition.X) + sqr(TargetPosition.Y);

  D := sqr(B) - 4 * A * C;

  if (D >= 0) and (A <> 0) then
  begin
    Time1 := (-B + sqrt(D)) / (2 * A);
    Time2 := (-B - sqrt(D)) / (2 * A);

    //Choose smallest positive time
    if (Time1 > 0) and (Time2 > 0) then
      Result := min(Time1, Time2)
    else
    if (Time1 < 0) and (Time2 < 0) then
      Result := 0
    else
      Result := max(Time1, Time2);
  end
  else
    Result := 0;
end;




end.
