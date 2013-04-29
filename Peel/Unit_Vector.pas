unit Unit_Vector;
interface


type
  TVector3f = record
    X,Y,Z: Single;
    nx, ny, nz: Single;
  end;

  TPoly3 = array [0..2] of Integer;

  function Vector3(X,Y,Z: Single): TVector3f;
  function Poly3(A,B,C: Integer): TPoly3;

implementation


function Vector3(X,Y,Z: Single): TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;


function Poly3(A,B,C: Integer): TPoly3;
begin
  Result[0] := A;
  Result[1] := B;
  Result[2] := C;
end;


end.
