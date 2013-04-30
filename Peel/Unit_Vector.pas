unit Unit_Vector;
interface


type
  TVector2f = record
    case Integer of
      0: (X, Y: Single);
      1: (U, V: Single);
    end;

    TVector2i = record X,Y: Integer; end;
    TVector2d = record X,Y: Double; end;
    TVector3f = record X,Y,Z: Single; end;
    TVector3d = record X,Y,Z: Double; end;
    TVector3i = record X,Y,Z: Integer; end;
    TVector4f = record X,Y,Z,W: Single; end;
    TVector4d = record X,Y,Z,W: Double; end;

    PVector3f = ^TVector3f;

  TVertice = record
    X,Y,Z: Single;
    nx, ny, nz: Single;
  end;

  TPoly3 = array [0..2] of Integer;

  function Vector2i(X, Y: Integer): TVector2i;
  function Vector3(X, Y, Z: Single): TVector3f;
  function Vector4d(X, Y, Z, W: Single): TVector4d;
  function Vertice(X, Y, Z, nx, ny, nz: Single): TVertice;
  function Poly3(A,B,C: Integer): TPoly3;

implementation


function Vector2i(X, Y: Integer): TVector2i;
begin
  Result.X := X;
  Result.Y := Y;
end;


function Vector3(X, Y, Z: Single): TVector3f;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;


function Vector4d(X, Y, Z, W: Single): TVector4d;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.W := W;
end;


function Vertice(X, Y, Z, nx, ny, nz: Single): TVertice;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.nx := nx;
  Result.ny := ny;
  Result.nz := nz;
end;


function Poly3(A,B,C: Integer): TPoly3;
begin
  Result[0] := A;
  Result[1] := B;
  Result[2] := C;
end;


end.
