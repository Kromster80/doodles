unit Unit_Galaxy;
interface
uses Classes, Controls, KromUtils, Math, Windows, SysUtils;

type
  TVector2f = record X,Y: Double; end;

  TParticle = record
    Loc: TVector2f; //Position
    Vec: TVector2f; //Vector
    Temp: Single; //Temperature
  end;

  TGalaxy = class
  private
    fCount: Integer;
    fRadX, fRadY: Word;
  public
    Particles: array of TParticle;
    constructor Create(aCount: Integer; aRadius: Integer);
    procedure Add(X, Y: Single);
    property Count: Integer read fCount;
    procedure Resize(aX,aY: Integer);
    procedure Update;
  end;


var
  fGalaxy: TGalaxy;

  GravityK: Word = 100;
  ElastisityK: Word = 250;


implementation


const
  ParticleRadius = 3;
  ParticleRadiusSq = ParticleRadius * ParticleRadius;


{ TGalaxy }
constructor TGalaxy.Create(aCount: Integer; aRadius: Integer);
var
  I: Integer;
  RotationCoef: Double;
  PX, PY, RX, RY: Double;
begin
  inherited Create;

  fCount := aCount;
  fRadX := aRadius;
  fRadY := aRadius;
  SetLength(Particles, fCount);

  RotationCoef := 1.5 + Random * 0.4 + 0.1;

  for I := 0 to fCount - 1 do
  if I <= fCount*5/6 then
  begin
    PX := (Random - 0.5);
    PY := (Random - 0.5);
    RX := (PY) * 12 * RotationCoef;
    RY := -(PX) * 12 * RotationCoef;

    Particles[I].Loc.X := PX * aRadius;
    Particles[I].Loc.Y := PY * aRadius;

    Particles[I].Vec.X := (Random - 0.5) * 5 + 0.5 * RX;
    Particles[I].Vec.Y := (Random - 0.5) * 5 + 0.5 * RY;

    Particles[I].Temp := 0.1 + Random * 0.15;
  end
  else
  if I <= fCount*8/9 then
  begin
    PX := (Random - 0.5) * 0.75;
    PY := (Random - 0.5) * 0.75;
    RX := (PY) * 12 * RotationCoef;
    RY := -(PX) * 12 * RotationCoef;

    Particles[I].Loc.X := PX * aRadius;
    Particles[I].Loc.Y := PY * aRadius;

    Particles[I].Vec.X := (Random - 0.5) * 7 + 3 * RX;
    Particles[I].Vec.Y := (Random - 0.5) * 7 + 3 * RY;

    Particles[I].Temp := 0.2 + Random * 0.25;
  end
  else
  begin
    PX := (Random - 0.5) * 0.25;
    PY := (Random - 0.5) * 0.25;
    RX := (PY) * 12 * RotationCoef;
    RY := -(PX) * 12 * RotationCoef;

    Particles[I].Loc.X := PX * aRadius;
    Particles[I].Loc.Y := PY * aRadius;

    Particles[I].Vec.X := (Random - 0.5) * 10 + 5 * RX;
    Particles[I].Vec.Y := (Random - 0.5) * 10 + 5 * RY;

    Particles[I].Temp := 0.2 + Random * 0.35;
  end;
end;


procedure TGalaxy.Add(X, Y: Single);
begin
  if Length(Particles) <= fCount then
    SetLength(Particles, fCount + 32);

  Particles[fCount].Loc.X := X;
  Particles[fCount].Loc.Y := Y;

  Particles[fCount].Vec.X := (Random - 0.5);
  Particles[fCount].Vec.Y := (Random - 0.5);

  Particles[fCount].Temp := 0.2 + Random * 0.35;

  Inc(fCount);
end;


procedure TGalaxy.Resize(aX,aY: Integer);
begin
  //Update bounds, particles will be clipped in Update
  fRadX := max(aX div 2, 1);
  fRadY := max(aY div 2, 1);
end;


procedure TGalaxy.Update;
var
  RotationCoef: Double;
  PX, PY, RX, RY: Double;

  Delta: Double;
  I, K: Integer;
  DX, DY: Double;
  DistSqr: Double;
  Dist: Double;
  DistCub: Double;
  ForceX, ForceY: Double;
  Coef: Double;
  MidVelX, MidVelY: Double;
begin
  Delta := 0.035;

  for I := 0 to fCount - 1 do
  begin
    //Move particle
    Particles[I].Loc.X := Particles[I].Loc.X + Particles[I].Vec.X * Delta;
    Particles[I].Loc.Y := Particles[I].Loc.Y + Particles[I].Vec.Y * Delta;

    //When particles fly away we return them back as hot
    if (Abs(Particles[I].Loc.X) > fRadX) or (Abs(Particles[I].Loc.Y) > fRadY) then
    begin
      RotationCoef := 1.5 + Random * 0.4 + 0.1;

      PX := (Random - 0.5);
      PY := (Random - 0.5);
      RX := (PY) * 12 * RotationCoef;
      RY := -(PX) * 12 * RotationCoef;

      Particles[I].Loc.X := PX * 400;
      Particles[I].Loc.Y := PY * 400;

      Particles[I].Vec.X := (Random - 0.5) * 6 + 3 * RX;
      Particles[I].Vec.Y := (Random - 0.5) * 6 + 3 * RY;

      Particles[I].Temp := 0.75 + Random / 2;
    end;


    //Cooling
    Particles[I].Temp := Max(Particles[I].Temp - Particles[I].Temp * Delta * 0.0825, 0.01);

    for K := 0 to I - 1 do
    begin
      DX := Particles[I].Loc.X - Particles[K].Loc.X;
      DY := Particles[I].Loc.Y - Particles[K].Loc.Y;
      DistSqr := DX * DX + DY * DY;
      Dist := Sqrt(DistSqr);
      DistCub := DistSqr * Dist; //Dist^3

      ForceX := GravityK * DX / (DistCub + 0.1);
      ForceY := GravityK * DY / (DistCub + 0.1);

      //If particles are too close - push them apart
      if (Dist < ParticleRadius) then
      begin
        Coef := (ParticleRadius - Dist) / ParticleRadius;
        ForceX := 0;
        ForceY := 0;

        ForceX := ForceX - ElastisityK * (DX / (Dist + 0.001)) * Sqr(ParticleRadius - Dist) / ParticleRadius;
        ForceY := ForceY - ElastisityK * (DY / (Dist + 0.001)) * Sqr(ParticleRadius - Dist) / ParticleRadius;

        MidVelX := (Particles[I].Vec.X + Particles[K].Vec.X) * 0.5;
        MidVelY := (Particles[I].Vec.Y + Particles[K].Vec.Y) * 0.5;

        Coef := 1.0 - (1.0 - Coef) * 0.015;

        Particles[I].Temp := Min(Particles[I].Temp + Coef * Delta * 0.007, 1);
        Particles[K].Temp := Min(Particles[K].Temp + Coef * Delta * 0.007, 1);

        Particles[I].Vec.X := Particles[I].Vec.X * Coef + MidVelX * (1 - Coef);
        Particles[I].Vec.Y := Particles[I].Vec.Y * Coef + MidVelY * (1 - Coef);
        Particles[K].Vec.X := Particles[K].Vec.X * Coef + MidVelX * (1 - Coef);
        Particles[K].Vec.Y := Particles[K].Vec.Y * Coef + MidVelY * (1 - Coef);
      end;

      Particles[I].Vec.X := Particles[I].Vec.X - ForceX * Delta;
      Particles[I].Vec.Y := Particles[I].Vec.Y - ForceY * Delta;
      Particles[K].Vec.X := Particles[K].Vec.X + ForceX * Delta;
      Particles[K].Vec.Y := Particles[K].Vec.Y + ForceY * Delta;
    end;
  end;
end;


end.
