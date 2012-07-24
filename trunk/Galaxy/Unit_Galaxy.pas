unit Unit_Galaxy;
interface
uses Classes, Controls, KromUtils, Math, Windows, SysUtils;

type
  TVector2f = record X,Y: Double; end;
  TColor4f = record R,G,B,A: Double; end;

  TParticle = record
    Loc: TVector2f;
    Vec: TVector2f;
    Temp: Double;
  end;


  procedure Galaxy_Create(X, Y: Integer);
  procedure Galaxy_Add(X, Y: Single);
  procedure Galaxy_Update;

var
  Particles: array of TParticle;


implementation


const
  ParticleRadius = 4;
  ParticleRadiusSq = ParticleRadius * ParticleRadius;
  GravityK = 100;
  ElastisityK = 250;


procedure Galaxy_Create(X, Y: Integer);
var
  I: Integer;
  RotationCoef: Double;
  PX, PY, RX, RY: Double;
begin
  RotationCoef := 2 + Random * 0.4 + 0.1;

  SetLength(Particles, 200);

  for I := 0 to High(Particles) do
  begin
    PX := (Random - 0.5);
    PY := (Random - 0.5);
    RX := (PY) * 12 * RotationCoef;
    RY := -(PX) * 12 * RotationCoef;

    Particles[I].Loc.X := PX * X;
    Particles[I].Loc.Y := PY * Y;

    Particles[I].Vec.X := (Random - 0.5) * 5 + 0.5 * RX;
    Particles[I].Vec.Y := (Random - 0.5) * 5 + 0.5 * RY;

    Particles[I].Temp := 2 + Random * 3.5;
  end;
end;


procedure Galaxy_Add(X, Y: Single);
var
  I: Integer;
begin
  SetLength(Particles, Length(Particles) + 1);

  I := High(Particles);

  Particles[I].Loc.X := X;
  Particles[I].Loc.Y := Y;

  Particles[I].Vec.X := (Random - 0.5);
  Particles[I].Vec.Y := (Random - 0.5);

  Particles[I].Temp := 2 + Random * 3.5;
end;


procedure Galaxy_Update;
var
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
  Delta := 0.03;

  for I := 0 to High(Particles) do
  begin
    //Move particle
    Particles[I].Loc.X := Particles[I].Loc.X + Particles[I].Vec.X * Delta;
    Particles[I].Loc.Y := Particles[I].Loc.Y + Particles[I].Vec.Y * Delta;
    Particles[I].Temp := Max(Particles[I].Temp - Particles[I].Temp * Delta * 0.0825, 0.5);

    for K := 0 to I - 1 do
    begin
      DX := Particles[I].Loc.X - Particles[K].Loc.X;
      DY := Particles[I].Loc.Y - Particles[K].Loc.Y;
      DistSqr := DX * DX + DY * DY;
      Dist := Sqrt(DistSqr);
      DistCub := DistSqr * Dist;

      ForceX := GravityK * DX / (DistCub + 0.1);
      ForceY := GravityK * DY / (DistCub + 0.1);

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

        Particles[I].Temp := Min(Particles[I].Temp + Coef * Delta * 0.07, 15.2);
        Particles[K].Temp := Min(Particles[K].Temp + Coef * Delta * 0.07, 15.2);

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
