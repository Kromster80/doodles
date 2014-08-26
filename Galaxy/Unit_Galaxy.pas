unit Unit_Galaxy;
interface
uses
  Classes, Controls, KromUtils, Math, Windows, SysUtils;

type
  // Use Double precision for smooth calcultions
  TVector2f = record X,Y: Double; end;

  TParticle = record
    Loc: TVector2f; // Position
    Vec: TVector2f; // Vector
    Temp: Single;   // Temperature
  end;
  PParticle = ^TParticle;

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
  gGalaxy: TGalaxy;


implementation


const
  GravityK = 100;
  ElastisityK = 250;
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
  // Update bounds, particles will be clipped in Update
  fRadX := Max(aX div 2, 1);
  fRadY := Max(aY div 2, 1);
end;


procedure TGalaxy.Update;
var
  Pt, Pk: PParticle;
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
    Pt := @Particles[I];

    //Move particle
    Pt.Loc.X := Pt.Loc.X + Pt.Vec.X * Delta;
    Pt.Loc.Y := Pt.Loc.Y + Pt.Vec.Y * Delta;

    //When particles fly away we return them back as hot
    if (Abs(Pt.Loc.X) > fRadX) or (Abs(Pt.Loc.Y) > fRadY) then
    begin
      RotationCoef := 1.5 + Random * 0.4 + 0.1;

      PX := (Random - 0.5);
      PY := (Random - 0.5);
      RX := (PY) * 12 * RotationCoef;
      RY := -(PX) * 12 * RotationCoef;

      Pt.Loc.X := PX * 400;
      Pt.Loc.Y := PY * 400;

      Pt.Vec.X := (Random - 0.5) * 6 + 3 * RX;
      Pt.Vec.Y := (Random - 0.5) * 6 + 3 * RY;

      Pt.Temp := 0.75 + Random / 2;
    end;

    //Cooling
    Pt.Temp := Max(Pt.Temp - Pt.Temp * Delta * 0.0825, 0.01);

    // Interactions
    for K := 0 to I - 1 do
    begin
      Pk := @Particles[K];
      DX := Pt.Loc.X - Pk.Loc.X;
      DY := Pt.Loc.Y - Pk.Loc.Y;
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

        MidVelX := (Pt.Vec.X + Pk.Vec.X) * 0.5;
        MidVelY := (Pt.Vec.Y + Pk.Vec.Y) * 0.5;

        Coef := 1.0 - (1.0 - Coef) * 0.015;

        Pt.Temp := Min(Pt.Temp + Coef * Delta * 0.007, 1);
        Pk.Temp := Min(Pk.Temp + Coef * Delta * 0.007, 1);

        Pt.Vec.X := Pt.Vec.X * Coef + MidVelX * (1 - Coef);
        Pt.Vec.Y := Pt.Vec.Y * Coef + MidVelY * (1 - Coef);
        Pk.Vec.X := Pk.Vec.X * Coef + MidVelX * (1 - Coef);
        Pk.Vec.Y := Pk.Vec.Y * Coef + MidVelY * (1 - Coef);
      end;

      Pt.Vec.X := Pt.Vec.X - ForceX * Delta;
      Pt.Vec.Y := Pt.Vec.Y - ForceY * Delta;
      Pk.Vec.X := Pk.Vec.X + ForceX * Delta;
      Pk.Vec.Y := Pk.Vec.Y + ForceY * Delta;
    end;
  end;
end;


end.
