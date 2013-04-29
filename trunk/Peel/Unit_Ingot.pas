unit Unit_Ingot;
interface
uses dglOpenGL, Unit_Vector;

type
  TIngot = class
  private
    fVerts: array of TVector3f;
    fNorms: array of TVector3f;
    fPolys: array of TPoly3;
    fVtxShd: GLUint;
    fIndShd: GLUint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init; //Create new ingot
    procedure Render;
  end;


implementation


{ TIngot }
constructor TIngot.Create;
begin
  inherited;

  glGenBuffers(1, @fVtxShd);
  glGenBuffers(1, @fIndShd);
end;


destructor TIngot.Destroy;
begin
  glDeleteBuffers(1, @fVtxShd);
  glDeleteBuffers(1, @fIndShd);

  inherited;
end;


procedure TIngot.Init;
const
  HeightDiv = 12;
  RadiusDiv = 16;
var
  I,K,J: Integer;
  Ang,X,Y,Z: Single;
  Off: Integer;
begin
  SetLength(fVerts, (HeightDiv * 2 - 1) * RadiusDiv + 2);
  J := 0;
  for I := -HeightDiv to HeightDiv do
  begin
    if I = -HeightDiv then
    begin
      fVerts[J] := Vector3(0, -0.5, 0);
      Inc(J);
    end
    else
    if I = HeightDiv then
    begin
      fVerts[J] := Vector3(0, 0.5, 0);
      Inc(J);
    end
    else
    for K := 0 to RadiusDiv - 1 do
    begin
      Ang := K / RadiusDiv * 2 * Pi;
      X := Sin(Ang) * Sin(((HeightDiv - I) / HeightDiv) / 2 * Pi) / 2;
      Y := Cos(((HeightDiv - I) / HeightDiv) / 2 * Pi) / 2;
      Z := Cos(Ang) * Sin(((HeightDiv - I) / HeightDiv) / 2 * Pi) / 2;
      fVerts[J] := Vector3(X,Y,Z);
      fVerts[J].nx := X;
      fVerts[J].ny := Y;
      fVerts[J].nz := Z;
      Inc(J);
    end;
  end;

  SetLength(fPolys, (HeightDiv * 2) * RadiusDiv * 2 * 3);

  //Caps
  J := 0;
  for K := 0 to RadiusDiv - 1 do
  begin
    fPolys[J] := Poly3(0, 1 + K, 1 + (K + 1) mod RadiusDiv);
    Inc(J);
  end;
  Off := 1 + (HeightDiv * 2 - 2) * RadiusDiv;
  for K := 0 to RadiusDiv - 1 do
  begin
    fPolys[J] := Poly3(Off + RadiusDiv, Off + K, Off + (K + 1) mod RadiusDiv);
    Inc(J);
  end;

  for I := -HeightDiv to HeightDiv-1 do
  begin
    Off := 1 + (I + HeightDiv) * RadiusDiv;
    for K := 0 to RadiusDiv - 1 do
    begin
      fPolys[J] := Poly3(Off + K, Off + RadiusDiv + K, Off + (K + 1) mod RadiusDiv);
      Inc(J);
      fPolys[J] := Poly3(Off + RadiusDiv + K, Off + (K + 1) mod RadiusDiv, Off + RadiusDiv + (K + 1) mod RadiusDiv);
      Inc(J);
    end;
  end;

  glBindBuffer(GL_ARRAY_BUFFER, fVtxShd);
  glBufferData(GL_ARRAY_BUFFER, Length(fVerts) * SizeOf(TVector3f), @fVerts[0].X, GL_STREAM_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndShd);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(fPolys) * SizeOf(fPolys[0]), @fPolys[0,0], GL_STREAM_DRAW);
end;


procedure TIngot.Render;
begin
  //Setup vertex and UV layout and offsets
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVector3f), Pointer(0));

  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT, SizeOf(TVector3f), Pointer(12));

  //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
  glDrawElements(GL_TRIANGLES, Length(fPolys) * Length(fPolys[0]), GL_UNSIGNED_INT, Pointer(0));

  glDisableClientState(GL_VERTEX_ARRAY);
  //glDisableClientState(GL_TEXTURE_COORD_ARRAY);
end;


end.
