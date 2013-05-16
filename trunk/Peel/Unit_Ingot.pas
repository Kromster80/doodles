unit Unit_Ingot;
interface
uses dglOpenGL, Unit_Vector;

type
  TIngot = class
  private
    fVerts: array of TVertice;
    fPolys: array of TPoly3;
    fVtxBuf: GLuint;
    fIndBuf: GLuint;

    fX: Single;
    fY: Single;
    fHead: Single;
    fPitch: Single;
    procedure Init; //Create new ingot
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(aFilename: string);
    procedure Move(X,Y: Single);
    procedure Rotate(X,Y: Single);
    procedure Render;
  end;


var
  fIngot: TIngot;


implementation
uses Unit_ColorCoder, Unit_Render;


{ TIngot }
constructor TIngot.Create;
begin
  inherited;

  glGenBuffers(1, @fVtxBuf);
  glGenBuffers(1, @fIndBuf);
end;


destructor TIngot.Destroy;
begin
  glDeleteBuffers(1, @fVtxBuf);
  glDeleteBuffers(1, @fIndBuf);

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
      fVerts[J] := Vertice(0, -0.5, 0, 0, -0.5, 0);
      Inc(J);
    end
    else
    if I = HeightDiv then
    begin
      fVerts[J] := Vertice(0, 0.5, 0, 0, 0.5, 0);
      Inc(J);
    end
    else
    for K := 0 to RadiusDiv - 1 do
    begin
      Ang := K / RadiusDiv * 2 * Pi;
      X := Sin(Ang) * Sin(((HeightDiv - I) / HeightDiv) / 2 * Pi) / 2;
      Y := Cos(((HeightDiv - I) / HeightDiv) / 2 * Pi) / 2;
      Z := Cos(Ang) * Sin(((HeightDiv - I) / HeightDiv) / 2 * Pi) / 2;
      fVerts[J] := Vertice(X,Y,Z, X,Y,Z);
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

  glBindBuffer(GL_ARRAY_BUFFER, fVtxBuf);
  glBufferData(GL_ARRAY_BUFFER, Length(fVerts) * SizeOf(TVertice), @fVerts[0].X, GL_STREAM_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndBuf);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(fPolys) * SizeOf(fPolys[0]), @fPolys[0,0], GL_STREAM_DRAW);
end;


procedure TIngot.LoadFromFile(aFilename: string);
begin
  //Use blank for now
  Init;
end;


procedure TIngot.Move(X, Y: Single);
begin
  fX := fX + X;
  fY := fY + Y;
end;


procedure TIngot.Rotate(X, Y: Single);
begin
  fHead := fHead + X;
  fPitch := fPitch + Y;
end;


procedure TIngot.Render;
  procedure SetRenderColor;
  begin
    if fRender.IsNormal then
    begin
      glColor3f(0.8, 1, 0.8);
    end
    else
    begin
      SetColorCode(ccIngot, 0);
    end;
  end;
begin
  glRotatef(fPitch, 1, 0, 0);
  glRotatef(fHead, 0, 1, 0);
  glTranslatef(fX, fY, 0);

  SetRenderColor;

  glBindBuffer(GL_ARRAY_BUFFER, fVtxBuf);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndBuf);

  //Setup vertex and UV layout and offsets
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertice), Pointer(0));

  if fRender.IsNormal then
  begin
    glEnableClientState(GL_NORMAL_ARRAY);
    glNormalPointer(GL_FLOAT, SizeOf(TVertice), Pointer(12));
  end;

  //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
  glDrawElements(GL_TRIANGLES, Length(fPolys) * Length(fPolys[0]), GL_UNSIGNED_INT, Pointer(0));

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
end;


end.
