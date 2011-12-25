unit Unit_Objects;
interface
uses dglOpenGL, KromUtils, OpenGL, SysUtils, Windows, Math;

type
  TKObject = class
  private
    fX:single;
    fY:single;
    fZ:single;
    procedure BeforeDraw;
    procedure AfterDraw;
  public
    constructor Create(X,Y,Z:single);
    property X:single read fX write fX;
    property Y:single read fY write fY;
    property Z:single read fZ write fZ;
    procedure Render; virtual;
  end;


  TKObjectStrip = class (TKObject)
  private
    fLength:single;
    fWidth:single;
    fCount:integer;
    fStep:single;
    fMode:byte;
    fVertices:array of Vector3f;
    fColors:array of Vector3f;
  public
    constructor Create(X,Y,Z:single; aLength,aWidth,aStep:single; aMode:byte);
    procedure Recalc;
    procedure Render; override;
  end;

  TKObjectXYZ = class (TKObject)
  private
    fSize:single;
  public
    constructor Create(aSize:single);
    procedure Render; override;
  end;


  TKObjectCollection = class
  private
    LOD:integer;
    fAxis:TKObjectXYZ;
    fGraph1:TKObjectStrip;
    fGraph2:TKObjectStrip;
  public
    constructor Create;
    procedure KeyDown(Key: Word);
    procedure Change;
    procedure Render;
  end;

  var
    fObjectCollection:TKObjectCollection;


implementation


constructor TKObject.Create(X,Y,Z:single);
begin       
  fX := X;
  fY := Y;
  fZ := Z;
end;


procedure TKObject.BeforeDraw;
begin
  glPushMatrix;
  glTranslatef(fX, fY, fZ);
end;


procedure TKObject.AfterDraw;
begin
  glPopMatrix;
end;


procedure TKObject.Render;
begin
  //Could render pivot point here
end;


constructor TKObjectStrip.Create(X,Y,Z:single; aLength,aWidth,aStep:single; aMode:byte);
begin
  Inherited Create(X,Y,Z);

  fLength:= aLength;
  fWidth := aWidth;
  fStep := aStep;

  fMode := aMode;
  Recalc;
end;


procedure TKObjectStrip.Recalc;
var i:integer;
begin
  fCount := round(fLength/fStep) + 1;
  SetLength(fVertices, fCount);
  SetLength(fColors, fCount);

  for i:=0 to fCount-1 do
  begin
    fVertices[i].X := i*fStep - fLength/2;
    case fMode of
      0: fVertices[i].Y := sin((i*fStep*2)*pi/2)+(random/10)-0.05;
      1: fVertices[i].Y := cos((i*fStep*3)*pi/2)+(random/10)-0.05;
    end;
    fColors[i].X := 2-(fVertices[i].Y+1); //Red
    fColors[i].Y := fVertices[i].Y+1; //Green
    fColors[i].Z := 1-abs(fVertices[i].Y); //Blue
  end;
end;


procedure TKObjectStrip.Render;
var i:integer;
begin
  BeforeDraw;
  Inherited;

  glBegin(GL_QUAD_STRIP);
    for i:=0 to fCount-1 do
    begin
      glColor3fv(@fColors[i]);
      glVertex3f(fVertices[i].X, fVertices[i].Y, fVertices[i].Z-fWidth/2);
      glVertex3f(fVertices[i].X, fVertices[i].Y, fVertices[i].Z+fWidth/2);
    end;
  glEnd;

  AfterDraw;
end;


constructor TKObjectXYZ.Create(aSize:single);
begin
  Inherited Create(0,0,0);

  fSize := aSize;
end;


procedure TKObjectXYZ.Render;
begin
  glPushMatrix;
    glScale(fSize,fSize,fSize);
    glBegin(GL_LINES);
      glColor3f(1, 0, 0);
      glVertex3f(-1, 0, 0); glVertex3f(1, 0, 0);
      glVertex3f(0.95, 0, -0.05); glVertex3f(0.95, 0, 0.05);
      glColor3f(0, 1, 0);
      glVertex3f(0, -1, 0); glVertex3f(0, 1, 0);
      glVertex3f(0, 0.95, -0.05); glVertex3f(0, 0.95, 0.05);
      glColor3f(0, 0, 1);
      glVertex3f(0, 0, -1); glVertex3f(0, 0, 1);
      glVertex3f(-0.05, 0, 0.95); glVertex3f(0.05, 0, 0.95);
    glEnd;
  glPopMatrix;
end;


constructor TKObjectCollection.Create;
begin
 Inherited;
 LOD := 20;
 Change;
end;


procedure TKObjectCollection.KeyDown(Key: Word);
begin
  if Key = VK_UP then inc(LOD);
  if Key = VK_DOWN then dec(LOD);
  LOD := EnsureRange(LOD, 2, 50);
  Change;
end;


procedure TKObjectCollection.Change;
begin
  fAxis := TKObjectXYZ.Create(5);
  fGraph1 := TKObjectStrip.Create(0,0,-0.5, 10, 0.25, 1/LOD, 0);
  fGraph2 := TKObjectStrip.Create(0,0, 0.5, 10, 0.25, 1/LOD, 1);
end;


procedure TKObjectCollection.Render;
begin
  fAxis.Render;
  fGraph1.Render;
  fGraph2.Render;
end;


end.
