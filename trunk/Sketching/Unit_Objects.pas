unit Unit_Objects;
interface
uses dglOpenGL, KromUtils, OpenGL, SysUtils, Windows;

type
  TKObject = class
    private
      fX:single;
      fY:single;
      fZ:single;
      fH:single;
      fP:single;
      fB:single;
      fColor:Vector3f;
      procedure BeforeDraw;
      procedure AfterDraw;
    public
      constructor Create(X,Y,Z:single);
      property X:single read fX write fX;
      property Y:single read fY write fY;
      property Z:single read fZ write fZ;
      property Heading:single write fH;
      property Pitch:single write fP;
      property Bank:single write fB;
      procedure Render; virtual;
    end;

  TKObjectCube = class (TKObject)
    private
      fWidth:single;
      fHeight:single;
      fDepth:single;
      fJitter:single;
      fVertices:array[0..7] of Vector3f;
      fIndexes:array[0..5,0..3] of byte;
    public
      constructor Create(X,Y,Z:single; aWidth,aHeight,aDepth,aJitter:single);
      procedure Render; override;
    end;

  TKObjectCylinder = class (TKObject)
    private
      fRadius:single;
      fHeight:single;
      fSections:byte;
      fJitter:single;
      fVertices:array of Vector3f;
    public
      constructor Create(X,Y,Z:single; aRadius,aHeight:single; aSections:byte; aJitter:single);
      procedure Render; override;
    end;


  TKObjectBall = class (TKObject)
    private
      fRadius:single;
      fSections:byte;
      fJitter:single;
      fVertices:array of Vector3f;
    public
      constructor Create(X,Y,Z:single; aRadius:single; aSections:byte; aJitter:single);
      procedure Render; override;
    end;


  TKObjectCollection = class
    private
      fBigCount:integer;
      fMedCount:integer;
      fTinCount:integer;
      fBig:array of TKObject;
      fMed:array of TKObject;
      fTin:array of TKObject;
    public
      constructor Create;
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

  fColor.X := random*0.3;
  fColor.Y := random*0.4;
  fColor.Z := random+0.2;
end;


procedure TKObject.BeforeDraw;
begin
  glPushMatrix;
  glTranslatef(fX, fY, fZ);
  glRotatef(fH, 0, 1, 0);
  glRotatef(fP, 1, 0, 0);
  glRotatef(fB, 0, 0, 1);
  glColor4f(fColor.X, fColor.Y, fColor.Z, 0.1);
end;


procedure TKObject.AfterDraw;
begin
  glPopMatrix;
end;


procedure TKObject.Render;
begin
  //Could render pivot point here
end;


constructor TKObjectCube.Create(X,Y,Z:single; aWidth,aHeight,aDepth,aJitter:single);
const UnitCube:array[0..7,0..2]of single =
                ((1,1,1),(-1,1,1),(1,1,-1),(-1,1,-1),(1,-1,1),(-1,-1,1),(1,-1,-1),(-1,-1,-1));
      UnitIndexes:array[0..5,0..3]of byte = ((0,1,3,2),(4,5,7,6),(0,1,5,4),(3,2,6,7),(1,3,7,5),(0,2,6,4));
var i,k:integer;
begin
  Inherited Create(X,Y,Z);

  fWidth := aWidth;
  fHeight := aHeight;
  fDepth := aDepth;
  fJitter := aJitter / 2;

  for i:=0 to 7 do
  begin
    fVertices[i].X := UnitCube[i,0] * fWidth / 2;
    fVertices[i].Y := UnitCube[i,1] * fHeight / 2;
    fVertices[i].Z := UnitCube[i,2] * fDepth / 2;
  end;

  for i:=0 to 5 do for k:=0 to 3 do
    fIndexes[i,k] := UnitIndexes[i,k];
end;


procedure TKObjectCube.Render;
var i,k:integer;
begin
  BeforeDraw;
  Inherited;

  for i:=0 to 5 do begin
    glBegin(GL_LINE_LOOP);
      for k:=0 to 3 do
        glVertex3f(
          fVertices[fIndexes[i,k]].X+(random-0.5)*fJitter,
          fVertices[fIndexes[i,k]].Y+(random-0.5)*fJitter,
          fVertices[fIndexes[i,k]].Z+(random-0.5)*fJitter);
    glEnd;
  end;

  AfterDraw;
end;


constructor TKObjectCylinder.Create(X,Y,Z:single; aRadius,aHeight:single; aSections:byte; aJitter:single);
var i,k:integer;
begin
  Inherited Create(X,Y,Z);

  fRadius := aRadius/2;
  fHeight := aHeight;
  fSections := aSections;
  fJitter := aJitter / 2;

  SetLength(fVertices, fSections*2);

  for i:=0 to 1 do
  for k:=0 to fSections-1 do
  begin
    fVertices[i*fSections+k].X := cos((k/fSections)*2*pi)*fRadius;
    fVertices[i*fSections+k].Y := fHeight/2 - fHeight*i;
    fVertices[i*fSections+k].Z := sin((k/fSections)*2*pi)*fRadius;
  end;
end;


procedure TKObjectCylinder.Render;
var i,k:integer;
begin
  BeforeDraw;
  Inherited;

  for i:=0 to 1 do begin
    glBegin(GL_LINE_LOOP);
      for k:=0 to fSections-1 do
        glVertex3f(
          fVertices[i*fSections+k].X+(random-0.5)*fJitter,
          fVertices[i*fSections+k].Y+(random-0.5)*fJitter,
          fVertices[i*fSections+k].Z+(random-0.5)*fJitter);
    glEnd;
  end;

  for k:=0 to (fSections-1) div 2 do begin
    glBegin(GL_LINES);
      for i:=0 to 1 do
        glVertex3f(
          fVertices[i*fSections+k*2].X+(random-0.5)*fJitter,
          fVertices[i*fSections+k*2].Y+(random-0.5)*fJitter,
          fVertices[i*fSections+k*2].Z+(random-0.5)*fJitter);
    glEnd;
  end;

  AfterDraw;
end;


constructor TKObjectBall.Create(X,Y,Z:single; aRadius:single; aSections:byte; aJitter:single);
var i,k:integer; r:single;
begin
  Inherited Create(X,Y,Z);

  fRadius := aRadius/2;
  fSections := aSections;
  fJitter := aJitter / 2;

  SetLength(fVertices, fSections*3);

  for i:=0 to 2 do
  for k:=0 to fSections-1 do
  begin
    r := (k/fSections)*2*pi;
    fVertices[i*fSections+k].X := cos(r)*fRadius * byte(i=0) + sin(r)*fRadius * byte(i=2);
    fVertices[i*fSections+k].Y := sin(r)*fRadius * byte(i=0) + cos(r)*fRadius * byte(i=1);
    fVertices[i*fSections+k].Z := cos(r)*fRadius * byte(i=2) + sin(r)*fRadius * byte(i=1);
  end;
end;


procedure TKObjectBall.Render;
var i,k:integer;
begin
  BeforeDraw;
  Inherited;

  for i:=0 to 2 do begin
    glBegin(GL_LINE_LOOP);
      for k:=0 to fSections-1 do
        glVertex3f(
          fVertices[i*fSections+k].X+(random-0.5)*fJitter,
          fVertices[i*fSections+k].Y+(random-0.5)*fJitter,
          fVertices[i*fSections+k].Z+(random-0.5)*fJitter);
    glEnd;
  end;

  AfterDraw;
end;


constructor TKObjectCollection.Create;
begin
 Inherited;
 Change;
end;


procedure TKObjectCollection.Change;
const BIG_SIZE = 70; MED_SIZE = 20; TIN_SIZE = 5;
var i:integer;
begin
  for i:=0 to fBigCount-1 do
    if fBig[i]<>nil then FreeAndNil(fBig[i]);

  for i:=0 to fMedCount-1 do
    if fMed[i]<>nil then FreeAndNil(fMed[i]);

  for i:=0 to fTinCount-1 do
    if fTin[i]<>nil then FreeAndNil(fTin[i]);

  fBigCount := random(2); //0..1
  fMedCount := random(3)+2; //2..5
  fTinCount := random(4)+2; //2..6

  SetLength(fBig, fBigCount);
  SetLength(fMed, fMedCount);
  SetLength(fTin, fTinCount);

  for i:=0 to fBigCount-1 do
  begin
    fBig[i] := TKObjectCube.Create(0, 0, 0, BIG_SIZE,BIG_SIZE,BIG_SIZE, 6); //Only big cube looks good
    fBig[i].Pitch := random(4)*90;
    fBig[i].Bank := random(4)*90;
  end;

  for i:=0 to fMedCount-1 do
  begin
    case Random(5) of
      0..2: fMed[i] := TKObjectCube.Create(0, 0, 0, MED_SIZE,MED_SIZE,MED_SIZE, 2);
      3: fMed[i] := TKObjectCylinder.Create(0, 0, 0, MED_SIZE+2,MED_SIZE,MED_SIZE, 2);
      4: fMed[i] := TKObjectBall.Create(0, 0, 0, MED_SIZE+2,MED_SIZE, 2);
    end;
    fMed[i].Pitch := random(4)*90;
    fMed[i].Bank := random(4)*90;
    fMed[i].X := round(Random*2-1)*MED_SIZE;
    fMed[i].Y := round(Random*2-1)*MED_SIZE;
    fMed[i].Z := round(Random*2-1)*MED_SIZE;
  end;

  for i:=0 to fTinCount-1 do
  begin
    case Random(5) of
      0..2: fTin[i] := TKObjectCube.Create(0, 0, 0, TIN_SIZE,TIN_SIZE,TIN_SIZE, 1);
      3: fTin[i] := TKObjectCylinder.Create(0, 0, 0, TIN_SIZE+1,TIN_SIZE,8, 1);
      4: fTin[i] := TKObjectBall.Create(0, 0, 0, TIN_SIZE+1,8, 1);
    end;
    fTin[i].X := round(Random*20-10)*TIN_SIZE;
    fTin[i].Y := round(Random*20-10)*TIN_SIZE;
    fTin[i].Z := round(Random*20-10)*TIN_SIZE;
  end;
end;


procedure TKObjectCollection.Render;
var i:integer;
begin
  for i:=0 to fBigCount-1 do fBig[i].Render;

  glRotatef((GetTickCount / 100), 0, 1, 0);
  for i:=0 to fTinCount-1 do fTin[i].Render;

  glRotatef((GetTickCount / 100), 0, 1, 0);
  for i:=0 to fMedCount-1 do fMed[i].Render;
end;


end.
