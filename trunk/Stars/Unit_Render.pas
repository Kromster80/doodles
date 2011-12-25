unit Unit_Render;
interface
uses Classes, Controls, dglOpenGL, OpenGL, KromOGLUtils, KromUtils, Math, Windows, SysUtils;

type
  TRender = class
    private
      h_DC: HDC;
      h_RC: HGLRC;
      fAreaX,fAreaY:integer;
      csStar, csPlanet, csSatellite: GLUint;
      procedure CompileCommonShapes;
    public
      constructor Create(RenderFrame:HWND; InX,InY:integer);
      destructor Destroy; override;
      procedure SetArea(InX,InY:integer);
      procedure RenderResize;
      procedure Render;
    end;

    
var
  fRender:TRender;


implementation
uses Unit1, Unit_Stars;


constructor TRender.Create(RenderFrame:HWND; InX,InY:integer);
begin
  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults;
  SetArea(InX,InY);

  glDisable(GL_LIGHTING);
  //glDisable(GL_BLEND);

  glEnable(GL_LINE_SMOOTH);

  glLineWidth(2);

  BuildFont(h_DC, 12, FW_BOLD);
  SetupVSync(True);

  CompileCommonShapes;
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(0,0);
  wglDeleteContext(h_RC);
  Inherited;
end;


procedure TRender.CompileCommonShapes;
const Sides = 16;
var i:integer;
begin
  csSatellite := glGenLists(1);
  glNewList(csSatellite, GL_COMPILE);
  glBegin(GL_POLYGON);
    for i:=0 to 5-1 do
      glVertex2f(sin(i/5*2*pi), cos(i/5*2*pi));
  glEnd;
  glEndList;

  csPlanet := glGenLists(1);
  glNewList(csPlanet, GL_COMPILE);
  glBegin(GL_POLYGON);
    for i:=0 to 16-1 do
      glVertex2f(sin(i/16*2*pi), cos(i/16*2*pi));
  glEnd;
  glEndList;

  csStar := glGenLists(1);
  glNewList(csStar, GL_COMPILE);
  glBegin(GL_LINES);
    for i:=0 to 16-1 do
      glVertex2f(sin(i/16*2*pi) * (1 + (i mod 2)/3), cos(i/16*2*pi) * (1 + (i mod 2)/3));
  glEnd;
  glBegin(GL_POLYGON);
    for i:=0 to 16-1 do
      glVertex2f(sin(i/16*2*pi) * (1 + (i mod 2)/3), cos(i/16*2*pi) * (1 + (i mod 2)/3));
  glEnd;
  glEndList;

end;


procedure TRender.SetArea(InX,InY:integer);
begin
  fAreaX := max(InX,1);
  fAreaY := max(InY,1);
  RenderResize;
end;


procedure TRender.RenderResize;
begin
  glViewport(0, 0, fAreaX, fAreaY);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluOrtho2D(0, fAreaX, fAreaY, 0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;


procedure TRender.Render;
var i:integer; P1, P2: TVector2f;
begin
  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  fStars.Update;

  glPushMatrix;
    glTranslatef(fAreaX/2, fAreaY/2, 0);

    //Links
    for i := 0 to fStars.Links.Count - 1 do
    begin
      P1 := fStars.Nodes[fStars.Links[i].ID1].Pos;
      P2 := fStars.Nodes[fStars.Links[i].ID2].Pos;

      glBegin(GL_LINES);
        glColor4fv(@fStars.Nodes[fStars.Links[i].ID1].Color);
        glVertex2fv(@P1);
        glColor4fv(@fStars.Nodes[fStars.Links[i].ID2].Color);
        glVertex2fv(@P2);
      glEnd;
    end;

    //Nodes
    for i := 0 to fStars.Nodes.Count - 1 do
    begin
      glPushMatrix;
        glTranslatef(fStars.Nodes[i].Pos.X, fStars.Nodes[i].Pos.Y, 0);
        glScalef(fStars.Nodes[i].Size, fStars.Nodes[i].Size, fStars.Nodes[i].Size);

        glColor4fv(@fStars.Nodes[i].Color);
        case fStars.Nodes[i].NodeType of
          stStar:       glCallList(csStar);
          stPlanet:     glCallList(csPlanet);
          stSatellite:  glCallList(csSatellite);
        end;

      glPopMatrix;
    end;

  glPopMatrix;

  glColor3f(0,0,0);
  glRasterPos2f(10, fAreaY-20);
  glPrint('Click and drag left mouse button');

  glFinish;
  SwapBuffers(h_DC);
end;


end.
