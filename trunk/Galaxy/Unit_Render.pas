unit Unit_Render;
interface
uses Classes, Controls, dglOpenGL, OpenGL, KromOGLUtils, KromUtils, Math, Windows, SysUtils;

type
  TRender = class
    private
      h_DC: HDC;
      h_RC: HGLRC;
      fAreaX,fAreaY:integer;
      csStar, csDisc, csSatellite: GLUint;
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
uses Unit1, Unit_Galaxy;


constructor TRender.Create(RenderFrame:HWND; InX,InY:integer);
begin
  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults;
  SetArea(InX,InY);

  glDisable(GL_LIGHTING);
  //glDisable(GL_BLEND);

  glEnable(GL_POINT_SMOOTH);

  glPointSize(2);

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
var I: Integer;
begin
  csDisc := glGenLists(1);
  glNewList(csDisc, GL_COMPILE);
  glBegin(GL_POLYGON);
    for I:=0 to 16-1 do
      glVertex2f(sin(I/16*2*pi), cos(I/16*2*pi));
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
var I: Integer;
begin
  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  Galaxy_Update;

  glPushMatrix;
    glTranslatef(fAreaX/2, fAreaY/2, 0);

    glPointSize(10);
    glBegin(GL_POINTS);
      for I := 0 to High(Particles) do
      begin
        glColor4f(Particles[I].Temp / 3, Particles[I].Temp / 6, Particles[I].Temp / 10, 0.12 + Particles[I].Temp/50);
        glVertex2dv(@Particles[I].Loc);
      end;
    glEnd;


    glPointSize(2);
    glBegin(GL_POINTS);
      for I := 0 to High(Particles) do
      begin
        glColor3f(0.5 + Particles[I].Temp / 4, 0.5, 0.5);
        glVertex2dv(@Particles[I].Loc);
      end;
    glEnd;


  glPopMatrix;

  glColor3f(0,0,0);
  glRasterPos2f(10, fAreaY-20);
  glPrint('Click and drag left mouse button');

  glFinish;
  SwapBuffers(h_DC);
end;


end.
