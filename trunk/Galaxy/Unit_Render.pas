unit Unit_Render;
interface
uses Classes, Controls, dglOpenGL, OpenGL, KromOGLUtils, KromUtils, Math, Windows, SysUtils;


type
  TRender = class
  private
    h_DC: HDC;
    h_RC: HGLRC;
    fAreaX, fAreaY: Integer;
  public
    constructor Create(RenderFrame: HWND; InX,InY: Integer);
    destructor Destroy; override;
    procedure SetArea(InX,InY: Integer);
    procedure RenderResize;
    procedure Render;
  end;

    
var
  fRender: TRender;


implementation
uses Unit1, Unit_Galaxy;


constructor TRender.Create(RenderFrame: HWND; InX,InY: Integer);
begin
  inherited Create;

  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults;
  SetArea(InX,InY);

  glDisable(GL_LIGHTING);
  //glDisable(GL_BLEND);

  glEnable(GL_POINT_SMOOTH);

  BuildFont(h_DC, 12, FW_BOLD);
  SetupVSync(False);
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(0,0);
  wglDeleteContext(h_RC);
  inherited;
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
        glColor4f(Particles[I].Temp * 3, Particles[I].Temp * 1.5, Particles[I].Temp/2, 0.12 + Particles[I].Temp/5);
        glVertex2dv(@Particles[I].Loc);
      end;
    glEnd;

    glPointSize(3);
    glBegin(GL_POINTS);
      for I := 0 to High(Particles) do
      begin
        glColor4f(0.25 + Particles[I].Temp*2, 0.5 - Particles[I].Temp/2, 0.5 - Particles[I].Temp, 1 - Particles[I].Temp);
        glVertex2dv(@Particles[I].Loc);
      end;
    glEnd;

  glPopMatrix;

  {glColor3f(1,1,1);
  glRasterPos2f(10, fAreaY-20);
  glPrint('Click and drag left mouse button');
  }
  glFinish;
  SwapBuffers(h_DC);
end;


end.
