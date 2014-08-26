unit Unit_Render;
interface
uses
  Classes, Controls, dglOpenGL, OpenGL, KromOGLUtils, KromUtils, Math, Windows, SysUtils;


type
  TRender = class
  private
    h_DC: HDC;
    h_RC: HGLRC;
    fWidth: Integer;
    fHeight: Integer;
  public
    constructor Create(RenderFrame: HWND; aWidth, aHeight: Integer);
    destructor Destroy; override;
    procedure Resize(aWidth,aHeight: Integer);
    procedure RenderResize;
    procedure Render;
  end;


var
  gRender: TRender;


implementation
uses
  Unit_Galaxy;


constructor TRender.Create(RenderFrame: HWND; aWidth, aHeight: Integer);
begin
  inherited Create;

  SetRenderFrame(RenderFrame, h_DC, h_RC);

  glClearColor(0, 0, 0, 0);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_LIGHTING);
  glEnable(GL_POINT_SMOOTH);

  Resize(aWidth, aHeight);

  BuildFont(h_DC, 12, FW_BOLD);
  SetupVSync(False);
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(0,0);
  wglDeleteContext(h_RC);

  inherited;
end;


procedure TRender.Resize(aWidth,aHeight: Integer);
begin
  fWidth := Max(aWidth, 1);
  fHeight := Max(aHeight, 1);
  RenderResize;
end;


procedure TRender.RenderResize;
begin
  glViewport(0, 0, fWidth, fHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(-fWidth/2, fWidth/2, fHeight/2, -fHeight/2, 0, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;


procedure TRender.Render;
var
  I: Integer;
  Pt: PParticle;
begin
  glClear(GL_COLOR_BUFFER_BIT);
  glLoadIdentity;

    //Temperature glow
    glPointSize(10);
    glBegin(GL_POINTS);
      with gGalaxy do
      for I := 0 to Count - 1 do
      begin
        Pt := @Particles[I];
        glColor4f(Pt.Temp * 3, Pt.Temp * 1.5, Pt.Temp/2, 0.12 + Pt.Temp/5);
        glVertex2dv(@Pt.Loc);
      end;
    glEnd;

    //Particle
    glPointSize(3);
    glBegin(GL_POINTS);
      with gGalaxy do
      for I := 0 to Count - 1 do
      begin
        Pt := @Particles[I];
        glColor4f(0.05 + Pt.Temp*2, 0.35 - Pt.Temp/2, 0.5 - Pt.Temp, 1 - Pt.Temp);
        glVertex2dv(@Pt.Loc);
      end;
    glEnd;

  {glColor3f(1,1,1);
  glRasterPos2f(10, fAreaY-20);
  glPrint('Click and drag left mouse button');
  }
  //glFinish;
  SwapBuffers(h_DC);
end;


end.
