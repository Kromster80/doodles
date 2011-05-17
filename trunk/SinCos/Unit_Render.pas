unit Unit_Render;
interface
uses Classes, Controls, dglOpenGL, OpenGL, KromOGLUtils, KromUtils, Math, Windows;


type
  TRender = class
  private
    h_DC: HDC;
    h_RC: HGLRC;
    xOld,yOld:integer;
    xRot,yRot:single;
    fAreaX,fAreaY:integer;
  public
    constructor Create(DummyFrame,RenderFrame:HWND; InX,InY:integer);
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);

    procedure SetArea(InX,InY:integer);
    procedure RenderResize;
    procedure Render;
  end;


{const
  LightPos:Array[0..3] of GLfloat = (-10,-30,-30,0);
  LightSpec:Array[0..3] of GLfloat = (0.7,0.7,0.7,0);
  LightDiff:Array[0..3] of GLfloat = (1,0,1,1);}


var
  fRender:TRender;


implementation
uses Unit_Objects;


constructor TRender.Create(DummyFrame,RenderFrame:HWND; InX,InY:integer);
begin
  Inherited Create;

  SetRenderFrameAA(DummyFrame, RenderFrame, 16, h_DC, h_RC);

  SetRenderDefaults;
  SetArea(InX,InY);

  yRot := 30;

  glEnable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);

  {glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpec);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);}

  SetupVSync(true);
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);
  Inherited;
end;


procedure TRender.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then exit;
  xOld := X; yOld := Y;
end;


procedure TRender.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    xRot := xRot - (X-xOld);
    yRot := yRot + (Y-yOld);
    xOld := X; yOld := Y;
  end;
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
  //gluOrtho2D(0, fAreaX, fAreaY, 0);
  gluPerspective(60, fAreaX/fAreaY, 0.01, 50.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;


procedure TRender.Render;
begin
  glEnable(GL_MULTISAMPLE_ARB);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glTranslatef(0,0,-1.1);

  glScalef(0.1,0.1,0.1);
  glRotatef(yRot, 1, 0, 0);
  glRotatef(-xRot, 0, 1, 0);

  fObjectCollection.Render;

  glFinish;
  SwapBuffers(h_DC);
end;


end.
