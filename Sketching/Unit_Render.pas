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
      fBackColor:single;
    public
      constructor Create(RenderFrame:HWND; InX,InY:integer);
      destructor Destroy; override;

      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure MouseMove(Shift: TShiftState; X, Y: Integer);

      procedure SetArea(InX,InY:integer);
      procedure ChangeBackground;
      procedure RenderResize;
      procedure Render;
    end;

    
var
  fRender:TRender;


implementation
uses Unit_Objects;


constructor TRender.Create(RenderFrame:HWND; InX,InY:integer);
begin
  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults;
  SetArea(InX,InY);

  yRot := 30;

  glDisable(GL_LIGHTING);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  SetupVSync(true);
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(0,0);
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


procedure TRender.ChangeBackground;
begin
  fBackColor := 1 - fBackColor;
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
  glLoadIdentity;
  glTranslatef(0,0,-1.1);

  glColor4f(fBackColor,fBackColor,fBackColor,0.125);
  glRect(-100, 100, 100, -100); //Make sure to cover whole screen

  glScalef(0.01,0.01,0.01);

  glRotatef(yRot, 1, 0, 0);
  glRotatef(-xRot, 0, 1, 0);

  fObjectCollection.Render;

  glFinish;
  SwapBuffers(h_DC);
end;           


end.
