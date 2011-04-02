unit Unit_Render;
interface
uses Classes, Controls, dglOpenGL, OpenGL, KromOGLUtils, KromUtils, TGATexture, Math, Windows;

type 
  TRender = class
    private
      h_DC: HDC;
      h_RC: HGLRC;
      fAreaX,fAreaY:integer;
      TexBG,TexH:GLUint;
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
uses Unit1;


constructor TRender.Create(RenderFrame:HWND; InX,InY:integer);
begin
  SetRenderFrame(RenderFrame, h_DC, h_RC);
  SetRenderDefaults;
  SetArea(InX,InY);

  LoadTexture(ExeDir+'Back.tga', TexBG, 0);
  LoadTexture(ExeDir+'House.tga', TexH, 0);

  glDisable(GL_LIGHTING);
  //glDisable(GL_BLEND);
  BuildFont(h_DC, 12, FW_BOLD);
  SetupVSync(true);
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(0,0);
  wglDeleteContext(h_RC);
  Inherited;
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
const Qty=10;
var inX,inY:integer;
var i,k:integer; s,d:cardinal; ss,sd:string;
begin
  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glBlendFunc(GL_ONE,GL_ZERO);

  //Background
  glBindTexture(GL_TEXTURE_2D, TexBG);
  glColor4f(1,1,1,1);
  glBegin(GL_QUADS);
    glTexCoord2f(0,0); glvertex2f(0,0);
    glTexCoord2f(0,1); glvertex2f(0,fAreaY);
    glTexCoord2f(1,1); glvertex2f(fAreaX,fAreaY);
    glTexCoord2f(1,0); glvertex2f(fAreaX,0);
  glEnd;


  if IsOverlay or IsTeamColor then
    glBindTexture(GL_TEXTURE_2D, 0)
  else
    glBindTexture(GL_TEXTURE_2D, TexH);


  inX := fAreaX div Qty;
  inY := fAreaY div Qty;


  for i:=1 to Qty do
  for k:=1 to Qty do
  begin
    case i of
    1: s:=GL_ZERO;
    2: s:=GL_ONE;
    3: s:=GL_SRC_COLOR;
    4: s:=GL_ONE_MINUS_SRC_COLOR;
    5: s:=GL_DST_COLOR;
    6: s:=GL_ONE_MINUS_DST_COLOR;
    7: s:=GL_SRC_ALPHA;
    8: s:=GL_ONE_MINUS_SRC_ALPHA;
    9: s:=GL_DST_ALPHA;
    10: s:=GL_ONE_MINUS_DST_ALPHA;
    //11: s:=GL_CONSTANT_COLOR;
    12: s:=GL_ONE_MINUS_CONSTANT_COLOR;
    13: s:=GL_CONSTANT_ALPHA;
    14: s:=GL_ONE_MINUS_CONSTANT_ALPHA;
    15: s:=GL_SRC_ALPHA_SATURATE;
    else s:=GL_ZERO;
    end;
    case k of
    1: d:=GL_ZERO;
    2: d:=GL_ONE;
    3: d:=GL_SRC_COLOR;
    4: d:=GL_ONE_MINUS_SRC_COLOR;
    5: d:=GL_DST_COLOR;
    6: d:=GL_ONE_MINUS_DST_COLOR;
    7: d:=GL_SRC_ALPHA;
    8: d:=GL_ONE_MINUS_SRC_ALPHA;
    9: d:=GL_DST_ALPHA;
    10: d:=GL_ONE_MINUS_DST_ALPHA;
    //11: d:=GL_CONSTANT_COLOR;
    12: d:=GL_ONE_MINUS_CONSTANT_COLOR;
    13: d:=GL_CONSTANT_ALPHA;
    14: d:=GL_ONE_MINUS_CONSTANT_ALPHA;
    else d:=GL_ZERO;
    end;

    if IsTeamColor then begin
      glBlendFunc(GL_ONE,GL_ZERO);
      glBegin(GL_QUADS);
        glColor4f(1,1,1,1);
        glvertex2f((i-1)*inX+4,(k)*inY-3);
        glvertex2f((i-1)*inX+4,(k-1)*inY+3);
        glColor4f(0,0,0,1);
        glvertex2f((i)*inX-4,(k-1)*inY+3);
        glvertex2f((i)*inX-4,(k)*inY-3);
      glEnd;
    end;

    glBlendFunc(s,d);

    if IsAlphaTest then begin
      glEnable(GL_ALPHA_TEST);
      glAlphaFunc(GL_GREATER,0.2);
    end;

    glBegin(GL_QUAD_STRIP);
      if IsShade then glColor4f(0.3,0.3,0.3,1)
                 else glColor4f(1,1,1,1);

      if IsTeamColor then glColor4f(0,0.5,1,0);

      glTexCoord2f(0,1); glvertex2f((i-1)*inX+4,(k)*inY-3);
      glTexCoord2f(0,0); glvertex2f((i-1)*inX+4,(k-1)*inY+3);

      if IsOverlay then glColor4f(0,0,0,1);

      if IsTeamColor then begin
        glColor4f(0,0.5,1,1);
        glvertex2f((i-0.5)*inX,(k)*inY-3);
        glvertex2f((i-0.5)*inX,(k-1)*inY+3);
        glColor4f(0,0.5,1,0);
      end;

      glTexCoord2f(1,1); glvertex2f((i)*inX-4,(k)*inY-3);
      glTexCoord2f(1,0); glvertex2f((i)*inX-4,(k-1)*inY+3);
    glEnd;

    if IsAlphaTest then
      glDisable(GL_ALPHA_TEST);
  end;

  if IsCaptions then begin
    glBlendFunc(GL_ONE,GL_ZERO);
    glColor4f(1,1,1,1);
    glBindTexture(GL_TEXTURE_2D,0);
    for i:=1 to Qty do
    for k:=1 to Qty do
    begin
      case i of
      1: ss:='ZERO';
      2: ss:='ONE';
      3: ss:='SRC_COLOR';
      4: ss:='1-SRC_COLOR';
      5: ss:='DST_COLOR';
      6: ss:='1-DST_COLOR';
      7: ss:='SRC_ALPHA';
      8: ss:='1-SRC_ALPHA';
      9: ss:='DST_ALPHA';
      10: ss:='1-DST_ALPHA';
     //11: ss:='CONSTANT_COLOR';
      12: ss:='1-CONSTANT_COLOR';
      13: ss:='CONSTANT_ALPHA';
      14: ss:='1-CONSTANT_ALPHA';
      15: ss:='SRC_ALPHA_SATURATE';
      else ss:='ZERO';
      end;
      case k of
      1: sd:='ZERO';
      2: sd:='ONE';
      3: sd:='SRC_COLOR';
      4: sd:='1-SRC_COLOR';
      5: sd:='DST_COLOR';
      6: sd:='1-DST_COLOR';
      7: sd:='SRC_ALPHA';
      8: sd:='1-SRC_ALPHA';
      9: sd:='DST_ALPHA';
      10: sd:='1-DST_ALPHA';
      //11: sd:='CONSTANT_COLOR';
      12: sd:='1-CONSTANT_COLOR';
      13: sd:='CONSTANT_ALPHA';
      14: sd:='1-CONSTANT_ALPHA';
      else sd:='ZERO';
      end;
      glRasterPos3f((i-1)*inX+6,(k)*inY-16,0);
      glPrint(ss);
      glRasterPos3f((i-1)*inX+6,(k)*inY-5,0);
      glPrint(sd);
    end;
  end;

  glFinish;
  SwapBuffers(h_DC);
end;


end.
