unit Unit_Render;
interface
uses
  Classes
  {$IFDEF MSWINDOWS}
  , dglOpenGL
  {$ENDIF}
  {$IFDEF MACOS}
  , Macapi.ObjectiveC, Macapi.OpenGL, Macapi.AppKit, Macapi.CocoaTypes, Macapi.Foundation, FMX.Platform.Mac
  {$ENDIF}

  , Math, FMX.Types, OpenGL_FMX, SysUtils;

type
  TRender = class
    private
      fSimpleOGL: TSimpleOGL;
      fAreaX,fAreaY:integer;
      csCircle:GLUint;
      procedure CompileCommonShapes;
    public
      constructor Create(RenderFrame: TFmxHandle; X,Y: Integer);
      destructor Destroy; override;
      procedure SetArea(InX,InY:integer);
      procedure RenderResize;
      procedure Render;
    end;

    
var
  fRender:TRender;


implementation
uses Unit1, Unit_Aiming;


constructor TRender.Create(RenderFrame:TFmxHandle; X,Y: Integer);
begin
  fSimpleOGL := TSimpleOGL.Create;

  fSimpleOGL.Init(0, RenderFrame, 0, True);

  glClearColor(0, 0, 0, 0); 	   //Background
  glClear(GL_COLOR_BUFFER_BIT);
  glShadeModel(GL_SMOOTH);                 //Enables Smooth Color Shading
  glPolygonMode(GL_FRONT,GL_FILL);
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_COLOR_MATERIAL);                 //Enable Materials
  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping

  SetArea(X,Y);

  glDisable(GL_LIGHTING);
  //glDisable(GL_BLEND);

  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POINT_SMOOTH);

  glLineWidth(1);

  //BuildFont(h_DC, 12, FW_BOLD);
  //SetupVSync(True);

  CompileCommonShapes;
end;


destructor TRender.Destroy;
begin
  fSimpleOGL.Close;
  fSimpleOGL.Free;
  Inherited;
end;


procedure TRender.CompileCommonShapes;
const Sides = 100;
var i:integer;
begin
  csCircle := glGenLists(1);
  glNewList(csCircle, GL_COMPILE);
  glBegin(GL_LINE_LOOP);
    for i:=0 to Sides-1 do
    glVertex2f(sin(i/Sides*2*pi), cos(i/Sides*2*pi));

  glEnd;
  glEndList;
end;


procedure TRender.SetArea(InX,InY:integer);
begin
  fAreaX := max(InX,1);
  fAreaY := max(InY,1);

  fSimpleOGL.Resize(fAreaX, fAreaY);

  RenderResize;
end;


procedure TRender.RenderResize;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, fAreaX, fAreaY, 0, -1000, 1000);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;


procedure TRender.Render;
var i:integer;
begin
  fSimpleOGL.Activate;

  glClearColor(1, 1, 1, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glPushMatrix;
  glTranslatef(fAreaX/2, fAreaY/2, 0);

  //Timestamps of flight distance
  glPointSize(12);
  for i:=1 to round(max(fAiming.GetTime,5)*5) do
  begin
    if i mod 5 = 0 then
      glColor3f(0.5,0.5,0.5)
    else
      glColor3f(0.9,0.9,0.9);
    glPushMatrix;
      glScalef(fAiming.ArrowSpeed*i/5, fAiming.ArrowSpeed*i/5, fAiming.ArrowSpeed*i/5);
      glCallList(csCircle);
    glPopMatrix;
  end;

  //Start and Target
  glColor3f(0,0,0);
  glBegin(GL_POINTS);
    glvertex2f(0,0);
    glvertex2fv(@fAiming.TargetPosition);
  glEnd;

  //Target movement vector
  glBegin(GL_LINE_STRIP);
    glvertex2fv(@fAiming.TargetPosition);
    glvertex2f(fAiming.GetTarget(1).X, fAiming.GetTarget(1).Y);
    glColor3f(0.75, 0.75, 0.75);
    glvertex2f(fAiming.GetTarget(max(fAiming.GetTime,5)).X, fAiming.GetTarget(max(fAiming.GetTime,5)).Y);
  glEnd;

  glPointSize(8);

  //Timestamps of target movement
  glColor3f(0.5,0.5,0.5);
  glBegin(GL_POINTS);
    for i:=1 to round(max(fAiming.GetTime,5)) do
    glvertex2f(fAiming.GetTarget(i).X, fAiming.GetTarget(i).Y);
  glEnd;

  if fAiming.GetHit then
  begin
    glColor3f(1,0,0);
    glPointSize(10);

    //Target hit
    glBegin(GL_POINTS);
      glvertex2f(fAiming.GetTarget(fAiming.GetTime).X, fAiming.GetTarget(fAiming.GetTime).Y);
    glEnd;

    //Arrow flight vector
    glBegin(GL_LINES);
      glvertex2f(0,0);
      glvertex2f(fAiming.GetTarget(fAiming.GetTime).X, fAiming.GetTarget(fAiming.GetTime).Y);
    glEnd;
  end;

  glPopMatrix;

  {if fAiming.GetHit then
  begin
    glColor3f(1,0,0);
    glRasterPos2f(10, fAreaY-60);
    glPrint('Target will be hit in '+floattostr(RoundTo(fAiming.GetTime,-3))+' seconds');
  end else
  begin
    glColor3f(0,0.6,0);
    glRasterPos2f(10, fAreaY-60);
    glPrint('Target will never be hit');
  end;

  glColor3f(0,0,0);
  glRasterPos2f(10, fAreaY-20);
  glPrint('Click and drag left mouse button to place an enemy and set its movement vector');}

  fSimpleOGL.Swap;
end;
   

end.
