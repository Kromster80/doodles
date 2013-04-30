unit Unit_Render;
interface
uses Classes, Controls, dglOpenGL, KromOGLUtils, KromUtils, Math, Windows, SysUtils,
  Unit_ColorCoder;

type
  TRenderMode = (rm2D, rmDeck, rm3D);

  TRender = class
  private
    PrevTime, PrevFrameTimes, FrameTime, FrameCount: Cardinal;
    fFPS: string;

    h_DC: HDC;
    h_RC: HGLRC;
    fOpenGL_Vendor, fOpenGL_Renderer, fOpenGL_Version: AnsiString;
    fWidth, fHeight: Word;

    function GetVersionInfo: string;
  public
    IsNormal: Boolean;
    constructor Create(RenderFrame: HWND; aLeft, aTop, aWidth, aHeight: integer);
    destructor Destroy; override;
    property Width: Word read fWidth;
    property Height: Word read fHeight;
    property FPS: string read fFPS;
    procedure Resize(aWidth, aHeight: Integer);
    property VersionInfo: string read GetVersionInfo;
    function CodeBelow(X,Y: Integer): TColorCodeId;
    procedure BeginFrame;
    procedure Switch(aMode: TRenderMode);
    procedure EndFrame;
  end;


const
  FPS_INTERVAL = 1000;

  LightPos: array [0..3] of GLfloat = (-20, 20, 20, 0);
  LightSpec: array [0..3] of GLfloat = (0.7, 0.7, 0.7, 0);
  LightDiff: array [0..3] of GLfloat = (1, 0.9, 1, 1);


var
  fRender: TRender;


implementation
uses
  Unit_Defaults;


constructor TRender.Create(RenderFrame: HWND; aLeft, aTop, aWidth, aHeight: Integer);
begin
  SetRenderFrame(RenderFrame, h_DC, h_RC);
  Resize(aWidth, aHeight);

  fOpenGL_Vendor   := glGetString(GL_VENDOR);
  fOpenGL_Renderer := glGetString(GL_RENDERER);
  fOpenGL_Version  := glGetString(GL_VERSION);

  glShadeModel(GL_SMOOTH);
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_COLOR_MATERIAL);

  glEnable(GL_DEPTH_TEST);

  glPolygonMode(GL_FRONT, GL_FILL);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpec);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @LightDiff);

  BuildFont(h_DC, 12, FW_BOLD);
  SetupVSync(True);
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(h_RC);

  inherited;
end;


procedure TRender.Resize(aWidth, aHeight: Integer);
begin
  fWidth := Max(aWidth, 1);
  fHeight := Max(aHeight, 1);
  glViewport(0, 0, fWidth, fHeight);
end;


procedure TRender.Switch(aMode: TRenderMode);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  case aMode of
    rm2D:   begin
              //Whole screen overlay
              glViewport(0, 0, fWidth, fHeight);
              glOrtho(0, fWidth, fHeight, 0, -10, 30);
            end;
    rmDeck: begin
              //Deck
              glViewport(0, 0, fWidth, DECK_HEIGHT);
              gluOrtho2D(0, fWidth, DECK_HEIGHT/2, -DECK_HEIGHT/2);
            end;
    rm3D:   begin
              //Ingot view
              glViewport(0, DECK_HEIGHT, fWidth, fHeight - DECK_HEIGHT);
              gluPerspective(85, fWidth / (fHeight - DECK_HEIGHT), 0.1, 10000);
            end;
  end;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  if aMode = rm3D then
    glTranslatef(0, 0, -1);
end;


function TRender.CodeBelow(X, Y: Integer): TColorCodeId;
var
  PosX, PosY: Integer;
  Pixel: Cardinal;
begin
  PosX := X;
  PosY := fHeight - Y - 1;

  glReadPixels(PosX, PosY, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @Pixel);
  Pixel := Pixel and $FFFFFF;
  Result := GetColorCode(Pixel);
end;


procedure TRender.BeginFrame;
begin
  if IsNormal then
    glClearColor(0.3, 0.3, 0.3, 1)
  else
    glClearColor(0, 0, 0, 1);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;


procedure TRender.EndFrame;
begin
  glFinish;
  SwapBuffers(h_DC);

  //Calculate FPS
  FrameTime := GetTickCount - PrevTime;
  PrevTime := GetTickCount;
  if FrameTime > 1000 then
    FrameTime := 1000;
  Inc(PrevFrameTimes, FrameTime);
  Inc(FrameCount);
  if PrevFrameTimes >= FPS_INTERVAL then
  begin
    fFPS := FloatToStr(RoundTo(1000 / (PrevFrameTimes / FrameCount), -2)) + ' fps';
    PrevFrameTimes := 0;
    FrameCount := 0;
  end;
end;


function TRender.GetVersionInfo: string;
begin
  Result := fOpenGL_Vendor + ' / ' + fOpenGL_Renderer + ' / ' + fOpenGL_Version;
end;


end.
