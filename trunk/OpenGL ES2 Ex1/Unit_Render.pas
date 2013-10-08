unit Unit_Render;
interface
uses
  Classes, Controls, dglOpenGL, KromOGLUtils, KromUtils, Math, Windows, SysUtils;


type
  TRender = class
  private
    h_DC: HDC;
    h_RC: HGLRC;
    fWidth, fHeight: Integer;
    csCircle: GLUint;

    mProgram: Integer;
    maPositionHandle: Integer;
    gvTexCoordHandle: Integer;
    gvSamplerHandle: Integer;
    fText1: GLUint;
    procedure BuildShapes;
    function LoadShader(aType: Integer; aCode: AnsiString): Integer;
  public
    constructor Create(aFrameHandle: HWND; aWidth, aHeight: Integer);
    destructor Destroy; override;
    procedure Resize(aWidth, aHeight: Integer);
    procedure RenderResize;
    procedure Render;
  end;


var
  fRender: TRender;


implementation
uses
  KM_Log;


constructor TRender.Create(aFrameHandle: HWND; aWidth, aHeight: Integer);
var
  m: TStringList;
  vertexShader: Integer;
  fragmentShader: Integer;
  len: Integer;
  err: Integer;
  txt: AnsiString;
  ms: TMemoryStream;
begin
  inherited Create;

  SetRenderFrame(aFrameHandle, h_DC, h_RC);
  Resize(aWidth, aHeight);

  glClearColor(0.7, 0.7, 0.7, 1.0);

  //Shaders
  try
    m := TStringList.Create;
    m.LoadFromFile('shader.vert');
    vertexShader := LoadShader(GL_VERTEX_SHADER, m.Text);
    m.LoadFromFile('shader.frag');
    fragmentShader := LoadShader(GL_FRAGMENT_SHADER, m.Text);
    m.Free;
    err := glGetError;
  except
    gLog.AddNoTime('The shaders could not be found.');
  end;

  // create empty OpenGL Program
  mProgram := glCreateProgram;

  glAttachShader(mProgram, vertexShader);   // add the vertex shader to program
  SetLength(txt, 256);
  glGetShaderInfoLog(vertexShader, 256, len, PAnsiChar(txt));
  SetLength(txt, len);
  gLog.AddNoTime('VS LOG: ' + txt);

  glAttachShader(mProgram, fragmentShader); // add the fragment shader to program
  SetLength(txt, 256);
  glGetShaderInfoLog(fragmentShader, 256, len, PAnsiChar(txt));
  SetLength(txt, len);
  gLog.AddNoTime('FS LOG: ' + txt);

  glLinkProgram(mProgram);                  // creates OpenGL program executables
  SetLength(txt, 256);
  glGetProgramInfoLog(mProgram, 256, len, PAnsiChar(txt));
  SetLength(txt, len);
  gLog.AddNoTime('PROG LOG: ' + txt);

  // get handles
  maPositionHandle := glGetAttribLocation(mProgram, 'vPosition');
  gvTexCoordHandle := glGetAttribLocation(mProgram, 'a_texCoord');
  gvSamplerHandle := glGetAttribLocation(mProgram, 's_texture');

  //Load texture
  glEnable(GL_TEXTURE_2D);
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile('512.tga');
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glGenTextures(1, @fText1);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, fText1);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 256, 256, 0, GL_RGBA, GL_UNSIGNED_BYTE, ms.Memory);
  finally
    ms.Free;
  end;

  BuildShapes;
end;


destructor TRender.Destroy;
begin
  wglMakeCurrent(0, 0);
  wglDeleteContext(h_RC);

  inherited;
end;


procedure TRender.BuildShapes;
const
  Sides = 100;
var
  i: Integer;
begin
  {csCircle := glGenLists(1);
  glNewList(csCircle, GL_COMPILE);
  glBegin(GL_LINE_LOOP);
  for i := 0 to Sides - 1 do
    glVertex2f(sin(i / Sides * 2 * pi), cos(i / Sides * 2 * pi));

  glEnd;
  glEndList;}
end;


function TRender.LoadShader(aType: Integer; aCode: AnsiString): Integer;
var
  len: Integer;
begin
  Result := glCreateShader(aType);
  len := Length(aCode);
  glShaderSource(Result, 1, @aCode, @len);
  glCompileShader(Result);
end;


procedure TRender.Resize(aWidth, aHeight: Integer);
begin
  fWidth:= max(aWidth, 1);
  fHeight := max(aHeight, 1);
  RenderResize;
end;


procedure TRender.RenderResize;
begin
  glViewport(0, 0, fWidth, fHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, fWidth, fHeight, 0, -100, 100);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;


procedure TRender.Render;
const
  verts: array [0..2, 0..4] of Single = (
    (-0.5, -0.5, 0,   0, 0),
    (   0,  0.5, 0, 0.5, 1),
    ( 0.5, -0.5, 0,   1, 0));
var
  i: Integer;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  // Add program to OpenGL environment
  glUseProgram(mProgram);
  // Prepare the triangle data
  glVertexAttribPointer(maPositionHandle, 3, GL_FLOAT, False, 20, @verts[0,0]);
  glVertexAttribPointer(gvTexCoordHandle, 2, GL_FLOAT, False, 20, @verts[0,3]);
  glEnableVertexAttribArray(maPositionHandle);
  glEnableVertexAttribArray(gvTexCoordHandle);
  //Bind texture
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, fText1);
  glUniform1i(gvSamplerHandle, 0);
  // Draw the triangle
  glDrawArrays(GL_TRIANGLES, 0, 3);
  //Disable arrays
  glDisableVertexAttribArray(maPositionHandle);
  glDisableVertexAttribArray(gvTexCoordHandle);

  glFinish;
  SwapBuffers(h_DC);
end;


end.
