unit KromOGLUtils;
interface
uses
  dglOpenGL,
  SysUtils, Windows;

type
    TColor4 = cardinal;

    procedure SetRenderFrameAA(DummyFrame,RenderFrame:HWND; AntiAliasing:byte; out h_DC: HDC; out h_RC: HGLRC);
    procedure SetRenderFrame(RenderFrame:HWND; out h_DC: HDC; out h_RC: HGLRC);

    procedure SetRenderDefaults;
    procedure BuildFont(h_DC:HDC; FontSize:integer; FontWeight:word=FW_NORMAL);
    procedure glPrint(text: AnsiString);
    procedure SetupVSync(aVSync:boolean);


implementation


function SetDCPixelFormat(h_DC:HDC; PixelFormat:Integer):boolean;
var
  nPixelFormat: Integer;
  PixelDepth:integer;
  pfd: TPixelFormatDescriptor;
begin
  PixelDepth := 32; //32bpp is common

  with pfd do begin
    nSize           := SizeOf(TPIXELFORMATDESCRIPTOR); // Size Of This Pixel Format Descriptor
    nVersion        := 1;                    // The version of this data structure
    dwFlags         := PFD_DRAW_TO_WINDOW    // Buffer supports drawing to window
                       or PFD_SUPPORT_OPENGL // Buffer supports OpenGL drawing
                       or PFD_DOUBLEBUFFER;  // Supports double buffering
    iPixelType      := PFD_TYPE_RGBA;        // RGBA color format
    cColorBits      := PixelDepth;           // OpenGL color depth
    cRedBits        := 0;                    // Number of red bitplanes
    cRedShift       := 0;                    // Shift count for red bitplanes
    cGreenBits      := 0;                    // Number of green bitplanes
    cGreenShift     := 0;                    // Shift count for green bitplanes
    cBlueBits       := 0;                    // Number of blue bitplanes
    cBlueShift      := 0;                    // Shift count for blue bitplanes
    cAlphaBits      := 0;                    // Not supported
    cAlphaShift     := 0;                    // Not supported
    cAccumBits      := 0;                    // No accumulation buffer
    cAccumRedBits   := 0;                    // Number of red bits in a-buffer
    cAccumGreenBits := 0;                    // Number of green bits in a-buffer
    cAccumBlueBits  := 0;                    // Number of blue bits in a-buffer
    cAccumAlphaBits := 0;                    // Number of alpha bits in a-buffer
    cDepthBits      := 16;                   // Specifies the depth of the depth buffer
    cStencilBits    := 0;                    // Turn off stencil buffer
    cAuxBuffers     := 0;                    // Not supported
    iLayerType      := PFD_MAIN_PLANE;       // Ignored
    bReserved       := 0;                    // Number of overlay and underlay planes
    dwLayerMask     := 0;                    // Ignored
    dwVisibleMask   := 0;                    // Transparent color of underlay plane
    dwDamageMask    := 0;                    // Ignored
  end;

  if PixelFormat = 0 then
    nPixelFormat := ChoosePixelFormat(h_DC, @pfd)
  else
    nPixelFormat := PixelFormat;

  if nPixelFormat = 0 then begin
    MessageBox(0, 'Unable to find a suitable pixel format', 'Error', MB_OK or MB_ICONERROR);
    Result := false;
    exit;
  end;

  //Even with known pixel format we still need to supply some PFD structure
  if not SetPixelFormat(h_DC, nPixelFormat, @pfd) then begin
    MessageBox(0, 'Unable to set the pixel format', 'Error', MB_OK or MB_ICONERROR);
    Result := false;
    exit;
  end;

  Result := true;
end;


function GetMultisamplePixelFormat(h_dc: HDC; AntiAliasing:byte): integer;
var
  pixelFormat: integer;
  ValidFormat: boolean;
  NumFormats: GLUint;
  iAttributes: array of GLint;
begin
  Result := 0;

  if not WGL_ARB_multisample or not Assigned(wglChoosePixelFormatARB) then
    Exit;

  SetLength(iAttributes,21);
  iAttributes[0] := WGL_DRAW_TO_WINDOW_ARB;
  iAttributes[1] := 1;
  iAttributes[2] := WGL_SUPPORT_OPENGL_ARB;
  iAttributes[3] := 1;
  iAttributes[4] := WGL_ACCELERATION_ARB;
  iAttributes[5] := WGL_FULL_ACCELERATION_ARB;
  iAttributes[6] := WGL_COLOR_BITS_ARB;
  iAttributes[7] := 24;
  iAttributes[8] := WGL_ALPHA_BITS_ARB;
  iAttributes[9] := 8;
  iAttributes[10] := WGL_DEPTH_BITS_ARB;
  iAttributes[11] := 16;
  iAttributes[12] := WGL_STENCIL_BITS_ARB;
  iAttributes[13] := 0;
  iAttributes[14] := WGL_DOUBLE_BUFFER_ARB;
  iAttributes[15] := 1;
  iAttributes[16] := WGL_SAMPLE_BUFFERS_ARB;
  iAttributes[17] := 1;
  iAttributes[18] := WGL_SAMPLES_ARB;
  iAttributes[19] := AntiAliasing;
  iAttributes[20] := 0;

  //Try to find mode with slightly worse AA before giving up
  repeat
    iAttributes[19] := AntiAliasing;
    ValidFormat := wglChoosePixelFormatARB(h_dc, @iattributes[0], nil, 1, @pixelFormat, @NumFormats);
    if ValidFormat and (NumFormats >= 1) then
    begin
      Result := pixelFormat;
      exit;
    end;
    AntiAliasing := AntiAliasing div 2;
  until(AntiAliasing < 2);
end;


procedure SetContexts(RenderFrame:HWND; PixelFormat:integer; out h_DC: HDC; out h_RC: HGLRC);
begin
  h_DC := GetDC(RenderFrame);

  if h_DC = 0 then
  begin
    MessageBox(HWND(nil), 'Unable to get a device context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;

  if not SetDCPixelFormat(h_DC, PixelFormat) then
    exit;

  h_RC := wglCreateContext(h_DC);

  if h_RC = 0 then
  begin
    MessageBox(HWND(nil), 'Unable to create an OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;

  if not wglMakeCurrent(h_DC, h_RC) then
  begin
    MessageBox(HWND(nil), 'Unable to activate OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
    exit;
  end;
end;


procedure SetRenderFrame(RenderFrame:HWND; out h_DC: HDC; out h_RC: HGLRC);
begin
  InitOpenGL;
  SetContexts(RenderFrame, 0, h_DC, h_RC);
  ReadImplementationProperties;
  ReadExtensions;
end;


{The key problem is this: the function we use to get WGL extensions is, itself, an OpenGL extension.
Thus like any OpenGL function, it requires an OpenGL context to call it. So in order to get the
functions we need to create a context, we have to... create a context.

Fortunately, this context does not need to be our final context. All we need to do is create a dummy
context to get function pointers, then use those functions directly. Unfortunately, Windows does not
allow recreation of a rendering context within a single HWND. We must destroy previous HWND context
and create final HWND context after we are finished with the dummy context.}
procedure SetRenderFrameAA(DummyFrame,RenderFrame:HWND; AntiAliasing:byte; out h_DC: HDC; out h_RC: HGLRC);
var PixelFormat:integer;
begin
  InitOpenGL;
  SetContexts(DummyFrame, 0, h_DC, h_RC);
  ReadExtensions;
  ReadImplementationProperties;

  PixelFormat := GetMultisamplePixelFormat(h_DC, AntiAliasing);
  wglMakeCurrent(h_DC, 0);
  wglDeleteContext(h_RC);

  SetContexts(RenderFrame, PixelFormat, h_DC, h_RC);
  ReadExtensions;
  ReadImplementationProperties;
end;


procedure SetRenderDefaults;
begin
  glClearColor(0, 0, 0, 0); 	   //Background
  glClear (GL_COLOR_BUFFER_BIT);
  glShadeModel(GL_SMOOTH);                 //Enables Smooth Color Shading
  glPolygonMode(GL_FRONT,GL_FILL);
  glEnable(GL_NORMALIZE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); //Set alpha mode
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_COLOR_MATERIAL);                 //Enable Materials
  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
end;


procedure BuildFont(h_DC:HDC; FontSize:integer; FontWeight:word=FW_NORMAL);
var Font: HFONT;
begin
//New parameter FontSize=16
  font:=CreateFontA(-abs(FontSize),0,0,0,FontWeight,0,0,0,ANSI_CHARSET,
  OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,
  ANTIALIASED_QUALITY,FF_DONTCARE or DEFAULT_PITCH,
  'Terminal');
  SelectObject(h_dc,font);
  wglUseFontBitmaps(h_dc,0,128,20000);
end;


procedure glPrint(text: AnsiString);
begin
  if text = '' then exit;
  glPushAttrib(GL_LIST_BIT);
  glListBase(20000);
  glCallLists(length(text), GL_UNSIGNED_BYTE, PAnsiChar(@text[1]));
  glPopAttrib;
end;


procedure SetupVSync(aVSync:boolean);
begin
  if WGL_EXT_swap_control then
    wglSwapIntervalEXT(byte(aVSync));
end;


end.
