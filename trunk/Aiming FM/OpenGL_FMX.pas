unit OpenGL_FMX;
interface
uses
  FMX.Types,
  {$IFDEF MSWINDOWS}
  dglOpenGL, Winapi.Windows, FMX.Platform.Win;
  {$ENDIF}

  {$IFDEF MACOS}
  Macapi.ObjectiveC, Macapi.OpenGL, Macapi.AppKit, Macapi.CocoaTypes, FMX.Platform.Mac, System.Rtti;
  {$ENDIF}


type
  TSimpleOGL = class
  private
    fOpenGL_Vendor, fOpenGL_Renderer, fOpenGL_Version, fOpenGL_Error: AnsiString;

  {$IFDEF MSWINDOWS}
    H_DC: HDC;
    H_RC: HGLRC;
    H_WND: HWND;
  {$ENDIF}
  {$IFDEF MACOS}
    H_WND: IObjectiveC;
    H_RC: NSOpenGLContext;
    function GetNSView: NSView;
  {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function VersionInfo: AnsiString;
    function Init(ADummy, AHandle: TFmxHandle; aAASamples: Byte; aVSync: Boolean): Boolean;
    procedure Close;
    function Activate: Boolean;
    procedure Deactivate;
    procedure Swap;
    procedure Resize(X,Y: Integer);
  end;


implementation


{ TSimpleOGL }
constructor TSimpleOGL.Create;
begin
  inherited;
end;


destructor TSimpleOGL.Destroy;
begin
  Close;
  inherited;
end;


function TSimpleOGL.VersionInfo: AnsiString;
begin
  Result := fOpenGL_Vendor + ' . ' +
            fOpenGL_Renderer + ' . ' +
            'Version: ' + fOpenGL_Version + ' . ' +
            'Err: ' + fOpenGL_Error
end;


function TSimpleOGL.Init(ADummy, AHandle: TFmxHandle; aAASamples: Byte; aVSync: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  PixelFormat: Integer;
  PixelFormatDescriptor: TPixelFormatDescriptor;
  Options: Cardinal;
{$ENDIF}

{$IFDEF MACOS}
var
  PixelFormat: NSOpenGLPixelFormat;
  Options: array of NSOpenGLPixelFormatAttribute;

  procedure AddOption(Opt: NSOpenGLPixelFormatAttribute);
  var
    Count: Integer;
  begin
    Count := Length(Options);
    SetLength(Options, Count + 1);
    Options[Count] := Opt;
  end;
{$ENDIF}

begin
  {$IFDEF MSWINDOWS}
    Result := False;
    H_WND := FmxHandleToHWND(AHandle);

    InitOpenGL;
    H_DC := GetDC(H_WND);

    if H_DC = 0 then
    begin
      MessageBox(HWND(nil), 'Unable to get a device context', 'Error', MB_OK or MB_ICONERROR);
      Exit;
    end;

    Options := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;

    FillChar(PixelFormatDescriptor, SizeOf(PixelFormatDescriptor), 0);
    PixelFormatDescriptor.nSize        := SizeOf(PixelFormatDescriptor);
    PixelFormatDescriptor.nVersion     := 1;
    PixelFormatDescriptor.dwFlags      := Options;
    PixelFormatDescriptor.iPixelType   := PFD_TYPE_RGBA;
    PixelFormatDescriptor.cColorBits   := 32;
    PixelFormatDescriptor.cDepthBits   := 24;
    PixelFormatDescriptor.cStencilBits := 0;
    PixelFormatDescriptor.iLayerType   := PFD_MAIN_PLANE;

    PixelFormat := ChoosePixelFormat(H_DC, @PixelFormatDescriptor);

    //Even with known pixel format we still need to supply some PFD structure
    if PixelFormat = 0 then begin
      MessageBox(0, 'Unable to find a suitable pixel format', 'Error', MB_OK or MB_ICONERROR);
      Exit;
    end;

    if not SetPixelFormat(H_DC, PixelFormat, @PixelFormatDescriptor) then begin
      MessageBox(0, 'Unable to set the pixel format', 'Error', MB_OK or MB_ICONERROR);
      Exit;
    end;

    H_RC := wglCreateContext(H_DC);

    if H_RC = 0 then
    begin
      MessageBox(HWND(nil), 'Unable to create an OpenGL rendering context', 'Error', MB_OK or MB_ICONERROR);
      Exit;
    end;

    ActivateRenderingContext(H_DC, H_RC);

    if aAASamples <> 0 then
    begin
      //PixelFormat := GetMultisamplePixelFormat(H_DC, AntiAliasing);
      //Close;
      //SetPixelFormat(PixelFormat)
      //ActivateRenderingContext(H_DC, H_RC);
    end;

    //Vertical synchronization
    if WGL_EXT_swap_control then
      wglSwapIntervalEXT(byte(aVSync));

    Result := True;
  {$ENDIF}

  {$IFDEF MACOS}
    H_WND := FmxHandleToObjC(AHandle);

    PixelFormat := TNSOpenGLPixelFormat.Create;
    try
      Initialize(Options);
      AddOption(NSOpenGLPFAAccelerated);
      AddOption(NSOpenGLPFADoubleBuffer);
      AddOption(NSOpenGLPFADepthSize);
      AddOption(16);
      AddOption(NSOpenGLPFAStencilSize);
      AddOption(0);
      AddOption(0); //Closing zero

      PixelFormat := TNSOpenGLPixelFormat.Wrap(PixelFormat.initWithAttributes(@Options[0]));

      H_RC := TNSOpenGLContext.Wrap(TNSOpenGLContext.Create.initWithFormat(PixelFormat, nil));
    finally
      PixelFormat.Release;
    end;

    Result := InitOpenGL > 0;

    //We should activate RC before executing any OGL calls
    Activate;
  {$ENDIF}

  //PAnsiChar is used because OSX returns PByte instead of PAnsiChar
  fOpenGL_Vendor   := PAnsiChar(glGetString(GL_VENDOR));
  fOpenGL_Renderer := PAnsiChar(glGetString(GL_RENDERER));
  fOpenGL_Version  := PAnsiChar(glGetString(GL_VERSION));
end;


procedure TSimpleOGL.Close;
begin
  {$IFDEF MSWINDOWS}
    wglMakeCurrent(H_WND, 0);
    wglDeleteContext(H_RC);
  {$ENDIF}

  {$IFDEF MACOS}
    H_RC.Release;
  {$ENDIF}
end;


{$IFDEF MACOS}
function TSimpleOGL.GetNSView : NSView;
var
  Ctx: TRttiContext;
  Obj: TObject;
  Prop: TRttiProperty;
  View: NSView;
begin
  Result := nil;
  Obj := TObject(H_WND);
  Ctx := TRttiContext.Create;
  try
    // special thanks! lynatan.
    Prop := Ctx.GetType(Obj.ClassType).GetProperty('View');
    View := Prop.GetValue(Obj).AsInterface as NSView;
    Result := View;
  finally
    Ctx.Free;
  end;
end;
{$ENDIF}


function TSimpleOGL.Activate: Boolean;
{$IFDEF MACOS}
var
  View: NSView;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
    wglMakeCurrent(H_DC, H_RC);
    Result := True;
  {$ENDIF}

  {$IFDEF MACOS}
    View := GetNSView;
    Result := View <> nil;
    if Result then
    begin
      H_RC.SetView(View);
      H_RC.MakeCurrentContext;
    end;
  {$ENDIF}
end;


procedure TSimpleOGL.Deactivate;
begin
  {$IFDEF MSWINDOWS}
    wglMakeCurrent(0, 0);
  {$ENDIF}
  {$IFDEF MACOS}

  {$ENDIF}
end;


procedure TSimpleOGL.Resize(X,Y: Integer);
begin
  {$IFDEF MACOS}
    H_RC.Update;
  {$ENDIF}

  glViewport(0, 0, X, Y);
end;


procedure TSimpleOGL.Swap;
begin
  glFinish;
  {$IFDEF MSWINDOWS}
    SwapBuffers(H_DC);
  {$ENDIF}
  {$IFDEF MACOS}
    glSwapAPPLE;
  {$ENDIF}
end;


end.
