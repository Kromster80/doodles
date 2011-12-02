unit OpenGL_FMX;
interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Platform, FMX.Types, FMX.Controls, FMX.Forms
  {$IFDEF MSWINDOWS}
  , dglOpenGL
  , Winapi.Windows, Winapi.OpenGL, FMX.Platform.Win
  {$ENDIF}

  {$IFDEF MACOS}
  ,Macapi.ObjectiveC, Macapi.OpenGL, Macapi.AppKit, Macapi.CocoaTypes, Macapi.Foundation, FMX.Platform.Mac, System.Rtti
  {$ENDIF}
  ;

type
  TSimpleOGL = class
  private
  {$IFDEF MSWINDOWS}
    H_DC: HDC;
    H_RC: HGLRC;
    H_WND: HWND;
  {$ENDIF}
  {$IFDEF MACOS}
    H_WND: IObjectiveC;
    FLibraryHandle: HMODULE;
    H_RC: NSOpenGLContext;
    function GetNSView: NSView;
  {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

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


function TSimpleOGL.Init(ADummy, AHandle: TFmxHandle; aAASamples: Byte; aVSync: Boolean): Boolean;
{$IFDEF MSWINDOWS}
var
  PixelFormat : Integer;
  PixelFormatDescriptor : TPixelFormatDescriptor;
  option : DWORD;
{$ENDIF}

{$IFDEF MACOS}
var
  PixelFormat: NSOpenGLPixelFormat;
  OpenGLContext: NSOpenGLContext;
  option: array of NSOpenGLPixelFormatAttribute;

  procedure addOption( opt : NSOpenGLPixelFormatAttribute );
  var
    len: integer;
  begin
    len := Length(option);
    SetLength(option, len+1);
    option[len] := opt;
  end;
{$ENDIF}

begin
{$IFDEF MSWINDOWS}
  H_WND := FmxHandleToHWND(AHandle);

  InitOpenGL;
  H_DC := GetDC(H_WND);

  if H_DC = 0 then
  begin
    MessageBox(HWND(nil), 'Unable to get a device context', 'Error', MB_OK or MB_ICONERROR);
    Exit;
  end;

  Option := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;

  FillChar(PixelFormatDescriptor, SizeOf(PixelFormatDescriptor), 0);
  PixelFormatDescriptor.nSize        := SizeOf(PixelFormatDescriptor);
  PixelFormatDescriptor.nVersion     := 1;
  PixelFormatDescriptor.dwFlags      := Option;
  PixelFormatDescriptor.iPixelType   := PFD_TYPE_RGBA;
  PixelFormatDescriptor.cColorBits   := 32;
  PixelFormatDescriptor.cDepthBits   := 16;
  PixelFormatDescriptor.cStencilBits := 0;
  PixelFormatDescriptor.iLayerType   := PFD_MAIN_PLANE;

  PixelFormat := ChoosePixelFormat(H_DC, @PixelFormatDescriptor);

  //Even with known pixel format we still need to supply some PFD structure
  if PixelFormat = 0 then begin
    MessageBox(0, 'Unable to find a suitable pixel format', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
    Exit;
  end;

  if not SetPixelFormat(H_DC, PixelFormat, @PixelFormatDescriptor) then begin
    MessageBox(0, 'Unable to set the pixel format', 'Error', MB_OK or MB_ICONERROR);
    Result := False;
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
{$ENDIF}

{$IFDEF MACOS}
  H_WND := FmxHandleToObjC(AHandle);

  PixelFormat := TNSOpenGLPixelFormat.Create;
  try
    Initialize(option);
    addOption(NSOpenGLPFAAccelerated);
    //addOption(NSOpenGLPFADoubleBuffer);
    addOption(NSOpenGLPFADepthSize);
    addOption(24);
    addOption(NSOpenGLPFAStencilSize);
    addOption(0);
    addOption(0); //Add closing zeroes

    PixelFormat := TNSOpenGLPixelFormat.Wrap(PixelFormat.initWithAttributes(@option[0]));

    H_RC := TNSOpenGLContext.Wrap(TNSOpenGLContext.Create.initWithFormat(PixelFormat, nil));
  finally
    PixelFormat.release;
  end;

  FLibraryHandle := InitOpenGL;

  //@Mark: Supposedly this will make it work on Mac. Please give it a try on Mac hardware
  //uncommenting the lines below

  //Activate;
  //ReadImplementationProperties;
  //ReadExtensions;
  //Deactivate; //?
{$ENDIF}

  Result := True;
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
  ctx: TRttiContext;
  obj: TObject;
  prop: TRttiProperty;
  view : NSView;
  rect : NSRect;
begin
  result := nil;
  obj := TObject(H_WND);
  ctx := TRttiContext.Create;
  try
    // special thanks! lynatan.
    prop := ctx.GetType(obj.ClassType).GetProperty('View');
    view := prop.GetValue(obj).AsInterface as NSView;
    result := view;
  finally
    ctx.Free;
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
      H_RC.setView(View);
      H_RC.makeCurrentContext;
    end;
  {$ENDIF}
end;


procedure TSimpleOGL.Deactivate;
begin
  {$IFDEF MSWINDOWS}
    wglMakeCurrent(0,0);
  {$ENDIF}
  {$IFDEF MACOS}

  {$ENDIF}
end;


procedure TSimpleOGL.Resize(X,Y: Integer);
begin
  {$IFDEF MACOS}
    H_RC.Update;
  {$ENDIF}
  Activate;
  glViewPort(0, 0, X, Y);
  Deactivate;
end;


procedure TSimpleOGL.Swap;
begin
  {$IFDEF MSWINDOWS}
    SwapBuffers(H_DC);
  {$ENDIF}
  {$IFDEF MACOS}
    glSwapAPPLE;
  {$ENDIF}
end;


initialization


end.
