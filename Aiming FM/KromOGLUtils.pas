unit KromOGLUtils;
interface
uses
  dglOpenGL,
  SysUtils
  {$IFDEF WINDOWS}
  ,Windows
  {$ENDIF}
  ;
type
    TColor4 = cardinal;
    KCode = (kNil=0, kObject=7);

    procedure SetRenderDefaults;
    procedure glkScale(x:single);
    procedure kSetColorCode(TypeOfValue: KCode;IndexNum:integer);
    procedure kGetColorCode(RGBColor:Pointer;var TypeOfValue:KCode;var IndexNum:integer);


implementation


{$IFDEF WINDOWS}
//Unused
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
{$ENDIF}


procedure SetRenderDefaults;
begin
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
end;


procedure glkScale(x:single);
begin
  glScalef(x,x,x);
end;


procedure kSetColorCode(TypeOfValue:KCode; IndexNum:integer);
begin
  glColor4ub(IndexNum mod 256,
            (IndexNum mod 65536) div 256,    // 1,2,4(524288) 8,16,32,64,128 //0..31
            (IndexNum mod 524288) div 65536 + byte(TypeOfValue)*8, 255);
end;


procedure kGetColorCode(RGBColor:Pointer; var TypeOfValue:KCode; var IndexNum:integer);
begin
  IndexNum := pword(cardinal(RGBColor))^+((pbyte(cardinal(RGBColor)+2)^)mod 8)*65536;
  TypeOfValue := KCode((pbyte(cardinal(RGBColor)+2)^)div 8);
end;


end.
