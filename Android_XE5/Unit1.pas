unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IoUtils,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Forms, FMX.Graphics, FMX.Types3D
  {$IFDEF ANDROID}
  ,Androidapi.Gles2
  ,Androidapi.Gles2ext
  {$ENDIF}
  {$IFDEF WIN32}
  ,Winapi.OpenGL, FMX.Controls3D, FMX.Objects3D
  {$ENDIF};

type
  TVertice = record
    X, Y, Z, U, V: Single;
  end;

  TForm1 = class(TForm3D)
    Timer1: TTimer;
    procedure Form3DRender(Sender: TObject; Context: TContext3D);
    procedure Form3DCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fPos: array [0..2] of TVertice;
    fVtxShd: GLUint;
    fText1: GLUint;
    mProgramObject: Integer;
    maPositionHandle: Integer;
    gvTexCoordHandle: Integer;
    gvSamplerHandle: Integer;
    d: array [0..512*512] of Cardinal;
  end;

var
  Form1: TForm1;

implementation
{$R *.fmx}
uses
  FMX.Consts;

const
  GLESHeaderHigh: array [0..24] of byte =
    (Byte('p'), Byte('r'), Byte('e'), Byte('c'), Byte('i'), Byte('s'), Byte('i'), Byte('o'), Byte('n'), Byte(' '),
     Byte('h'), Byte('i'), Byte('g'), Byte('h'), Byte('p'), Byte(' '), Byte(' '), Byte(' '), Byte('f'), Byte('l'),
     Byte('o'), Byte('a'), Byte('t'), Byte(';'), Byte(#13));

procedure RaiseContextExceptionFmt(ResStringRec: PResStringRec; const Args: array of const);
begin
  {$IFDEF ANDROID}
  Log.d('[Context Exception]: ' + Format(LoadResString(ResStringRec), Args));
  {$ENDIF}
  EContext3DException.CreateResFmt(ResStringRec, Args);
end;

function GLGetErrorFlags: Integer;
const
  MaxStopLock = 16;
var
  Flag, StopLock: Integer;
begin
  Result := 0;

  StopLock := MaxStopLock;

  repeat
    Flag := glGetError;
    if Flag <> GL_NO_ERROR then
      Result := Result or Flag;

    Dec(StopLock);
  until (Flag = GL_NO_ERROR) or (StopLock <= 0);
end;

function GLHasAnyErrors: Boolean;
begin
  Result := GLGetErrorFlags <> 0;
end;

function BuildShader(AType: Integer; const ACode: String): Integer;
var
  log: array of Byte;
  len, compiled: Integer;
  code: array of Byte;
  I: Integer;
begin
  Result := 0;

    if AType = GL_FRAGMENT_SHADER then
    begin
      SetLength(code, Length(GLESHeaderHigh) + Length(ACode));
      for I := 0 to High(GLESHeaderHigh) do
        code[I] := GLESHeaderHigh[I];
      for I := 0 to High(ACode) do
        code[Length(GLESHeaderHigh) + I] := Ord(ACode[I]);
    end else begin
      SetLength(code, Length(ACode));
      for I := 0 to High(ACode) do
        code[I] := Ord(ACode[I]);
    end;
    Result := glCreateShader(AType);
    len := Length(code);
    glShaderSource(Result, 1, @code, @len);
    glCompileShader(Result);
    glGetShaderiv(Result, GL_COMPILE_STATUS, @compiled);
    if compiled = 0 then
    begin
      glGetShaderiv(Result, GL_INFO_LOG_LENGTH, @compiled);
      if (compiled > 0) then
      begin
        {$WARNINGS OFF}
        SetLength(log, compiled);
        glGetShaderInfoLog(Result, compiled, @compiled, MarshaledAString(log));
        FMX.Types.Log.d(MarshaledAString(log));
        if AType = GL_VERTEX_SHADER then
          RaiseContextExceptionFmt(@SCannotCreateVertexShader, [''])
        else
          RaiseContextExceptionFmt(@SCannotCreatePixelShader, ['']);
        {$WARNINGS ON}
      end;
    end;
    if (GLHasAnyErrors()) then
      RaiseContextExceptionFmt(@SCannotCreateShader, ['']);
end;


procedure TForm1.Form3DCreate(Sender: TObject);
const
  vShaderStr: String =
            'attribute vec4 vPosition;    '
          + 'attribute vec2 a_texCoord;   '
          + 'varying vec2 v_texCoord;     '
          + 'void main()                  '
          + '{                            '
          + '   gl_Position = vPosition;  '
          + '   v_texCoord = a_texCoord;  '
          + '}                            ';

  fShaderStr: String =
            'precision mediump float;                     '
          + 'varying vec2 v_texCoord;                     '
          + 'uniform sampler2D s_texture;                 '
          + 'void main()                                  '
          + '{                                            '
          + '  gl_FragColor = texture2D(s_texture, v_texCoord); '
          + '}                                            ';
var
  vertexShader: Integer;
  fragmentShader: Integer;
  programObject: Integer;
  linked: Integer;
  ms: TMemoryStream;
begin
        // Load the vertex/fragment shaders
        vertexShader := BuildShader(GL_VERTEX_SHADER, vShaderStr);
        fragmentShader := BuildShader(GL_FRAGMENT_SHADER, fShaderStr);

        // Create the program object
        programObject := glCreateProgram();

        if (programObject = 0) then
            Exit;

        glAttachShader(programObject, vertexShader);
        glAttachShader(programObject, fragmentShader);

        // Bind vPosition to attribute 0
        //glBindAttribLocation(programObject, 0, 'vPosition');

        // Link the program
        glLinkProgram(programObject);

        // Check the link status
        glGetProgramiv(programObject, GL_LINK_STATUS, @linked);

        if (linked = 0) then
        begin
            Log.TimeStamp('Error linking program:');
            //Log.TimeStamp(glGetProgramInfoLog(programObject));
            glDeleteProgram(programObject);
            Exit;
        end;

        maPositionHandle := glGetAttribLocation(programObject, 'vPosition');
        gvTexCoordHandle := glGetAttribLocation(programObject, 'a_texCoord');
        gvSamplerHandle := glGetUniformLocation(programObject, 's_texture');


        // Store the program object
        mProgramObject := programObject;

        glClearColor(0.7, 0.7, 0.7, 1);


  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile('/storage/sdcard/xe5/Tiles512.tga');
    ms.Position := 0;

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
    glGenTextures(1, @fText1);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, fText1);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

    //FillChar(d[0], SizeOf(d), $ff);
    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 64, 64, 0, GL_RGBA, GL_UNSIGNED_BYTE, @d[0]);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 512, 512, 0, GL_RGB, GL_UNSIGNED_BYTE, ms.Memory);
  finally
    ms.Free;
  end;
        Log.TimeStamp(IntToStr(glGetError));
        Log.TimeStamp(IntToStr(glGetError));
        Log.TimeStamp(IntToStr(glGetError));

  Caption := Context.QualifiedClassName;
end;


procedure TForm1.Form3DRender(Sender: TObject; Context: TContext3D);
begin
  if Context.BeginScene then
  try
    //Log.TimeStamp(IntToStr(glGetError));

    fPos[0].X := Random-0.5; fPos[0].Y := Random-0.5;  fPos[0].Z := 0;
    fPos[1].X := Random-0.5; fPos[1].Y := Random-0.5;  fPos[1].Z := 0;
    fPos[2].X := Random-0.5; fPos[2].Y := Random-0.5;  fPos[2].Z := 0;

    fPos[0].U := Random-0.5; fPos[0].V := Random-0.5;
    fPos[1].U := Random-0.5; fPos[1].V := Random-0.5;
    fPos[2].U := Random-0.5; fPos[2].V := Random-0.5;

        // Set the viewport
        //glViewport(0, 0, Width, Height);

        // Clear the color buffer
        glClear(GL_COLOR_BUFFER_BIT);

        // Use the program object
        glUseProgram(mProgramObject);

        // Load the vertex data
        glVertexAttribPointer(maPositionHandle, 3, GL_FLOAT, 0, 20, @fPos[0].X);
        glVertexAttribPointer(gvTexCoordHandle, 2, GL_FLOAT, 0, 20, @fPos[0].U);
        glEnableVertexAttribArray(maPositionHandle);
        glEnableVertexAttribArray(gvSamplerHandle);
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, fText1);
        glUniform1i(gvSamplerHandle, 0);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glDisableVertexAttribArray(maPositionHandle);
        glDisableVertexAttribArray(gvTexCoordHandle);

{        glVertexAttribPointer(maPositionHandle, 3, GL_FLOAT, 0, 20, @fPos[0].X);
        glEnableVertexAttribArray(maPositionHandle);
        glBindTexture(GL_TEXTURE_2D, 0);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glDisableVertexAttribArray(maPositionHandle);}
  finally
    Context.EndScene;
  end;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Invalidate;
end;


end.
