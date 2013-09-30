unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Forms, FMX.Graphics, FMX.Types3D
  {$IFDEF ANDROID}
  ,Androidapi.Gles, Androidapi.Gles2
  {$ENDIF}
  {$IFDEF WIN32}
  ,Winapi.OpenGL
  {$ENDIF};

type
  TVertice = record
    X, Y, Z, R, G, B, A: Single;
  end;

  TForm1 = class(TForm3D)
    Timer1: TTimer;
    procedure Form3DRender(Sender: TObject; Context: TContext3D);
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fPos: array of TVertice;
    fInd: array of Integer;
    fVtxShd: GLUint;
    fIndShd: GLUint;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Form3DCreate(Sender: TObject);
begin
  //glGenBuffers(1, @fVtxShd);
  //glGenBuffers(1, @fIndShd);

  SetLength(fPos, 3);
  SetLength(fInd, 1);
end;

procedure TForm1.Form3DDestroy(Sender: TObject);
begin
  //glDeleteBuffers(1, @fVtxShd);
  //glDeleteBuffers(1, @fIndShd);
end;

procedure TForm1.Form3DRender(Sender: TObject; Context: TContext3D);
begin
  Context.BeginScene;

  Log.TimeStamp(IntToStr(glGetError));

  glClearColor(Random, Random, Random, 1);
  glClear(GL_COLOR_BUFFER_BIT);

  fPos[0].X := Random(10); fPos[0].Y := Random(10);  fPos[0].Z := Random(10);
  fPos[1].X := Random(10); fPos[1].Y := Random(10);  fPos[1].Z := Random(10);
  fPos[2].X := Random(10); fPos[2].Y := Random(10);  fPos[2].Z := Random(10);

  fInd[0] := 0;
  fInd[1] := 1;
  fInd[2] := 2;

  Context.EndScene;
  {glBindBuffer(GL_ARRAY_BUFFER, fVtxShd);
  glBufferData(GL_ARRAY_BUFFER, Length(fPos) * SizeOf(TVertice), @fPos[0].X, GL_STREAM_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndShd);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(fInd) * SizeOf(fInd[0]), @fInd[0], GL_STREAM_DRAW);

  //Setup vertex and UV layout and offsets
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_COLOR_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertice), Pointer(0));
  glColorPointer(4, GL_FLOAT, SizeOf(TVertice), Pointer(12));

  //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
  glDrawElements(GL_TRIANGLES, Length(fInd), GL_UNSIGNED_INT, Pointer(0));

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);}
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  InvalidateRect(RectF(0, 0, width, height));
end;

end.
