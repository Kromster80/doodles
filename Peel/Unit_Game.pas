unit Unit_Game;
interface
uses Forms, Controls, Classes, Windows, MMSystem, SysUtils, Math,
  Unit1, Unit_Session, Unit_Render;


type
  TGame = class
  private
    fExeDir: string;
    fMainForm: TForm1;
    fRender: TRender;
    fSession: TSession;
    PrevX, PrevY: Single;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Session: TSession read fSession;
    procedure NewGame;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure Render;
    procedure Resize(aWidth, aHeight: Integer);
  end;


const
  FPS_INTERVAL = 1000;

var
  fGame: TGame;
  OldTimeFPS,OldFrameTimes,FrameTime,FrameCount: Cardinal;


implementation


{ TGame }
constructor TGame.Create;
begin
  inherited;

  Randomize;
  fExeDir := ExtractFilePath(Application.ExeName);

  Application.Title := 'Peely v.alpha';
  Application.CreateForm(TForm1, fMainForm);
  Application.OnIdle := OnIdle;
  fRender := TRender.Create(fMainForm.Handle, 0, 0, fMainForm.ClientWidth, fMainForm.ClientHeight);
end;


destructor TGame.Destroy;
begin

  inherited;
end;


procedure TGame.NewGame;
begin
  fSession := TSession.Create;
  fSession.New;
end;


procedure TGame.OnIdle(Sender: TObject; var Done: Boolean);
begin //Counting FPS
  if not fMainForm.Active then exit;

  FrameTime:=GetTickCount-OldTimeFPS;
  OldTimeFPS:=GetTickCount;
  if FrameTime>1000 then FrameTime:=1000;
  inc(OldFrameTimes,FrameTime);
  inc(FrameCount);
  if OldFrameTimes>=FPS_INTERVAL then begin
    fMainForm.Caption := floattostr(RoundTo(1000/(OldFrameTimes/FrameCount),-2))+' fps';
    OldFrameTimes:=0;
    FrameCount:=0;
  end; //FPS calculation complete

  Render;
  Done := false;
end;


procedure TGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PrevX := X;
  PrevY := Y;
end;


procedure TGame.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    fSession.Rotate((PrevX - X)/1, (PrevY - Y)/1);

  PrevX := X;
  PrevY := Y;
end;


procedure TGame.Render;
begin
  fRender.BeginFrame;

  fRender.Switch(rm3D);

  fSession.Render;

  fRender.Switch(rm2D);

  fRender.EndFrame;
end;


procedure TGame.Resize(aWidth, aHeight: Integer);
begin
  fRender.Resize(0, 0, aWidth, aHeight);
end;


end.
