unit Unit_Game;
interface
uses Forms, Controls, Classes, Windows, MMSystem, SysUtils, Math,
  Unit1, Unit_Session, Unit_Render, Unit_UserInterface;


type
  TGame = class
  private
    fExeDir: string;
    fMainForm: TForm1;
    fSession: TSession;
    fUserInterface: TUserInterface;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Session: TSession read fSession;
    procedure NewGame;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
uses Unit_Fonts, Unit_Defaults;


{ TGame }
constructor TGame.Create;
begin
  inherited;

  Randomize;
  fExeDir := ExtractFilePath(Application.ExeName);

  Application.Title := 'Peely v.alpha';
  Application.CreateForm(TForm1, fMainForm);
  Application.OnIdle := OnIdle;

  fMainForm.OnKeyDown := KeyDown;

  fRender := TRender.Create(fMainForm.Handle, 0, 0, fMainForm.ClientWidth, fMainForm.ClientHeight);
  fFontLib := TLFontLib.Create;
  fUserInterface := TUserInterface.Create(fMainForm.ClientWidth, fMainForm.ClientHeight);
end;


destructor TGame.Destroy;
begin
  fUserInterface.Free;
  fFontLib.Free;
  fRender.Free;

  inherited;
end;


procedure TGame.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F5 then
    SHOW_TEXT_BOUNDS := not SHOW_TEXT_BOUNDS;

  if Key = VK_F7 then
    SHOW_CONTROLS_OVERLAYS := not SHOW_CONTROLS_OVERLAYS;
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
  fSession.MouseDown(Button, Shift, X, Y);
end;


procedure TGame.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  fSession.MouseMove(Shift, X, Y);
end;


procedure TGame.Render;
begin
  fRender.BeginFrame;

  fSession.Render;

  fRender.Switch(rm2D);
  fUserInterface.Render;

  fRender.EndFrame;
end;


procedure TGame.Resize(aWidth, aHeight: Integer);
begin
  fRender.Resize(aWidth, aHeight);
  fUserInterface.Resize(aWidth, aHeight);
end;


end.
