unit Unit1;
interface
uses
  Classes, Controls, Forms, Math, SysUtils, Windows, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;

const
  FPS_INTERVAL = 1000;
  {$IFDEF GALAXY_SCREENSAVER}
  CURSOR_HIDE_TIME = 2000;
  {$ENDIF}

var
  Form1: TForm1;
  {$IFDEF GALAXY_SCREENSAVER}
  LastCursorMove: Cardinal;
  {$ENDIF}
  OldTimeFPS,OldFrameTimes,FrameTime,FrameCount: Cardinal;


implementation
{$R *.DFM}
uses Unit_Render, Unit_Galaxy;


procedure TForm1.FormActivate(Sender: TObject);
begin
  {$IFDEF GALAXY_SCREENSAVER}
  WindowState := wsMaximized;
  {$ENDIF}
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;

  {$IFDEF GALAXY_APPLICATION}
  BorderIcons := [biSystemMenu, biMinimize, biMaximize];
  BorderStyle := bsSizeable;
  {$ENDIF}

  {$IFDEF GALAXY_SCREENSAVER}
  BorderIcons := [];
  BorderStyle := bsNone;
  LastCursorMove := GetTickCount;
  {$ENDIF}

  fGalaxy := TGalaxy.Create(600, Max(ClientWidth, ClientHeight));
  fRender := TRender.Create(Handle, ClientWidth, ClientHeight);

  Application.OnIdle := OnIdle;
  Form1.FormResize(Self);
end;


procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin
  {$IFDEF GALAXY_SCREENSAVER}
  if LastCursorMove + CURSOR_HIDE_TIME < GetTickCount then
    Cursor := crNone;
  {$ENDIF}

  //Counting FPS
  FrameTime := GetTickCount - OldTimeFPS;
  OldTimeFPS := GetTickCount;
  if FrameTime > 1000 then
    FrameTime := 1000;
  inc(OldFrameTimes, FrameTime);
  inc(FrameCount);
  if OldFrameTimes >= FPS_INTERVAL then
  begin
    Caption := floattostr(RoundTo(1000 / (OldFrameTimes / FrameCount), -2)) + ' fps';
    OldFrameTimes := 0;
    FrameCount := 0;
  end; //FPS calculation complete

  fRender.Render;
  Done := False;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fRender.Free;
  fGalaxy.Free;
end;


procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {$IFDEF GALAXY_SCREENSAVER}
  //Screen saver should close on any key pressed
  Close;
  {$ENDIF}
end;


procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    fGalaxy.Add(X - ClientWidth / 2, Y - ClientHeight / 2);

  {$IFDEF GALAXY_SCREENSAVER}
  Cursor := crDefault;
  LastCursorMove := GetTickCount; //Screen saver should hide the cursor when its inactive
  {$ENDIF}
end;


procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fGalaxy.Add(X - ClientWidth / 2, Y - ClientHeight / 2);
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  if fRender = nil then
    Exit;

  fGalaxy.Resize(ClientWidth, ClientHeight);
  fRender.Resize(ClientWidth, ClientHeight);
end;


end.
