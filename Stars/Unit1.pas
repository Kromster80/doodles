unit Unit1;
interface
uses
  Classes, Controls, Forms, Math, SysUtils, Windows, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;

const
      FPS_INTERVAL      =    1000;

var
  Form1: TForm1;
  ExeDir:string;

  OldTimeFPS,OldFrameTimes,FrameTime,FrameCount:cardinal;


implementation
{$R *.DFM}
uses Unit_Render, Unit_Stars;


procedure TForm1.FormCreate(Sender: TObject);
var i: integer; R1, R2: Integer;
begin
  Randomize;

  ExeDir := ExtractFilePath(Application.ExeName);

  fRender := TRender.Create(Handle, ClientWidth, ClientHeight);
  fStars := TStars.Create;

  for i:=0 to 99 do
    fStars.Nodes.Add(random*300-150, random*300-150, TNodeType(Random(Byte(High(TNodeType))+1)));

  for i:=0 to 199 do
  begin
    R1 := Random(100);
    R2 := Random(100);
    if Abs(Integer(fStars.Nodes[R1].NodeType) - Integer(fStars.Nodes[R2].NodeType)) = 1 then
      fStars.Links.Add(R1, R2);
  end;

  fStars.Select(1);


  Application.OnIdle := OnIdle;
  Form1.FormResize(Self);
end;


procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin //Counting FPS
  if not Form1.Active then exit;

  FrameTime:=GetTickCount-OldTimeFPS;
  OldTimeFPS:=GetTickCount;
  if FrameTime>1000 then FrameTime:=1000;
  inc(OldFrameTimes,FrameTime);
  inc(FrameCount);
  if OldFrameTimes>=FPS_INTERVAL then begin
    Caption := floattostr(RoundTo(1000/(OldFrameTimes/FrameCount),-2))+' fps';
    OldFrameTimes:=0;
    FrameCount:=0;
  end; //FPS calculation complete

  fRender.Render;
  Done := false;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fStars.Free;
  fRender.Free;
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  if fRender=nil then exit;
  fRender.SetArea(ClientWidth, ClientHeight);
end;


procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fStars.Select(X - ClientWidth div 2, Y - ClientHeight div 2);
end;


procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    fStars.Move((X - ClientWidth div 2), (Y - ClientHeight div 2));
end;


procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = 'q' then fStars.Order;
end;


end.

