unit Unit1;
interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Diagnostics, FMX.Objects;

type
  TForm1 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;

const
  FPS_INTERVAL = 300;

var
  Form1: TForm1;
  ExeDir: string;

  SW: TStopWatch;
  OldTimeFPS,OldFrameTimes,FrameTime,FrameCount: Int64;


implementation
{$R *.fmx}
uses Unit_Render, Unit_Aiming;


procedure TForm1.FormCreate(Sender: TObject);
begin
  SW := TStopWatch.Create;
  SW.Start;

  Randomize;

  ExeDir := ExtractFilePath(ParamStr(0));

  fRender := TRender.Create(Handle, ClientWidth, ClientHeight);
  fAiming := TAiming.Create;

  Application.OnIdle := OnIdle;

  FormResize(Self);
end;


procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin //Counting FPS

  FrameTime := (SW.GetTimeStamp div 10000 - OldTimeFPS) ;
  OldTimeFPS := SW.GetTimeStamp div 10000;

  inc(OldFrameTimes, FrameTime);
  inc(FrameCount);

  if OldFrameTimes >= FPS_INTERVAL then begin
    Caption := Format('%f fps', [1000 / OldFrameTimes * FrameCount]);
    OldFrameTimes := 0;
    FrameCount := 0;
  end;

  fRender.Render;
  Done := False;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fAiming.Free;
  fRender.Free;
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  if fRender=nil then exit;
  fRender.SetArea(ClientWidth, ClientHeight);
end;


procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  fAiming.TargetPosition := PointF(X - ClientWidth div 2, Y - ClientHeight div 2);
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then
    fAiming.TargetVector := PointF((X - ClientWidth div 2) - fAiming.TargetPosition.X,
                                   (Y - ClientHeight div 2) - fAiming.TargetPosition.Y);
end;

{procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = 'q' then fAiming.UpdatePerformance;
end;}


end.
