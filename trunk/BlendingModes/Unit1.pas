unit Unit1;
interface
uses
  Classes, Controls, Forms, Math, SysUtils, Windows, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
  private
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  end;


const
  FPS_INTERVAL = 1000;

var
  Form1: TForm1;
  ExeDir: string;

  IsAlphaTest,IsCaptions,IsShade,IsOverlay,IsTeamColor: Boolean;

  OldTimeFPS,OldFrameTimes,FrameTime,FrameCount: Cardinal;


implementation
{$R *.DFM}
uses Unit_Render;


procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;

  ExeDir := ExtractFilePath(Application.ExeName);

  fRender := TRender.Create(Handle, ClientWidth, ClientHeight - Panel1.Height);

  Application.OnIdle := OnIdle;
  Form1.FormResize(Self);
end;


procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
begin // Counting FPS
  if not Form1.Active then
    Exit;

  FrameTime := GetTickCount - OldTimeFPS;
  OldTimeFPS := GetTickCount;
  if FrameTime > 1000 then
    FrameTime := 1000;
  Inc(OldFrameTimes, FrameTime);
  Inc(FrameCount);
  if OldFrameTimes >= FPS_INTERVAL then
  begin
    Caption := floattostr(RoundTo(1000 / (OldFrameTimes / FrameCount), -2)) + ' fps';
    OldFrameTimes := 0;
    FrameCount := 0;
  end; // FPS calculation complete

  fRender.Render;
  Done := false;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  fRender.Free;
end;


procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = ord('A') then IsAlphaTest := not IsAlphaTest;
  if Key = ord('C') then IsCaptions := not IsCaptions;
  if Key = ord('O') then IsOverlay := not IsOverlay;
  if Key = ord('S') then IsShade := not IsShade;
  if Key = ord('T') then IsTeamColor := not IsTeamColor;
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  if fRender = nil then
    Exit;

  fRender.SetArea(ClientWidth, ClientHeight - Panel1.Height);
end;


procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  IsAlphaTest := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  IsCaptions := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  IsOverlay := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  IsShade := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  IsTeamColor := TCheckBox(Sender).Checked;
end;


end.
