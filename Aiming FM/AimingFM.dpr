program AimingFM;

uses
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Render in 'Unit_Render.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Aiming FM';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
