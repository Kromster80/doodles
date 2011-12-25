program Aiming;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Render in 'Unit_Render.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Aiming';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.


