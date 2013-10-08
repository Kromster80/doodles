program Ex1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Render in 'Unit_Render.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'OpenGL ES 2.0 Ex1';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.


