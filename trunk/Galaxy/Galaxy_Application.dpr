program Galaxy_Application;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Galaxy in 'Unit_Galaxy.pas',
  Unit_Render in 'Unit_Render.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Galaxy';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.


