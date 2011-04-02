program Sketching;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Render in 'Unit_Render.pas',
  Unit_Objects in 'Unit_Objects.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Sketching';
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.


