program Peel;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Game in 'Unit_Game.pas',
  Unit_Ingot in 'Unit_Ingot.pas',
  Unit_Render in 'Unit_Render.pas',
  Unit_Session in 'Unit_Session.pas';

{$R *.RES}

begin
  Application.Initialize;

  fGame := TGame.Create;
  fGame.NewGame;

  Application.Run;

end.


