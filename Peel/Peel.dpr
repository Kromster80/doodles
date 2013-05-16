program Peel;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Color in 'Unit_Color.pas',
  Unit_ColorCoder in 'Unit_ColorCoder.pas',
  Unit_Controls in 'Unit_Controls.pas',
  Unit_ControlsRender in 'Unit_ControlsRender.pas',
  Unit_Cursor in 'Unit_Cursor.pas',
  Unit_Deck in 'Unit_Deck.pas',
  Unit_Defaults in 'Unit_Defaults.pas',
  Unit_Game in 'Unit_Game.pas',
  Unit_Ingot in 'Unit_Ingot.pas',
  Unit_Pieces in 'Unit_Pieces.pas',
  Unit_Render in 'Unit_Render.pas',
  Unit_Session in 'Unit_Session.pas',
  Unit_UserInterface in 'Unit_UserInterface.pas',
  Unit_Vector in 'Unit_Vector.pas';

{$R *.RES}

begin
  Application.Initialize;

  fGame := TGame.Create;
  fGame.NewGame;

  Application.Run;
end.


