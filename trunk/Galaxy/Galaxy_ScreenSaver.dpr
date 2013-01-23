program Galaxy_ScreenSaver;

uses
  Forms,
  KromUtils,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Galaxy in 'Unit_Galaxy.pas',
  Unit_Render in 'Unit_Render.pas';

{$R *.RES}

const
  //Random GUID generated in Delphi by Ctrl+Shift+G
  GALAXY_MUTEX = '{26C80D46-F53E-45F5-B0E3-F0A13A80AF28}';

begin

  //Only one screensaver instance is allowed at a time
  if not CheckDuplicateApplication(GALAXY_MUTEX) then
  begin
    Application.Initialize;
    Application.Title := 'Galaxy';
    Application.CreateForm(TForm1, Form1);
    Application.Run;
  end;

end.


