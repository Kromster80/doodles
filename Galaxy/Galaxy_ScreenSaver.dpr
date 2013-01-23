program Galaxy_ScreenSaver;

uses
  Forms,
  KromUtils,
  SysUtils,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Galaxy in 'Unit_Galaxy.pas',
  Unit_Render in 'Unit_Render.pas';

{$R *.RES}

const
  //Random GUID generated in Delphi by Ctrl+Shift+G
  GALAXY_MUTEX = '{26C80D46-F53E-45F5-B0E3-F0A13A80AF28}';

var
  Option: AnsiString;

begin

  //Only one screensaver instance is allowed at a time
  if CheckDuplicateApplication(GALAXY_MUTEX) then Exit;

  Application.Initialize;
  Application.Title := 'Galaxy';

  if ParamCount > 0 then
  begin
    // only check the first two characters, in case windows decides to add
    // extra stuff
    Option := LowerCase(Copy(ParamStr(1), 1, 2));
    if Option = '/c' then
      //ShowSettings
    else if Option = '/p' then
      //ShowPreview
    else if Option = '/s' then
      Application.CreateForm(TForm1, Form1)
    else
      //ShowSettings;
  end
  else
    Application.CreateForm(TForm1, Form1);


  Application.Run;

end.


