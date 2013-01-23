program Galaxy_ScreenSaver;

uses
  Forms,
  KromUtils,
  SysUtils,
  Windows,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Galaxy in 'Unit_Galaxy.pas',
  Unit_Render in 'Unit_Render.pas';

{$R *.RES}

const
  //Random GUID generated in Delphi by Ctrl+Shift+G
  GALAXY_MUTEX = '{26C80D46-F53E-45F5-B0E3-F0A13A80AF28}';

var
  Option: AnsiString;
  Rect: TRect;

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
      //Show settings form modal to the foreground window
    else
    if Option = '/p' then
    begin
      //Preview Screen Saver as child of window <HWND>
      Application.CreateForm(TForm1, Form1);
      Application.MainFormOnTaskBar := False;
      Form1.BorderIcons := [];
      Form1.BorderStyle := bsNone;
      Form1.ParentWindow := StrToIntDef(ParamStr(2), 0);
      Form1.Visible := True;
      GetWindowRect(Form1.ParentWindow, Rect);
      with Rect do
        Form1.SetBounds(0, 0, Right-Left, Bottom-Top);
      Form1.FormCreate(nil); //Reinit render (cos parent changed?)
    end
    else
    if Option = '/s' then
    begin
      //Run the Screen Saver
      Application.CreateForm(TForm1, Form1);
      Form1.BorderIcons := [];
      Form1.BorderStyle := bsNone;
      Form1.FormStyle := fsStayOnTop;
      Form1.WindowState := wsMaximized;
    end
    else
      //Show settings form
      ;
  end
  else
    Application.CreateForm(TForm1, Form1);


  Application.Run;

end.


