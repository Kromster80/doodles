unit Unit_Game;
interface
uses Forms, Controls, Classes, Windows, MMSystem, SysUtils, Math,
  Unit1, Unit_Session, Unit_Render, Unit_UserInterface;


type
  TGame = class
  private
    fExeDir: string;
    fMainForm: TForm1;
    fSession: TSession;
    fUserInterface: TUserInterface;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Session: TSession read fSession;
    procedure NewGame;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure Render;
    procedure Resize(aWidth, aHeight: Integer);
  end;


var
  fGame: TGame;


implementation
uses
  Unit_Cursor, Unit_Defaults;


{ TGame }
constructor TGame.Create;
begin
  inherited;

  Randomize;
  fExeDir := ExtractFilePath(Application.ExeName);

  Application.Title := 'Peely v.alpha';
  Application.CreateForm(TForm1, fMainForm);
  Application.OnIdle := OnIdle;

  TimeBeginPeriod(1); //initialize timer precision

  fMainForm.OnKeyDown := KeyDown;

  fRender := TRender.Create(fMainForm.Handle, 0, 0, fMainForm.ClientWidth, fMainForm.ClientHeight);
  fCursor := TPCursor.Create;
  fUserInterface := TUserInterface.Create(fMainForm.ClientWidth, fMainForm.ClientHeight);
end;


destructor TGame.Destroy;
begin
  fUserInterface.Free;
  fRender.Free;

  TimeEndPeriod(1); //initialize timer precision

  inherited;
end;


procedure TGame.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_F5: SHOW_TEXT_BOUNDS := not SHOW_TEXT_BOUNDS;
    VK_F6: SHOW_SELECTION_BUFFER := not SHOW_SELECTION_BUFFER;
    VK_F7: SHOW_CONTROLS_OVERLAYS := not SHOW_CONTROLS_OVERLAYS;
  end;
end;


procedure TGame.NewGame;
begin
  fSession := TSession.Create;
  fSession.New;
end;


procedure TGame.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if not fMainForm.Active then
    exit;

  Render;
  Done := False;
end;


procedure TGame.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fSession.MouseDown(Button, Shift, X, Y);
end;


procedure TGame.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  fSession.MouseMove(Shift, X, Y);
  fUserInterface.MouseMove(Shift, X, Y);
end;


procedure TGame.Render;
begin
  fRender.IsNormal := True;
  fRender.BeginFrame;

  fSession.Render;

  fRender.Switch(rm2D);
  fUserInterface.Render;
  fCursor.Render;

  if not SHOW_SELECTION_BUFFER then
    fRender.EndFrame;

  fRender.IsNormal := False;
  fRender.BeginFrame;
  fSession.Render;

  if SHOW_SELECTION_BUFFER then
    fRender.EndFrame;
end;


procedure TGame.Resize(aWidth, aHeight: Integer);
begin
  fRender.Resize(aWidth, aHeight);
  fUserInterface.Resize(aWidth, aHeight);
end;


end.
