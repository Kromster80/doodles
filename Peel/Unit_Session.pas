unit Unit_Session;
interface
uses Controls, Classes, dglOpenGL, Unit_Deck, Unit_Ingot, Unit_Pieces, Unit_Defaults;


type
  TMouseArea = (maIngot, maDeck);

  TSession = class
  private
    fDeck: TDeck;
    fIngot: TIngot;

    fMouseArea: TMouseArea;
    PrevX, PrevY: Single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure New;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure Render;
  end;


implementation
uses
  Unit_Render;


{ TSession }
constructor TSession.Create;
begin
  inherited;

end;


destructor TSession.Destroy;
begin

  inherited;
end;


procedure TSession.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PrevX := X;
  PrevY := Y;
  if Y < fRender.Height - DECK_HEIGHT then
    fMouseArea := maIngot
  else
    fMouseArea := maDeck;

  case fMouseArea of
    maIngot:  ;//fIngot.MouseDown
    maDeck:   ;//fDeck.MouseDown
  end;
end;


procedure TSession.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if not (ssLeft in Shift) then
    if Y < fRender.Height - DECK_HEIGHT then
      fMouseArea := maIngot
    else
      fMouseArea := maDeck;

  case fMouseArea of
    maIngot:  if ssLeft in Shift then
                fIngot.Rotate(-(PrevX - X)/1, -(PrevY - Y)/1);
    maDeck:   fDeck.MouseMove(Shift, X, Y);
  end;

  PrevX := X;
  PrevY := Y;
end;


procedure TSession.New;
begin
  fIngot := TIngot.Create;
  fIngot.LoadFromFile('');

  fPieces := TPiecesCollection.Create;
  fPieces.LoadFromFile('');

  fDeck := TDeck.Create;
end;


procedure TSession.Render;
begin
  fRender.Switch(rm3D);
  fIngot.Render;

  fRender.Switch(rmDeck);
  fDeck.Render;
end;


end.
