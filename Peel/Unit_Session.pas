unit Unit_Session;
interface
uses dglOpenGL, Unit_Deck, Unit_Ingot, Unit_Pieces;


type
  TSession = class
  private
    fPieces: TPiecesCollection;
    fDeck: TDeck;
    fIngot: TIngot;

    fX: Single;
    fY: Single;
    fHead: Single;
    fPitch: Single;
  public
    constructor Create;
    destructor Destroy; override;
    procedure New;
    procedure Move(X,Y: Single);
    procedure Rotate(X,Y: Single);
    procedure Render;
  end;


implementation


{ TSession }
constructor TSession.Create;
begin
  inherited;

end;


destructor TSession.Destroy;
begin

  inherited;
end;


procedure TSession.Move(X, Y: Single);
begin
  fX := fX + X;
  fY := fY + Y;
end;


procedure TSession.New;
begin
  fPieces := TPiecesCollection.Create;

  fDeck := TDeck.Create;

  fIngot := TIngot.Create;
  fIngot.Init;
end;


procedure TSession.Render;
begin
  glRotatef(fPitch, 1, 0, 0);
  glRotatef(fHead, 0, 1, 0);

  glTranslatef(fX, fY, 0);

  fIngot.Render;
end;


procedure TSession.Rotate(X, Y: Single);
begin
  fHead := fHead + X;
  fPitch := fPitch + Y;
end;


end.
