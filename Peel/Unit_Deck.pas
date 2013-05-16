unit Unit_Deck;
interface
uses Classes, Controls, dglOpenGL,
  Unit_Controls, Unit_Pieces;


type
  TDeck = class
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure PiecePick(aPiece: TPiece);

    procedure Render;
  end;

var
  fDeck: TDeck;

implementation
uses Unit_Cursor, Unit_Defaults, Unit_ColorCoder, Unit_Render;


{ TDeck }
constructor TDeck.Create;
begin
  inherited;

end;


destructor TDeck.Destroy;
begin

  inherited;
end;


//Piece has been picked, rearrange other pieces
procedure TDeck.PiecePick(aPiece: TPiece);
begin
  //
end;


procedure TDeck.Render;
var
  I: Integer;
begin
  glPushMatrix;

    glScalef(150, 150, 150);
    glTranslatef(0.5, 0, 0);

    for I := 0 to fPieces.Count - 1 do
    if fPieces[I].Location = plDeck then
    begin
      glPushMatrix;
        glTranslatef(fPieces[I].DeckPosition.X, fPieces[I].DeckPosition.Y, 0);
        fPieces[I].Render2D;
      glPopMatrix;
    end;

  glPopMatrix;
end;


end.
