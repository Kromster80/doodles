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

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);

    procedure Render;
  end;


implementation
uses Unit_Defaults, Unit_ColorCoder;


{ TDeck }
constructor TDeck.Create;
begin
  inherited;

end;


destructor TDeck.Destroy;
begin

  inherited;
end;


procedure TDeck.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Piece: TPiece;
begin
  if fCursor.Code.Code = ccPiece then
  begin
    Piece := fPieces.PieceById(fCursor.Code.Id);
    Piece.Location := plCursor;
  end;

  todo:
end;


procedure TDeck.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  if Shift = [] then
  begin
    I := Round((X - 75) / 75);
    fPieces.Selected := I;
  end;
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
