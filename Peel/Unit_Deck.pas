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

    procedure MouseMove(Shift: TShiftState; X, Y: Integer);

    procedure Render;
  end;


implementation


{ TDeck }
constructor TDeck.Create;
begin
  inherited;

end;


destructor TDeck.Destroy;
begin

  inherited;
end;


procedure TDeck.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  I := Round((X - 75) / 75);

  fPieces.Selected := I;
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
      TPiece(fPieces[I]).Render2D;
      glTranslatef(0.5, 0, 0);
    end;

  glPopMatrix;
end;


end.
