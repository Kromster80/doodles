unit Unit_Cursor;
interface
uses Classes, Controls, dglOpenGL,
  Unit_ColorCoder, Unit_Controls, Unit_Pieces;


type
  TMouseArea = (maIngot, maDeck);

  //Ingame cursor that can grap pieces and move them around and across Ingot
  TPCursor = class
  private
    fMouseArea: TMouseArea;
    PrevX, PrevY: Single;
    fPickedPiece: TPiece;
  public
    X: Integer;
    Y: Integer;
    CodeBelow: TColorCodeId;
    constructor Create;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);

    procedure Render;
  end;


var
  fCursor: TPCursor;


implementation
uses Unit_Defaults, Unit_Deck, Unit_Ingot, Unit_Session, Unit_Render;


{ TCursor }
constructor TPCursor.Create;
begin
  inherited;

end;


destructor TPCursor.Destroy;
begin

  inherited;
end;


procedure TPCursor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PrevX := X;
  PrevY := Y;
  if Y < fRender.Height - DECK_HEIGHT then
    fMouseArea := maIngot
  else
    fMouseArea := maDeck;

  case fMouseArea of
    maIngot:  ;//fIngot.MouseDown;
    maDeck:   if CodeBelow.Code = ccPiece then
              begin
                fPickedPiece := fPieces.PieceById(CodeBelow.Id);
                fPickedPiece.Location := plCursor;

                fDeck.PiecePick(fPickedPiece);
              end;
  end;
end;


procedure TPCursor.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  X := X;
  Y := Y;
  CodeBelow := fRender.CodeBelow(X, Y);

  if not (ssLeft in Shift) then
    if Y < fRender.Height - DECK_HEIGHT then
      fMouseArea := maIngot
    else
      fMouseArea := maDeck;

  case fMouseArea of
    maIngot:  if (ssLeft in Shift) and (CodeBelow.Code in [ccNone, ccIngot]) then
                fIngot.Rotate(-(PrevX - X)/1, -(PrevY - Y)/1);
    maDeck:   if Shift = [] then
              begin
                if CodeBelow.Code = ccPiece then
                  fPieces.Selected := CodeBelow.Id
                else
                  fPieces.Selected := -1;
              end;
  end;

  PrevX := X;
  PrevY := Y;
end;


procedure TPCursor.Render;
var
  I: Integer;
begin
  glPushMatrix;

    glScalef(150, 150, 150);
    glTranslatef(0.5, 0, 0);

    if fPickedPiece <> nil then
    begin
      glTranslatef(X, Y, 0);
      fPickedPiece.Render2D;
    end;

  glPopMatrix;
end;


end.
