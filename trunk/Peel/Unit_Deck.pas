unit Unit_Deck;
interface
uses Classes, dglOpenGL, Unit_Pieces;


type
  TDeck = class
  private
    fList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;
  end;


implementation


{ TDeck }
constructor TDeck.Create;
begin
  inherited;

  fList := TList.Create;
end;


destructor TDeck.Destroy;
begin
  fList.Free;

  inherited;
end;


procedure TDeck.Render;
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
  begin
    TPiece(fList[I]).Render2D;
    glTranslatef(100, 0, 0);
  end;
end;


end.
