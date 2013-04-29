unit Unit_Pieces;
interface
uses dglOpenGL;


type
  TPiece = class
    procedure Render2D;
  end;

  TPiecesCollection = class
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation


{ TPiece }
procedure TPiece.Render2D;
begin
  //
end;


{ TPiecesCollection }
constructor TPiecesCollection.Create;
begin

end;


destructor TPiecesCollection.Destroy;
begin

  inherited;
end;


end.
