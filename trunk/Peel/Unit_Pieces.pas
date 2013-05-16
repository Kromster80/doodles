unit Unit_Pieces;
interface
uses Classes, dglOpenGL, Math, Unit_Vector;


type
  TPieceLoc = (plDeck, plCursor, plIngot);

  TPiece = class
  private
    fVerts: array of TVertice;
    fPolys: array of TPoly3;
    fVtxBuf: GLuint;
    fIndBuf: GLuint;

    //Where the piece should be located on Ingot
    fGoal: TVector3f;
  public
    Id: Integer;
    Location: TPieceLoc;
    Selected: Boolean;
    DeckPosition: TVector2i;
    constructor Create(aVtxBuf, aIndBuf: GLuint);
    procedure Init;

    procedure Render2D;
    procedure Render3D;
  end;

  TPiecesCollection = class
  private
    fPieces: TList;
    fSelected: Integer;
    function GetCount: Integer;
    function GetPiece(aIndex: Integer): TPiece;
    procedure SetSelected(aValue: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Selected: Integer read fSelected write SetSelected;
    property Pieces[aIndex: Integer]: TPiece read GetPiece; default;
    function PieceById(aIndex: Integer): TPiece;
    procedure LoadFromFile(aFilename: string);
  end;


var
  fPieces: TPiecesCollection;


implementation
uses
  Unit_Render, Unit_ColorCoder;


{ TPiece }
constructor TPiece.Create(aVtxBuf, aIndBuf: GLuint);
begin
  inherited Create;

  fVtxBuf := aVtxBuf;
  fIndBuf := aIndBuf;
end;


procedure TPiece.Init;
begin
  glBindBuffer(GL_ARRAY_BUFFER, fVtxBuf);
  glBufferData(GL_ARRAY_BUFFER, Length(fVerts) * SizeOf(TVertice), @fVerts[0].X, GL_STREAM_DRAW);

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndBuf);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(fPolys) * SizeOf(fPolys[0]), @fPolys[0,0], GL_STREAM_DRAW);
end;


//Render piece on the Deck
procedure TPiece.Render2D;
  procedure SetRenderColor;
  begin
    if fRender.IsNormal then
    begin
      if Selected then
        glColor3f(1,1,1)
      else
        glColor3f(0.5, 0.6, 0.5);
    end
    else
    begin
      SetColorCode(ccPiece, Id);
    end;
  end;
begin
  SetRenderColor;

  glBindBuffer(GL_ARRAY_BUFFER, fVtxBuf);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, fIndBuf);

  //Setup vertex and UV layout and offsets
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertice), Pointer(0));

  if fRender.IsNormal then
  begin
    glEnableClientState(GL_NORMAL_ARRAY);
    glNormalPointer(GL_FLOAT, SizeOf(TVertice), Pointer(12));
  end;

  //Here and above OGL requests Pointer, but in fact it's just a number (offset within Array)
  glDrawElements(GL_TRIANGLES, Length(fPolys) * Length(fPolys[0]), GL_UNSIGNED_INT, Pointer(0));

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
end;


//Render piece on the Ingot
procedure TPiece.Render3D;
begin

end;


{ TPiecesCollection }
constructor TPiecesCollection.Create;
begin
  fPieces := TList.Create;

  fSelected := -1;
end;


destructor TPiecesCollection.Destroy;
begin
  fPieces.Free;

  inherited;
end;


function TPiecesCollection.GetCount: Integer;
begin
  Result := fPieces.Count;
end;


function TPiecesCollection.GetPiece(aIndex: Integer): TPiece;
begin
  Result := fPieces[aIndex];
end;


procedure TPiecesCollection.LoadFromFile(aFilename: string);
var
  I: Integer;
  NewCount: Integer;
  VtxBuf: array of GLuint;
  IndBuf: array of GLuint;
  NewPiece: TPiece;
begin
  NewCount := 10;

  //Generate all buffers in batch
  SetLength(VtxBuf, NewCount);
  SetLength(IndBuf, NewCount);
  glGenBuffers(NewCount, @VtxBuf[0]);
  glGenBuffers(NewCount, @IndBuf[0]);

  for I := 0 to NewCount - 1 do
  begin
    NewPiece := TPiece.Create(VtxBuf[I], IndBuf[I]);
    NewPiece.Id := I;
    NewPiece.fGoal := Vector3(0.5, 0, 0);

    SetLength(NewPiece.fVerts, 4);
    NewPiece.fVerts[0] := Vertice(-0.1,  0.1, 0, -0.1,  0.1, 1);
    NewPiece.fVerts[1] := Vertice( 0.1,  0.1, 0,  0.1,  0.1, 1);
    NewPiece.fVerts[2] := Vertice( 0.1, -0.1, 0,  0.1, -0.1, 1);
    NewPiece.fVerts[3] := Vertice(-0.1, -0.1, 0, -0.1, -0.1, 1);
    SetLength(NewPiece.fPolys, 2);
    NewPiece.fPolys[0] := Poly3(0, 2, 1);
    NewPiece.fPolys[1] := Poly3(0, 2, 3);

    NewPiece.DeckPosition := Vector2i(I, 0);

    NewPiece.Init;
    fPieces.Add(NewPiece);
  end;
end;


function TPiecesCollection.PieceById(aIndex: Integer): TPiece;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  if Pieces[I].Id = aIndex then
  begin
    Result := Pieces[I];
    Break;
  end;
end;

procedure TPiecesCollection.SetSelected(aValue: Integer);
begin
  //Deselect previous
  if InRange(fSelected, 0, Count - 1) then
    Pieces[fSelected].Selected := False;

  fSelected := aValue;

  //Select current
  if InRange(fSelected, 0, Count - 1) then
    Pieces[fSelected].Selected := True;
end;


end.
