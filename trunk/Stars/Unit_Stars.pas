unit Unit_Stars;
interface
uses Classes, Controls, KromUtils, Math, Windows, SysUtils;

type
  TVector2f = record X,Y: Single; end;
  TColor4f = record R,G,B,A: Single; end;

  TNodeType = (stStar, stPlanet, stSatellite);

  TNode = class
  private
    fPos: TVector2f;
    fTarget: TVector2f;
    fTargetTime: Int64;
    fAnimLength: Word;
    function GetPos: TVector2f;
  public
    Size: Single;
    Color: TColor4f;
    Level: Byte;
    NodeType: TNodeType;
    constructor Create(X,Y: Single);
    property Pos: TVector2f read GetPos;
    procedure SetTarget(X,Y: Single; aAnimLength: Word);
  end;

  TLink = class
  public
    ID1, ID2: Cardinal;
    Active: Boolean;
  end;

  TNodeList = class
  private
    fNodes: TList;
    function GetNode(aIndex: Integer): TNode;
    function GetCount: Integer;
  public
    constructor Create;
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TNode read GetNode; default;
    procedure Add(X,Y: Single; aNodeType: TNodeType); overload;
    procedure Add(aNode: TNode); overload;
  end;

  TLinkList = class
  private
    fLinks: TList;
    function GetLink(aIndex: Integer): TLink;
    function GetCount: Integer;
  public
    constructor Create;
    property Count: Integer read GetCount;
    property Items[aIndex: Integer]: TLink read GetLink; default;
    procedure Add(ID1, ID2: Integer);
  end;


  TPointF = record
    X,Y:single;
  end;

  TStars = class
  private
    fNodes: TNodeList;
    fLinks: TLinkList;
    fSelect: Integer;
  public
    constructor Create;
    property Nodes: TNodeList read fNodes;
    property Links: TLinkList read fLinks;
    procedure Select(ID: Integer); overload;
    procedure Select(X, Y :Integer); overload;
    procedure Move(X, Y :Integer);
    procedure Reset;
    procedure Order;
    procedure Update;
  end;


  function PointF(X,Y:single):TPointF;

var
  fStars: TStars;


implementation
uses Unit1;


function PointF(X,Y:single):TPointF;
begin
  Result.X := X;
  Result.Y := Y;
end;


{ TNodeList }
procedure TNodeList.Add(X,Y: Single; aNodeType: TNodeType);
var N: TNode;
begin
  N := TNode.Create(X,Y);

  N.Color.R := 0.9;
  N.Color.G := 0.9;
  N.Color.B := 0.9;
  N.Color.A := 1;

  N.Size := 10;

  N.NodeType := aNodeType;

  fNodes.Add(N);
end;

procedure TNodeList.Add(aNode: TNode);
begin
  fNodes.Add(aNode);
end;

constructor TNodeList.Create;
begin
  Inherited;
  fNodes := TList.Create;
end;

function TNodeList.GetCount: Integer;
begin
  Result := fNodes.Count;
end;

function TNodeList.GetNode(aIndex: Integer): TNode;
begin
  Result := fNodes[aindex];
end;


{ TLinkList }
procedure TLinkList.Add(ID1, ID2: Integer);
var L: TLink;
begin
  if ID1 = ID2 then Exit;

  L := TLink.Create;
  L.ID1 := ID1;
  L.ID2 := ID2;
  fLinks.Add(L);
end;

constructor TLinkList.Create;
begin
  Inherited;
  fLinks := TList.Create;
end;

function TLinkList.GetCount: Integer;
begin
  Result := fLinks.Count;
end;

function TLinkList.GetLink(aIndex: Integer): TLink;
begin
  Result := fLinks[aindex];
end;

{ TStars }
constructor TStars.Create;
var i:integer;
begin
  Inherited;
  fNodes := TNodeList.Create;
  fLinks := TLinkList.Create;
end;

procedure TStars.Select(ID: Integer);
var i: Integer;
begin
  if not InRange(ID, 0, fNodes.Count-1) then Exit;

  for i:=0 to fNodes.Count-1 do
  begin
    fNodes[i].Color.R := 0.9;
    fNodes[i].Color.G := 0.9;
    fNodes[i].Color.B := 0.9;
    fNodes[i].Color.A := 1;
  end;

  fSelect := ID;

  fNodes[fSelect].Color.R := 1;
  fNodes[fSelect].Color.G := 0;
  fNodes[fSelect].Color.B := 0;
  fNodes[fSelect].Color.A := 1;

  Order;
end;

procedure TStars.Select(X, Y: Integer);
var i: Integer;
begin
  for i:=0 to fNodes.Count-1 do
  if GetLength(fNodes[i].Pos.X - X, fNodes[i].Pos.Y - Y) <= 7 then
  begin
    Select(i);
    Exit;
  end;
end;

procedure TStars.Move(X, Y: Integer);
begin
  //if fSelect = -1 then Exit;
  //fNodes[fSelect].SetTarget(X, Y, 250);
end;

procedure TStars.Reset;
var i: integer;
begin
  //Reset and randomize
  for i:=0 to fNodes.Count-1 do
  begin
    fNodes[i].Level := 6; //Out of range
    fNodes[fSelect].Color.R := 1;
    fNodes[fSelect].Color.G := 1;
    fNodes[fSelect].Color.B := 1;
    fNodes[fSelect].Color.A := 0.5;
    fNodes[fSelect].SetTarget(Random(300)-150, Random(300)-150, 1200);
  end;

  for i:=0 to fLinks.Count-1 do
    fLinks[i].Active := False;
end;

procedure TStars.Order;
const
  Levels: array [0..5] of Single = (1, 0.85, 0.65, 0.45, 0.2, 0.0);

  //Assign all nodes to levels
  procedure Waterfall(ID: Integer; L: Byte);
  var
    i: Integer;
  begin
    if L > 5 then Exit;

    if L < fNodes[ID].Level then
    begin
      fNodes[ID].Level := L;

      for i:=0 to fLinks.Count-1 do
      begin
        fLinks[i].Active := fLinks[i].Active or (fLinks[i].ID1 = ID) or (fLinks[i].ID2 = ID);

        if fLinks[i].ID1 = ID then
          Waterfall(fLinks[i].ID2, L + 1);

        if fLinks[i].ID2 = ID then
          Waterfall(fLinks[i].ID1, L + 1);
      end;
    end;
  end;

var
  i: Integer;
  Ang, D: Single;
  Cluster: TNodeList;
begin
  Reset;

  if fSelect = -1 then Exit;

  //Select cluster
  Waterfall(fSelect, 0);

  Cluster := TNodeList.Create;

  for i:=0 to fNodes.Count-1 do
  if fNodes[i].Level < 6 then
    Cluster.Add(fNodes[i])
  else
  begin
    D := 1 + Random / 5;
    fNodes[i].Color.A := 1.25 - D;
    D := D * 300;
    Ang := Random(360);
    fNodes[i].SetTarget(Cos(Ang/180*pi) * D, Sin(Ang/180*pi) * D, 1200);
  end;

  for i:=0 to Cluster.Count - 1 do
  begin
    Cluster[i].Color.R := 0.5;
    Cluster[i].Color.G := 0.5;
    Cluster[i].Color.B := 1;

    D := sqrt(Cluster[i].Level / 6);

    Cluster[i].Color.A := 1.25 - D;

    D := D * 300;
    Ang := Random(360);
    Cluster[i].SetTarget(Cos(Ang/180*pi) * D, Sin(Ang/180*pi) * D, 1200);
  end;

  fNodes[fSelect].SetTarget(0, 0, 1200);
end;

procedure TStars.Update;
begin
  //
end;


{ TNode }
constructor TNode.Create(X,Y: Single);
begin
  Inherited Create;
  fPos.X := X;
  fPos.Y := Y;
  fTarget := fPos;
end;

function TNode.GetPos: TVector2f;
var D: Single;
begin
  if (fTargetTime = 0) or (fAnimLength = 0) then
    Result := fPos
  else
  begin
    D := Min(1 - (fTargetTime - GetTickCount) / fAnimLength, 1);

    D := Sin(D/2 * pi);

    Result.X := Lerp(fPos.X, fTarget.X, D);
    Result.Y := Lerp(fPos.Y, fTarget.Y, D);

    if D >= 1 then
    begin
      fTargetTime := 0;
      fAnimLength := 0;
      fPos := fTarget;
    end;
  end;
end;

procedure TNode.SetTarget(X,Y: Single; aAnimLength: Word);
begin
  fTarget.X := X;
  fTarget.Y := Y;
  fAnimLength := aAnimLength;
  fTargetTime := GetTickCount + fAnimLength;
end;

end.
