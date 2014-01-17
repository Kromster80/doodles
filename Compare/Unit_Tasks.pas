unit Unit_Tasks;
interface
uses Classes, Generics.Collections, SysUtils, XMLIntf;


type
  TPair = record
    A,B: string;
    Checked: Boolean;
  end;

  TTask = class
    fPaths: TList<TPair>;
    fFilterFiles: string;
    fFilterFolders: string;
  private
    function GetCount: Integer;
    function GetPath(aIndex: Integer): TPair;
    procedure SetPath(aIndex: Integer; const Value: TPair);
  public
    Title: string;

    constructor Create;
    destructor Destroy; override;

    procedure AddPaths(A,B: string);
    property Count: Integer read GetCount;
    procedure Delete(aIndex: Integer);
    property FilterFiles: string read fFilterFiles;
    property FilterFolders: string read fFilterFolders;
    property Paths[aIndex: Integer]: TPair read GetPath write SetPath;
    procedure LoadFromXML(aNode: IXMLNode);
    procedure SaveToXML(aNode: IXMLNode);
  end;


  TTasks = class(TList<TTask>)
  public
    procedure LoadFromXML(aNode: IXMLNode);
    procedure SaveToXML(aNode: IXMLNode);
  end;


implementation


{ TTask }
constructor TTask.Create;
begin
  inherited;

  fPaths := TList<TPair>.Create;
end;


destructor TTask.Destroy;
begin
  fPaths.Free;

  inherited;
end;


function TTask.GetCount: Integer;
begin
  Result := fPaths.Count;
end;


function TTask.GetPath(aIndex: Integer): TPair;
begin
  Result := fPaths[aIndex];
end;


procedure TTask.SetPath(aIndex: Integer; const Value: TPair);
begin
  fPaths[aIndex] := Value;
end;


procedure TTask.AddPaths(A,B: string);
var
  Pair: TPair;
begin
  Pair.A := A;
  Pair.B := B;
  fPaths.Add(Pair);
end;


procedure TTask.Delete(aIndex: Integer);
begin
  fPaths.Delete(aIndex);
end;


procedure TTask.LoadFromXML(aNode: IXMLNode);
var
  NPaths, NFilters: IXMLNode;
  I: Integer;
  Pair: TPair;
begin
  fPaths.Clear;

  Title := aNode.Attributes['Title'];

  NPaths := aNode.ChildNodes['Paths'];
  for I := 0 to NPaths.ChildNodes.Count - 1 do
  begin
    Pair.A := NPaths.ChildNodes[I].Attributes['From'];
    Pair.B := NPaths.ChildNodes[I].Attributes['To'];
    Pair.Checked := NPaths.ChildNodes[I].HasAttribute('Checked') and NPaths.ChildNodes[I].Attributes['Checked'];
    fPaths.Add(Pair);
  end;

  NFilters := aNode.ChildNodes['Filters'];
  fFilterFiles := NFilters.Attributes['ExcludeFiles'];
  fFilterFolders := NFilters.Attributes['ExcludeFolders'];
end;


procedure TTask.SaveToXML(aNode: IXMLNode);
var
  N, NPaths, NFilters: IXMLNode;
  I: Integer;
begin
  aNode.Attributes['Title'] := Title;

  NPaths := aNode.AddChild('Paths');
  for I := 0 to fPaths.Count - 1 do
  begin
    N := NPaths.AddChild('Path' + IntToStr(I));
    N.Attributes['From'] := fPaths[I].A;
    N.Attributes['To'] := fPaths[I].B;
    N.Attributes['Chcked'] := fPaths[I].Checked;
  end;

  NFilters := aNode.AddChild('Filters');
  NFilters.Attributes['ExcludeFiles'] := fFilterFiles;
  NFilters.Attributes['ExcludeFolders'] := fFilterFolders;
end;


{ TTasks }
procedure TTasks.LoadFromXML(aNode: IXMLNode);
var
  I: Integer;
  T: TTask;
begin
  for I := 0 to aNode.ChildNodes.Count - 1 do
  begin
    T := TTask.Create;
    T.LoadFromXML(aNode.ChildNodes[I]);
    Add(T);
  end;
end;


procedure TTasks.SaveToXML(aNode: IXMLNode);
var
  I: Integer;
  N: IXMLNode;
begin
  for I := 0 to Count - 1 do
  begin
    N := aNode.AddChild('t' + IntToStr(I));
    Items[I].SaveToXML(N);
  end;
end;


end.
