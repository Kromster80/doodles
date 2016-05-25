unit Unit_TDiff;
interface
uses
  Forms, Classes, StdCtrls, Windows, ComCtrls, SysUtils, DateUtils, KromUtils, KromIOUtils, Masks, Unit_TScan;

type
  dtDiffType = (dtAdded, dtOlder, dtNewer, dtContent);

  TDiffInfo = record
    ItemID: Integer;
    DiffType: dtDiffType;
  end;

  TDiff = class
  private
    fScan1, fScan2: TScan;

    fCountFolders: Integer;
    fCountFiles: Integer;

    function DiffCheckParentCorrespondID(aFolderID: Integer): Integer;
    procedure DiffFolders(aFolderID1, aFolderID2: Integer);
    procedure DiffFiles(aFolderID1, aFolderID2: Integer);
  public
    Folders: array of TDiffInfo;
    Files: array of TDiffInfo;

    property CountFolders: Integer read fCountFolders;
    property CountFiles: Integer read fCountFiles;
    property Scan: TScan read fScan1;

    procedure FindDifference(aScan1, aScan2: TScan);
  end;


implementation


{ TDiff }
function TDiff.DiffCheckParentCorrespondID(aFolderID: Integer): Integer;
begin
  Result := 0;
  while (aFolderID <> 0) do
  begin
    if fScan1.Folders[aFolderID].CorrespondingID = -1 then
      Result := -1;
    aFolderID := fScan1.Folders[aFolderID].ParentID;
  end;
end;


procedure TDiff.DiffFolders(aFolderID1, aFolderID2: Integer);
var
  I,K: Integer;
begin
  if (fScan1.Folders[aFolderID1].SubFolderA = 0)or
     (fScan2.Folders[aFolderID2].SubFolderA = 0) then exit;

  for I := fScan1.Folders[aFolderID1].SubFolderA to fScan1.Folders[aFolderID1].SubFolderZ do
  begin
    fScan1.Folders[I].CorrespondingID := -1;
    for K := fScan2.Folders[aFolderID2].SubFolderA to fScan2.Folders[aFolderID2].SubFolderZ do
      if AnsiCompareFileName(fScan1.Folders[I].Name, fScan2.Folders[K].Name) = 0 then
      begin
        fScan1.Folders[I].CorrespondingID := K;
        Break;
      end;

    if (fScan1.Folders[I].Size > 0) and (fScan1.Folders[I].CorrespondingID = -1) then
    begin
      Inc(fCountFolders);
      Folders[fCountFolders].ItemID := I;
      Folders[fCountFolders].DiffType := dtAdded;
    end;
  end;
end;


procedure TDiff.DiffFiles(aFolderID1, aFolderID2: Integer);
var
  I, K: Integer;
  dt: dtDiffType;
begin
  if (fScan1.Folders[aFolderID1].SubFileA = 0)
  or (fScan2.Folders[aFolderID2].SubFileA = 0) then
    Exit;

  for I := fScan1.Folders[aFolderID1].SubFileA to fScan1.Folders[aFolderID1].SubFileZ do
  begin
    dt := dtAdded;
    fScan1.Files[I].CorrespondingID := -1;
    for K := fScan2.Folders[aFolderID2].SubFileA to fScan2.Folders[aFolderID2].SubFileZ do
      if UpperCase(fScan1.Files[I].Name) = UpperCase(fScan2.Files[K].Name) then
      begin
        if fScan1.Files[I].FileTime > fScan2.Files[K].FileTime then
          dt := dtNewer
        else
        if fScan1.Files[I].FileTime < fScan2.Files[K].FileTime then
          dt := dtOlder
        else
        if fScan1.Files[I].Size <> fScan2.Files[K].Size then
          dt := dtContent
        else
          // Files do match in size in time
          fScan1.Files[I].CorrespondingID := K;

        Break;
      end;

    // No such file or files don't match
    if fScan1.Files[I].CorrespondingID = -1 then
    begin
      Inc(fCountFiles);
      Files[fCountFiles].ItemID := I;
      Files[fCountFiles].DiffType := dt;
    end;
  end;
end;


procedure TDiff.FindDifference(aScan1, aScan2: TScan);
var
  I: integer;
begin
  fScan1 := aScan1;
  fScan2 := aScan2;

  fCountFolders := 0;
  FillChar(Folders, sizeof(Folders),#0);
  SetLength(Folders, 10000);

  DiffFolders(0, 0);

  for I := 1 to fScan1.CountFolders do
    if DiffCheckParentCorrespondID(I) <> -1 then
      DiffFolders(I, fScan1.Folders[I].CorrespondingID);

  fCountFiles := 0;
  FillChar(Files, sizeof(Files),#0); //reset
  setlength(Files, 10000);

  DiffFiles(0, 0);

  for I := 1 to fScan1.CountFolders do
    if DiffCheckParentCorrespondID(I) <> -1 then
      DiffFiles(I, fScan1.Folders[I].CorrespondingID);
end;


end.
