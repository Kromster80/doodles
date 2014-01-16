unit Unit_TDiff;
interface
uses Forms, Classes, StdCtrls, Windows, ComCtrls, SysUtils, DateUtils, KromUtils, KromIOUtils, Masks, Unit_TScan;

type
  dtDiffType = (dtAdded,dtOlder,dtNewer,dtContent);

  TCompareSide = (csLeft, csRight);

  TDiffInfo = record
    ItemID: Integer;
    DiffType: dtDiffType;
  end;

type
  TDiff = class
  private
    fScan1, fScan2: TScan;

    DiffFolderLen: Integer;
    DiffFolder: array of TDiffInfo;
    DiffFileLen: Integer;
    DiffFile: array of TDiffInfo;

    function DiffCheckParentCorrespondID(FolderID:integer):integer;
    procedure DiffFolders(FolderID1, FolderID2: Integer);
    procedure DiffFiles(FolderID1,FolderID2:integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure FindDifference(aScan1, aScan2: TScan);
    procedure FillList(LV:TListView);
  end;


var
  fScan: TDiff;


implementation


{ TDiff }
constructor TDiff.Create;
begin
  inherited Create;

end;


destructor TDiff.Destroy;
begin

  inherited;
end;


function TDiff.DiffCheckParentCorrespondID(FolderID:integer):integer;
begin
  Result := 0;
  while (FolderID <> 0) do
  begin
    if fScan1.Folders[FolderID].CorrespondingID = -1 then
      Result := -1;
    FolderID := fScan1.Folders[FolderID].ParentID;
  end;
end;


procedure TDiff.DiffFolders(FolderID1, FolderID2: Integer);
var i,k:integer;
begin
  if (fScan1.Folders[FolderID1].SubFolderA=0)or
     (fScan2.Folders[FolderID2].SubFolderA=0) then exit;

  for i:=fScan1.Folders[FolderID1].SubFolderA to
  fScan1.Folders[FolderID1].SubFolderZ do begin

    fScan1.Folders[i].CorrespondingID:=-1;
    for k:=fScan2.Folders[FolderID2].SubFolderA to
           fScan2.Folders[FolderID2].SubFolderZ do
      if AnsiCompareFileName(fScan1.Folders[i].Name, fScan2.Folders[k].Name)=0 then begin
        fScan1.Folders[i].CorrespondingID:=k;
        break;
      end;

    if fScan1.Folders[i].Size>0 then
    if fScan1.Folders[i].CorrespondingID=-1 then begin
      inc(DiffFolderLen);
      DiffFolder[DiffFolderLen].ItemID:=i;
      DiffFolder[DiffFolderLen].DiffType := dtNewer;
    end;

  end;
end;

procedure TDiff.DiffFiles(FolderID1,FolderID2:integer);
var i,k:integer; dt:dtDiffType;
begin
  if (fScan1.Folders[FolderID1].SubFileA=0)
  or (fScan2.Folders[FolderID2].SubFileA=0) then exit;

  for i:=fScan1.Folders[FolderID1].SubFileA to
         fScan1.Folders[FolderID1].SubFileZ do begin

    dt := dtAdded;
    fScan1.Files[i].CorrespondingID:=-1;
    for k:=fScan2.Folders[FolderID2].SubFileA to
           fScan2.Folders[FolderID2].SubFileZ do
      if UpperCase(fScan1.Files[i].Name) = UpperCase(fScan2.Files[k].Name) then begin
        if fScan1.Files[i].Time = fScan2.Files[k].Time then
        if fScan1.Files[i].Size = fScan2.Files[k].Size then
          fScan1.Files[i].CorrespondingID:=k;

        if fScan1.Files[i].Size <> fScan2.Files[k].Size then
          dt:=dtContent;

        if fScan1.Files[i].Time > fScan2.Files[k].Time then
          dt:=dtNewer;

        if fScan1.Files[i].Time < fScan2.Files[k].Time then
          dt:=dtOlder;

        break;
      end;

    if fScan1.Files[i].CorrespondingID=-1 then begin
      DiffFile[DiffFileLen].ItemID:=i;
      DiffFile[DiffFileLen].DiffType:=dt;
    end;

  end;
end;


procedure TDiff.FindDifference(aScan1, aScan2: TScan);
var i:integer;
begin
  DiffFolderLen:=0;
  FillChar(DiffFolder,sizeof(DiffFolder),#0);
  setlength(DiffFolder,10000);

  DiffFolders(0,0);

  for i:=1 to fScan1.CurFolder do
    if DiffCheckParentCorrespondID(i)<>-1 then
      DiffFolders(i,fScan1.Folders[i].CorrespondingID);

  DiffFileLen := 0;
  FillChar(DiffFile,sizeof(DiffFile),#0); //reset
  setlength(DiffFile,10000);

  DiffFiles(0,0);

  for i:=1 to fScan1.CurFolder do
    if DiffCheckParentCorrespondID(i)<>-1 then
      DiffFiles(i,fScan1.Folders[i].CorrespondingID);
end;


procedure TDiff.FillList(LV:TListView);
var i:integer;
begin
  for i:=1 to DiffFolderLen do
  with LV.Items.Add do begin
    Caption := fScan1.GetRelativePath(DiffFolder[i].ItemID);
    SubItems.Add(fScan1.GetFolderDateTime(DiffFolder[i].ItemID));
    SubItems.Add(fScan1.GetFolderSize(DiffFolder[i].ItemID));
    SubItems.Add('A');
    ImageIndex := 0;
  end;

  for i:=1 to DiffFileLen do
  with LV.Items.Add do begin
    Caption := fScan1.GetRelativeFileName(DiffFile[i].ItemID);
    SubItems.Add(fScan1.GetFileDateTime(DiffFile[i].ItemID));
    SubItems.Add(fScan1.GetFileSize(DiffFile[i].ItemID));
    case DiffFile[i].DiffType of
      dtAdded:  SubItems.Add('A');
      dtOlder:  SubItems.Add('O');
      dtNewer:  SubItems.Add('N');
      dtContent:SubItems.Add('C');
      else      SubItems.Add('?');
    end;
    ImageIndex := 1;
  end;
end;


end.
