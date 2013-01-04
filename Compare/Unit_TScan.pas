unit Unit_TScan;
interface
uses Forms, Classes, StdCtrls, Windows, ComCtrls, SysUtils, DateUtils, KromUtils, KromIOUtils, Masks;

type
  dtDiffType = (dtAdded,dtOlder,dtNewer,dtContent);

type
  TDiffInfo = record
                ScanID:integer;
                ItemID:integer;
                DiffType:dtDiffType;
              end;

type
  TScan = class
  private
    fStopCompare: Boolean;
    fFilters: TStringList;
    fScanPath:array[1..2] of string;
    ScanResult:array[1..2] of record
      CurFolder,CurFile:integer;
      Folders:array of record
        Name:string;
        Time:integer;
        Size:int64; //4GB+
        ParentID:integer;
        SubFolderA,SubFolderZ:integer;
        SubFileA,SubFileZ:integer;
        CorrespondingID:integer;
      end;
      Files:array of record
        Name:string;
        Time:integer;
        Size:int64; //4GB+
        ParentID:integer;
        CorrespondingID:integer;
      end;
    end;
    DiffFolderLen:array[1..2] of integer;
    DiffFolder:array[1..2] of array of TDiffInfo;
    DiffFileLen:array[1..2] of integer;
    DiffFile:array[1..2] of array of TDiffInfo;

    ExludedCount:array[1..2]of integer;

    function IsExludeFileName(ScanID:integer; FileName:string):boolean;
    function GetFullPath(ScanID,FolderID:integer):string;
    function GetRelativePath(ScanID,FolderID:integer):string;
    function GetRelativeFileName(ScanID,FileID:integer):string;
    function GetFolderDateTime(ScanID,FolderID:integer):string;
    function GetFileDateTime(ScanID,FileID:integer):string;
    function GetFolderSize(ScanID,FolderID:integer):string;
    function GetFileSize(ScanID,FileID:integer):string;
    function CheckParentCorrespondID(ScanID,FolderID:integer):integer;
    procedure IncParentFoldersSize(ScanID,FolderID:integer; Size:Int64);
    procedure SearchFolder(ScanID:integer; Path:string; FolderID:integer);
    procedure CompareFolders(ScanID1,ScanID2,FolderID1,FolderID2:integer);
    procedure CompareFiles(ScanID1,ScanID2,FolderID1,FolderID2:integer);

    function ScanPath(Path:string; ScanID:integer; Progress:TLabel; App:TApplication):boolean;
  public
    constructor Create(aFilter: string);
    destructor Destroy; override;

    function ScanPaths(aPath1, aPath2: string; aProgress1, aProgress2: TLabel; aApp: TApplication): Boolean;
    procedure StopCompare;
    procedure FindDifference(ScanID1,ScanID2:integer);
    procedure FillList(ScanID:integer; LV:TListView);
    function GetExludedCount(ScanID:integer):integer;
    function GetPath(ID:integer):string;
  end;


var
  fScan: TScan;


implementation


{ TScan }
constructor TScan.Create(aFilter: string);
begin
  inherited Create;

  fFilters := TStringList.Create;
  fFilters.Delimiter := ';';
  fFilters.StrictDelimiter := True;
  fFilters.DelimitedText := aFilter;
end;


destructor TScan.Destroy;
begin
  fFilters.Free;

  inherited;
end;


{Check wherever filename should be excluded }
function TScan.IsExludeFileName(ScanID: Integer; FileName: string): Boolean;
var I: Integer;
begin
  Result := False;
  FileName := UpperCase(FileName);

  for I := 0 to fFilters.Count - 1 do
  if MatchesMask(FileName, fFilters[I]) then
  begin
    Inc(ExludedCount[ScanID]);
    Result := True;
    Break;
  end;
end;


{Return full Path for given FolderID/ScanID}
function TScan.GetFullPath(ScanID,FolderID:integer):string;
var
  SearchPath: string;
begin
  SearchPath := '';
  while (FolderID <> -1) do
  begin
    SearchPath := ScanResult[ScanID].Folders[FolderID].Name + '\' + SearchPath;
    FolderID := ScanResult[ScanID].Folders[FolderID].ParentID;
  end;
  Result := SearchPath;
end;


{Return Path for given FolderID/ScanID except topmost parent folder}
function TScan.GetRelativePath(ScanID, FolderID: integer): string;
var
  SearchPath: string;
begin
  SearchPath := '';
  while (FolderID <> 0) do
  begin
    SearchPath := ScanResult[ScanID].Folders[FolderID].Name + '\' + SearchPath;
    FolderID := ScanResult[ScanID].Folders[FolderID].ParentID;
  end;
  Result := SearchPath;
end;


{Relative filename including path}
function TScan.GetRelativeFileName(ScanID,FileID:integer):string;
begin
  Result := GetRelativePath(ScanID, ScanResult[ScanID].Files[FileID].ParentID) + ScanResult[ScanID].Files[FileID].Name;
end;


function TScan.GetFolderDateTime(ScanID,FolderID:integer):string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(ScanResult[ScanID].Folders[FolderID].Time));
end;


function TScan.GetFileDateTime(ScanID,FileID:integer):string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(ScanResult[ScanID].Files[FileID].Time));
end;


function TScan.GetFolderSize(ScanID,FolderID:integer):string;
begin
  Result := ReturnSize(ScanResult[ScanID].Folders[FolderID].Size);
end;


function TScan.GetFileSize(ScanID,FileID:integer):string;
begin
  Result := ReturnSize(ScanResult[ScanID].Files[FileID].Size);
end;


function TScan.CheckParentCorrespondID(ScanID,FolderID:integer):integer;
begin
  Result := 0;
  while (FolderID <> 0) do
  begin
    if ScanResult[ScanID].Folders[FolderID].CorrespondingID = -1 then
      Result := -1;
    FolderID := ScanResult[ScanID].Folders[FolderID].ParentID;
  end;
end;


procedure TScan.IncParentFoldersSize(ScanID,FolderID:integer; Size:Int64);
begin
  while (FolderID <> 0) do
  begin
    inc(ScanResult[ScanID].Folders[FolderID].Size, Size);
    FolderID := ScanResult[ScanID].Folders[FolderID].ParentID;
  end;
end;


procedure TScan.SearchFolder(ScanID:integer; Path:string; FolderID:integer);
var SearchRec:TSearchRec; i,k:integer; DT:_FILETIME; ST,STL:_SYSTEMTIME; D:TDateTime;
begin
  FindFirst(Path+'\*', faAnyFile or faDirectory, SearchRec);
  i:=ScanResult[ScanID].CurFolder; k:=ScanResult[ScanID].CurFile;
  ScanResult[ScanID].Folders[FolderID].SubFolderA:=i+1;
  ScanResult[ScanID].Folders[FolderID].SubFileA:=k+1;
  repeat
    if (SearchRec.Name<>'.')and(SearchRec.Name<>'..') then
      if (SearchRec.Attr and faDirectory = faDirectory) then begin
        inc(i);
        if i+1>=length(ScanResult[ScanID].Folders) then
          setlength(ScanResult[ScanID].Folders,i+100);
        ScanResult[ScanID].Folders[i].Name:=SearchRec.Name;
        ScanResult[ScanID].Folders[i].Time:=SearchRec.Time;
        ScanResult[ScanID].Folders[i].ParentID:=FolderID;
      end
      else
      if not IsExludeFileName(ScanID,SearchRec.Name) then begin
        inc(k);
        if k+1>=length(ScanResult[ScanID].Files) then
          setlength(ScanResult[ScanID].Files,k+100);
        ScanResult[ScanID].Files[k].Name:=SearchRec.Name;
        ScanResult[ScanID].Files[k].Time:=SearchRec.Time;
        ScanResult[ScanID].Files[k].Size:=
          Int64(SearchRec.FindData.nFileSizeHigh) shl Int64(32) +
          Int64(SearchRec.FindData.nFileSizeLow);

          {
        FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime,DT);
        FileTimeToSystemTime(DT,ST);
        SystemTimeToTzSpecificLocalTime(nil, ST, STL);
        D := EncodeDateTime(STL.wYear, STL.wMonth, STL.wDay, STL.wHour, STL.wMinute, STL.wSecond, STL.wMilliseconds);
        ScanResult[ScanID].Files[k].Time:=DateTimeToFileDate(D);
          }

        ScanResult[ScanID].Files[k].ParentID:=FolderID;
        IncParentFoldersSize(ScanID,FolderID,ScanResult[ScanID].Files[k].Size);
      end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
  ScanResult[ScanID].Folders[FolderID].SubFolderZ:=i;
  ScanResult[ScanID].Folders[FolderID].SubFileZ:=k;
  ScanResult[ScanID].CurFolder:=i;
  ScanResult[ScanID].CurFile:=k;
end;


procedure TScan.CompareFolders(ScanID1,ScanID2,FolderID1,FolderID2:integer);
var i,k:integer;
begin
  if (ScanResult[ScanID1].Folders[FolderID1].SubFolderA=0)or
     (ScanResult[ScanID2].Folders[FolderID2].SubFolderA=0) then exit;

  for i:=ScanResult[ScanID1].Folders[FolderID1].SubFolderA to
  ScanResult[ScanID1].Folders[FolderID1].SubFolderZ do begin

    ScanResult[ScanID1].Folders[i].CorrespondingID:=-1;
    for k:=ScanResult[ScanID2].Folders[FolderID2].SubFolderA to
           ScanResult[ScanID2].Folders[FolderID2].SubFolderZ do
      if AnsiCompareFileName(ScanResult[ScanID1].Folders[i].Name,ScanResult[ScanID2].Folders[k].Name)=0 then begin
        ScanResult[ScanID1].Folders[i].CorrespondingID:=k;
        break;
      end;

    if ScanResult[ScanID1].Folders[i].Size>0 then
    if ScanResult[ScanID1].Folders[i].CorrespondingID=-1 then begin
      inc(DiffFolderLen[ScanID1]);
      DiffFolder[ScanID1,DiffFolderLen[ScanID1]].ScanID:=ScanID1;
      DiffFolder[ScanID1,DiffFolderLen[ScanID1]].ItemID:=i;
      DiffFolder[ScanID1,DiffFolderLen[ScanID1]].DiffType := dtNewer;
    end;

  end;
end;

procedure TScan.CompareFiles(ScanID1,ScanID2,FolderID1,FolderID2:integer);
var i,k:integer; dt:dtDiffType;
begin
  if (ScanResult[ScanID1].Folders[FolderID1].SubFileA=0)
  or (ScanResult[ScanID2].Folders[FolderID2].SubFileA=0) then exit;

  for i:=ScanResult[ScanID1].Folders[FolderID1].SubFileA to
         ScanResult[ScanID1].Folders[FolderID1].SubFileZ do begin

    dt := dtAdded;
    ScanResult[ScanID1].Files[i].CorrespondingID:=-1;
    for k:=ScanResult[ScanID2].Folders[FolderID2].SubFileA to
           ScanResult[ScanID2].Folders[FolderID2].SubFileZ do
      if UpperCase(ScanResult[ScanID1].Files[i].Name) = UpperCase(ScanResult[ScanID2].Files[k].Name) then begin
        if ScanResult[ScanID1].Files[i].Time = ScanResult[ScanID2].Files[k].Time then
        if ScanResult[ScanID1].Files[i].Size = ScanResult[ScanID2].Files[k].Size then
          ScanResult[ScanID1].Files[i].CorrespondingID:=k;

        if ScanResult[ScanID1].Files[i].Size <> ScanResult[ScanID2].Files[k].Size then
          dt:=dtContent;

        if ScanResult[ScanID1].Files[i].Time > ScanResult[ScanID2].Files[k].Time then
          dt:=dtNewer;

        if ScanResult[ScanID1].Files[i].Time < ScanResult[ScanID2].Files[k].Time then
          dt:=dtOlder;

        break;
      end;

    if ScanResult[ScanID1].Files[i].CorrespondingID=-1 then begin
      inc(DiffFileLen[ScanID1]);
      DiffFile[ScanID1,DiffFileLen[ScanID1]].ScanID:=ScanID1;
      DiffFile[ScanID1,DiffFileLen[ScanID1]].ItemID:=i;
      DiffFile[ScanID1,DiffFileLen[ScanID1]].DiffType:=dt;
    end;

  end;
end;


procedure TScan.FindDifference(ScanID1,ScanID2:integer);
var i:integer;
begin
  DiffFolderLen[ScanID1]:=0;
  FillChar(DiffFolder[ScanID1],sizeof(DiffFolder[ScanID1]),#0);
  setlength(DiffFolder[ScanID1],10000);

  CompareFolders(ScanID1,ScanID2,0,0);

  for i:=1 to ScanResult[ScanID1].CurFolder do
    if CheckParentCorrespondID(ScanID1,i)<>-1 then
      CompareFolders(ScanID1,ScanID2,i,ScanResult[ScanID1].Folders[i].CorrespondingID);

  DiffFileLen[ScanID1] := 0;
  FillChar(DiffFile[ScanID1],sizeof(DiffFile[ScanID1]),#0); //reset
  setlength(DiffFile[ScanID1],10000);

  CompareFiles(ScanID1,ScanID2,0,0);

  for i:=1 to ScanResult[ScanID1].CurFolder do
    if CheckParentCorrespondID(ScanID1,i)<>-1 then
      CompareFiles(ScanID1,ScanID2,i,ScanResult[ScanID1].Folders[i].CorrespondingID);
end;


procedure TScan.FillList(ScanID:integer; LV:TListView);
var i:integer;
begin
  for i:=1 to DiffFolderLen[ScanID] do
  with LV.Items.Add do begin
    Caption := GetRelativePath(ScanID,DiffFolder[ScanID,i].ItemID);
    SubItems.Add(GetFolderDateTime(ScanID,DiffFolder[ScanID,i].ItemID));
    SubItems.Add(GetFolderSize(ScanID,DiffFolder[ScanID,i].ItemID));
    SubItems.Add('A');
    ImageIndex := 0;
  end;

  for i:=1 to DiffFileLen[ScanID] do
  with LV.Items.Add do begin
    Caption := GetRelativeFileName(ScanID,DiffFile[ScanID,i].ItemID);
    SubItems.Add(GetFileDateTime(ScanID,DiffFile[ScanID,i].ItemID));
    SubItems.Add(GetFileSize(ScanID,DiffFile[ScanID,i].ItemID));
    case DiffFile[ScanID,i].DiffType of
      dtAdded:  SubItems.Add('A');
      dtOlder:  SubItems.Add('O');
      dtNewer:  SubItems.Add('N');
      dtContent:SubItems.Add('C');
      else      SubItems.Add('?');
    end;
    ImageIndex := 1;
  end;
end;


function TScan.GetExludedCount(ScanID:integer):integer;
begin
  Result := ExludedCount[ScanID];
end;

function TScan.GetPath(ID:integer):string;
begin
  Result := fScanPath[ID];
end;


function TScan.ScanPath(Path:string; ScanID:integer; Progress:TLabel; App:TApplication):boolean;
var ScanFolder:integer;
begin
  fScanPath[ScanID]:=Path;

  FillChar(ScanResult[ScanID],sizeof(ScanResult[ScanID]),#0);
  setlength(ScanResult[ScanID].Folders,2);
  setlength(ScanResult[ScanID].Files,0);

  ScanFolder:=0;

  // set root path
  ScanResult[ScanID].Folders[0].Name:=Path;
  ScanResult[ScanID].Folders[0].ParentID:=-1;

  repeat
    Progress.Caption:=GetRelativePath(ScanID,ScanFolder);
    Progress.Repaint;
    App.ProcessMessages;
    ///sleep(100);
    if fStopCompare then begin
      Result:=false;
      exit;
    end;

    SearchFolder(ScanID,GetFullPath(ScanID,ScanFolder),ScanFolder);
    inc(ScanFolder);
  until(ScanResult[ScanID].Folders[ScanFolder].Name='');
  Result := True;
end;


function TScan.ScanPaths(aPath1, aPath2: string; aProgress1, aProgress2: TLabel; aApp: TApplication): Boolean;
begin
  Result := ScanPath(aPath1, 1, aProgress1, aApp) and ScanPath(aPath2, 2, aProgress2, aApp);
end;


procedure TScan.StopCompare;
begin
  fStopCompare := true;
end;


end.
