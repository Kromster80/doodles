unit Unit_TScan;
interface
uses Forms, Classes, StdCtrls, Windows, ComCtrls, SysUtils, DateUtils, KromUtils, KromIOUtils, Masks;

type
  TCompareFolder = record
    Name: string;
    Time: Integer;
    Size: Int64; //4GB+
    ParentID: Integer;
    SubFolderA, SubFolderZ: Integer;
    SubFileA, SubFileZ: Integer;
    CorrespondingID: Integer;
  end;

  TCompareFile = record
    Name: string;
    Time: Integer;
    Size: Int64; //4GB+
    ParentID: Integer;
    CorrespondingID: Integer;
  end;

type
  TScan = class
  private
    fStopCompare: Boolean;
    fFilters: TStringList;
    fScanPath: string;

    fExludedCount: Integer;

  public
    CurFolder, CurFile: Integer;
    Folders: array of TCompareFolder;
    Files: array of TCompareFile;

    constructor Create(aFilter: string);
    destructor Destroy; override;

    function IsExludeFileName(FileName:string):boolean;
    function GetFullPath(FolderID:integer):string;
    function GetRelativePath(FolderID:integer):string;
    function GetRelativeFileName(FileID:integer):string;
    function GetFolderDateTime(FolderID:integer):string;
    function GetFileDateTime(FileID:integer):string;
    function GetFolderSize(FolderID:integer):string;
    function GetFileSize(FileID:integer):string;
    procedure IncParentFoldersSize(FolderID:integer; Size:Int64);
    procedure SearchFolder(Path:string; FolderID:integer);
    function ScanPath(aPath:string; aProgress:TLabel; aApp:TApplication):boolean;

    procedure Clear;
    function ScanPaths(aPath1: string; aProgress1: TLabel; aApp: TApplication): Boolean;
    procedure StopCompare;
    property ExludedCount: Integer read fExludedCount;
    property Path: string read fScanPath;
  end;


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
function TScan.IsExludeFileName(FileName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  FileName := UpperCase(FileName);

  for I := 0 to fFilters.Count - 1 do
  if MatchesMask(FileName, fFilters[I]) then
  begin
    Inc(fExludedCount);
    Result := True;
    Break;
  end;
end;


{Return full Path for given FolderID/ScanID}
function TScan.GetFullPath(FolderID: Integer):string;
var
  SearchPath: string;
begin
  SearchPath := '';
  while (FolderID <> -1) do
  begin
    SearchPath := Folders[FolderID].Name + '\' + SearchPath;
    FolderID := Folders[FolderID].ParentID;
  end;
  Result := SearchPath;
end;


{Return Path for given FolderID/ScanID except topmost parent folder}
function TScan.GetRelativePath(FolderID: integer): string;
var
  SearchPath: string;
begin
  SearchPath := '';
  while (FolderID <> 0) do
  begin
    SearchPath := Folders[FolderID].Name + '\' + SearchPath;
    FolderID := Folders[FolderID].ParentID;
  end;
  Result := SearchPath;
end;


{Relative filename including path}
function TScan.GetRelativeFileName(FileID:integer):string;
begin
  Result := GetRelativePath(Files[FileID].ParentID) + Files[FileID].Name;
end;


function TScan.GetFolderDateTime(FolderID:integer):string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(Folders[FolderID].Time));
end;


function TScan.GetFileDateTime(FileID:integer):string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(Files[FileID].Time));
end;


function TScan.GetFolderSize(FolderID:integer):string;
begin
  Result := ReturnSize(Folders[FolderID].Size);
end;


function TScan.GetFileSize(FileID:integer):string;
begin
  Result := ReturnSize(Files[FileID].Size);
end;


procedure TScan.IncParentFoldersSize(FolderID:integer; Size:Int64);
begin
  while (FolderID <> 0) do
  begin
    inc(Folders[FolderID].Size, Size);
    FolderID := Folders[FolderID].ParentID;
  end;
end;


procedure TScan.SearchFolder(Path:string; FolderID:integer);
var SearchRec:TSearchRec; i,k:integer; DT:_FILETIME; ST,STL:_SYSTEMTIME; D:TDateTime;
begin
  FindFirst(Path+'\*', faAnyFile or faDirectory, SearchRec);
  i:=CurFolder; k:=CurFile;
  Folders[FolderID].SubFolderA:=i+1;
  Folders[FolderID].SubFileA:=k+1;
  repeat
    if (SearchRec.Name<>'.')and(SearchRec.Name<>'..') then
      if (SearchRec.Attr and faDirectory = faDirectory) then begin
        inc(i);
        if i+1>=length(Folders) then
          setlength(Folders,i+100);
        Folders[i].Name:=SearchRec.Name;
        Folders[i].Time:=SearchRec.Time;
        Folders[i].ParentID:=FolderID;
      end
      else
      if not IsExludeFileName(SearchRec.Name) then begin
        inc(k);
        if k+1>=length(Files) then
          setlength(Files,k+100);
        Files[k].Name:=SearchRec.Name;
        Files[k].Time:=SearchRec.Time;
        Files[k].Size:=
          Int64(SearchRec.FindData.nFileSizeHigh) shl Int64(32) +
          Int64(SearchRec.FindData.nFileSizeLow);

          {
        FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime,DT);
        FileTimeToSystemTime(DT,ST);
        SystemTimeToTzSpecificLocalTime(nil, ST, STL);
        D := EncodeDateTime(STL.wYear, STL.wMonth, STL.wDay, STL.wHour, STL.wMinute, STL.wSecond, STL.wMilliseconds);
        Files[k].Time:=DateTimeToFileDate(D);
          }

        Files[k].ParentID:=FolderID;
        IncParentFoldersSize(FolderID,Files[k].Size);
      end;
  until (FindNext(SearchRec)<>0);
  FindClose(SearchRec);
  Folders[FolderID].SubFolderZ:=i;
  Folders[FolderID].SubFileZ:=k;
  CurFolder:=i;
  CurFile:=k;
end;


function TScan.ScanPath(aPath: string; aProgress: TLabel; aApp: TApplication): Boolean;
var
  curFolder: Integer;
begin
  fScanPath := aPath;

  curFolder := 0;

  // set root path
  SetLength(Folders, 1);
  Folders[0].Name := aPath;
  Folders[0].ParentID := -1;

  repeat
    aProgress.Caption := GetRelativePath(curFolder);
    aProgress.Repaint;
    aApp.ProcessMessages;

    //sleep(100);
    if fStopCompare then
    begin
      Result := False;
      Exit;
    end;

    SearchFolder(GetFullPath(curFolder), curFolder);
    Inc(curFolder);
  until (Folders[curFolder].Name = '');

  Result := True;
end;


procedure TScan.Clear;
begin
  fScanPath := '';
  CurFolder := 0;
  CurFile := 0;
  SetLength(Folders, 0);
  SetLength(Files, 0);
  fExludedCount := 0;
end;


function TScan.ScanPaths(aPath1: string; aProgress1: TLabel; aApp: TApplication): Boolean;
begin
  Clear;

  Result := ScanPath(aPath1, aProgress1, aApp);
end;


procedure TScan.StopCompare;
begin
  fStopCompare := true;
end;


end.
