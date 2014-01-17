unit Unit_TScan;
interface
uses Forms, Classes, StdCtrls, Windows, ComCtrls, SysUtils, DateUtils, KromUtils, KromIOUtils, Masks;

type
  TCompareFolder = record
    Name: string;
    FileTime: Integer;
    Size: Int64; //4GB+
    ParentID: Integer;
    SubFolderA, SubFolderZ: Integer;
    SubFileA, SubFileZ: Integer;
    CorrespondingID: Integer;
  end;

  TCompareFile = record
    Name: string;
    FileTime: Integer;
    Size: Int64; //4GB+
    ParentID: Integer;
    CorrespondingID: Integer;
  end;

type
  TScan = class
  private
    fStopCompare: Boolean;
    fFilterFiles: TStringList;
    fFilterFolders: TStringList;
    fScanPath: string;

    fExludedCount: Integer;
  public
    CurFolder, CurFile: Integer;
    Folders: array of TCompareFolder;
    Files: array of TCompareFile;

    constructor Create(aFilterFiles, aFilterFolders: string);
    destructor Destroy; override;

    function SkipFile(aFileName: string): boolean;
    function SkipFolder(aFolderName: string): boolean;
    function GetFullPath(FolderID: Integer): string;
    function GetRelativePath(FolderID: Integer): string;
    function GetRelativeFileName(FileID: Integer): string;
    function GetFolderDateTime(FolderID: Integer): string;
    function GetFileDateTime(FileID: Integer): string;
    function GetFolderSize(FolderID: Integer): string;
    function GetFileSize(FileID: Integer): string;
    procedure IncParentFoldersSize(FolderID: Integer; Size: Int64);
    procedure SearchFolder(Path: string; FolderID: Integer);
    function ScanPath(aPath: string; aProgress: TLabel; aApp: TApplication): Boolean;

    procedure Clear;
    function ScanPaths(aPath1: string; aProgress1: TLabel; aApp: TApplication): Boolean;
    procedure StopCompare;
    property ExludedCount: Integer read fExludedCount;
    property Path: string read fScanPath;
  end;


implementation


{ TScan }
constructor TScan.Create(aFilterFiles, aFilterFolders: string);
begin
  inherited Create;

  fFilterFiles := TStringList.Create;
  fFilterFiles.Delimiter := ';';
  fFilterFiles.StrictDelimiter := True;
  fFilterFiles.DelimitedText := aFilterFiles;
  fFilterFolders := TStringList.Create;
  fFilterFolders.Delimiter := ';';
  fFilterFolders.StrictDelimiter := True;
  fFilterFolders.DelimitedText := aFilterFolders;
end;


destructor TScan.Destroy;
begin
  fFilterFiles.Free;
  fFilterFolders.Free;

  inherited;
end;


//Check wherever file should be skipped
function TScan.SkipFile(aFileName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  aFileName := UpperCase(aFileName);

  for I := 0 to fFilterFiles.Count - 1 do
  if MatchesMask(aFileName, fFilterFiles[I]) then
  begin
    Inc(fExludedCount);
    Result := True;
    Break;
  end;
end;


//Check wherever folder should be skipped
function TScan.SkipFolder(aFolderName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  aFolderName := UpperCase(aFolderName);

  for I := 0 to fFilterFolders.Count - 1 do
  if MatchesMask(aFolderName, fFilterFolders[I]) then
  begin
    Inc(fExludedCount);
    Result := True;
    Break;
  end;
end;


//Return full Path for given FolderID
function TScan.GetFullPath(FolderID: Integer): string;
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


//Return Path for given FolderID except topmost parent folder
function TScan.GetRelativePath(FolderID: Integer): string;
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


//Relative filename including path
function TScan.GetRelativeFileName(FileID: Integer): string;
begin
  Result := GetRelativePath(Files[FileID].ParentID) + Files[FileID].Name;
end;


function TScan.GetFolderDateTime(FolderID: Integer): string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(Folders[FolderID].FileTime));
end;


function TScan.GetFileDateTime(FileID: Integer): string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(Files[FileID].FileTime));
end;


function TScan.GetFolderSize(FolderID: Integer): string;
begin
  Result := ReturnSize(Folders[FolderID].Size);
end;


function TScan.GetFileSize(FileID: Integer): string;
begin
  Result := ReturnSize(Files[FileID].Size);
end;


procedure TScan.IncParentFoldersSize(FolderID: Integer; Size: Int64);
begin
  while (FolderID <> 0) do
  begin
    Inc(Folders[FolderID].Size, Size);
    FolderID := Folders[FolderID].ParentID;
  end;
end;


procedure TScan.SearchFolder(Path: string; FolderID: Integer);
var
  SearchRec: TSearchRec;
  I, K: Integer;
  DT:_FILETIME;
  ST, STL:_SYSTEMTIME;
  D: TDateTime;
begin
  FindFirst(Path + '\*', faAnyFile, SearchRec);
  I := CurFolder;
  K := CurFile;
  Folders[FolderID].SubFolderA := I+1;
  Folders[FolderID].SubFileA := K+1;
  repeat
    //Skip uplinks
    if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
      Continue;

    if (SearchRec.Attr and faDirectory = faDirectory) then
    begin
      //Process folder
      if not SkipFolder(SearchRec.Name) then
      begin
        Inc(I);
        if I+1 >= Length(Folders) then
          SetLength(Folders, I+100);
        Folders[I].Name := SearchRec.Name;
        Folders[I].FileTime := SearchRec.Time;
        Folders[I].ParentID := FolderID;
      end;
    end
    else
    begin
      //Process file
      if not SkipFile(SearchRec.Name) then
      begin
        Inc(K);
        if K+1 >= Length(Files) then
          SetLength(Files, K+100);
        Files[K].Name := SearchRec.Name;
        Files[K].FileTime := SearchRec.Time;
        Files[K].Size := Int64(SearchRec.FindData.nFileSizeHigh) shl Int64(32) +
                         Int64(SearchRec.FindData.nFileSizeLow);

          {
        FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime,DT);
        FileTimeToSystemTime(DT,ST);
        SystemTimeToTzSpecificLocalTime(nil, ST, STL);
        D := EncodeDateTime(STL.wYear, STL.wMonth, STL.wDay, STL.wHour, STL.wMinute, STL.wSecond, STL.wMilliseconds);
        Files[K].Time:=DateTimeToFileDate(D);
          }

        Files[K].ParentID := FolderID;
        IncParentFoldersSize(FolderID, Files[K].Size);
      end;
    end;
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);

  Folders[FolderID].SubFolderZ := I;
  Folders[FolderID].SubFileZ := K;
  CurFolder := I;
  CurFile := K;
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
