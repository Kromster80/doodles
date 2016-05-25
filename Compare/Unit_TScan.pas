unit Unit_TScan;
interface
uses
  Forms, Classes, StdCtrls, Windows, ComCtrls, SysUtils, DateUtils, KromUtils, KromIOUtils, Masks;

type
  TCompareFolder = record
    Name: string;
    FileTime: Integer;
    Size: Int64; //4GB+
    ParentID: Integer;
    SubFolderA, SubFolderZ: Integer;
    SubFileA, SubFileZ: Integer;
    CorrespondingID: Integer;
    class function New(aName: string; aTime: Integer; aParentId: Integer): TCompareFolder; static;
    function DateTimeString: string;
    function SizeString: string;
  end;

  TCompareFile = record
    Name: string;
    FileTime: Integer;
    Size: Int64; //4GB+
    ParentID: Integer;
    CorrespondingID: Integer;
    class function New(aName: string; aTime: Integer; aSize: Int64; aParentId: Integer): TCompareFile; static;
    function DateTimeString: string;
    function SizeString: string;
  end;

type
  TScan = class
  private
    fStopCompare: Boolean;
    fFilterFiles: TStringList;
    fFilterFolders: TStringList;
    fScanPath: string;

    fCountFolders: Integer;
    fCountFiles: Integer;

    fCountExluded: Integer;
    function GetFullPath(aFolderID: Integer): string;
    function SkipFile(aFileName: string): boolean;
    function SkipFolder(aFolderName: string): boolean;
    procedure IncParentFoldersSize(aFolderID: Integer; aSize: Int64);
  public
    Folders: array of TCompareFolder;
    Files: array of TCompareFile;

    constructor Create(aFilterFiles, aFilterFolders: string);
    destructor Destroy; override;

    function GetRelativePath(aFolderID: Integer): string;
    function GetRelativeFileName(aFileID: Integer): string;
    procedure SearchFolder(aPath: string; aFolderID: Integer);
    function ScanPath(aPath: string; aProgress: TLabel; aApp: TApplication): Boolean;

    procedure Clear;
    function ScanPaths(aPath1: string; aProgress1: TLabel; aApp: TApplication): Boolean;
    procedure StopCompare;
    property Path: string read fScanPath;
    property CountFolders: Integer read fCountFolders;
    property CountFiles: Integer read fCountFiles;
    property CountExluded: Integer read fCountExluded;
  end;


implementation


{ TCompareFolder }
function TCompareFolder.DateTimeString: string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(FileTime));
end;


class function TCompareFolder.New(aName: string; aTime: Integer; aParentId: Integer): TCompareFolder;
begin
  Result.Name := aName;
  Result.FileTime := aTime;
  Result.ParentID := aParentId;
end;


function TCompareFolder.SizeString: string;
begin
  Result := ReturnSize(Size);
end;


{ TCompareFile }
function TCompareFile.DateTimeString: string;
begin
  DateTimeToString(Result, 'c', FileDateToDateTime(FileTime));
end;


class function TCompareFile.New(aName: string; aTime: Integer; aSize: Int64; aParentId: Integer): TCompareFile;
begin
  Result.Name := aName;
  Result.FileTime := aTime;
  Result.Size := aSize;
  Result.ParentID := aParentId;
end;


function TCompareFile.SizeString: string;
begin
  Result := ReturnSize(Size);
end;


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
    Inc(fCountExluded);
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
    Inc(fCountExluded);
    Result := True;
    Break;
  end;
end;


//Return full Path for given FolderID
function TScan.GetFullPath(aFolderID: Integer): string;
var
  SearchPath: string;
begin
  SearchPath := '';
  while (aFolderID <> -1) do
  begin
    SearchPath := Folders[aFolderID].Name + '\' + SearchPath;
    aFolderID := Folders[aFolderID].ParentID;
  end;
  Result := SearchPath;
end;


// Return Path for given FolderID except topmost parent folder
function TScan.GetRelativePath(aFolderID: Integer): string;
var
  SearchPath: string;
begin
  SearchPath := '';
  while (aFolderID <> 0) do
  begin
    SearchPath := Folders[aFolderID].Name + '\' + SearchPath;
    aFolderID := Folders[aFolderID].ParentID;
  end;
  Result := SearchPath;
end;


// Relative filename including path
function TScan.GetRelativeFileName(aFileID: Integer): string;
begin
  Result := GetRelativePath(Files[aFileID].ParentID) + Files[aFileID].Name;
end;


procedure TScan.IncParentFoldersSize(aFolderID: Integer; aSize: Int64);
begin
  while (aFolderID <> 0) do
  begin
    Inc(Folders[aFolderID].Size, aSize);
    aFolderID := Folders[aFolderID].ParentID;
  end;
end;


procedure TScan.SearchFolder(aPath: string; aFolderID: Integer);
var
  SearchRec: TSearchRec;
  DT:_FILETIME;
  ST, STL:_SYSTEMTIME;
  D: TDateTime;
begin
  FindFirst(aPath + '\*', faAnyFile, SearchRec);

  Folders[aFolderID].SubFolderA := fCountFolders + 1;
  Folders[aFolderID].SubFileA := fCountFiles + 1;
  repeat
    // Skip uplinks
    if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
      Continue;

    if (SearchRec.Attr and faDirectory = faDirectory) then
    begin
      // Process folder
      if not SkipFolder(SearchRec.Name) then
      begin
        Inc(fCountFolders);
        if fCountFolders+1 >= Length(Folders) then
          SetLength(Folders, fCountFolders+100);
        Folders[fCountFolders] := TCompareFolder.New(SearchRec.Name, SearchRec.Time, aFolderID);
      end;
    end
    else
    begin
      // Process file
      if not SkipFile(SearchRec.Name) then
      begin
        Inc(fCountFiles);
        if fCountFiles+1 >= Length(Files) then
          SetLength(Files, fCountFiles+100);
        Files[fCountFiles] := TCompareFile.New(SearchRec.Name, SearchRec.Time,
                                                Int64(SearchRec.FindData.nFileSizeHigh) shl Int64(32) +
                                                Int64(SearchRec.FindData.nFileSizeLow),
                                                aFolderID);

          {
        FileTimeToLocalFileTime(SearchRec.FindData.ftLastWriteTime,DT);
        FileTimeToSystemTime(DT,ST);
        SystemTimeToTzSpecificLocalTime(nil, ST, STL);
        D := EncodeDateTime(STL.wYear, STL.wMonth, STL.wDay, STL.wHour, STL.wMinute, STL.wSecond, STL.wMilliseconds);
        Files[fCountFiles].Time:=DateTimeToFileDate(D);
          }

        IncParentFoldersSize(aFolderID, Files[fCountFiles].Size);
      end;
    end;
  until (FindNext(SearchRec) <> 0);
  FindClose(SearchRec);

  Folders[aFolderID].SubFolderZ := fCountFolders;
  Folders[aFolderID].SubFileZ := fCountFiles;
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
  SetLength(Folders, 0);
  SetLength(Files, 0);
  fCountFolders := 0;
  fCountFiles := 0;
  fCountExluded := 0;
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
