unit Unit_TSettings;
interface
uses SysUtils, Math, FileCtrl, KromUtils;

type
  TSettings = class
  private

  public
    TaskCount:integer;
    TaskList:array[1..16]of record
      Title,PathA,PathB:string;
    end;
    FilterCount:integer;
    FilterList:array[1..256]of string;
    constructor Create(INIfilename:string);
    procedure SetDefaultSettings();
    function LoadSettingsFromFile(INIfilename:string):boolean;
    procedure SaveSettingsToFile(INIfilename:string);
    function AddNewTask(aTitle,aPathA,aPathB:string):boolean;
    procedure RemoveTask(ID:integer);
  end;

var
  fSettings:TSettings;

implementation


constructor TSettings.Create(INIfilename:string);
begin
  if not LoadSettingsFromFile(INIfilename) then SetDefaultSettings;
end;


procedure TSettings.SetDefaultSettings();
begin
  TaskCount:=0;
end;

function TSettings.LoadSettingsFromFile(INIfilename:string):boolean;
var ft:textfile; st:string;
begin
  Result:=false;
  TaskCount:=0;
  FilterCount:=0;
  if not CheckFileExists(INIfilename) then exit;

  assignfile(ft,INIfilename); reset(ft);

  repeat
    readln(ft,st);

    if (length(st)<2)or(st[1]+st[2]='//') then
      repeat readln(ft,st); until((eof(ft))or((st<>'')and(st[1]+st[2]<>'//')));

    if st='Task' then begin
      inc(TaskCount);
      readln(ft,TaskList[TaskCount].Title);
      readln(ft,TaskList[TaskCount].PathA);
      readln(ft,TaskList[TaskCount].PathB);
    end;

    if st='Filter' then begin
      inc(FilterCount);
      readln(ft,FilterList[FilterCount]);
      FilterList[FilterCount]:=UpperCase(FilterList[FilterCount]);
    end;

  until(eof(ft));
  closefile(ft);
  Result:=true;
end;


procedure TSettings.SaveSettingsToFile(INIfilename:string);
var f:textfile; i:integer;
begin
  assignfile(f,INIfilename);
  rewrite(f);
  writeln(f,'//Compare INI file');
  writeln(f);

  for i:=1 to TaskCount do begin
    writeln(f,'Task');
    writeln(f,TaskList[i].Title);
    writeln(f,TaskList[i].PathA);
    writeln(f,TaskList[i].PathB);
    writeln(f);
  end;

  for i:=1 to FilterCount do begin
    writeln(f,'Filter');
    writeln(f,FilterList[i]);
    writeln(f);
  end;

  closefile(f);
end;


function TSettings.AddNewTask(aTitle,aPathA,aPathB:string):boolean;
begin
  Result:=false;
  if TaskCount=length(TaskList) then exit;
  if length(aTitle)>32 then setlength(aTitle,32);

  inc(TaskCount);
  TaskList[TaskCount].Title:=aTitle;
  TaskList[TaskCount].PathA:=aPathA;
  TaskList[TaskCount].PathB:=aPathB;
  Result:=true;
end;


procedure TSettings.RemoveTask(ID:integer);
var i:integer;
begin
  if not InRange(ID,1,TaskCount) then exit;

  dec(TaskCount);
  for i:=ID to TaskCount do
    TaskList[i]:=TaskList[i+1];
end;

end.
