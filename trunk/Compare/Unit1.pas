unit Unit1;
interface

uses
  Windows, Messages, ShellAPI, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, FileCtrl, ExtCtrls, Dialogs,
  Unit_TScan, Unit_TSettings, ImgList, KromUtils, KromIOUtils,
  Menus, Math;

type
  TForm1 = class(TForm)
    B_LaunchCompare: TButton;
    DirectoryListBox2: TDirectoryListBox;
    DriveComboBox2: TDriveComboBox;
    Memo1: TMemo;
    ListView1: TListView;
    Label1: TLabel;
    Label2: TLabel;
    B_StopCompare: TButton;
    B_CopyOver1: TButton;
    ImageList1: TImageList;
    ListView2: TListView;
    B_Delete2: TButton;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    Bevel1: TBevel;
    RGSyncMode: TRadioGroup;
    Label3: TLabel;
    B_AddTask: TButton;
    B_RemTask: TButton;
    ListTasks: TListView;
    B_Open1: TButton;
    B_OpenF1: TButton;
    B_Delete1: TButton;
    B_Open2: TButton;
    B_CopyOver2: TButton;
    B_OpenF2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;
    procedure GreyButtons(Grey:boolean);
    procedure LaunchCompareClick(Sender: TObject);
    procedure ListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure StopCompareClick(Sender: TObject);
    procedure FillLists(ScanID:integer; LV:TListView);
    procedure ListViewDblClick(Sender: TObject);
    procedure B_CopyOverClick(Sender: TObject);
    procedure B_DeleteClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AddNewTask(Sender: TObject);
    procedure RefreshTaskListBox(ID:integer);
    procedure B_RemTaskClick(Sender: TObject);
    procedure B_OpenClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ExeDir:string;
  f1,f2:file;
  c,d:array[1..262144] of char;
//  Path1,Path2:string;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
DragAcceptFiles(Form1.Handle,true);
fScan := TScan.Create;

GreyButtons(true);
B_AddTask.Enabled:=true;
B_RemTask.Enabled:=true;
B_StopCompare.Enabled:=false;

ExeDir:=ExtractFilePath(Application.ExeName);
fSettings:=TSettings.Create(ExeDir+'Compare_tasks.ini');

RefreshTaskListBox(1);
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  fSettings.SaveSettingsToFile(ExeDir+'Compare_tasks.ini');
  CanClose:=true;
end;   


procedure TForm1.AcceptFiles( var msg : TMessage );
const
  cnMaxFileNameLen = 255;
var
  i,DropCount: integer;
  acFileName : array [0..cnMaxFileNameLen] of char;
  DropPoint:TPoint;
begin
  // find out how many files we're accepting
  DropCount := DragQueryFile(msg.WParam, $FFFFFFFF, acFileName, cnMaxFileNameLen);
  // find out where drop occured
  DragQueryPoint( msg.WParam, DropPoint);

  // query for the file names
  for i := 1 to DropCount do begin
    DragQueryFile( msg.WParam, i-1, acFileName, cnMaxFileNameLen );

    if DropPoint.X<=Form1.Width div 2 then
      DirectoryListBox1.Directory:=acFileName
    else
      DirectoryListBox2.Directory:=acFileName;
  end;

  DragFinish( msg.WParam );
end;

procedure TForm1.GreyButtons(Grey:boolean);
begin
  B_AddTask.Enabled:=not Grey;
  B_RemTask.Enabled:=not Grey;
  B_StopCompare.Enabled:=Grey;
  if Grey or (ListView1.Items.Count>0) then begin
    B_CopyOver1.Enabled:=not Grey;
    B_Open1.Enabled:=not Grey;
    B_OpenF1.Enabled:=not Grey;
    B_Delete1.Enabled:=not Grey;
  end;
  if Grey or (ListView2.Items.Count>0) then begin
    B_CopyOver2.Enabled:=not Grey;
    B_Open2.Enabled:=not Grey;
    B_OpenF2.Enabled:=not Grey;
    B_Delete2.Enabled:=not Grey;
  end;
  //RGSyncMode.Enabled:=not Grey;
  Label1.Caption:=inttostr(ListView1.Items.Count)+' entries';
  Label2.Caption:=inttostr(ListView2.Items.Count)+' entries';

  

end;

procedure TForm1.LaunchCompareClick(Sender: TObject);
begin
  Memo1.Clear;

  GreyButtons(true);

  if fScan<>nil then FreeAndNil(fScan);

  fScan := TScan.Create;

  if not fScan.ScanPath(DirectoryListBox1.Directory,1,Label1,Application) then exit;
  if not fScan.ScanPath(DirectoryListBox2.Directory,2,Label2,Application) then exit;

  fScan.FindDifference(1,2);
  fScan.FindDifference(2,1);

  FillLists(1,ListView1);
  FillLists(2,ListView2);

  GreyButtons(false);

  Memo1.Lines.Add('Temporary files excluded '+inttostr(fScan.GetExludedCount(1))+' and '+inttostr(fScan.GetExludedCount(2)));
end;

procedure TForm1.ListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with Sender do begin
    Canvas.Brush.Color := $FF00FF;
    if Item.SubItems.Strings[2]='A' then
      Canvas.Brush.Color := $88CC88;
    if Item.SubItems.Strings[2]='O' then
      Canvas.Brush.Color := $888888;
    if Item.SubItems.Strings[2]='N' then
      Canvas.Brush.Color := $CCCCCC;
    if Item.SubItems.Strings[2]='C' then
      Canvas.Brush.Color := $88CCCC;
    Canvas.TextOut(Item.DisplayRect(drLabel).Left+2,
      Item.DisplayRect(drLabel).Top, Item.Caption);
    DefaultDraw := true;
  end;
end;

procedure TForm1.StopCompareClick(Sender: TObject);
begin
  fScan.StopCompare;
end;

procedure TForm1.FillLists(ScanID:integer; LV:TListView);
begin
  LV.Clear;
  LV.Column[0].Width:=285;
  LV.Column[0].MinWidth:=285;
  LV.Column[0].MaxWidth:=285;
  LV.Column[1].Width:=115;
  LV.Column[1].MinWidth:=115;
  LV.Column[1].MaxWidth:=115;
  LV.Column[2].Width:=50;
  LV.Column[2].MinWidth:=50;
  LV.Column[2].MaxWidth:=50;
  LV.Column[3].Width:=20;
  LV.Column[3].MinWidth:=20;
  LV.Column[3].MaxWidth:=20;

  fScan.FillList(ScanID,LV);
end;

procedure TForm1.ListViewDblClick(Sender: TObject);
var s:string;
begin
  if (Sender as TListView).ItemIndex=-1 then exit;
  s:=(Sender as TListView).Items.Item[(Sender as TListView).ItemIndex].Caption;
  if Sender=ListView1 then
    ShellExecute(handle, 'open', @(fScan.GetPath(1)+'\'+s)[1], NiL, Nil, SW_SHOWNORMAL);
  if Sender=ListView2 then
    ShellExecute(handle, 'open', @(fScan.GetPath(2)+'\'+s)[1], NiL, Nil, SW_SHOWNORMAL);
end;

procedure TForm1.B_CopyOverClick(Sender: TObject);
var
  i:integer; s1,s2:string; ToDel:array of boolean; TLI:TListItem;
  CopyList1,CopyList2:string;
  LV_A,LV_B:TListView; Path_A,Path_B:string;//Copy from A to B

    function FindAnyMatches():boolean;
    var i:integer;
    begin
    Result:=false;
    for i:=1 to LV_A.Items.Count do
      if (LV_A.Items.Item[i-1].Selected)or(LV_A.SelCount=0) then
        if fileexists(Path_B+'\'+LV_A.Items.Item[i-1].Caption) then begin
          Result:=true;
          exit;
        end;
    end;

begin
  if Sender=B_CopyOver1 then begin
    LV_A:=ListView1;
    LV_B:=ListView2;
    Path_A:=fScan.GetPath(1);
    Path_B:=fScan.GetPath(2);
  end else
  if Sender=B_CopyOver2 then begin
    LV_A:=ListView2;
    LV_B:=ListView1;
    Path_A:=fScan.GetPath(2);
    Path_B:=fScan.GetPath(1);
  end else
    Assert(false,'Bad sender');

if FindAnyMatches then
  if MessageBox(Form1.Handle,'Overwrite matching files?','Please confirm', MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2 ) <> IDYES then exit;

GreyButtons(true);
setlength(ToDel,LV_A.Items.Count+1);
CopyList1:='';
CopyList2:='';
for i:=1 to LV_A.Items.Count do
  if (LV_A.Items.Item[i-1].Selected)or(LV_A.SelCount=0) then begin
    s1:=Path_A+'\'+LV_A.Items.Item[i-1].Caption;
    s2:=Path_B+'\'+LV_A.Items.Item[i-1].Caption;
    if FileExists(s1) then begin //File
      FileSetAttr(s2, $00000000); //remove all attributes cos file gets overwritten anyway
      //CopyList1:=CopyList1+s1+#0;
      //CopyList2:=CopyList2+s2+#0;
     if CopyFile(@s1[1],@s2[1],false) then
        ToDel[i]:=true
      else
        Memo1.Lines.Add('CopyFile error');
    end else begin //Folder
      decs(s1);
      decs(s2);
      //CopyList1:=CopyList1+s1+'\*.*'+#0;
      //CopyList2:=CopyList2+s2+'\'+#0;
      if DirectoryExists(s1) then
        if CopyDir(s1,s2) then begin
          ToDel[i]:=true;
          Memo1.Lines.Add(s1+'\ copied')
        end else
          Memo1.Lines.Add('CopyDir error');
      end;
  end;

  for i:=LV_A.Items.Count downto 1 do
  if ToDel[i] then begin //First remove entry from List2
    TLI:=LV_B.FindCaption(0,LV_A.Items.Item[i-1].Caption,false,true,true);
    if TLI<>nil then TLI.Delete;
    LV_A.Items[i-1].Delete;
  end;

  GreyButtons(false);
end;

procedure TForm1.B_DeleteClick(Sender: TObject);
var
  i,h:integer;
  s:string;
  ToDel:array of boolean;
  TLI:TListItem;
  LV_A,LV_B:TListView;
  Path_A,Path_B:string;//Delete from A
begin
  if Sender=B_Delete1 then begin
    LV_A:=ListView1;
    LV_B:=ListView2;
    Path_A:=fScan.GetPath(1);
    Path_B:=fScan.GetPath(2);
  end else
  if Sender=B_Delete2 then begin
    LV_A:=ListView2;
    LV_B:=ListView1;
    Path_A:=fScan.GetPath(2);
    Path_B:=fScan.GetPath(1);
  end else
  Assert(false,'Wrong Delete sender');

  if MessageBox(Form1.Handle,'Delete these files?','Please confirm', MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2 ) <> IDYES then exit;
  GreyButtons(true);
  setlength(ToDel,LV_A.Items.Count+1);
  for i:=1 to LV_A.Items.Count do
  if (LV_A.Items.Item[i-1].Selected)or(LV_A.SelCount=0) then begin
    s:=Path_A+'\'+LV_A.Items.Item[i-1].Caption;

    if s[length(s)]='\' then begin
      decs(s); //Remove "\" from end

      if DirectoryExists(s) then begin
        for h:=1 to 3 do //now this is funny, sometimes first request doesn't work, but second works fine, ?
          if DelDir(s) then
            ToDel[i]:=true;
        if not ToDel[i] then
          Memo1.Lines.Add('DeleteDir failed');
      end else
          Memo1.Lines.Add('DeleteDir failed, no such Dir exists');

    end else begin //Delete file

      if FileExists(s) then begin
        FileSetAttr(s, $00000000); //remove all attributes cos file gets deleted anyway
        for h:=1 to 3 do //now this is funny, sometimes first request doesn't work, but second works fine, ?
          if DeleteFile(s) then
            ToDel[i]:=true;
        if not ToDel[i] then
          Memo1.Lines.Add('DeleteFile failed')
      end else
        Memo1.Lines.Add('DeleteFile failed, no such File exists');
    end;
  end;

  for i:=LV_A.Items.Count downto 1 do
  if ToDel[i] then begin
    TLI:=LV_B.FindCaption(0,LV_A.Items.Item[i-1].Caption,false,true,true);
    if TLI<>nil then TLI.SubItems[2]:='A';
    LV_A.Items[i-1].Delete;
  end;

  LV_B.Repaint;

  GreyButtons(false);
end;


procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if not InRange(ListTasks.ItemIndex+1,1,fSettings.TaskCount) then begin
    DirectoryListBox1.Directory:=ExeDir;
    DirectoryListBox1.Directory:=ExeDir;
    exit;
  end;
  DirectoryListBox1.Directory:=fSettings.TaskList[ListTasks.ItemIndex+1].PathA;
  DirectoryListBox2.Directory:=fSettings.TaskList[ListTasks.ItemIndex+1].PathB;
end;


procedure TForm1.AddNewTask(Sender: TObject);
begin
  if not fSettings.AddNewTask(
    InputBox('New task title','Input task title:','New Task Name'),
    DirectoryListBox1.Directory,
    DirectoryListBox2.Directory)
  then
    MessageBox(Form1.Handle,'Error','Too many tasks',MB_OK);
  RefreshTaskListBox(fSettings.TaskCount);
end;


procedure TForm1.RefreshTaskListBox(ID:integer);
var i:integer; ListItem:TListItem;
begin
  ListTasks.Clear;
  for i:=1 to fSettings.TaskCount do begin
    ListItem:=ListTasks.Items.Add;
    ListItem.Caption:=fSettings.TaskList[i].Title;
    ListItem.SubItems.Add(fSettings.TaskList[i].PathA);
    ListItem.SubItems.Add(fSettings.TaskList[i].PathB);
  end;

  ListTasks.Columns[0].Width:=120;
  ListTasks.Columns[1].Width:=(ListTasks.Width-120) div 2;
  ListTasks.Columns[2].Width:=ListTasks.Width-ListTasks.Columns[1].Width-120-4;

  if ID<=ListTasks.Items.Count then
    ListTasks.ItemIndex:=ID-1;
end;


procedure TForm1.B_RemTaskClick(Sender: TObject);
var ID:integer;
begin
  ID:=ListTasks.ItemIndex+1;
  if ID=0 then exit;
  if MessageBox(Form1.Handle,@('Remove the task?'+eol+fSettings.TaskList[ID].PathA+eol+fSettings.TaskList[ID].PathB)[1] ,'Please confirm', MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2 ) <> IDYES then exit;
  fSettings.RemoveTask(ID);
  RefreshTaskListBox(EnsureRange(ID,1,fSettings.TaskCount));
end;


procedure TForm1.B_OpenClick(Sender: TObject);
var LV:TListView; i,ID:integer; s:string;
begin
  if (Sender=B_Open1)or(Sender=B_OpenF1) then begin LV:=ListView1; ID:=1; end;
  if (Sender=B_Open2)or(Sender=B_OpenF2) then begin LV:=ListView2; ID:=2; end;

  if LV.SelCount>1 then
  if MessageBox(Form1.Handle,@('Open '+inttostr(LV.SelCount)+' selected items at once?')[1] ,'Please confirm', MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2 ) <> IDYES then exit;

  for i:=1 to LV.Items.Count do
  if LV.Items[i-1].Selected then begin
    s:=fScan.GetPath(ID)+'\'+LV.Items[i-1].Caption;
    if (Sender=B_Open1)or(Sender=B_Open2) then
      ShellExecute(handle, 'open', @(fScan.GetPath(ID)+'\'+LV.Items[i-1].Caption)[1], nil, nil, SW_SHOWNORMAL);
    if (Sender=B_OpenF1)or(Sender=B_OpenF2) then
      ShellExecute(handle, 'open', @ExtractFilePath(fScan.GetPath(ID)+'\'+LV.Items[i-1].Caption)[1], nil, nil, SW_SHOWNORMAL);
  end;
end;


procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
//var LV:TListView; i,ID:integer; acc:int64;
begin
  {if Sender=ListView1 then begin LV:=ListView1; ID:=1; end;
  if Sender=ListView2 then begin LV:=ListView2; ID:=2; end;

  acc := 0;
  for i:=1 to LV.Items.Count do
  if LV.Items[i-1].Selected then
//    acc := fScanacc + fScan.GetPath(ID)+'\'+LV.Items[i-1].Caption;

  //Label4.Caption := inttostr(acc);
   }
end;

end.
