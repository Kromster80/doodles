unit Unit1;
interface

uses
  Windows, Messages, ShellAPI, SysUtils, Classes, Graphics, Controls, Forms, StrUtils,
  StdCtrls, ComCtrls, FileCtrl, ExtCtrls, Dialogs, XMLDoc, XMLIntf,
  Unit_TScan, Unit_TDiff, ImgList, KromUtils, KromIOUtils, Unit_Tasks,
  Menus, Math;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    Panel1: TPanel;
    Label3: TLabel;
    B_LaunchCompare: TButton;
    DirectoryListBox2: TDirectoryListBox;
    DriveComboBox2: TDriveComboBox;
    Memo1: TMemo;
    B_StopCompare: TButton;
    DirectoryListBox1: TDirectoryListBox;
    DriveComboBox1: TDriveComboBox;
    btnTaskAdd: TButton;
    btnTaskRem: TButton;
    Panel2: TPanel;
    ListView1: TListView;
    B_CopyOver1: TButton;
    B_Open1: TButton;
    B_OpenF1: TButton;
    B_Delete1: TButton;
    Label1: TLabel;
    Panel3: TPanel;
    ListView2: TListView;
    B_Delete2: TButton;
    B_Open2: TButton;
    B_CopyOver2: TButton;
    B_OpenF2: TButton;
    Label2: TLabel;
    lstPaths: TListView;
    lstTasks: TListBox;
    btnPathAdd: TButton;
    btnPathRem: TButton;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure AcceptFiles(var msg: TMessage); message WM_DROPFILES;
    procedure LaunchCompareClick(Sender: TObject);
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure StopCompareClick(Sender: TObject);
    procedure FillLists(aDiff: TDiff; LV:TListView);
    procedure ListViewDblClick(Sender: TObject);
    procedure B_CopyOverClick(Sender: TObject);
    procedure B_DeleteClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnTaskAddClick(Sender: TObject);
    procedure btnTaskRemClick(Sender: TObject);
    procedure B_OpenClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPathAddClick(Sender: TObject);
    procedure btnPathRemClick(Sender: TObject);
    procedure lstTasksClick(Sender: TObject);
    procedure lstPathsItemChecked(Sender: TObject; Item: TListItem);
  private
    fTasks: TTasks;
    fScan1: TScan;
    fScan2: TScan;
    fDiff1: TDiff;
    fDiff2: TDiff;
  public
    procedure GreyButtons(Grey: Boolean);
    procedure RefreshTasks(aIndex: Integer);
    procedure RefreshPaths(aIndex: Integer);
    procedure LoadSettings(aFile: string);
    procedure SaveSettings(aFile: string);
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
  ExeDir := ExtractFilePath(Application.ExeName);
  DragAcceptFiles(Form1.Handle,true);

  fTasks := TTasks.Create;
  LoadSettings(ExeDir + 'compare_settings.xml');

  GreyButtons(False);

  RefreshTasks(0);
  RefreshPaths(lstTasks.ItemIndex);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveSettings(ExeDir + 'compare_settings.xml');
  fTasks.Free;
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  Panel1.Left := (ClientWidth - Panel1.Width) div 2;
  Panel2.Width := ClientWidth div 2;
  Panel3.Width := ClientWidth div 2;
  Panel3.Left  := ClientWidth - Panel3.Width;
end;


procedure TForm1.LoadSettings(aFile: string);
var
  X: IXMLDocument;
  nodeRoot, nodeTasks: IXMLNode;
begin
  if not FileExists(aFile) then
    Exit;

  X := TXMLDocument.Create(Self);
  X.LoadFromFile(aFile);
  X.Active := True;

  nodeRoot := X.DocumentElement;

  //Load Tasks
  nodeTasks := nodeRoot.ChildNodes['Tasks'];
  fTasks.LoadFromXML(nodeTasks);

  X := nil;
end;


procedure TForm1.lstPathsItemChecked(Sender: TObject; Item: TListItem);
var
  I,K: Integer;
  Pair: TPair;
begin
  I := lstTasks.ItemIndex;
  if I = -1 then Exit;

  K := Item.Index;

  Pair := fTasks[I].Paths[K];
  Pair.Checked := Item.Checked;
  fTasks[I].Paths[K] := Pair;
end;


procedure TForm1.lstTasksClick(Sender: TObject);
begin
  RefreshPaths(lstTasks.ItemIndex);
end;


procedure TForm1.SaveSettings(aFile: string);
var
  X: IXMLDocument;
  nodeRoot, nodeTasks: IXMLNode;
begin
  X := NewXMLDocument;
  X.Encoding := 'utf-8';
  X.Options := [doNodeAutoIndent];
//  X.Active := True;

  nodeRoot := X.AddChild('Root');

  //Save Tasks
  nodeTasks := nodeRoot.AddChild('Tasks');
  fTasks.SaveToXML(nodeTasks);

  X.SaveToFile(aFile);
  X := nil;
end;


procedure TForm1.AcceptFiles(var msg: TMessage);
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
  DragQueryPoint(msg.WParam, DropPoint);

  // query for the file names
  for I := 0 to DropCount - 1 do
  begin
    DragQueryFile(msg.WParam, I, acFileName, cnMaxFileNameLen);

    if DropPoint.X <= Form1.Width div 2 then
      DirectoryListBox1.Directory := acFileName
    else
      DirectoryListBox2.Directory := acFileName;
  end;

  DragFinish(msg.WParam);
end;


//Disable buttons while compare tasks is executed
procedure TForm1.LaunchCompareClick(Sender: TObject);
var
  I,K: Integer;
begin
  I := lstTasks.ItemIndex;
  if I = -1 then Exit;

  Memo1.Clear;

  GreyButtons(True);
  try
    FreeAndNil(fScan1);
    FreeAndNil(fScan2);

    fScan1 := TScan.Create(fTasks[0].ExludeFilter);
    fScan2 := TScan.Create(fTasks[0].ExludeFilter);

    if not fScan1.ScanPaths(DirectoryListBox1.Directory, Label1, Application)
    or not fScan2.ScanPaths(DirectoryListBox1.Directory, Label1, Application) then
      Exit;

    fDiff1.FindDifference(fScan1, fScan2);
    fDiff2.FindDifference(fScan2, fScan1);

    FillLists(fDiff1, ListView1);
    FillLists(fDiff2, ListView2);
  finally
    GreyButtons(false);
  end;

  Memo1.Lines.Add('Temporary files excluded ' + IntToStr(fScan1.ExludedCount) + ' and ' + IntToStr(fScan2.ExludedCount));
end;


procedure TForm1.ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  with Sender do
  begin
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
    DefaultDraw := True;
  end;
end;


procedure TForm1.StopCompareClick(Sender: TObject);
begin
  fScan1.StopCompare;
  fScan2.StopCompare;
end;


procedure TForm1.FillLists(aDiff: TDiff; LV:TListView);
begin
  LV.Clear;
  LV.Column[0].Width := 285;
  LV.Column[0].MinWidth := 285;
  LV.Column[0].MaxWidth := 285;
  LV.Column[1].Width := 115;
  LV.Column[1].MinWidth := 115;
  LV.Column[1].MaxWidth := 115;
  LV.Column[2].Width := 50;
  LV.Column[2].MinWidth := 50;
  LV.Column[2].MaxWidth := 50;
  LV.Column[3].Width := 20;
  LV.Column[3].MinWidth := 20;
  LV.Column[3].MaxWidth := 20;

  aDiff.FillList(LV);
end;

procedure TForm1.ListViewDblClick(Sender: TObject);
var s:string;
begin
  if (Sender as TListView).ItemIndex=-1 then exit;
  s:=(Sender as TListView).Items.Item[(Sender as TListView).ItemIndex].Caption;
  if Sender=ListView1 then
    ShellExecute(handle, 'open', @(fScan1.Path+'\'+s)[1], NiL, Nil, SW_SHOWNORMAL);
  if Sender=ListView2 then
    ShellExecute(handle, 'open', @(fScan2.Path+'\'+s)[1], NiL, Nil, SW_SHOWNORMAL);
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
    Path_A:=fScan1.Path;
    Path_B:=fScan2.Path;
  end else
  if Sender=B_CopyOver2 then begin
    LV_A:=ListView2;
    LV_B:=ListView1;
    Path_A:=fScan2.Path;
    Path_B:=fScan1.Path;
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
    Path_A:=fScan1.Path;
    Path_B:=fScan2.Path;
  end else
  if Sender=B_Delete2 then begin
    LV_A:=ListView2;
    LV_B:=ListView1;
    Path_A:=fScan2.Path;
    Path_B:=fScan1.Path;
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
var I,K: Integer;
begin
  DirectoryListBox1.Directory := ExeDir;
  DirectoryListBox2.Directory := ExeDir;

  I := lstTasks.ItemIndex;
  if not InRange(I, 0, fTasks.Count - 1) then Exit;

  K := lstPaths.ItemIndex;
  if not InRange(K, 0, fTasks[I].Count - 1) then Exit;

  DirectoryListBox1.Directory := fTasks[I].Paths[K].A;
  DirectoryListBox2.Directory := fTasks[I].Paths[K].B;
end;


procedure TForm1.btnPathAddClick(Sender: TObject);
var I: Integer;
begin
  I := lstTasks.ItemIndex;
  if not InRange(I, 0, fTasks.Count - 1) then Exit;

  fTasks[I].AddPaths(DirectoryListBox1.Directory, DirectoryListBox2.Directory);

  RefreshPaths(I);
end;


procedure TForm1.btnPathRemClick(Sender: TObject);
var I: Integer;
begin
  I := lstTasks.ItemIndex;
  if not InRange(I, 0, fTasks.Count - 1) then Exit;

  fTasks[I].Delete(I);

  RefreshPaths(I);
end;


procedure TForm1.btnTaskAddClick(Sender: TObject);
var
  T: TTask;
  NewTitle: string;
begin
  NewTitle := 'New Task Name';
  if not InputQuery('New task title', 'Input task title:', NewTitle) then Exit;

  T := TTask.Create;
  T.Title := NewTitle;
  fTasks.Add(T);

  RefreshTasks(fTasks.Count);
end;


procedure TForm1.btnTaskRemClick(Sender: TObject);
var I: Integer;
begin
  I := lstTasks.ItemIndex;
  if I = -1 then Exit;
  if MessageBox(Handle, @('Remove the task?' + eol + fTasks[I].Title)[1] , 'Please confirm', MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2 ) <> IDYES then Exit;

  fTasks.Delete(I);
  RefreshTasks(EnsureRange(I, 0, fTasks.Count - 1));
end;


procedure TForm1.GreyButtons(Grey: Boolean);
var
  Act1, Act2: Boolean;
begin
  btnTaskAdd.Enabled := not Grey;
  btnTaskRem.Enabled := not Grey;
  btnPathAdd.Enabled := not Grey;
  btnPathRem.Enabled := not Grey;

  B_LaunchCompare.Enabled := not Grey;
  B_StopCompare.Enabled := Grey;

  Act1 := not Grey and (ListView1.Items.Count > 0);
  B_CopyOver1.Enabled := Act1;
  B_Open1.Enabled := Act1;
  B_OpenF1.Enabled := Act1;
  B_Delete1.Enabled := Act1;

  Act2 := not Grey and (ListView2.Items.Count > 0);
  B_CopyOver2.Enabled := Act2;
  B_Open2.Enabled := Act2;
  B_OpenF2.Enabled := Act2;
  B_Delete2.Enabled := Act2;

  Label1.Caption := IntToStr(ListView1.Items.Count) + ' entries';
  Label2.Caption := IntToStr(ListView2.Items.Count) + ' entries';
end;


procedure TForm1.RefreshTasks(aIndex: Integer);
var
  I: Integer;
begin
  lstTasks.Clear;

  for I := 0 to fTasks.Count - 1 do
    lstTasks.Items.Add(fTasks[I].Title);

  if InRange(aIndex, 0, lstTasks.Items.Count - 1) then
    lstTasks.ItemIndex := aIndex;
end;


procedure TForm1.RefreshPaths(aIndex: Integer);
  function FirstMatch(A, B: string): Integer;
  var I: Integer;
  begin
    for I := 1 to Min(Length(A), Length(B)) do
      if A[I] <> B[I] then
        Break;
    Result := I;
  end;
var
  I,K,H: Integer;
  LI: TListItem;
begin
  lstPaths.Clear;

  I := lstTasks.ItemIndex;
  if I = -1 then Exit;

  for K := 0 to fTasks[I].Count - 1 do
  begin
    H := FirstMatch(fTasks[I].Paths[K].A, fTasks[I].Paths[K].B) - 1;
    LI := lstPaths.Items.Add;
    //LI.Caption := 'Caption' + Copy(fTasks[I].Paths[K].A, 1, H);
    LI.Caption := RightStr(fTasks[I].Paths[K].A, Length(fTasks[I].Paths[K].A) - H);
    LI.SubItems.Add(RightStr(fTasks[I].Paths[K].B, Length(fTasks[I].Paths[K].B) - H));
    LI.Checked := True;
  end;

  if InRange(aIndex, 0, lstPaths.Items.Count - 1) then
    lstPaths.ItemIndex := aIndex;
end;


procedure TForm1.B_OpenClick(Sender: TObject);
var LV:TListView; i,ID:integer; s:string;
begin
  if (Sender=B_Open1)or(Sender=B_OpenF1) then begin LV:=ListView1; ID:=1; end;
  if (Sender=B_Open2)or(Sender=B_OpenF2) then begin LV:=ListView2; ID:=2; end;

  if LV.SelCount>1 then
  if MessageBox(Handle,@('Open '+inttostr(LV.SelCount)+' selected items at once?')[1] ,'Please confirm', MB_YESNO or MB_ICONWARNING or MB_DEFBUTTON2 ) <> IDYES then exit;

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
