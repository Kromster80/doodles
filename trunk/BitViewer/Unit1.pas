unit Unit1;

interface

uses
Windows, SysUtils, Forms, StdCtrls, Controls, FileCtrl, Graphics, Classes, ExtCtrls, Math;

type
  TForm1 = class(TForm)
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    FileListBox1: TFileListBox;
    Image2: TImage;
    CBFlipH: TCheckBox;
    CBFlipV: TCheckBox;
    ComboBox1: TComboBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    procedure LoadFile(Sender: TObject);
    procedure Redraw(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  f:file;
  sizeH,sizeV:integer;
  i,j,k:integer;
  zoom:array[1..5] of real=(0.25,0.5,1,2,4);
  r,g,b:array[1..1048576]of integer;
  v:array[1..1048576] of byte;
  Bitm:TBitmap;
  x:integer;
  Datasize:integer;
  s:string;

implementation

{$R *.DFM}

procedure TForm1.LoadFile(Sender: TObject);
begin
if not FileExists (filelistbox1.FileName) then exit;
assignfile(f,filelistbox1.FileName);
Filemode:=0; reset(f,1); Filemode:=2;
blockread(f,v,1048576,Datasize);
closefile(f);
ReDraw(Form1);
end;


procedure TForm1.Redraw(Sender: TObject);
var x,t:integer;
begin
SizeH:=strtoint(ComboBox1.Text);

//1bit  Black&White
if RadioGroup1.ItemIndex=0 then begin
SizeV:=min(Datasize div SizeH * 8 + 1,Image2.Height);
  for i:=1 to sizeH*sizeV do begin
    for j:=0 to 7 do begin
      t:=(i-1)*8+j+1;
      x:=round(power(2,7-j));
      if v[i] and x = x then r[t]:=255 else r[t]:=0;
      g[t]:=r[t];
      b[t]:=r[t];
    end;
  end;
end;

//4bit  GrayScale
if RadioGroup1.ItemIndex=1 then begin
SizeV:=min(Datasize div SizeH * 2 + 1,Image2.Height);
i:=sizeH*sizeV;
repeat                  
x:=v[i];
r[i*2-1]:=0;
r[i*2-0]:=0;
if x>=16 then repeat x:=x-16; inc(r[i*2-0]); until(x<16); r[i*2-0]:=r[i*2-0]*17;
if x>=1 then repeat x:=x-1;  inc(r[i*2-1]); until(x<1);  r[i*2-1]:=r[i*2-1]*17;
g[i*2-1]:=r[i*2-1];
g[i*2-0]:=r[i*2-0];
b[i*2-1]:=r[i*2-1];
b[i*2-0]:=r[i*2-0];
i:=i-1;
until(i=0);       end;

//8bit  GrayScale
if RadioGroup1.ItemIndex=2 then begin
SizeV:=min(Datasize div SizeH + 1,Image2.Height);
if SizeV>Image2.Height then SizeV:=Image2.Height;
i:=sizeH*sizeV;
repeat                  
r[i]:=v[i]; g[i]:=v[i]; b[i]:=v[i];
i:=i-1;
until(i=0); end;

//15bit  5.5.5
if RadioGroup1.ItemIndex=3 then begin
SizeV:=min(Datasize div SizeH div 2 + 1,Image2.Height);
i:=sizeH*sizeV;
repeat                 
x:=v[i*2-1]+v[i*2-0]*256;
r[i]:=0; g[i]:=0; b[i]:=0;
if x>=1024 then repeat x:=x-1024; inc(r[i]); until(x<1024); r[i]:=r[i]*8;
if x>=32 then repeat x:=x-32; inc(g[i]); until(x<32); g[i]:=g[i]*8;
if x>=1 then repeat x:=x-1; inc(b[i]); until(x<1); b[i]:=b[i]*8;
i:=i-1;
until(i=0);  end;

//16bit  5.6.5
if RadioGroup1.ItemIndex=4 then begin
SizeV:=min(Datasize div SizeH div 2 + 1,Image2.Height);
i:=sizeH*sizeV;
repeat                  
x:=v[i*2-1]+v[i*2-0]*256;
r[i]:=0; g[i]:=0; b[i]:=0;
if x>=2048 then repeat x:=x-2048; inc(r[i]); until(x<2048); r[i]:=r[i]*8;      //32
if x>=32 then repeat x:=x-32; inc(g[i]); until(x<32); g[i]:=g[i]*4;            //16
if x>=1 then repeat x:=x-1; inc(b[i]); until(x<1); b[i]:=b[i]*8;               //32
i:=i-1;
until(i=0);     end;

//24bit  8.8.8
if RadioGroup1.ItemIndex=5 then begin
  SizeV:=Datasize div SizeH div 3 + 1;
  i:=sizeH*sizeV;
  repeat
    b[i]:=v[i*3-2];
    g[i]:=v[i*3-1];
    r[i]:=v[i*3-0];
    i:=i-1;
  until(i=0);
end;

//32bit  8.8.8.0
if RadioGroup1.ItemIndex=6 then begin
SizeV:=Datasize div SizeH div 4 + 1;
i:=sizeH*sizeV;
repeat                  
b[i]:=v[i*4-3];
g[i]:=v[i*4-2];
r[i]:=v[i*4-1];
i:=i-1;
until(i=0); end;

Image2.Refresh;
Image2.Canvas.Brush.Color:=$777777;
Image2.Canvas.FillRect(Image2.ClientRect);
Bitm:=TBitmap.Create;
Bitm.Width:=sizeH;
Bitm.Height:=sizeV;
for k:=0 to sizeH do for j:=0 to sizeV do
  if CBFlipV.Checked then
    if CBFlipH.Checked then
      Bitm.Canvas.Pixels[k,j]:=r[k+j*sizeH+1]+g[k+j*sizeH+1]*256+b[k+j*sizeH+1]*65536
    else
      Bitm.Canvas.Pixels[sizeH-k,j]:=r[k+j*sizeH+1]+g[k+j*sizeH+1]*256+b[k+j*sizeH+1]*65536
  else
  if CBFlipH.Checked then
    Bitm.Canvas.Pixels[k,sizeV-j]:=r[k+j*sizeH+1]+g[k+j*sizeH+1]*256+b[k+j*sizeH+1]*65536
  else
    Bitm.Canvas.Pixels[sizeH-k,sizeV-j]:=r[k+j*sizeH+1]+g[k+j*sizeH+1]*256+b[k+j*sizeH+1]*65536;

Image2.Canvas.Draw(0,0,Bitm);
end;



end.
