unit Unit1;
interface
uses
  Windows, SysUtils, Forms, StdCtrls, Controls, FileCtrl, Graphics, Classes,
  ExtCtrls, Math;

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
    v: array [1 .. 1048576] of byte;
    fDatasize: integer;
  end;


implementation
{$R *.DFM}


procedure TForm1.LoadFile(Sender: TObject);
var
  f: file;
begin
  if not FileExists(FileListBox1.FileName) then
    Exit;

  assignfile(f, FileListBox1.FileName);
  Filemode := 0;
  reset(f, 1);
  Filemode := 2;
  blockread(f, v, 1048576, fDatasize);
  closefile(f);

  Redraw(nil);
end;


procedure TForm1.Redraw(Sender: TObject);
var
  i, j, k: integer;
  x, t: integer;
  sizeH, sizeV: integer;
  Bitm: TBitmap;
  r, g, b: array [1 .. 1048576] of integer;
begin
  sizeH := strtoint(ComboBox1.Text);

  // 1bit  Black&White
  if RadioGroup1.ItemIndex = 0 then
  begin
    sizeV := min(fDatasize div sizeH * 8 + 1, Image2.Height);
    for i := 1 to sizeH * sizeV do
    begin
      for j := 0 to 7 do
      begin
        t := (i - 1) * 8 + j + 1;
        x := round(power(2, 7 - j));
        if v[i] and x = x then
          r[t] := 255
        else
          r[t] := 0;
        g[t] := r[t];
        b[t] := r[t];
      end;
    end;
  end;

  // 4bit  GrayScale
  if RadioGroup1.ItemIndex = 1 then
  begin
    sizeV := min(fDatasize div sizeH * 2 + 1, Image2.Height);
    i := sizeH * sizeV;
    repeat
      x := v[i];
      r[i * 2 - 1] := 0;
      r[i * 2 - 0] := 0;
      if x >= 16 then
        repeat
          x := x - 16;
          inc(r[i * 2 - 0]);
        until (x < 16);
      r[i * 2 - 0] := r[i * 2 - 0] * 17;
      if x >= 1 then
        repeat
          x := x - 1;
          inc(r[i * 2 - 1]);
        until (x < 1);
      r[i * 2 - 1] := r[i * 2 - 1] * 17;
      g[i * 2 - 1] := r[i * 2 - 1];
      g[i * 2 - 0] := r[i * 2 - 0];
      b[i * 2 - 1] := r[i * 2 - 1];
      b[i * 2 - 0] := r[i * 2 - 0];
      i := i - 1;
    until (i = 0);
  end;

  // 8bit  GrayScale
  if RadioGroup1.ItemIndex = 2 then
  begin
    sizeV := min(fDatasize div sizeH + 1, Image2.Height);
    if sizeV > Image2.Height then
      sizeV := Image2.Height;
    i := sizeH * sizeV;
    repeat
      r[i] := v[i];
      g[i] := v[i];
      b[i] := v[i];
      i := i - 1;
    until (i = 0);
  end;

  // 15bit  5.5.5
  if RadioGroup1.ItemIndex = 3 then
  begin
    sizeV := min(fDatasize div sizeH div 2 + 1, Image2.Height);
    i := sizeH * sizeV;
    repeat
      x := v[i * 2 - 1] + v[i * 2 - 0] * 256;
      r[i] := 0;
      g[i] := 0;
      b[i] := 0;
      if x >= 1024 then
        repeat
          x := x - 1024;
          inc(r[i]);
        until (x < 1024);
      r[i] := r[i] * 8;
      if x >= 32 then
        repeat
          x := x - 32;
          inc(g[i]);
        until (x < 32);
      g[i] := g[i] * 8;
      if x >= 1 then
        repeat
          x := x - 1;
          inc(b[i]);
        until (x < 1);
      b[i] := b[i] * 8;
      i := i - 1;
    until (i = 0);
  end;

  // 16bit  5.6.5
  if RadioGroup1.ItemIndex = 4 then
  begin
    sizeV := min(fDatasize div sizeH div 2 + 1, Image2.Height);
    i := sizeH * sizeV;
    repeat
      x := v[i * 2 - 1] + v[i * 2 - 0] * 256;
      r[i] := 0;
      g[i] := 0;
      b[i] := 0;
      if x >= 2048 then
        repeat
          x := x - 2048;
          inc(r[i]);
        until (x < 2048);
      r[i] := r[i] * 8; // 32
      if x >= 32 then
        repeat
          x := x - 32;
          inc(g[i]);
        until (x < 32);
      g[i] := g[i] * 4; // 16
      if x >= 1 then
        repeat
          x := x - 1;
          inc(b[i]);
        until (x < 1);
      b[i] := b[i] * 8; // 32
      i := i - 1;
    until (i = 0);
  end;

  // 24bit  8.8.8
  if RadioGroup1.ItemIndex = 5 then
  begin
    sizeV := fDatasize div sizeH div 3 + 1;
    i := sizeH * sizeV;
    repeat
      b[i] := v[i * 3 - 2];
      g[i] := v[i * 3 - 1];
      r[i] := v[i * 3 - 0];
      i := i - 1;
    until (i = 0);
  end;

  // 32bit  8.8.8.0
  if RadioGroup1.ItemIndex = 6 then
  begin
    sizeV := fDatasize div sizeH div 4 + 1;
    i := sizeH * sizeV;
    repeat
      b[i] := v[i * 4 - 3];
      g[i] := v[i * 4 - 2];
      r[i] := v[i * 4 - 1];
      i := i - 1;
    until (i = 0);
  end;

  Image2.Refresh;
  Image2.Canvas.Brush.Color := $777777;
  Image2.Canvas.FillRect(Image2.ClientRect);
  Bitm := TBitmap.Create;
  try
    Bitm.Width := sizeH;
    Bitm.Height := sizeV;
    for k := 0 to sizeH do
    for j := 0 to sizeV do
    begin
      I := k + j * sizeH + 1;
      if CBFlipV.Checked then
        if CBFlipH.Checked then
          Bitm.Canvas.Pixels[k, j] := r[I] + g[I] shl 8 + b[I] shl 16
        else
          Bitm.Canvas.Pixels[sizeH - k, j] := r[I] + g[I] shl 8 + b[I] shl 16
      else
        if CBFlipH.Checked then
          Bitm.Canvas.Pixels[k, sizeV - j] := r[I] + g[I] shl 8 + b[I] shl 16
        else
          Bitm.Canvas.Pixels[sizeH - k, sizeV - j] := r[I] + g[I] shl 8 + b[I] shl 16;
    end;

    Image2.Canvas.Draw(0, 0, Bitm);
  finally
    Bitm.Free;
  end;
end;


end.
