unit Unit1;
interface
uses
  SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, PNGImage, Math;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  I,K: Integer;
  PNG: TPngImage;
  V: Single;
begin
  PNG := TPngImage.CreateBlank(COLOR_GRAYSCALE, 8, 512, 512);

  for I := 0 to 511 do
  for K := 0 to 511 do
  begin
    V := RandG(0.5, 0.15);
    //EnsureRange is needed because tiny portion of values gets out of 0..1 range
    PNG.Pixels[K,I] := EnsureRange((Trunc(V * 254) + 1), 1, 255) * 65793;
  end;

  PNG.SaveToFile('out.png');
end;


end.
