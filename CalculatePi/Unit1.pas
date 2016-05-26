unit Unit1;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  end;


var
  Form1: TForm1;


implementation
{$R *.dfm}


// Given that Pi can be estimated using the function 4 * (1 - 1/3 + 1/5 - 1/7 + …)
// with more terms giving greater accuracy, this is a function that calculates Pi
// to a required accuracy
function CalculatePi(aPrecision: Extended): string;
var
  Sign: Integer;
  Divider: Int64;
  Iterations: Int64;
  Res: Extended;
  I: Int64;
begin
  Res := 0;
  Sign := 1;
  Divider := 1;

  Assert(aPrecision >= 0.000000001);

  Iterations := Round (1 / (aPrecision / 4));

  I := 0;
  while I < Iterations do
  begin
    Res := Res + Sign / Divider;
    Sign := -Sign;
    Inc(Divider, 2);
    Inc(I);
  end;

  Res := Res * 4;

  Result := 'Calculated: ' + FloatToStr(Res) + sLineBreak +
            'Constant:   ' + FloatToStr(Pi) + sLineBreak +
            'in ' + FloatToStr(Iterations);
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Lines.Add(CalculatePi(0.000000001));
end;


end.
