unit Unit1;
interface
uses
  Classes, Controls, Forms, Math, SysUtils, Windows, StdCtrls;

type
  TForm1 = class(TForm)
    procedure FormResize(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  end;


implementation
{$R *.DFM}
uses Unit_Game;


procedure TForm1.FormResize(Sender: TObject);
begin
  fGame.Resize(ClientWidth, ClientHeight);
end;


procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  fGame.MouseDown(Button, Shift, X, Y);
end;


procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  fGame.MouseMove(Shift, X, Y);
end;


end.

