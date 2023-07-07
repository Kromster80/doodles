unit Unit1;
{$I CompilerDirectives.inc}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  Utils2;

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Test' + REV1 + REV2;
end;


end.
