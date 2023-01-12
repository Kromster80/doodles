program BitV;
uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}


var
  Form1: TForm1;


begin
  Application.Initialize;
  Application.Title := 'Bit Viewer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
