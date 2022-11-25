program GoogleSheetsExportTest;
uses
  Vcl.Forms,
  UnitForm1 in 'UnitForm1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
