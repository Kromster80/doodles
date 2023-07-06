program Project1;
{$I CompilerDirectives.inc}
uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  {$IFDEF DESKTOP}  Utils in 'Utils.pas' {$ENDIF},
  {$IF Defined(DESKTOP) AND Defined(WDC)} Utils2 in 'Utils2.pas' {$ENDIF};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
