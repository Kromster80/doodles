program Project1;
{$I CompilerDirectives.inc}
uses
  // As a rule of thumb, Delphi prefers to have one unit per line (especially when in IFDEFs)
  //{$IFDEF WDC} FastMM4, {$ENDIF} // Can be used only in Delphi, not Lazarus

  {$IFDEF MSWINDOWS} Vcl.Forms, {$ENDIF}
  {$IFDEF MSWINDOWS} Vcl.Dialogs, {$ENDIF}
  {$IFDEF MSWINDOWS} System.UITypes, {$ENDIF}
  SysUtils,

  Unit1 in 'Unit1.pas' {Form1},
  {$IFDEF DESKTOP}  Utils in 'Utils.pas', {$ENDIF}
  {$IF Defined(DESKTOP) AND Defined(WDC)} Utils2 in 'src\Utils2.pas' {$ENDIF};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
