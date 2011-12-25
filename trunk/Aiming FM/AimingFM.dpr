program AimingFM;

{$DEFINE MACOS_VM_DEBUG}

uses
  //FMX.Filter in 'FMX.Filter.pas',
  FMX.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_Render in 'Unit_Render.pas',
  OpenGL_FMX in 'OpenGL_FMX.pas';

{$R *.res}

begin
  {$IFDEF MACOS_VM_DEBUG}
  FMX.Types.GlobalUseHWEffects := False;
  {$ENDIF}

  Application.Initialize;
  Application.Title := 'Aiming FM';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
