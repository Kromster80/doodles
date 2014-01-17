program Compare;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit_TDiff in 'Unit_TDiff.pas',
  Unit_TScan in 'Unit_TScan.pas',
  Unit_Tasks in 'Unit_Tasks.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Compare';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
