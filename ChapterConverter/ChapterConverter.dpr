program ChapterConverter;
uses
  Vcl.Forms,
  FormChapterConverter in 'FormChapterConverter.pas' {Form1};

{$R *.res}

var
  Form1: TForm1;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
