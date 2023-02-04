unit FormChapterConverter;
interface
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnConvert: TButton;
    Label1: TLabel;
    memInput: TMemo;
    memOutput: TMemo;
    btnSave: TButton;
    sdSave: TSaveDialog;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnConvertClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    procedure Convert(aIn, aOut: TStrings);
  end;

implementation
{$R *.dfm}


procedure TForm1.Convert(aIn, aOut: TStrings);
var
  I: Integer;
  sLine: string;
  chapterTimeStart: string;
  chapterTitle: string;
  separator: Integer;
begin
  aOut.BeginUpdate;
  aOut.Clear;

  aOut.Append('<?xml version="1.0"?>');
  aOut.Append('<!-- <!DOCTYPE Chapters SYSTEM "matroskachapters.dtd"> -->');
  aOut.Append('<Chapters>');
  aOut.Append('  <EditionEntry>');
  aOut.Append('    <EditionUID>3661229946342586706</EditionUID>');

  for I := 0 to aIn.Count - 1 do
  begin
    sLine := aIn[I];
    separator := Pos(' - ', sLine);

    chapterTimeStart := Copy(sLine, 1, separator-1);
    chapterTitle := Copy(sLine, separator+3, 999);

    aOut.Append('    <ChapterAtom>');
    aOut.Append(Format('      <ChapterTimeStart>%s.000000</ChapterTimeStart>', [chapterTimeStart]));
    aOut.Append(Format('      <ChapterUID>2705120389247460006</ChapterUID>', [Random(MaxInt)]));
    aOut.Append('      <ChapterDisplay>');
    aOut.Append(Format('        <ChapterString>%s</ChapterString>', [chapterTitle]));
    aOut.Append('        <ChapterLanguage>und</ChapterLanguage>');
    aOut.Append('        <ChapLanguageIETF>und</ChapLanguageIETF>');
    aOut.Append('      </ChapterDisplay>');
    aOut.Append('    </ChapterAtom>');
  end;

  aOut.Append('  </EditionEntry>');
  aOut.Append('</Chapters>');
  aOut.EndUpdate;
end;


procedure TForm1.btnConvertClick(Sender: TObject);
begin
  Convert(memInput.Lines, memOutput.Lines);
end;


procedure TForm1.btnSaveClick(Sender: TObject);
begin
  if sdSave.Execute then
    memOutput.Lines.SaveToFile(sdSave.FileName);
end;


end.
