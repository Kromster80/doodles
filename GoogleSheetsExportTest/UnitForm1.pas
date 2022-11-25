unit UnitForm1;
interface
uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Clipbrd, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnExportOriginal: TButton;
    btnExportEscaped: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnExportOriginalClick(Sender: TObject);
    procedure btnExportEscapedClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fStringList: TStringList;
    function GetCombinations: string;
  end;

var
  Form1: TForm1;

implementation
uses
  KromStringUtils;

{$R *.dfm}

function TForm1.GetCombinations: string;
const
  CHUNKS: array [0..5] of string = ('', ' ', 'abc', '"', #39, '=');

  function Stack(const aCombination: string; aLevel: Byte): string;
  var
    I: Integer;
    s: string;
  begin
    if aLevel = Length(CHUNKS) then
    begin
      s := '';
      for I := Low(aCombination) to High(aCombination) do
        s := s + CHUNKS[StrToInt(aCombination[I])];

      fStringList.Append(s);
    end else
      for I := Low(CHUNKS) to High(CHUNKS) do
      // We can skip stacking of trivial chunks (empty, space, text)
      // Other special characters might be interesting to stack and test
      if (aCombination = '') or (I = 3) or (I = 4) or (I = 5) or (IntToStr(I) <> aCombination[aLevel]) then
        Stack(aCombination + IntToStr(I), aLevel + 1);
  end;
begin
  fStringList.Clear;

  Stack('', 0);
end;


procedure TForm1.btnExportOriginalClick(Sender: TObject);
begin
  GetCombinations;

  btnExportOriginal.Caption := IntToStr(fStringList.Count);

  Clipboard.AsText := fStringList.Text;
end;


procedure TForm1.btnExportEscapedClick(Sender: TObject);
var
  I: Integer;
begin
  GetCombinations;

  btnExportEscaped.Caption := IntToStr(fStringList.Count);

  // Special wrapping
  for I := 0 to fStringList.Count - 1 do
    fStringList[I] := EscapeTextForGoogleSheets(fStringList[I]);

  Clipboard.AsText := fStringList.Text;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  fStringList := TStringList.Create;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fStringList);
end;


end.
