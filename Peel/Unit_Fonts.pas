unit Unit_Fonts;
interface
uses Classes, Controls, Graphics, Math, StrUtils, SysUtils, Windows,
    dglOpenGL, OGLUtils,
    MyGLBitmapFont, Unit_Color, Unit_Defaults;

const
  SIZES_COUNT = 6;

type
  TFontProps = record
    Color: TColor4c;
    Size: Integer;
    Style: Integer;
    Justify: TAlignment;
  end;

  TLFontLib = class
  private
    fFontScale: Single; //Tweak fonts scale to match VCL fonts/layout
    fRenderScale: Single; //Viewport scale to design-size (DESK_SIZE_X * DESK_SIZE_Y)
    fSizes: array [1..SIZES_COUNT] of Integer;
    fLastIndex: Integer;
    function ChooseFontSize(aFontSize: Single): Integer;
  public
    FontStyles: array [0..8, 1..SIZES_COUNT] of TMyBitmapFont;
    constructor Create;
    destructor Destroy; override;

    function AddFont(aFile,aFontName: string; aFontStyle: TFontStyles): Integer;

    function GetFont(aStyle: Integer): TFont;
    procedure SetFontName(aStyle: Integer; aName: string);

    property FontScale: Single read fFontScale write fFontScale;
    property RenderScale: Single read fRenderScale write fRenderScale;

    function GetCharWidth(aChar: Char; const aFont: TFontProps): Single;
    function GetStringWidth(aString: string; const aFont: TFontProps): Single;
    function GetStringHeight(const aFont: TFontProps; aString: string = ''): Single;
    function RepositionEOLs(const aString: string; const aFont: TFontProps; aWidth,aPadFirst: Integer): string;

    procedure SaveToFiles(aPath: String);
    procedure Render(x,y,w,h: Integer; s: Single; const aString: string; const aFont: TFontProps; aCutLines: Boolean = True);
    procedure RenderV(X,Y,W,H: Integer; aScale: Single; const aString: string; const aFont: TFontProps; aCutLines: Boolean = True);
  end;


  function MakeFont(aColor: TColor4c; aSize: Integer; aStyle: Integer; aJustify: TAlignment): TFontProps;


var
  fFontLib: TLFontLib;

  fsArialNormal,
  fsArialBold,
  fsArialItalic,
  fsCourierNormal,
  fsCourierBold:integer;


implementation


function MakeFont(aColor: TColor4c; aSize: Integer; aStyle: Integer; aJustify: TAlignment): TFontProps;
begin
  Result.Color := aColor;
  Result.Size := aSize;
  Result.Style := aStyle;
  Result.Justify := aJustify;
end;


{ TLFontLib }
constructor TLFontLib.Create;
begin
  inherited;

  fLastIndex := -1;
  fFontScale := 1;
  fRenderScale := 1;

  fSizes[1] := 8;
  fSizes[2] := 9;
  fSizes[3] := 10;
  fSizes[4] := 12;
  fSizes[5] := 24;
  fSizes[6] := 36;

  fsArialNormal := AddFont('', 'Tahoma', []);
  fsArialBold := AddFont('', 'Tahoma', [fsBold]);
end;


destructor TLFontLib.Destroy;
var
  i:integer;
  k:integer;
begin
  for i:=low(FontStyles) to high(FontStyles) do
    for k:=1 to SIZES_COUNT do
      if FontStyles[i,k] <> nil then
        FontStyles[i,k].Free;

  //RemoveFontResourceEx(PChar(ExeDir+'Vegas Desert.ttf'), FR_PRIVATE, nil);

  inherited;
end;


function TLFontLib.AddFont(aFile,aFontName: String; aFontStyle:TFontStyles):integer;
var k:integer;
begin

  //Load font from file
  if aFile<>'' then begin
    if not FileExists(aFile) then begin
      MessageBox(0, PChar('Font file could not be found:'+eol+aFile), 'Error', MB_ICONEXCLAMATION or MB_OK);
      Result := -1;
      exit;
    end;

    k := AddFontResourceEx(PChar(aFile), FR_PRIVATE, nil);
    if k < 1 then begin
      MessageBox(0, PChar('Font file could not be loaded:'+eol+aFile), 'Error', MB_ICONEXCLAMATION or MB_OK);
      Result := -1;
      exit;
    end;
  end;

  Result := fLastIndex + 1;

  //SetLength(FontStyles, Result+1);

  for k:=1 to SIZES_COUNT do
  begin
    FontStyles[Result,k] := TMyBitmapFont.Create;
    FontStyles[Result,k].Font.Name := aFontName;
    FontStyles[Result,k].Font.Charset := DEFAULT_CHARSET;
    FontStyles[Result,k].Font.Style := aFontStyle;
    FontStyles[Result,k].Font.Size := fSizes[k];
    FontStyles[Result,k].Ranges.Clear;
    FontStyles[Result,k].Ranges.Add( #32, #125);
    FontStyles[Result,k].Ranges.Add(#129, #129);
    FontStyles[Result,k].Ranges.Add(#171);
    FontStyles[Result,k].Ranges.Add(#184);
    FontStyles[Result,k].Ranges.Add(#185);
    FontStyles[Result,k].Ranges.Add(#187);
    FontStyles[Result,k].Ranges.Add(#192, #255);

    FontStyles[Result,k].PrepareImage;
  end;

  inc(fLastIndex);
end;


function TLFontLib.GetFont(aStyle:integer):TFont;
begin
  Result := FontStyles[aStyle,1].Font;
end;


procedure TLFontLib.SetFontName(aStyle:integer; aName: String);
var k:integer;
begin
  for k:=1 to SIZES_COUNT do begin
    FontStyles[aStyle,k].Font.Name := aName;
    FontStyles[aStyle,k].PrepareImage;
  end;
end;


//Choose best font size depending on render scale
//(bigger scale requires smoother fonts) and font scale
function TLFontLib.ChooseFontSize(aFontSize: Single):integer;
var k:integer;
begin
  Result := 1;
  for k:=1 to SIZES_COUNT do
  if aFontSize*fRenderScale*fFontScale >= fSizes[k] then
    Result := k;
end;


function TLFontLib.GetCharWidth(aChar: Char; const aFont:TFontProps): Single;
var
  F: TMyBitmapFont;
begin
  Result := 0;
  F := FontStyles[aFont.Style, ChooseFontSize(aFont.Size)];
  if F = nil then exit;
  //fRenderScale is not required, since it's already commited to render area size
  Result := F.CalcCharWidth(AnsiChar(aChar)) * (aFont.Size/F.Font.Size) * fFontScale;
end;


function TLFontLib.GetStringWidth(aString: string; const aFont: TFontProps): Single;
var
  Fnt: TMyBitmapFont;
  SL: TStringList;
  I: Integer;
begin
  Result := 0;
  Fnt := FontStyles[aFont.Style, ChooseFontSize(aFont.Size)];
  if Fnt = nil then exit;

  SL := TStringList.Create;
  SL.Text := aString;

  //fRenderScale is not required, since it's already commited to render area size
  for I := 0 to SL.Count - 1 do
    Result := max(Result, Fnt.CalcStringWidth(AnsiString(SL[I])) * (aFont.Size/Fnt.Font.Size) * fFontScale);

  SL.Free;
end;


function TLFontLib.GetStringHeight(const aFont: TFontProps; aString: string = ''): Single;
var
  F: TMyBitmapFont;
  SL: TStringList;
begin
  Result := 1;
  F := FontStyles[aFont.Style, ChooseFontSize(aFont.Size)];
  if F = nil then exit;

  //fRenderScale is not required, since it's already commited to render area size
  Result := F.CharHeight * (aFont.Size / F.Font.Size) * fFontScale;

  if aString = '' then exit;

  SL := TStringList.Create;
  SL.Text := aString;
  Result := Result * SL.Count; //todo: Tweak line spacing
  SL.Free;
end;


function TLFontLib.RepositionEOLs(const aString: string; const aFont:TFontProps; aWidth,aPadFirst:Integer): string;
var
  s: string;
  LineLen: single;
  LastSpace,LineCount,I: Integer;
begin
  if aWidth = 0 then exit; //Avoid endless loops

  //Clear old EOLs
  s := StringReplace(aString, eol, ' ', [rfReplaceAll]);
  s := TrimRight(s);
  LineLen := 0;
  LastSpace := 0;
  LineCount := 1;
  I := 1;
  if Length(s) > 0 then
  repeat
    LineLen := LineLen + GetCharWidth(s[I], aFont);
    if (((LineCount=1)and(LineLen > aWidth-aPadFirst))or(LineLen > aWidth)) then
    begin
      //If there are no spaces or line-breaks in the line - just cut it
      if LastSpace=0 then LastSpace := I;
      Insert(eol, s, LastSpace);
      LineLen := 0;
      I := LastSpace;
      LastSpace := 0;
      Inc(LineCount);
    end;
    if (s[I] = ' ') or (s[I] = '-') then LastSpace := I;
    Inc(I);
  until(I > Length(s));

  Result := s;
end;


procedure TLFontLib.SaveToFiles(aPath: String);
var
  i:integer;
  k:integer;
  Caption: String;
begin
  for i:=low(FontStyles) to high(FontStyles) do
  for k:=1 to SIZES_COUNT do
  if FontStyles[i,k] <> nil then
  begin
    Caption := FontStyles[i,k].Font.Name;
    if fsBold in FontStyles[i,k].Font.Style then Caption := Caption + '_Bold';
    if fsItalic in FontStyles[i,k].Font.Style then Caption := Caption + '_Italic';
    Caption := Caption + '_' + inttostr(FontStyles[i,k].Font.Size);
    FontStyles[i,k].SaveToFile(aPath+Caption+'.bmp');
  end;
end;


//X,Y defines top-left
//W,H defines bounding box
//aCutLines will trim the line and add '...' to the end
procedure TLFontLib.Render(x,y,w,h:integer; s: Single; const aString: string; const aFont:TFontProps; aCutLines:boolean=true);
var
  F: TMyBitmapFont;
  TxWidth,TxHeight: Single;
  StringList: TStringList;
  I: Integer;
begin
  F := FontStyles[aFont.Style, ChooseFontSize(aFont.Size*s)];
  if F = nil then exit;
  if aString = '' then exit;

//  aString := aString + '"_pI|,/\';

  TxWidth := GetStringWidth(aString, aFont);
  TxHeight := GetStringHeight(aFont, aString);

  glPushMatrix;
    glTranslatef(x,y,0);

    if SHOW_TEXT_BOUNDS then
    begin
      glColor4f(1,0,0,0.5);
      glPushMatrix;
        glTranslatef(0.5,0.5,0);
        glBegin(GL_LINE_LOOP);
          glVertex2f(0,3);
          glVertex2f(3,0);
          glVertex2f(0,-3);
          glVertex2f(-3,0);
        glEnd;
        glBegin(GL_LINE_LOOP);
          glVertex2f(0,0);
          glVertex2f(w,0);
          glVertex2f(w,h);
          glVertex2f(0,h);
        glEnd;
      glPopMatrix;
    end;

    glRotatef(180, 1, 0, 0);
    glTranslatef(0,-Round((h-TxHeight)/2),0);

    case aFont.Justify of
      taLeftJustify:  glTranslatef(0,0,0);
      taCenter:       glTranslatef(Round((w-TxWidth)/2),0,0);
      taRightJustify: glTranslatef(w-TxWidth,0,0);
    end;

    if SHOW_TEXT_BOUNDS then
    begin
      glColor4f(1,0,0,0.5);
      glBegin(GL_LINE_LOOP);
        glVertex2f(0.5,-0.5);
        glVertex2f(TxWidth-0.5,-0.5);
        glVertex2f(TxWidth-0.5,-TxHeight+0.5);
        glVertex2f(0.5,-TxHeight+0.5);
      glEnd;
    end;

    StringList := TStringList.Create;
    StringList.Text := aString;

    for I := 0 to StringList.Count - 1 do
    begin
      glPushMatrix;

        if (w <> 0) and aCutLines then
        begin
          StringList[I] := Trim(StringList[I]);
          if (GetStringWidth(StringList[I], aFont) > w) then
            if (GetStringWidth('...', aFont) < w) then
              StringList[I] := LeftStr(StringList[I], Round(Length(StringList[I]) / GetStringWidth(StringList[I], aFont) * w) - 3) + '...'
            else
              StringList[I] := '';
        end;

        case aFont.Justify of
          taLeftJustify:  ;
          taCenter:       glTranslatef(Round(TxWidth-GetStringWidth(StringList[I],aFont)) div 2,0,0);
          taRightJustify: glTranslatef(Round(TxWidth-GetStringWidth(StringList[I],aFont)),0,0);
        end;

        glPushMatrix;
          glkScale(fFontScale * (aFont.Size/F.Font.Size));
          F.RenderString(AnsiString(StringList[I]), aFont.Color);
        glPopMatrix;

      glPopMatrix;
      glTranslatef(0, -Round(GetStringHeight(aFont)), 0);
    end;

  glPopMatrix;
  StringList.Free;
end;


//X,Y defines top-left
//W,H defines bounding box
procedure TLFontLib.RenderV(X,Y,W,H: Integer; aScale: Single; const aString: string; const aFont: TFontProps; aCutLines: Boolean = True);
var
  F:TMyBitmapFont;
  TxWidth,TxHeight: Single;
  StringList:TStringList;
  i:integer;
begin
  F := FontStyles[aFont.Style, ChooseFontSize(aFont.Size*aScale)];
  if F = nil then exit;
  if aString = '' then exit;

  TxWidth := GetStringWidth(aString, aFont);
  TxHeight := GetStringHeight(aFont, aString);

  glPushMatrix;
    glTranslatef(X,Y,0);

    if SHOW_TEXT_BOUNDS then begin
      glColor4f(1,0,0,0.5);
      glPushMatrix;
      glTranslatef(0.5,0.5,0);
      glBegin(GL_LINE_LOOP);
        glVertex2f(0,3);
        glVertex2f(3,0);
        glVertex2f(0,-3);
        glVertex2f(-3,0);
      glEnd;
      glBegin(GL_LINE_LOOP);
        glVertex2f(0,0);
        glVertex2f(W,0);
        glVertex2f(W,H);
        glVertex2f(0,H);
      glEnd;
      glPopMatrix;
    end;

    glRotatef(180, 1, 0, 0);
    glRotatef(90, 0, 0, 1);

    glTranslatef(0,-(W-TxHeight)/2,0);

    case aFont.Justify of
      taLeftJustify:  glTranslatef(-H,0,0);
      taCenter:       glTranslatef(-(H-TxWidth)/2,0,0);
      taRightJustify: glTranslatef(0,0,0);
    end;

    if SHOW_TEXT_BOUNDS then begin
      glColor4f(1,0,0,0.5);
      glPushMatrix;
      glTranslatef(0.5,0.5,0);
      glBegin(GL_LINE_LOOP);
        glVertex2f(0,0);
        glVertex2f(TxWidth,0);
        glVertex2f(TxWidth,-TxHeight);
        glVertex2f(0,-TxHeight);
      glEnd;
      glPopMatrix;
    end;

    StringList := TStringList.Create;
    StringList.Text := aString;

    for i:=0 to StringList.Count-1 do begin
      glPushMatrix;

        if aCutLines then begin
          StringList[i] := Trim(StringList[i]);
          if (H<>0) and (GetStringWidth(StringList[i], aFont) >= H) then
            StringList[i] := LeftStr(StringList[i], round(length(StringList[i])/GetStringWidth(StringList[i], aFont)*H)-3)+'...';
        end;

        case aFont.Justify of
          taLeftJustify:  ;
          taCenter:       glTranslatef((TxWidth-GetStringWidth(StringList[i],aFont))/2,0,0);
          taRightJustify: glTranslatef((TxWidth-GetStringWidth(StringList[i],aFont)),0,0);
        end;

        glPushMatrix;
          glkScale(fFontScale * (aFont.Size/F.Font.Size));
          F.RenderString(AnsiString(StringList[i]), aFont.Color);
        glPopMatrix;

      glPopMatrix;
      glTranslatef(0, -GetStringHeight(aFont), 0);
    end;

  glPopMatrix;
  glEnable(GL_TEXTURE_2D); //GLScene text renderer disables it!
  StringList.Free;
end;


end.
