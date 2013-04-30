unit MyGLBitmapFont;
interface
uses Classes, Graphics, Math, Types, dglOpenGL, Unit_Vector;

const
  MAX_CHAR = 256;
  MIN_TEX_SIZE = 64;
  MAX_TEX_SIZE = 1024;

type
  TFontRanges = class
  private
    fCount: Integer;
    fRanges: array of record First, Last: AnsiChar; end;
  public
    procedure Clear;
    procedure Add(aChar: AnsiChar); overload;
    procedure Add(aFirstChar, aLastChar: AnsiChar); overload;
    property Count:integer read fCount;
    function CharacterCount: Integer;
  end;

	TMyBitmapFont = class
  private
    fRanges: TFontRanges;
    fGlyphs: TPicture;
    fCharWidth,fCharHeight: Byte;
    fHSpace,fVSpace,fHSpaceFix: Integer;
    fTexture: Cardinal;
    fCharWidths: array of Integer;
    fFont: TFont;
    fCharRects: array of TVector4f;
  protected
    procedure ResetCharWidths(w: Integer = -1);
    procedure SetCharWidths(index, value: Integer);
    procedure SetCharRects(index: Integer; X,Y,Z,W: single);

    procedure GetCharTexCoords(ch: AnsiChar; var topLeft, bottomRight: TVector2f);

    procedure LoadWindowsFont;
    property Glyphs: TPicture read fGlyphs;
  public
    constructor Create;
    destructor Destroy; override;

    property Ranges: TFontRanges read fRanges;
    property Font: TFont read fFont;
    procedure RenderString(const aString: AnsiString; const color: GLUint);

    function GetCharWidth(aChar: AnsiChar): Integer;
    function CalcCharWidth(aChar: AnsiChar): Integer;
    function CalcStringWidth(const aString: AnsiString): Integer;
    property CharHeight: Byte read fCharHeight;

    procedure PrepareImage;
    procedure LoadFromFile(AFileName: string);
    procedure SaveToFile(AFileName: string);
	end;


implementation
uses SysUtils;


{ TFontRanges }
procedure TFontRanges.Clear;
begin
  fCount := 0;
  setlength(fRanges,fCount);
end;


procedure TFontRanges.Add(aChar: AnsiChar);
begin
  Add(aChar, aChar);
end;


procedure TFontRanges.Add(aFirstChar, aLastChar: AnsiChar);
begin
  Inc(fCount);
  setlength(fRanges,fCount);
  fRanges[fCount-1].First := aFirstChar;
  fRanges[fCount-1].Last := aLastChar;
end;


function TFontRanges.CharacterCount: Integer;
var I: Integer;
begin
  Result := 0;
  for I := 0 to fCount - 1 do
    Inc(Result, Integer(fRanges[I].Last)-Integer(fRanges[I].First)+1);
end;


{ TMyBitmapFont }
constructor TMyBitmapFont.Create;
begin
  inherited;
  fRanges := TFontRanges.Create;
  fGlyphs := TPicture.Create;

  fCharWidth  := 16;
  fCharHeight := 16;
  fHSpace := 1;
  fVSpace := 1;

  fFont := TFont.Create;
  fFont.Color := clWhite;
end;


destructor TMyBitmapFont.Destroy;
begin
  fRanges.Free;
  fGlyphs.Free;
  fFont.Free;
  inherited;
end;


function TMyBitmapFont.GetCharWidth(aChar: AnsiChar): Integer;
begin
  if Length(fCharWidths) = 0 then
    ResetCharWidths;
  Result := fCharWidths[Integer(aChar)];
end;


function TMyBitmapFont.CalcCharWidth(aChar: AnsiChar): Integer;
begin
  Result := fHSpace + fHSpaceFix + GetCharWidth(aChar);
end;


function TMyBitmapFont.CalcStringWidth(const aString: AnsiString): Integer;
var I: Integer;
begin
  if aString <> '' then
  begin
    //Calc spaces width. There's no space after last character
    Result := (Length(aString)-1) * (fHSpaceFix+fHSpace);
    for I := 1 to Length(aString) do
      Result := Result + GetCharWidth(aString[I]);
  end
  else
    Result := 0;
end;


procedure TMyBitmapFont.ResetCharWidths(w: Integer = -1);
var I: Integer;
begin
  if Length(fCharWidths) = 0 then
    SetLength(fCharWidths, MAX_CHAR);
  if w < 0 then
    w := fCharWidth;
  for I := 0 to MAX_CHAR - 1 do
    fCharWidths[I] := w;
end;


procedure TMyBitmapFont.SetCharWidths(index, value : Integer);
begin
   fCharWidths[index]:=value;
end;


procedure TMyBitmapFont.SetCharRects(index: Integer; X,Y,Z,W: Single);
begin
  if Length(fCharRects) < MAX_CHAR then
    Setlength(fCharRects, MAX_CHAR);
  fCharRects[index].X := X;
  fCharRects[index].Y := Y;
  fCharRects[index].Z := Z;
  fCharRects[index].W := W;
end;


procedure TMyBitmapFont.PrepareImage;
var
  I, K: Integer;
  p: PByteArray;
  w: array of byte;
begin
  LoadWindowsFont;

  SetLength(w, fGlyphs.Width * fGlyphs.Height * 4);

  for I := 0 to fGlyphs.Height - 1 do
  begin
    p := fGlyphs.Bitmap.ScanLine[fGlyphs.Height-I-1];

    //Make it sharper
    for K := 0 to fGlyphs.Width - 1 do
    begin
      //Take RGB components (cos of sub-pixel AA) and make them sharper by Sqr
      w[(I*Glyphs.Width+K)*4+3] := Round(sqr((p[K*4+0] + p[K*4+1] + p[K*4+2])/765)*255);
      //w[(I*Glyphs.Width+K)*4+3] := (p[K*4+0] + p[K*4+1] + p[K*4+2]) div 3;
      //w[(I*Glyphs.Width+K)*4+3] := p[K*4+1];
    end;
  end;

  if fTexture = 0 then
    glGenTextures(1, @fTexture);

  glBindTexture(GL_TEXTURE_2D, fTexture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

  glTexImage2d(GL_TEXTURE_2D, 0, GL_ALPHA, fGlyphs.Width, fGlyphs.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, w);
end;


procedure TMyBitmapFont.RenderString(const aString : AnsiString; const color : GLUint);
var
  I: Integer;
  topLeft, bottomRight: TVector2f;
  vTopLeft, vBottomRight: TVector2f;
  deltaH, spaceDeltaH: Integer;
  currentChar: AnsiChar;
begin
  if (fGlyphs.Width=0) or (aString='') then exit;

  vTopLeft.X := 0;
  vTopLeft.Y := 0;

  vBottomRight.X := 0;
  vBottomRight.Y := vTopLeft.Y - fCharHeight;

  spaceDeltaH := GetCharWidth(#32) + fHSpaceFix + fHSpace;

  // set states
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_LIGHTING);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glBindTexture(GL_TEXTURE_2D, fTexture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

  // start rendering
  glColor4ubv(@color);
  glBegin(GL_QUADS);
    for I := 1 to Length(aString) do
    begin
      currentChar := aString[I];
      case currentChar of
        #0..#31:  ; //Ignore
        #32:      vTopLeft.X := vTopLeft.X + spaceDeltaH;
        else      deltaH := GetCharWidth(currentChar);
                  if deltaH > 0 then
                  begin
                    GetCharTexCoords(currentChar, topLeft, bottomRight);
                    vBottomRight.X := vTopLeft.X + deltaH;

                    glTexCoord2fv(@topLeft);
                    glVertex2fv(@vTopLeft);

                    glTexCoord2f(topLeft.X, bottomRight.Y);
                    glVertex2f(vTopLeft.X, vBottomRight.Y);

                    glTexCoord2fv(@bottomRight);
                    glVertex2fv(@vBottomRight);

                    glTexCoord2f(bottomRight.X, topLeft.Y);
                    glVertex2f(vBottomRight.X, vTopLeft.Y);

                    vTopLeft.X := vTopLeft.X + deltaH + fHSpace;
                   end;
      end;
    end;
  glEnd;
  glBindTexture(GL_TEXTURE_2D, 0);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;


procedure TMyBitmapFont.GetCharTexCoords(ch: AnsiChar; var topLeft, bottomRight: TVector2f);
begin
  topLeft.X := fCharRects[Integer(ch)].X;
  topLeft.Y := fCharRects[Integer(ch)].Y;
  bottomRight.X := fCharRects[Integer(ch)].Z;
  bottomRight.Y := fCharRects[Integer(ch)].W;
end;


procedure TMyBitmapFont.LoadWindowsFont;
var
   textureWidth, textureHeight : Integer;

   function ComputeCharRects(x, y : Integer; canvas : TCanvas) : Integer;
   var
      px, py, cw, n: Integer;
      rect: TRect;
   begin
      Result:=0;
      n:=0;
      px:=0;
      py:=0;
      while n < MAX_CHAR do
      begin
         cw := fCharWidths[n];
         if cw > 0 then
         begin
            Inc(cw, 2);
            if Assigned(canvas) then
            begin
               SetCharRects(n, (px+1)/textureWidth,
                               (textureHeight-py)/textureHeight,
                               (px+cw-1)/textureWidth,
                               (textureHeight-(py+fCharHeight))/textureHeight);
               rect.Left:=px;
               rect.Top:=py;
               rect.Right:=px+cw;
               rect.Bottom:=py+fCharHeight;
               // Draw the Char, the trailing space is to properly handle the italics.
               canvas.TextRect(rect, px+1, py+1, AnsiChar(n)+' ');
               //canvas.DrawFocusRect(rect);
            end;
            if ((n < MAX_CHAR - 1) and (px+cw+fCharWidths[n+1]+2<=x)) or (n = MAX_CHAR - 1) then
               Inc(px, cw)
            else
            begin
               px := 0;
               Inc(py, fCharHeight);
               if py + fCharHeight > y then
                 Break;
            end;
            Inc(Result);
         end;
         Inc(n);
      end;
   end;
var
   bitmap: TBitmap;
   ch: AnsiChar;
   x, y, i, cw: Integer;
   nbChars, n: Integer;
   texMem, bestTexMem: Integer;

begin
  bitmap := fGlyphs.Bitmap;
  fGlyphs.OnChange := nil;

  bitmap.PixelFormat := pf32bit;
  bitmap.Canvas.Font := fFont;
//bitmap.Canvas.Font.Quality := fqAntialiased;
  bitmap.Canvas.Font.Color := clWhite;

  with bitmap.Canvas do
  begin
    // get characters dimensions for the font
    fCharWidth  := max( max(TextWidth('M'),TextWidth('W')), TextWidth('_')) + 2;
    fCharHeight := TextHeight('"_pI|,') + 2; //Return height does not depends on given chars (WinXP?)

    if fsItalic in Font.Style then
    begin
      // italics aren't properly acknowledged in font width
      fHSpaceFix := -(fCharWidth div 3);
      fCharWidth := fCharWidth-fHSpaceFix;
    end
    else
      fHSpaceFix := 0;
  end;

  nbChars := Ranges.CharacterCount;

  // Retrieve width of all characters (texture width)
  ResetCharWidths(0);
  for I := 0 to Ranges.Count - 1 do
    for ch := Ranges.fRanges[I].First to Ranges.fRanges[I].Last do
    begin
      cw := Bitmap.canvas.TextWidth(String(ch)) - fHSpaceFix;
      SetCharWidths(Integer(ch), cw);
    end;

  // compute texture size: look for best fill ratio
  // and as square a texture as possible
  bestTexMem := MaxInt;
  textureWidth  := 0;
  textureHeight := 0;
  y := MIN_TEX_SIZE;
  while y <= MAX_TEX_SIZE do
  begin
    x := MIN_TEX_SIZE;
    while x <= MAX_TEX_SIZE do
    begin

      n := ComputeCharRects(x, y, nil); //Compute the number of characters that fits
      if n = nbChars then
      begin
        texMem := x * y;
        if (texMem<bestTexMem) or ((texMem=bestTexMem) and (Abs(x-y)<Abs(textureWidth-textureHeight))) then
        begin
          textureWidth  := x;
          textureHeight := y;
          bestTexMem := texMem;
        end;
      end;
      x := x * 2;
    end;
    y := y * 2;
  end;

  if bestTexMem = MaxInt then
  begin
    Font.Size := 6;
    Assert(false, 'Characters are too large or too many. Unable to create font texture.');
  end;

  bitmap.Width  := textureWidth;
  bitmap.Height := textureHeight;

  with bitmap.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clBlack;
    FillRect(Rect(0, 0, textureWidth, textureHeight));
  end;

  ComputeCharRects(textureWidth, textureHeight, bitmap.Canvas);
end;


procedure TMyBitmapFont.LoadFromFile(AFileName: string);
var
  S: TFileStream;
  Count, i: integer;
  F1, F2, F3, F4: single;
  I1: integer;
  Bmp: TBitmap;
begin
  if not FileExists(AFileName) then
  begin
    S := TFileStream.Create(AFileName, fmOpenRead);
    try
      //Load Glyphs
      Bmp := TBitmap.Create;
      Bmp.LoadFromStream(S);
      fGlyphs.Assign(Bmp);
      Bmp.Free;

      //Load font settings
      // char props
      S.Read(I1, SizeOf(fCharWidth));
      fCharWidth := I1;
      S.Read(I1, SizeOf(fCharHeight));
      fCharHeight := I1;
      // char rects
      S.Read(Count, SizeOf(Count));
      SetLength(fCharRects, Count);
      for i := 0 to High(fCharRects) do
      begin
        S.Read(F1, SizeOf(fCharRects[i].X));
        S.Read(F2, SizeOf(fCharRects[i].Y));
        S.Read(F3, SizeOf(fCharRects[i].Z));
        S.Read(F4, SizeOf(fCharRects[i].W));
        SetCharRects(i, F1, F2, F3, F4);
      end;
      // char wds
      S.Read(Count, SizeOf(Count));
      for i := 0 to High(fCharWidths) do
      begin
        S.Read(I1, SizeOf(fCharWidths[i]));
        SetCharWidths(i, I1);
      end;
    finally
      S.Free;
    end;
  end;
end;


procedure TMyBitmapFont.SaveToFile(AFileName: string);
var
  S: TFileStream;
  i, Count: integer;
  Bmp: TBitmap;
begin
  if fGlyphs.Graphic = nil then Exit;

  S := TFileStream.Create(AFileName, fmCreate);
  try
    //Save glyphs
    Bmp := TBitmap.Create;
    Bmp.Assign(fGlyphs.Graphic);
    Bmp.SaveToStream(S);
    Bmp.Free;

    //Save settings
    // char props
    S.Write(fCharWidth, SizeOf(fCharWidth));
    S.Write(fCharHeight, SizeOf(fCharHeight));
    // char rects
    Count := Length(fCharRects);
    S.Write(Count, SizeOf(Count));
    for i := 0 to High(fCharRects) do
      S.Write(fCharRects[i], SizeOf(fCharRects[i]));

    // char widths
    Count := Length(fCharWidths);
    S.Write(Count, SizeOf(Count));
    for i := 0 to High(fCharWidths) do
      S.Write(fCharWidths[i], SizeOf(fCharWidths[i]));
  finally
    S.Free;
  end;
end;


end.
