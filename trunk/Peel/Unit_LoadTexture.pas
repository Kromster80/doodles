unit Unit_LoadTexture;
interface
uses
  Windows, Classes, Graphics, JPEG, SysUtils, dglOpenGL, PNGImage, StrUtils;


type
  TLTexture = record
    ID: Cardinal;
    U, V: Single;
    X, Y: Word;
  end;

  function LoadTexture(Filename: string; aFromRes: Boolean; var Texture: TLTexture): Boolean;
  function LoadTextureTGAMask(Filename: string; pMask: Pointer; aHeight, aWidth: Integer): Boolean;


implementation


function CreateTexture: TLTexture; overload;
begin
  Result.ID := 0;
  Result.U := 0;
  Result.V := 0;
  Result.X := 0;
  Result.Y := 0;
end;


function CreateTexture(aWidth, aHeight, aFormat: Word; aComponents: Byte; aFilter: Boolean; pData: Pointer): TLTexture; overload;
  function RoundToPOT(aValue: Word): Word;
  begin
    aValue := aValue - 1;
    aValue := aValue OR (aValue SHR 1);
    aValue := aValue OR (aValue SHR 2);
    aValue := aValue OR (aValue SHR 4);
    aValue := aValue OR (aValue SHR 8);
    aValue := aValue OR (aValue SHR 16);
    Result := aValue + 1;
  end;
var
  Texture: GLuint;
  WidthPOT,HeightPOT: Integer;
  wData: array of byte;
  I: Word;
begin
  WidthPOT  := RoundToPOT(aWidth);
  HeightPOT := RoundToPOT(aHeight);

  //Copy memory and append 1 px edge to avoid black edges caused by interpolation
  if (WidthPOT<>aWidth) or (HeightPOT<>aHeight) then
  begin
    SetLength(wData, WidthPOT*HeightPOT*aComponents);
    for I := 0 to aHeight - 1 do
    begin
      Move(PByte(Cardinal(pData)+(I*aWidth)*aComponents)^, wData[I*WidthPOT*aComponents], aComponents*aWidth);
      Move(wData[(I*WidthPOT+aWidth-1)*aComponents], wData[(I*WidthPOT+aWidth)*aComponents], aComponents);
    end;
    if HeightPOT <> aHeight then
      Move(PByte(Cardinal(pData)+Cardinal((aHeight-1)*aWidth)*aComponents)^, wData[aHeight*WidthPOT*aComponents], aComponents*aWidth);
    pData := wData;
  end;

  glGenTextures(1, @Texture);
  glBindTexture(GL_TEXTURE_2D, Texture);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE); //Blends background
  //glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL); //No blending

  //GL_NEAREST               - Basic texture (grainy looking texture)
  //GL_LINEAR                - BiLinear filtering
  //GL_LINEAR_MIPMAP_NEAREST - Basic mipmapped texture
  //GL_LINEAR_MIPMAP_LINEAR  - BiLinear Mipmapped texture

  //if aFilter then
  //  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR) { only first two can be used }
  //else
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); { only first two can be used }
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  {case aFormat of //Force OpenGL to build mipMaps
    GL_RGBA, GL_BGRA: gluBuild2DMipmaps(GL_TEXTURE_2D, aComponents, WidthPOT, HeightPOT, aFormat, GL_UNSIGNED_BYTE, pData);
    GL_RGB, GL_BGR:  gluBuild2DMipmaps(GL_TEXTURE_2D, aComponents, WidthPOT, HeightPOT, aFormat, GL_UNSIGNED_BYTE, pData);
  end;}

  case aFormat of // Use when not wanting mipmaps to be built by openGL
    GL_RGBA, GL_BGRA: glTexImage2D(GL_TEXTURE_2D, 0, aComponents, WidthPOT, HeightPOT, 0, aFormat, GL_UNSIGNED_BYTE, pData);
    GL_RGB,  GL_BGR:  glTexImage2D(GL_TEXTURE_2D, 0, aComponents, WidthPOT, HeightPOT, 0, aFormat, GL_UNSIGNED_BYTE, pData);
  end;

  Result.ID := Texture;
  Result.U  := aWidth/WidthPOT;
  Result.V  := aHeight/HeightPOT;
  Result.X  := aWidth;
  Result.Y  := aHeight;
end;


function LoadTextureBMP(Filename: String; var Texture: TLTexture):boolean;
var
  pData: Pointer;
  Width: LongWord;
  Height: LongWord;
  bpp:byte;

  FileHeader: TBITMAPFILEHEADER;
  InfoHeader: TBITMAPINFOHEADER;
  Palette: array of RGBQUAD;
  BitmapFile: THandle;
  BitmapLength: LongWord;
  PaletteLength: LongWord;
  ReadBytes: LongWord;
begin
  if not FileExists(Filename) then
  begin
    {$IFNDEF VER_ACTIVEX}
    MessageBox(0, PChar('File not found  - ' + Filename), PChar('BMP Unit'), MB_OK);
    {$ENDIF}
    Result:= false;
    exit;
  end;

  BitmapFile := CreateFile(PChar(Filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
  if (BitmapFile = INVALID_HANDLE_VALUE) then
  begin
    MessageBox(0, PChar('Error opening "' + Filename), PChar('BMP Unit'), MB_OK);
    Result := false;
    Exit;
  end;

  // Get header information
  ReadFile(BitmapFile, FileHeader, SizeOf(FileHeader), ReadBytes, nil);
  ReadFile(BitmapFile, InfoHeader, SizeOf(InfoHeader), ReadBytes, nil);

  // Get palette
  PaletteLength := InfoHeader.biClrUsed;
  SetLength(Palette, PaletteLength);
  ReadFile(BitmapFile, Palette, PaletteLength, ReadBytes, nil);
  if (ReadBytes <> PaletteLength) then begin
    MessageBox(0, PChar('Error reading palette'), PChar('BMP Unit'), MB_OK);
    Result := false;
    Exit;
  end;

  Width  := InfoHeader.biWidth;
  Height := InfoHeader.biHeight;
  bpp    := InfoHeader.biBitCount;
  BitmapLength := InfoHeader.biSizeImage;
  if BitmapLength = 0 then
    BitmapLength := Width * Height * InfoHeader.biBitCount Div 8;

  // Get the actual pixel data
  GetMem(pData, BitmapLength);
  ReadFile(BitmapFile, pData^, BitmapLength, ReadBytes, nil);
  if (ReadBytes <> BitmapLength) then begin
    MessageBox(0, PChar('Error reading bitmap data'), PChar('BMP Unit'), MB_OK);
    Result := false;
    Exit;
  end;
  CloseHandle(BitmapFile);

  case bpp of
    24: Texture := CreateTexture(Width, Height, GL_BGR,  3, True, pData);
    32: Texture := CreateTexture(Width, Height, GL_BGRA, 4, True, pData);
    else Texture := CreateTexture();
  end;

  FreeMem(pData);
  Result := true;
end;


function LoadTexturePNG(Filename: string; aFromRes: Boolean; var Texture: TLTexture): Boolean;
type
  TCardinalArray  = array[0..$effffff] of Cardinal;
  PCardinalArray = ^TCardinalArray;
var
  pData: PCardinalArray;
  T: Cardinal;
  I,K: Integer;
  Width: Integer;
  Height: Integer;
  PNG: TPNGObject;
  rStream: TResourceStream;
begin
  if not (aFromRes or FileExists(Filename)) then
  begin
    {$IFNDEF VER_ACTIVEX}
    MessageBox(0, PChar('File not found  - ' + Filename), PChar('BMP Unit'), MB_OK);
    {$ENDIF}
    Result := False;
    Exit;
  end;

  PNG := TPNGObject.Create;
  if aFromRes then
  begin
    rStream := TResourceStream.Create(hInstance, Filename, RT_RCDATA);
    PNG.LoadFromStream(rStream);
    rStream.Free;
  end
  else
    PNG.LoadFromFile(Filename);

  Width  := PNG.Width;
  Height := PNG.Height;

  // Get the actual pixel data
  GetMem(pData, Width * Height * 4);

  //There are ways to process PNG transparency
  case PNG.TransparencyMode of
    ptmNone:
      for I := 0 to Height - 1 do for K := 0 to Width - 1 do
        pData[I * Width + K] := cardinal(PNG.Pixels[K, Height - I - 1]) OR $FF000000;
    ptmBit:
      for I := 0 to Height - 1 do for K := 0 to Width - 1 do
        if PNG.Pixels[K, Height - I - 1] = PNG.TransparentColor then
          pData[I * Width + K] := cardinal(PNG.Pixels[K, Height - I - 1]) AND $FFFFFF //avoid black edging
        else
          pData[I * Width + K] := cardinal(PNG.Pixels[K, Height - I - 1]) OR $FF000000;
    ptmPartial:
      for I := 0 to Height - 1 do for K := 0 to Width - 1 do
      begin
        T := (PNG.AlphaScanline[Height - I - 1]^[K]) shl 24; //semitransparency is killed by render later-on
        pData[I * Width + K] := cardinal(PNG.Pixels[K, Height - I - 1]) OR T;
      end;
    else Assert(false, 'Unknown PNG transparency mode')
  end;

  Texture := CreateTexture(Width, Height, GL_RGBA, 4, False, pData);

  {case PNG.TransparencyMode of
    ptmNone: Texture := CreateTexture(Width, Height, GL_BGR,  4, False, pData);
    else     Texture := CreateTexture(Width, Height, GL_BGRA, 4, False, pData);
  end;}

  PNG.Free;

  FreeMem(pData);
  Result := True;
end;


function LoadTextureJPG(Filename: string; var Texture: TLTexture): Boolean;
var
  JPGi: TJPEGImage;
  BMPi: TBitmap;
  pData: array of Cardinal;
  I,K: Integer;
  Line: ^Cardinal;
begin
  result := False;
  JPGi := TJPEGImage.Create;

  if not FileExists(Filename) then
  begin
    //MyMessageBox(0, PChar('File not found  - ' + Filename), PChar('JPG Texture'), MB_OK);

    setlength(pData, 64*32); //size in bytes
    for I := 0 to Length(pData) - 1 do
      pData[I]:=random($FFFFFF);
    Texture := CreateTexture(64, 32, GL_RGB, 3, True, @pData[0]);
    JPGi.Free;
    exit;
  end;

  JPGi.LoadFromFile(Filename);

  BMPi := TBitmap.Create;
  BMPi.pixelformat := pf32bit;
  BMPi.Width  := JPGi.Width;
  BMPi.Height := JPGi.Height;
  BMPi.canvas.draw(0,0,JPGi); //Copy JPEG to Bitmap

  SetLength(pData, BMPi.Width*BMPi.Height); //size in bytes

  //Move TBitmap data to usable array
  for I := 0 to BMPi.Height - 1 do
  begin
    Line := BMPi.ScanLine[I];
    for K := 0 to BMPi.Width - 1 do
    begin
      //Flip vertically and set Alpha
      pData[(BMPi.Height - I - 1) * BMPi.Width + K] := Line^ OR $FF000000;
      inc(Line);
    end;
  end;

  Texture := CreateTexture(BMPi.Width, BMPi.Height, GL_BGRA, 4, True, @pData[0]);

  JPGi.Free;
  BMPi.Free;
  Result := true;
end;


function LoadTextureTGA(Filename: string; aFromRes: Boolean; var Texture : TLTexture): Boolean;
    // Copy a pixel from source to dest and Swap the RGB color values
    procedure CopySwapPixel( const Source, Destination : Pointer);
    asm
      push ebx
      mov bl,[eax+2]
      mov bh,[eax+1]
      mov [edx+0],bl
      mov [edx+1],bh
      mov bl,[eax+0]
      mov bh,[eax+3]
      mov [edx+2],bl
      mov [edx+3],bh
      pop ebx
    end;
var
  TGAHeader : packed record   // Header type for TGA images
    FileType     : Byte;
    ColorMapType : Byte;
    ImageType    : Byte;
    ColorMapSpec : Array[0..4] of Byte;
    OrigX  : Array [0..1] of Byte;
    OrigY  : Array [0..1] of Byte;
    Width  : Array [0..1] of Byte;
    Height : Array [0..1] of Byte;
    BPP    : Byte;
    ImageInfo : Byte;
  end;
//  TGAFile   : File;
  bytesRead : Integer;
  Image     : Pointer;
  CompImage : Pointer;
  Width,Height : Integer;
  ColorDepth   : Integer;
  ImageSize    : Integer;
  BufferIndex : Integer;
  CurrentByte : Integer;
  CurrentPixel : Integer;
  CompSize:integer;
  I : Integer;
  Front: ^Byte;
  rStream: TResourceStream;
  MemStream: TMemoryStream;
begin
  Result := False;

  if not (aFromRes or FileExists(Filename)) then
  begin
    {$IFNDEF VER_ACTIVEX}
    MessageBox(0, PChar('File not found  - ' + Filename), PChar('TGA Texture'), MB_OK);
    {$ENDIF}
    Exit;
  end;

  MemStream := TMemoryStream.Create;
  try
    if aFromRes then
    begin
      rStream := TResourceStream.Create(hInstance, Filename, RT_RCDATA);
      MemStream.CopyFrom(rStream, rStream.Size);
      rStream.Free;
    end
    else
      MemStream.LoadFromFile(Filename);

    MemStream.Position := 0;
    MemStream.Read(TGAHeader, SizeOf(TGAHeader));

    // Only support 24, 32 bit images
    if not (TGAHeader.ImageType in [2, 10]) then //TGA_RGB, Compressed RGB
    begin
      MessageBox(0, PChar('Couldn''t load "' + Filename + '". Only 24 and 32bit TGA supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

    // Don't support colormapped files
    if TGAHeader.ColorMapType <> 0 then
    begin
      MessageBox(0, PChar('Couldn''t load "' + Filename + '". Colormapped TGA files not supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

    if not (TGAHeader.BPP in [24, 32]) then
    begin
      MessageBox(0, PChar('Couldn''t load "' + Filename + '". Only 24 and 32 bit TGA files supported.'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

    // Get the width, height, and color depth
    Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
    Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
    ColorDepth := TGAHeader.BPP;
    ImageSize := Width * Height * (ColorDepth div 8);

    if TGAHeader.ImageType = 2 then   // Standard 24, 32 bit TGA file
    begin
      GetMem(Image, ImageSize);
      bytesRead := MemStream.Read(image^, ImageSize);
      if bytesRead <> ImageSize then
      begin
        FreeMem(Image);
        MessageBox(0, PChar('Couldn''t read file "'+ Filename +'".'), PChar('TGA File Error'), MB_OK);
        Exit;
      end;

      case TGAHeader.BPP of
        24: Texture := CreateTexture(Width, Height, GL_BGR,  3, true, Image);
        32: Texture := CreateTexture(Width, Height, GL_BGRA, 4, True, Image);
      end;
      FreeMem(Image);
    end;

    // Compressed 24, 32 bit TGA files
    if TGAHeader.ImageType = 10 then
    begin
      ColorDepth := ColorDepth div 8;
      CompSize := MemStream.Size - SizeOf(TGAHeader);

      GetMem(CompImage, CompSize);
      bytesRead := MemStream.Read(CompImage^, CompSize);   // load compressed data into memory
      if bytesRead <> CompSize then
      begin
        FreeMem(CompImage);
        MessageBox(0, PChar('Couldn''t read file "'+ Filename +'".'), PChar('TGA File Error'), MB_OK);
        Exit;
      end;

      CurrentByte  := 0;
      CurrentPixel := 0;
      BufferIndex  := 0;
      GetMem(Image, ImageSize + 100); //+100 fixes memory access overlap caused by decompressing

      // Extract pixel information from compressed data
      repeat
        Front := Pointer(Integer(CompImage) + BufferIndex);
        Inc(BufferIndex);
        if Front^ < 128 then
        begin
        for I := 0 to Front^ do
          begin
            CopySwapPixel(Pointer(Integer(CompImage)+BufferIndex+I*ColorDepth), Pointer(Integer(image)+CurrentByte));
            CurrentByte := CurrentByte + ColorDepth;
            inc(CurrentPixel);
          end;
          BufferIndex := BufferIndex + (Front^+1)*ColorDepth
        end
        else
        begin
        for I := 0 to Front^ -128 do
          begin
            CopySwapPixel(Pointer(Integer(CompImage)+BufferIndex), Pointer(Integer(image)+CurrentByte));
            CurrentByte := CurrentByte + ColorDepth;
            inc(CurrentPixel);
          end;
          BufferIndex := BufferIndex + ColorDepth
        end;
      until(CurrentPixel >= Width*Height);

      case TGAHeader.BPP of
        24: Texture := CreateTexture(Width, Height, GL_RGB,  3, True, Image);
        32: Texture := CreateTexture(Width, Height, GL_RGBA, 4, True, Image);
      end;

      FreeMem(CompImage);
      FreeMem(Image);
    end;

    Result := True; //Everything worked
  finally
    MemStream.Free;
  end;
end;


function LoadTextureTGAMask(Filename: string; pMask:pointer; aHeight, aWidth:integer):boolean;
  procedure CreateMask(aImage:pointer; aWidth, aHeight:word; aBPP:byte; aMask:pointer; bWidth, bHeight:word);
  var
    I, K: Word;
    Px, Py: Word;
    P: Cardinal;
    T: Boolean;
  begin
    for I:=0 to bHeight-1 do
    begin
      Py := round(I / (bHeight-1) * (aHeight-1));
      for K:=0 to bWidth-1 do
      begin
        Px := round(K / (bWidth-1) * (aWidth-1));
        P := (Py*aWidth + Px)*aBPP;

        //Test RGB components
        T := (pbyte(cardinal(aImage) + P  )^ <> 0) or
             (pbyte(cardinal(aImage) + P+1)^ <> 0) or
             (pbyte(cardinal(aImage) + P+2)^ <> 0);

        pbyte(cardinal(aMask)+cardinal(bHeight-1-I)*(bWidth)+K)^ := byte(T);
      end;
    end;
  end;

var
  TGAHeader : packed record   // Header type for TGA images
    FileType     : Byte;
    ColorMapType : Byte;
    ImageType    : Byte;
    ColorMapSpec : Array[0..4] of Byte;
    OrigX  : Array [0..1] of Byte;
    OrigY  : Array [0..1] of Byte;
    Width  : Array [0..1] of Byte;
    Height : Array [0..1] of Byte;
    BPP    : Byte;
    ImageInfo : Byte;
  end;
  TGAFile   : File;
  bytesRead : Integer;
  Image     : Pointer;
  Width,Height : Integer;
  ColorDepth   : Integer;
  ImageSize    : Integer;
begin
  Result := false;

  if FileExists(Filename) then
  begin
    AssignFile(TGAFile, Filename);
    FileMode:=0; Reset(TGAFile,1); FileMode:=2; //Open ReadOnly
    BlockRead(TGAFile, TGAHeader, SizeOf(TGAHeader));
  end else
  begin
    MessageBox(0, PChar('File not found  - ' + Filename), PChar('TGA Texture'), MB_OK);
    Exit;
  end;

  // Only support 24, 32 bit images
  if TGAHeader.ImageType <> 2 then //TGA_RGB
  begin
    CloseFile(TGAFile);
    MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 24 and 32bit TGA supported.'), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  // Don't support colormapped files
  if TGAHeader.ColorMapType <> 0 then
  begin
    CloseFile(TGAFile);
    MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Colormapped TGA files not supported.'), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  if not (TGAHeader.BPP in [24,32]) then
  begin
    CloseFile(TGAFile);
    MessageBox(0, PChar('Couldn''t load "'+ Filename +'". Only 24 and 32 bit TGA files supported.'), PChar('TGA File Error'), MB_OK);
    Exit;
  end;

  // Get the width, height, and color depth
  Width  := TGAHeader.Width[0]  + TGAHeader.Width[1]  * 256;
  Height := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;
  ColorDepth := TGAHeader.BPP;
  ImageSize  := Width*Height*(ColorDepth div 8);

  if TGAHeader.ImageType = 2 then   // Standard 24, 32 bit TGA file
  begin
    GetMem(Image, ImageSize);
    BlockRead(TGAFile, image^, ImageSize, bytesRead);
    CloseFile(TGAFile);
    if bytesRead <> ImageSize then
    begin
      CloseFile(TGAFile);
      FreeMem(Image);
      MessageBox(0, PChar('Couldn''t read file "'+ Filename +'".'), PChar('TGA File Error'), MB_OK);
      Exit;
    end;

    case TGAHeader.BPP of
      24: CreateMask(Image, Width, Height, 3, pMask, aWidth, aHeight);
      32: CreateMask(Image, Width, Height, 4, pMask, aWidth, aHeight);
    end;
    FreeMem(Image);
  end;

  Result := true; //Everything worked
end;


function LoadTexture(Filename: string; aFromRes: Boolean; var Texture: TLTexture): Boolean;
begin
  if aFromRes then
  begin
    Filename := ExtractFileName(Filename);
    Filename := StringReplace(Filename, '.', '_', [rfReplaceAll, rfIgnoreCase]);
    Filename := StringReplace(Filename, '.', '_', [rfReplaceAll, rfIgnoreCase]);
  end;

  if SameText(RightStr(Filename, 3), 'bmp') then
    Result := LoadTextureBMP(Filename, Texture)
  else
  if SameText(RightStr(Filename, 3), 'jpg') then
    Result := LoadTextureJPG(Filename, Texture)
  else
  if SameText(RightStr(Filename, 3), 'png') then
    Result := LoadTexturePNG(Filename, aFromRes, Texture)
  else
  if SameText(RightStr(Filename, 3), 'tga') then
    Result := LoadTextureTGA(Filename, aFromRes, Texture)
  else
    Result := False;
end;


end.
