unit Unit_ColorCoder;
interface
uses
  dglOpenGL;

type
  TColorCode = (ccNone, ccIngot, ccPiece);  //1..31 are ok

  procedure SetColorCode(aCode: TColorCode; aId: Integer);
  procedure GetColorCode(RGBColor: Pointer; var aCode: TColorCode; var aId: Integer);


implementation


procedure SetColorCode(aCode: TColorCode; aId: Integer);
begin
  glColor4ub(aId mod 256,
            (aId mod 65536) div 256,    // 1,2,4(524288) 8,16,32,64,128 //0..31
            (aId mod 524288) div 65536 + Byte(aCode) * 8, 255);
end;


procedure GetColorCode(RGBColor: Pointer; var aCode: TColorCode; var aId: Integer);
begin
  aId := PWord(Cardinal(RGBColor))^ + ((PByte(Cardinal(RGBColor) + 2)^) mod 8) * 65536;
  aCode := TColorCode((PByte(Cardinal(RGBColor) + 2)^) div 8);
end;


end.
