unit Unit_ColorCoder;
interface
uses
  dglOpenGL;

type
  TColorCode = (ccNone, ccIngot, ccPiece);  //1..31 are ok

  TColorCodeId = record
    Code: TColorCode;
    Id: Integer;
  end;

  procedure SetColorCode(aCode: TColorCode; aId: Integer);
  function GetColorCode(RGBColor: Cardinal): TColorCodeId;


const
  CodeString: array [TColorCode] of string = ('none', 'Ingot', 'Piece');


implementation


procedure SetColorCode(aCode: TColorCode; aId: Integer);
begin
  glColor4ub(aId mod 256,
            (aId mod 65536) div 256,    // 1,2,4(524288) 8,16,32,64,128 //0..31
            (aId mod 524288) div 65536 + Byte(aCode) * 8, 255);
end;


function GetColorCode(RGBColor: Cardinal): TColorCodeId;
begin
  Result.Id := RGBColor and $07FFFF;
  Result.Code := TColorCode(RGBColor shr 19);
end;


end.
