unit KromUtils;
interface
uses classes, sysutils,windows,Controls,forms,typinfo,ExtCtrls,Math, Dialogs, Registry, ShellApi, shlobj,
  Unit_CommonEvents, Unit_Color, Unit_Vector;

type
  PSingleArray = ^TSingleArray;
  TSingleArray = array[1..1024000] of Single;
  PStringArray = ^TStringArray;
  TStringArray = array[1..256] of String;

procedure FreeThenNil(var Obj);

function int2fix(Number,Len:integer):string;
function float2fix(Number:single; Digits:integer):string;
function int2(c1,c2:char):integer; overload;
function int2(c1,c2,c3,c4:char):integer; overload;
function chr2(x,len:integer):string; overload;
function chr2(t:string; len:integer):string; overload;
procedure Color2RGB(Col:integer; out R,G,B:byte);
function ColorToColor4(Col:TColor4f): TColor4c;

function MakeVector(A,B: Single): TVector2f; overload;
function MakeVector(A,B,C: Single): TVector3f; overload;
function MakeVector(A,B,C: Double): TVector3d; overload;
function MakeVector(A,B,C: Integer): TVector3i; overload;
function MakeVector(A,B,C,D: Double): TVector4d; overload;
function ColorMake(A,B,C,D:single):TColor4f; overload;
function ColorMake(RGB:TColor3f; A:single):TColor4f; overload;
function ColorMake(A,B,C:single):TColor3f; overload;

function Min(const A,B,C: integer):integer; overload;
function Min(const A,B,C: single):single; overload;
function Max(const A,B,C: integer):integer; overload;
function Max(const A,B,C: single):single; overload;

  function GetLengthSQR(ix,iy,iz:integer): integer; //Length without SQRT
  function GetLength(ix,iy,iz:single): single; overload;
  function GetLength(ix:TVector3f): single; overload;
  function GetLength(ix,iy:single): single; overload;

  function InBetween(A,B,X:single): boolean;
  function Lerp(x1,x2,MixValue: Single): Single; overload;
  function Lerp(x1,x2,MixValue: Double): Double; overload;

  procedure Normalize(ix,iy,iz:single; nx,ny,nz:psingle); overload;
  procedure Normalize(var ix,iy,iz:single); overload;
  procedure Normalize(var v:TVector2f); overload;
  procedure Normalize(var v:TVector3f); overload;
  procedure Normalize(var ix,iy:single); overload;
  procedure VectorScale(var v:TVector3f; Scale:single);
  procedure ColorScale(var v:TColor3f; Scale:single); overload;
  procedure ColorScale(var v:TColor4f; Scale:single); overload;

//  function Mix(x1,x2,MixValue:single):single; overload;
//  function Mix(x1,x2:integer; MixValue:single):integer; overload;
//  function Mix(x1,x2:TVector3f; MixValue:single):TVector3f; overload;
  function Mix(ColorA, ColorB: TColor4f; MixValue: Single): TColor4f; overload;

  procedure Matrix2Angles(matrix09:array of single; Qty:integer; i1,i2,i3:pinteger);
  procedure Angles2Matrix(ax,ay,az:single; matrix:pointer; Qty:integer);

  procedure Angles2Vector(degreeX,degreeY,iz:single; out nx,ny,nz:single);

  function DotProduct(x1,y1,z1,x2,y2,z2:single):single; overload;
  function DotProduct(v1,v2:TVector3f):single; overload;
  function Perpendecular2D(v1,v2,v3:TVector3f; Len:single):TVector3f;
  procedure Normal2Poly(v1,v2,v3:array of single; nx,ny,nz:psingle); overload;
  procedure Normal2Poly(v1,v2,v3:TVector3f; n:PVector3f); overload;
  procedure Normal2Poly(u1,v1,u2,v2,u3,v3:single; out n:single); overload;
  function Normal2Poly(v1,v2,v3:TVector3f):TVector3f; overload;

  function ClosestPointOnLine(A,B,aPoint: TVector2f): TVector2f;

  function VectorAdd(v1,v2:TVector3f):TVector3f;
  function VectorRotateAroundX(v: TVector3f; Angle:single):TVector3f;
  function VectorRotateAroundY(v: TVector3f; Angle:single):TVector3f;
  function VectorRotateAroundZ(v: TVector3f; Angle:single):TVector3f;
  function VectorDistance(v1,v2:TVector3f): single;
  function VectorMix(x1,x2:TVector3f; MixValue:single):TVector3f;


procedure decs(var AText:string; const Len:integer=1); overload;
procedure decs(var AText:widestring; const Len:integer=1); overload;
function  decs(AText:string; Len,RunAsFunction:integer):string; overload;

procedure SwapStr(var A,B:string);
procedure SwapInt(var A,B:byte); overload;
procedure SwapInt(var A,B:word); overload;
procedure SwapInt(var A,B:integer); overload;
procedure SwapInt(var A,B:cardinal); overload;
procedure SwapFloat(var A,B:single); overload;
procedure SwapFloat(var A,B:double); overload;
procedure SwapFloat(var A, B: TDateTime); overload;

function MakePOT(num:cardinal):cardinal;
function PerformanceCounter(Sender: TEvent):cardinal;


const
  eol:string=#13+#10; //EndOfLine


implementation


function MakeVector(A,B: Single): TVector2f; overload;
begin
  Result.U := A;
  Result.V := B;
end;


function MakeVector(A,B,C: Single): TVector3f; overload;
begin
  Result.X := A;
  Result.Y := B;
  Result.Z := C;
end;


function MakeVector(A,B,C: Double): TVector3d; overload;
begin
  Result.X := A;
  Result.Y := B;
  Result.Z := C;
end;


function MakeVector(A,B,C: Integer): TVector3i; overload;
begin
  Result.X := A;
  Result.Y := B;
  Result.Z := C;
end;


function MakeVector(A,B,C,D: Double): TVector4d; overload;
begin
  Result.X := A;
  Result.Y := B;
  Result.Z := C;
  Result.W := D;
end;


function ColorMake(A,B,C:single):TColor3f; overload;
begin
  Result.R:=A;
  Result.G:=B;
  Result.B:=C;
end;


function ColorMake(A,B,C,D:single):TColor4f; overload;
begin
  Result.R:=A;
  Result.G:=B;
  Result.B:=C;
  Result.A:=D;
end;


function ColorMake(RGB:TColor3f; A:single):TColor4f; overload;
begin
  Result.R:=RGB.R;
  Result.G:=RGB.G;
  Result.B:=RGB.B;
  Result.A:=A;
end;


function Min(const A,B,C: integer): integer; overload;
begin if A < B then if A < C then Result := A else Result := C
               else if B < C then Result := B else Result := C;
end;

function Min(const A,B,C: single): single; overload;
begin if A < B then if A < C then Result := A else Result := C
               else if B < C then Result := B else Result := C;
end;

function Max(const A,B,C: integer): integer; overload;
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;

function Max(const A,B,C: single): single; overload;
begin if A > B then if A > C then Result := A else Result := C
               else if B > C then Result := B else Result := C;
end;


procedure FreeThenNil(var Obj);
begin
  TObject(Obj).Free;
  Pointer(Obj) := nil;
end;


function ReverseString(s1:string):string;
var s2:string; i:integer;
begin
s2:=s1; //preparing ?
for i:=1 to length(s1) do
s2[i]:=s1[length(s1)-i+1];
ReverseString:=s2;
end;


function int2fix(Number,Len:integer):string;
var ss:string; x:byte;
begin
  ss := inttostr(Number);
  for x:=length(ss) to Len-1 do
    ss := '0' + ss;
  if length(ss)>Len then
    ss:='**********';//ss[99999999]:='0'; //generating an error in lame way
  setlength(ss, Len);
  Result := ss;
end;


function float2fix(Number:single; Digits:integer):string;
begin
  Result := FloatToStrF(Number, ffGeneral, Digits+1, Digits);
end;

function int2(c1,c2:char):integer; overload;
begin int2:=ord(c1)+ord(c2)*256; end;

function int2(c1,c2,c3,c4:char):integer; overload;
var x:integer;
begin if ord(c4)>127 then begin
x:=-(255-ord(c1)+(255-ord(c2))*256+(255-ord(c3))*65536+(255-ord(c4))*16777216);
int2:=x-1;
end else
int2:=ord(c1)+ord(c2)*256+ord(c3)*65536+ord(c4)*16777216; end;

function chr2(x,len:integer):string; overload;
var tmp:integer;
begin tmp:=x;
if x<0 then begin inc(tmp);
chr2:=chr(tmp-1)+char(tmp div 256-1)+char(tmp div 65536-1)+char(tmp div 16777216-1)
end else
chr2:=chr(tmp)+char(tmp div 256)+char(tmp div 65536)+char(tmp div 16777216);
end;

function chr2(t:string; len:integer):string; overload;
var i:integer; begin
for i:=length(t) to len-1 do t:=t+#0;
setlength(t,len);
chr2:=t;
end;

procedure Color2RGB(Col:integer; out R,G,B:byte);
begin
  R := Col AND $FF;
  G := Col AND $FF00 SHR 8;
  B := Col AND $FF0000 SHR 16;
end;


function ColorToColor4(Col:TColor4f): TColor4c;
begin
  Result := Round(Col.R * 255) +
            Round(Col.G * 255) shl 8 +
            Round(Col.B * 255) shl 16 +
            Round(Col.A * 255) shl 24;
end;


procedure Angles2Matrix(ax,ay,az:single; matrix:pointer; qty:integer);
var a,b,c,d,e,f:single;  A1:^single; N:integer;
begin
N:=0;
ax:=ax/180*pi;
ay:=ay/180*pi;
az:=az/180*pi;
a:=cos(ax);
b:=sin(ax);
c:=cos(ay);
d:=sin(ay);
e:=cos(az);//1
f:=sin(az);//0
A1:=pointer(integer(matrix)+N*4); A1^:=C*E;         inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-C*F;        inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=D;           inc(N);
if qty=16 then inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=B*D*E+A*F;   inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-B*D*F+A*E;  inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-B*C;        inc(N);
if qty=16 then inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=-A*D*E+B*F;  inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=A*D*F+B*E;   inc(N);
A1:=pointer(integer(matrix)+N*4); A1^:=A*C;
{        |  CE      -CF       D  |              //  C   0   D      D -BC AC
    M  = |  BDE+AF  -BDF+AE  -BC |              //  BD  A  -BC     0  A  B
         | -ADE+BF   ADF+BE   AC |}             // -AD  B   AC     C BD -AD
end;

procedure Matrix2Angles(matrix09:array of single; Qty:integer; i1,i2,i3:pinteger);
var Ax,Ay,Az:single; Num:byte; a1,a2,a3:integer; m:array[1..9] of single;
begin
if Qty=16 then Num:=1 else Num:=0;
m[1]:=matrix09[0]; m[2]:=matrix09[1]; m[3]:=matrix09[2];
m[4]:=matrix09[3+Num]; m[5]:=matrix09[4+Num]; m[6]:=matrix09[5+Num];
m[7]:=matrix09[6+Num*2]; m[8]:=matrix09[7+Num*2]; m[9]:=matrix09[8+Num*2];

        Ax:=arctan2(-m[6],m[9]);               // -BC : AC
        Az:=arctan2(-m[2],m[1]);               // -CF : CE
        if round(m[9]*1000)<>0 then
          Ay:=arctan2(m[3],m[9]/cos(Ax))       //   D : AC/A
        else
  	    if round(m[2]*1000)<>0 then
            Ay:=arctan2(m[3],-m[6]/sin(Ax))    //   D :-BC/B
          else
            if round(m[1]*1000)<>0 then
              Ay:=arctan2(m[3],m[1]/cos(Az))   //   D : CE/E
            else //if round(m[2]*1000)<>0 then
              Ay:=arctan2(m[3],-m[2]/cos(Az)); //   D :-CF/F

{        |  CE      -CF       D  |    1-2-3
    M  = |  BDE+AF  -BDF+AE  -BC |    4-5-6
         | -ADE+BF   ADF+BE   AC |    7-8-9
         }
a1:=round(Ax*180/pi); if a1>180 then dec(a1,360); if a1<-180 then inc(a1,360);
a2:=round(Ay*180/pi); if a2>180 then dec(a2,360); if a2<-180 then inc(a2,360);
a3:=round(Az*180/pi); if a3>180 then dec(a3,360); if a3<-180 then inc(a3,360);
i1^:=a1;
i2^:=a2;
i3^:=a3;
end;

procedure Angles2Vector(degreeX,degreeY,iz:single; out nx,ny,nz:single);
begin
  nx:=sin(degreeX/180*pi) * cos(degreeY/180*pi);
  ny:=sin(degreeY/180*pi);
  nz:=cos(degreeX/180*pi) * cos(degreeY/180*pi);
end;


//Return closest bigger PowerOfTwo number
function MakePOT(num:cardinal):cardinal;
begin
  num := num - 1;
  num := num OR (num SHR 1);
  num := num OR (num SHR 2);
  num := num OR (num SHR 4);
  num := num OR (num SHR 8);
  num := num OR (num SHR 16); //32bit needs no more
  Result := num + 1;
end;


function GetLengthSQR(ix,iy,iz:integer): integer;
begin
  Result:=sqr(ix)+sqr(iy)+sqr(iz);
end;


function GetLength(ix,iy,iz:single): single; overload;
begin
  Result:=sqrt(sqr(ix)+sqr(iy)+sqr(iz));
end;

function GetLength(ix:TVector3f): single; overload;
begin
  Result:=sqrt(sqr(ix.x)+sqr(ix.y)+sqr(ix.z));
end;

function GetLength(ix,iy:single): single; overload;
begin
  Result:=sqrt(sqr(ix)+sqr(iy));
end;


function InBetween(A,B,X:single): boolean;
begin
  if A>B then
    Result:=(A>X)and(X>B)
  else
  if A<B then
    Result:=(A<X)and(X<B)
  else
    Result:=false
end;


function Lerp(x1,x2,MixValue: Single): Single;
begin
  Result := x1 + (x2 - x1) * MixValue;
end;


function Lerp(x1,x2,MixValue: Double): Double;
begin
  Result := x1 + (x2 - x1) * MixValue;
end;


procedure Normalize(ix,iy,iz:single; nx,ny,nz:psingle);
var len:single;
begin
  len:=sqrt(sqr(ix)+sqr(iy)+sqr(iz));
  if len=0 then len:=1;
  nx^:=ix/len;
  ny^:=iy/len;
  nz^:=iz/len;
end;

procedure Normalize(var ix,iy,iz:single);
var len:single;
begin
len:=sqrt(sqr(ix)+sqr(iy)+sqr(iz));
if len=0 then len:=1;
ix:=ix/len;
iy:=iy/len;
iz:=iz/len;
end;

procedure Normalize(var v:TVector2f);
var len:single;
begin
  len:=sqrt(sqr(v.x)+sqr(v.y));
  if len=0 then len:=1;
  v.x:=v.x/len;
  v.y:=v.y/len;
end;

procedure Normalize(var v:TVector3f);
var len:single;
begin
  len:=sqrt(sqr(v.x)+sqr(v.y)+sqr(v.z));
  if len=0 then len:=1;
  v.x:=v.x/len;
  v.y:=v.y/len;
  v.z:=v.z/len;
end;

procedure Normalize(var ix,iy:single);
var len:single;
begin
len:=sqrt(sqr(ix)+sqr(iy));
if len=0 then len:=1;
ix:=ix/len;
iy:=iy/len;
end;

procedure VectorScale(var v:TVector3f; Scale:single);
begin
  v.x := v.x * Scale;
  v.y := v.y * Scale;
  v.z := v.z * Scale;
end;


procedure ColorScale(var v:TColor3f; Scale:single); overload;
begin
  v.R := v.R * Scale;
  v.G := v.G * Scale;
  v.B := v.B * Scale;
end;


procedure ColorScale(var v:TColor4f; Scale:single); overload;
begin
  v.R := v.R * Scale;
  v.G := v.G * Scale;
  v.B := v.B * Scale;
end;


function Mix(ColorA, ColorB: TColor4f; MixValue: Single): TColor4f; overload;
begin
  Result.R := ColorA.R + (ColorB.R - ColorA.R) * MixValue;
  Result.G := ColorA.G + (ColorB.G - ColorA.G) * MixValue;
  Result.B := ColorA.B + (ColorB.B - ColorA.B) * MixValue;
  Result.A := ColorA.A + (ColorB.A - ColorA.A) * MixValue;
end;

function DotProduct(x1,y1,z1,x2,y2,z2:single):single; overload;
begin
Result:=x1*x2+y1*y2+z1*z2;
end;

function DotProduct(v1,v2:TVector3f):single; overload;
begin
Result:=v1.X*v2.X+v1.Y*v2.Y+v1.Z*v2.Z;
end;

function Perpendecular2D(v1,v2,v3:TVector3f; Len:single):TVector3f;
var Tmp:TVector3f;
begin
  Tmp := MakeVector(v1.X-v3.X, 0, v1.Z-v3.Z);
  Normalize(Tmp);
  Result.X := v2.X-Tmp.Z*Len;
  Result.Y := v2.Y;
  Result.Z := v2.Z+Tmp.X*Len;
end;

procedure Normal2Poly(v1,v2,v3:array of single; nx,ny,nz:psingle); overload;
begin  //aka Cross product of 2 vectors
  nx^:= ((v1[1]-v2[1])*(v1[2]-v3[2])-(v1[2]-v2[2])*(v1[1]-v3[1]))/256;
  ny^:=-((v1[0]-v2[0])*(v1[2]-v3[2])-(v1[2]-v2[2])*(v1[0]-v3[0]))/256;
  nz^:= ((v1[0]-v2[0])*(v1[1]-v3[1])-(v1[1]-v2[1])*(v1[0]-v3[0]))/256;
end;

procedure Normal2Poly(v1,v2,v3:TVector3f; n:PVector3f); overload;
begin  //aka Cross product of 2 vectors
  n^.x:= ((v1.Y-v2.Y)*(v1.Z-v3.Z)-(v1.Z-v2.Z)*(v1.Y-v3.Y));
  n^.y:=-((v1.X-v2.X)*(v1.Z-v3.Z)-(v1.Z-v2.Z)*(v1.X-v3.X));
  n^.z:= ((v1.X-v2.X)*(v1.Y-v3.Y)-(v1.Y-v2.Y)*(v1.X-v3.X));
end;

procedure Normal2Poly(u1,v1,u2,v2,u3,v3:single; out n:single); overload;
begin  //aka Cross product of 2 vectors
  n := (u1-u2)*(v1-v3)-(v1-v2)*(u1-u3);
end;


function Normal2Poly(v1,v2,v3:TVector3f):TVector3f; overload;
begin  //aka Cross product of 2 vectors
  Result.x:= ((v1.Y-v2.Y)*(v1.Z-v3.Z)-(v1.Z-v2.Z)*(v1.Y-v3.Y));
  Result.y:=-((v1.X-v2.X)*(v1.Z-v3.Z)-(v1.Z-v2.Z)*(v1.X-v3.X));
  Result.z:= ((v1.X-v2.X)*(v1.Y-v3.Y)-(v1.Y-v2.Y)*(v1.X-v3.X));
end;


function ClosestPointOnLine(A,B,aPoint: TVector2f): TVector2f;
var
  LineLenSqr, T: Single;
begin
  LineLenSqr := Sqr(B.X - A.X) + Sqr(B.Y - A.Y);

  if LineLenSqr = 0 then //A = B
    Result := A
  else
  begin
    //Consider the line extending the segment as A + T (B - A).
    //T = ((aPoint-A)x(B-A)) / (B-A)^2
    T := ((aPoint.X - A.X) * (B.X - A.X) + (aPoint.Y - A.Y) * (B.Y - A.Y)) / LineLenSqr;

    if T < 0 then
      Result := A //Beyond A
    else
    if T > 1 then
      Result := B //Beyond B
    else
    begin                                     //Within segment
      Result.X := A.X + T * (B.X - A.X);
      Result.Y := A.Y + T * (B.Y - A.Y);
    end;
  end;
end;


function VectorAdd(v1,v2:TVector3f):TVector3f;
begin
  Result.X := v1.X + v2.X;
  Result.Y := v1.Y + v2.Y;
  Result.Z := v1.Z + v2.Z;
end;


function VectorRotateAroundX(v: TVector3f; Angle:single):TVector3f;
var c,s:Extended;
begin
   SinCos(Angle, s, c);
   Result.X:=v.X;
   Result.Y:=c*v.Y+s*v.Z;
   Result.Z:=c*v.Z-s*v.Y;
end;


function VectorRotateAroundY(v: TVector3f; Angle:single):TVector3f;
var c,s:Extended;
begin
   SinCos(Angle, s, c);
   Result.Y:=v.Y;
   Result.X:=c*v.X+s*v.Z;
   Result.Z:=c*v.Z-s*v.X;
end;


function VectorRotateAroundZ(v: TVector3f; Angle:single):TVector3f;
var c,s:Extended;
begin
   SinCos(Angle, s, c);
   Result.X:=c*v.X+s*v.Y;
   Result.Y:=c*v.Y-s*v.X;
   Result.Z:=v.Z;
end;


function VectorDistance(v1,v2:TVector3f):single;
begin
  Result := sqrt(sqr(v1.X-v2.X)+sqr(v1.Y-v2.Y)+sqr(v1.Z-v2.Z));
end;


function VectorMix(x1,x2:TVector3f; MixValue:single):TVector3f;
begin
  Result.X := x1.X*(1-MixValue) + x2.X*MixValue;
  Result.Y := x1.Y*(1-MixValue) + x2.Y*MixValue;
  Result.Z := x1.Z*(1-MixValue) + x2.Z*MixValue;
end;


procedure decs(var AText:string; const Len:integer=1);
begin
if length(AText)<=abs(Len) then Atext:=''
else
if Len>=0 then AText:=Copy(AText, 1, length(AText)-Len)
          else AText:=Copy(AText, 1+abs(Len), length(AText)-abs(Len));
end;

procedure decs(var AText:widestring; const Len:integer=1);
begin
if length(AText)<=abs(Len) then Atext:=''
else
if Len>=0 then AText:=Copy(AText, 1, length(AText)-Len)
          else AText:=Copy(AText, 1+abs(Len), length(AText)-abs(Len));
end;

function decs(AText:string; Len,RunAsFunction:integer):string; overload;
begin
if length(AText)<=abs(Len) then result:=''
else
if Len>=0 then result:=Copy(AText, 1, length(AText)-Len)
          else result:=Copy(AText, 1+abs(Len), length(AText)-abs(Len));
end;


procedure SwapStr(var A,B:string);
var s:string;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A,B:byte);
var s:byte;
begin
  s:=A; A:=B; B:=s;
end;


procedure SwapInt(var A,B:word);
var s:word;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A,B:integer);
var s:integer;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapInt(var A,B:cardinal);
var s:cardinal;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapFloat(var A,B:single);
var s:single;
begin
  s:=A; A:=B; B:=s;
end;

procedure SwapFloat(var A,B:double);
var s:double;
begin
  s:=A; A:=B; B:=s;
end;


procedure SwapFloat(var A, B: TDateTime);
var t: TDateTime;
begin
  t := A;
  A := B;
  B := t;
end;


function PerformanceCounter(Sender: TEvent):cardinal;
var
  counter : Int64;
  Seconds : Double;
  pf, pc1, pc2 : Int64;
begin
  Seconds := 0.0;
  counter := 0;
  QueryPerformanceFrequency(pf);
  QueryPerformanceCounter(pc2);

  repeat
    pc1 := pc2;

    Sender;

    QueryPerformanceCounter(pc2);
    inc(counter);
    Seconds := Seconds + (pc2 - pc1) / pf;
  until (Seconds >= 0.1);

  Result := counter;
end;


end.
