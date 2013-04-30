unit Unit_ControlsRender;
interface
uses
  Classes, Controls, dglOpenGL, Math, SysUtils, Windows,
  Unit_LoadTexture, Unit_Color;

type
  TImageRenderStyle = (
    isCenter,  //Position in center
    isFit,     //Fit picture without cropping
    isStretch, //Non-proportional stretch
//    isBleed,   //Center the picture and bleed it's edges
    is3TapV,   //9-tap vertical only
    is3TapH,   //9-tap vertical only
    is9Tap,    //9-tap render
    isTile     //tile image

    );

  procedure RenderLine(x,y,w,h: Single; aWidth: Single; aCol: TColor4c);
  procedure RenderCircle(X,Y,Rad,Width: Single; aCol: TColor4c);
  procedure RenderCircleFill(X,Y,Rad: Single; aCol: TColor4c);
  procedure RenderQuad(Mode: glUint; x,y,w,h: Single; aCol: TColor4c);
  procedure RenderTex(Tex: TLTexture; aStyle: TImageRenderStyle; X,Y,W,H: Single; aCol: TColor4c);
  procedure RenderShadowQuad(X,Y,W,H: Single; aBlur: Word; aCol: TColor4c);

  procedure ScissorQuad; overload;
  procedure ScissorQuad(X,Y,W,H: Single); overload;


implementation


procedure RenderLine(x,y,w,h:single; aWidth:single; aCol:TColor4c);
begin
  glPushAttrib(GL_LINE_BIT);
    glLineWidth(max(aWidth,0.1)); //0 is not allowed
    glColor4ubv(@aCol);
    glBegin(GL_LINES);
      glVertex2f(x,y);
      glVertex2f(x+w,y+h);
    glEnd;
  glPopAttrib;
end;


procedure RenderCircle(X,Y,Rad,Width: Single; aCol: TColor4c);
var I: Integer; Ang: Single;
begin
  glPushAttrib(GL_LINE_BIT);
    glLineWidth(max(Width, 0.1)); //0 is not allowed
    glColor4ubv(@aCol);
    glBegin(GL_LINE_LOOP);
      for I := 0 to 15 do
      begin
        Ang := I/8*pi;
        glVertex2f(X + Sin(Ang) * Rad, Y + Cos(Ang) * Rad);
      end;
    glEnd;
  glPopAttrib;
end;


procedure RenderCircleFill(X,Y,Rad: Single; aCol: TColor4c);
var I: Integer; Ang: Single;
begin
  glColor4ubv(@aCol);
  glBegin(GL_POLYGON);
    for I := 0 to 15 do
    begin
      Ang := I/8*pi;
      glVertex2f(X + Sin(Ang) * Rad, Y + Cos(Ang) * Rad);
    end;
  glEnd;
end;


procedure RenderQuad(Mode: glUint; x,y,w,h:single; aCol:TColor4c);
begin
  glColor4ubv(@aCol);
  glBegin(Mode);
    glVertex2f(x,y);
    glVertex2f(x,y+h);
    glVertex2f(x+w,y+h);
    glVertex2f(x+w,y);
  glEnd;
end;


procedure RenderTex(Tex: TLTexture; aStyle: TImageRenderStyle; X,Y,W,H: Single; aCol: TColor4c);
  procedure DoSub(x1,x2,u1,u2,y1,y2,v1,v2: Single);
  begin
    glTexCoord2f(Tex.U * u1, Tex.V * v1); glVertex2f(x1,y1);
    glTexCoord2f(Tex.U * u1, Tex.V * v2); glVertex2f(x1,y2);
    glTexCoord2f(Tex.U * u2, Tex.V * v2); glVertex2f(x2,y2);
    glTexCoord2f(Tex.U * u2, Tex.V * v1); glVertex2f(x2,y1);
  end;
const
  DEF_TEX_INS = 10;
var
  ShiftX, ShiftY, ImageSize, InsX, InsY: Single;
begin
  if Tex.ID = 0 then Exit;

  glPushMatrix;

    glTranslatef(X, Y, 0);
    glBindTexture(GL_TEXTURE_2D, Tex.ID);
    glColor4ubv(@aCol);
      case aStyle of
        isCenter:
            begin
              ShiftX := Round((W - Tex.X) / 2);
              ShiftY := Round((H - Tex.Y) / 2);
              glBegin(GL_QUADS);
                glTexCoord2f(0,     Tex.V); glVertex2f(ShiftX,         ShiftY);
                glTexCoord2f(0,         0); glVertex2f(ShiftX,         ShiftY + Tex.Y);
                glTexCoord2f(Tex.U,     0); glVertex2f(ShiftX + Tex.X, ShiftY + Tex.Y);
                glTexCoord2f(Tex.U, Tex.V); glVertex2f(ShiftX + Tex.X, ShiftY);
              glEnd;
            end;
        isFit:
            begin
              ImageSize := Min(W, H) / 2;
              glBegin(GL_QUADS);
                glTexCoord2f(0,     Tex.V); glVertex2f(W/2-ImageSize, H/2-ImageSize);
                glTexCoord2f(0,         0); glVertex2f(W/2-ImageSize, H/2+ImageSize);
                glTexCoord2f(Tex.U,     0); glVertex2f(W/2+ImageSize, H/2+ImageSize);
                glTexCoord2f(Tex.U, Tex.V); glVertex2f(W/2+ImageSize, H/2-ImageSize);
              glEnd;
            end;
        isStretch:
            begin
              glBegin(GL_QUADS);
                glTexCoord2f(0,     Tex.V); glVertex2f(0, 0);
                glTexCoord2f(0,         0); glVertex2f(0, H);
                glTexCoord2f(Tex.U,     0); glVertex2f(W, H);
                glTexCoord2f(Tex.U, Tex.V); glVertex2f(W, 0);
              glEnd;
            end;
        is3TapV, is3TapH, is9Tap:
            begin
              InsX := W / 2;
              InsY := H / 2;
              if aStyle = is3TapV then
                InsY := Min(H / 3, InsX * 2)
              else
              if aStyle = is3TapH then
                InsX := Min(W / 3, InsY * 2)
              else
              begin
                InsX := Min(DEF_TEX_INS, W/2);
                InsY := Min(DEF_TEX_INS, H/2);
              end;
              glBegin(GL_QUADS);
                //Top row
                DoSub(0,      InsX,   0,  0.5, 0,      InsY,   1,0.5);
                DoSub(InsX,   W-InsX, 0.5,0.5, 0,      InsY,   1,0.5);
                DoSub(W-InsX, W,      0.5,1,   0,      InsY,   1,0.5);
                //Middle row
                DoSub(0,      InsX,   0,  0.5, InsY,   H-InsY, 0.5,0.5);
                DoSub(InsX,   W-InsX, 0.5,0.5, InsY,   H-InsY, 0.5,0.5);
                DoSub(W-InsX, W,      0.5,1,   InsY,   H-InsY, 0.5,0.5);
                //Bottom row
                DoSub(0,      InsX,   0,  0.5, H-InsY, H,      0.5,0);
                DoSub(InsX,   W-InsX, 0.5,0.5, H-InsY, H,      0.5,0);
                DoSub(W-InsX, W,      0.5,1,   H-InsY, H,      0.5,0);
              glEnd;
            end;
        isTile:
            begin
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
              glBegin(GL_QUADS);
                glTexCoord2f(        0, H / Tex.Y); glVertex2f(0, 0);
                glTexCoord2f(        0,         0); glVertex2f(0, H);
                glTexCoord2f(W / Tex.X,         0); glVertex2f(W, H);
                glTexCoord2f(W / Tex.X, H / Tex.Y); glVertex2f(W, 0);
              glEnd;
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
            end;
      end;

    glBindTexture(GL_TEXTURE_2D, 0);

  glPopMatrix;
end;


procedure RenderShadowQuad(X,Y,W,H: Single; aBlur: Word; aCol: TColor4c);
  procedure DoNode(aX, aY: Single; aColor: TColor4c);
  begin
    glColor4ubv(@aColor);
    glVertex2f(aX,aY);
  end;
var
  bCol: TColor4c;
begin
  glPushMatrix;
    glTranslatef(aBlur/4.5, aBlur/3.5, 0); //Slightly shifted shadow looks nicer
    RenderQuad(GL_QUADS, X, Y, W, H, aCol);
    bCol := aCol AND $FFFFFF; //Makes color transition better for non-black shadows
    glBegin(GL_QUAD_STRIP);
      DoNode(X-aBlur,Y,bCol);
      DoNode(X,Y,aCol);
      DoNode(X-aBlur*0.7,Y-aBlur*0.7,bCol);
      DoNode(X,Y,aCol);
      DoNode(X,Y-aBlur,bCol);
      DoNode(X,Y,aCol);

      DoNode(X+W,Y-aBlur,bCol);
      DoNode(X+W,Y,aCol);
      DoNode(X+W+aBlur*0.7,Y-aBlur*0.7,bCol);
      DoNode(X+W,Y,aCol);
      DoNode(X+W+aBlur,Y,bCol);
      DoNode(X+W,Y,aCol);

      DoNode(X+W+aBlur,Y+H,bCol);
      DoNode(X+W,Y+H,aCol);
      DoNode(X+W+aBlur*0.7,Y+H+aBlur*0.7,bCol);
      DoNode(X+W,Y+H,aCol);
      DoNode(X+W,Y+H+aBlur,bCol);
      DoNode(X+W,Y+H,aCol);

      DoNode(X,Y+H+aBlur,bCol);
      DoNode(X,Y+H,aCol);
      DoNode(X-aBlur*0.7,Y+H+aBlur*0.7,bCol);
      DoNode(X,Y+H,aCol);
      DoNode(X-aBlur,Y+H,bCol);
      DoNode(X,Y+H,aCol);

      DoNode(X-aBlur,Y,bCol);
      DoNode(X,Y,aCol);
    glEnd;
  glPopMatrix;
end;


procedure ScissorQuad; overload;
begin
  glDisable(GL_STENCIL_TEST);
end;


procedure ScissorQuad(X,Y,W,H: Single); overload;
begin
  glClear(GL_STENCIL_BUFFER_BIT);

  //Setup stencil mask
  glEnable(GL_STENCIL_TEST);
  glStencilFunc(GL_ALWAYS, 1, 1);
  glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);

  //Do not render anything on screen while setting up stencil mask
  glColorMask(False, False, False, False);

  glColor3f(1, 1, 1);
  glBegin(GL_QUADS);
    glVertex2f(X,Y);
    glVertex2f(X,Y+H);
    glVertex2f(X+W,Y+H);
    glVertex2f(X+W,Y);
  glEnd;

  glStencilFunc(GL_EQUAL, 1, 1);
  glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
  glColorMask(True, True, True, True);
end;


end.
