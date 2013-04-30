unit Unit_UserInterface;
interface
uses Classes, dglOpenGL, SysUtils, Unit_Controls;

type
  TUserInterface = class
  private
  protected
    Panel_Main: TLPanel;
      Panel_Deck: TLPanel;
      Label_VerInfo: TLLabel;
      Label_String: TLLabel;
  public
    constructor Create(aWidth, aHeight: Integer);
    destructor Destroy; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure Resize(aWidth, aHeight: Integer);
    procedure Render;
  end;


implementation
uses Unit_Defaults, Unit_ColorCoder, Unit_Render, Unit_Fonts, Unit_Game;


{ TUserInterface }
constructor TUserInterface.Create(aWidth, aHeight: Integer);
begin
  inherited Create;

  fFontLib := TLFontLib.Create;

  Panel_Main := TLPanel.Create(nil, 0, 0, aWidth, aHeight);

  Label_VerInfo := TLLabel.Create(Panel_Main, 5, 5, 0, 10);
  Label_VerInfo.Font := MakeFont(UI_COLOR_FONT, 10, fsArialNormal, taLeftJustify);
  Label_VerInfo.Caption := fRender.VersionInfo;

  Label_String := TLLabel.Create(Panel_Main, 5, 25, 0, 10);
  Label_String.Font := MakeFont($FF00FF00, 10, fsArialNormal, taLeftJustify);
end;


destructor TUserInterface.Destroy;
begin
  fFontLib.Free;

  inherited;
end;


procedure TUserInterface.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Code: TColorCodeId;
begin
  fCursor.X := X;
  fCursor.Y := Y;
  Code := fRender.CodeBelow(fCursor.X, fCursor.Y);
  Label_String.Caption := Format('%d - %d', [Byte(Code.Code), Code.Id]);
end;


procedure TUserInterface.Resize(aWidth, aHeight: Integer);
begin
  Panel_Main.LocWidth := aWidth;
  Panel_Main.LocHeight := aHeight;
end;


procedure TUserInterface.Render;
begin
  Label_VerInfo.Caption := fRender.VersionInfo + ' / ' + fRender.FPS;

  Panel_Main.Render;
end;


end.
