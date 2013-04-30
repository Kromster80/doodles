unit Unit_UserInterface;
interface
uses Classes, dglOpenGL, Unit_Controls;

type
  TUserInterface = class
  private
  protected
    Panel_Main: TLPanel;
      Panel_Deck: TLPanel;
      Label_VerInfo: TLLabel;
  public
    constructor Create(aWidth, aHeight: Integer);
    destructor Destroy; override;
    procedure Init;
    procedure Resize(aWidth, aHeight: Integer);
    procedure Render;
  end;


implementation
uses Unit_Render, Unit_Fonts;


{ TUserInterface }
constructor TUserInterface.Create(aWidth, aHeight: Integer);
begin
  inherited Create;

  Panel_Main := TLPanel.Create(nil, 0, 0, aWidth, aHeight);

  Label_VerInfo := TLLabel.Create(Panel_Main, 5, 5, 10, 10);
  Label_VerInfo.Caption := fRender.VersionInfo;
  Label_VerInfo.Font := MakeFont(UI_COLOR_FONT, 8, fsArialNormal, taLeftJustify);
end;


destructor TUserInterface.Destroy;
begin

  inherited;
end;


procedure TUserInterface.Init;
begin
  //Panel_Main := tm
end;


procedure TUserInterface.Resize(aWidth, aHeight: Integer);
begin
  Panel_Main.LocWidth := aWidth;
  Panel_Main.LocHeight := aHeight;
end;


procedure TUserInterface.Render;
begin
  Panel_Main.Render;
end;


end.
