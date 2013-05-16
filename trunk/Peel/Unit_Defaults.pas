unit Unit_Defaults;
interface
uses Unit_Vector, Unit_Color, Unit_ColorCoder;

const
  MENU_DESIGN_X     =     960;
  MENU_DESIGN_Y     =     680;
  DECK_HEIGHT = 180;

  MAX_AA_LEVEL = 4;

  ZoomLow   = 0.4;
  ZoomHigh  = 1.5;

  //Lighting properties
  LightPos: TVector4f = (X:-1; Y:-2; Z:1; W:0);
  LightAmb: TColor4f = (R:0.1; G:0.1; B:0.1; A:0);
  LightDiff: TColor4f = (R:0.9; G:0.9; B:0.9; A:0);
  LightSpec: TColor4f = (R:1.0; G:1.0; B:1.0; A:0);
  LightShine: Single = 32;

  Eol = #13#10; //End-of-line symbol

  SETTINGS_PATH: string = 'settings.ini';

var
  DATA_DIR: string;
  SHOW_CONTROLS_OVERLAYS: Boolean  = False;
  SHOW_TEXT_BOUNDS: Boolean        = False; //F7 Show text areas bounds
  SHOW_SELECTION_BUFFER: Boolean   = False;
  LOAD_TEX_FROM_RES: Boolean = False;


implementation


end.
