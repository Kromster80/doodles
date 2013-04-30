unit Unit_Controls;
interface
uses Classes, Controls, Forms, Math, SysUtils, Windows, DateUtils, StrUtils, MMSystem,
    dglOpenGL, Unit_LoadTexture, KromUtils, Unit_ControlsRender,
    Unit_Fonts, Unit_Color, Unit_Vector;


const
  UI_COLOR_FORM_FACE      = $FFEEEEEE;
  UI_COLOR_POPUP_FACE     = $FFDDDDDD;
  UI_COLOR_SECTION: Cardinal    = $FFF0D060;
  UI_COLOR_LINES          = $40000000;
  UI_COLOR_FONT           = $FF000000;
  UI_COLOR_FONT_GREY      = $FF606060;
  UI_COLOR_HINT_BG        = $FFD0FFFF;
  UI_COLOR_DISABLED       = $B0FFFFFF;
  UI_COLOR_OVER           = $18000000;
  UI_COLOR_SELECTED       = $30000000;
  UI_COLOR_OVERLAY        = $FFFFFFFF;

  UI_ANIM_FRM_TIME  = 750;
  UI_ANIM_CTRL_TIME = UI_ANIM_FRM_TIME div 2;

  UI_SHADOW_FRM     = 32;
  UI_SHADOW_CTRL    = UI_SHADOW_FRM div 2;

  LINE_HEIGHT_MUL   = 1.25;
  DEF_TEX_INS       = 10; //Default inset for 9-piece Quad render
  DEF_FONT_SIZE     = 9;
  DEF_TEXT_MARGIN   = 2; //default text margin inside of controls cells
  DEF_SCROLL_PAD    = 4; //Default padding from listbox to its scroll bar


type
  TAnimRule = (arLinear, arLinearCos, arCurve);
  TAnchorSet = set of (akLeft, akRight, akTop, akBottom);
  TButtonTexSet = record
    Disa, Norm, Over, Down: TLTexture;
  end;
  TControlStateSet = set of (csDown, csFocus, csOver);

  TLPanel = class;

  TLControl = class
  private
    fParent: TLPanel;
    fLeft: Integer;
    fTop: Integer;
    fHeight: Integer;
    fWidth: Integer;
    fScale: Single;

    fTag: Cardinal;
    fVisible: Boolean;
    fEnabled: Boolean;
    fAnchors: TAnchorSet;
    fControlState: TControlStateSet;
    fHint: string;
    fTimeOfLastClick: Cardinal; //Required to handle double-clicks
    fPressedDown: TPoint;
    fIsDragged: Boolean;
    fCanClick: Boolean;

    fOnClick: TNotifyEvent;
    fOnDoubleClick: TNotifyEvent;
    fOnFocusLoose: TNotifyEvent;
    fOnMouseDown: TMouseEvent;
    fOnMouseMove: TMouseMoveEvent;
    fOnMouseUp: TMouseEvent;
    fOnMouseEnter: TNotifyEvent;
    fOnMouseExit: TNotifyEvent;
    fOnMouseWheel: TMouseWheelEvent;

    procedure FocusLoose; virtual;
    procedure SetHint(const Value: string); //Perform actions on focus loose (e.g. check input values)
  protected
    fUpdating: Boolean;
    function GetAbsLeft: Integer;
    function GetAbsTop: Integer;
    procedure SetAbsLeft(aValue: Integer);
    procedure SetAbsTop(aValue: Integer);
    function GetScale: Single;
    function GetAbsWidth: Integer;
    function GetAbsHeight: Integer;
    procedure SetLocLeft(aValue: Integer); virtual;
    procedure SetLocTop(aValue: Integer); virtual;
    procedure SetLocWidth(aValue: Integer); virtual;
    procedure SetLocHeight(aValue: Integer); virtual;

    procedure SetEnabled(aValue: Boolean); virtual;
    function  GetVisible: Boolean;
    procedure SetVisible(aValue: Boolean); virtual;

    procedure DoClick; virtual;
    procedure DoDoubleClick; virtual;
    procedure DragStart(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure Drag(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DragEnd(Target: TObject; X, Y: Integer); virtual;
    procedure MouseEnter; virtual; //Perform actions on mouse enter
    procedure MouseExit; virtual; //Perform actions on mouse exit
    procedure DoUpdate; virtual;
  public
    Cursor: TCursor;
    CanDrag: Boolean;
    Hitable: Boolean;
    OnDragStart: TMouseEvent;
    OnDrag: TMouseMoveEvent;
    OnDragEnd: TDragDropEvent;

    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
    destructor Destroy; override;

    property Parent: TLPanel read fParent write fParent;

    //Access to local position and dimensions
    property LocLeft: Integer read fLeft write SetLocLeft;
    property LocTop: Integer read fTop write SetLocTop;
    property LocHeight: Integer read fHeight write SetLocHeight;
    property LocWidth: Integer read fWidth write SetLocWidth;

    //Absolute positioning and dimensions
    property AbsLeft: Integer read GetAbsLeft write SetAbsLeft;
    property AbsTop: Integer read GetAbsTop write SetAbsTop;
    property AbsHeight: Integer read GetAbsHeight;
    property AbsWidth: Integer read GetAbsWidth;
    property Scale: Single read GetScale write fScale;

    property Anchors: TAnchorSet read fAnchors write fAnchors;
    property ControlState: TControlStateSet read fControlState;
    property Enabled: Boolean read fEnabled write SetEnabled;
    property Hint: string read fHint write SetHint;
    property IsDragged: Boolean read fIsDragged;
    property Tag: Cardinal read fTag write fTag;
    property Visible: Boolean read GetVisible write SetVisible;

    property OnClick: TNotifyEvent read fOnClick write fOnClick;
    property OnDoubleClick: TNotifyEvent read fOnDoubleClick write fOnDoubleClick;
    property OnFocusLoose: TNotifyEvent read fOnFocusLoose write fOnFocusLoose;
    property OnMouseDown: TMouseEvent read fOnMouseDown write fOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read fOnMouseMove write fOnMouseMove;
    property OnMouseUp: TMouseEvent read fOnMouseUp write fOnMouseUp;
    property OnMouseEnter: TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseExit: TNotifyEvent read fOnMouseExit write fOnMouseExit;
    property OnMouseWheel: TMouseWheelEvent read fOnMouseWheel write fOnMouseWheel;

    function Master: TLPanel; //Return top-most Panel
    function HasParent(aCheck: TLControl): Boolean;

    procedure MoveAbove(aControl: TLControl);
    procedure MoveBelow(aControl: TLControl);
    procedure MoveToTop; virtual;

    function  HitTest(X,Y: Integer): Boolean; virtual;
    function  HitControl(X,Y: Integer): TLControl; virtual;
    procedure KeyDown(Key:word; Shift: TShiftState); virtual;
    procedure KeyPress(Key: Char); virtual;
    procedure KeyUp(Key:word; Shift: TShiftState); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean); virtual;

    procedure ExportControlsTree(S: TStringList; aTab: Byte); virtual;

    procedure UpdateState; virtual;
    procedure Render; virtual;
    procedure RenderOutlines; virtual;
    end;

  TLButton = class;
  TLEditControl = class;
  TLListBox = class;
  TLToolTip = class;

  TLPanel = class (TLControl)
  private
    fToolTip_Hint: TLToolTip;
    fChildCount: Integer;
    fCtrlDown: TLControl;
    fCtrlFocus: TLControl;
    fCtrlOver: TLControl;
    fCtrlUp: TLControl;
    fCtrlDrag: TLControl;
    procedure SetCtrlDown(aCtrl: TLControl);
    procedure SetCtrlFocus(aCtrl: TLControl);
    procedure SetCtrlOver(aCtrl: TLControl);
    procedure SetCtrlUp(aCtrl: TLControl);
  protected
    procedure SetLocWidth(aValue: Integer); override;
    procedure SetLocHeight(aValue: Integer); override;
  public
    CursorPos: TPoint;
    Childs: array of TLControl; //1..n
    Key: Cardinal; //Something like a Tag, but only for Panels
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
    destructor Destroy; override;

    procedure ExportControlsTree(S: TStringList; aTab: Byte = 0); override;
    procedure AddChild(aControl: TLControl); virtual;
    procedure InsertChild(aControl: TLControl; aIndex: Integer);
    procedure RemChild(aControl: TLControl);
    procedure RemAllChilds;
    procedure HintShow(X,Y: Integer; aCaption: string);
    procedure HintHide;
    function HintVisible: Boolean;

    property ChildCount: Integer read fChildCount;
    property CtrlDown: TLControl read fCtrlDown write SetCtrlDown;
    property CtrlFocus: TLControl read fCtrlFocus write SetCtrlFocus;
    property CtrlOver: TLControl read fCtrlOver write SetCtrlOver;
    property CtrlUp: TLControl read fCtrlUp write SetCtrlUp;
    property CtrlDrag: TLControl read fCtrlDrag;

    function  HitControl(X,Y: Integer): TLControl; override;
    procedure KeyDown(Key:word; Shift: TShiftState); override;
    procedure KeyPress(Key: Char); override;
    procedure KeyUp(Key:word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean); override;

    procedure UpdateState; override;
    procedure Render; override;
    procedure RenderOutlines; override;
  end;

  TLImage = class (TLControl)
  private
    fImagePath: string;
    fImagePathAct: string;

    fUseMask: Boolean;
    fMask:packed array[0..15, 0..15] of byte; // 0/1

    TransparentTime: Cardinal; //How long it takes to become fully opaque
    TickTransparent: Cardinal;
    fOpaqueness: Single;

    procedure SetImagePath(aPath: string);
    procedure SetImagePathAct(aPath: string);
  public
    RenderStyle: TImageRenderStyle;
    ImageMain: TLTexture;
    ImageAlt: TLTexture;
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aPath: string; aPathAct: string='');
    property  ImagePath: string read fImagePath write SetImagePath;
    property Opaqueness: Single read fOpaqueness write fOpaqueness;
    procedure AnimShow(AnimLength:word=UI_ANIM_FRM_TIME);

    function  HitTest(X,Y: Integer): Boolean; override;

    procedure UpdateState; override;
    procedure Render; override;
  end;

  TLShape = class (TLControl)
  private
    fColor: TColor4c;
  public
    DropShadow: Byte; //Usually in range of 5..20
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aColor: TColor4c);
    property Color: TColor4c read fColor write fColor;
    procedure Render; override;
  end;

  TLLabel = class (TLControl)
  private
    fCaption: string;
    fFont: TFontProps;
    procedure SetFont(const Value: TFontProps);
    procedure SetCaption(const Value: string);
  public
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer;
                       aCaption: string = ''; aSize: Integer = DEF_FONT_SIZE;
                       aStyle: Integer = 0; aJustify: TAlignment = taLeftJustify);
    function HitTest(X,Y: Integer): Boolean; override;
    property Caption: string read fCaption write SetCaption;
    property Font: TFontProps read fFont write SetFont;
    function TextWidth: Single;
    function TextHeight: Single;
    procedure Render; override;
  end;

  TLToolTip = class(TLControl)
  private
    fCaption: string;
    fPositionAt: TPoint;
  public
    FillColor: TColor4c;
    Font: TFontProps;
    constructor Create(aParent: TLPanel);
    procedure PositionAt(X, Y: Integer; aCaption: string);
    procedure Render; override;
  end;

  TLEditControl = class (TLControl)
  private
    fOnChange: TNotifyEvent;
    function GetBackColor: TColor4c;
    procedure NotifyChange; //Set Changed and generate OnChange event
  public
    Font: TFontProps;
    ReadOnly: Boolean;
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
    property OnChange: TNotifyEvent write fOnChange;
    procedure Render; override; //Render common properties
  end;

  TLEdit = class (TLEditControl)
  private
    fCursorPos: Integer;
    fText: string;
    procedure SetText(aText: string);
    procedure UpdateCursorPos(X: Single);
  public
    MaskText: Boolean;
    MaxLength: Integer;
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
    property Text: string read fText write SetText;
    procedure KeyDown(Key:word; Shift: TShiftState); override;
    procedure KeyPress(Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure Render; override;
  end;

  TLButton = class (TLControl)
  private
    fFace: TButtonTexSet;
    fImage: TLTexture;
    fDown: Boolean;
    fDownSince: Cardinal;
    fRepeatClick: Boolean;
    procedure SetDown(const Value: Boolean);
  public
    Caption: string;
    Font: TFontProps;
    ButtonFaceStyle: TImageRenderStyle;
    ImageStyle: TImageRenderStyle;
    constructor Create(aParent: TLPanel; aLeft,aTop: Integer; aTexMask: string); overload;
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string); overload;
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aTextures: TButtonTexSet); overload;
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption, aImage: string); overload;
    property Down: Boolean read fDown write SetDown;
    procedure Render; override;
    procedure UpdateState; override;
  end;

  TScrollAxis = (sa_Vertical, sa_Horizontal);

  {Scroll bar}
  TLScrollBar = class(TLControl)
  private
    fThumbLen: Integer;             //Thumb length
    fBtnSize: Integer;              //Button length (local)
    fMoveOffset: Integer;           //Offset between thumb center and mouse pos
    fMinValue: Integer;
    fPageSize: Integer;
    fMaxValue: Integer;
    fPosition: Integer;
    fScrollAxis: TScrollAxis;
    ScrollDec: TLButton;
    ScrollInc: TLButton;
    fOnChange: TNotifyEvent;
    procedure SetMaxValue(aValue: Integer);
    procedure IncPosition(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure DecPosition(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure UpdateSize;
    procedure SetPosition(aValue: Integer);
    procedure SetPageSize(aValue: Integer);
    function ThumbPos: Integer;
  protected
    procedure SetEnabled(aValue: Boolean); override;
    procedure SetLocHeight(aValue: Integer); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TLPanel; aLeft,aTop,aLength: Integer; aScrollAxis: TScrollAxis);
    destructor Destroy; override;

    property MinValue: Integer read fMinValue write fMinValue;
    property MaxValue: Integer read fMaxValue write SetMaxValue;
    property PageSize: Integer read fPageSize write SetPageSize;
    property Position: Integer read fPosition write SetPosition;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean); override;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    procedure Render; override;
  end;


  TLContainer = class(TLPanel)
  private
    fVScroll: TLScrollBar;

    fTopOffset: Integer;
    procedure ScrollChange(Sender:TObject);
    function GetScrollPosition: Integer;
    procedure SetScrollPosition(const Value: Integer);
  protected
    procedure SetLocWidth(aValue: Integer); override;
    procedure SetLocHeight(aValue: Integer); override;
    procedure SetVisible(aValue: Boolean); override;
  public
    constructor Create(aParent: TLPanel; aLeft, aTop, aWidth, aHeight: Integer);

    procedure AddChild(aControl: TLControl); override;
    function HitControl(X,Y: Integer): TLControl; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean); override;
    property ScrollPosition: Integer read GetScrollPosition write SetScrollPosition;

    procedure UpdateScroll;
    procedure Render; override;
  end;


  TLTrackBar = class(TLControl)
  private
    fMinValue: Integer;
    fMaxValue: Integer;
    fPosition: Integer;
    fFontHeight: Integer;
    fCaption: string;
    Font: TFontProps;
    RenderPos: Integer;
    fThumbHeight: Integer;
    fThumbWidth: Integer;
    fOffsetX: Integer;

    fOnChange: TNotifyEvent;
    procedure SetPosition(aValue: Integer);
  public
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer; aCaption: string=''; aFontSize: Integer=DEF_FONT_SIZE);

    property MinValue: Integer read fMinValue write fMinValue;
    property MaxValue: Integer read fMaxValue write fMaxValue;
    property Position: Integer read fPosition write SetPosition;

    function  HitTest(X,Y: Integer): Boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean); override;
    property OnChange: TNotifyEvent write fOnChange;
    procedure Render; override;
  end;

  TLListBox = class (TLControl)
  private
    fFont: TFontProps;
    fLinesPerRow: Integer; //Allow multiple lines per row
    fItemHeight: Integer;
    fItemHighlight: Integer;
    fItemIndex: Integer;
    fAreaWidth: Integer; //Width of visible area depending on ScrollBar visibility
    fVisibleItems: Integer;
    fHeaderHeight: Integer; //Show column headers (mainly used by TLColumnListBox)
    fShowLineLines: Boolean;
    fItems: TStringList;
    fScrollBar: TLScrollBar;
    procedure SetItemIndex(aValue: Integer);
    function GetTopIndex: Integer;
    procedure SetTopIndex(aValue: Integer);
    procedure UpdateSize; virtual;
    procedure SetFont(const Value: TFontProps);
  protected
    procedure SetLocWidth(aValue: Integer); override;
    procedure SetLocHeight(aValue: Integer); override;
    procedure SetVisible(aValue: Boolean); override;
    procedure DoUpdate; override;
  public
    TagString: string;
    OnChange: TNotifyEvent;
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
    destructor Destroy; override;

    property Font: TFontProps read fFont write SetFont;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property ItemIndex: Integer read fItemIndex write SetItemIndex;
    property LinesPerRow: Integer read fLinesPerRow write fLinesPerRow;
    procedure AddItem(const aText: string);
    procedure Clear;
    function  ItemCount: Integer;

    function  HitTest(X,Y: Integer): Boolean; override;
    procedure KeyDown(Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean); override;

    procedure Render; override;
  end;

  TLCheckBox = class (TLEditControl)
  private
    fChecked: Boolean;
    fCaption: string;
  public
    constructor Create(aParent: TLPanel; aLeft,aTop: Integer; aCaption: string);
    property Caption: string read fCaption write fCaption;
    property Checked: Boolean read fChecked write fChecked;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure Render; override;
  end;

  TLRadioAxis = (raHorizontal, raVertical);

  TLRadio = class(TLControl)
  private
    fFont: TFontProps;
    fRadioAxis: TLRadioAxis;

    fCount: Integer;
    fItemIndex: Integer;
    fItems: array of TLButton;
    procedure ItemClick(Sender: TObject);
  protected
    procedure SetVisible(aValue: Boolean); override;
  public
    OnChange: TNotifyEvent;

    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
    destructor Destroy; override;

    procedure SetItems(aImages, aCaptions: array of string); overload;
    procedure SetItems(aButtons: array of TButtonTexSet); overload;
    property ItemIndex: Integer read fItemIndex write fItemIndex;
    property RadioAxis: TLRadioAxis read fRadioAxis write fRadioAxis;

    procedure Render; override;
  end;

  TLMemo = class(TLEditControl)
  private
    fCaption: string;        //Text caption rendered on top-left
    fCaptionWidth: Integer;  //Width of the caption
    fCursorPos: Integer;     //Character number
    fCursorCoord: TPoint;    //Cursor position in control-space coordinates
    fHeightOfLines: Integer; //Height of visible lines (<=fHeight)
    fLineHeight: Integer;    //Height of single line of text
    fLineSpacing: Integer;   //Spacing between lines tops
    fLineCount: Integer;     //How many lines of text we've got after word-wrapping
    fText: string;           //User text
    fTextRender: TStringList; //word-wrapped text for render
    fScrollBar: TLScrollBar;
    procedure SetText(aText: string);
    procedure EnsureCursorInView(Y: Integer);
    procedure UpdateCursorCoord(aCursorPos: Integer);
    procedure UpdateCursorPos(X,Y: Integer);
    procedure UpdateSize;
  public
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
    destructor Destroy; override;

    property Caption: string read fCaption write fCaption;
    property LineSpacing: Integer read fLineSpacing write fLineSpacing;
    property Text: string read fText write SetText;

    procedure KeyDown(Key: Word; Shift: TShiftState); override;
    procedure KeyPress(Key: Char); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean); override;
    procedure Render; override;
  end;

  TColumnKind = (ckText, ckCheck); //Text kind is default

  TLColumnHead = class
  private
    fCaption: string;
    fChecked: Boolean;
    fHasCheckbox: Boolean;
    fKind: TColumnKind;
    fVerticalCaption: Boolean;
    fVisible: Boolean;
    fWidth: Word;
  public
    constructor Create;
    property Caption: string read fCaption write fCaption;
    property HasCheckbox: Boolean read fHasCheckbox write fHasCheckbox;
    property Checked: Boolean read fChecked write fChecked;
    property VerticalCaption: Boolean read fVerticalCaption write fVerticalCaption;
    property Visible: Boolean read fVisible write fVisible;
    property Width: Word read fWidth write fWidth;
    property Kind: TColumnKind read fKind write fKind;
    procedure AutoSize(ItemHeight: Integer; const Font: TFontProps);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
    procedure Render(X,Height,CheckBoxHeight: Integer; WidthMultiplier,Scale: Single; const Font: TFontProps);
  end;


  TLColumns = class
  private
    fCount: Integer;
    fColumns:array of TLColumnHead;
    fColumnRows: Byte;
    fFont: TFontProps;
    fStretchToFit: Boolean;
    fStretchFactor: Single;
    fItemHeight: Integer;
    fDesiredWidth: Integer;          //Desired width
    procedure SetCount(aNewCount: Integer);
    procedure SetVerticalCaptions(aValue: Boolean);
    function GetColumn(Index: Integer): TLColumnHead;
    function GetUsedWidth: Single;
    function GetHeight: Integer;
    procedure SetFontSize(Value: Byte);
  public
    Width: Integer;                 //Available width

    constructor Create;
    destructor Destroy; override;

    property Count: Integer read fCount write SetCount;
    property Columns[Index: Integer]: TLColumnHead read GetColumn; default;
    property ColumnRows: Byte read fColumnRows write fColumnRows;
    property FontSize: Byte write SetFontSize;
    property StretchToFit: Boolean read fStretchToFit write fStretchToFit;
    property StretchFactor: Single read fStretchFactor;
    property VerticalCaptions: Boolean write SetVerticalCaptions;
    property UsedWidth: Single read GetUsedWidth;
    property Height: Integer read GetHeight;

    procedure AutoSize;
    function HitTestColumn(X: Single): Integer;

    procedure SubRender(Scale: Single);
  end;


  TLColumnListBox = class (TLListBox)
  private
    fCellDown: TPoint; //Store cell where mbLeft was pressed down (for checkboxes in cells)
    fColumns: TLColumns;
    fColumnCount: Integer;
    fColumnHighlight: Integer;
    fReadOnly: Boolean;
    fMoreItems:array of TStringList;
    function GetItems(Index: Integer): TStringList;
    function HitTestHeader(Y: Integer): Boolean;
    procedure SetColumnCount(aNewCount: Integer);
    procedure UpdateSize; override;
  public
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
    destructor Destroy; override;

    procedure Clear;
    property Columns: TLColumns read fColumns;
    property ColumnCount: Integer read fColumnCount write SetColumnCount;
    property Items[Index: Integer]: TStringList read GetItems;
    property ReadOnly: Boolean read fReadOnly write fReadOnly;
    procedure AddItem(const aText: array of string);
    procedure AutoSizeColumns;
    procedure ReformatColumnsText;
    function DesiredWidth: Integer;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure Render; override;
  end;

  TLDropBoxCommon = class (TLEditControl)
  private
    fRowCount: Integer;
    fCaption: string;
    fCaptionWidth: Integer;
    fBoxText: TStringList; //What to show in box (could be multi-line text)

    fItemHeight: Integer;
    fItemCount: Integer;
    fItems: TStringList;
    fTags: array of Integer;
    fDropCount: Integer;
    fButton: TLButton;
    fShape: TLShape;
    fPanel: TLPanel;
    fShape2: TLShape;
    procedure PanelShow(Sender: TObject); virtual;
    procedure PanelHide(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer); virtual;
    procedure SetCaption(aCaption: string);
    procedure UpdateSize;
    procedure UpdateBoxText; virtual;
  public
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
    destructor Destroy; override;
    property  Caption: string read fCaption write SetCaption;
    property  DropCount: Integer read fDropCount write fDropCount;
    property  RowCount: Integer read fRowCount write fRowCount;
    property  ItemCount: Integer read fItemCount;
    procedure Render; override;
  end;

  TLDropBox = class (TLDropBoxCommon)
  private
    fItemIndex: Integer;
    fList: TLListBox;
    procedure SetItemIndex(aIndex: Integer);
    procedure PanelShow(Sender: TObject); override;
    procedure ListPick(Sender: TObject);
    function GetBoxText: string;
    procedure UpdateBoxText; override;
    function GetItemTag: Integer;
  public
    constructor Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
    destructor Destroy; override;

    property  ItemIndex: Integer read fItemIndex write SetItemIndex;
    property  ItemTag: Integer read GetItemTag;
    property  BoxText: string read GetBoxText;
    procedure AddEntry(aItem: string; aTag: Integer = 0);
    function IndexOfTag(aTag: Integer): Integer;
    procedure Clear;
  end;

  TLAnimator = class
  private
    fCtrlCount: Integer;
    fCtrls:array of
    record
      Ctrl: TLControl; //Usually TLPanel
      FirstTick: Cardinal; //TimeGetTime is first
      AnimScale1,AnimScale2: Single; //From->To
      AnimMove1, AnimMove2: TPoint; //From->To
      AnimSize1, AnimSize2: TPoint; //From->To
      AnimTime: Word; //Length of animation
      AnimRule: TAnimRule; //How to animate
      fDone: Boolean; //Set this flag when animation is done
      fAnimSender: TObject;
      fOnAnimEnd: TNotifyEvent; //Call this procedure with Sender on done
    end;
  public
    constructor Create;
    procedure Add(aCtrl: TLControl; XY1,XY2,SZ1,SZ2: TPoint; S1,S2: Single; aLength:word; aRule: TAnimRule=arLinearCos; aOnAnimEnd: TNotifyEvent=nil; aSender: TObject=nil);
    procedure AddDest(aCtrl: TLControl; X2,Y2: Integer; S2: Single; aLength:word; aRule: TAnimRule=arLinearCos; aOnAnimEnd: TNotifyEvent=nil; aSender: TObject=nil); overload;
    procedure AddDest(aCtrl: TLControl; TargetX, TargetY, TargetW, TargetH: Integer; aLength: Word; aRule: TAnimRule = arLinearCos; aOnAnimEnd: TNotifyEvent = nil; aSender: TObject = nil); overload;

    procedure Remove(aCtrl: TLControl);
    function AnimWIP(aCtrl: TLControl): Boolean;
    procedure UpdateState;
  end;


  procedure LoadCommonTextures(aPath: string);


var
  fAnimator: TLAnimator;


implementation
uses Unit_Defaults;

var
  TexBtnDefault:  TButtonTexSet; //Glassy buttons
  TexChkChecked:  TButtonTexSet; //Checkbox icon
  TexChkUnchecked:TButtonTexSet; //Checkbox icon
  TexDropBox:     TButtonTexSet; //Drop box icon
  TexScrollVLine:   TButtonTexSet; //ScrollBar vertical line
  TexScrollVThumb:  TButtonTexSet; //ScrollBar thumb
  TexScrollUp:      TButtonTexSet; //ScrollBar Up
  TexScrollDown:    TButtonTexSet; //ScrollBar Down
  TexScrollHLine:   TButtonTexSet; //ScrollBar vertical line
  TexScrollHThumb:  TButtonTexSet; //ScrollBar thumb
  TexScrollLeft:    TButtonTexSet; //ScrollBar Up
  TexScrollRight:   TButtonTexSet; //ScrollBar Down


procedure LoadCommonTextures(aPath: string);
begin
  //Scroll_bar
  LoadTexture(aPath+'scrl_up.png', LOAD_TEX_FROM_RES, TexScrollUp.Norm);
  LoadTexture(aPath+'scrl_up.png', LOAD_TEX_FROM_RES, TexScrollUp.Over);
  LoadTexture(aPath+'scrl_up.png', LOAD_TEX_FROM_RES, TexScrollUp.Down);
  LoadTexture(aPath+'scrl_up.png', LOAD_TEX_FROM_RES, TexScrollUp.Disa);
  LoadTexture(aPath+'scrl_down.png', LOAD_TEX_FROM_RES, TexScrollDown.Norm);
  LoadTexture(aPath+'scrl_down.png', LOAD_TEX_FROM_RES, TexScrollDown.Over);
  LoadTexture(aPath+'scrl_down.png', LOAD_TEX_FROM_RES, TexScrollDown.Down);
  LoadTexture(aPath+'scrl_down.png', LOAD_TEX_FROM_RES, TexScrollDown.Disa);
  LoadTexture(aPath+'scrl_v_line.png', LOAD_TEX_FROM_RES, TexScrollVLine.Norm);
  LoadTexture(aPath+'scrl_v_line.png', LOAD_TEX_FROM_RES, TexScrollVLine.Over);
  LoadTexture(aPath+'scrl_v_line.png', LOAD_TEX_FROM_RES, TexScrollVLine.Down);
  LoadTexture(aPath+'scrl_v_line.png', LOAD_TEX_FROM_RES, TexScrollVLine.Disa);
  LoadTexture(aPath+'scrl_v_thumb_norm.png', LOAD_TEX_FROM_RES, TexScrollVThumb.Norm);
  LoadTexture(aPath+'scrl_v_thumb_over.png', LOAD_TEX_FROM_RES, TexScrollVThumb.Over);
  LoadTexture(aPath+'scrl_v_thumb_down.png', LOAD_TEX_FROM_RES, TexScrollVThumb.Down);
  LoadTexture(aPath+'scrl_v_thumb_disa.png', LOAD_TEX_FROM_RES, TexScrollVThumb.Disa);

  LoadTexture(aPath+'scrl_left.png', LOAD_TEX_FROM_RES, TexScrollLeft.Norm);
  LoadTexture(aPath+'scrl_left.png', LOAD_TEX_FROM_RES, TexScrollLeft.Over);
  LoadTexture(aPath+'scrl_left.png', LOAD_TEX_FROM_RES, TexScrollLeft.Down);
  LoadTexture(aPath+'scrl_left.png', LOAD_TEX_FROM_RES, TexScrollLeft.Disa);
  LoadTexture(aPath+'scrl_right.png', LOAD_TEX_FROM_RES, TexScrollRight.Norm);
  LoadTexture(aPath+'scrl_right.png', LOAD_TEX_FROM_RES, TexScrollRight.Over);
  LoadTexture(aPath+'scrl_right.png', LOAD_TEX_FROM_RES, TexScrollRight.Down);
  LoadTexture(aPath+'scrl_right.png', LOAD_TEX_FROM_RES, TexScrollRight.Disa);
  LoadTexture(aPath+'scrl_h_line.png', LOAD_TEX_FROM_RES, TexScrollHLine.Norm);
  LoadTexture(aPath+'scrl_h_line.png', LOAD_TEX_FROM_RES, TexScrollHLine.Over);
  LoadTexture(aPath+'scrl_h_line.png', LOAD_TEX_FROM_RES, TexScrollHLine.Down);
  LoadTexture(aPath+'scrl_h_line.png', LOAD_TEX_FROM_RES, TexScrollHLine.Disa);
  LoadTexture(aPath+'scrl_h_thumb_norm.png', LOAD_TEX_FROM_RES, TexScrollHThumb.Norm);
  LoadTexture(aPath+'scrl_h_thumb_over.png', LOAD_TEX_FROM_RES, TexScrollHThumb.Over);
  LoadTexture(aPath+'scrl_h_thumb_down.png', LOAD_TEX_FROM_RES, TexScrollHThumb.Down);
  LoadTexture(aPath+'scrl_h_thumb_disa.png', LOAD_TEX_FROM_RES, TexScrollHThumb.Disa);

  LoadTexture(aPath+'check_on.png', LOAD_TEX_FROM_RES, TexChkChecked.Norm);
  LoadTexture(aPath+'check_on.png', LOAD_TEX_FROM_RES, TexChkChecked.Over);
  LoadTexture(aPath+'check_on.png', LOAD_TEX_FROM_RES, TexChkChecked.Down);

  LoadTexture(aPath+'check_off.png', LOAD_TEX_FROM_RES, TexChkUnchecked.Norm);
  LoadTexture(aPath+'check_off.png', LOAD_TEX_FROM_RES, TexChkUnchecked.Over);
  LoadTexture(aPath+'check_off.png', LOAD_TEX_FROM_RES, TexChkUnchecked.Down);

  LoadTexture(aPath+'dropbox_norm.png', LOAD_TEX_FROM_RES, TexDropBox.Norm);
  LoadTexture(aPath+'dropbox_over.png', LOAD_TEX_FROM_RES, TexDropBox.Over);
  LoadTexture(aPath+'dropbox_down.png', LOAD_TEX_FROM_RES, TexDropBox.Down);
  LoadTexture(aPath+'dropbox_disa.png', LOAD_TEX_FROM_RES, TexDropBox.Disa);

  LoadTexture(aPath+'btn_empty_norm.png', LOAD_TEX_FROM_RES, TexBtnDefault.Norm);
  LoadTexture(aPath+'btn_empty_over.png', LOAD_TEX_FROM_RES, TexBtnDefault.Over);
  LoadTexture(aPath+'btn_empty_down.png', LOAD_TEX_FROM_RES, TexBtnDefault.Down);
  LoadTexture(aPath+'btn_empty_disa.png', LOAD_TEX_FROM_RES, TexBtnDefault.Disa);
end;


procedure glScale2(aValue: Single);
begin
  glScalef(aValue, aValue, aValue);
end;


{ TLControl }
constructor TLControl.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create;
  fLeft     := aLeft;
  fTop      := aTop;
  fWidth    := aWidth;
  fHeight   := aHeight;
  fScale    := 1;
  fAnchors  := [akLeft, akTop];
  fEnabled  := True;
  fVisible  := True;

  Cursor    := crDefault;
  Hitable   := True;

  if aParent <> nil then
    aParent.AddChild(Self);
end;


destructor TLControl.Destroy;
var P: TLPanel;
begin
  fOnFocusLoose := nil; //So we won't get OnFocusLoose event

  P := Master; //Top-most parent which keeps an eye on CtrlFocus and etc

  //Remove ourselves from master parent
  if P <> nil then begin
    if P.CtrlFocus = Self then P.fCtrlFocus := nil; //Set to nil without calling OnFocusLoose event
    if P.CtrlDown = Self then P.fCtrlDown := nil;   //cos it could try to access other destroyed controls
    if P.CtrlOver = Self then P.fCtrlOver := nil;
    if P.CtrlUp = Self then P.fCtrlUp := nil;
  end;

  if Parent <> nil then //nil if we are Master
    Parent.RemChild(Self);

  fAnimator.Remove(Self);

  inherited;
end;


procedure TLControl.DoClick;
begin
  if Assigned(fOnClick) then
    fOnClick(Self);
end;


procedure TLControl.DoDoubleClick;
begin
  if Assigned(fOnDoubleClick) then
    fOnDoubleClick(Self)
  else
    DoClick;
end;


procedure TLControl.DoUpdate;
begin
  //if not fUpdating then Update;
end;


procedure TLControl.DragStart(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnDragStart) then
    OnDragStart(Self, Button, Shift, X, Y);
  fIsDragged := True;
end;


procedure TLControl.Drag(Shift: TShiftState; X, Y: Integer);
begin
  if fIsDragged and Assigned(OnDrag) then
    OnDrag(Self, Shift, X, Y);
end;


procedure TLControl.DragEnd(Target: TObject; X, Y: Integer);
begin
  if Assigned(OnDragEnd) then
    OnDragEnd(Self, Target, X, Y);
  fIsDragged := False;
end;


procedure TLControl.ExportControlsTree(S: TStringList; aTab: Byte);
var
  Tmp: string;
begin
  Tmp := StringOfChar(#32, aTab * 2) + Self.ClassName;
  if Self is TLPanel then  Tmp := Tmp + ' [_]';
  if Self is TLButton then Tmp := Tmp + ' [' + TLButton(Self).Caption + ']';
  if Self is TLImage then  Tmp := Tmp + ' |...' + RightStr(TLImage(Self).ImagePath, 10) + '|';
  if Self is TLLabel then  Tmp := Tmp + ' /' + TLLabel(Self).Caption + '/';
  if Self is TLEdit then   Tmp := Tmp + ' \' + TLEdit(Self).fText + '\';
  Tmp := StringReplace(Tmp, eol, '_', [rfReplaceAll]) + eol;

  S.Append(Tmp);
end;


function TLControl.GetAbsLeft: Integer;
begin
  if Parent = nil then
    Result := fLeft
  else
    Result := Round(Parent.GetAbsLeft + fLeft * Parent.Scale);
end;


function TLControl.GetAbsTop: Integer;
begin
  if Parent = nil then
    Result := fTop
  else
    Result := Round(Parent.GetAbsTop + fTop * Parent.Scale);
end;


procedure TLControl.SetAbsLeft(aValue: Integer);
begin
  if Parent = nil then
    LocLeft := aValue
  else
    LocLeft := Round((aValue - Parent.GetAbsLeft) / Parent.Scale);
end;


procedure TLControl.SetAbsTop(aValue: Integer);
begin
  if Parent = nil then
    LocTop := aValue
  else
    LocTop := Round((aValue - Parent.GetAbsTop) / Parent.Scale);
end;


function TLControl.GetAbsWidth: Integer;
begin
  Result := Round(fWidth * Scale);
end;


function TLControl.GetAbsHeight: Integer;
begin
  Result := Round(fHeight * Scale);
end;


function TLControl.GetScale: Single;
begin
  if Parent=nil then
    Result := fScale
  else
    Result := fScale * Parent.GetScale;
end;


//Composite controls override this setting (e.g. ScrollBar must repeat it to its buttons)
procedure TLControl.SetEnabled(aValue: Boolean);
begin
  fEnabled := aValue;
end;


procedure TLControl.SetHint(const Value: string);
begin
  //This is for dynamicaly set hints (e.g. lines text in trees)
  //Hide previous hint, so that new ones shows below cursor
  Master.HintHide;
  fHint := Value;
end;


procedure TLControl.SetLocLeft(aValue: Integer);
begin
  fLeft := aValue;
end;


procedure TLControl.SetLocTop(aValue: Integer);
begin
  fTop := aValue;
end;


procedure TLControl.SetLocWidth(aValue: Integer);
begin
  fWidth := aValue;
end;


procedure TLControl.SetLocHeight(aValue: Integer);
begin
  fHeight := aValue;
end;


function TLControl.GetVisible: Boolean;
begin
  if Parent = nil then
    Result := fVisible
  else
    Result := fVisible and Parent.GetVisible;
end;


//Composite controls override this setting (e.g. ScrollBar must repeat it to its buttons)
procedure TLControl.SetVisible(aValue: Boolean);
begin
  fVisible := aValue;
end;


function TLControl.HitTest(X,Y: Integer): Boolean;
begin
  Result := Hitable and InRange(X - AbsLeft, 0, AbsWidth) and InRange(Y - AbsTop, 0, AbsHeight);
end;


// Every Control can access it's Master TLPanel, instead of global variable (?)
function TLControl.Master: TLPanel;
begin
  if Parent <> nil then
    Result := Parent.Master
  else
  begin
    Assert(Self is TLPanel);
    Result := TLPanel(Self);
  end;
end;


function TLControl.HasParent(aCheck: TLControl): Boolean;
begin
  if Parent <> nil then begin
    Result := Parent=aCheck;
    if not Result then
      Result := Parent.HasParent(aCheck);
  end else
    Result := Parent=aCheck;
end;


procedure TLControl.MoveAbove(aControl: TLControl);
var I: Integer; InsertPos: Integer;
begin
  Parent.RemChild(Self); //Remove first, cos it may affect found Pos

  InsertPos := 0; //Find position to insert to
  for I := 1 to aControl.Parent.fChildCount do
    if aControl.Parent.Childs[I] = aControl then
    begin
      InsertPos := I + 1; //insert above
      break;
    end;
  Assert(InsertPos <> 0);

  aControl.Parent.InsertChild(Self, InsertPos);
end;


procedure TLControl.MoveBelow(aControl: TLControl);
var i: Integer; InsertPos: Integer;
begin
  Parent.RemChild(Self); //Remove first, cos it may affect found Pos

  InsertPos := 0; //Find position to insert to
  for i:=1 to aControl.Parent.fChildCount do
    if aControl.Parent.Childs[i]=aControl then begin
      InsertPos := i; //insert below
      break;
    end;
  Assert(InsertPos<>0);

  aControl.Parent.InsertChild(Self, InsertPos);
end;


procedure TLControl.MoveToTop;
var InsertPos: Integer;
begin
  Parent.RemChild(Self); //Remove first, cos it may affect found Pos

  InsertPos := Parent.fChildCount + 1; //Find position to insert to

  Parent.InsertChild(Self, InsertPos);
end;


function TLControl.HitControl(X,Y: Integer): TLControl;
begin
  if HitTest(X,Y) then
    Result := Self
  else
    Result := nil;
end;


procedure TLControl.KeyDown(Key:word; Shift: TShiftState);
begin
  //
end;


procedure TLControl.KeyPress(Key: Char);
begin
  //
end;


procedure TLControl.KeyUp(Key:word; Shift: TShiftState);
begin
  //
end;


procedure TLControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fPressedDown := Point(X,Y);
  fCanClick := True;
  if Assigned(fOnMouseDown) then
    fOnMouseDown(Self, Button, Shift, X, Y);
end;


procedure TLControl.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  fCanClick := fCanClick
               and (Abs(fPressedDown.X - X) <= 2)
               and (Abs(fPressedDown.Y - Y) <= 2);

  if Screen.Cursor <> Cursor then
    Screen.Cursor := Cursor;

  if Assigned(fOnMouseMove) then
    fOnMouseMove(Self, Shift, X, Y);
end;


procedure TLControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Assigned(fOnMouseUp) then fOnMouseUp(Self, Button, Shift, X, Y);
  if Enabled and (csDown in fControlState) and (Button = mbLeft) and fCanClick then
  begin
    //Process double-click separately (actual sequence is Click & Double-Click)
    //cos we would not like to delay Click just to make sure it is single.
    if {Assigned(fOnDoubleClick)
    and} (fTimeOfLastClick+GetDoubleClickTime > TimeGetTime) then
    begin
      fTimeOfLastClick := 0;
      DoDoubleClick;
    end else
    begin
      fTimeOfLastClick := TimeGetTime;
      DoClick;
    end;
  end;
end;


procedure TLControl.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean);
begin
  //MouseMove(Shift, X, Y);
  Handled := False;
end;


procedure TLControl.FocusLoose;
begin
  //
end;


procedure TLControl.MouseEnter;
begin
  //
end;


procedure TLControl.MouseExit;
begin
  //
end;


procedure TLControl.UpdateState;
begin
  //
end;


procedure TLControl.Render;
begin
  if SHOW_CONTROLS_OVERLAYS then
    RenderOutlines;
end;


procedure TLControl.RenderOutlines;
var Col: TColor4c;
begin
  glLineWidth(1);

  Col := $20000080;
  if Self is TLPanel then       Col := $100000FF;
  if Self is TLLabel then       Col := $1000FFFF;
  if Self is TLButton then      Col := $1000FF00;
  if Self is TLShape then       Col := $10FFFF80;
  if Self is TLDropBox then     Col := $10FF0000;
  if Self is TLImage then       Col := $00000000;
  if Self is TLCheckBox then    Col := $100080FF;
  if Self is TLEditControl then Col := $10008000;
  if Self is TLMemo then        Col := $10800080;

  RenderQuad(GL_QUADS, AbsLeft, AbsTop, AbsWidth, AbsHeight, Col);
  RenderQuad(GL_LINE_LOOP, AbsLeft+0.5, AbsTop+0.5, AbsWidth-1, AbsHeight-1, $80FFFFFF);

  Col := $800000FF;
  if csOver in fControlState then Col := $8000FFFF;
  if csDown in fControlState then Col := $80FFFFFF;

  RenderQuad(GL_QUADS, AbsLeft-3, AbsTop-3, 6, 6, Col);
end;


{ TLPanel }
constructor TLPanel.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fChildCount := 0;
  SetLength(Childs, 0);

  //if we are Master
  if aParent = nil then
  begin
    fToolTip_Hint := TLToolTip.Create(nil);
    fToolTip_Hint.Parent := Self; //Set parent, but without adding to ChildList
    fToolTip_Hint.Visible := False;
  end;
end;


destructor TLPanel.Destroy;
begin
  RemAllChilds;
  fToolTip_Hint.Free;
  inherited;
end;


procedure TLPanel.ExportControlsTree(S: TStringList; aTab: Byte = 0);
var I: Integer;
begin
  inherited;
  for I := 1 to fChildCount do
    Childs[I].ExportControlsTree(S, aTab + 1);
end;


{ Rest flag on previous control, set flag to new control and update CtrlDown}
procedure TLPanel.SetCtrlDown(aCtrl: TLControl);
begin
  if (fCtrlDown <> aCtrl) and (aCtrl<>nil) then CtrlFocus := aCtrl;

  if fCtrlDown <> nil then fCtrlDown.fControlState := fCtrlDown.fControlState - [csDown];
  if aCtrl <> nil then aCtrl.fControlState := aCtrl.fControlState + [csDown];
  fCtrlDown := aCtrl;
end;


procedure TLPanel.SetCtrlFocus(aCtrl: TLControl);
var C: TLControl;
begin
  if fCtrlFocus <> nil then fCtrlFocus.fControlState := fCtrlFocus.fControlState - [csFocus];
  if aCtrl <> nil then aCtrl.fControlState := aCtrl.fControlState + [csFocus];

  //Remember old focused control to send OnFocusLoose to it after assigning new CtrlFocus,
  //so it can be known by event handler
  C := fCtrlFocus;
  fCtrlFocus := aCtrl;

  if (C <> nil) and (C <> fCtrlFocus) then begin
    C.FocusLoose; //Perform internal magic and issue event afterwards
    if Assigned(C.fOnFocusLoose) then
      C.fOnFocusLoose(C); //Sender=Self, but CtrlFocus=New focused control
  end;
end;


procedure TLPanel.SetCtrlOver(aCtrl: TLControl);
begin
  if fCtrlOver <> nil then
  begin
    fCtrlOver.fControlState := fCtrlOver.fControlState - [csOver];
    fCtrlOver.MouseExit; //Perform internal magic
    if Assigned(fCtrlOver.fOnMouseExit) then fCtrlOver.fOnMouseExit(fCtrlOver);
  end;
  if aCtrl <> nil then
  begin
    aCtrl.fControlState := aCtrl.fControlState + [csOver];
    aCtrl.MouseEnter; //Perform internal magic
    if Assigned(aCtrl.fOnMouseEnter) then aCtrl.fOnMouseEnter(aCtrl);
  end;
  fCtrlOver := aCtrl;
end;


procedure TLPanel.SetCtrlUp(aCtrl: TLControl);
begin
  fCtrlUp := aCtrl;
end;


procedure TLPanel.SetLocHeight(aValue: Integer);
var
  I: Integer;
begin
  for I := 1 to fChildCount do
    if (akTop in Childs[I].Anchors) and (akBottom in Childs[I].Anchors) then
      Childs[I].LocHeight := Childs[I].LocHeight + (aValue - fHeight)
    else
    if akTop in Childs[I].Anchors then
      //Do nothing
    else
    if akBottom in Childs[I].Anchors then
      Childs[I].fTop := Childs[I].fTop + (aValue - fHeight)
    else
      Childs[I].fTop := Childs[I].fTop + (aValue - fHeight) div 2;

  inherited;
end;


procedure TLPanel.SetLocWidth(aValue: Integer);
var
  I: Integer;
begin
  for I := 1 to fChildCount do
    if (akLeft in Childs[I].Anchors) and (akRight in Childs[I].Anchors) then
      Childs[I].LocWidth := Childs[I].LocWidth + (aValue - fWidth)
    else
    if akLeft in Childs[I].Anchors then
      //Do nothing
    else
    if akRight in Childs[I].Anchors then
      Childs[I].fLeft := Childs[I].fLeft + (aValue - fWidth)
    else
      Childs[I].fLeft := Childs[I].fLeft + (aValue - fWidth) div 2;

  inherited;
end;


procedure TLPanel.HintShow(X, Y: Integer; aCaption: string);
begin
  fToolTip_Hint.Visible := aCaption <> '';
  fToolTip_Hint.PositionAt(X, Y, aCaption);
end;


procedure TLPanel.HintHide;
begin
  fToolTip_Hint.Visible := False;
end;


function TLPanel.HintVisible: Boolean;
begin
  Result := fToolTip_Hint.fVisible;
end;


procedure TLPanel.AddChild(aControl: TLControl);
begin
  inc(fChildCount);
  SetLength(Childs, fChildCount + 1);
  Childs[fChildCount] := aControl; //1..n
  aControl.Parent := Self;
end;


procedure TLPanel.InsertChild(aControl: TLControl; aIndex: Integer);
var I: Integer;
begin
  inc(fChildCount);
  SetLength(Childs, fChildCount + 1);

  for I := fChildCount - 1 downto aIndex do //Move all down the list
    Childs[I+1] := Childs[I];

  Childs[aIndex] := aControl;
  aControl.Parent := Self;
end;


procedure TLPanel.RemChild(aControl: TLControl);
var i,k: Integer;
begin
  for i:=fChildCount downto 1 do
  if Childs[i] = aControl then begin

    for k:=i to fChildCount-1 do
      Childs[k] := Childs[k+1];

    dec(fChildCount);
    SetLength(Childs, fChildCount+1); //Trim
    //aControl.Parent := nil; //Clean up
    exit;
  end;
end;


//Keep on removing existing childs
//We do it this way because some Childs will remove their Childs/Neighbours as well
//(e.g. ScrollBars have 2 buttons as "childs", thus removing a ScrollBar remove 3 controls at once)
procedure TLPanel.RemAllChilds;
begin
  while fChildCount > 0 do
    Childs[1].Free;
end;


//Will always return last created control (which one is on top)
function TLPanel.HitControl(X,Y: Integer): TLControl;
var I: Integer; C: TLControl;
begin
  for I := fChildCount downto 1 do
    if Childs[I].fVisible and Childs[I].fEnabled and Childs[I].Hitable then
    begin
      C := Childs[I].HitControl(X, Y);
      if C <> nil then
      begin
        Result := C;
        Exit;
      end;
    end;
  Result := nil;
end;


procedure TLPanel.KeyDown(Key: word; Shift: TShiftState);
begin
  //If we are the top-most Panel of all
  if (Parent = nil) and (CtrlFocus <> nil) then
    CtrlFocus.KeyDown(Key, Shift);
end;


procedure TLPanel.KeyPress(Key: Char);
begin
  //If we are the top-most Panel of all
  if (Parent = nil) and (CtrlFocus <> nil) then
    CtrlFocus.KeyPress(Key);
end;


procedure TLPanel.KeyUp(Key:word; Shift: TShiftState);
begin
  //If we are the top-most Panel of all
  if (Parent = nil) and (CtrlFocus <> nil) then
    CtrlFocus.KeyUp(Key, Shift);
end;


{ Send MouseDown to the last created control}
procedure TLPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  //If we are the top-most Panel of all
  if (Parent = nil) then
  begin
    CtrlDown := HitControl(X,Y);
    if (CtrlDown <> nil) then CtrlDown.MouseDown(Button, Shift, X, Y);
  end;
end;


procedure TLPanel.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  //If we are the top-most Panel of all
  if (Parent = nil) then
  begin
    CursorPos.X := X;
    CursorPos.Y := Y;

    if (fCtrlDown <> nil) and fCtrlDown.CanDrag then
      if not fCtrlDown.IsDragged then
      begin
        fCtrlDrag := fCtrlDown;
        fCtrlDrag.DragStart(mbLeft, Shift, X, Y);
      end
      else
      begin
        fCtrlDrag.Drag(Shift, X, Y);
        //When we drag something we can check if controls below can accept the Drop
        //Check CanDrop of controls below
        Exit;
      end;

    if CtrlDown = nil then
      CtrlOver := HitControl(X,Y);
    if CtrlOver <> nil then
      CtrlOver.MouseMove(Shift, X, Y);
  end;
end;


procedure TLPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  //If we are the top-most Panel of all
  if (Parent = nil) then
  begin
    //If we dragged something - tell it where we released it
    //Don't generate MouseUp for control above which we released
    if (fCtrlDrag <> nil) then
      fCtrlDrag.DragEnd(fCtrlUp, X, Y)
    else
    begin
      CtrlUp := HitControl(X,Y);
      if (CtrlUp <> nil) and (CtrlUp = CtrlDown) then
        CtrlUp.MouseUp(Button, Shift, X, Y);
    end;

    CtrlDown := nil; //Release it after OnClick checked it
    fCtrlDrag := nil;

    Screen.Cursor := crDefault; //Make sure cursor is visible if it was hidden and released
  end;
end;


procedure TLPanel.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean);
var C: TLControl;
begin
  //If we are the top-most Panel of all
  if (Parent <> nil) then Exit;

  C := HitControl(X,Y);
  if (C <> nil) then
    C.MouseWheel(Shift, WheelDelta, X, Y, Handled);

  //Signal MouseWheel to parent control until it gets handled (excl. Self)
  while not Handled and (C.Parent <> nil) and (C.Parent <> Self) do
  begin
    C := C.Parent;
    C.MouseWheel(Shift, WheelDelta, X, Y, Handled);
  end;
end;


procedure TLPanel.UpdateState;
var i: Integer;
begin
  inherited;
  for i:=1 to fChildCount do
    if Childs[i].fVisible then
      Childs[i].UpdateState;
end;


procedure TLPanel.Render;
var i: Integer;
begin
  for i:=1 to fChildCount do
    if Childs[i].fVisible then
      Childs[i].Render;

  if (Parent = nil) and fToolTip_Hint.fVisible then
    fToolTip_Hint.Render;
end;


procedure TLPanel.RenderOutlines;
var I: Integer;
begin
  inherited;
  for I := 1 to fChildCount do
    if Childs[I].fVisible then
      Childs[I].RenderOutlines;
end;


{ TLImage }
constructor TLImage.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aPath: string; aPathAct: string='');
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  TickTransparent := 0;
  TransparentTime := 0;
  fOpaqueness := 1;

  RenderStyle := isStretch;

  SetImagePath(aPath);
  SetImagePathAct(aPathAct);

  fImagePath   := aPath;
  fImagePathAct:= aPathAct;
end;


procedure TLImage.SetImagePath(aPath: string);
var Ext: string;
begin
  if fImagePath = aPath then Exit;
  if not LOAD_TEX_FROM_RES and not FileExists(aPath) then Exit;

  fImagePath := aPath;
  LoadTexture(aPath, LOAD_TEX_FROM_RES, ImageMain);

  //Load click-mask
  Ext := LeftStr(aPath,length(aPath)-4) + '_' + RightStr(aPath, 4);
  if FileExists(Ext) then
  begin
    FillChar(fMask, SizeOf(fMask),#255);
    LoadTextureTGAMask(Ext, @fMask, 16, 16);
    fUseMask := true;
  end;
end;


procedure TLImage.SetImagePathAct(aPath: string);
begin
  if fImagePathAct = aPath then Exit;
  if not LOAD_TEX_FROM_RES and not FileExists(aPath) then Exit;

  fImagePathAct := aPath;
  LoadTexture(aPath, LOAD_TEX_FROM_RES, ImageAlt);
end;


procedure TLImage.AnimShow(AnimLength:word=UI_ANIM_FRM_TIME);
begin
  fOpaqueness := 0;
  TickTransparent := TimeGetTime;
  TransparentTime := AnimLength;
end;


function TLImage.HitTest(X,Y: Integer): Boolean;
var px,py: Integer;
begin
  Result := inherited HitTest(X,Y);

  if Result and fUseMask then
  begin
    px := Round((X-AbsLeft)/AbsWidth * 15); //0..15
    py := Round((Y-AbsTop)/AbsHeight * 15); //0..15
    Result := fMask[py,px] <> 0;
  end;
end;


procedure TLImage.UpdateState;
var MixValue: Single;
begin
  if TickTransparent <> 0 then
  begin

    MixValue := min(TimeGetTime - TickTransparent, TransparentTime) / TransparentTime; //Get 0..1 range
    if MixValue >= 1 then
    begin
      TickTransparent := 0;
      fOpaqueness := 1;
      Exit;
    end;

    fOpaqueness := MixValue;
  end;
end;


procedure TLImage.Render;
var Col: TColor4c;
begin
  inherited;
  if Opaqueness = 0 then Exit;

  Col := $FFFFFF OR (Byte(Round(fOpaqueness*255)) SHL 24);

  if (csOver in fControlState) and Assigned(fOnClick) and (ImageAlt.ID <> 0) then
    RenderTex(ImageAlt, RenderStyle, AbsLeft, AbsTop, AbsWidth, AbsHeight, Col)
  else
    RenderTex(ImageMain, RenderStyle, AbsLeft, AbsTop, AbsWidth, AbsHeight, Col);
end;


{ TLShape }
constructor TLShape.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aColor: TColor4c);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fColor     := aColor;
  DropShadow := 0;
end;


procedure TLShape.Render;
begin
  inherited;
  if DropShadow > 0 then RenderShadowQuad(AbsLeft, AbsTop, AbsWidth, AbsHeight, DropShadow, $68000000);
  RenderQuad(GL_QUADS, AbsLeft, AbsTop, AbsWidth, AbsHeight, fColor);
end;


{ TLLabelCommon }
constructor TLLabel.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer;
                           aCaption: string = ''; aSize: Integer = DEF_FONT_SIZE;
                           aStyle: Integer = 0; aJustify: TAlignment = taLeftJustify);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  fFont := MakeFont(UI_COLOR_FONT, aSize, aStyle, aJustify);
  SetCaption(aCaption);
end;


function TLLabel.HitTest(X,Y: Integer): Boolean;
var Off: Integer;
begin
  Off := 0;

  if LocWidth = 0 then
  case fFont.Justify of
    taLeftJustify:  Off := 0;
    taCenter:       Off := -AbsWidth div 2;
    taRightJustify: Off := -AbsWidth;
    else            Off := 0;
  end;

  Result := InRange(X, AbsLeft + Off, AbsLeft + AbsWidth + Off) and
            InRange(Y, AbsTop, AbsTop + AbsHeight);
end;


procedure TLLabel.SetCaption(const Value: string);
begin
  fCaption := Value;
  fHeight := Round(fFontLib.GetStringHeight(fFont, fCaption) * LINE_HEIGHT_MUL);
end;


procedure TLLabel.SetFont(const Value: TFontProps);
begin
  fFont := Value;
  fHeight := Round(fFontLib.GetStringHeight(fFont, fCaption) * LINE_HEIGHT_MUL);
end;


function TLLabel.TextWidth: Single;
begin
  Result := fFontLib.GetStringWidth(fCaption, fFont);
end;


function TLLabel.TextHeight: Single;
begin
  Result := fFontLib.GetStringHeight(fFont, fCaption);
end;


procedure TLLabel.Render;
begin
  inherited;
  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);
    fFontLib.Render(0, 0, fWidth, fHeight, Scale, fCaption, fFont, True);
  glPopMatrix;
end;


constructor TLEditControl.Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
begin;
  inherited Create(aParent, aLeft,aTop,aWidth,0);
end;


procedure TLEditControl.Render;
begin
  inherited;
end;


function TLEditControl.GetBackColor: TColor4c;
begin
  if Enabled then
    if not ReadOnly and ((csOver in fControlState) or (csFocus in fControlState)) then
      Result := $FFE0E0E0
    else
      Result := $FFFFFFFF
  else
    Result := $B0FFFFFF;
end;


procedure TLEditControl.NotifyChange;
begin
  if Assigned(fOnChange) then fOnChange(Self);
end;


{ TLEdit }
constructor TLEdit.Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
begin;
  inherited Create(aParent, aLeft,aTop,aWidth);
  Font.Color := UI_COLOR_FONT;
  Font.Size := DEF_FONT_SIZE;
  fCursorPos := 0;
  fText := '';
  MaxLength := 32;
end;


procedure TLEdit.SetText(aText: string);
begin
  fText := aText;
  fCursorPos := length(fText);
end;


procedure TLEdit.KeyDown(Key:word; Shift: TShiftState);
begin
  inherited;
  if not Enabled or ReadOnly then exit;

  case Key of
    VK_BACK:    begin Delete(fText, fCursorPos, 1); dec(fCursorPos); end;
    VK_DELETE:  Delete(fText, fCursorPos+1, 1);
    VK_LEFT:    dec(fCursorPos);
    VK_RIGHT:   inc(fCursorPos);
  end;

  fCursorPos := EnsureRange(fCursorPos, 0, length(fText));
  NotifyChange;
end;


procedure TLEdit.KeyPress(Key: Char);
begin
  if not Enabled or ReadOnly then exit;
  if not ((Key in [#32..#125, #185, #192..#255]) and (Length(fText) < MaxLength)) then Exit;

  fText := Copy(fText, 1, fCursorPos) + Key + Copy(fText, fCursorPos+1, length(fText)-fCursorPos);
  inc(fCursorPos);

  NotifyChange;
end;


procedure TLEdit.UpdateCursorPos(X: Single);
var i: Integer; LineLen,Best: Single;
begin
  fCursorPos := 0;
  Best := maxint;

  for i:=0 to length(fText) do begin
    LineLen := DEF_TEXT_MARGIN + fFontLib.GetStringWidth(Copy(fText,1,i), Font);
    if abs(LineLen - X) >= Best then
      break;
    fCursorPos := i;
    Best := abs(LineLen - X);
  end;
end;


procedure TLEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  UpdateCursorPos((X - AbsLeft)/Scale);
end;


procedure TLEdit.Render;
var
  TextWidth: Integer;
  TextToRender: string;
begin
  inherited;
  glPushMatrix;
    glTranslatef(AbsLeft,AbsTop,0);
    glScale2(Scale);

    fHeight := Round(fFontLib.GetStringHeight(Font)*LINE_HEIGHT_MUL);

    //RenderTex(TexBackground, is9Tap, 0, 0, fWidth, fHeight, GetBackColor);

    if MaskText then
      TextToRender := DupeString('*', length(fText))
    else
      TextToRender := stringReplace(fText, eol, ' ', [rfReplaceAll]);

    fFontLib.Render(DEF_TEXT_MARGIN, 0, fWidth-DEF_TEXT_MARGIN*2, fHeight, Scale, TextToRender, Font);

    TextWidth := Round(fFontLib.GetStringWidth(Copy(TextToRender, 1, fCursorPos),Font));

    //Blinking cursor
    if Enabled and not ReadOnly and (csFocus in fControlState) and ((TimeGetTime div 500) mod 2 = 0) then
      RenderLine(DEF_TEXT_MARGIN+TextWidth, fHeight*0.15, 0, fHeight*0.7, 2*Scale, Font.Color);

  glPopMatrix;
end;


constructor TLMemo.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin;
  inherited Create(aParent, aLeft,aTop,aWidth);
  fHeight := aHeight;
  Font.Color := UI_COLOR_FONT;
  Font.Size := DEF_FONT_SIZE;
  fText := '';
  fCursorPos := 0;
  fTextRender := TStringList.Create;

  fScrollBar := TLScrollBar.Create(aParent, 0, 0, aHeight, sa_Vertical);
  fScrollBar.MinValue := 0;
  fScrollBar.Position := 0;
end;


destructor TLMemo.Destroy;
begin
  fTextRender.Free;
  inherited;
end;


procedure TLMemo.SetText(aText: string);
begin
  fText := aText;
  fCursorPos := length(fText);
end;


//Fit coordinates to Controls bounds
procedure TLMemo.EnsureCursorInView(Y: Integer);
begin
{  if fScrollBar.Position > 0 then
    Y := Y - fLineSpacing;}

  if Y < (fScrollBar.Position)*fLineSpacing then
    fScrollBar.Position := max(Y div fLineSpacing, fScrollBar.MinValue);

  if Y >= fScrollBar.Position*fLineSpacing + fHeightOfLines then
    fScrollBar.Position := min((Y - fHeightOfLines) div fLineSpacing + 1, fScrollBar.MaxValue);
end;


//Convert coordinates to CursorPos
procedure TLMemo.UpdateCursorPos(X,Y: Integer);
var i,CharNum: Integer; LineLen: Single; LineNum: Integer;
begin
  UpdateSize;

  //Fit range
  X := EnsureRange(X, 0, fWidth);
  Y := EnsureRange(Y, 0, fScrollBar.MaxValue*LineSpacing + fHeightOfLines);
  LineNum := Y div LineSpacing; //0..n
  fCursorPos := 0;

  //Add all lines from above
  for i:=0 to min(fTextRender.Count, LineNum)-1 do
    inc(fCursorPos, length(fTextRender[i]));

  //Add current line
  if LineNum < fTextRender.Count then
  begin
    //First line contains Caption
    if LineNum = 0 then LineLen := fCaptionWidth
                   else LineLen := 0;
    CharNum := 0;
    while (LineLen<X) and (CharNum<length(fTextRender[LineNum])) do begin
      inc(CharNum);
      LineLen := fFontLib.GetStringWidth(Copy(fTextRender[LineNum],1,CharNum), Font);
      if LineNum = 0 then LineLen := LineLen + fCaptionWidth; //Add captions width
    end;
    inc(fCursorPos, CharNum);
  end;
end;


procedure TLMemo.UpdateCursorCoord(aCursorPos: Integer);
var i: Integer; LineNum: Integer;
begin
  LineNum := 1;
  i := aCursorPos;

  //Skip previous lines
  while (LineNum<fTextRender.Count) and (i >= length(fTextRender[LineNum-1])) do begin
    dec(i, length(fTextRender[LineNum-1]));
    inc(LineNum);
  end;

  //Skip previous chars
  if fTextRender.Count >= LineNum then
    fCursorCoord.X := Round(fFontLib.GetStringWidth(Copy(fTextRender[LineNum-1], 0, i), Font))
  else
    fCursorCoord.X := 0;

  //Add captions width
  if LineNum=1 then inc(fCursorCoord.X, fCaptionWidth);

  fCursorCoord.Y := (LineNum-1)*LineSpacing; //1st line - no offset
end;


procedure TLMemo.KeyDown(Key:word; Shift: TShiftState);
begin
  inherited;
  if not Enabled or (ReadOnly and ((Key = VK_BACK) or (Key = VK_DELETE))) then exit;

  case Key of
    VK_BACK:    begin
                  Delete(fText, fCursorPos, 1);
                  dec(fCursorPos);
                end;
    VK_DELETE:  Delete(fText, fCursorPos+1, 1);
    VK_UP:      begin
                  dec(fCursorCoord.Y, LineSpacing);
                  UpdateCursorPos(fCursorCoord.X, fCursorCoord.Y);
                end;
    VK_DOWN:    begin
                  inc(fCursorCoord.Y, LineSpacing);
                  UpdateCursorPos(fCursorCoord.X, fCursorCoord.Y);
                end;
    VK_LEFT:    dec(fCursorPos);
    VK_RIGHT:   inc(fCursorPos);
  end;

  fCursorPos := EnsureRange(fCursorPos, 0, length(fText));

  UpdateCursorCoord(fCursorPos);
  EnsureCursorInView(fCursorCoord.Y);

  if Key in [VK_BACK, VK_DELETE] then //Only these two change text
    NotifyChange;
end;


procedure TLMemo.KeyPress(Key: Char);
begin
  if not Enabled or ReadOnly then exit;
  if not (Key in [#32..#125, #185, #192..#255]) then exit;

  fText := Copy(fText, 1, fCursorPos) + Key + Copy(fText, fCursorPos+1, length(fText)-fCursorPos);
  inc(fCursorPos);

  UpdateSize; //It will reposition EOLs
  UpdateCursorCoord(fCursorPos);
  NotifyChange;
end;


procedure TLMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  UpdateCursorPos(X-AbsLeft, ((Y-AbsTop) div fLineSpacing + fScrollBar.Position) * fLineSpacing);
  //Place cursor on right coords (i.e. user clicked on a char, place Cursor next to it)
  UpdateCursorCoord(fCursorPos);
end;


procedure TLMemo.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean);
begin
  inherited;
  fScrollBar.MouseWheel(Shift, WheelDelta, X, Y, Handled);
end;


procedure TLMemo.UpdateSize;
begin
  //Metrics
  fLineHeight := Round(fFontLib.GetStringHeight(Font)*LINE_HEIGHT_MUL);
  fLineSpacing := max(fLineSpacing, fLineHeight);
  fCaptionWidth := Round(fFontLib.GetStringWidth(fCaption, Font)+fLineHeight*0.25);
  fHeightOfLines := fHeight div fLineSpacing * fLineSpacing;

  //Reposition end-of-lines
  fTextRender.Text := fFontLib.RepositionEOLs(fText, Font, fWidth-fLineHeight, fCaptionWidth);
  fLineCount := fTextRender.Count;

  //SCROLLBAR
  //fScrollBar.Visible := fLineCount > (fHeight div fLineSpacing);
  fScrollBar.fScale := fScale;
  fScrollBar.fLeft := fLeft + Round((fWidth-fLineHeight)*fScale);
  fScrollBar.fTop := fTop;
  fScrollBar.fWidth := fLineHeight;
  fScrollBar.fHeight := fHeight;

  fScrollBar.MinValue := 0;
  fScrollBar.MaxValue := max(fLineCount - (fHeight div fLineSpacing), 0);
  fScrollBar.PageSize := fLineCount;
  fScrollBar.Enabled := fScrollBar.MaxValue <> fScrollBar.MinValue;
end;


procedure TLMemo.Render;
var I: Integer;
begin
  inherited;
  UpdateSize;
  glPushMatrix;
    glTranslatef(AbsLeft,AbsTop,0);
    glScale2(Scale);

    //RenderTex(TexBackground, is9Tap, 0, 0, fWidth, fHeight-fLineHeight, GetBackColor);

    fFontLib.Render(0, 0, fCaptionWidth, fLineHeight, Scale, fCaption, Font);
    if fTextRender.Count > 0 then
      fFontLib.Render(fCaptionWidth, 0, fWidth-fLineHeight-fCaptionWidth, fLineHeight, Scale, fTextRender[0], Font, false);

    for I := 1 to min(fTextRender.Count, fHeight div fLineSpacing) - 1 do
      fFontLib.Render(0, I*LineSpacing, fWidth-fLineHeight, fLineHeight, Scale, fTextRender[fScrollBar.Position+I], Font, false);

    //Cursor
    if not ReadOnly and Enabled and (csFocus in fControlState) and ((TimeGetTime div 500) mod 2 = 0) then
      RenderLine(fCursorCoord.X, fCursorCoord.Y + fLineHeight*0.15 - fScrollBar.Position*LineSpacing, 0, fLineHeight*0.7, 2*Scale, Font.Color);

    //Show exact cursor position for debug
    if SHOW_TEXT_BOUNDS then
      RenderQuad(GL_QUADS, fCursorCoord.X, fCursorCoord.Y - fScrollBar.Position*LineSpacing, 1, LineSpacing, $FF0000FF);

  glPopMatrix;
end;


{ TLCheckBox }
constructor TLCheckBox.Create(aParent: TLPanel; aLeft,aTop: Integer; aCaption: string);
begin
  inherited Create(aParent, aLeft, aTop, 0);
  Font.Color  := UI_COLOR_FONT;
  Font.Size   := DEF_FONT_SIZE;
  fCaption    := aCaption;
  fChecked    := False;
  fHeight := Ceil(fFontLib.GetStringHeight(Font, fCaption) * LINE_HEIGHT_MUL);
  fWidth  := Ceil(fFontLib.GetStringWidth(fCaption, Font) + TexChkChecked.Norm.X * 1.5) + 1;
end;


procedure TLCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if (csDown in fControlState) and (Button = mbLeft)
  and Enabled and not ReadOnly then
  begin
    fChecked := not fChecked;
    NotifyChange; //OnChange preceeds OnClick
  end;

  inherited; //OnClick will have actual Checked state
end;


procedure TLCheckBox.Render;
var
  Col: TColor4c;
  Tex: TLTexture;
begin
  inherited;

  if fWidth <= 0 then Exit;

  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    if Enabled then Col := $FFFFFFFF
               else Col := UI_COLOR_DISABLED;

    if Checked then
      if not fEnabled then
        Tex := TexChkChecked.Disa
      else
      if (csDown in fControlState) then
        Tex := TexChkChecked.Down
      else
      if (csOver in fControlState) then
        Tex := TexChkChecked.Over
      else
        Tex := TexChkChecked.Norm
    else
      if not fEnabled then
        Tex := TexChkUnchecked.Disa
      else
      if (csDown in fControlState) then
        Tex := TexChkUnchecked.Down
      else
      if (csOver in fControlState) then
        Tex := TexChkUnchecked.Over
      else
        Tex := TexChkUnchecked.Norm;

    RenderTex(Tex, isCenter, 0, (fHeight - Tex.Y) div 2, Tex.X, Tex.Y, Col);

    //Render text
    fFontLib.Render(Trunc(Tex.X * 1.5), 0, fWidth - Trunc(Tex.X * 1.5), fHeight-2, Scale, Caption, Font);

  glPopMatrix;
end;


{ TLButton }
constructor TLButton.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption: string);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  Font.Color    := UI_COLOR_FONT;
  Font.Justify  := taCenter;
  Font.Size     := DEF_FONT_SIZE;
  Caption       := aCaption;
  fFace         := TexBtnDefault;
  ButtonFaceStyle := is9Tap;
end;


constructor TLButton.Create(aParent: TLPanel; aLeft, aTop: Integer; aTexMask: string);
begin
  inherited Create(aParent, aLeft, aTop, 0, 0);
  LoadTexture(Format(aTexMask, ['disa']), LOAD_TEX_FROM_RES, fFace.Disa);
  LoadTexture(Format(aTexMask, ['norm']), LOAD_TEX_FROM_RES, fFace.Norm);
  LoadTexture(Format(aTexMask, ['over']), LOAD_TEX_FROM_RES, fFace.Over);
  LoadTexture(Format(aTexMask, ['down']), LOAD_TEX_FROM_RES, fFace.Down);
  ButtonFaceStyle := isStretch;
  fWidth := fFace.Norm.X;
  fHeight := fFace.Norm.Y;
end;


constructor TLButton.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aTextures: TButtonTexSet);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  Font.Color    := UI_COLOR_FONT;
  Font.Justify  := taCenter;
  Font.Size     := DEF_FONT_SIZE;
  Caption       := '';
  fFace         := aTextures;
  ButtonFaceStyle := is9Tap;
  ImageStyle      := isCenter;
end;


constructor TLButton.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer; aCaption, aImage: string);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);
  Font.Color    := UI_COLOR_FONT;
  Font.Justify  := taCenter;
  Font.Size     := DEF_FONT_SIZE;
  Caption       := aCaption;
  fFace         := TexBtnDefault;
  ButtonFaceStyle     := is9Tap;
  if aImage <> '' then
    LoadTexture(aImage, LOAD_TEX_FROM_RES, fImage);
end;


procedure TLButton.SetDown(const Value: Boolean);
begin
  fDown := Value;
end;


//Repeat Click if user holds mouse button (currently used for Srollbars)
procedure TLButton.UpdateState;
begin
  inherited;
  if not fRepeatClick then exit;

  if csDown in fControlState then
  begin
    if fDownSince = 0 then
      fDownSince := TimeGetTime
    else
      if TimeGetTime > fDownSince + 100 then
      begin
        if Assigned(fOnMouseDown) then fOnMouseDown(Self, mbLeft, [], 0, 0);
        fDownSince := TimeGetTime;
      end;
  end
  else
    fDownSince := 0; //Button released
end;


procedure TLButton.Render;
var
  Fnt: TFontProps;
  Inset: Word;
  T: TLTexture;
begin
  if fWidth <= 0 then Exit;

  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    if Enabled then
      if fDown or (csDown in fControlState) then
        T := fFace.Down
      else if (csOver in fControlState) then
        T := fFace.Over
      else
        T := fFace.Norm
    else
      T := fFace.Disa;

    RenderTex(T, ButtonFaceStyle, 0, 0, fWidth, fHeight, $FFFFFFFF);

    if Enabled then
      RenderTex(fImage, ImageStyle, 0, 0, fWidth, fHeight, $FFFFFFFF)
    else
      RenderTex(fImage, ImageStyle, 0, 0, fWidth, fHeight, $80FFFFFF);

    if Caption <> '' then
    begin
      Fnt := Font;
      if not Enabled then
        Fnt.Color := $FF888888; //Greyed

      Inset := Round(Font.Size * 0.66);
      fFontLib.Render(Inset, Inset, fWidth-Inset*2, fHeight-Inset*2, Scale, Caption, Fnt);
    end;

  glPopMatrix;
end;


{ TLScrollBar }
constructor TLScrollBar.Create(aParent: TLPanel; aLeft,aTop,aLength: Integer; aScrollAxis: TScrollAxis);
var
  W, H: Integer;
  TexInc, TexDec: TButtonTexSet;
begin
  //Depending on scroll axis aWidth is either Width or Height of a scroll line
  fScrollAxis := aScrollAxis;
  fPageSize := 1;
  fPosition := 1;
  fMinValue := 1;
  fMaxValue := 10;

  W := aLength;
  H := aLength;
  if fScrollAxis = sa_Vertical then
  begin
    W := TexScrollUp.Norm.X;
    TexDec := TexScrollUp;
    TexInc := TexScrollDown;
  end
  else
  begin
    H := TexScrollLeft.Norm.Y;
    TexDec := TexScrollLeft;
    TexInc := TexScrollRight;
  end;

  inherited Create(aParent, aLeft, aTop, W, H);

  //These two will be added to collection by themselfes
  ScrollDec := TLButton.Create(aParent, 0, 0, 0, 0, TexDec);
  ScrollDec.ButtonFaceStyle := isCenter;
  ScrollDec.OnMouseDown := DecPosition;
  ScrollDec.fRepeatClick := True;

  ScrollInc := TLButton.Create(aParent, 0, 0, 0, 0, TexInc);
  ScrollInc.ButtonFaceStyle := isCenter;
  ScrollInc.OnMouseDown := IncPosition;
  ScrollInc.fRepeatClick := True;

  UpdateSize;
end;


destructor TLScrollBar.Destroy;
begin
  ScrollDec.Free;
  ScrollInc.Free;
  inherited;
end;


function TLScrollBar.ThumbPos: Integer;
begin
  Result := 0;

  if MinValue = MaxValue then
    case fScrollAxis of
      sa_Vertical:    Result := (fHeight - fBtnSize * 2 - fThumbLen) div 2;
      sa_Horizontal:  Result := (fWidth - fBtnSize * 2 - fThumbLen) div 2;
    end
  else
    case fScrollAxis of
      sa_Vertical:    Result := Round((Position - MinValue) /
                                      (MaxValue - MinValue) *
                                      (fHeight - fBtnSize * 2 - fThumbLen));
      sa_Horizontal:  Result := Round((Position - MinValue) /
                                      (MaxValue - MinValue) *
                                      (fWidth - fBtnSize * 2 - fThumbLen));
    end;
end;


procedure TLScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  inherited;

  case fScrollAxis of
    sa_Vertical:    fMoveOffset := Y - AbsTop - Round((fBtnSize + fThumbLen / 2) * Scale) - ThumbPos;
    sa_Horizontal:  fMoveOffset := X - AbsLeft - Round((fBtnSize + fThumbLen / 2) * Scale) - ThumbPos;
  end;

  if Abs(fMoveOffset) > fThumbLen / 2 then
    fMoveOffset := 0;

  MouseMove(Shift,X,Y); //Will change Position and call OnChange event
end;


procedure TLScrollBar.MouseMove(Shift: TShiftState; X,Y: Integer);
var
  NewPos: Integer;
  Loc, Span: Single;
begin
  inherited;
  if not (ssLeft in Shift) then Exit;

  Loc := 0;
  Span := 0;

  case fScrollAxis of
    sa_Vertical:    begin
                      Loc := Y - AbsTop - (fBtnSize + fThumbLen / 2) * Scale - fMoveOffset;
                      Span := (fHeight - fBtnSize * 2 - fThumbLen) * Scale;
                    end;
    sa_Horizontal:  begin
                      Loc := X - AbsLeft - (fBtnSize + fThumbLen / 2) * Scale - fMoveOffset;
                      Span := (fWidth - fBtnSize * 2 - fThumbLen) * Scale;
                    end;
  end;

  NewPos := EnsureRange(MinValue + Round(Loc / Span * (MaxValue - MinValue)), MinValue, MaxValue);

  if NewPos <> Position then
  begin
    Position := NewPos;
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;


procedure TLScrollBar.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean);
var
  OldPosition: Integer;
begin
  inherited;

  OldPosition := fPosition;
  Position := fPosition - Max(fPageSize div 10, 1) * Sign(WheelDelta);
  Handled := OldPosition <> fPosition;

  if Handled and Assigned(fOnChange) then
    fOnChange(Self);
end;


{Refresh button sizes and etc.}
procedure TLScrollBar.UpdateSize;
begin
  //Common properties
  ScrollDec.fLeft := fLeft;
  ScrollDec.fTop := fTop;
  ScrollDec.fScale := fScale;
  ScrollInc.fScale := fScale;

  //Orientation dependant
  case fScrollAxis of
    sa_Vertical:
      begin
        fBtnSize := Min(fWidth, fHeight div 3); //no less than 1/3 of height
        ScrollDec.fHeight := fBtnSize;
        ScrollDec.fWidth := fWidth;

        ScrollInc.fHeight := fBtnSize;
        ScrollInc.fWidth := fWidth;
        ScrollInc.fLeft := fLeft;
        ScrollInc.fTop := fTop + Round((fHeight - fBtnSize) * fScale);

        fThumbLen := Round(Min(fPageSize / (MaxValue - MinValue + fPageSize), 0.8) * (fHeight - 2 * fBtnSize));
        fThumbLen := EnsureRange(fThumbLen, fBtnSize, fHeight - 2 * fBtnSize);
      end;
    sa_Horizontal:
      begin
        fBtnSize := Min(fHeight, fWidth div 3); //1/3 of width
        ScrollDec.fHeight := fHeight;
        ScrollDec.fWidth := fBtnSize;

        ScrollInc.fHeight := fHeight;
        ScrollInc.fWidth := fBtnSize;
        ScrollInc.fLeft := fLeft + Round((fWidth - fBtnSize) * fScale);
        ScrollInc.fTop := fTop;

        fThumbLen := Round(Min(fPageSize / (MaxValue - MinValue + fPageSize), 0.8) * (fWidth - 2 * fBtnSize));
        fThumbLen := EnsureRange(fThumbLen, fBtnSize, fWidth - 2 * fBtnSize);
      end;
  end;
end;


{ Copy property to child buttons. Otherwise they won't be rendered }
procedure TLScrollBar.SetVisible(aValue: Boolean);
begin
  inherited;
  ScrollDec.Visible := fVisible;
  ScrollInc.Visible := fVisible;
end;


{ Copy property to child buttons. Otherwise they won't be rendered }
procedure TLScrollBar.SetEnabled(aValue: Boolean);
begin
  inherited;
  ScrollDec.Enabled := fEnabled;
  ScrollInc.Enabled := fEnabled;
end;


procedure TLScrollBar.SetLocHeight(aValue: Integer);
begin
  inherited;
  UpdateSize;
end;


procedure TLScrollBar.SetMaxValue(aValue: Integer);
begin
  fMaxValue := aValue;
  SetPosition(fPosition);
end;


procedure TLScrollBar.SetPageSize(aValue: Integer);
begin
  fPageSize := EnsureRange(aValue, 1, MaxValue - MinValue);
end;


procedure TLScrollBar.SetPosition(aValue: Integer);
begin
  fPosition := EnsureRange(aValue, MinValue, MaxValue);
end;


procedure TLScrollBar.IncPosition(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  Position := Position + Max(fPageSize div 10, 1);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TLScrollBar.DecPosition(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  Position := Position - Max(fPageSize div 10, 1);
  if Assigned(fOnChange) then fOnChange(Self);
end;


procedure TLScrollBar.Render;
var
  T: TLTexture;
  Col: TColor4c;
begin
  inherited;
  UpdateSize;
  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    if Enabled then Col := $FFFFFFFF else Col := UI_COLOR_DISABLED;

    if fScrollAxis = sa_Vertical then
      RenderTex(TexScrollVLine.Norm, is3TapV, 0, fBtnSize, fWidth, fHeight - fBtnSize * 2, Col)
    else
      RenderTex(TexScrollHLine.Norm, isStretch, fBtnSize, 0, fWidth - fBtnSize * 2, fHeight, Col);

    //Render thumb
    if fScrollAxis = sa_Vertical then
      if not fEnabled then
        T := TexScrollVThumb.Disa
      else
        if (csDown in fControlState) then
          T := TexScrollVThumb.Down
        else
          if (csOver in fControlState) then
            T := TexScrollVThumb.Over
          else
            T := TexScrollVThumb.Norm
    else
      if not fEnabled then
        T := TexScrollHThumb.Disa
      else
        if (csDown in fControlState) then
          T := TexScrollHThumb.Down
        else
          if (csOver in fControlState) then
            T := TexScrollHThumb.Over
          else
            T := TexScrollHThumb.Norm;

    if fScrollAxis = sa_Vertical then
      RenderTex(T, is3TapV, 0, fBtnSize + ThumbPos, fWidth, fThumbLen, Col)
    else
      RenderTex(T, is3TapH, fHeight + ThumbPos, 0, fThumbLen, fHeight, Col);

  glPopMatrix;
end;


{ TLDropBoxCommon }
constructor TLDropBoxCommon.Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
const ItemHeight = 20;
begin
  inherited Create(aParent, aLeft,aTop,aWidth);
  fItemHeight := ItemHeight;
  fCaption := '';
  fItemCount := 0;
  fDropCount := 10;
  fRowCount := 1;
  fBoxText := TStringList.Create;

  OnClick := PanelShow;

  fItems := TStringList.Create;

  Font.Color := UI_COLOR_FONT;
  Font.Size := DEF_FONT_SIZE;

  fButton := TLButton.Create(aParent, aLeft+aWidth-ItemHeight, aTop, ItemHeight, ItemHeight, TexDropBox);
  fButton.OnClick := PanelShow;

  //Create full-screen shape below ListBox to register clicks and hide the list
  fShape := TLShape.Create(Master, -4000, -4000, 8000, 8000, $00000000);
  fShape.Visible := False;
  fShape.OnMouseDown := PanelHide; //Will catch any clicks and hide ListBox

  fPanel := TLPanel.Create(Master, 0, 0, 0, 0);
  fPanel.Visible := False;
  fShape2 := TLShape.Create(fPanel, 0, 0, 0, 0, UI_COLOR_POPUP_FACE);
  fShape2.Anchors := [akLeft, akRight, akTop, akBottom];
  fShape2.DropShadow := UI_SHADOW_CTRL;
end;


destructor TLDropBoxCommon.Destroy;
begin
  fBoxText.Free;
  fItems.Free;
  fButton.Free;
  fShape.Free;
  fPanel.Free;
  //fShape2.Free //Freed by fPanel
  inherited;
end;


procedure TLDropBoxCommon.PanelShow(Sender: TObject);
var
  NewLeft, NewTop: Integer;
begin
  fShape.Visible := True;
  fPanel.Visible := True;
  fPanel.Scale := Scale;

  NewLeft := EnsureRange(-Master.AbsLeft + AbsLeft, 0, Master.AbsWidth - fPanel.AbsWidth);
  NewTop := EnsureRange(-Master.AbsTop + AbsTop + AbsHeight, 0, Master.AbsHeight - fPanel.AbsHeight);
  fAnimator.Add(fPanel,
                Point(-Master.AbsLeft + AbsLeft, -Master.AbsTop + AbsTop + AbsHeight),
                Point(NewLeft, NewTop),
                Point(fWidth, 0),
                Point(fPanel.LocWidth, fPanel.LocHeight),
                Scale, Scale,
                UI_ANIM_CTRL_TIME);
end;


procedure TLDropBoxCommon.PanelHide(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fShape.Visible := false;
  fPanel.Visible := false;
end;


procedure TLDropBoxCommon.SetCaption(aCaption: string);
begin
  fCaption := aCaption;
  fItemHeight := Round(fFontLib.GetStringHeight(Font)*LINE_HEIGHT_MUL);
  fCaptionWidth := Round(fFontLib.GetStringWidth(fCaption, Font) + fItemHeight * 0.25);
end;


procedure TLDropBoxCommon.UpdateSize;
begin
  fItemHeight := Round(fFontLib.GetStringHeight(Font)*LINE_HEIGHT_MUL);
  fHeight := fItemHeight * fRowCount;
  fCaptionWidth := Round(fFontLib.GetStringWidth(fCaption, Font) + fItemHeight * 0.25);

  fButton.Visible := not ReadOnly; //Hide

  fButton.fScale := fScale;
  fButton.fLeft := fLeft+Round((fWidth-fItemHeight)*fScale);
  fButton.fTop  := fTop + fHeight - fItemHeight;
  fButton.fWidth  := fItemHeight;
  fButton.fHeight := fItemHeight;
  fButton.fEnabled := fEnabled; //Copy property
end;


procedure TLDropBoxCommon.UpdateBoxText;
begin
  fBoxText.Text := fFontLib.RepositionEOLs(fBoxText.Text, Font, fWidth, fCaptionWidth + DEF_TEXT_MARGIN);

  //Adjoin last visible row with the next one
  //to be sure there will be '...' instead of clean linebreak
  if (fBoxText.Count > fRowCount) and (fBoxText.Count > 1) then
  begin
    fBoxText[fRowCount-1] := fBoxText[fRowCount-1] + fBoxText[fRowCount];
    fBoxText.Delete(fRowCount);
  end;
end;


procedure TLDropBoxCommon.Render;
  function PadLeft(aLine: Integer): Integer;
  begin
    if (aLine > 0) or (fCaption='') then
      Result := 0
    else
      Result := fCaptionWidth + DEF_TEXT_MARGIN;
  end;
  function PadRight(aLine: Integer): Integer;
  begin
    if ((fRowCount > 1) and (aLine <> fRowCount - 1)) or ReadOnly then
      Result := 0
    else
      Result := fItemHeight;
  end;
var
  I: Integer;
  T: TLTexture;
begin
  inherited;
  UpdateSize;
  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    if Enabled then
      if (csOver in fControlState) then
        T := TexBtnDefault.Over
      else
        T := TexBtnDefault.Norm
    else
      T := TexBtnDefault.Disa;

    fFontLib.Render(0, 0, fWidth - PadRight(0), fItemHeight, Scale, fCaption, Font);

    for I := 0 to fRowCount - 1 do
      RenderTex(T, is9Tap, PadLeft(I), I * fItemHeight, fWidth-PadLeft(I)-PadRight(I), fItemHeight, GetBackColor);


    for I := 0 to min(fRowCount, fBoxText.Count) - 1 do
      fFontLib.Render(DEF_TEXT_MARGIN + PadLeft(I), I * fItemHeight, fWidth-PadLeft(I)-PadRight(I), fItemHeight, Scale, fBoxText[I], Font);

  glPopMatrix;
end;


{ TLDropBox }
constructor TLDropBox.Create(aParent: TLPanel; aLeft,aTop,aWidth: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth);
  fItemIndex := -1;
  fList := TLListBox.Create(fPanel, 0, 0, 0, 0);
  fList.Anchors := [akLeft, akRight, akTop, akBottom];
  fList.OnClick := ListPick;
end;


destructor TLDropBox.Destroy;
begin
  fList.Free;
  inherited;
end;


procedure TLDropBox.SetItemIndex(aIndex: Integer);
begin
  if InRange(aIndex, 0, fItemCount-1) then
    fItemIndex := aIndex
  else
    fItemIndex := -1;
  UpdateBoxText;
end;


{procedure TLDropBox.FindByTag(const aTag: string);
var i: Integer;
begin
  fItemIndex := -1;
  for i:=0 to fItemCount-1 do
    if fTags[i]=aTag then begin
      fItemIndex := i;
      UpdateBoxText;
    end;
end;}


procedure TLDropBox.PanelShow(Sender: TObject);
var I: Integer;
begin
  if fPanel.Visible then
  begin
    PanelHide(nil, mbLeft, [], 0, 0);
    Exit;
  end;

  if (fItemCount = 0) or ReadOnly then Exit;

  fList.Clear;
  for I := 0 to fItemCount - 1 do
    fList.AddItem(fItems[I]);

  fList.Font := Font; //Copy font properties
  fList.ItemIndex := fItemIndex;
  fList.fItemHighlight := -1;
  fList.TopIndex := fItemIndex - fDropCount div 2; //Desired index, will be tweaked by TLListBox on render

  Master.CtrlFocus := fList;

  fPanel.AbsLeft := AbsLeft;
  fPanel.AbsTop  := AbsTop + AbsHeight;
  fPanel.LocWidth  := LocWidth;
  fPanel.LocHeight := Min(fDropCount, fList.ItemCount) * fItemHeight;

  inherited; //If we got here - show the underlying Panel and Shape
end;


procedure TLDropBox.ListPick(Sender: TObject);
begin
  if fList.ItemIndex = -1 then exit;
  fItemIndex := fList.ItemIndex;
  UpdateBoxText;
  NotifyChange;
  PanelHide(nil, mbLeft, [], 0, 0);
end;


function TLDropBox.GetBoxText: string;
begin
  if (fItemIndex <> -1) and (fBoxText.Text <> '') then
    Result := fBoxText[0]
  else
    Result := '';
end;


function TLDropBox.GetItemTag: Integer;
begin
  Assert(ItemIndex <> -1);
  Result := fTags[ItemIndex];
end;


function TLDropBox.IndexOfTag(aTag: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to fItemCount - 1 do
  if fTags[I] = aTag then
  begin
    Result := I;
    Break;
  end;
end;


procedure TLDropBox.UpdateBoxText;
begin
  if fItemIndex <> -1 then
    fBoxText.Text := fItems.Strings[fItemIndex]
  else
    fBoxText.Clear;
  inherited; //Reformats the text
end;


procedure TLDropBox.Clear;
begin
  fItemIndex := -1;
  fItems.Clear;
  fItemCount := 0;
  UpdateBoxText;
end;


procedure TLDropBox.AddEntry(aItem: string; aTag: Integer = 0);
begin
  fItems.Add(aItem);
  SetLength(fTags, fItemCount + 1);
  fTags[fItemCount] := aTag;
  Inc(fItemCount);
end;


{ TLListBox }
constructor TLListBox.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited;
  fLinesPerRow      := 1;
  fItemHeight       := 20;
  fItemIndex        := -1;
  fItemHighlight    := -1;
  fHeaderHeight     := 0;
  fShowLineLines    := false;

  fItems := TStringList.Create;

  fFont.Color := UI_COLOR_FONT;
  fFont.Size := DEF_FONT_SIZE;

  fScrollBar := TLScrollBar.Create(aParent, 0, 0, aHeight, sa_Vertical);
  fScrollBar.MinValue := 0;
  fScrollBar.Position := 0;

  UpdateSize;
end;


destructor TLListBox.Destroy;
begin
  fItems.Free;
  fScrollBar.Free;
  inherited;
end;


procedure TLListBox.DoUpdate;
begin
  inherited;
  //Allows for batch updating within BeginUpdate/EndUpdate block
  if not fUpdating then
    UpdateSize;
end;


procedure TLListBox.SetFont(const Value: TFontProps);
begin
  fFont := Value;
  DoUpdate;
end;


procedure TLListBox.SetItemIndex(aValue: Integer);
begin
  if InRange(aValue, 0, fItems.Count - 1) then
    fItemIndex := aValue
  else
    fItemIndex := -1;

  TopIndex := fItemIndex - fVisibleItems div 2 + 1;
end;


function TLListBox.GetTopIndex: Integer;
begin
  Result := fScrollBar.Position;
end;


procedure TLListBox.SetTopIndex(aValue: Integer);
begin
  fScrollBar.Position := aValue;
end;


procedure TLListBox.SetLocHeight(aValue: Integer);
begin
  inherited;
  DoUpdate;
end;


procedure TLListBox.SetLocWidth(aValue: Integer);
begin
  inherited;
  fScrollBar.fLeft := fLeft + fWidth - fScrollBar.LocWidth;
  DoUpdate;
end;


procedure TLListBox.SetVisible(aValue: Boolean);
begin
  inherited;
  fScrollBar.Visible := fVisible;
end;


procedure TLListBox.AddItem(const aText: string);
begin
  fItems.Add(aText);
  DoUpdate;
end;


procedure TLListBox.Clear;
begin
  fItemIndex      := -1;
  fItemHighlight  := -1;
  fItems.Clear;
  DoUpdate;
end;


function TLListBox.ItemCount: Integer;
begin
  Result := fItems.Count;
end;


procedure TLListBox.KeyDown(Key: Word; Shift: TShiftState);
var NewIndex: Integer;
begin
  inherited;

  NewIndex := fItemIndex;
  if fItemIndex <> -1 then
  case Key of
    VK_UP:    NewIndex := Max(fItemIndex - 1, 0);
    VK_DOWN:  NewIndex := Min(fItemIndex + 1, ItemCount - 1);
    else Exit;
  end;

  if NewIndex <> fItemIndex then
  begin
    if TopIndex > fItemIndex then
      TopIndex := fItemIndex;
    if TopIndex < fItemIndex - fVisibleItems+2 then
      TopIndex := fItemIndex - fVisibleItems+2;
    fItemHighlight := ItemIndex;

    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;


//Exclude scrollbar and gap before it
function TLListBox.HitTest(X,Y: Integer): Boolean;
begin
  Result := InRange(X, AbsLeft, AbsLeft+fAreaWidth*Scale) and InRange(Y, AbsTop, AbsTop+AbsHeight);
end;


procedure TLListBox.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  if (X >= AbsLeft + fAreaWidth*Scale) or (Y - AbsTop - fHeaderHeight*Scale < 0) then
  begin
    fItemHighlight := -1;
    exit;
  end;

  fItemHighlight := TopIndex + Round(Y - AbsTop - fHeaderHeight*Scale) div Round(fItemHeight*Scale);
  if not InRange(fItemHighlight - TopIndex, 0, min(fItems.Count-1, fHeight div fItemHeight - 1)) then
    fItemHighlight := -1;
  inherited;
end;


procedure TLListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Enabled and (csDown in fControlState) and (Button = mbLeft) then
    if InRange(fItemHighlight, 0, fItems.Count - 1) then //Valid index
      if fItemHighlight <> fItemIndex then
      begin
        fItemIndex := fItemHighlight;
        if Assigned(OnChange) then
          OnChange(Self);
      end
      else
    else
      fItemIndex := -1;
  inherited;
end;


procedure TLListBox.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean);
begin
  inherited;
  fScrollBar.MouseWheel(Shift, WheelDelta, X,Y, Handled);
end;


procedure TLListBox.UpdateSize;
begin
  fItemHeight := Round((fLinesPerRow + LINE_HEIGHT_MUL - 1) * fFontLib.GetStringHeight(Font));

  fScrollBar.Visible := (fItems.Count * fItemHeight + fHeaderHeight) > fHeight;
  fScrollBar.LocHeight := LocHeight;

  fAreaWidth := fWidth;
  fVisibleItems := (fHeight - fHeaderHeight) div fItemHeight + 1; //Last item will be clipped

  if fScrollBar.fVisible then
  begin
    fAreaWidth := fWidth - fScrollBar.fWidth;

    fScrollBar.fScale := fScale;
    fScrollBar.fLeft := fLeft + fWidth - fScrollBar.LocWidth;
    fScrollBar.fTop := fTop;

    fScrollBar.MinValue := 0;
    fScrollBar.MaxValue := Max(fItems.Count - fVisibleItems + 1, 0); //Show last clipped empty line
    fScrollBar.PageSize := fVisibleItems;
    fScrollBar.Enabled := fScrollBar.MaxValue <> fScrollBar.MinValue;
  end
  else
  begin
    fScrollBar.MinValue := 0;
    fScrollBar.MaxValue := 0;
    fScrollBar.Enabled := False;
  end;
end;


procedure TLListBox.Render;
var I: Integer; cp: array[0..3]of real; //ClipPlane X+Y+Z=-D
begin
  inherited;
  //UpdateSize;

  if fWidth <= 0 then Exit;

  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    cp[0] := 0; //X
    cp[1] := -1; //Y
    cp[2] := 0; //Z
    cp[3] := fHeight; //D
    glEnable(GL_CLIP_PLANE0);
    glClipPlane(GL_CLIP_PLANE0, @cp);

    //RenderTexQuad(TexBackground, 0, 0, fAreaWidth, fHeight, DEF_TEX_INS, $FFFFFFFF);

    //Over
    if csOver in fControlState then
    if InRange(fItemHighlight-TopIndex, 0, fVisibleItems-1) then
      RenderQuad(GL_QUADS, 0, (fItemHighlight-TopIndex)*fItemHeight, fAreaWidth, fItemHeight, UI_COLOR_OVER);

    //Selected
    if InRange(fItemIndex-TopIndex, 0, fVisibleItems-1) then
      RenderQuad(GL_QUADS, 0, (fItemIndex-TopIndex)*fItemHeight, fAreaWidth, fItemHeight, UI_COLOR_SELECTED);

    for I := 0 to Min(fItems.Count-TopIndex, fVisibleItems) - 1 do
      fFontLib.Render(DEF_TEXT_MARGIN, I*fItemHeight, fAreaWidth-DEF_TEXT_MARGIN*2, fItemHeight, Scale, fItems.Strings[TopIndex+I], Font);

    glDisable(GL_CLIP_PLANE0);

  glPopMatrix;
end;


{ TLColumnHead }
constructor TLColumnHead.Create;
begin
  inherited;
  Visible := true;
end;


procedure TLColumnHead.AutoSize(ItemHeight: Integer; const Font: TFontProps);
begin
  if fVerticalCaption then
    fWidth := ceil(fFontLib.GetStringHeight(Font, fCaption))
  else
    fWidth := ceil(fFontLib.GetStringWidth(fCaption, Font)+DEF_TEXT_MARGIN*2);

  if fHasCheckbox then
    fWidth := max(fWidth, ItemHeight);
end;


procedure TLColumnHead.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  {if (Button = mbLeft) then
  begin
    fCellDown.X := HitTestColumn((X-AbsLeft)/Scale);
    fCellDown.Y := fItemHighlight; //Remember the item to place a Click on it

    if (fCellDown.X <> -1) and HitTestHeader(Y) then
      fColumns[fCellDown.X].MouseDown(Button, Shift, X, Y);
  end;}

  inherited;
end;


procedure TLColumnHead.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  {if Enabled and not ReadOnly and HitTestHeader(Y) then
    fColumnHighlight := HitTestColumn((X-AbsLeft)/Scale)
  else
    fColumnHighlight := -1;

  if fColumnHighlight <> -1 then
    fColumns[fColumnHighlight].MouseMove(Shift, X, Y);

  if (fColumnCount = 0) then
    fItemHighlight := -1;}
end;


procedure TLColumnHead.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  fChecked := not fChecked;
  inherited;
end;


procedure TLColumnHead.Render(X,Height,CheckBoxHeight: Integer; WidthMultiplier,Scale: Single; const Font: TFontProps);
var OffsetX,AbsWidth: Integer; Tex: TLTexture;
begin
  if not fVisible then exit;

  OffsetX := CheckBoxHeight*byte(fHasCheckbox);
  AbsWidth := Round(Width*WidthMultiplier);

  if fChecked then
    Tex := TexChkChecked.Norm
  else
    Tex := TexChkUnchecked.Norm;

  if fVerticalCaption then
  begin
    fFontLib.RenderV(X+DEF_TEXT_MARGIN, DEF_TEXT_MARGIN, AbsWidth-DEF_TEXT_MARGIN*2, Height-OffsetX-DEF_TEXT_MARGIN*2, Scale, fCaption, Font);
    if fHasCheckbox then
      RenderTex(Tex, is3TapH, X+(AbsWidth-CheckBoxHeight) div 2, Height-CheckBoxHeight, CheckBoxHeight, CheckBoxHeight, $FFFFFFFF);
  end else
  begin
    fFontLib.Render(X+DEF_TEXT_MARGIN+OffsetX, DEF_TEXT_MARGIN, AbsWidth-OffsetX-DEF_TEXT_MARGIN*2, Height-DEF_TEXT_MARGIN*2, Scale, fCaption, Font);
    if fHasCheckbox then
      RenderTex(Tex, is3TapH, X+2, (Height-CheckBoxHeight) div 2, CheckBoxHeight, CheckBoxHeight, $FFFFFFFF);
  end;
end;


{ TLColumns }
constructor TLColumns.Create;
begin
  inherited;
  fColumnRows := 1;
  fStretchToFit := false;
  fStretchFactor := 1;

  fFont.Color := $FF606060;
  fFont.Style := fsArialBold;
end;


destructor TLColumns.Destroy;
var i: Integer;
begin
  for i:=0 to fCount-1 do fColumns[i].Free;
  inherited;
end;


procedure TLColumns.SetCount(aNewCount: Integer);
var i: Integer;
begin
  if aNewCount > fCount then begin
    SetLength(fColumns, aNewCount);
    for i:=fCount to aNewCount-1 do
      fColumns[i] := TLColumnHead.Create;
  end else
  if aNewCount < fCount then begin
    for i:=aNewCount to fCount-1 do
      fColumns[i].Free;
    SetLength(fColumns, aNewCount);
  end;

  fCount := aNewCount;
end;


procedure TLColumns.SetFontSize(Value: Byte);
begin
  fFont.Size := Value;
  fItemHeight := Round(fFontLib.GetStringHeight(fFont) * LINE_HEIGHT_MUL);
end;


procedure TLColumns.SetVerticalCaptions(aValue: Boolean);
var i: Integer;
begin
  for i:=0 to fCount-1 do
    fColumns[i].VerticalCaption := aValue;
end;


function TLColumns.GetColumn(Index: Integer): TLColumnHead;
begin
  Assert(InRange(Index, 0, fCount-1));
  Result := fColumns[Index];
end;


function TLColumns.GetUsedWidth: Single;
begin
  Result := fDesiredWidth * fStretchFactor;
end;


function TLColumns.GetHeight: Integer;
begin
  Result := fItemHeight * fColumnRows;
end;


function TLColumns.HitTestColumn(X: Single): Integer;
var i: Integer;
begin
  Result := -1;
  for i:=0 to fCount-1 do
  begin
    if fColumns[i].Visible then
      X := X - fColumns[i].Width*fStretchFactor;
    if X < 0 then
    begin
      Result := i;
      exit;
    end;
  end;
end;


procedure TLColumns.SubRender(Scale: Single);
var
  i: Integer;
  ColWidth: Single;
begin
  if ColumnRows = 0 then exit;

  RenderQuad(GL_QUADS, 0, 0, UsedWidth, fItemHeight * fColumnRows, $10000000);
  ColWidth := 0;
  for i:=0 to fCount-1 do
    if fColumns[i].Visible then begin
//      if (csOver in fControlState) and (fColumnHighlight = i) then
//        RenderQuad(GL_QUADS, ColWidth, 0, fColumns[i].Width*Columns.StretchFactor, fHeaderHeight, $10000000);
      fColumns[i].Render(Round(ColWidth), fItemHeight * fColumnRows, fItemHeight, fStretchFactor, Scale, fFont);
      ColWidth := ColWidth + fColumns[i].Width * fStretchFactor;
    end;
  RenderLine(0, fItemHeight * fColumnRows, ColWidth, 0, 1*Scale, $40000000);
end;


procedure TLColumns.AutoSize;
var i: Integer;
begin
  fDesiredWidth := 0;
  for i:=0 to fCount-1 do
    if fColumns[i].Visible then
      inc(fDesiredWidth, fColumns[i].Width);

  if fStretchToFit then
    fStretchFactor := Width / fDesiredWidth
  else
    fStretchFactor := 1;
end;


{ TLColumnListBox }
constructor TLColumnListBox.Create(aParent: TLPanel; aLeft,aTop,aWidth,aHeight: Integer);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,aHeight);

  fShowLineLines    := true;
  fColumns := TLColumns.Create;
  fColumnCount      := 0;
  fColumnHighlight := -1;
end;


destructor TLColumnListBox.Destroy;
var i: Integer;
begin
  for i:=0 to fColumnCount-1 do fMoreItems[i].Free;
  fColumns.Free;
  inherited;
end;


//fMoreItems[0] is empty and always substituted with fItems from parent class TListBox
function TLColumnListBox.GetItems(Index: Integer): TStringList;
begin
  if Index = 0 then
    Result := fItems
  else
  if InRange(Index, 1, fColumnCount-1) then
    Result := fMoreItems[Index]
  else
    Result := nil;
end;


//See if there's header below
function TLColumnListBox.HitTestHeader(Y: Integer): Boolean;
begin
  Result := InRange((Y-AbsTop)/Scale, 0, fHeaderHeight);
end;


procedure TLColumnListBox.SetColumnCount(aNewCount: Integer);
var i: Integer;
begin
  if aNewCount > fColumnCount then begin
    SetLength(fMoreItems, aNewCount);
    for i:=fColumnCount to aNewCount-1 do
      fMoreItems[i] := TStringList.Create;
  end else
  if aNewCount < fColumnCount then begin
    for i:=aNewCount to fColumnCount-1 do
      fMoreItems[i].Free;
    SetLength(fMoreItems, aNewCount);
  end;

  fColumns.Count := aNewCount;
  fColumnCount := aNewCount;
end;


procedure TLColumnListBox.Clear;
var i: Integer;
begin
  fItemIndex      := -1;
  fItemHighlight  := -1;
  for i:=0 to fColumnCount-1 do
    GetItems(i).Clear;
end;


procedure TLColumnListBox.AddItem(const aText:array of string);
var i: Integer;
begin
  for i:=0 to min(high(aText), fColumnCount-1) do
    GetItems(i).Add(aText[i]);
end;


//Resizes columns depending on header and table contents(!)
procedure TLColumnListBox.AutoSizeColumns;
var i,k: Integer; Fnt: TFontProps;
begin
  Fnt := Font;
  Fnt.Color := $FF606060;
  Fnt.Style := fsArialBold;

  //Go through all columns and choose max from (caption, checkbox, contents)
  for i:=0 to fColumnCount-1 do
  begin
    fColumns[i].AutoSize(fItemHeight, Fnt);
    case fColumns[i].fKind of
      ckCheck:  fColumns[i].Width := max(fColumns[i].Width, fItemHeight);
      ckText:   for k:=0 to Items[i].Count-1 do //text margins are larger in tables, for better readability
                  fColumns[i].Width := max(fColumns[i].Width, ceil(fFontLib.GetStringWidth(Items[i][k], Font)+DEF_TEXT_MARGIN*4));
    end;
  end;
  fColumns.AutoSize;
end;


procedure TLColumnListBox.ReformatColumnsText;
var i: Integer;
begin
  UpdateSize;
  for i:=0 to fColumnCount-1 do
    if Columns[i].VerticalCaption then
      Columns[i].Caption := fFontLib.RepositionEOLs(Columns[i].Caption, Font, fHeaderHeight - byte(Columns[i].HasCheckbox)*fItemHeight - DEF_TEXT_MARGIN*6, 0)
    else
      Columns[i].Caption := fFontLib.RepositionEOLs(Columns[i].Caption, Font, Columns[i].Width, 0);
end;


function TLColumnListBox.DesiredWidth: Integer;
begin
  Result := byte(fScrollBar.Visible) * (fItemHeight + DEF_SCROLL_PAD) + Columns.fDesiredWidth;
end;


procedure TLColumnListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  if Enabled and (Button = mbLeft) then
  begin
    fCellDown.X := Columns.HitTestColumn((X-AbsLeft)/Scale);
    fCellDown.Y := fItemHighlight; //Remember the item to place a Click on it

    if (fCellDown.X <> -1) and HitTestHeader(Y) then
      fColumns[fCellDown.X].MouseDown(Button, Shift, X, Y);
  end;

  inherited;
end;


procedure TLColumnListBox.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  inherited;
  if Enabled and HitTestHeader(Y) then
    fColumnHighlight := Columns.HitTestColumn((X-AbsLeft)/Scale)
  else
    fColumnHighlight := -1;

  if fColumnHighlight <> -1 then
    fColumns[fColumnHighlight].MouseMove(Shift, X, Y);

  if (fColumnCount = 0) then
    fItemHighlight := -1;
end;


procedure TLColumnListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
  procedure ToggleCell(Cell: TPoint);
  begin
    if Items[Cell.X].Strings[Cell.Y] = booltostr(false) then
      Items[Cell.X].Strings[Cell.Y] := booltostr(true)
    else
      Items[Cell.X].Strings[Cell.Y] := booltostr(false);
  end;
var NewCol: Integer;
begin
  NewCol := Columns.HitTestColumn((X-AbsLeft)/Scale);

  if Enabled and (Button = mbLeft) and
    (NewCol <> -1) and HitTestHeader(Y) then
    fColumns[NewCol].MouseUp(Button, Shift, X, Y);

  //Check that the cell is the same as on MouseDown
  if Enabled and not ReadOnly and (Button = mbLeft) and
    (fItemHighlight <> -1) and (fItemHighlight = fCellDown.Y) and
    (NewCol <> -1) and (NewCol = fCellDown.X) and
    (fColumns[NewCol].Kind = ckCheck) then
      ToggleCell(fCellDown);

  inherited;
end;


procedure TLColumnListBox.UpdateSize;
begin
  inherited;

  Columns.FontSize := Font.Size;

  fHeaderHeight := Columns.Height;
  if fScrollBar.fVisible then dec(fAreaWidth, DEF_SCROLL_PAD);

  Columns.Width := fAreaWidth;
  Columns.AutoSize;
end;


procedure TLColumnListBox.Render;
var i,k: Integer; s: string;
cp:array[0..3]of real; //ClipPlane X+Y+Z=-D
begin
  inherited;
  UpdateSize;

  if fWidth <= 0 then Exit;

  glPushMatrix;
    glTranslatef(AbsLeft,AbsTop,0);
    glScale2(Scale);

    //RenderTex(TexBackground, is9Tap, 0, 0, fAreaWidth, fHeight, $FFFFFFFF);

    cp[0] := -1; //X
    cp[1] := 0; //Y
    cp[2] := 0; //Z
    cp[3] := fAreaWidth; //D
    glEnable(GL_CLIP_PLANE0);
    glClipPlane(GL_CLIP_PLANE0, @cp);
    cp[0] := 0; //X
    cp[1] := -1; //Y
    cp[2] := 0; //Z
    cp[3] := fHeight; //D
    glEnable(GL_CLIP_PLANE1);
    glClipPlane(GL_CLIP_PLANE1, @cp);

    if csOver in fControlState then
    if InRange(fItemHighlight-TopIndex, 0, fVisibleItems-1) then
      RenderQuad(GL_QUADS, 2, (fItemHighlight-TopIndex)*fItemHeight+fHeaderHeight+2, Columns.UsedWidth-4, fItemHeight-4, $20000000);

    if InRange(fItemIndex-TopIndex, 0, fVisibleItems-1) then
      RenderQuad(GL_QUADS, 2, (fItemIndex-TopIndex)*fItemHeight+fHeaderHeight+2, Columns.UsedWidth-4, fItemHeight-4, $40000000);

    //Render column titles
    Columns.SubRender(Scale);

    //Lines
    if fShowLineLines and (ItemCount > 0) then
    for i:=1 to min(ItemCount, fVisibleItems) do
      RenderLine(0, i*fItemHeight + fHeaderHeight, Columns.UsedWidth, 0, 1*Scale, $40000000);

    //Values
    for k:=0 to fColumnCount-1 do
    if fColumns[k].Visible then
    begin
      case fColumns[k].Kind of
        ckCheck:
          for i:=0 to min(Items[k].Count-TopIndex, fVisibleItems)-1 do
          begin
            s := Items[k].Strings[TopIndex+i];
            if s = booltostr(false) then
              RenderTex(TexChkUnchecked.Norm, isStretch, Round(fColumns[k].Width*Columns.StretchFactor - fItemHeight) div 2, i*fItemHeight + fHeaderHeight, fItemHeight, fItemHeight, $FFFFFFFF)
            else if s = booltostr(true) then
              RenderTex(TexChkChecked.Down, isStretch, Round(fColumns[k].Width*Columns.StretchFactor - fItemHeight) div 2, i*fItemHeight + fHeaderHeight, fItemHeight, fItemHeight, $FFFFFFFF)
            else //Some unexpected value
              //RenderTex(TexNull, isStretch, Round(fColumns[k].Width*Columns.StretchFactor - fItemHeight) div 2, i*fItemHeight + fHeaderHeight, fItemHeight, fItemHeight, $FFFFFFFF);
          end;
        ckText:
          for i:=0 to min(Items[k].Count-TopIndex, fVisibleItems)-1 do
            fFontLib.Render(DEF_TEXT_MARGIN, i*fItemHeight + fHeaderHeight, Round(fColumns[k].Width*Columns.StretchFactor)-DEF_TEXT_MARGIN*2, fItemHeight, Scale, Items[k].Strings[TopIndex+i], Font);
      end;
      glTranslatef(fColumns[k].Width*Columns.StretchFactor,0,0);
      RenderLine(0, 0, 0, min(fVisibleItems,ItemCount)*fItemHeight + fHeaderHeight, 1*Scale, $40000000);
    end;

    glDisable(GL_CLIP_PLANE0);
    glDisable(GL_CLIP_PLANE1);

  glPopMatrix;
end;


{ TLAnimator }
constructor TLAnimator.Create;
begin
  inherited;
  fCtrlCount := 0;
  SetLength(fCtrls, fCtrlCount+1);
end;


procedure TLAnimator.Add(aCtrl: TLControl; XY1,XY2,SZ1,SZ2: TPoint; S1,S2: Single; aLength:word; aRule: TAnimRule=arLinearCos; aOnAnimEnd: TNotifyEvent=nil; aSender: TObject=nil);
begin
  inc(fCtrlCount);
  SetLength(fCtrls, fCtrlCount + 1);

  with fCtrls[fCtrlCount] do begin
    Ctrl := aCtrl;

    AnimMove1 := XY1;
    AnimSize1 := SZ1;
    AnimScale1 := S1;

    AnimMove2 := XY2;
    AnimSize2 := SZ2;
    AnimScale2 := S2;

    FirstTick := TimeGetTime;
    AnimTime := aLength;
    AnimRule := aRule;

    fDone := false;
    fOnAnimEnd := aOnAnimEnd;
    fAnimSender := aSender;

    //Set right away cos Render may occur before next UpdateState
    Ctrl.LocLeft   := AnimMove1.X;
    Ctrl.LocTop    := AnimMove1.Y;
    Ctrl.LocWidth  := AnimSize1.X;
    Ctrl.LocHeight := AnimSize1.Y;
    Ctrl.fScale    := AnimScale1;
  end;
end;


//Add by destination data
procedure TLAnimator.AddDest(aCtrl: TLControl; X2,Y2: Integer; S2: Single; aLength:word; aRule: TAnimRule=arLinearCos; aOnAnimEnd: TNotifyEvent=nil; aSender: TObject=nil);
begin
  Add(aCtrl, Point(aCtrl.fLeft, aCtrl.fTop),
             Point(X2, Y2),
             Point(aCtrl.fWidth, aCtrl.fHeight),
             Point(aCtrl.fWidth, aCtrl.fHeight),
             aCtrl.fScale, S2, aLength, aRule, aOnAnimEnd, aSender);
end;


procedure TLAnimator.AddDest(aCtrl: TLControl;
  TargetX, TargetY, TargetW, TargetH: Integer; aLength: Word;
  aRule: TAnimRule = arLinearCos; aOnAnimEnd: TNotifyEvent = nil; aSender: TObject = nil);
begin
  Add(aCtrl, Point(aCtrl.fLeft, aCtrl.fTop),
             Point(TargetX, TargetY),
             Point(aCtrl.fWidth, aCtrl.fHeight),
             Point(TargetW, TargetH),
             aCtrl.fScale, aCtrl.fScale, aLength, aRule, aOnAnimEnd, aSender);
end;


//Remove control from the list (i.e. when control gets destroyed but still exists in animation list)
procedure TLAnimator.Remove(aCtrl: TLControl);
var I: Integer;
begin
  //It's likely to be near the end of list, cos it was added recently
  for I := fCtrlCount downto 1 do
    if (fCtrls[I].Ctrl = aCtrl) and not fCtrls[I].fDone then
    begin
      fCtrls[I].fDone := True; //just disable updating
      //Don't exit just yet, we might have a few instances added by mistake - disable them all
    end;
end;


//Returns true if we are in a middle of animation
function TLAnimator.AnimWIP(aCtrl: TLControl): Boolean;
var I: Integer;
begin
  Result := false;
  for I := fCtrlCount downto 1 do
    if (fCtrls[I].Ctrl = aCtrl) and (not fCtrls[I].fDone) then
    begin
      Result := True;
      Exit;
    end;
end;


procedure TLAnimator.UpdateState;
var
  I: Integer;
  MixMove, MixScale: Single;
begin
  for I := 1 to fCtrlCount do
  with fCtrls[I] do
  begin
    if fDone then Continue; //Skip till we have a working AnimatorCollection with Add/Rem

    MixMove := min(TimeGetTime - FirstTick, AnimTime) / AnimTime; //Get 0..1 range
    MixScale := MixMove;

    case AnimRule of
      arLinear:     ;
      arLinearCos:  begin MixMove := (1-(cos(sqrt(MixMove)*pi)+1)/2); MixScale := MixMove; end;
      arCurve:      begin MixMove := sin(MixMove*pi/2); end;
    end;

    Ctrl.LocLeft    := Round(AnimMove1.X + (AnimMove2.X - AnimMove1.X) * MixMove);
    Ctrl.LocTop     := Round(AnimMove1.Y + (AnimMove2.Y - AnimMove1.Y) * MixMove);
    Ctrl.LocWidth   := Round(AnimSize1.X + (AnimSize2.X - AnimSize1.X) * MixMove);
    Ctrl.LocHeight  := Round(AnimSize1.Y + (AnimSize2.Y - AnimSize1.Y) * MixMove);
    Ctrl.Scale      := AnimScale1 + (AnimScale2-AnimScale1) * MixScale;

    fDone := (MixMove >= 1);
    if fDone and Assigned(fOnAnimEnd) then
    begin
      fOnAnimEnd(fAnimSender);
      fOnAnimEnd := nil; //Only call once
    end;
  end;
end;


{ TLTrackBar }
constructor TLTrackBar.Create(aParent: TLPanel; aLeft, aTop, aWidth: integer; aCaption: string=''; aFontSize: Integer=DEF_FONT_SIZE);
begin
  inherited Create(aParent, aLeft,aTop,aWidth,0);
  fMinValue := 0;
  fMaxValue := 100;
  fPosition := 0;
  fCaption := aCaption;
  Font := MakeFont(UI_COLOR_FONT, aFontSize, fsArialNormal, taLeftJustify);
  fFontHeight := Round(fFontLib.GetStringHeight(Font));
  fThumbHeight := fFontHeight;
  fThumbWidth := Round(fFontLib.GetStringWidth('88888', Font));
  fHeight := fFontHeight*2;
end;


procedure TLTrackBar.SetPosition(aValue: Integer);
begin
  fPosition := EnsureRange(aValue, fMinValue, fMaxValue);
  RenderPos := Round((fPosition-fMinValue)/(fMaxValue-fMinValue)*(fWidth-fThumbWidth)) + fThumbWidth div 2;
end;


function TLTrackBar.HitTest(X, Y: integer): boolean;
begin
  Result := InRange(X,AbsLeft,AbsLeft+AbsWidth) and InRange(Y,AbsTop+fFontHeight,AbsTop+AbsHeight);
end;


procedure TLTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  fOffsetX := Round(MinValue + ((X - AbsLeft - fThumbWidth div 2) / (AbsWidth - fThumbWidth)) * (MaxValue - MinValue)) - fPosition;
  if Abs(fOffsetX) > fThumbWidth / 2 then
    fOffsetX := 0;

  MouseMove(Shift,X,Y); //Will change Position and call OnChange event
end;


procedure TLTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var NewPos: integer;
begin
  inherited;

  NewPos := Position;
  if (ssLeft in Shift) then begin

    if InRange(X-AbsLeft, 0, AbsWidth) then
      NewPos := Round(MinValue+((X-AbsLeft-fThumbWidth div 2)/(AbsWidth-fThumbWidth))*(MaxValue-MinValue) - fOffsetX);

    if NewPos <> Position then begin
      SetPosition(NewPos);
      if Assigned(fOnChange) then
        fOnChange(Self);
    end;
  end;
end;


procedure TLTrackBar.MouseWheel(Shift: TShiftState; WheelDelta, X, Y: Integer; var Handled: Boolean);
begin
  inherited;

  //Not supported yet
end;


procedure TLTrackBar.Render;
var Col: TColor4c;
begin
  inherited;
  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    Font.Justify := taLeftJustify;
    fFontLib.Render(0, 0, fWidth, fFontHeight, Scale, fCaption, Font);

    RenderLine(0, (fFontHeight*3) div 2, fWidth, 0, 2, UI_COLOR_LINES);

    RenderQuad(GL_POLYGON, RenderPos - fThumbWidth div 2, Round(fFontHeight*1.5 - fThumbHeight/2), fThumbWidth, fThumbHeight, UI_COLOR_FORM_FACE);

    if csDown in fControlState then
      Col := UI_COLOR_SELECTED
    else
    if csOver in fControlState then
      Col := UI_COLOR_OVER
    else
      Col := $00000000;

    RenderQuad(GL_POLYGON, RenderPos - fThumbWidth div 2, Round(fFontHeight*1.5 - fThumbHeight/2), fThumbWidth, fThumbHeight, Col);
    RenderQuad(GL_LINE_LOOP, RenderPos - fThumbWidth div 2 + 0.5, Round(fFontHeight*1.5 - fThumbHeight/2) + 0.5, fThumbWidth - 1, fThumbHeight - 1, UI_COLOR_FONT);

    Font.Justify := taCenter;
    fFontLib.Render(RenderPos - fThumbWidth div 2, fFontHeight, fThumbWidth, fFontHeight, Scale, IntToStr(fPosition), Font);

  glPopMatrix;
end;


{ TLContainer }
constructor TLContainer.Create(aParent: TLPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited;

  fVScroll := TLScrollBar.Create(aParent, aWidth-20, aTop, aHeight, sa_Vertical);
  //fVScroll.Anchors := [akRight, akTop, akBottom];
  fVScroll.OnChange := ScrollChange;
  UpdateScroll;
end;


function TLContainer.GetScrollPosition: Integer;
begin
  Result := fVScroll.Position;
end;


procedure TLContainer.SetScrollPosition(const Value: Integer);
begin
  fVScroll.Position := Value;
end;


procedure TLContainer.SetLocHeight(aValue: Integer);
begin
  inherited;
  fVScroll.LocHeight := aValue;
  UpdateScroll;
end;


procedure TLContainer.SetVisible(aValue: Boolean);
begin
  inherited;

  //Check if we need to show scrollbar on left
  UpdateScroll;
end;


procedure TLContainer.SetLocWidth(aValue: Integer);
begin
  inherited;
  fVScroll.LocLeft := fLeft + fWidth - fVScroll.LocWidth;
end;


procedure TLContainer.AddChild(aControl: TLControl);
begin
  inherited;
  UpdateScroll;
end;


function TLContainer.HitControl(X, Y: Integer): TLControl;
begin
  if HitTest(X,Y) then
    Result := inherited HitControl(X, Y)
  else
    Result := nil;
end;


procedure TLContainer.ScrollChange(Sender: TObject);
var I: Integer;
begin
  for I := 1 to fChildCount do
    Childs[I].LocTop := Childs[I].LocTop - (fVScroll.Position - fTopOffset);

  fTopOffset := fVScroll.Position;
end;


procedure TLContainer.UpdateScroll;
var
  I, OldPosition: Integer;
begin
  OldPosition := fVScroll.Position;
  fVScroll.MaxValue := 0;

  for I := 1 to fChildCount do
  if Childs[I].Visible then
    fVScroll.MaxValue := Max(Childs[I].LocTop + fTopOffset + Childs[I].LocHeight - LocHeight, fVScroll.MaxValue);

  fVScroll.Visible := fVScroll.MaxValue > fVScroll.MinValue;
  fVScroll.Enabled := fVScroll.Visible;
  fVScroll.PageSize := fHeight;
  fVScroll.Position := OldPosition;
  ScrollChange(fVScroll);

  //Now when we know if scroll is visible we can update childs widths
  for I := 1 to fChildCount do
  if Childs[I].Visible then
  if [akLeft, akRight] <= Childs[I].Anchors then
    Childs[I].LocWidth := LocWidth - fVScroll.LocWidth * Byte(fVScroll.Visible);
end;


//Same as TLPanel but with regard to fTopOffset
procedure TLContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  //If we are the top-most Panel of all
  if (Self.Parent = nil) then begin
    CtrlDown := HitControl(X,Y - fTopOffset);
    if (CtrlDown <> nil) then CtrlDown.MouseDown(Button, Shift, X, Y - fTopOffset);
  end;
end;


//Same as TLPanel but with regard to fTopOffset
procedure TLContainer.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  //If we are the top-most Panel of all
  if (Self.Parent = nil) then begin
    if CtrlDown = nil then
      CtrlOver := HitControl(X,Y - fTopOffset);
    if (CtrlOver <> nil) then CtrlOver.MouseMove(Shift, X, Y - fTopOffset);
  end;
end;


//Same as TLPanel but with regard to fTopOffset
procedure TLContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  //If we are the top-most Panel of all
  if (Self.Parent = nil) then begin
    CtrlUp := HitControl(X,Y - fTopOffset);
    if (CtrlUp <> nil) then CtrlUp.MouseUp(Button, Shift, X, Y - fTopOffset);
    CtrlDown := nil; //Release it after OnClick checked it
  end;
end;


//Same as TLPanel but with regard to fTopOffset
procedure TLContainer.MouseWheel(Shift: TShiftState; WheelDelta: Integer; X,Y: Integer; var Handled: Boolean);
var C: TLControl;
begin
  //If we are the top-most Panel of all
  if (Self.Parent = nil) then
  begin
    C := HitControl(X,Y - fTopOffset);
    if (C <> nil) then
      C.MouseWheel(Shift, WheelDelta, X, Y - fTopOffset, Handled);

    while not Handled and (C.Parent <> nil) do
    begin
      C := C.Parent;
      C.MouseWheel(Shift, WheelDelta, X, Y - fTopOffset, Handled);
    end;
  end
  else
  begin
    inherited;
    fVScroll.MouseWheel(Shift, WheelDelta, X, Y - fTopOffset, Handled);
  end;
end;


//Render with our own clipping code
procedure TLContainer.Render;
var
  I: Integer;
  CP1, CP2, CP3, CP4: TVector4d; //Set of 4 clipping planes X + Y + Z = -D
begin
  //For simplicity we assume that:
  // - there are no nested containers with nested clipping planes
  // - childs will never access clipping planes beyond 0..1
  //Said that we can safely use planes 2..5 (HW supports at least 6 planes)

  //Set up clipping area
  CP1 := Vector4d(1, 0, 0, -AbsLeft);
  glEnable(GL_CLIP_PLANE2);
  glClipPlane(GL_CLIP_PLANE2, @CP1);

  CP2 := Vector4d(-1, 0, 0, AbsLeft + AbsWidth);
  glEnable(GL_CLIP_PLANE3);
  glClipPlane(GL_CLIP_PLANE3, @CP2);

  CP3 := Vector4d(0, 1, 0, -AbsTop);
  glEnable(GL_CLIP_PLANE4);
  glClipPlane(GL_CLIP_PLANE4, @CP3);

  CP4 := Vector4d(0, -1, 0, AbsTop + AbsHeight);
  glEnable(GL_CLIP_PLANE5);
  glClipPlane(GL_CLIP_PLANE5, @CP4);

  //todo: Render only those controls that are partialy visible within the area
  for I := 1 to fChildCount do
    if Childs[I].fVisible then
      Childs[I].Render;

  glDisable(GL_CLIP_PLANE2);
  glDisable(GL_CLIP_PLANE3);
  glDisable(GL_CLIP_PLANE4);
  glDisable(GL_CLIP_PLANE5);
end;


{ TLRadio }
constructor TLRadio.Create(aParent: TLPanel; aLeft, aTop, aWidth, aHeight: Integer);
begin
  inherited;

  fRadioAxis := raHorizontal;
  fFont := MakeFont(UI_COLOR_FONT, 8, fsArialNormal, taLeftJustify)
end;


destructor TLRadio.Destroy;
begin

  inherited;
end;


procedure TLRadio.ItemClick(Sender: TObject);
begin
  fItemIndex := TLButton(Sender).Tag;

  if Assigned(OnChange) then
    OnChange(Self);
end;


procedure TLRadio.SetItems(aImages, aCaptions: array of string);
var I, X, Y, W, H: Integer;
begin
  Assert(Length(aImages) = Length(aCaptions));

  fCount := Length(aImages);
  SetLength(fItems, fCount);

  for I := 0 to fCount - 1 do
  begin
    if fRadioAxis = raHorizontal then
    begin
      X := fLeft + I * (fWidth div fCount);
      Y := fTop + 0;
      W := (fWidth div fCount) - 3;
      H := fHeight;
    end
    else
    begin
      X := fLeft + 0;
      Y := fTop + I * (fHeight div fCount);
      W := fWidth;
      H := (fHeight div fCount) - 3;
    end;

    fItems[I] := TLButton.Create(fParent, X, Y, W, H, aCaptions[I], aImages[I]);
    fItems[I].Tag := I;
    fItems[I].OnClick := ItemClick;
  end;
end;


procedure TLRadio.SetItems(aButtons: array of TButtonTexSet);
var I, X, Y, W, H: Integer;
begin
  fCount := Length(aButtons);
  SetLength(fItems, fCount);

  for I := 0 to fCount - 1 do
  begin
    if fRadioAxis = raHorizontal then
    begin
      X := fLeft + I * (aButtons[0].Norm.X + 4);
      Y := fTop;
      W := aButtons[0].Norm.X;
      H := aButtons[0].Norm.Y;
    end
    else
    begin
      X := fLeft;
      Y := fTop + I * (aButtons[0].Norm.Y + 4);
      W := aButtons[0].Norm.X;
      H := aButtons[0].Norm.Y;
    end;

    fItems[I] := TLButton.Create(fParent, X, Y, W, H, aButtons[I]);
    fItems[I].ButtonFaceStyle := isStretch;
    fItems[I].Tag := I;
    fItems[I].OnClick := ItemClick;
  end;
end;


procedure TLRadio.SetVisible(aValue: Boolean);
var I: Integer;
begin
  inherited;
  for I := 0 to fCount - 1 do
    fItems[I].Visible := aValue;
end;


procedure TLRadio.Render;
var I: Integer;
begin
  inherited;

  for I := 0 to fCount - 1 do
    fItems[I].Down := (I = fItemIndex);

  glPushAttrib(GL_LINE_BIT);
  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    glLineWidth(1);

    {for I := 0 to fCount - 1 do
    begin
      RenderQuad(GL_LINE_LOOP, fItems[I].Offset.X+0.5, fItems[I].Offset.Y+0.5, (fWidth div fCount) - 1, fHeight - 1, $FF000000);
      fFontLib.Render(fItems[I].CapOffset.X, fItems[I].CapOffset.Y, 0, 0, 1, fItems[I].Caption, fFont);
    end;}

  glPopAttrib;
  glPopMatrix;
end;


{ TLToolTip }
constructor TLToolTip.Create(aParent: TLPanel);
begin
  inherited Create(aParent, 0, 0, 0, 0);

  Font := MakeFont($FF000000, 8, fsArialNormal, taCenter);
  FillColor := UI_COLOR_HINT_BG;
end;


procedure TLToolTip.PositionAt(X, Y: Integer; aCaption: string);
var
  W, H: Integer;
begin
  fPositionAt := Point(X, Y);

  fCaption := aCaption;

  W := Round(fFontLib.GetStringWidth(fCaption, Font) / 2);
  H := Round(fFontLib.GetStringHeight(Font, fCaption));

  X := EnsureRange(X, W + 10, Master.LocWidth - W - 10);

  //Make sure Hint does not get clipped by upper edge of the frame
  if Y - H - 10 > 5 then
    Y := Y - H - 15
  else
    Y := Y + 15;

  AbsLeft := X - W - 5;
  AbsTop := Y;

  fWidth := W * 2 + 10;
  fHeight := H + 2;
end;


//Render tooltip text on a plate with a small tip
procedure TLToolTip.Render;
begin
  inherited;

  glPushMatrix;
    glTranslatef(AbsLeft, AbsTop, 0);
    glScale2(Scale);

    RenderShadowQuad(0, 0, fWidth, fHeight, 4, $48000000);
    RenderQuad(GL_QUADS, 0, 0, fWidth, fHeight, FillColor);

    //Render tip
    if AbsTop < fPositionAt.Y then
    begin
      RenderQuad(GL_QUADS, fPositionAt.X - AbsLeft - 3, fHeight, 6, 1, FillColor);
      RenderQuad(GL_QUADS, fPositionAt.X - AbsLeft - 2, fHeight+1, 4, 1, FillColor);
      RenderQuad(GL_QUADS, fPositionAt.X - AbsLeft - 1, fHeight+2, 2, 1, FillColor);
    end
    else
    begin
      RenderQuad(GL_QUADS, fPositionAt.X - AbsLeft - 3, -1, 6, 1, FillColor);
      RenderQuad(GL_QUADS, fPositionAt.X - AbsLeft - 2, -2, 4, 1, FillColor);
      RenderQuad(GL_QUADS, fPositionAt.X - AbsLeft - 1, -3, 2, 1, FillColor);
    end;

    fFontLib.Render(0, 0, fWidth, fHeight, Scale, fCaption, Font, False);
  glPopMatrix;
end;


initialization
  fAnimator := TLAnimator.Create;


finalization
  FreeAndNil(fAnimator);


end.
