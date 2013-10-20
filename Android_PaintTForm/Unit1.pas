unit Unit1;
interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Androidapi.gles2;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    fx, fy: Single;
    procedure DoIdle(Sender: TObject; var Done: Boolean);
  protected
    procedure PaintRects(const UpdateRects: array of TRectF); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DoIdle(Sender: TObject; var Done: Boolean);
begin
  Invalidate;
  Done := False;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := DoIdle;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  fx := x;
  fy := y;
  Invalidate;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
    //Invalidate;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
    //Invalidate;
end;

procedure TForm1.PaintRects(const UpdateRects: array of TRectF);
begin
  inherited;
  glViewport(0, 0, Round(fx), Round(fy));
  glClearColor(Random, Random, Random, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

end.
