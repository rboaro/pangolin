unit Pangolin.GDI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Types, System.UITypes,
  Vcl.StdCtrls, Vcl.ExtCtrls, GDI.Core.Contracts;

type
  TForm1 = class(TForm)
    Button1: TButton;
    tmrAnimation: TTimer;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure tmrAnimationTimer(Sender: TObject);

  private
    FShapes: TArray<IDrawable>;
    FAnimationStep: Single;
    procedure InitializeShapes;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses GDI.Infra.CanvasAdapter, GDI.Domain.Shapes;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Reset animation state
  FAnimationStep := 0.0;
  tmrAnimation.Enabled := True;
end;




procedure TForm1.FormCreate(Sender: TObject);
begin
  // DoubleBuffered is CRITICAL for smooth GDI animations to prevent flickering
  Self.DoubleBuffered := True;
  InitializeShapes;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  LContext: IGraphicsContext;
  LShape: IDrawable;
begin
  LContext := TVCLCanvasAdapter.Create(Self.Canvas);

  for LShape in FShapes do
    LShape.Render(LContext);
end;

procedure TForm1.InitializeShapes;
var
  LLogoPath: string;
begin
  LLogoPath := 'Pangolin.jpeg';

  FShapes := [
    TRectangleShape.Create(RectF(50, 50, 150, 150), TAlphaColors.Red),
    TCircleShape.Create(RectF(200, 50, 300, 150), TAlphaColors.Green),
    TRectangleShape.Create(RectF(350, 50, 450, 150), TAlphaColors.Blue),
    TImageShape.Create(RectF(500, 50, 600, 150), LLogoPath)
  ];
end;

procedure TForm1.tmrAnimationTimer(Sender: TObject);
var
  LShape: IDrawable;
begin
  // Increment progress (e.g., 2% per frame)
  FAnimationStep := FAnimationStep + 0.02;

  // Update Domain Objects
  for LShape in FShapes do
    LShape.SetProgress(FAnimationStep);

  // Check for completion
  if FAnimationStep >= 1.0 then
  begin
    FAnimationStep := 1.0;
    tmrAnimation.Enabled := False;
  end;

  // Trigger Repaint
  Self.Invalidate;
end;
end.
