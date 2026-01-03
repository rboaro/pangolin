unit drawingGraphics.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  drawingGraphics.Laser.Domain,  drawingGraphics.Laser.Renderer.OpenGL_V2, FMX.Objects, drawingGraphics.Laser.FrameAnimation,
  drawingGraphics.Laser.frameModel,
  {$IFDEF MSWINDOWS}
//  drawingGraphics.Laser.Controls.DX11 // or Laser.Controls.OpenGL
   drawingGraphics.Laser.Controls.OpenGL
  {$ENDIF};

type
  TfrmDrawingGraphics = class(TForm)
    Rectangle1: TRectangle;
    RenderTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RenderTimerTimer(Sender: TObject);
  private
    //FLaser: TLaserDX11Control; // or TLaserOpenGLControl
    FLaser: TLaserOpenGLControl;
    FScene: ILaserScene;
    FLaserRenderer : TLaserOpenGLRendererV2;
    FScale: Single;
    FScaleStep: Single;
    FAnimator: TLaserFrameAnimator;
    procedure AnimationOne;
    procedure AnimationTwo;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmDrawingGraphics: TfrmDrawingGraphics;

implementation

uses  FMX.Platform.Win, Winapi.Windows;

{$R *.fmx}

{ TfrmDrawingGraphics }

procedure TfrmDrawingGraphics.AnimationOne;
begin
  FScene := TNeonOrbitScene.Create;

  FLaser := TLaserOpenGLControl.Create(Self);
  FLaser.Parent := Rectangle1;
  FLaser.Align := TAlignLayout.Contents;
  FLaser.Scene := FScene;
  FLaser.Visible := True;
  Flaser.HitTest := False;
end;

constructor TfrmDrawingGraphics.Create(AOwner: TComponent);
begin
  inherited;
  FAnimator := TLaserFrameAnimator.Create(0.9, 0.05, 0.01);
    { *************************************************************************************************

     Remove the comment from this code and comment out the OnTimer event code to see the other animation.

     **************************************************************************************************
    }

    //AnimationOne;

end;


procedure TfrmDrawingGraphics.FormDestroy(Sender: TObject);
begin
  RenderTimer.Enabled := False;
  FLaserRenderer.Free;
  FAnimator.Free;
end;

procedure TfrmDrawingGraphics.FormHide(Sender: TObject);
begin
  RenderTimer.Enabled := False;
end;

procedure TfrmDrawingGraphics.FormShow(Sender: TObject);
var
  WinHandle: HWND;
begin
  WinHandle := WindowHandleToPlatform(Handle).Wnd;

  if FLaserRenderer = nil then
    FLaserRenderer := TLaserOpenGLRendererV2.Create(WinHandle);

  // Animation setup
  FScale := 0.9;
  FScaleStep := 0.01;

  RenderTimer.Interval := 16; // ~60 FPS
  RenderTimer.Enabled := True;
end;

procedure TfrmDrawingGraphics.RenderTimerTimer(Sender: TObject);
begin
  AnimationTwo;
end;

procedure TfrmDrawingGraphics.AnimationTwo;
var
  Frame: drawingGraphics.Laser.TLaserFrame;
  I: Integer;
begin
  Frame := FAnimator.NextFrame;

  FLaserRenderer.BeginFrame;
  FLaserRenderer.SetColor(0, 1, 0);
  FLaserRenderer.SetLineWidth(2);

  for I := 0 to High(Frame.Segments) do
  begin
    FLaserRenderer.MoveTo(Frame.Segments[I].A.X, Frame.Segments[I].A.Y);
    FLaserRenderer.LineTo(Frame.Segments[I].B.X, Frame.Segments[I].B.Y);
  end;

  FLaserRenderer.EndFrame;
end;


end.
