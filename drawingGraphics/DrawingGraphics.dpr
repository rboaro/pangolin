program DrawingGraphics;

uses
  System.StartUpCopy,
  FMX.Forms,
  drawingGraphics.Main in 'drawingGraphics.Main.pas' {frmDrawingGraphics},
  drawingGraphics.Laser.Domain in 'drawingGraphics.Laser.Domain.pas',
  drawingGraphics.Laser.Renderer.OpenGL in 'drawingGraphics.Laser.Renderer.OpenGL.pas',
  drawingGraphics.Laser.Controls.OpenGL in 'drawingGraphics.Laser.Controls.OpenGL.pas',
  drawingGraphics.Laser.Renderer.DX11 in 'drawingGraphics.Laser.Renderer.DX11.pas',
  drawingGraphics.Laser.Controls.DX11 in 'drawingGraphics.Laser.Controls.DX11.pas',
  drawingGraphics.Laser.Renderer.OpenGL_V2 in 'drawingGraphics.Laser.Renderer.OpenGL_V2.pas',
  drawingGraphics.Laser.FrameModel in 'drawingGraphics.Laser.FrameModel.pas',
  drawingGraphics.Laser.FrameAnimation in 'drawingGraphics.Laser.FrameAnimation.pas',
  Tests.Laser.FrameAnimator in 'Tests.Laser.FrameAnimator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDrawingGraphics, frmDrawingGraphics);
  Application.Run;
end.
