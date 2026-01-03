unit drawingGraphics.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  drawingGraphics.Laser.Domain,
  {$IFDEF MSWINDOWS}
//  drawingGraphics.Laser.Controls.DX11 // or Laser.Controls.OpenGL
   drawingGraphics.Laser.Controls.OpenGL, FMX.Objects
  {$ENDIF};

type
  TfrmDrawingGraphics = class(TForm)
    Rectangle1: TRectangle;
  private
    //FLaser: TLaserDX11Control; // or TLaserOpenGLControl
    FLaser: TLaserOpenGLControl;
    FScene: ILaserScene;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmDrawingGraphics: TfrmDrawingGraphics;

implementation

{$R *.fmx}

{ TfrmDrawingGraphics }

constructor TfrmDrawingGraphics.Create(AOwner: TComponent);
begin
  inherited;

  FScene := TNeonOrbitScene.Create;

  FLaser := TLaserOpenGLControl.Create(Self);
  FLaser.Parent := Rectangle1;
  FLaser.Align := TAlignLayout.Contents;
  FLaser.Scene := FScene;
  FLaser.Visible := True;
  Flaser.HitTest := False;

end;

end.
