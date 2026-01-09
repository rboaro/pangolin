unit OpenGL.Domain.Laser;

interface

uses
  OpenGL.Core.Contracts, System.UITypes, System.Math;

type
  TLaserBeam = class(TInterfacedObject, I3DObject)
  strict private
    FColor: TAlphaColor;
    FStartX, FStartY, FStartZ: Single;
    FEndX, FEndY, FEndZ: Single;
    FSpeed: Single;
    FTime: Double;
  public
    constructor Create(const AColor: TAlphaColor; const ASpeed: Single);
    procedure Update(const ADeltaTime: Double);
    procedure Render(const AContext: IRenderContext);
  end;

implementation

constructor TLaserBeam.Create(const AColor: TAlphaColor; const ASpeed: Single);
begin
  FColor := AColor;
  FSpeed := ASpeed;
  FTime := Random(100); // Random offset

  // Initial position (center)
  FStartX := 0;
  FStartY := 0;
  FStartZ := -5.0;
end;

procedure TLaserBeam.Update(const ADeltaTime: Double);
begin
  FTime := FTime + (ADeltaTime * FSpeed);

  // Simulate chaotic movement using Sine/Cosine waves
  // End point moves in a Lissajous-like pattern
  FEndX := Sin(FTime) * 3.0;
  FEndY := Cos(FTime * 1.3) * 2.5;
  FEndZ := -5.0 + (Sin(FTime * 0.7) * 2.0); // Depth oscillation
end;

procedure TLaserBeam.Render(const AContext: IRenderContext);
begin
  AContext.SetColor(FColor);
  AContext.SetLineWidth(2.0); // Thicker line for "Laser" effect

  // Draw the beam from center to the calculated end point
  AContext.DrawLine(FStartX, FStartY, FStartZ, FEndX, FEndY, FEndZ);

  // Optional: Draw a "core" for the laser (white, thinner)
  AContext.SetColor(TAlphaColors.White);
  AContext.SetLineWidth(1.0);
  AContext.DrawLine(FStartX, FStartY, FStartZ, FEndX, FEndY, FEndZ);
end;

end.

