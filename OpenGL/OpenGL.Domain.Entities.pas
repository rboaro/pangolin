unit OpenGL.Domain.Entities;

interface

uses
  OpenGL.Core.Contracts, System.UITypes;

type
  TSpinningCube = class(TInterfacedObject, I3DObject)
  strict private
    FRotation: Single;
    FColor: TAlphaColor;
    FPositionX: Single;
  public
    constructor Create(const AColor: TAlphaColor; const APosX: Single);
    procedure Update(const ADeltaTime: Double);
    procedure Render(const AContext: IRenderContext);
  end;

implementation

constructor TSpinningCube.Create(const AColor: TAlphaColor; const APosX: Single);
begin
  FColor := AColor;
  FPositionX := APosX;
  FRotation := 0;
end;

procedure TSpinningCube.Update(const ADeltaTime: Double);
begin
  // Rotate 90 degrees per second
  FRotation := FRotation + (90.0 * ADeltaTime);
  if FRotation > 360 then FRotation := FRotation - 360;
end;

procedure TSpinningCube.Render(const AContext: IRenderContext);
begin
  AContext.PushMatrix;
  try
    AContext.Translate(FPositionX, 0.0, -6.0); // Move back into screen
    AContext.Rotate(FRotation, 1.0, 1.0, 0.0); // Rotate on X and Y axis

    AContext.SetColor(FColor);
    AContext.DrawCube(1.0);
  finally
    AContext.PopMatrix;
  end;
end;

end.
