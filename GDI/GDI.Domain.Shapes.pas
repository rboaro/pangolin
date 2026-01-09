unit GDI.Domain.Shapes;

interface

uses
  System.Types, System.UITypes, GDI.Core.Contracts, System.SysUtils,
  Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.jpeg;

type
  TShapeBase = class abstract(TInterfacedObject, IDrawable)
  strict protected
    FBounds: TRectF;
    FColor: TAlphaColor;
    FProgress: Single;
    function GetAnimatedRect: TRectF;
  public
    constructor Create(const ABounds: TRectF; const AColor: TAlphaColor);
    procedure SetProgress(const AValue: Single);
    procedure Render(const AContext: IGraphicsContext); virtual; abstract;
  end;

  TRectangleShape = class(TShapeBase)
  public
    procedure Render(const AContext: IGraphicsContext); override;
  end;

  TCircleShape = class(TShapeBase)
  public
    procedure Render(const AContext: IGraphicsContext); override;
  end;


type
  TImageShape = class(TShapeBase)
  strict private
    FPicture: TPicture;
  public
    constructor Create(const ABounds: TRectF; const AFilePath: string);
    destructor Destroy; override;
    procedure Render(const AContext: IGraphicsContext); override;
  end;

implementation

{ TShapeBase }

constructor TImageShape.Create(const ABounds: TRectF;
  const AFilePath: string);
begin
  // Pass TAlphaColors.Null because we don't use a brush color for images
  inherited Create(ABounds, TAlphaColors.Null);

  FPicture := TPicture.Create;
  try
    FPicture.LoadFromFile(AFilePath);
  except
    raise Exception.Create('Error, file not found or corrupted.');
  end;
end;

destructor TImageShape.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TImageShape.Render(const AContext: IGraphicsContext);
var
  LRect: TRectF;
begin
  LRect := GetAnimatedRect;
  if LRect.IsEmpty then Exit;

  // Delegate drawing to the context
  if FPicture.Graphic <> nil then
    AContext.DrawImage(FPicture.Graphic, LRect);
end;

constructor TShapeBase.Create(const ABounds: TRectF; const AColor: TAlphaColor);
begin
  inherited Create;
  FBounds := ABounds;
  FColor := AColor;
  FProgress := 1.0; // Default to fully visible
end;

procedure TShapeBase.SetProgress(const AValue: Single);
begin
  // Clamp value between 0 and 1
  if AValue < 0 then FProgress := 0
  else if AValue > 1 then FProgress := 1
  else FProgress := AValue;
end;

function TShapeBase.GetAnimatedRect: TRectF;
var
  LCenter: TPointF;
  LWidth, LHeight: Single;
begin
  if FProgress >= 1.0 then
    Exit(FBounds);

  if FProgress <= 0.0 then
    Exit(TRectF.Empty);

  // Calculate dimensions based on progress
  LWidth := FBounds.Width * FProgress;
  LHeight := FBounds.Height * FProgress;

  // Center the new rect
  LCenter := FBounds.CenterPoint;

  Result := TRectF.Create(
    LCenter.X - (LWidth / 2),
    LCenter.Y - (LHeight / 2),
    LCenter.X + (LWidth / 2),
    LCenter.Y + (LHeight / 2)
  );
end;

{ TRectangleShape }

procedure TRectangleShape.Render(const AContext: IGraphicsContext);
var
  LRect: TRectF;
begin
  LRect := GetAnimatedRect;
  if LRect.IsEmpty then Exit;

  AContext.SetPen(TAlphaColors.Black, 2.0);
  AContext.SetBrush(FColor);
  AContext.DrawRectangle(LRect);
end;

{ TCircleShape }

procedure TCircleShape.Render(const AContext: IGraphicsContext);
var
  LRect: TRectF;
begin
  LRect := GetAnimatedRect;
  if LRect.IsEmpty then Exit;

  AContext.SetPen(TAlphaColors.Blue, 1.0);
  AContext.SetBrush(FColor);
  AContext.DrawEllipse(LRect);
end;

end.
