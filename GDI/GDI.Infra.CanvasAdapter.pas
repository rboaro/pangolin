unit GDI.Infra.CanvasAdapter;

interface

uses
  System.Types, System.UITypes, System.UIConsts, Vcl.Graphics, GDI.Core.Contracts,
  Winapi.Windows;

type
  /// <summary>
  ///   Concrete implementation of IGraphicsContext using VCL TCanvas (GDI).
  /// </summary>
  TVCLCanvasAdapter = class(TInterfacedObject, IGraphicsContext)
  strict private
    FCanvas: TCanvas;
    function AlphaColorToColor(const AColor: TAlphaColor): TColor;
  public
    constructor Create(const ACanvas: TCanvas);
    procedure SetPen(const AColor: TAlphaColor; const AThickness: Single);
    procedure SetBrush(const AColor: TAlphaColor);
    procedure DrawRectangle(const ARect: TRectF);
    procedure DrawEllipse(const ARect: TRectF);
    procedure DrawText(const AText: string; const ARect: TRectF);
    procedure DrawImage(const AImage: TObject; const ARect: TRectF);

  end;

implementation

uses
  System.Math;

constructor TVCLCanvasAdapter.Create(const ACanvas: TCanvas);
begin
  inherited Create;
  FCanvas := ACanvas;
end;

function TVCLCanvasAdapter.AlphaColorToColor(const AColor: TAlphaColor): TColor;
begin
  // Simple conversion for VCL GDI (which doesn't natively support Alpha in all GDI calls)
  // In a real GDI+ or Direct2D scenario, this would be different.
  Result := TColor(AColor and $00FFFFFF);
end;

procedure TVCLCanvasAdapter.SetPen(const AColor: TAlphaColor; const AThickness: Single);
begin
  FCanvas.Pen.Color := AlphaColorToColor(AColor);
  FCanvas.Pen.Width := Round(AThickness);
  FCanvas.Pen.Style := psSolid;
end;

procedure TVCLCanvasAdapter.SetBrush(const AColor: TAlphaColor);
begin
  FCanvas.Brush.Color := AlphaColorToColor(AColor);
  FCanvas.Brush.Style := bsSolid;
end;

procedure TVCLCanvasAdapter.DrawRectangle(const ARect: TRectF);
begin
  // GDI Call: Rectangle
  FCanvas.Rectangle(ARect.Round);
end;

procedure TVCLCanvasAdapter.DrawEllipse(const ARect: TRectF);
begin
  // GDI Call: Ellipse
  FCanvas.Ellipse(ARect.Round);
end;

procedure TVCLCanvasAdapter.DrawImage(const AImage: TObject;
  const ARect: TRectF);
var
  LGraphic: TGraphic;
begin
  if (AImage <> nil) and (AImage is TGraphic) then
  begin
    LGraphic := TGraphic(AImage);
    // GDI Call: StretchDraw resizes the image to fit the rect
    FCanvas.StretchDraw(ARect.Round, LGraphic);
  end;
end;

procedure TVCLCanvasAdapter.DrawText(const AText: string; const ARect: TRectF);
var
  LRect: TRect;
begin
  LRect := ARect.Round;
  // GDI Call: DrawText
  Winapi.Windows.DrawText(FCanvas.Handle, PChar(AText), Length(AText), LRect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
end;

end.
