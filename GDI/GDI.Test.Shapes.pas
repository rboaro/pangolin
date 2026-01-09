unit GDI.Test.Shapes;


interface

uses
  DUnitX.TestFramework, GDI.Core.Contracts, GDI.Domain.Shapes, System.Types, System.UITypes;

type
  /// <summary>
  ///   Manual Mock for IGraphicsContext to verify calls.
  ///   (In a real project, you might use Delphi-Mocks or Spring4D)
  /// </summary>
  TMockGraphicsContext = class(TInterfacedObject, IGraphicsContext)
  public
    LastDrawShape: string;
    LastColor: TAlphaColor;
    procedure SetPen(const AColor: TAlphaColor; const AThickness: Single);
    procedure SetBrush(const AColor: TAlphaColor);
    procedure DrawRectangle(const ARect: TRectF);
    procedure DrawEllipse(const ARect: TRectF);
    procedure DrawText(const AText: string; const ARect: TRectF);
    procedure DrawImage(const AImage: TObject; const ARect: TRectF);
  end;

  [TestFixture]
  TShapeTests = class
  public
    [Test]
    procedure Test_Rectangle_Draws_Correctly;
    [Test]
    procedure Test_Circle_Draws_Correctly;
  end;

implementation

{ TMockGraphicsContext }

procedure TMockGraphicsContext.SetBrush(const AColor: TAlphaColor);
begin
  LastColor := AColor;
end;

procedure TMockGraphicsContext.DrawImage(const AImage: TObject;
  const ARect: TRectF);
begin

end;

procedure TMockGraphicsContext.DrawRectangle(const ARect: TRectF);
begin
  LastDrawShape := 'Rectangle';
end;

procedure TMockGraphicsContext.DrawEllipse(const ARect: TRectF);
begin
  LastDrawShape := 'Ellipse';
end;

procedure TMockGraphicsContext.SetPen(const AColor: TAlphaColor; const AThickness: Single); begin end;
procedure TMockGraphicsContext.DrawText(const AText: string; const ARect: TRectF); begin end;

{ TShapeTests }

procedure TShapeTests.Test_Rectangle_Draws_Correctly;
var
  LContext: TMockGraphicsContext;
  LShape: IDrawable;
begin
  // Arrange
  LContext := TMockGraphicsContext.Create;
  LShape := TRectangleShape.Create(TRectF.Create(0,0,10,10), TAlphaColors.Red);

  // Act
  LShape.Render(LContext);

  // Assert
  Assert.AreEqual('Rectangle', LContext.LastDrawShape);
  Assert.AreEqual(TAlphaColors.Red, LContext.LastColor);
end;

procedure TShapeTests.Test_Circle_Draws_Correctly;
var
  LContext: TMockGraphicsContext;
  LShape: IDrawable;
begin
  // Arrange
  LContext := TMockGraphicsContext.Create;
  LShape := TCircleShape.Create(TRectF.Create(0,0,10,10), TAlphaColors.Blue);

  // Act
  LShape.Render(LContext);

  // Assert
  Assert.AreEqual('Ellipse', LContext.LastDrawShape);
  Assert.AreEqual(TAlphaColors.Blue, LContext.LastColor);
end;

end.
