unit Tests.Laser.FrameAnimator;

interface

uses
  DUnitX.TestFramework,
  drawingGraphics.Laser.FrameAnimation,
  drawingGraphics.Laser.FrameModel,
  System.Math;

type
  [TestFixture]
  TLaserFrameAnimatorTests = class
  private
    function NearlyEqual(const A, B: Single; const Eps: Single = 1e-6): Boolean;
  public
    [Test]
    procedure NextFrame_DecreasesScale;

    [Test]
    procedure NextFrame_ResetsScaleWhenBelowMin;

    [Test]
    procedure SquareFrame_HasFourSegments;

    [Test]
    procedure SquareFrame_CorrectCoordinatesAtKnownScale;
  end;

implementation

function TLaserFrameAnimatorTests.NearlyEqual(const A, B, Eps: Single): Boolean;
begin
  Result := Abs(A - B) <= Eps;
end;

procedure TLaserFrameAnimatorTests.NextFrame_DecreasesScale;
var
  A: TLaserFrameAnimator;
  S1, S2: Single;
begin
  A := TLaserFrameAnimator.Create(0.9, 0.05, 0.01);
  try
    S1 := A.CurrentScale; // 0.9
    A.NextFrame;
    S2 := A.CurrentScale; // 0.89

    Assert.IsTrue(S2 < S1, 'Scale should decrease after NextFrame');
    Assert.IsTrue(NearlyEqual(S2, 0.89), 'Expected scale 0.89');
  finally
    A.Free;
  end;
end;

procedure TLaserFrameAnimatorTests.NextFrame_ResetsScaleWhenBelowMin;
var
  A: TLaserFrameAnimator;
  I: Integer;
begin
  // Small numbers so test runs fast
  A := TLaserFrameAnimator.Create(0.2, 0.05, 0.05);
  try
    // Start = 0.2
    // Tick1 => 0.15
    // Tick2 => 0.10
    // Tick3 => 0.05 => reset to 0.2 (because <= MinScale)
    for I := 1 to 3 do
      A.NextFrame;

    Assert.IsTrue(NearlyEqual(A.CurrentScale, 0.2), 'Scale should reset to ResetScale');
  finally
    A.Free;
  end;
end;

procedure TLaserFrameAnimatorTests.SquareFrame_HasFourSegments;
var
  F: TLaserFrame;
begin
  F := TLaserFrame.Square(0.8);
  Assert.AreEqual(4, Length(F.Segments), 'Square must contain 4 line segments');
end;

procedure TLaserFrameAnimatorTests.SquareFrame_CorrectCoordinatesAtKnownScale;
var
  F: TLaserFrame;
  S: Single;
begin
  S := 0.8;
  F := TLaserFrame.Square(S);

  // Segment 0: (-S,-S) -> (S,-S)
  Assert.IsTrue(NearlyEqual(F.Segments[0].A.X, -S));
  Assert.IsTrue(NearlyEqual(F.Segments[0].A.Y, -S));
  Assert.IsTrue(NearlyEqual(F.Segments[0].B.X,  S));
  Assert.IsTrue(NearlyEqual(F.Segments[0].B.Y, -S));

  // Segment 1: (S,-S) -> (S,S)
  Assert.IsTrue(NearlyEqual(F.Segments[1].A.X,  S));
  Assert.IsTrue(NearlyEqual(F.Segments[1].A.Y, -S));
  Assert.IsTrue(NearlyEqual(F.Segments[1].B.X,  S));
  Assert.IsTrue(NearlyEqual(F.Segments[1].B.Y,  S));

  // Segment 2: (S,S) -> (-S,S)
  Assert.IsTrue(NearlyEqual(F.Segments[2].A.X,  S));
  Assert.IsTrue(NearlyEqual(F.Segments[2].A.Y,  S));
  Assert.IsTrue(NearlyEqual(F.Segments[2].B.X, -S));
  Assert.IsTrue(NearlyEqual(F.Segments[2].B.Y,  S));

  // Segment 3: (-S,S) -> (-S,-S)
  Assert.IsTrue(NearlyEqual(F.Segments[3].A.X, -S));
  Assert.IsTrue(NearlyEqual(F.Segments[3].A.Y,  S));
  Assert.IsTrue(NearlyEqual(F.Segments[3].B.X, -S));
  Assert.IsTrue(NearlyEqual(F.Segments[3].B.Y, -S));
end;

initialization
  TDUnitX.RegisterTestFixture(TLaserFrameAnimatorTests);

end.

