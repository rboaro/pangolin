unit drawingGraphics.Laser.FrameAnimation;

interface

uses
  drawingGraphics.Laser.FrameModel;

type
  TLaserFrameAnimator = class
  private
    FScale: Single;
    FStep: Single;
    FMinScale: Single;
    FResetScale: Single;
  public
    constructor Create(const ResetScale, MinScale, Step: Single);

    function CurrentScale: Single;

    /// Advances one “tick” and returns a new frame.
    function NextFrame: TLaserFrame;
  end;

implementation

uses
  System.SysUtils;

constructor TLaserFrameAnimator.Create(const ResetScale, MinScale, Step: Single);
begin
  inherited Create;
  if Step <= 0 then
    raise Exception.Create('Step must be > 0');
  if ResetScale <= 0 then
    raise Exception.Create('ResetScale must be > 0');
  if (MinScale <= 0) or (MinScale >= ResetScale) then
    raise Exception.Create('MinScale must be > 0 and < ResetScale');

  FResetScale := ResetScale;
  FMinScale := MinScale;
  FStep := Step;

  FScale := FResetScale;
end;

function TLaserFrameAnimator.CurrentScale: Single;
begin
  Result := FScale;
end;

function TLaserFrameAnimator.NextFrame: TLaserFrame;
begin
  // shrink
  FScale := FScale - FStep;

  // reset if too small
  if FScale <= FMinScale then
    FScale := FResetScale;

  Result := TLaserFrame.Square(FScale);
end;

end.

