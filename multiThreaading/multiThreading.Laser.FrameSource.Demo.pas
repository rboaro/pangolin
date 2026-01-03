unit multiThreading.Laser.FrameSource.Demo;

interface

uses
  System.SysUtils, System.Math, System.Diagnostics,

  multiThreading.Laser.Domain, multiThreading.Laser.Abstractions;

type
  TDemoFrameSource = class(TInterfacedObject, IFrameSource)
  private
    FFrameId: Int64;
    FClock: TStopwatch;
  public
    constructor Create;
    function NextFrame: TLaserFrame;
  end;

implementation

uses
  System.UITypes;

constructor TDemoFrameSource.Create;
begin
  inherited Create;
  FClock := TStopwatch.StartNew;
end;

function TDemoFrameSource.NextFrame: TLaserFrame;
const
  KPoints = 30000;
var
  Pts: TArray<TLaserPoint>;
  I: Integer;
  T, A: Double;
  C: TAlphaColor;
begin
  Inc(FFrameId);
  T := FClock.Elapsed.TotalSeconds;

  SetLength(Pts, KPoints);

  for I := 0 to KPoints - 1 do
  begin
    A := (I / KPoints) * 2 * Pi;

    Pts[I].X := Cos(A * 3 + T * 1.3) * 0.8 + Cos(A * 11 - T * 0.7) * 0.15;
    Pts[I].Y := Sin(A * 2 + T * 1.1) * 0.8 + Sin(A * 13 - T * 0.5) * 0.15;

    C := $FF000000 or (Cardinal((I * 5) and $FF) shl 16) or (Cardinal((I * 3) and $FF) shl 8) or Cardinal((I * 7) and $FF);
    Pts[I].Color := C;
  end;

  Result := TLaserFrame.Create(Pts, FFrameId, T);
end;

end.


