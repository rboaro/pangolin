unit drawingGraphics.Laser.FrameModel;

interface

uses
  System.Types;

type
  TLineSegment = record
    A: TPointF;
    B: TPointF;
    class function Create(const AX, AY, BX, BY: Single): TLineSegment; static;
  end;

  TLaserFrame = record
    Segments: TArray<TLineSegment>;
    class function Square(const HalfSize: Single): TLaserFrame; static;
  end;

implementation

{ TLineSegment }

class function TLineSegment.Create(const AX, AY, BX, BY: Single): TLineSegment;
begin
  Result.A := PointF(AX, AY);
  Result.B := PointF(BX, BY);
end;

{ TLaserFrame }

class function TLaserFrame.Square(const HalfSize: Single): TLaserFrame;
var
  S: Single;
begin
  S := HalfSize;

  // Square = 4 segments
  SetLength(Result.Segments, 4);
  Result.Segments[0] := TLineSegment.Create(-S, -S,  S, -S);
  Result.Segments[1] := TLineSegment.Create( S, -S,  S,  S);
  Result.Segments[2] := TLineSegment.Create( S,  S, -S,  S);
  Result.Segments[3] := TLineSegment.Create(-S,  S, -S, -S);
end;

end.

