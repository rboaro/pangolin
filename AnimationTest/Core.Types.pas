unit Core.Types;

interface

uses
  Core.Interfaces;

type
  /// <summary>
  ///  Simple color record.
  /// </summary>
  TColorRGB = record
    R, G, B: Single;
    class function Create(AR, AG, AB: Single): TColorRGB; static;
  end;

  /// <summary>
  ///  Vertex structure for array-based rendering.
  /// </summary>
  TVertex = record
    X, Y, Z: Single;
  end;

  PVertex = ^TVertex;

const
  clSkyBlue: TColorRGB = (R: 0.529; G: 0.808; B: 0.922);
  clBirdBody: TColorRGB = (R: 1.0; G: 0.0; B: 0.0); // Red bird
  clBirdWing: TColorRGB = (R: 0.8; G: 0.0; B: 0.0); // Darker red wings

implementation

{ TColorRGB }

class function TColorRGB.Create(AR, AG, AB: Single): TColorRGB;
begin
  Result.R := AR;
  Result.G := AG;
  Result.B := AB;
end;

end.
