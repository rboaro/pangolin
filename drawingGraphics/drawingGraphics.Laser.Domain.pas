unit drawingGraphics.Laser.Domain;

interface

uses
  System.Types, System.UITypes, System.Generics.Collections, System.Math;

type
  TLaserLine = record
    A, B: TPointF;
    Color: TAlphaColor;
    Thickness: Single; // in pixels
  end;

  TLaserFrame = record
    Lines: TArray<TLaserLine>;
    class function Create(const ALines: TArray<TLaserLine>): TLaserFrame; static;
  end;

  ILaserScene = interface
    ['{1B0C8E0E-7E3C-47E2-A2E6-1FD02B0D6F8E}']
    function BuildFrame(const AViewportSize: TSizeF; const ATimeSeconds: Double): TLaserFrame;
  end;

  ILineRenderer = interface
    ['{F8B12C6A-5E54-4D8A-9A23-9B7F5B4A1A8C}']
    procedure Resize(const AWidth, AHeight: Integer);
    procedure BeginFrame;
    procedure Draw(const AFrame: TLaserFrame);
    procedure EndFrame;
  end;

  TNeonOrbitScene = class(TInterfacedObject, ILaserScene)
  private
    function HueToColor(const H: Single; const Alpha: Byte = $FF): TAlphaColor;
  public
    function BuildFrame(const AViewportSize: TSizeF; const ATimeSeconds: Double): TLaserFrame;
  end;

implementation

{ TLaserFrame }

class function TLaserFrame.Create(const ALines: TArray<TLaserLine>): TLaserFrame;
begin
  Result.Lines := ALines;
end;

{ TNeonOrbitScene }

function TNeonOrbitScene.HueToColor(const H: Single; const Alpha: Byte): TAlphaColor;
var
  R, G, B: Single;
  HH, X, C, M: Single;
begin
  // HSV -> RGB with S=1, V=1 (bright neon)
  HH := Frac(H) * 6;
  C := 1;
  X := C * (1 - Abs(Frac(HH) * 2 - 1));

  if (HH < 1) then begin R := C; G := X; B := 0; end
  else if (HH < 2) then begin R := X; G := C; B := 0; end
  else if (HH < 3) then begin R := 0; G := C; B := X; end
  else if (HH < 4) then begin R := 0; G := X; B := C; end
  else if (HH < 5) then begin R := X; G := 0; B := C; end
  else begin R := C; G := 0; B := X; end;

  M := 0; // V=1, so M=0
  Result :=
    (Alpha shl 24) or
    (Round((R + M) * 255) shl 16) or
    (Round((G + M) * 255) shl 8) or
    (Round((B + M) * 255));
end;

function TNeonOrbitScene.BuildFrame(const AViewportSize: TSizeF; const ATimeSeconds: Double): TLaserFrame;
const
  KPoints = 420;
var
  Lines: TArray<TLaserLine>;
  Center: TPointF;
  I: Integer;
  T: Double;
  P0, P1: TPointF;
  Angle0, Angle1: Double;
  R0, R1: Double;
  Hue: Single;
begin
  SetLength(Lines, KPoints);

  Center := PointF(AViewportSize.Width * 0.5, AViewportSize.Height * 0.5);
  T := ATimeSeconds;

  for I := 0 to KPoints - 1 do
  begin
    Angle0 := (I / KPoints) * 2 * Pi;
    Angle1 := ((I + 1) / KPoints) * 2 * Pi;

    R0 := (Min(AViewportSize.Width, AViewportSize.Height) * 0.35) *
          (0.65 + 0.35 * Sin(Angle0 * 3 + T * 1.3));
    R1 := (Min(AViewportSize.Width, AViewportSize.Height) * 0.35) *
          (0.65 + 0.35 * Sin(Angle1 * 3 + T * 1.3));

    P0 := PointF(
      Center.X + Cos(Angle0 + T * 0.4) * R0 + Cos(Angle0 * 2 - T * 0.7) * (R0 * 0.15),
      Center.Y + Sin(Angle0 + T * 0.4) * R0 + Sin(Angle0 * 2 - T * 0.7) * (R0 * 0.15)
    );

    P1 := PointF(
      Center.X + Cos(Angle1 + T * 0.4) * R1 + Cos(Angle1 * 2 - T * 0.7) * (R1 * 0.15),
      Center.Y + Sin(Angle1 + T * 0.4) * R1 + Sin(Angle1 * 2 - T * 0.7) * (R1 * 0.15)
    );

    Hue := I / KPoints;

    Lines[I].A := P0;
    Lines[I].B := P1;
    Lines[I].Color := HueToColor(Hue);
    Lines[I].Thickness := 2.0;
  end;

  Result := TLaserFrame.Create(Lines);
end;


end.
