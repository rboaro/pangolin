unit multiThreading.Laser.Codec.Demo;

interface

uses
  System.SysUtils, System.Math,
  System.Hash, // for demo hash-based "encryption" mixing

  multiThreading.Laser.Domain, multiThreading.Laser.Abstractions;

type
  TDemoProjectorCodec = class(TInterfacedObject, IProjectorCodec)
  private
    function TransformPoint(const P: TLaserPoint; const ProjectorId: Integer): TLaserPoint;
    function Serialize(const Points: TArray<TLaserPoint>): TBytes;
    function CompressRLE(const Data: TBytes): TBytes;
    function EncryptXorStream(const Data: TBytes; const KeySeed: Integer): TBytes;
  public
    function Encode(const Frame: TLaserFrame; const ProjectorId: Integer): TLaserPacket;
  end;

implementation

function TDemoProjectorCodec.TransformPoint(const P: TLaserPoint; const ProjectorId: Integer): TLaserPoint;
var
  Angle: Double;
  X, Y: Double;
begin
  Angle := (ProjectorId mod 10) * 0.01; // tiny variation per unit
  X := P.X * Cos(Angle) - P.Y * Sin(Angle);
  Y := P.X * Sin(Angle) + P.Y * Cos(Angle);

  Result := P;
  Result.X := X;
  Result.Y := Y;
end;

function TDemoProjectorCodec.Serialize(const Points: TArray<TLaserPoint>): TBytes;
type
  TPackedPoint = packed record
    X16, Y16: SmallInt;
    A, R, G, B: Byte;
  end;
var
  I: Integer;
  OutBuf: TBytes;
  PP: TPackedPoint;
  Idx: Integer;

  function ToI16(const V: Single): SmallInt;
  var
    Clamped: Single;
  begin
    Clamped := EnsureRange(V, -1.0, 1.0);
    Result := Round(Clamped * 32767);
  end;

  procedure WritePoint(const P: TLaserPoint);
  begin
    PP.X16 := ToI16(P.X);
    PP.Y16 := ToI16(P.Y);
    PP.A := (P.Color shr 24) and $FF;
    PP.R := (P.Color shr 16) and $FF;
    PP.G := (P.Color shr 8) and $FF;
    PP.B := (P.Color) and $FF;

    Move(PP, OutBuf[Idx], SizeOf(PP));
    Inc(Idx, SizeOf(PP));
  end;

begin
  SetLength(OutBuf, Length(Points) * SizeOf(TPackedPoint));
  Idx := 0;

  for I := 0 to High(Points) do
    WritePoint(Points[I]);

  Result := OutBuf;
end;

function TDemoProjectorCodec.CompressRLE(const Data: TBytes): TBytes;
var
  I, J: Integer;
  Count: Byte;
begin
  SetLength(Result, 0);
  I := 0;

  while I < Length(Data) do
  begin
    Count := 1;
    while (I + Count < Length(Data)) and (Count < 255) and (Data[I + Count] = Data[I]) do
      Inc(Count);

    J := Length(Result);
    SetLength(Result, J + 2);
    Result[J] := Data[I];
    Result[J + 1] := Count;

    Inc(I, Count);
  end;
end;

function TDemoProjectorCodec.EncryptXorStream(const Data: TBytes; const KeySeed: Integer): TBytes;
var
  I: Integer;
  Key: TBytes;
begin
  Key := THashSHA2.GetHashBytes(IntToStr(KeySeed));
  SetLength(Result, Length(Data));

  for I := 0 to High(Data) do
    Result[I] := Data[I] xor Key[I mod Length(Key)];
end;

function TDemoProjectorCodec.Encode(const Frame: TLaserFrame; const ProjectorId: Integer): TLaserPacket;
var
  Transformed: TArray<TLaserPoint>;
  I: Integer;
  Raw, Compressed, Encrypted: TBytes;
begin
  SetLength(Transformed, Length(Frame.Points));
  for I := 0 to High(Frame.Points) do
    Transformed[I] := TransformPoint(Frame.Points[I], ProjectorId);

  Raw := Serialize(Transformed);
  Compressed := CompressRLE(Raw);
  Encrypted := EncryptXorStream(Compressed, ProjectorId * 7919 + Integer(Frame.FrameId and $7FFFFFFF));

  Result := TLaserPacket.Create(Frame.FrameId, Encrypted);
end;

end.


