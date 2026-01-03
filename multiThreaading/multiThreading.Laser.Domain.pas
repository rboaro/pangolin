unit multiThreading.Laser.Domain;

interface

uses
  System.SysUtils, System.Types, System.UITypes;

type
  TLaserPoint = packed record
    X, Y: Single;
    Color: TAlphaColor;
  end;

  TLaserFrame = record
    Points: TArray<TLaserPoint>;
    FrameId: Int64;
    TimeSeconds: Double;
    class function Create(const APoints: TArray<TLaserPoint>; const AFrameId: Int64; const ATimeSeconds: Double): TLaserFrame; static;
  end;

  TLaserPacket = record
    FrameId: Int64;
    Payload: TBytes;
    class function Create(const AFrameId: Int64; const APayload: TBytes): TLaserPacket; static;
  end;

implementation

class function TLaserFrame.Create(const APoints: TArray<TLaserPoint>; const AFrameId: Int64; const ATimeSeconds: Double): TLaserFrame;
begin
  Result.Points := APoints;
  Result.FrameId := AFrameId;
  Result.TimeSeconds := ATimeSeconds;
end;

class function TLaserPacket.Create(const AFrameId: Int64; const APayload: TBytes): TLaserPacket;
begin
  Result.FrameId := AFrameId;
  Result.Payload := APayload;
end;


end.
