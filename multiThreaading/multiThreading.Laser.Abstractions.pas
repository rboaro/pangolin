unit multiThreading.Laser.Abstractions;


interface

uses
  System.SysUtils,
  multiThreading.Laser.Domain;

type
  IFrameSource = interface
    ['{E6B2A6C2-B60F-4B5E-9EAB-9A0E0B3FAE57}']
    function NextFrame: TLaserFrame;
  end;

  IProjectorTransport = interface
    ['{8C2E79F5-1C09-4F1C-A0B3-4A0A66E1F6D1}']
    procedure Connect;
    procedure Disconnect;
    function Connected: Boolean;
    procedure Send(const Data: TBytes);
  end;

  IProjectorCodec = interface
    ['{D6B24A2F-2C3C-4B2A-9E9B-62B0B4C0F2B3}']
    function Encode(const Frame: TLaserFrame; const ProjectorId: Integer): TLaserPacket;
  end;

implementation

end.

