unit externalInterfaces.LaserBox.Session;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.SyncObjs,
  externalInterfaces.LaserBox.Contracts, externalInterfaces.LaserBox.Protocol;

type
  TLaserBoxSession = class(TInterfacedObject, ILaserBoxSession)
  private
    FTransport: IByteTransport;
    FProtocol: TLaserProtocol;

    FConnected: Boolean;
    FOnLog: TLogEvent;
    FOnTelemetry: TTelemetryEvent;

    FRecvLock: TCriticalSection;

    procedure Log(const S: string);

    procedure HandleBytes(const Data: TBytes);
    procedure HandleConnectedChanged(const Connected: Boolean);

    procedure ProcessPacket(const P: TLaserPacket);
    procedure SendPacket(const Cmd: Byte; const Payload: TBytes);
  public
    constructor Create(const Transport: IByteTransport);
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    function Connected: Boolean;

    procedure SendPing;
    procedure SendFrameChunk(const Payload: TBytes);

    procedure SetOnLog(const Handler: TLogEvent);
    procedure SetOnTelemetry(const Handler: TTelemetryEvent);

  end;

implementation

{ TLaserBoxSession }

constructor TLaserBoxSession.Create(const Transport: IByteTransport);
begin
  inherited Create;

  if Transport = nil then
    raise EArgumentException.Create('Transport is required.');

  FTransport := Transport;
  FProtocol := TLaserProtocol.Create;
  FRecvLock := TCriticalSection.Create;

  FTransport.SetOnBytesReceived(HandleBytes);
  FTransport.SetOnConnectedChanged(HandleConnectedChanged);
  FTransport.SetOnLog(
    procedure(const Msg: string)
    begin
      Log(Msg);
    end
  );
end;

destructor TLaserBoxSession.Destroy;
begin
  Disconnect;
  FRecvLock.Free;
  FProtocol.Free;
  inherited;
end;

procedure TLaserBoxSession.SetOnLog(const Handler: TLogEvent);
begin
  FOnLog := Handler;
end;

procedure TLaserBoxSession.SetOnTelemetry(const Handler: TTelemetryEvent);
begin
  FOnTelemetry := Handler;
end;

procedure TLaserBoxSession.Log(const S: string);
begin
  if Assigned(FOnLog) then
    FOnLog('[SESSION] ' + S);
end;

procedure TLaserBoxSession.Connect;
begin
  if FTransport.IsOpen then Exit;
  FTransport.Open;
end;

procedure TLaserBoxSession.Disconnect;
begin
  if FTransport.IsOpen then
    FTransport.Close;
end;

function TLaserBoxSession.Connected: Boolean;
begin
  Result := FConnected;
end;

procedure TLaserBoxSession.HandleConnectedChanged(const Connected: Boolean);
begin
  FConnected := Connected;
  if Connected then
    Log('Connected')
  else
    Log('Disconnected');
end;

procedure TLaserBoxSession.HandleBytes(const Data: TBytes);
var
  Packets: TArray<TLaserPacket>;
  P: TLaserPacket;
begin
  FRecvLock.Enter;
  try
    FProtocol.PushBytes(Data);
    Packets := FProtocol.PopAllPackets;
  finally
    FRecvLock.Leave;
  end;

  for P in Packets do
    ProcessPacket(P);
end;

procedure TLaserBoxSession.ProcessPacket(const P: TLaserPacket);
var
  S: string;
begin
  case P.Cmd of
    Ord(TLaserCmd.Telemetry):
      begin
        // Example: telemetry payload is UTF-8 text (device-dependent)
        S := TEncoding.UTF8.GetString(P.Payload);
        if Assigned(FOnTelemetry) then
          FOnTelemetry(S);
      end;
  else
    Log(Format('RX cmd=$%.2x len=%d', [P.Cmd, Length(P.Payload)]));
  end;
end;

procedure TLaserBoxSession.SendPacket(const Cmd: Byte; const Payload: TBytes);
var
  Bytes: TBytes;
begin
  Bytes := FProtocol.Encode(TLaserPacket.Create(Cmd, Payload));
  FTransport.Write(Bytes);
end;

procedure TLaserBoxSession.SendPing;
begin
  SendPacket(Ord(TLaserCmd.Ping), nil);
end;

procedure TLaserBoxSession.SendFrameChunk(const Payload: TBytes);
begin
  SendPacket(Ord(TLaserCmd.FrameChunk), Payload);
end;

end.


