unit externalInterfaces.LaserBox.Contracts;

interface

uses
  System.SysUtils, System.Classes;

type
  ELaserCommError = class(Exception);

  TBytesReceivedEvent = reference to procedure(const Data: TBytes);
  TConnectedChangedEvent = reference to procedure(const Connected: Boolean);
  TLogEvent = reference to procedure(const Msg: string);
  TTelemetryEvent = reference to procedure(const Telemetry: string);


  IByteTransport = interface
    ['{D3561B65-3AFB-4A08-9A8F-9F2C9B7DB7F2}']
    procedure Open;
    procedure Close;
    function IsOpen: Boolean;

    procedure Write(const Data: TBytes);

    procedure SetOnBytesReceived(const Handler: TBytesReceivedEvent);
    procedure SetOnConnectedChanged(const Handler: TConnectedChangedEvent);
    procedure SetOnLog(const Handler: TLogEvent);
  end;

  ILaserBoxSession = interface
    ['{79A6C7D5-9A9E-4D2C-A850-0D4F67C7D8A2}']
    procedure Connect;
    procedure Disconnect;
    function Connected: Boolean;

    procedure SendPing;
    procedure SendFrameChunk(const Payload: TBytes);

    procedure SetOnLog(const Handler: TLogEvent);
    procedure SetOnTelemetry(const Handler: TTelemetryEvent);

end;

implementation
end.


