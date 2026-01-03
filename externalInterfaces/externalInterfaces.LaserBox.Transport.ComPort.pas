unit externalInterfaces.LaserBox.Transport.ComPort;


interface

{$IFDEF MSWINDOWS}

uses
  System.SysUtils, System.Classes,
  externalInterfaces.LaserBox.Contracts,

  ComPort;

type
  TComPortTransport = class(TInterfacedObject, IByteTransport)
  private
    FPort: TComPort;

    FOnBytes: TBytesReceivedEvent;
    FOnConn: TConnectedChangedEvent;
    FOnLog: TLogEvent;

    procedure Log(const S: string);
    procedure HandleRxChar(Sender: TObject);
  public
    constructor Create(const ComName: string; const BaudRate: Integer = 115200);
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    function IsOpen: Boolean;

    procedure Write(const Data: TBytes);

    procedure SetOnBytesReceived(const Handler: TBytesReceivedEvent);
    procedure SetOnConnectedChanged(const Handler: TConnectedChangedEvent);
    procedure SetOnLog(const Handler: TLogEvent);
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

{ TComPortTransport }

constructor TComPortTransport.Create(const ComName: string; const BaudRate: Integer);
begin
  inherited Create;

  FPort := TComPort.Create(nil);
  FPort.DeviceName := ComName;

  FPort.BaudRate := TBaudRate(BaudRate);
  FPort.DataBits := db8;
  FPort.StopBits := sb1;

  FPort.OnRxChar := HandleRxChar;
end;

destructor TComPortTransport.Destroy;
begin
  Close;
  FPort.Free;
  inherited;
end;

procedure TComPortTransport.Log(const S: string);
begin
  if Assigned(FOnLog) then
    FOnLog('[COM] ' + S);
end;

procedure TComPortTransport.Open;
begin
  if IsOpen then Exit;
  Log('Opening ' + FPort.DeviceName);
  FPort.Open;
  if Assigned(FOnConn) then
    FOnConn(True);
end;

procedure TComPortTransport.Close;
begin
  if IsOpen then
  begin
    FPort.Close;
    if Assigned(FOnConn) then
      FOnConn(False);
    Log('Closed');
  end;
end;

function TComPortTransport.IsOpen: Boolean;
begin
  Result := (FPort <> nil) and FPort.Active;
end;

procedure TComPortTransport.Write(const Data: TBytes);
begin
  if not IsOpen then
    raise ELaserCommError.Create('COM transport not open.');

  if Length(Data) > 0 then
    FPort.Write(@Data[0], length(Data));
end;

procedure TComPortTransport.HandleRxChar(Sender: TObject);
var
  Available: Integer;
  Str: String;
begin
  Available := FPort.InputCount;

  if Available <= 0 then
    Exit;

  Str := FPort.ReadString;
end;

procedure TComPortTransport.SetOnBytesReceived(const Handler: TBytesReceivedEvent);
begin
  FOnBytes := Handler;
end;

procedure TComPortTransport.SetOnConnectedChanged(const Handler: TConnectedChangedEvent);
begin
  FOnConn := Handler;
end;

procedure TComPortTransport.SetOnLog(const Handler: TLogEvent);
begin
  FOnLog := Handler;
end;

{$ENDIF}

end.


