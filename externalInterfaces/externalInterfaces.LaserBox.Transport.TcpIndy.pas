unit externalInterfaces.LaserBox.Transport.TcpIndy;

interface

{$IFDEF MSWINDOWS}

uses
  System.SysUtils, System.Classes, System.SyncObjs,
  IdTCPClient, IdGlobal,

  externalInterfaces.LaserBox.Contracts;

type
  TTcpIndyTransport = class(TInterfacedObject, IByteTransport)
  private
    FClient: TIdTCPClient;
    FReader: TThread;
    FLock: TCriticalSection;

    FOnBytes: TBytesReceivedEvent;
    FOnConn: TConnectedChangedEvent;
    FOnLog: TLogEvent;

    procedure Log(const S: string);
    procedure SetConnected(const V: Boolean);

    procedure ReaderLoop;
  public
    constructor Create(const Host: string; const Port: Integer);
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

{ TTcpIndyTransport }

constructor TTcpIndyTransport.Create(const Host: string; const Port: Integer);
begin
  inherited Create;
  FLock := TCriticalSection.Create;

  FClient := TIdTCPClient.Create(nil);
  FClient.Host := Host;
  FClient.Port := Port;
  FClient.ReadTimeout := 250; // short poll
end;

destructor TTcpIndyTransport.Destroy;
begin
  Close;
  FClient.Free;
  FLock.Free;
  inherited;
end;

procedure TTcpIndyTransport.Log(const S: string);
begin
  if Assigned(FOnLog) then
    FOnLog('[TCP] ' + S);
end;

procedure TTcpIndyTransport.SetConnected(const V: Boolean);
begin
  if Assigned(FOnConn) then
    FOnConn(V);
end;

procedure TTcpIndyTransport.Open;
begin
  if IsOpen then Exit;

  Log(Format('Connecting to %s:%d ...', [FClient.Host, FClient.Port]));
  FClient.Connect;
  SetConnected(True);

  FReader := TThread.CreateAnonymousThread(
    procedure
    begin
      ReaderLoop;
    end
  );
  FReader.FreeOnTerminate := False;
  FReader.Start;
end;

procedure TTcpIndyTransport.Close;
begin
  if FReader <> nil then
  begin
    FReader.Terminate;
    FReader.WaitFor;
    FreeAndNil(FReader);
  end;

  if FClient.Connected then
  begin
    FClient.Disconnect;
    SetConnected(False);
    Log('Disconnected');
  end;
end;

function TTcpIndyTransport.IsOpen: Boolean;
begin
  Result := (FClient <> nil) and FClient.Connected;
end;

procedure TTcpIndyTransport.Write(const Data: TBytes);
begin
  if not IsOpen then
    raise ELaserCommError.Create('TCP transport not connected.');

  FLock.Enter;
  try
    FClient.IOHandler.Write(TIdBytes(Data));
  finally
    FLock.Leave;
  end;
end;

procedure TTcpIndyTransport.ReaderLoop;
var
  Buf: TIdBytes;
  ReadCount: Integer;
  OutBytes: TBytes;
begin
  SetLength(Buf, 4096);

  try
    while not TThread.CurrentThread.CheckTerminated do
    begin
      if not IsOpen then Break;

      ReadCount := 0;
      try
        FClient.IOHandler.ReadBytes(Buf, 1, False); // non-block-ish
        ReadCount := FClient.IOHandler.ReadByte;
      except
        on E: Exception do
        begin
          Log('Read error: ' + E.Message);
          Break;
        end;
      end;

      if ReadCount > 0 then
      begin
        SetLength(OutBytes, ReadCount);
        Move(Buf[0], OutBytes[0], ReadCount);

        if Assigned(FOnBytes) then
          FOnBytes(OutBytes);
      end
      else
        Sleep(1);
    end;
  finally
    if IsOpen then
      try
        FClient.Disconnect;
      except
      end;
    SetConnected(False);
  end;
end;

procedure TTcpIndyTransport.SetOnBytesReceived(const Handler: TBytesReceivedEvent);
begin
  FOnBytes := Handler;
end;

procedure TTcpIndyTransport.SetOnConnectedChanged(const Handler: TConnectedChangedEvent);
begin
  FOnConn := Handler;
end;

procedure TTcpIndyTransport.SetOnLog(const Handler: TLogEvent);
begin
  FOnLog := Handler;
end;

{$ENDIF}

end.


