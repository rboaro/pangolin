unit multiThreading.Laser.Transport.TcpStub;


interface

uses
  System.SysUtils,

  multiThreading.Laser.Abstractions;

type
  TTcpStubTransport = class(TInterfacedObject, IProjectorTransport)
  private
    FConnected: Boolean;
    FHost: string;
    FPort: Integer;
  public
    constructor Create(const Host: string; const Port: Integer);

    procedure Connect;
    procedure Disconnect;
    function Connected: Boolean;
    procedure Send(const Data: TBytes);
  end;

implementation

constructor TTcpStubTransport.Create(const Host: string; const Port: Integer);
begin
  inherited Create;
  FHost := Host;
  FPort := Port;
end;

procedure TTcpStubTransport.Connect;
begin
  FConnected := True;
end;

procedure TTcpStubTransport.Disconnect;
begin
  FConnected := False;
end;

function TTcpStubTransport.Connected: Boolean;
begin
  Result := FConnected;
end;

procedure TTcpStubTransport.Send(const Data: TBytes);
begin
  if not FConnected then
    Exit;
end;

end.


