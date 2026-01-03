unit logDebbuging.Laser.Controller;


interface

uses
  System.SysUtils,
  logDebbuging.Logging.Contracts;

type
  TLaserController = class
  private
    FLogger: ILogger;
  public
    constructor Create(const Logger: ILogger);
    procedure InitializeHardware;
    procedure SendFrame;
  end;

implementation

constructor TLaserController.Create(const Logger: ILogger);
begin
  inherited Create;
  FLogger := Logger;
end;

procedure TLaserController.InitializeHardware;
begin
  FLogger.EnterMethod('InitializeHardware');
  try
    FLogger.Log(Info, 'Initializing laser hardware');
    Sleep(50);
  finally
    FLogger.ExitMethod('InitializeHardware');
  end;
end;

procedure TLaserController.SendFrame;
begin
  FLogger.EnterMethod('SendFrame');
  try
    FLogger.Log(Info, 'Sending frame to projector');
    Sleep(20);
  except
    on E: Exception do
    begin
      FLogger.Log(Error, 'SendFrame failed: ' + E.Message);
      raise;
    end;
  end;
  FLogger.ExitMethod('SendFrame');
end;

end.

