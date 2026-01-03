program LogDebbuging;

uses
  System.StartUpCopy,
  FMX.Forms,
  logDebbuging.Main in 'logDebbuging.Main.pas' {Form5},
  logDebbuging.Logging.Contracts in 'logDebbuging.Logging.Contracts.pas',
  logDebbuging.Laser.Controller in 'logDebbuging.Laser.Controller.pas',
  System.SysUtils,
  logDebbuging.Logging.FileEncrypted in 'logDebbuging.Logging.FileEncrypted.pas';

{$R *.res}

var
  Logger : ILogger;

begin

  Logger := TEncryptedFileLogger.Create;
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  try
    Application.Run;
  except
    on E: Exception do
    begin
      Logger.Log(Fatal, 'Unhandled exception: ' + E.ClassName + ' - ' + E.Message);
      raise; // MadExcept intercepts here
    end;
end;

end.
