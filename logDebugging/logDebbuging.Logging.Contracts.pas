unit logDebbuging.Logging.Contracts;


interface

type
  TLogLevel = (Trace, Info, Warning, Error, Fatal);

  ILogger = interface
    ['{F6C13D1B-5E93-4E2E-A5A5-0D9E1EFAF5D7}']
    procedure Log(const Level: TLogLevel; const Msg: string);
    procedure EnterMethod(const MethodName: string);
    procedure ExitMethod(const MethodName: string);
  end;

implementation

end.


