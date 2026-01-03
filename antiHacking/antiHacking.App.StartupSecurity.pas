unit antiHacking.App.StartupSecurity;


interface

uses
  antiHacking.Security.Integrity, antiHacking.Licensing.Service;

type
  TStartupDecision = (Allow, AllowWithWarning, Block);

  TStartupSecurity = class
  private
    FIntegrity: IIntegrityChecker;
    FLicensing: ILicenseService;
  public
    constructor Create(const Integrity: IIntegrityChecker; const Licensing: ILicenseService);
    function Evaluate(out MessageText: string): TStartupDecision;
  end;

implementation

uses
  System.SysUtils;

constructor TStartupSecurity.Create(const Integrity: IIntegrityChecker; const Licensing: ILicenseService);
begin
  inherited Create;
  FIntegrity := Integrity;
  FLicensing := Licensing;
end;

function TStartupSecurity.Evaluate(out MessageText: string): TStartupDecision;
var
  IntRes: TIntegrityResult;
  LicRes: TLicenseStatus;
begin
  Result := Allow;
  MessageText := '';

  IntRes := FIntegrity.VerifySelf;
  LicRes := FLicensing.Check;

  if LicRes <> Valid then
  begin
    MessageText := 'License is not valid.';
    Exit(Block);
  end;

  case IntRes of
    Ok: Result := Allow;
    Unknown:
      begin
        MessageText := 'Integrity not verified (debug/unknown build).';
        Result := AllowWithWarning;
      end;
    Failed:
      begin
        MessageText := 'Integrity check failed. Please reinstall from the official source.';
        Result := Block;
      end;
  end;
end;

end.
