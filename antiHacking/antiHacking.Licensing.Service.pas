unit antiHacking.Licensing.Service;

interface

type
  TLicenseStatus = (Valid, Expired, Invalid);

  ILicenseService = interface
    ['{E4EAE3B6-72A2-4A9C-9F78-CC8B2C7AAB5D}']
    function Check: TLicenseStatus;
  end;

function CreateLicenseService: ILicenseService;

implementation

type
  TLicenseService = class(TInterfacedObject, ILicenseService)
  public
    function Check: TLicenseStatus;
  end;

function CreateLicenseService: ILicenseService;
begin
  Result := TLicenseService.Create;
end;

function TLicenseService.Check: TLicenseStatus;
begin
  Result := Valid;
end;

end.

