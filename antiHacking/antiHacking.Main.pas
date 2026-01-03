unit antiHacking.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  antiHacking.App.StartupSecurity, antiHacking.Security.Integrity, antiHacking.Licensing.Service;

type
  TForm4 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.FormCreate(Sender: TObject);
var
  Sec: TStartupSecurity;
  Msg: string;
  Decision: TStartupDecision;
begin
  Sec := TStartupSecurity.Create(CreateIntegrityChecker, CreateLicenseService);
  try
    Decision := Sec.Evaluate(Msg);

    if (Msg <> '') then
      ShowMessage(Msg);

    if Decision = Block then
    begin
      Application.Terminate;
      Exit;
    end;
  finally
    Sec.Free;
  end;
end;

end.
