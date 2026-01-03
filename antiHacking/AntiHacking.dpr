program AntiHacking;

uses
  System.StartUpCopy,
  FMX.Forms,
  antiHacking.Main in 'antiHacking.Main.pas' {Form4},
  antiHacking.Security.Config in 'antiHacking.Security.Config.pas',
  antiHacking.Security.Integrity in 'antiHacking.Security.Integrity.pas',
  antiHacking.Licensing.Service in 'antiHacking.Licensing.Service.pas',
  antiHacking.App.StartupSecurity in 'antiHacking.App.StartupSecurity.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
