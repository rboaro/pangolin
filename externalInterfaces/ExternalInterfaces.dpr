program ExternalInterfaces;

uses
  System.StartUpCopy,
  FMX.Forms,
  externalInterfaces.Main in 'externalInterfaces.Main.pas' {Form2},
  externalInterfaces.LaserBox.Contracts in 'externalInterfaces.LaserBox.Contracts.pas',
  externalInterfaces.LaserBox.Protocol in 'externalInterfaces.LaserBox.Protocol.pas',
  externalInterfaces.LaserBox.Transport.TcpIndy in 'externalInterfaces.LaserBox.Transport.TcpIndy.pas',
  externalInterfaces.LaserBox.Transport.ComPort in 'externalInterfaces.LaserBox.Transport.ComPort.pas',
  externalInterfaces.LaserBox.Session in 'externalInterfaces.LaserBox.Session.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
