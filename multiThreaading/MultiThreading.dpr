program MultiThreading;

uses
  System.StartUpCopy,
  FMX.Forms,
  multiThreading.Main in 'multiThreading.Main.pas' {frmMultiThreadingMain},
  multiThreading.Laser.Domain in 'multiThreading.Laser.Domain.pas',
  multiThreading.Laser.Abstractions in 'multiThreading.Laser.Abstractions.pas',
  multiThreading.Laser.FrameSource.Demo in 'multiThreading.Laser.FrameSource.Demo.pas',
  multiThreading.Laser.Codec.Demo in 'multiThreading.Laser.Codec.Demo.pas',
  multiThreading.Laser.Projector.Engine in 'multiThreading.Laser.Projector.Engine.pas',
  multiThreading.Laser.Transport.TcpStub in 'multiThreading.Laser.Transport.TcpStub.pas',
  multiThreading.Laser.Progress in 'multiThreading.Laser.Progress.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMultiThreadingMain, frmMultiThreadingMain);
  Application.Run;
end.
