program CloudComputing;

uses
  System.StartUpCopy,
  FMX.Forms,
  cloudComputing.Main in 'cloudComputing.Main.pas' {Form6},
  cloudComputing.HTTP.Example in 'cloudComputing.HTTP.Example.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
