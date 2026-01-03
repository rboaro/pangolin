program MultiPlatform;

uses
  System.StartUpCopy,
  FMX.Forms,
  multiPlatform.Main in 'multiPlatform.Main.pas' {Form9};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
