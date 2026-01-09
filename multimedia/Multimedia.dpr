program Multimedia;

uses
  System.StartUpCopy,
  FMX.Forms,
  multimedia.Main in 'multimedia.Main.pas' {Form10};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
