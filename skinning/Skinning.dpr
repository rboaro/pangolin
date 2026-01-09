program Skinning;

uses
  System.StartUpCopy,
  FMX.Forms,
  skinning.Main in 'skinning.Main.pas' {Form8};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm8, Form8);
  Application.Run;
end.
