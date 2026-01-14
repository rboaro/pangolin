program BirdAnimation;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  Core.Interfaces in 'Core.Interfaces.pas',
  Core.Types in 'Core.Types.pas',
  Core.Renderer in 'Core.Renderer.pas',
  Entities.Bird in 'Entities.Bird.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
