program OpenGL;

uses
  Vcl.Forms,
  Pangolin.OpenGL in 'Pangolin.OpenGL.pas' {Form3},
  OpenGL.Core.Contracts in 'OpenGL.Core.Contracts.pas',
  OpenGL.Infra.Context in 'OpenGL.Infra.Context.pas',
  OpenGL.Domain.Entities in 'OpenGL.Domain.Entities.pas',
  OpenGL.Domain.Laser in 'OpenGL.Domain.Laser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
