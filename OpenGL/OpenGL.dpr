program OpenGL;

uses
  Vcl.Forms,
  Pangolin.OpenGL in '..\..\..\..\embarcadero\Studio\Projects\Pangolin.OpenGL.pas' {Form3},
  OpenGL.Core.Contracts in '..\..\..\..\embarcadero\Studio\Projects\OpenGL.Core.Contracts.pas',
  OpenGL.Infra.Context in '..\..\..\..\embarcadero\Studio\Projects\OpenGL.Infra.Context.pas',
  OpenGL.Domain.Entities in '..\..\..\..\embarcadero\Studio\Projects\OpenGL.Domain.Entities.pas',
  OpenGL.Domain.Laser in '..\..\..\..\embarcadero\Studio\Projects\OpenGL.Domain.Laser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
