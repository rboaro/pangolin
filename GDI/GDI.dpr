program GDI;

uses
  Vcl.Forms,
  Pangolin.GDI in 'Pangolin.GDI.pas' {Form1},
  GDI.Core.Contracts in 'GDI.Core.Contracts.pas',
  GDI.Infra.CanvasAdapter in 'GDI.Infra.CanvasAdapter.pas',
  GDI.Domain.Shapes in 'GDI.Domain.Shapes.pas',
  GDI.Test.Shapes in 'GDI.Test.Shapes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
