program ThreadAdvanced;

uses
  Vcl.Forms,
  Thread.MainForm in 'Thread.MainForm.pas' {MainForm},
  Thread.SimpleThreadPool in 'Thread.SimpleThreadPool.pas',
  Thread.Utils in '..\Common\Thread.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
