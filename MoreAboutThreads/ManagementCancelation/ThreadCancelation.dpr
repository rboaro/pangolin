program ThreadCancelation;

uses
  Vcl.Forms,
  ThreadCancelation.MainForm in 'ThreadCancelation.MainForm.pas' {MainForm},
  ThreadCancelation.SharedData in 'ThreadCancelation.SharedData.pas',
  ThreadCancelation.PausableWorkerThread in 'ThreadCancelation.PausableWorkerThread.pas',
  Thread.Utils in '..\Common\Thread.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
