program ThreadPPL;

uses
  Vcl.Forms,
  ThreadPPL.MainForm in 'ThreadPPL.MainForm.pas' {MainForm},
  Thread.Utils in '..\Common\Thread.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
