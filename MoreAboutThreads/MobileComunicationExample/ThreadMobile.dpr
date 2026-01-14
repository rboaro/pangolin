program ThreadMobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  ThreadMobile.MainForm in 'ThreadMobile.MainForm.pas' {MainForm},
  Thread.Utils in '..\Common\Thread.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
