program DragAndDropObject;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form7},
  DragDrop in 'DragDrop.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
