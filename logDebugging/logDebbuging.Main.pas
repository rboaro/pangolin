unit logDebbuging.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  logDebbuging.Logging.FileEncrypted;

type
  TForm5 = class(TForm)
    Memo1: TMemo;
    btnLoadLog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadLogClick(Sender: TObject);
  private
    FLogger: TEncryptedFileLogger;
  public
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.btnLoadLogClick(Sender: TObject);
var
  Text: string;
begin
  Text := (FLogger as TEncryptedFileLogger).ReadDecryptedSnapshot;
  Memo1.Lines.Text := Text;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin
  FLogger := TEncryptedFileLogger.Create;
//  raise Exception.Create('TEST: simulated startup failure in FormCreate');
end;

end.
