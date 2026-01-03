unit cloudComputing.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm6 = class(TForm)
    btnHttp: TButton;
    procedure btnHttpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

uses cloudComputing.HTTP.Example;

procedure TForm6.btnHttpClick(Sender: TObject);
begin
  var Ret := TStringList.Create;
  try
    TTodoClass.getTodo(Ret);
    ShowMessage(Ret.Text);
  finally
    Ret.free;
  end;
end;

end.
