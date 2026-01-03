unit externalInterfaces.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, ComPort,

  externalInterfaces.LaserBox.Contracts, externalInterfaces.LaserBox.Session

  {$IFDEF MSWINDOWS}
  , externalInterfaces.LaserBox.Transport.ComPort
  , externalInterfaces.LaserBox.Transport.TcpIndy, FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo
  {$ENDIF}

  ;


type
  TForm2 = class(TForm)
    Memo1: TMemo;
    btnConnect: TButton;
    btnPing: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPingClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    FSession: ILaserBoxSession;
    FConnected: Boolean;

    procedure Log(const S: string);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.btnConnectClick(Sender: TObject);
begin
  if not FConnected then
  begin
    FSession.Connect;
    FConnected := True;
    btnConnect.Text := 'Disconnect';
  end
  else
  begin
    FSession.Disconnect;
    FConnected := False;
    btnConnect.Text := 'Connect';
  end;
end;

procedure TForm2.btnPingClick(Sender: TObject);
begin
  FSession.SendPing;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  Transport: IByteTransport;
begin
  // 1) Ethernet box via TCP
  Transport := TTcpIndyTransport.Create('192.168.0.12', 7765);  // my MacOs machine

  // 2) USB box as Virtual COM Port
  // Transport := TComPortTransport.Create('COM5', 115200);

  FSession := TLaserBoxSession.Create(Transport);

  FSession.SetOnLog(
    procedure(const Msg: string)
    begin
      TThread.Queue(nil,
        procedure
        begin
          Log(Msg);
        end
      );
    end
  );

  FSession.SetOnTelemetry(
    procedure(const Telemetry: string)
    begin
      TThread.Queue(nil,
        procedure
        begin
          Log('[TEL] ' + Telemetry);
        end
      );
    end
  );
end;

procedure TForm2.Log(const S: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' ' + S);
end;

end.
