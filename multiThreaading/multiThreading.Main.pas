unit multiThreading.Main;

interface

uses
  System.Classes, System.SysUtils, System.UITypes,
  FMX.Forms, FMX.Types, FMX.Controls, FMX.Layouts, FMX.StdCtrls, FMX.Objects,
  FMX.Controls.Presentation,

  multiThreading.Laser.Abstractions, multiThreading.Laser.Domain,
  multiThreading.Laser.FrameSource.Demo, multiThreading.Laser.Codec.Demo,
  multiThreading.Laser.Projector.Engine, multiThreading.Laser.Transport.TcpStub,
  multiThreading.Laser.Progress;

type
  TfrmMultiThreadingMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFleet: TProjectorFleet;

    FScroll: TVertScrollBox;
    FBars: array of TProgressBar;
    FStages: array of TLabel;

    type
      TUiProgressReporter = class(TInterfacedObject, IProgressReporter)
      private
        FOwner: TfrmMultiThreadingMain;
      public
        constructor Create(AOwner: TfrmMultiThreadingMain);
        procedure Report(const ProjectorId: Integer; const Percent: Single; const Stage: string);
      end;

    procedure BuildProjectorUI(const Count: Integer);
    procedure UiSetProgress(const ProjectorId: Integer; const Percent: Single; const Stage: string);
  public
  end;

var
  frmMultiThreadingMain: TfrmMultiThreadingMain;

implementation

{$R *.fmx}

procedure TfrmMultiThreadingMain.BuildProjectorUI(const Count: Integer);
var
  I: Integer;
  Row: TLayout;
  Lbl: TLabel;
  Bar: TProgressBar;
  Stage: TLabel;
begin
  FScroll := TVertScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.Align := TAlignLayout.Client;

  SetLength(FBars, Count);
  SetLength(FStages, Count);

  for I := 0 to Count - 1 do
  begin
    Row := TLayout.Create(FScroll);
    Row.Parent := FScroll;
    Row.Align := TAlignLayout.Top;
    Row.Height := 42;
    Row.Padding.Left := 10;
    Row.Padding.Right := 10;
    Row.Padding.Top := 6;
    Row.Padding.Bottom := 6;

    Lbl := TLabel.Create(Row);
    Lbl.Parent := Row;
    Lbl.Align := TAlignLayout.Left;
    Lbl.Width := 110;
    Lbl.Text := Format('Projector %d', [I]);
    Lbl.VertTextAlign := TTextAlign.Center;

    Bar := TProgressBar.Create(Row);
    Bar.Parent := Row;
    Bar.Align := TAlignLayout.Client;
    Bar.Min := 0;
    Bar.Max := 100;
    Bar.Value := 0;

    Stage := TLabel.Create(Row);
    Stage.Parent := Row;
    Stage.Align := TAlignLayout.Right;
    Stage.Width := 170;
    Stage.Text := 'Idle';
    Stage.VertTextAlign := TTextAlign.Center;

    FBars[I] := Bar;
    FStages[I] := Stage;
  end;
end;

procedure TfrmMultiThreadingMain.FormCreate(Sender: TObject);
const
  KProjectors = 40;
var
  Source: IFrameSource;
  Codec: IProjectorCodec;
  Reporter: IProgressReporter;
  I: Integer;
  Cfg: TProjectorConfig;
  Transport: IProjectorTransport;
begin
  Width := 980;
  Height := 720;
  Caption := 'Multi-Projector Threading Demo (Progress per Thread)';

  BuildProjectorUI(KProjectors);

  Source := TDemoFrameSource.Create;
  Codec := TDemoProjectorCodec.Create;
  Reporter := TUiProgressReporter.Create(Self);

  FFleet := TProjectorFleet.Create(Source, Codec, Reporter);

  for I := 0 to KProjectors - 1 do
  begin
    Cfg.Id := I;
    Cfg.PointsPerSecond := 250000 + (I mod 9) * 20000; // per-projector consumption rate

    Transport := TTcpStubTransport.Create('192.168.0.' + IntToStr(100 + I), 7765);
    FFleet.AddProjector(Cfg, Transport);
  end;

  FFleet.Start;
end;

procedure TfrmMultiThreadingMain.FormDestroy(Sender: TObject);
begin
  FFleet.Free;
end;

procedure TfrmMultiThreadingMain.UiSetProgress(const ProjectorId: Integer; const Percent: Single; const Stage: string);
begin
  if (ProjectorId < 0) or (ProjectorId > High(FBars)) then Exit;
  FBars[ProjectorId].Value := Percent;
  FStages[ProjectorId].Text := Stage;
end;

{ TfrmMultiThreadingMain.TUiProgressReporter }

constructor TfrmMultiThreadingMain.TUiProgressReporter.Create(AOwner: TfrmMultiThreadingMain);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TfrmMultiThreadingMain.TUiProgressReporter.Report(const ProjectorId: Integer; const Percent: Single; const Stage: string);
begin
  // Transport/codec threads call this => marshal to UI thread
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOwner) then
        FOwner.UiSetProgress(ProjectorId, Percent, Stage);
    end
  );
end;

end.
