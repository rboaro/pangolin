unit multiThreading.Laser.Projector.Engine;


interface

uses
  System.SysUtils, System.Classes, System.Diagnostics, System.SyncObjs, System.Generics.Collections,
  System.Threading,

  multiThreading.Laser.Domain, multiThreading.Laser.Abstractions, multiThreading.Laser.Progress;

type
  TProjectorConfig = record
    Id: Integer;
    PointsPerSecond: Integer;
  end;

  TProjectorSender = class(TThread)
  private
    FConfig: TProjectorConfig;
    FTransport: IProjectorTransport;
    FQueue: TThreadedQueue<TLaserPacket>;
    FStatsLock: TCriticalSection;
    FLastSentFrameId: Int64;

    FReporter: IProgressReporter;

    procedure Report(const Percent: Single; const Stage: string);
    procedure SimulatePacedProgress(const FromPct, ToPct: Single; const Stage: string; const TotalMs: Integer);
    procedure RateSleep(const PointsInFrame: Integer);
  protected
    procedure Execute; override;
  public
    constructor Create(const Config: TProjectorConfig; const Transport: IProjectorTransport;
                       const QueueCapacity: Integer = 3);
    destructor Destroy; override;

    function Enqueue(const Packet: TLaserPacket): Boolean;

    function LastSentFrameId: Int64;
    property Config: TProjectorConfig read FConfig;
  end;

  TProjectorFleet = class
  private
    FSource: IFrameSource;
    FCodec: IProjectorCodec;
    FSenders: TObjectList<TProjectorSender>;
    FRunning: Boolean;
    FFleetThread: TThread;

    FReporter: IProgressReporter;

    procedure FleetLoop;
  public
    constructor Create(const Source: IFrameSource; const Codec: IProjectorCodec; const Reporter: IProgressReporter);
    destructor Destroy; override;

    procedure AddProjector(const Config: TProjectorConfig; const Transport: IProjectorTransport);
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  System.Math;

{ TProjectorSender }

constructor TProjectorSender.Create(const Config: TProjectorConfig; const Transport: IProjectorTransport;
  const QueueCapacity: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := False;

  FConfig := Config;
  FTransport := Transport;

  FQueue := TThreadedQueue<TLaserPacket>.Create(QueueCapacity, 0, 0);

  FStatsLock := TCriticalSection.Create;

 // Start;
end;

destructor TProjectorSender.Destroy;
begin
  Terminate;
  FQueue.DoShutDown;
  WaitFor;

  FTransport := nil;
  FQueue.Free;
  FStatsLock.Free;
  inherited;
end;

function TProjectorSender.Enqueue(const Packet: TLaserPacket): Boolean;
begin
  Result := (FQueue.PushItem(Packet) = wrSignaled);
end;

function TProjectorSender.LastSentFrameId: Int64;
begin
  FStatsLock.Enter;
  try
    Result := FLastSentFrameId;
  finally
    FStatsLock.Leave;
  end;
end;

procedure TProjectorSender.RateSleep(const PointsInFrame: Integer);
var
  Seconds: Double;
  Ms: Integer;
begin
  if FConfig.PointsPerSecond <= 0 then Exit;

  Seconds := PointsInFrame / FConfig.PointsPerSecond;
  Ms := Max(0, Round(Seconds * 1000));

  if Ms > 0 then
    Sleep(Ms);
end;

procedure TProjectorSender.Report(const Percent: Single; const Stage: string);
begin
  if FReporter <> nil then
    FReporter.Report(FConfig.Id, EnsureRange(Percent, 0, 100), Stage);
end;

procedure TProjectorSender.SimulatePacedProgress(const FromPct, ToPct: Single; const Stage: string; const TotalMs: Integer);
const
  KSteps = 10;
var
  I: Integer;
  P: Single;
  StepMs: Integer;
begin
  if TotalMs <= 0 then Exit;

  StepMs := Max(1, TotalMs div KSteps);
  for I := 0 to KSteps do
  begin
    if Terminated then Exit;
    P := FromPct + (ToPct - FromPct) * (I / KSteps);
    Report(P, Stage);
    Sleep(StepMs);
  end;
end;

procedure TProjectorSender.Execute;
var
  Item: TLaserPacket;
  WaitRes: TWaitResult;
begin
  TThread.NameThreadForDebugging(Format('ProjectorSender-%d', [FConfig.Id]));

  try
    Report(0, 'Connecting...');
    FTransport.Connect;
    Report(10, 'Connected');

    while not Terminated do
    begin
      Report(15, 'Waiting packet...');
      WaitRes := FQueue.PopItem(Item);

      if Terminated then Break;
      if WaitRes <> wrSignaled then
        Continue;

      // Simulated send progression
      Report(70, 'Sending...');
      SimulatePacedProgress(70, 85, 'Sending...', 30 + (FConfig.Id mod 7) * 10);

      if FTransport.Connected then
        FTransport.Send(Item.Payload);

      FStatsLock.Enter;
      try
        FLastSentFrameId := Item.FrameId;
      finally
        FStatsLock.Leave;
      end;

      RateSleep(30000);
    end;

  finally
    try
      FTransport.Disconnect;
    except
    end;
    Report(0, 'Disconnected');
  end;
end;

{ TProjectorFleet }

constructor TProjectorFleet.Create(const Source: IFrameSource; const Codec: IProjectorCodec; const Reporter: IProgressReporter);
begin
  inherited Create;
  FSource := Source;
  FCodec := Codec;
  FReporter := Reporter;
  FSenders := TObjectList<TProjectorSender>.Create(True);
end;

destructor TProjectorFleet.Destroy;
begin
  Stop;
  FSenders.Free;
  inherited;
end;

procedure TProjectorFleet.AddProjector(const Config: TProjectorConfig; const Transport: IProjectorTransport);
begin
  FSenders.Add(TProjectorSender.Create(Config, Transport, 3));
end;

procedure TProjectorFleet.Start;
begin
  if FRunning then Exit;

  FRunning := True;

  FFleetThread := TThread.CreateAnonymousThread(
    procedure
    begin
      FleetLoop;
    end
  );
  FFleetThread.FreeOnTerminate := False;
  FFleetThread.Start;
end;

procedure TProjectorFleet.Stop;
var
  Sender: TProjectorSender;
begin
  if not FRunning then Exit;

  FRunning := False;

  if FFleetThread <> nil then
  begin
    FFleetThread.Terminate;
    FFleetThread.WaitFor;
    FreeAndNil(FFleetThread);
  end;

  for Sender in FSenders do
    Sender.Terminate;
end;

procedure TProjectorFleet.FleetLoop;
var
  Frame: TLaserFrame;
  Tasks: array of ITask;
  I: Integer;
  Sender: TProjectorSender;
begin
  TThread.NameThreadForDebugging('FleetSupervisor');

  while not TThread.CurrentThread.CheckTerminated do
  begin
    Frame := FSource.NextFrame;

    SetLength(Tasks, FSenders.Count);
    for I := 0 to FSenders.Count - 1 do
    begin
      Sender := FSenders[I];

      Tasks[I] := TTask.Run(
        procedure
        var
          Packet: TLaserPacket;
        begin
          // Simulated “heavy” steps per projector (so progress bars move)
          if FReporter <> nil then FReporter.Report(Sender.Config.Id, 20, 'Encoding: transform');
          Sleep(5 + (Sender.Config.Id mod 5));

          if FReporter <> nil then FReporter.Report(Sender.Config.Id, 35, 'Encoding: compress');
          Sleep(6 + (Sender.Config.Id mod 7));

          if FReporter <> nil then FReporter.Report(Sender.Config.Id, 50, 'Encoding: encrypt');
          Sleep(7 + (Sender.Config.Id mod 9));

          Packet := FCodec.Encode(Frame, Sender.Config.Id);

          if FReporter <> nil then FReporter.Report(Sender.Config.Id, 60, 'Encoded');
          Sender.Enqueue(Packet);
        end
      );
    end;

    TTask.WaitForAll(Tasks);

    Sleep(1);
  end;
end;

end.


