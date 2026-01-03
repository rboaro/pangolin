unit multiThreading.Laser.Progress;

interface

type
  IProgressReporter = interface
    ['{B2E3D1B0-8B05-4EAD-8B3A-DC1C3A1B606E}']
    procedure Report(const ProjectorId: Integer; const Percent: Single; const Stage: string);
  end;

implementation

end.


