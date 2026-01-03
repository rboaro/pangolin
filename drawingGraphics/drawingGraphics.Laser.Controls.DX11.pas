unit drawingGraphics.Laser.Controls.DX11;

interface

{$IFDEF MSWINDOWS}

uses
  System.Classes, System.Types, System.Diagnostics,
  FMX.Controls, FMX.Types, FMX.Platform.Win,
  Winapi.Windows,

  drawingGraphics.Laser.Domain, drawingGraphics.Laser.Renderer.DX11;

type
  TLaserDX11Control = class(TControl)
  private
    FScene: ILaserScene;
    FRenderer: ILineRenderer;
    FClock: TStopwatch;
    FTimer: TTimer;

    function GetHostHWND: HWND;
    procedure HandleTick(Sender: TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure DoRootChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Scene: ILaserScene read FScene write FScene;
  end;

{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

uses
  System.SysUtils, FMX.Forms;

constructor TLaserDX11Control.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;

  FClock := TStopwatch.StartNew;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 16;
  FTimer.OnTimer := HandleTick;
  FTimer.Enabled := True;
end;

destructor TLaserDX11Control.Destroy;
begin
  FTimer.Enabled := False;
  inherited;
end;

procedure TLaserDX11Control.DoRootChanged;
var
  Host: HWND;
begin
  inherited;
  Host := GetHostHWND;
  if (Host <> 0) and (FRenderer = nil) then
  begin
    FRenderer := TDX11LineRenderer.Create(Host);
    FRenderer.Resize(Round(Width), Round(Height));
  end;
end;

function TLaserDX11Control.GetHostHWND: HWND;
begin
  Result := 0;
  if (Root = nil) or (Root.GetObject is TCustomForm = False) then Exit;
  Result := WindowHandleToPlatform(TCustomForm(Root.GetObject).Handle).Wnd;
end;

procedure TLaserDX11Control.HandleTick(Sender: TObject);
begin
  Repaint;
end;

procedure TLaserDX11Control.Resize;
begin
  inherited;
  if FRenderer <> nil then
    FRenderer.Resize(Round(Width), Round(Height));
end;

procedure TLaserDX11Control.Paint;
var
  T: Double;
  Frame: TLaserFrame;
begin
  inherited;

  if (FRenderer = nil) or (FScene = nil) then
    Exit;

  T := FClock.Elapsed.TotalSeconds;
  Frame := FScene.BuildFrame(TSizeF.Create(Width, Height), T);

  FRenderer.BeginFrame;
  try
    FRenderer.Draw(Frame);
  finally
    FRenderer.EndFrame;
  end;
end;

{$ENDIF}

end.

