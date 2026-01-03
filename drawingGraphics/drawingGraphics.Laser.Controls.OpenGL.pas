unit drawingGraphics.Laser.Controls.OpenGL;

interface

{$IFDEF MSWINDOWS}

uses
  System.Classes, System.Types, System.Diagnostics,
  FMX.Controls, FMX.Types, FMX.Platform.Win,
  Winapi.Windows,

  drawingGraphics.Laser.Domain, drawingGraphics.Laser.Renderer.OpenGL;

type
  TLaserOpenGLControl = class(TControl)
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

constructor TLaserOpenGLControl.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;

  FClock := TStopwatch.StartNew;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 16; // ~60 fps
  FTimer.OnTimer := HandleTick;
  FTimer.Enabled := True;
end;

destructor TLaserOpenGLControl.Destroy;
begin
  FTimer.Enabled := False;
  inherited;
end;

procedure TLaserOpenGLControl.DoRootChanged;
var
  Host: HWND;
begin
  inherited;

  Host := GetHostHWND;

  if (Host <> 0) and (FRenderer = nil) then
  begin
    FRenderer := TOpenGLLineRenderer.Create(Host);
    FRenderer.Resize(Round(Width), Round(Height));
  end;
end;

function TLaserOpenGLControl.GetHostHWND: HWND;
var
  Wnd: HWND;
begin
  Result := 0;

  if (Root = nil) or (Root.GetObject is TCustomForm = False) then Exit;

  Wnd := WindowHandleToPlatform(TCustomForm(Root.GetObject).Handle).Wnd;
  Result := Wnd;
end;

procedure TLaserOpenGLControl.HandleTick(Sender: TObject);
begin
  Repaint;
end;

procedure TLaserOpenGLControl.Resize;
begin
  inherited;

  if FRenderer <> nil then
    FRenderer.Resize(Round(Width), Round(Height));
end;

procedure TLaserOpenGLControl.Paint;
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

