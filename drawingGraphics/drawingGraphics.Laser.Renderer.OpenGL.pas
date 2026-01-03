unit drawingGraphics.Laser.Renderer.OpenGL;

interface

{$IFDEF MSWINDOWS}

uses
  System.SysUtils, System.Types, System.UITypes,
  Winapi.Windows, Winapi.OpenGL,

  drawingGraphics.Laser.Domain;

type
  TOpenGLLineRenderer = class(TInterfacedObject, ILineRenderer)
  private
    FWnd: HWND;
    FDC: HDC;
    FRC: HGLRC;
    FWidth: Integer;
    FHeight: Integer;

    procedure CreateContext;
    procedure DestroyContext;
    procedure SetupPixelFormat;
    procedure ApplyViewport;
    procedure SetOrtho2D;
    class function ColorToRGBAf(const C: TAlphaColor; out R,G,B,A: Single): Boolean; static;
  public
    constructor Create(const ATargetWindow: HWND);
    destructor Destroy; override;

    procedure Resize(const AWidth, AHeight: Integer);
    procedure BeginFrame;
    procedure Draw(const AFrame: TLaserFrame);
    procedure EndFrame;
  end;

{$ENDIF}

implementation

uses
  System.Math;

{$IFDEF MSWINDOWS}

{ TOpenGLLineRenderer }

constructor TOpenGLLineRenderer.Create(const ATargetWindow: HWND);
begin
  inherited Create;

  if ATargetWindow = 0 then
    raise EArgumentException.Create('OpenGL renderer requires a valid HWND.');

  FWnd := ATargetWindow;
  FDC := GetDC(FWnd);

  if FDC = 0 then
    RaiseLastOSError;

  CreateContext;
end;

destructor TOpenGLLineRenderer.Destroy;
begin
  DestroyContext;

  if (FDC <> 0) and (FWnd <> 0) then
    ReleaseDC(FWnd, FDC);

  inherited;
end;

procedure TOpenGLLineRenderer.CreateContext;
begin
  SetupPixelFormat;

  FRC := wglCreateContext(FDC);
  if FRC = 0 then
    RaiseLastOSError;

  if not wglMakeCurrent(FDC, FRC) then
    RaiseLastOSError;

  glDisable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
end;

procedure TOpenGLLineRenderer.DestroyContext;
begin
  if FRC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(FRC);
    FRC := 0;
  end;
end;

procedure TOpenGLLineRenderer.SetupPixelFormat;
var
  PFD: TPixelFormatDescriptor;
  PixelFormat: Integer;
begin
  FillChar(PFD, SizeOf(PFD), 0);
  PFD.nSize := SizeOf(PFD);
  PFD.nVersion := 1;
  PFD.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  PFD.iPixelType := PFD_TYPE_RGBA;
  PFD.cColorBits := 32;
  PFD.cAlphaBits := 8;
  PFD.cDepthBits := 0;
  PFD.iLayerType := PFD_MAIN_PLANE;

  PixelFormat := ChoosePixelFormat(FDC, @PFD);
  if PixelFormat = 0 then
    RaiseLastOSError;

  if not SetPixelFormat(FDC, PixelFormat, @PFD) then
    RaiseLastOSError;
end;

procedure TOpenGLLineRenderer.Resize(const AWidth, AHeight: Integer);
begin
  FWidth := Max(1, AWidth);
  FHeight := Max(1, AHeight);
  ApplyViewport;
end;

procedure TOpenGLLineRenderer.ApplyViewport;
begin
  if (FRC = 0) then Exit;
  if not wglMakeCurrent(FDC, FRC) then
    RaiseLastOSError;

  glViewport(0, 0, FWidth, FHeight);
  SetOrtho2D;
end;

procedure TOpenGLLineRenderer.SetOrtho2D;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, FWidth, FHeight, 0, -1, 1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

class function TOpenGLLineRenderer.ColorToRGBAf(const C: TAlphaColor; out R, G, B, A: Single): Boolean;
var
  AA, RR, GG, BB: Byte;
begin
  AA := (C shr 24) and $FF;
  RR := (C shr 16) and $FF;
  GG := (C shr 8) and $FF;
  BB := (C) and $FF;

  A := AA / 255;
  R := RR / 255;
  G := GG / 255;
  B := BB / 255;
  Result := True;
end;

procedure TOpenGLLineRenderer.BeginFrame;
begin
  if not wglMakeCurrent(FDC, FRC) then
    RaiseLastOSError;

  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT);
end;

procedure TOpenGLLineRenderer.Draw(const AFrame: TLaserFrame);
var
  L: TLaserLine;
  R, G, B, A: Single;
begin
  for L in AFrame.Lines do
  begin
    ColorToRGBAf(L.Color, R, G, B, A);
    glColor4f(R, G, B, A);

    glLineWidth(Max(1.0, L.Thickness));
    glBegin(GL_LINES);
      glVertex2f(L.A.X, L.A.Y);
      glVertex2f(L.B.X, L.B.Y);
    glEnd;
  end;
end;

procedure TOpenGLLineRenderer.EndFrame;
begin
  SwapBuffers(FDC);
end;

{$ENDIF}

end.

