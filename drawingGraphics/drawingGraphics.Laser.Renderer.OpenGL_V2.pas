unit drawingGraphics.Laser.Renderer.OpenGL_V2;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.Types;

type
  TLaserOpenGLRendererV2 = class
  private
    FWnd: HWND;
    FDC: HDC;
    FRC: HGLRC;
    FCurrentPos: TPointF;
    procedure SetupOrtho;
    procedure SetupPixelFormat;
  public
    constructor Create(const WindowHandle: HWND);
    destructor Destroy; override;

    procedure BeginFrame;
    procedure EndFrame;

    procedure MoveTo(const X, Y: Single);
    procedure LineTo(const X, Y: Single);

    procedure SetColor(const R, G, B: Single);
    procedure SetLineWidth(const W: Single);
  end;

implementation

{ TLaserOpenGLRenderer }

constructor TLaserOpenGLRendererV2.Create(const WindowHandle: HWND);
begin
  inherited Create;

  FWnd := WindowHandle;
  FDC := GetDC(FWnd);
  SetupPixelFormat;

  FRC := wglCreateContext(FDC);
  wglMakeCurrent(FDC, FRC);

  FCurrentPos := PointF(0, 0);
end;


destructor TLaserOpenGLRendererV2.Destroy;
begin
  if FRC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(FRC);
    FRC := 0;
  end;

  if (FWnd <> 0) and (FDC <> 0) then
  begin
    ReleaseDC(FWnd, FDC);  // ✅ now valid
    FDC := 0;
  end;

  inherited;
end;


procedure TLaserOpenGLRendererV2.SetupPixelFormat;
var
  PFD: TPixelFormatDescriptor;
  PF: Integer;
begin
  FillChar(PFD, SizeOf(PFD), 0);
  PFD.nSize := SizeOf(PFD);
  PFD.nVersion := 1;
  PFD.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  PFD.iPixelType := PFD_TYPE_RGBA;
  PFD.cColorBits := 24;

  PF := ChoosePixelFormat(FDC, @PFD);
  SetPixelFormat(FDC, PF, @PFD);
end;

procedure TLaserOpenGLRendererV2.SetupOrtho;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(-1, 1, -1, 1, -1, 1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TLaserOpenGLRendererV2.BeginFrame;
begin
  wglMakeCurrent(FDC, FRC);

  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT);

  SetupOrtho;

  glDisable(GL_DEPTH_TEST);
  glEnable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
end;

procedure TLaserOpenGLRendererV2.EndFrame;
begin
  glFlush;
  SwapBuffers(FDC);
end;

procedure TLaserOpenGLRendererV2.SetColor(const R, G, B: Single);
begin
  glColor3f(R, G, B);
end;

procedure TLaserOpenGLRendererV2.SetLineWidth(const W: Single);
begin
  glLineWidth(W);
end;

procedure TLaserOpenGLRendererV2.MoveTo(const X, Y: Single);
begin
  FCurrentPos := PointF(X, Y);
end;

procedure TLaserOpenGLRendererV2.LineTo(const X, Y: Single);
begin
  glBegin(GL_LINES);
    glVertex2f(FCurrentPos.X, FCurrentPos.Y);
    glVertex2f(X, Y);
  glEnd;

  FCurrentPos := PointF(X, Y);
end;

end.


