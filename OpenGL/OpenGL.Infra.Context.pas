unit OpenGL.Infra.Context;

interface

uses
  Winapi.Windows, Winapi.OpenGL, Vcl.Controls, System.UITypes, OpenGL.Core.Contracts;

type
  TOpenGLContext = class(TInterfacedObject, IRenderContext)
  strict private
    FOwner: TWinControl;
    FDC: HDC;
    FRC: HGLRC;
    procedure SetupPixelFormat;
    function AlphaToColorRef(const AColor: TAlphaColor): TColorRef;
  public
    constructor Create(const AOwner: TWinControl);
    destructor Destroy; override;

    procedure Initialize;
    procedure Resize(const AWidth, AHeight: Integer);
    procedure BeginScene(const ABackgroundColor: TAlphaColor);
    procedure EndScene;

    procedure SetLineWidth(const AWidth: Single);
    procedure DrawLine(const X1, Y1, Z1, X2, Y2, Z2: Single);

    procedure SetColor(const AColor: TAlphaColor);
    procedure DrawCube(const ASize: Single);
    procedure Rotate(const AAngle: Single; const X, Y, Z: Single);
    procedure Translate(const X, Y, Z: Single);
    procedure PushMatrix;
    procedure PopMatrix;
  end;

implementation

uses
  System.SysUtils;

constructor TOpenGLContext.Create(const AOwner: TWinControl);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TOpenGLContext.Destroy;
begin
  if FRC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(FRC);
  end;
  if FDC <> 0 then
    ReleaseDC(FOwner.Handle, FDC);
  inherited;
end;

procedure TOpenGLContext.Initialize;
begin
  FDC := GetDC(FOwner.Handle);
  SetupPixelFormat;
  FRC := wglCreateContext(FDC);
  if FRC = 0 then
    RaiseLastOSError;

  if not wglMakeCurrent(FDC, FRC) then
    RaiseLastOSError;

  glEnable(GL_DEPTH_TEST); // Enable Z-Buffer
  glEnable(GL_CULL_FACE);  // Optimize rendering
end;

procedure TOpenGLContext.SetupPixelFormat;
var
  LPixelFormat: Integer;
  LPFD: PIXELFORMATDESCRIPTOR;
begin
  FillChar(LPFD, SizeOf(PIXELFORMATDESCRIPTOR), 0);
  with LPFD do
  begin
    nSize := SizeOf(PIXELFORMATDESCRIPTOR);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cDepthBits := 16; // 16-bit Z-Buffer
    iLayerType := PFD_MAIN_PLANE;
  end;

  LPixelFormat := ChoosePixelFormat(FDC, @LPFD);
  if LPixelFormat = 0 then RaiseLastOSError;

  if not SetPixelFormat(FDC, LPixelFormat, @LPFD) then RaiseLastOSError;
end;

procedure TOpenGLContext.Resize(const AWidth, AHeight: Integer);
begin
  if AHeight <= 0 then Exit;
  glViewport(0, 0, AWidth, AHeight);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  // Simple Perspective
  gluPerspective(45.0, AWidth / AHeight, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TOpenGLContext.BeginScene(const ABackgroundColor: TAlphaColor);
var
  R, G, B: Single;
begin
  R := TAlphaColorRec(ABackgroundColor).R / 255;
  G := TAlphaColorRec(ABackgroundColor).G / 255;
  B := TAlphaColorRec(ABackgroundColor).B / 255;

  glClearColor(R, G, B, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
end;

procedure TOpenGLContext.EndScene;
begin
  SwapBuffers(FDC); // Double Buffering swap
end;

function TOpenGLContext.AlphaToColorRef(const AColor: TAlphaColor): TColorRef;
begin
  Result := RGB(TAlphaColorRec(AColor).R, TAlphaColorRec(AColor).G, TAlphaColorRec(AColor).B);
end;

procedure TOpenGLContext.SetColor(const AColor: TAlphaColor);
begin
  glColor3ub(TAlphaColorRec(AColor).R, TAlphaColorRec(AColor).G, TAlphaColorRec(AColor).B);
end;

procedure TOpenGLContext.SetLineWidth(const AWidth: Single);
begin
  glLineWidth(AWidth);
end;

procedure TOpenGLContext.DrawCube(const ASize: Single);
var
  H: Single;
begin
  H := ASize / 2;
  // Legacy OpenGL Immediate Mode (for simplicity in VCL example)
  glBegin(GL_QUADS);
    // Front Face
    glNormal3f(0.0, 0.0, 1.0);
    glVertex3f(-H, -H,  H);
    glVertex3f( H, -H,  H);
    glVertex3f( H,  H,  H);
    glVertex3f(-H,  H,  H);
    // Back Face
    glNormal3f(0.0, 0.0, -1.0);
    glVertex3f(-H, -H, -H);
    glVertex3f(-H,  H, -H);
    glVertex3f( H,  H, -H);
    glVertex3f( H, -H, -H);
    // Top Face
    glNormal3f(0.0, 1.0, 0.0);
    glVertex3f(-H,  H, -H);
    glVertex3f(-H,  H,  H);
    glVertex3f( H,  H,  H);
    glVertex3f( H,  H, -H);
    // Bottom Face
    glNormal3f(0.0, -1.0, 0.0);
    glVertex3f(-H, -H, -H);
    glVertex3f( H, -H, -H);
    glVertex3f( H, -H,  H);
    glVertex3f(-H, -H,  H);
    // Right face
    glNormal3f(1.0, 0.0, 0.0);
    glVertex3f( H, -H, -H);
    glVertex3f( H,  H, -H);
    glVertex3f( H,  H,  H);
    glVertex3f( H, -H,  H);
    // Left Face
    glNormal3f(-1.0, 0.0, 0.0);
    glVertex3f(-H, -H, -H);
    glVertex3f(-H, -H,  H);
    glVertex3f(-H,  H,  H);
    glVertex3f(-H,  H, -H);
  glEnd;
end;

procedure TOpenGLContext.DrawLine(const X1, Y1, Z1, X2, Y2, Z2: Single);
begin
  glBegin(GL_LINES);
  glVertex3f(X1, Y1, Z1);
  glVertex3f(X2, Y2, Z2);
  glEnd;
end;

procedure TOpenGLContext.Rotate(const AAngle: Single; const X, Y, Z: Single);
begin
  glRotatef(AAngle, X, Y, Z);
end;

procedure TOpenGLContext.Translate(const X, Y, Z: Single);
begin
  glTranslatef(X, Y, Z);
end;

procedure TOpenGLContext.PushMatrix; begin glPushMatrix; end;
procedure TOpenGLContext.PopMatrix; begin glPopMatrix; end;

end.
