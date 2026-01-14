unit Core.Renderer;

interface

uses
  Winapi.Windows, Winapi.OpenGL, System.Classes, System.SysUtils, Vcl.Controls,
  System.Generics.Collections, Core.Interfaces, Core.Types;

type
  TOpenGLRenderer = class(TInterfacedObject, IRenderContext, IRenderer)
  private
    FDC: HDC;
    FRC: HGLRC;
    FTarget: TWinControl;
    FObjects: TList<IGameObject>;
    procedure SetupPixelFormat(DC: HDC);
  public
    constructor Create;
    destructor Destroy; override;

    { IRenderContext }
    procedure Initialize(ATarget: TWinControl);
    procedure Resize(AWidth, AHeight: Integer);
    procedure BeginScene;
    procedure EndScene;
    procedure Shutdown;

    { IRenderer }
    procedure AddObject(const AObject: IGameObject);
    procedure RenderFrame;
    function GetContext: IRenderContext;
  end;

implementation

{ TOpenGLRenderer }

constructor TOpenGLRenderer.Create;
begin
  FObjects := TList<IGameObject>.Create;
end;

destructor TOpenGLRenderer.Destroy;
begin
  Shutdown;
  FObjects.Free;
  inherited;
end;

procedure TOpenGLRenderer.SetupPixelFormat(DC: HDC);
var
  pfd: PIXELFORMATDESCRIPTOR;
  PixelFormat: Integer;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do
  begin
    nSize := SizeOf(PIXELFORMATDESCRIPTOR);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cDepthBits := 16;
    iLayerType := PFD_MAIN_PLANE;
  end;

  PixelFormat := ChoosePixelFormat(DC, @pfd);
  if PixelFormat = 0 then
    RaiseLastOSError;

  if not SetPixelFormat(DC, PixelFormat, @pfd) then
    RaiseLastOSError;
end;

procedure TOpenGLRenderer.Initialize(ATarget: TWinControl);
begin
  FTarget := ATarget;
  FDC := GetDC(FTarget.Handle);
  if FDC = 0 then
    RaiseLastOSError;

  SetupPixelFormat(FDC);

  FRC := wglCreateContext(FDC);
  if FRC = 0 then
    RaiseLastOSError;

  if not wglMakeCurrent(FDC, FRC) then
    RaiseLastOSError;

  // Basic OpenGL setup
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  glClearColor(clSkyBlue.R, clSkyBlue.G, clSkyBlue.B, 1.0);
end;

procedure TOpenGLRenderer.Resize(AWidth, AHeight: Integer);
begin
  if AHeight = 0 then AHeight := 1;
  glViewport(0, 0, AWidth, AHeight);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  // Simple perspective
  gluPerspective(45.0, AWidth / AHeight, 0.1, 100.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TOpenGLRenderer.BeginScene;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  // Move camera back a bit to see the scene
  glTranslatef(0.0, 0.0, -5.0);
end;

procedure TOpenGLRenderer.EndScene;
begin
  SwapBuffers(FDC);
end;

procedure TOpenGLRenderer.Shutdown;
begin
  if FRC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(FRC);
    FRC := 0;
  end;

  if (FDC <> 0) and (FTarget <> nil) then
  begin
    ReleaseDC(FTarget.Handle, FDC);
    FDC := 0;
  end;
end;

procedure TOpenGLRenderer.AddObject(const AObject: IGameObject);
begin
  FObjects.Add(AObject);
end;

procedure TOpenGLRenderer.RenderFrame;
var
  Obj: IGameObject;
begin
  BeginScene;
  for Obj in FObjects do
  begin
    if Obj.Renderable <> nil then
      Obj.Renderable.Render;
  end;
  EndScene;
end;

function TOpenGLRenderer.GetContext: IRenderContext;
begin
  Result := Self;
end;

end.
