unit GLRunner;

interface

uses
  System.SysUtils, System.Classes, System.Types, System.Diagnostics,
  Winapi.Windows, Winapi.OpenGL, FMX.Forms,
  FMX.Graphics,
  FMX.Platform.Win;

type
  TGLSpriteRunner = class
  private
    FHWND: HWND;
    FDC: HDC;
    FRC: HGLRC;

    FTex: GLuint;
    FTexW, FTexH: Integer;

    FSpriteW, FSpriteH: Single;   // on-screen size (pixels)
    FX, FY: Single;               // position (pixels)
    FSpeed: Single;               // pixels/sec

    FFrameCount: Integer;
    FFramesPerRow: Integer;
    FFrameIndex: Integer;
    FFPS: Single;

    FClock: TStopwatch;
    FAccum: Double;

    procedure CreateGLContext;
    procedure DestroyGLContext;

    procedure Setup2D(const W, H: Integer);
    procedure UploadTextureFromBitmap(const Bmp: TBitmap);

    procedure BindTexture;
    procedure DrawTexturedQuad(const X, Y, W, H: Single;
      const U0, V0, U1, V1: Single);

  public
    constructor Create(const AForm: TForm);
    destructor Destroy; override;

    procedure LoadSpriteSheet(const FileName: string; AFrameCount, AFramesPerRow: Integer);

    // tuning
    procedure SetRunAnimation(const AFPS: Single);
    procedure SetMovement(const AX, AY, ASpeed: Single);
    procedure SetSpriteSize(const AW, AH: Single);

    // main loop
    procedure RenderFrame(const ViewW, ViewH: Integer);
  end;



implementation

const
  GL_BGRA = $80E1; // standard OpenGL value

function FormToHWND(const AForm: TCommonCustomForm): HWND;
var
  Wnd: TWinWindowHandle;
begin
  Wnd := WindowHandleToPlatform(AForm.Handle);
  Result := Wnd.Wnd;
end;

{ TGLSpriteRunner }

constructor TGLSpriteRunner.Create(const AForm: TForm);
begin
  inherited Create;
  FHWND := FormToHWND(AForm);
  FDC := 0;
  FRC := 0;
  FTex := 0;

   FSpriteW := 180;
  FSpriteH := 180;
  FX := 0;
  FY := 200;
  FSpeed := 250; // px/s

  FFrameCount := 8;
  FFramesPerRow := 8;
  FFrameIndex := 0;
  FFPS := 14;

  FAccum := 0;
  FClock := TStopwatch.Create;

  CreateGLContext;

  glDisable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  FClock.Start;
end;

destructor TGLSpriteRunner.Destroy;
begin
  if FTex <> 0 then
    glDeleteTextures(1, @FTex);

  DestroyGLContext;
  inherited;
end;

procedure TGLSpriteRunner.CreateGLContext;
var
  pfd: TPixelFormatDescriptor;
  pf: Integer;
begin
  FDC := GetDC(FHWND);
  if FDC = 0 then
    raise Exception.Create('Failed to get DC.');

  ZeroMemory(@pfd, SizeOf(pfd));
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 32;
  pfd.cAlphaBits := 8;
  pfd.cDepthBits := 0;
  pfd.iLayerType := PFD_MAIN_PLANE;

  pf := ChoosePixelFormat(FDC, @pfd);
  if pf = 0 then
    raise Exception.Create('ChoosePixelFormat failed.');

  if not SetPixelFormat(FDC, pf, @pfd) then
    raise Exception.Create('SetPixelFormat failed.');

  FRC := wglCreateContext(FDC);
  if FRC = 0 then
    raise Exception.Create('wglCreateContext failed.');

  if not wglMakeCurrent(FDC, FRC) then
    raise Exception.Create('wglMakeCurrent failed.');
end;

procedure TGLSpriteRunner.DestroyGLContext;
begin
  if FRC <> 0 then
  begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(FRC);
    FRC := 0;
  end;

  if FDC <> 0 then
  begin
    ReleaseDC(FHWND, FDC);
    FDC := 0;
  end;
end;

procedure TGLSpriteRunner.Setup2D(const W, H: Integer);
begin
  glViewport(0, 0, W, H);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  glOrtho(0, W, H, 0, -1, 1);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TGLSpriteRunner.UploadTextureFromBitmap(const Bmp: TBitmap);
var
  Data: TBitmapData;
begin
  if Bmp.IsEmpty then
    raise Exception.Create('Bitmap is empty.');

  if FTex = 0 then
    glGenTextures(1, @FTex);

  FTexW := Bmp.Width;
  FTexH := Bmp.Height;

  glBindTexture(GL_TEXTURE_2D, FTex);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  // FireMonkey TBitmap is typically BGRA on Windows
  if not Bmp.Map(TMapAccess.Read, Data) then
    raise Exception.Create('Failed to map bitmap.');

  try
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);

    // Internal format RGBA, source format BGRA
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Bmp.Width, Bmp.Height, 0,
      GL_BGRA, GL_UNSIGNED_BYTE, Data.Data);
  finally
    Bmp.Unmap(Data);
  end;
end;

procedure TGLSpriteRunner.BindTexture;
begin
  glBindTexture(GL_TEXTURE_2D, FTex);
end;

procedure TGLSpriteRunner.DrawTexturedQuad(const X, Y, W, H: Single;
  const U0, V0, U1, V1: Single);
begin
  glBegin(GL_QUADS);
    glTexCoord2f(U0, V0); glVertex2f(X,     Y);
    glTexCoord2f(U1, V0); glVertex2f(X + W, Y);
    glTexCoord2f(U1, V1); glVertex2f(X + W, Y + H);
    glTexCoord2f(U0, V1); glVertex2f(X,     Y + H);
  glEnd;
end;

procedure TGLSpriteRunner.LoadSpriteSheet(const FileName: string; AFrameCount,
  AFramesPerRow: Integer);
var
  Bmp: TBitmap;
begin
  FFrameCount := AFrameCount;
  FFramesPerRow := AFramesPerRow;

  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromFile(FileName);
    UploadTextureFromBitmap(Bmp);
  finally
    Bmp.Free;
  end;
end;

procedure TGLSpriteRunner.SetRunAnimation(const AFPS: Single);
begin
  FFPS := AFPS;
end;

procedure TGLSpriteRunner.SetMovement(const AX, AY, ASpeed: Single);
begin
  FX := AX;
  FY := AY;
  FSpeed := ASpeed;
end;

procedure TGLSpriteRunner.SetSpriteSize(const AW, AH: Single);
begin
  FSpriteW := AW;
  FSpriteH := AH;
end;

procedure TGLSpriteRunner.RenderFrame(const ViewW, ViewH: Integer);
var
  dt, step: Double;
  col, row, rows: Integer;
  u0, v0, u1, v1: Single;
  frameW, frameH: Single;
begin
  if (FRC = 0) or (FTex = 0) then Exit;

  dt := FClock.Elapsed.TotalSeconds;
  FClock.Reset;
  FClock.Start;

  FAccum := FAccum + dt;
  step := 1.0 / FFPS;

  while FAccum >= step do
  begin
    FAccum := FAccum - step;
    Inc(FFrameIndex);
    if FFrameIndex >= FFrameCount then
      FFrameIndex := 0;
  end;

  // movement (wrap)
  FX := FX + (FSpeed * dt);
  if FX > ViewW + FSpriteW then
    FX := -FSpriteW;

  // compute UV for current frame (grid)
  rows := (FFrameCount + FFramesPerRow - 1) div FFramesPerRow;
  col := FFrameIndex mod FFramesPerRow;
  row := FFrameIndex div FFramesPerRow;

  frameW := 1.0 / FFramesPerRow;
  frameH := 1.0 / rows;

  u0 := col * frameW;
  v0 := row * frameH;
  u1 := u0 + frameW;
  v1 := v0 + frameH;

  // render
  wglMakeCurrent(FDC, FRC);
  Setup2D(ViewW, ViewH);

  glClearColor(0.08, 0.08, 0.10, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);

  BindTexture;
  glColor4f(1, 1, 1, 1);

  DrawTexturedQuad(FX, FY, FSpriteW, FSpriteH, u0, v0, u1, v1);

  SwapBuffers(FDC);
end;

end.

