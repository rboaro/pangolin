unit Pangolin.OpenGL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  OpenGL.Core.Contracts, OpenGL.Infra.Context, OpenGL.Domain.Entities, System.UITypes,
  OpenGl.Domain.Laser;

type
  TForm3 = class(TForm)
    tmrRender: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tmrRenderTimer(Sender: TObject);
  private
    FContext: IRenderContext;
    FObjects: TArray<I3DObject>;
    FLastTime: Cardinal;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  // 1. Initialize Context
  FContext := TOpenGLContext.Create(Self);
  FContext.Initialize;

  // 2. Create Domain Objects
  FObjects := [
    TSpinningCube.Create(TAlphaColors.Red, -1.5),
    TSpinningCube.Create(TAlphaColors.Blue, 1.5),

    TLaserBeam.Create(TAlphaColors.Lime, 2.0),   // Green Laser, Fast
    TLaserBeam.Create(TAlphaColors.Yellow, 1.5), // Yellow Laser, Medium
    TLaserBeam.Create(TAlphaColors.Cyan, 3.0)    // Cyan Laser, Very Fast
  ];

  FLastTime := GetTickCount;
  tmrRender.Interval := 16; // ~60 FPS
  tmrRender.Enabled := True;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  // Interface reference counting handles destruction,
  // but FContext will be released when FContext := nil or Form closes.
  FContext := nil;
end;

procedure TForm3.FormResize(Sender: TObject);
begin
  if FContext <> nil then
    FContext.Resize(ClientWidth, ClientHeight);
end;

procedure TForm3.tmrRenderTimer(Sender: TObject);
var
  LCurrentTime: Cardinal;
  LDeltaTime: Double;
  LObj: I3DObject;
begin
  // Calculate Delta Time for smooth animation
  LCurrentTime := GetTickCount;
  LDeltaTime := (LCurrentTime - FLastTime) / 1000.0;
  FLastTime := LCurrentTime;

  // 1. Update Logic
  for LObj in FObjects do
    LObj.Update(LDeltaTime);

  // 2. Render Scene
  FContext.BeginScene(TAlphaColors.Black);
  try
    for LObj in FObjects do
      LObj.Render(FContext);
  finally
    FContext.EndScene;
  end;
end;
end.
