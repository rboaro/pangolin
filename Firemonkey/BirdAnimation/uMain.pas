unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects, FMX.Ani,
  GLRunner;

type
  TForm1 = class(TForm)
    imgBird: TImage;
    runAnim: TBitmapListAnimation;
    moveX: TFloatAnimation;
    bobY: TFloatAnimation;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    GL: TGLSpriteRunner;
  public
    procedure SetupBirdAnimation(const FileName: string; FrameCount: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  GL := TGLSpriteRunner.Create(Self);

  // Load a sprite sheet PNG:
  // Example: 8 frames in one row (strip)
  GL.LoadSpriteSheet('bird_run_strip.png', 8, 8);

  // Tune to match the video feel
  GL.SetRunAnimation(14);                 // frames per second
  GL.SetSpriteSize(180, 180);             // on-screen size
  GL.SetMovement(-180, ClientHeight-220, 260); // x, y, speed(px/s)

  Timer1.Interval := 16; // ~60fps
  Timer1.Enabled := True;
end;



procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  GL.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // keep on "ground"
  GL.SetMovement(-180, ClientHeight - 220, 260);
end;

procedure TForm1.SetupBirdAnimation(const FileName: string;
  FrameCount: Integer);
var
  Sheet: TBitmap;
  FrameW: Single;
begin
//  Sheet := TBitmap.Create;
//  try
//    Sheet.LoadFromFile(FileName);
//
//    // Assign the sprite sheet to the Image first
//    imgBird.Bitmap.Assign(Sheet);
//
//    // Configure the bitmap list animation
//    animBird.Parent := imgBird;
//    animBird.PropertyName := 'Bitmap';
//    animBird.Loop := True;
//
//    // Duration = seconds for a full loop; tweak to match your video speed.
//    // For example: 8 frames at ~14 FPS ≈ 0.57s per loop
//    animBird.Duration := 0.57;
//
//    // Frame setup:
//    // TBitmapListAnimation expects frames along X by default (sprite strip).
//    FrameW := Sheet.Width / FrameCount;
//    animBird.AnimationBitmap.Assign(Sheet);
//    animBird.AnimationCount := FrameCount;
//    animBird.AnimationRowCount := 1;
//  finally
//    Sheet.Free;
//  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  GL.RenderFrame(Round(ClientWidth), Round(ClientHeight));
end;

end.
