unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, System.Generics.Collections,
  Core.Interfaces, Core.Renderer, Entities.Bird;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FRenderer: IRenderer;
    FUpdatables: TList<IUpdatable>;
    FLastTime: Cardinal;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Bird: TBird;
begin
  FUpdatables := TList<IUpdatable>.Create;

  // Initialize Renderer
  FRenderer := TOpenGLRenderer.Create;
  FRenderer.GetContext.Initialize(Self);
  
  // Create and add Bird
  Bird := TBird.Create;
  FRenderer.AddObject(Bird);
  FUpdatables.Add(Bird);
  
  // Setup Game Loop
  FLastTime := GetTickCount;
  Application.OnIdle := OnIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Application.OnIdle := nil;
  if FRenderer <> nil then
    FRenderer.GetContext.Shutdown;
  FUpdatables.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if FRenderer <> nil then
    FRenderer.GetContext.Resize(ClientWidth, ClientHeight);
end;

procedure TForm1.OnIdle(Sender: TObject; var Done: Boolean);
var
  CurrentTime: Cardinal;
  DeltaTime: Double;
  Updatable: IUpdatable;
begin
  CurrentTime := GetTickCount;
  DeltaTime := (CurrentTime - FLastTime) / 1000.0;
  FLastTime := CurrentTime;

  // Update all game objects
  for Updatable in FUpdatables do
    Updatable.Update(DeltaTime);

  // Render
  FRenderer.RenderFrame;
  
  Done := False; // Request more idle time immediately
end;

end.
