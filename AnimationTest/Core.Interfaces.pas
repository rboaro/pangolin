unit Core.Interfaces;

interface

uses
  System.Classes, Vcl.Controls;

type
  /// <summary>
  ///  Represents a 3D vector.
  /// </summary>
  TVector3 = record
    X, Y, Z: Single;
    class function Create(AX, AY, AZ: Single): TVector3; static;
  end;

  /// <summary>
  ///  Interface for objects that need to be updated every frame.
  /// </summary>
  IUpdatable = interface
    ['{A1B2C3D4-E5F6-4789-0001-1234567890AB}']
    procedure Update(const DeltaTime: Double);
  end;

  /// <summary>
  ///  Interface for objects that can be rendered.
  /// </summary>
  IRenderable = interface
    ['{A1B2C3D4-E5F6-4789-0002-1234567890AB}']
    procedure Render;
  end;

  /// <summary>
  ///  Composite interface for game objects.
  /// </summary>
  IGameObject = interface(IInterface)
    ['{A1B2C3D4-E5F6-4789-0003-1234567890AB}']
    function GetUpdatable: IUpdatable;
    function GetRenderable: IRenderable;
    property Updatable: IUpdatable read GetUpdatable;
    property Renderable: IRenderable read GetRenderable;
  end;

  /// <summary>
  ///  Interface for the rendering context (OpenGL).
  /// </summary>
  IRenderContext = interface
    ['{A1B2C3D4-E5F6-4789-0004-1234567890AB}']
    procedure Initialize(ATarget: TWinControl);
    procedure Resize(AWidth, AHeight: Integer);
    procedure BeginScene;
    procedure EndScene;
    procedure Shutdown;
  end;

  /// <summary>
  ///  Interface for the main renderer system.
  /// </summary>
  IRenderer = interface
    ['{A1B2C3D4-E5F6-4789-0005-1234567890AB}']
    procedure AddObject(const AObject: IGameObject);
    procedure RenderFrame;
    function GetContext: IRenderContext;
  end;

implementation

{ TVector3 }

class function TVector3.Create(AX, AY, AZ: Single): TVector3;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := AZ;
end;

end.
