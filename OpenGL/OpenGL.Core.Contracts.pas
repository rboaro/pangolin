unit OpenGL.Core.Contracts;

interface

uses
  System.UITypes, System.Types;

type
  /// <summary>
  ///   Abstracts the 3D Rendering Context.
  /// </summary>
  IRenderContext = interface
    ['{1A2B3C4D-5E6F-7890-1234-567890ABCDEF}']
    procedure Initialize;
    procedure Resize(const AWidth, AHeight: Integer);
    procedure BeginScene(const ABackgroundColor: TAlphaColor);
    procedure EndScene;

    procedure SetLineWidth(const AWidth: Single);
    procedure DrawLine(const X1, Y1, Z1, X2, Y2, Z2: Single);

    // Basic Drawing Primitives (Abstracting GL calls)
    procedure SetColor(const AColor: TAlphaColor);
    procedure DrawCube(const ASize: Single);
    procedure Rotate(const AAngle: Single; const X, Y, Z: Single);
    procedure Translate(const X, Y, Z: Single);
    procedure PushMatrix;
    procedure PopMatrix;
  end;

  /// <summary>
  ///   Represents a 3D object.
  /// </summary>
  I3DObject = interface
    ['{2B3C4D5E-6F78-9012-3456-7890ABCDEF12}']
    procedure Render(const AContext: IRenderContext);
    procedure Update(const ADeltaTime: Double);
  end;

implementation

end.
