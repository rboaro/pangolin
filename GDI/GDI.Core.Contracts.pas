unit GDI.Core.Contracts;


interface

uses
  System.UITypes, System.Types;

type
  IGraphicsContext = interface
    ['{A1B2C3D4-E5F6-7890-1234-567890ABCDEF}']
    procedure SetPen(const AColor: TAlphaColor; const AThickness: Single);
    procedure SetBrush(const AColor: TAlphaColor);
    procedure DrawRectangle(const ARect: TRectF);
    procedure DrawEllipse(const ARect: TRectF);
    procedure DrawText(const AText: string; const ARect: TRectF);
    procedure DrawImage(const AImage: TObject; const ARect: TRectF);
  end;

  IDrawable = interface
    ['{B2C3D4E5-F678-9012-3456-7890ABCDEF12}']
    procedure Render(const AContext: IGraphicsContext);
    /// <summary>
    ///   Sets the animation progress from 0.0 (invisible) to 1.0 (full size).
    /// </summary>
    procedure SetProgress(const AValue: Single);
  end;

implementation

end.
