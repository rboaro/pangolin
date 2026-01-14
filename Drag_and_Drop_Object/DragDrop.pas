unit DragDrop;

interface

uses   Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage ;

type
  TImageDragObject = class(TDragObject)
  private
    FBitmap: TBitmap;
    FSourceControl: TControl;
    FSourceParent: TWinControl;
  public
    constructor Create(ASourceControl: TControl; const AGraphic: TGraphic);
    destructor Destroy; override;

    property Bitmap: TBitmap read FBitmap;
    property SourceControl: TControl read FSourceControl;
    property SourceParent: TWinControl read FSourceParent;
  end;

implementation

{ TImageDragObject }

constructor TImageDragObject.Create(ASourceControl: TControl;
  const AGraphic: TGraphic);
begin
  inherited Create;

  FSourceControl := ASourceControl;
  if ASourceControl <> nil then
    FSourceParent := ASourceControl.Parent
  else
    FSourceParent := nil;

  FBitmap := TBitmap.Create;
  if (AGraphic <> nil) and (not AGraphic.Empty) then
    FBitmap.Assign(AGraphic); // converts to bitmap if needed
end;

destructor TImageDragObject.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

end.
