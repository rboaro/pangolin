unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage,
  DragDrop;

type
  TForm7 = class(TForm)
    PanelSource: TPanel;
    PanelTarget: TPanel;
    Image: TImage;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure ImageEndDrag(Sender, Target: TObject; X, Y: Integer);

    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    FCurrentDragSource: TObject;
    FCurrentDrag: TImageDragObject;

  public
    FHoverPanel: TPanel;
    FOldHoverColor: TColor;

    function GetImageDragObject(Source: TObject): TImageDragObject;
    procedure HoverPanelSet(APanel: TPanel);
    procedure HoverPanelClear;
    procedure DropBitmapIntoPanel(ABitmap: TBitmap; ATarget: TPanel; X, Y: Integer);
    function WantCopy: Boolean;
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

{ TForm7 }

procedure TForm7.DropBitmapIntoPanel(ABitmap: TBitmap; ATarget: TPanel; X,
  Y: Integer);
var
  NewImg: TImage;
  NewLeft, NewTop: Integer;
begin
  NewImg := TImage.Create(ATarget);
  NewImg.Parent := ATarget;
  NewImg.Picture.Bitmap.Assign(ABitmap);
  NewImg.AutoSize := True;
  NewImg.Transparent := True;

  // Make the new image draggable too
  NewImg.OnMouseDown := ImageMouseDown;
  NewImg.OnStartDrag := ImageStartDrag;
  NewImg.OnEndDrag   := ImageEndDrag;
  NewImg.Cursor := crHandPoint;

  // Position centered at drop point
  NewLeft := X - (NewImg.Width div 2);
  NewTop  := Y - (NewImg.Height div 2);

  // Clamp inside panel
  if NewLeft < 0 then NewLeft := 0;
  if NewTop < 0 then NewTop := 0;
  if NewLeft + NewImg.Width > ATarget.ClientWidth then
    NewLeft := ATarget.ClientWidth - NewImg.Width;
  if NewTop + NewImg.Height > ATarget.ClientHeight then
    NewTop := ATarget.ClientHeight - NewImg.Height;

  NewImg.Left := NewLeft;
  NewImg.Top  := NewTop;
end;

procedure TForm7.FormCreate(Sender: TObject);
begin
  Image.OnMouseDown := ImageMouseDown;
  Image.OnStartDrag := ImageStartDrag;
  Image.OnEndDrag   := ImageEndDrag;

  Image.Cursor := crHandPoint;
end;

function TForm7.GetImageDragObject(Source: TObject): TImageDragObject;
begin
  if (FCurrentDrag <> nil) and (Source = FCurrentDragSource) then
    Result := FCurrentDrag
  else
    Result := nil;

end;

procedure TForm7.HoverPanelClear;
begin
  if FHoverPanel <> nil then
  begin
    FHoverPanel.Color := FOldHoverColor;
    FHoverPanel := nil;
  end;
end;

procedure TForm7.HoverPanelSet(APanel: TPanel);
begin
  if FHoverPanel = APanel then
    Exit;

  HoverPanelClear;

  if APanel <> nil then
  begin
    FHoverPanel := APanel;
    FOldHoverColor := APanel.Color;

    // Lightweight visual feedback. Keep it simple/cheap.
    APanel.Color := clInfoBk;
  end;
end;

procedure TForm7.ImageEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  HoverPanelClear;
  FCurrentDrag := nil;
  FCurrentDragSource := nil;
end;


procedure TForm7.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    // Start manual drag; drag object will be created in OnStartDrag
    (Sender as TControl).BeginDrag(False);
  end;
end;

procedure TForm7.ImageStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FCurrentDrag := TImageDragObject.Create(Sender as TControl,
                                          (Sender as TImage).Picture.Graphic);
  FCurrentDragSource := Sender;
  DragObject := FCurrentDrag;
end;


procedure TForm7.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DragObj: TImageDragObject;
  TargetPanel: TPanel;
  SrcCtrl: TControl;
  DoCopy: Boolean;
begin
  TargetPanel := Sender as TPanel;
  DragObj := GetImageDragObject(Source);
  if DragObj = nil then Exit;

  DoCopy := WantCopy;
  SrcCtrl := DragObj.SourceControl;

  if DoCopy then
  begin
    // Copy: create a new TImage in target
    DropBitmapIntoPanel(DragObj.Bitmap, TargetPanel, X, Y);
  end
  else
  begin
    // Move: reparent the original control and place it
    if (SrcCtrl <> nil) and (SrcCtrl is TImage) then
    begin
      SrcCtrl.Parent := TargetPanel;
      SrcCtrl.Left := X - (SrcCtrl.Width div 2);
      SrcCtrl.Top  := Y - (SrcCtrl.Height div 2);

      // Clamp
      if SrcCtrl.Left < 0 then SrcCtrl.Left := 0;
      if SrcCtrl.Top < 0 then SrcCtrl.Top := 0;
      if SrcCtrl.Left + SrcCtrl.Width > TargetPanel.ClientWidth then
        SrcCtrl.Left := TargetPanel.ClientWidth - SrcCtrl.Width;
      if SrcCtrl.Top + SrcCtrl.Height > TargetPanel.ClientHeight then
        SrcCtrl.Top := TargetPanel.ClientHeight - SrcCtrl.Height;
    end
    else
    begin
      // Fallback: if source control is missing, still drop a copy
      DropBitmapIntoPanel(DragObj.Bitmap, TargetPanel, X, Y);
    end;
  end;

  HoverPanelClear;
end;

procedure TForm7.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DragObj: TImageDragObject;
  TargetPanel: TPanel;
begin
  TargetPanel := Sender as TPanel;
  DragObj := GetImageDragObject(Source);

  Accept := (DragObj <> nil) and (DragObj.Bitmap <> nil) and (not DragObj.Bitmap.Empty);

  // Optional: disallow dropping back onto same parent when moving
  if Accept and (not WantCopy) then
    Accept := (DragObj.SourceParent <> TargetPanel);

  if Accept then
    HoverPanelSet(TargetPanel)
  else if FHoverPanel = TargetPanel then
    HoverPanelClear;
end;

function TForm7.WantCopy: Boolean;
begin
  // Standard UX: Ctrl => Copy, else Move
  Result := GetKeyState(VK_CONTROL) < 0;
end;

end.
