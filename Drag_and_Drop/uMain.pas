unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlSource: TPanel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlSourceDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure pnlSourceDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    function IsImageSource(const Source: TObject): Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  pnlLeft.OnDragOver := pnlSourceDragOver;
  pnlLeft.OnDragDrop := pnlSourceDragDrop;

  pnlRight.OnDragOver := pnlSourceDragOver;
  pnlRight.OnDragDrop := pnlSourceDragDrop;

  Image1.Cursor := crHandPoint;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    (Sender as TControl).BeginDrag(True);
  end;
end;

function TForm1.IsImageSource(const Source: TObject): Boolean;
begin
  Result := (Source is TImage) and (TImage(Source).Picture.Graphic <> nil);
end;

procedure TForm1.pnlSourceDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Img: TImage;
  P: TPoint;
  NewLeft, NewTop: Integer;
begin
  if not (Sender is TPanel) then
    Exit;
  if not IsImageSource(Source) then
    Exit;

  Img := TImage(Source);

  P := Point(X, Y);

  Img.Parent := TPanel(Sender);

  NewLeft := P.X - (Img.Width div 2);
  NewTop  := P.Y - (Img.Height div 2);

  if NewLeft < 0 then NewLeft := 0;
  if NewTop < 0 then NewTop := 0;
  if NewLeft + Img.Width > Img.Parent.ClientWidth then
    NewLeft := Img.Parent.ClientWidth - Img.Width;
  if NewTop + Img.Height > Img.Parent.ClientHeight then
    NewTop := Img.Parent.ClientHeight - Img.Height;

  Img.Left := NewLeft;
  Img.Top  := NewTop;
end;

procedure TForm1.pnlSourceDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := IsImageSource(Source);

  if Accept then
    Accept := (Sender is TPanel) and ((Source as TControl).Parent <> Sender);
end;

end.
