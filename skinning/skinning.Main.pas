unit skinning.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, FMX.Edit;

type
  TForm8 = class(TForm)
    StyleBookDark: TStyleBook;
    StyleBookLight: TStyleBook;
    cmbStyle: TComboBox;
    btnTest: TButton;
    edtName: TEdit;
    lblTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cmbStyleChange(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    procedure ApplyStyle(const StyleName: string);
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.fmx}

procedure TForm8.ApplyStyle(const StyleName: string);
begin
  if SameText(StyleName, 'Dark') then
    Self.StyleBook := StyleBookDark
  else
    Self.StyleBook := StyleBookLight;
end;

procedure TForm8.btnTestClick(Sender: TObject);
begin
  ShowMessage('Hello ' + edtName.Text);
end;

procedure TForm8.cmbStyleChange(Sender: TObject);
begin
  if cmbStyle.Selected <> nil then
    ApplyStyle(cmbStyle.Selected.Text);
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  lblTitle.Text := 'FMX StyleBook Demo';
  edtName.Text := 'Type your name';

  cmbStyle.Items.Add('Light');
  cmbStyle.Items.Add('Dark');
  cmbStyle.ItemIndex := 0;

  ApplyStyle('Light');
end;

end.
