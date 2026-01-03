unit multipleLanguageFMX.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Edit, FMX.ListBox, FMX.Controls.Presentation,

  multipleLanguageFMX.i18n.Contracts, multipleLanguageFMX.i18n.LangManAdapter;

type
  TfrmMain = class(TForm)
    cmbLang: TComboBox;
    btnOk: TButton;
    edtName: TEdit;
    chkEnable: TCheckBox;
    memLog: TMemo;
    lblTitle: TLabel;
    Lang1: TLang;
    procedure FormCreate(Sender: TObject);
    procedure cmbLangChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    FI18n: ILocalization;

    procedure ApplyTexts;
    procedure Log(const S: string);
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.fmx}

procedure TfrmMain.ApplyTexts;
begin
  lblTitle.Text := FI18n.T('TITLE', 'Runtime Localization Demo');
  btnOk.Text    := FI18n.T('BTN_OK', 'OK');
  edtName.Text  := FI18n.T('PROMPT_NAME', 'Type your name');
  chkEnable.Text := FI18n.T('CHK_ENABLE', 'Enable output');
end;

procedure TfrmMain.btnOkClick(Sender: TObject);
var
  Name: string;
begin
  Name := edtName.Text.Trim;
  if Name = '' then
  begin
    ShowMessage(FI18n.T('ERR_NAME_REQUIRED', 'Name is required.'));
    Exit;
  end;

  ShowMessage(Format(FI18n.T('MSG_HELLO_FMT', 'Hello, %s!'), [Name]));
end;

procedure TfrmMain.cmbLangChange(Sender: TObject);
var
  Lang: string;
begin
  Lang := cmbLang.Selected.Text;
  FI18n.SetLanguage(Lang);
  ApplyTexts;
  Log('Switched to ' + Lang);
end;


procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FI18n := TLangManLocalization.Create(Lang1, ExtractFilePath(ParamStr(0)));

  cmbLang.Items.Add('en');
  cmbLang.Items.Add('pt');
  cmbLang.Items.Add('es');
  cmbLang.ItemIndex := 0;

  FI18n.SetLanguage(cmbLang.Selected.Text);
  ApplyTexts;
  Log('Started');
end;

procedure TfrmMain.Log(const S: string);
begin
  memLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' ' + S);
end;

end.
