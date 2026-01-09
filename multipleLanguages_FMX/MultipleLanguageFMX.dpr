program MultipleLanguageFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  multipleLanguageFMX.Main in 'multipleLanguageFMX.Main.pas' {frmMain},
  multipleLanguageFMX.i18n.LangManAdapter in 'multipleLanguageFMX.i18n.LangManAdapter.pas',
  multipleLanguageFMX.i18n.DictionaryJson in 'multipleLanguageFMX.i18n.DictionaryJson.pas',
  multipleLanguageFMX.i18n.Contracts in 'multipleLanguageFMX.i18n.Contracts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
