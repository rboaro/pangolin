unit multipleLanguageFMX.i18n.LangManAdapter;

interface

uses
  System.SysUtils,
  multipleLanguageFMX.i18n.Contracts, multipleLanguageFMX.i18n.DictionaryJson;

type
  TLangManLocalization = class(TInterfacedObject, ILocalization)
  private
    FLangMan: TObject;
    FDict: TJsonDictionary;
    FBasePath: string;
    procedure ApplyLangManUiLanguage(const LangCode: string);
  public
    constructor Create(const LangManComponent: TObject; const BasePath: string);
    destructor Destroy; override;

    procedure SetLanguage(const LangCode: string);
    function T(const Key, DefaultText: string): string;
  end;

implementation

constructor TLangManLocalization.Create(const LangManComponent: TObject; const BasePath: string);
begin
  inherited Create;
  FLangMan := LangManComponent;
  FBasePath := IncludeTrailingPathDelimiter(BasePath);
  FDict := TJsonDictionary.Create;
end;

destructor TLangManLocalization.Destroy;
begin
  FDict.Free;
  inherited;
end;

procedure TLangManLocalization.ApplyLangManUiLanguage(const LangCode: string);
begin
  //
  // (A) FLangMan.Language := LangCode;
  // (B) FLangMan.SetLanguage(LangCode);
  // (C) FLangMan.LoadLanguageFromFile(FBasePath + 'ui\' + LangCode + '.lng');
  // (D) FLangMan.TranslateForm(Application.MainForm);

  // For now, this is intentionally left as the only “adapter point”.
end;

procedure TLangManLocalization.SetLanguage(const LangCode: string);
begin
  ApplyLangManUiLanguage(LangCode);

  FDict.LoadFromFile(FBasePath + 'i18n\' + LangCode + '.json');
end;

function TLangManLocalization.T(const Key, DefaultText: string): string;
begin
  Result := FDict.GetText(Key, DefaultText);
end;

end.


