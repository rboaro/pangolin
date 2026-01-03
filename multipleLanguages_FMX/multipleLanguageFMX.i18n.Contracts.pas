unit multipleLanguageFMX.i18n.Contracts;

interface

type
  ILocalization = interface
    ['{6D9A1F27-43D0-4B09-AE5B-3DF8E2D4C7C0}']
    procedure SetLanguage(const LangCode: string);
    function T(const Key, DefaultText: string): string;
  end;

implementation

end.

