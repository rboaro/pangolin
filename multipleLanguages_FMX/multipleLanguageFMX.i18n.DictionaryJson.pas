unit multipleLanguageFMX.i18n.DictionaryJson;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.IOUtils;

type
  TJsonDictionary = class
  private
    FData: TJSONObject;
  public
    destructor Destroy; override;

    procedure LoadFromFile(const FilePath: string);
    function GetText(const Key, DefaultText: string): string;
  end;

implementation

destructor TJsonDictionary.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TJsonDictionary.LoadFromFile(const FilePath: string);
var
  S: string;
begin
  FreeAndNil(FData);

  if not FileExists(FilePath) then
    Exit;

  S := TFile.ReadAllText(FilePath, TEncoding.UTF8);
  FData := TJSONObject.ParseJSONValue(S) as TJSONObject;
end;

function TJsonDictionary.GetText(const Key, DefaultText: string): string;
var
  V: TJSONValue;
begin
  if FData <> nil then
  begin
    V := FData.Values[Key];
    if (V <> nil) and (V.Value <> '') then
      Exit(V.Value);
  end;

  Result := DefaultText;
end;

end.

