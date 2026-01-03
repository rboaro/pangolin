unit antiHacking.Security.Integrity;

interface

uses
  System.SysUtils;

type
  TIntegrityResult = (Ok, Unknown, Failed);

  IIntegrityChecker = interface
    ['{5E5B6E6D-2E84-4F84-8D71-6B4E3C6B62C9}']
    function VerifySelf: TIntegrityResult;
  end;

function CreateIntegrityChecker: IIntegrityChecker;

implementation

uses
  System.IOUtils,
  System.Hash,

  antiHacking.Security.Config;

type
  TSelfIntegrityChecker = class(TInterfacedObject, IIntegrityChecker)
  private
    function GetExePath: string;
    function Sha256HexOfFile_UsingGetHashString(const Path: string): string;
    function BytesToHexString(const Data: TBytes): string;
  public
    function VerifySelf: TIntegrityResult;
  end;

function CreateIntegrityChecker: IIntegrityChecker;
begin
  Result := TSelfIntegrityChecker.Create;
end;

function TSelfIntegrityChecker.BytesToHexString(const Data: TBytes): string;
const
  Hex: array[0..15] of Char = '0123456789ABCDEF';
var
  I: Integer;
begin
  SetLength(Result, Length(Data) * 2);
  for I := 0 to High(Data) do
  begin
    Result[(I * 2) + 1] := Hex[Data[I] shr 4];
    Result[(I * 2) + 2] := Hex[Data[I] and $0F];
  end;
end;


function TSelfIntegrityChecker.GetExePath: string;
begin
  Result := ParamStr(0);
end;

function TSelfIntegrityChecker.Sha256HexOfFile_UsingGetHashString(const Path: string): string;
var
  Bytes: TBytes;
  HexText: string;
begin
  Bytes := TFile.ReadAllBytes(Path);

  HexText := BytesToHexString(Bytes);

  Result := THashSHA2.GetHashString(HexText, THashSHA2.TSHA2Version.SHA256);
end;


function TSelfIntegrityChecker.VerifySelf: TIntegrityResult;
var
  Expected, Actual: string;
begin
  {$IFDEF SECURITY_RELAXED}
  Exit(Unknown); // relaxed in debug builds
  {$ENDIF}

  Expected := EXPECTED_EXE_SHA256_HEX.Trim;
  if Expected = '' then
    Exit(Unknown);

  Actual := Sha256HexOfFile_UsingGetHashString(GetExePath);

  if SameText(Expected, Actual) then
    Result := Ok
  else
    Result := Failed;
end;

end.


