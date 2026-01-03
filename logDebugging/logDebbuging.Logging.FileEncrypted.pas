unit logDebbuging.Logging.FileEncrypted;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.IOUtils,
  logDebbuging.Logging.Contracts;

type
  TEncryptedFileLogger = class(TInterfacedObject, ILogger)
  private
    FLock: TCriticalSection;
    FStream: TFileStream;
    FKey: Byte;

    function Encrypt(const S: string): TBytes;
    function DecryptBytesRollingXor(const Data: TBytes; const InitialKey: Byte): string;
    procedure WriteLine(const Line: string);
    function LevelToText(Level: TLogLevel): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Log(const Level: TLogLevel; const Msg: string);
    procedure EnterMethod(const MethodName: string);
    procedure ExitMethod(const MethodName: string);
    function ReadDecryptedSnapshot: string;


  end;

implementation


const
  LOG_FOLDER = 'LOG';
  LOG_FILE   = 'application.log';

constructor TEncryptedFileLogger.Create;
var
  Path: string;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FKey := $A7;

  Path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + LOG_FOLDER;
  ForceDirectories(Path);

  FStream := TFileStream.Create(
    IncludeTrailingPathDelimiter(Path) + LOG_FILE,
    fmOpenReadWrite or fmShareDenyNone
  );
  FStream.Seek(0, soEnd);
end;

function TEncryptedFileLogger.DecryptBytesRollingXor(const Data: TBytes; const InitialKey: Byte): string;
var
  I: Integer;
  Key: Byte;
begin
  SetLength(Result, Length(Data));
  Key := InitialKey;

  for I := 0 to High(Data) do
  begin
    Result[I + 1] := Char(Data[I] xor Key);
    Key := Data[I];
  end;
end;

destructor TEncryptedFileLogger.Destroy;
begin
  FStream.Free;
  FLock.Free;
  inherited;
end;

function TEncryptedFileLogger.Encrypt(const S: string): TBytes;
var
  I: Integer;
  B: Byte;
begin
  SetLength(Result, Length(S));
  for I := 1 to Length(S) do
  begin
    B := Ord(S[I]) xor FKey;
    Result[I - 1] := B;
    FKey := B;
  end;
end;

procedure TEncryptedFileLogger.WriteLine(const Line: string);
var
  Data: TBytes;
begin
  Data := Encrypt(Line + sLineBreak);
  FStream.WriteBuffer(Data, Length(Data));
end;

function TEncryptedFileLogger.LevelToText(Level: TLogLevel): string;
begin
  case Level of
    Trace:   Result := 'TRACE';
    Info:    Result := 'INFO';
    Warning: Result := 'WARN';
    Error:   Result := 'ERROR';
    Fatal:   Result := 'FATAL';
  end;
end;

procedure TEncryptedFileLogger.Log(const Level: TLogLevel; const Msg: string);
var
  Line: string;
begin
  FLock.Enter;
  try
    Line :=
      FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' [' +
      LevelToText(Level) + '] [TID=' +
      IntToStr(TThread.CurrentThread.ThreadID) + '] ' +
      Msg;

    WriteLine(Line);
  finally
    FLock.Leave;
  end;
end;

function TEncryptedFileLogger.ReadDecryptedSnapshot: string;
var
  SavedPos: Int64;
  Buf: TBytes;
begin
  FLock.Enter;
  try
    SavedPos := FStream.Position;
    try
      FStream.Position := 0;
      SetLength(Buf, FStream.Size);

      if Length(Buf) > 0 then
      begin
        FStream.ReadBuffer(Buf[0], Length(Buf));
      end;

      FStream.Position := SavedPos;
    except
      try FStream.Position := SavedPos; except end;
      raise;
    end;

    Result := DecryptBytesRollingXor(Buf, $A7);
  finally
    FLock.Leave;
  end;
end;

procedure TEncryptedFileLogger.EnterMethod(const MethodName: string);
begin
  Log(Trace, 'ENTER ' + MethodName);
end;

procedure TEncryptedFileLogger.ExitMethod(const MethodName: string);
begin
  Log(Trace, 'EXIT  ' + MethodName);
end;



end.
