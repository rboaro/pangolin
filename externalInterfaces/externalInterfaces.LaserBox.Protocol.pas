unit externalInterfaces.LaserBox.Protocol;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  // Example frame:
  // [0]= $AA  [1..2]=Length (UInt16 LE)  [3]=Cmd  [4..]=Payload  [end-1]=CRC8
  TLaserCmd = (Ping = $01, Telemetry = $02, FrameChunk = $10);

  TLaserPacket = record
    Cmd: Byte;
    Payload: TBytes;
    class function Create(const ACmd: Byte; const APayload: TBytes): TLaserPacket; static;
  end;

  TLaserProtocol = class
  private
    FBuffer: TBytes;

    function CalcCrc8(const Data: TBytes; const Offset, Count: Integer): Byte;
    procedure BufferAppend(const Data: TBytes);
    function TryExtractPacket(out Packet: TLaserPacket): Boolean;
  public
    function Encode(const Packet: TLaserPacket): TBytes;
    procedure PushBytes(const Data: TBytes);
    function PopAllPackets: TArray<TLaserPacket>;
  end;

implementation

{ TLaserPacket }

class function TLaserPacket.Create(const ACmd: Byte; const APayload: TBytes): TLaserPacket;
begin
  Result.Cmd := ACmd;
  Result.Payload := APayload;
end;

{ TLaserProtocol }

function TLaserProtocol.CalcCrc8(const Data: TBytes; const Offset, Count: Integer): Byte;
var
  I: Integer;
  C: Byte;
begin
  // Simple CRC8 (XOR) for demo; replace with device-specific CRC if needed.
  C := 0;
  for I := Offset to Offset + Count - 1 do
    C := C xor Data[I];
  Result := C;
end;

procedure TLaserProtocol.BufferAppend(const Data: TBytes);
var
  OldLen, AddLen: Integer;
begin
  AddLen := Length(Data);
  if AddLen = 0 then Exit;

  OldLen := Length(FBuffer);
  SetLength(FBuffer, OldLen + AddLen);
  Move(Data[0], FBuffer[OldLen], AddLen);
end;

function TLaserProtocol.Encode(const Packet: TLaserPacket): TBytes;
var
  Len: Word;
  Total: Integer;
  Crc: Byte;
begin
  Len := 1 + Length(Packet.Payload); // cmd + payload
  Total := 1 + 2 + Len + 1;          // SOF + len(2) + data + crc

  SetLength(Result, Total);
  Result[0] := $AA;
  Result[1] := Byte(Len and $FF);
  Result[2] := Byte((Len shr 8) and $FF);

  Result[3] := Packet.Cmd;
  if Length(Packet.Payload) > 0 then
    Move(Packet.Payload[0], Result[4], Length(Packet.Payload));

  Crc := CalcCrc8(Result, 0, Total - 1);
  Result[Total - 1] := Crc;
end;

procedure TLaserProtocol.PushBytes(const Data: TBytes);
begin
  BufferAppend(Data);
end;

function TLaserProtocol.TryExtractPacket(out Packet: TLaserPacket): Boolean;
var
  I: Integer;
  Len: Word;
  Total: Integer;
  CrcExpected, CrcActual: Byte;
  Cmd: Byte;
  PayloadLen: Integer;
  Payload: TBytes;
begin
  Result := False;

  I := 0;
  while (I < Length(FBuffer)) and (FBuffer[I] <> $AA) do
    Inc(I);

  if I > 0 then
  begin
    Delete(FBuffer, 0, I);
  end;

  if Length(FBuffer) < 1 + 2 + 1 + 1 then
    Exit;

  Len := Word(FBuffer[1]) or (Word(FBuffer[2]) shl 8);
  Total := 1 + 2 + Len + 1;

  if Length(FBuffer) < Total then
    Exit;

  CrcExpected := FBuffer[Total - 1];
  CrcActual := CalcCrc8(FBuffer, 0, Total - 1);
  if CrcActual <> CrcExpected then
  begin
    Delete(FBuffer, 0, 1);
    Exit;
  end;

  Cmd := FBuffer[3];
  PayloadLen := Len - 1;
  SetLength(Payload, PayloadLen);
  if PayloadLen > 0 then
    Move(FBuffer[4], Payload[0], PayloadLen);

  Packet := TLaserPacket.Create(Cmd, Payload);

  Delete(FBuffer, 0, Total);
  Result := True;
end;

function TLaserProtocol.PopAllPackets: TArray<TLaserPacket>;
var
  List: TList<TLaserPacket>;
  P: TLaserPacket;
begin
  List := TList<TLaserPacket>.Create;
  try
    while TryExtractPacket(P) do
      List.Add(P);
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

end.

