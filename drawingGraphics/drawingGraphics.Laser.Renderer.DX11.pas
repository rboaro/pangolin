unit drawingGraphics.Laser.Renderer.DX11;

interface

{$IFDEF MSWINDOWS}

uses
  System.SysUtils, System.Types, System.UITypes,
  Winapi.Windows, Winapi.DxgiType,
  Winapi.DXGI, Winapi.D3D11, Winapi.D3DCommon,
  Winapi.D3DCompiler,  Winapi.DxgiFormat,

  drawingGraphics.Laser.Domain;

type
  TDX11LineRenderer = class(TInterfacedObject, ILineRenderer)
  private
    FWnd: HWND;
    FWidth: Integer;
    FHeight: Integer;

    // D3D
    FDevice: ID3D11Device;
    FContext: ID3D11DeviceContext;
    FSwapChain: IDXGISwapChain;
    FRTV: ID3D11RenderTargetView;

    // Pipeline
    FVS: ID3D11VertexShader;
    FPS: ID3D11PixelShader;
    FInputLayout: ID3D11InputLayout;
    FVB: ID3D11Buffer;

    procedure CreateDeviceAndSwapChain;
    procedure CreateBackBufferRTV;
    procedure ReleaseBackBufferRTV;
    procedure CreateShadersAndLayout;
    procedure EnsureVertexBufferCapacity(const AVertexCount: Integer);
    procedure SetViewport;

    function CompileShader(const Source, EntryPoint, Target: AnsiString): ID3DBlob;

    procedure CheckHR(const HR: HRESULT; const Msg: string);

  public
    constructor Create(const ATargetWindow: HWND);
    destructor Destroy; override;

    procedure Resize(const AWidth, AHeight: Integer);
    procedure BeginFrame;
    procedure Draw(const AFrame: TLaserFrame);
    procedure EndFrame;
  end;

{$ENDIF}

implementation

uses
  System.Math;

{$IFDEF MSWINDOWS}

type
  TVertex = packed record
    Pos: array[0..1] of Single;   // x,y in clip space
    Col: array[0..3] of Single;   // r,g,b,a
  end;

const
  KVS: AnsiString =
    'struct VSIn { float2 pos : POSITION; float4 col : COLOR; };' + #10 +
    'struct VSOut { float4 pos : SV_POSITION; float4 col : COLOR; };' + #10 +
    'VSOut main(VSIn v){ VSOut o; o.pos=float4(v.pos,0,1); o.col=v.col; return o; }';

  KPS: AnsiString =
    'struct PSIn { float4 pos : SV_POSITION; float4 col : COLOR; };' + #10 +
    'float4 main(PSIn p) : SV_TARGET { return p.col; }';

{ Utility }

procedure TDX11LineRenderer.CheckHR(const HR: HRESULT; const Msg: string);
begin
  if Failed(HR) then
    raise Exception.CreateFmt('%s (HRESULT=$%.8x)', [Msg, Cardinal(HR)]);
end;

function AlphaToFloats(const C: TAlphaColor): TVertex;
begin
  Result.Pos[0] := 0;
  Result.Pos[1] := 0;
  Result.Col[3] := ((C shr 24) and $FF) / 255;
  Result.Col[0] := ((C shr 16) and $FF) / 255;
  Result.Col[1] := ((C shr 8) and $FF) / 255;
  Result.Col[2] := ((C) and $FF) / 255;
end;

{ TDX11LineRenderer }

constructor TDX11LineRenderer.Create(const ATargetWindow: HWND);
begin
  inherited Create;
  if ATargetWindow = 0 then
    raise EArgumentException.Create('DX11 renderer requires a valid HWND.');

  FWnd := ATargetWindow;
  CreateDeviceAndSwapChain;
  CreateBackBufferRTV;
  CreateShadersAndLayout;
end;

destructor TDX11LineRenderer.Destroy;
begin
  ReleaseBackBufferRTV;
  FVB := nil;
  FInputLayout := nil;
  FVS := nil;
  FPS := nil;
  FSwapChain := nil;
  FContext := nil;
  FDevice := nil;
  inherited;
end;

procedure TDX11LineRenderer.CreateDeviceAndSwapChain;
var
  SD: DXGI_SWAP_CHAIN_DESC;
  FeatureLevels: array[0..0] of TD3D_FEATURE_LEVEL;
  Flags: UINT;
  HR: HRESULT;
  pFeatureLevel: Cardinal;
begin
  ZeroMemory(@SD, SizeOf(SD));
  SD.BufferCount := 2;
  SD.BufferDesc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
  SD.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
  SD.OutputWindow := FWnd;
  SD.SampleDesc.Count := 1;
  SD.Windowed := True;
  SD.SwapEffect := DXGI_SWAP_EFFECT_DISCARD;

  FeatureLevels[0] := D3D_FEATURE_LEVEL_11_0;

  Flags := 0;
  {$IFDEF DEBUG}
  Flags := Flags or D3D11_CREATE_DEVICE_DEBUG;
  {$ENDIF}

  HR := D3D11CreateDeviceAndSwapChain(
    nil, D3D_DRIVER_TYPE_HARDWARE, 0, Flags,
    @FeatureLevels[0], Length(FeatureLevels), D3D11_SDK_VERSION,
    @SD, FSwapChain, FDevice, pFeatureLevel, FContext
  );
  CheckHR(HR, 'Failed to create D3D11 device/swap chain');
end;

procedure TDX11LineRenderer.CreateBackBufferRTV;
var
  BackBuffer: ID3D11Texture2D;
  HR: HRESULT;
begin
  HR := FSwapChain.GetBuffer(0, ID3D11Texture2D, BackBuffer);
  CheckHR(HR, 'GetBuffer(backbuffer) failed');

  HR := FDevice.CreateRenderTargetView(BackBuffer, nil, FRTV);
  CheckHR(HR, 'CreateRenderTargetView failed');

  BackBuffer := nil;
end;

procedure TDX11LineRenderer.ReleaseBackBufferRTV;
begin
  FRTV := nil;
end;

function TDX11LineRenderer.CompileShader(const Source, EntryPoint, Target: AnsiString): ID3DBlob;
var
  Code, Errors: ID3DBlob;
  HR: HRESULT;
  Flags: UINT;
begin
  Flags := D3DCOMPILE_ENABLE_STRICTNESS;
  {$IFDEF DEBUG}
  Flags := Flags or D3DCOMPILE_DEBUG or D3DCOMPILE_SKIP_OPTIMIZATION;
  {$ENDIF}

  HR := D3DCompile(
    PAnsiChar(Source), Length(Source),
    nil, nil, nil,
    PAnsiChar(EntryPoint),
    PAnsiChar(Target),
    Flags, 0,
    Code, Errors
  );

  if Failed(HR) then
  begin
    if Errors <> nil then
      raise Exception.Create(string(PAnsiChar(Errors.GetBufferPointer)))
    else
      CheckHR(HR, 'Shader compile failed');
  end;

  Result := Code;
end;

procedure TDX11LineRenderer.CreateShadersAndLayout;
var
  VSBlob, PSBlob: ID3DBlob;
  HR: HRESULT;
  Layout: array[0..1] of TD3D11_INPUT_ELEMENT_DESC;
begin
  VSBlob := CompileShader(KVS, 'main', 'vs_4_0');
  PSBlob := CompileShader(KPS, 'main', 'ps_4_0');

  HR := FDevice.CreateVertexShader(VSBlob.GetBufferPointer, VSBlob.GetBufferSize, nil, PID3D11VertexShader(FVS));
  CheckHR(HR, 'CreateVertexShader failed');

  HR := FDevice.CreatePixelShader(PSBlob.GetBufferPointer, PSBlob.GetBufferSize, nil, FPS);
  CheckHR(HR, 'CreatePixelShader failed');

  ZeroMemory(@Layout, SizeOf(Layout));
  Layout[0].SemanticName := 'POSITION';
  Layout[0].SemanticIndex := 0;
  Layout[0].Format := DXGI_FORMAT_R32G32_FLOAT;
  Layout[0].InputSlot := 0;
  Layout[0].AlignedByteOffset := 0;
  Layout[0].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;

  Layout[1].SemanticName := 'COLOR';
  Layout[1].SemanticIndex := 0;
  Layout[1].Format := DXGI_FORMAT_R32G32B32A32_FLOAT;
  Layout[1].InputSlot := 0;
  Layout[1].AlignedByteOffset := 8;
  Layout[1].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;

  HR := FDevice.CreateInputLayout(@Layout[0], Length(Layout),
    VSBlob.GetBufferPointer, VSBlob.GetBufferSize, FInputLayout);
  CheckHR(HR, 'CreateInputLayout failed');
end;

procedure TDX11LineRenderer.SetViewport;
var
  VP: TD3D11_VIEWPORT;
begin
  ZeroMemory(@VP, SizeOf(VP));
  VP.TopLeftX := 0;
  VP.TopLeftY := 0;
  VP.Width := FWidth;
  VP.Height := FHeight;
  VP.MinDepth := 0;
  VP.MaxDepth := 1;
  FContext.RSSetViewports(1, @VP);
end;

procedure TDX11LineRenderer.Resize(const AWidth, AHeight: Integer);
var
  HR: HRESULT;
begin
  FWidth := Max(1, AWidth);
  FHeight := Max(1, AHeight);

  if FSwapChain = nil then Exit;

  ReleaseBackBufferRTV;

  HR := FSwapChain.ResizeBuffers(0, FWidth, FHeight, DXGI_FORMAT_UNKNOWN, 0);
  CheckHR(HR, 'ResizeBuffers failed');

  CreateBackBufferRTV;
end;

procedure TDX11LineRenderer.BeginFrame;
var
  Clear: array[0..3] of single;
begin
  Clear[0] := 0; Clear[1] := 0; Clear[2] := 0; Clear[3] := 1;

  FContext.OMSetRenderTargets(1, FRTV, nil);
  SetViewport;

  // Pipeline setup
  FContext.IASetInputLayout(FInputLayout);
  FContext.VSSetShader(FVS, nil, 0);
  FContext.PSSetShader(FPS, nil, 0);
end;

procedure TDX11LineRenderer.EnsureVertexBufferCapacity(const AVertexCount: Integer);
var
  Desc: TD3D11_BUFFER_DESC;
  Init: TD3D11_SUBRESOURCE_DATA;
  BytesNeeded: Integer;
  HR: HRESULT;
begin
  BytesNeeded := AVertexCount * SizeOf(TVertex);

  if FVB <> nil then
  begin
    Exit;
  end;

  ZeroMemory(@Desc, SizeOf(Desc));
  Desc.Usage := D3D11_USAGE_DYNAMIC;
  Desc.ByteWidth := BytesNeeded;
  Desc.BindFlags := D3D11_BIND_VERTEX_BUFFER;
  Desc.CPUAccessFlags := D3D11_CPU_ACCESS_WRITE;

  ZeroMemory(@Init, SizeOf(Init));

  HR := FDevice.CreateBuffer(Desc, nil, FVB);
  CheckHR(HR, 'CreateBuffer(VB) failed');
end;

procedure TDX11LineRenderer.Draw(const AFrame: TLaserFrame);
var
  Vertices: TArray<TVertex>;
  VCount: Integer;
  I, idx: Integer;
  L: TLaserLine;
  Map: TD3D11_MAPPED_SUBRESOURCE;
  HR: HRESULT;
  Stride, Offset: UINT;

  function ToClipX(const X: Single): Single;
  begin
    Result := (X / FWidth) * 2 - 1;
  end;

  function ToClipY(const Y: Single): Single;
  begin
     Result := 1 - (Y / FHeight) * 2;
  end;

  procedure SetColor(var V: TVertex; const C: TAlphaColor);
  begin
    V.Col[3] := ((C shr 24) and $FF) / 255;
    V.Col[0] := ((C shr 16) and $FF) / 255;
    V.Col[1] := ((C shr 8) and $FF) / 255;
    V.Col[2] := ((C) and $FF) / 255;
  end;

begin
  VCount := Length(AFrame.Lines) * 2;
  if VCount = 0 then Exit;

  SetLength(Vertices, VCount);
  idx := 0;

  for I := 0 to High(AFrame.Lines) do
  begin
    L := AFrame.Lines[I];

    Vertices[idx].Pos[0] := ToClipX(L.A.X);
    Vertices[idx].Pos[1] := ToClipY(L.A.Y);
    SetColor(Vertices[idx], L.Color);
    Inc(idx);

    Vertices[idx].Pos[0] := ToClipX(L.B.X);
    Vertices[idx].Pos[1] := ToClipY(L.B.Y);
    SetColor(Vertices[idx], L.Color);
    Inc(idx);
  end;

  EnsureVertexBufferCapacity(VCount);

  HR := FContext.Map(FVB, 0, D3D11_MAP_WRITE_DISCARD, 0, Map);
  CheckHR(HR, 'Map(VB) failed');
  try
    Move(Vertices[0], Map.pData^, VCount * SizeOf(TVertex));
  finally
    FContext.Unmap(FVB, 0);
  end;

  Stride := SizeOf(TVertex);
  Offset := 0;
  FContext.IASetVertexBuffers(0, 1, FVB, @Stride, @Offset);
  FContext.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_LINELIST);

  FContext.Draw(VCount, 0);
end;

procedure TDX11LineRenderer.EndFrame;
begin
  FSwapChain.Present(1, 0);
end;

{$ENDIF}

end.

