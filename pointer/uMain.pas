unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, OpenGL, dglOpenGL;

type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..MaxInt div SizeOf(TRGBTriple) - 1] of TRGBTriple;

  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..MaxInt div SizeOf(TRGBQuad) - 1] of TRGBQuad;

  TVertex = packed record
    X, Y, Z: Single;
    U, V: Single;
  end;

  TForm9 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;



var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.Button1Click(Sender: TObject);

  procedure SwapInt(a, b: PInteger);
  var
    t: Integer;
  begin
    t := a^;
    a^ := b^;
    b^ := t;
  end;

var x, y: Integer;
begin
  x := 1; y := 2;
  SwapInt(@x, @y);
end;

procedure TForm9.Button2Click(Sender: TObject);
var
  p, cur: PByte;
  i: Integer;
begin
  // Allocate 1024 bytes and set them to increasing values mod 256 using pointer increments.

  GetMem(p, 1024);
  try
    cur := p;
    for i := 0 to 1023 do
    begin
      cur^ := Byte(i);
      Inc(cur);
    end;

    {
      about hexadecimal values

      this:   $A47F862
       $ → hexadecimal number
       A47F862 → hex digits

       $0A47F862 = 172,001,378 (decimal)

       PByte($A47F862)^     or  PInteger($A47F862)^

       delphi watch debug window

        var
          P: PByte;
          i: Integer;
        begin
          P := PByte($0A47F862);

          for i := 0 to 15 do
            OutputDebugString(PChar(
              Format('Byte[%d] = %d', [i, P[i]])
            ));
        end;

    }

  finally
    FreeMem(p);
  end;
end;


procedure TForm9.Button3Click(Sender: TObject);

{
  Given a buffer containing:
  •	2 bytes: Version (Word)
  •	4 bytes: Length (Cardinal)

}

var
  version: Word;
  len: Cardinal;
  cur: PByte;
  Buf: Pointer;

begin
  // sure here Buf doesn't have value, it's just and example to work with it.
  cur := Buf;

  Move(cur^, version, SizeOf(version));
  Inc(cur, SizeOf(version));

  Move(cur^, len, SizeOf(len));
end;


procedure TForm9.Button4Click(Sender: TObject);

  procedure Invert8(p: PByte; Count: NativeInt);
  var
    i: NativeInt;
  begin
    for i := 0 to Count - 1 do
    begin
      p^ := 255 - p^;
      Inc(p);
    end;
  end;
begin
  //
end;

procedure TForm9.Button5Click(Sender: TObject);

    procedure InvertBitmap24(const Bmp: TBitmap);
    var
      y, x: Integer;
      Row: PRGBTripleArray;
      P: TRGBTriple;
    begin
      Bmp.PixelFormat := pf24bit;

      for y := 0 to Bmp.Height - 1 do
      begin
        Row := Bmp.ScanLine[y];
        for x := 0 to Bmp.Width - 1 do
        begin
          P := Row^[x];
          P.rgbtRed   := 255 - P.rgbtRed;
          P.rgbtGreen := 255 - P.rgbtGreen;
          P.rgbtBlue  := 255 - P.rgbtBlue;
          Row^[x] := P;
        end;
      end;
    end;

begin
//
end;


procedure TForm9.Button6Click(Sender: TObject);

  procedure GrayscaleBitmap32(const Bmp: TBitmap);
  var
    y, x: Integer;
    Row: PRGBQuadArray;
    Gray: Byte;
    Q: TRGBQuad;
  begin
    Bmp.PixelFormat := pf32bit;

    for y := 0 to Bmp.Height - 1 do
    begin
      Row := Bmp.ScanLine[y];
      for x := 0 to Bmp.Width - 1 do
      begin
        Q := Row^[x];
        // simple luminance approximation
        Gray := Byte((Integer(Q.rgbRed) * 77 + Integer(Q.rgbGreen) * 150 + Integer(Q.rgbBlue) * 29) shr 8);
        Q.rgbRed := Gray; Q.rgbGreen := Gray; Q.rgbBlue := Gray;
        Row^[x] := Q;
      end;
    end;
  end;


begin

end;

procedure TForm9.Button7Click(Sender: TObject);

  var
    VBO: Cardinal;
    Vertices: array[0..2] of TVertex; // triangle

  procedure InitVBO;
  begin
    // Fill Vertices...
    Vertices[0].X := -0.5; Vertices[0].Y := -0.5; Vertices[0].Z := 0; Vertices[0].U := 0; Vertices[0].V := 0;
    Vertices[1].X :=  0.5; Vertices[1].Y := -0.5; Vertices[1].Z := 0; Vertices[1].U := 1; Vertices[1].V := 0;
    Vertices[2].X :=  0.0; Vertices[2].Y :=  0.5; Vertices[2].Z := 0; Vertices[2].U := 0.5; Vertices[2].V := 1;

    glGenBuffers(1, @VBO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, SizeOf(Vertices), @Vertices[0], GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;

begin

end;


procedure TForm9.Button8Click(Sender: TObject);

   {
      When a VBO is bound, the last parameter of glVertexPointer/glTexCoordPointer is treated as a byte offset, not a CPU pointer.
   }

  procedure DrawVBO;
  const
    Stride = SizeOf(TVertex);
    OffPos = 0;
    OffUV  = 3 * SizeOf(Single); // X,Y,Z are 3 singles
  begin
   // glBindBuffer(GL_ARRAY_BUFFER, VBO);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    glVertexPointer(3, GL_FLOAT, Stride, Pointer(NativeInt(OffPos)));
    glTexCoordPointer(2, GL_FLOAT, Stride, Pointer(NativeInt(OffUV)));

    glDrawArrays(GL_TRIANGLES, 0, 3);

    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_VERTEX_ARRAY);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;



begin

end;

procedure TForm9.Button9Click(Sender: TObject);

  procedure UploadBitmapToTexture(const Bmp: TBitmap; Texture: Cardinal);
  var
    y: Integer;
    Row: Pointer;
  begin
    Bmp.PixelFormat := pf32bit;

    glBindTexture(GL_TEXTURE_2D, Texture);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);

    // Allocate texture storage once
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, Bmp.Width, Bmp.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, nil);

    // Upload each row (handles bitmap stride safely)
    for y := 0 to Bmp.Height - 1 do
    begin
      Row := Bmp.ScanLine[y];
      glTexSubImage2D(GL_TEXTURE_2D, 0, 0, y, Bmp.Width, 1, GL_BGRA, GL_UNSIGNED_BYTE, Row);
    end;

    glBindTexture(GL_TEXTURE_2D, 0);
  end;


begin

end;

end.
