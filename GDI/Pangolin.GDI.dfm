object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnPaint = FormPaint
  TextHeight = 15
  object Button1: TButton
    Left = 256
    Top = 296
    Width = 113
    Height = 33
    Caption = 'Animate Images'
    TabOrder = 0
    OnClick = Button1Click
  end
  object tmrAnimation: TTimer
    Enabled = False
    Interval = 300
    OnTimer = tmrAnimationTimer
    Left = 304
    Top = 224
  end
end
