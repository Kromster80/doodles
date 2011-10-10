object Form1: TForm1
  Left = 315
  Top = 301
  Width = 808
  Height = 627
  Caption = 'Blending modes'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 800
    Height = 41
    Align = alTop
    TabOrder = 0
    object CheckBox1: TCheckBox
      Left = 8
      Top = 12
      Width = 73
      Height = 17
      Caption = 'Alpha-test'
      TabOrder = 0
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 88
      Top = 12
      Width = 65
      Height = 17
      Caption = 'Captions'
      TabOrder = 1
      OnClick = CheckBox2Click
    end
    object CheckBox3: TCheckBox
      Left = 160
      Top = 12
      Width = 57
      Height = 17
      Caption = 'Overlay'
      TabOrder = 2
      OnClick = CheckBox3Click
    end
    object CheckBox4: TCheckBox
      Left = 232
      Top = 12
      Width = 57
      Height = 17
      Caption = 'Shade'
      TabOrder = 3
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 304
      Top = 12
      Width = 73
      Height = 17
      Caption = 'TeamColor'
      TabOrder = 4
      OnClick = CheckBox5Click
    end
  end
end
