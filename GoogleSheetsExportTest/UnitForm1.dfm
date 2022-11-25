object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'GoogleSheetsExportTest'
  ClientHeight = 193
  ClientWidth = 297
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 40
    Top = 40
    Width = 209
    Height = 41
    AutoSize = False
    Caption = 
      'The gist is that Escaped data being pasted into GoogleSheets and' +
      'copied back should perfectly match the Original'
    WordWrap = True
  end
  object btnExportOriginal: TButton
    Left = 40
    Top = 112
    Width = 217
    Height = 25
    Caption = 'Place Original data into clipboard'
    TabOrder = 0
    OnClick = btnExportOriginalClick
  end
  object btnExportEscaped: TButton
    Left = 40
    Top = 144
    Width = 217
    Height = 25
    Caption = 'Place Escaped data into clipboard'
    TabOrder = 1
    OnClick = btnExportEscapedClick
  end
end
