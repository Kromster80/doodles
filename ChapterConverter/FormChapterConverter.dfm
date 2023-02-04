object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Chapter Converter'
  ClientHeight = 521
  ClientWidth = 1001
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 123
    Height = 13
    Caption = 'Input - YouTube chapters'
  end
  object Label2: TLabel
    Left = 504
    Top = 16
    Width = 109
    Height = 13
    Caption = 'Output - MKV chapters'
  end
  object Label3: TLabel
    Left = 608
    Top = 480
    Width = 225
    Height = 33
    AutoSize = False
    Caption = 
      'Generated chapters can be imported into MKV file using MKVToolNi' +
      'x Chapter Editor tab'
    WordWrap = True
  end
  object btnConvert: TButton
    Left = 408
    Top = 480
    Width = 89
    Height = 25
    Caption = 'Convert'
    TabOrder = 0
    OnClick = btnConvertClick
  end
  object memInput: TMemo
    Left = 16
    Top = 32
    Width = 481
    Height = 441
    Lines.Strings = (
      '00:00 - Intro'
      '01:04 - Chapter 1'
      '02:51 - Credits')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object memOutput: TMemo
    Left = 504
    Top = 32
    Width = 481
    Height = 441
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object btnSave: TButton
    Left = 504
    Top = 480
    Width = 89
    Height = 25
    Caption = 'Save'
    TabOrder = 3
    OnClick = btnSaveClick
  end
  object sdSave: TSaveDialog
    DefaultExt = '.xml'
    Filter = 'Chapters (*.xml)|.xml'
    Left = 928
    Top = 40
  end
end
