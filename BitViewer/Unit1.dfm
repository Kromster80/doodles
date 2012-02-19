object Form1: TForm1
  Left = 118
  Top = 83
  HorzScrollBar.Smooth = True
  VertScrollBar.Smooth = True
  Caption = 'Bit Viewer'
  ClientHeight = 351
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  DesignSize = (
    742
    351)
  PixelsPerInch = 96
  TextHeight = 13
  object Image2: TImage
    Left = 236
    Top = 0
    Width = 512
    Height = 360
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object DriveComboBox1: TDriveComboBox
    Left = 0
    Top = 1
    Width = 129
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 0
  end
  object FileListBox1: TFileListBox
    Left = 0
    Top = 144
    Width = 129
    Height = 217
    ItemHeight = 13
    TabOrder = 1
    OnChange = LoadFile
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 0
    Top = 25
    Width = 129
    Height = 113
    FileList = FileListBox1
    TabOrder = 2
  end
  object CBFlipH: TCheckBox
    Left = 136
    Top = 0
    Width = 89
    Height = 17
    Caption = 'Flip Horizontal'
    TabOrder = 3
    OnClick = Redraw
  end
  object CBFlipV: TCheckBox
    Left = 136
    Top = 16
    Width = 89
    Height = 17
    Caption = 'Flip Vertical'
    TabOrder = 4
    OnClick = Redraw
  end
  object ComboBox1: TComboBox
    Left = 132
    Top = 40
    Width = 73
    Height = 21
    ItemIndex = 3
    TabOrder = 5
    Text = '256'
    OnChange = Redraw
    Items.Strings = (
      '32'
      '64'
      '128'
      '256'
      '512'
      '640'
      '768'
      '800'
      '1024')
  end
  object RadioGroup1: TRadioGroup
    Left = 132
    Top = 72
    Width = 101
    Height = 153
    Caption = 'Bit Depth'
    ItemIndex = 5
    Items.Strings = (
      '1 - Black&White'
      '4 - GrayScale'
      '8 - GrayScale'
      '15 - 5.5.5'
      '16 - 5.6.5'
      '24 - 8.8.8'
      '32 - 8.8.8.0')
    TabOrder = 6
    OnClick = Redraw
  end
  object RadioGroup2: TRadioGroup
    Left = 132
    Top = 236
    Width = 101
    Height = 113
    Caption = 'Zoom'
    ItemIndex = 2
    Items.Strings = (
      '1/4x'
      '1/2x'
      '1x'
      '2x'
      '4x')
    TabOrder = 7
    OnClick = Redraw
  end
end
