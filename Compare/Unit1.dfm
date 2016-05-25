object Form1: TForm1
  Left = 87
  Top = 128
  Caption = 'Folder Mirror'
  ClientHeight = 681
  ClientWidth = 1001
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    1001
    681)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1001
    Height = 241
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1001
      241)
    object Label3: TLabel
      Left = 576
      Top = 88
      Width = 38
      Height = 13
      Caption = 'Info log:'
    end
    object Label4: TLabel
      Left = 216
      Top = 8
      Width = 32
      Height = 13
      Caption = 'Tasks:'
    end
    object Label5: TLabel
      Left = 344
      Top = 8
      Width = 30
      Height = 13
      Caption = 'Paths:'
    end
    object B_LaunchCompare: TButton
      Left = 520
      Top = 128
      Width = 129
      Height = 25
      Caption = 'Compare'
      TabOrder = 0
      OnClick = LaunchCompareClick
    end
    object DirectoryListBox2: TDirectoryListBox
      Left = 792
      Top = 32
      Width = 201
      Height = 201
      Anchors = [akLeft, akTop, akBottom]
      TabOrder = 1
    end
    object DriveComboBox2: TDriveComboBox
      Left = 872
      Top = 8
      Width = 121
      Height = 22
      DirList = DirectoryListBox2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
    object Memo1: TMemo
      Left = 216
      Top = 160
      Width = 569
      Height = 73
      Anchors = [akLeft, akTop, akBottom]
      TabOrder = 3
    end
    object B_StopCompare: TButton
      Left = 656
      Top = 128
      Width = 129
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 4
      OnClick = StopCompareClick
    end
    object DirectoryListBox1: TDirectoryListBox
      Left = 8
      Top = 32
      Width = 201
      Height = 201
      Anchors = [akLeft, akTop, akBottom]
      TabOrder = 5
    end
    object DriveComboBox1: TDriveComboBox
      Left = 8
      Top = 8
      Width = 120
      Height = 22
      DirList = DirectoryListBox1
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
    end
    object btnTaskAdd: TButton
      Left = 216
      Top = 128
      Width = 49
      Height = 25
      Caption = 'Add'
      TabOrder = 7
      OnClick = btnTaskAddClick
    end
    object btnTaskRem: TButton
      Left = 272
      Top = 128
      Width = 65
      Height = 25
      Caption = 'Remove'
      TabOrder = 8
      OnClick = btnTaskRemClick
    end
    object lstPaths: TListView
      Left = 344
      Top = 24
      Width = 441
      Height = 97
      Checkboxes = True
      Columns = <
        item
          Caption = 'From'
          Width = 200
        end
        item
          Caption = 'To'
          Width = 200
        end>
      ColumnClick = False
      GridLines = True
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 9
      ViewStyle = vsReport
      OnClick = ListBox1Click
      OnItemChecked = lstPathsItemChecked
    end
    object lstTasks: TListBox
      Left = 216
      Top = 24
      Width = 121
      Height = 97
      ItemHeight = 13
      TabOrder = 10
      OnClick = lstTasksClick
    end
    object btnPathAdd: TButton
      Left = 344
      Top = 128
      Width = 49
      Height = 25
      Caption = 'Add'
      TabOrder = 11
      OnClick = btnPathAddClick
    end
    object btnPathRem: TButton
      Left = 400
      Top = 128
      Width = 65
      Height = 25
      Caption = 'Remove'
      TabOrder = 12
      OnClick = btnPathRemClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 240
    Width = 497
    Height = 441
    Anchors = [akLeft, akTop, akBottom]
    BevelOuter = bvNone
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      497
      441)
    object Label1: TLabel
      Left = 8
      Top = 424
      Width = 151
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'C:\...\Desktop\Delphi\Compare'
      ExplicitTop = 384
    end
    object ListView1: TListView
      Left = 8
      Top = 40
      Width = 481
      Height = 381
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Path'
        end
        item
          Caption = 'Date'
        end
        item
          Caption = 'Size'
        end
        item
          Caption = 'Type'
        end>
      GridLines = True
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      SmallImages = ImageList1
      TabOrder = 0
      ViewStyle = vsReport
      OnCustomDrawItem = ListViewCustomDrawItem
      OnDblClick = ListViewDblClick
    end
    object B_CopyOver1: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Copy over   >>>'
      Enabled = False
      TabOrder = 1
      OnClick = B_CopyOverClick
    end
    object B_Open1: TButton
      Left = 104
      Top = 8
      Width = 41
      Height = 25
      Caption = 'Open'
      TabOrder = 2
      OnClick = B_OpenClick
    end
    object B_OpenF1: TButton
      Left = 152
      Top = 8
      Width = 105
      Height = 25
      Caption = 'Open parent folder'
      TabOrder = 3
      OnClick = B_OpenClick
    end
    object B_Delete1: TButton
      Left = 264
      Top = 8
      Width = 73
      Height = 25
      Caption = 'Delete   XXX'
      Enabled = False
      TabOrder = 4
      OnClick = B_DeleteClick
    end
  end
  object Panel3: TPanel
    Left = 496
    Top = 240
    Width = 505
    Height = 441
    Anchors = [akLeft, akTop, akBottom]
    BevelOuter = bvNone
    Color = clSkyBlue
    ParentBackground = False
    TabOrder = 2
    DesignSize = (
      505
      441)
    object Label2: TLabel
      Left = 9
      Top = 424
      Width = 152
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'D:\...\Desktop\Delphi\Compare'
      ExplicitTop = 384
    end
    object ListView2: TListView
      Left = 8
      Top = 40
      Width = 489
      Height = 381
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Path'
        end
        item
          Caption = 'Date'
        end
        item
          Caption = 'Size'
        end
        item
          Caption = 'Type'
        end>
      GridLines = True
      HideSelection = False
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      SmallImages = ImageList1
      TabOrder = 0
      ViewStyle = vsReport
      OnCustomDrawItem = ListViewCustomDrawItem
      OnDblClick = ListViewDblClick
    end
    object B_Delete2: TButton
      Left = 264
      Top = 8
      Width = 73
      Height = 25
      Caption = 'Delete   XXX'
      Enabled = False
      TabOrder = 1
      OnClick = B_DeleteClick
    end
    object B_Open2: TButton
      Left = 104
      Top = 8
      Width = 41
      Height = 25
      Caption = 'Open'
      TabOrder = 2
      OnClick = B_OpenClick
    end
    object B_CopyOver2: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Copy over   <<<'
      Enabled = False
      TabOrder = 3
      OnClick = B_CopyOverClick
    end
    object B_OpenF2: TButton
      Left = 152
      Top = 8
      Width = 105
      Height = 25
      Caption = 'Open parent folder'
      TabOrder = 4
      OnClick = B_OpenClick
    end
  end
  object ImageList1: TImageList
    DrawingStyle = dsTransparent
    Left = 24
    Top = 326
    Bitmap = {
      494C010101000500040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D7D7D700C2C2C200B8B8
      B800B8B8B800B8B8B800B8B8B800B8B8B800B8B8B800B8B8B800B8B8B800B8B8
      B800B8B8B800C2C2C200D7D7D700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7D7D700ADADAD00787878006363
      6300636363006363630063636300636363006363630063636300636363006363
      63006363630078787800ADADAD00D7D7D7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C2C2C2000D73A5000D73A5000D73
      A5000D73A5000D73A5000D73A5000D73A5000D73A5000D73A5000D73A5000D73
      A5000D73A5005959590078787800C2C2C2000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC6001C9CC7009CFFFF006CD7
      FF006CD7FF006CD7FF006CD7FF006CD7FF006CD7FF006CD7FF006CD7FF006CD7
      FF002999BF000D73A50063636300B8B8B8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC6001A9AC6007AE4F0009CFF
      FF007CE3FF007CE3FF007CE3FF007CE3FF007CE3FF007CE3FF007CE3FF007CDF
      FF0043B2DE001A7B9D0059595900A3A3A3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC60026A2CF0040B8D7009CFF
      FF0084EBFF0084EBFF0084EBFF0084EBFF0084EBFF0084EBFF0084EBFF0084E7
      FF0043BAEF00199AC60059595900787878000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC60043B3E20021A0C900A5FF
      FF0094F7FF0094F7FF0094F7FF0094F7FF0094F7FF0094F7FF0094F7FF0094F7
      FF0053BEE7005CBCCE000D73A500636363000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC60070D5FD00199AC60089F0
      F7009CFFFF009CFFFF009CFFFF009CFFFF009CFFFF009CFFFF009CFFFF009CFF
      FF005BC7FF0096F9FB00197B9B00636363000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC60084D7FF00199AC6006CBF
      DA00FFFFFF00FFFFFF00F7FBFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084E7FF00FFFFFF00197EA100787878000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC60084EBFF0050C1E200199A
      C600199AC600199AC600199AC600199AC600199AC600199AC600199AC600199A
      C600199AC600199AC6001989B100ADADAD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC6009CF3FF008CF3FF008CF3
      FF008CF3FF008CF3FF008CF3FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00199AC6001A7B9D00ADADAD00D7D7D7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000199AC600FFFFFF009CFFFF009CFF
      FF009CFFFF009CFFFF00FFFFFF00199AC600199AC600199AC600199AC600199A
      C600199AC600C2C2C200D7D7D700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000022A2CE00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00199AC600ADADAD00D7D7D70000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000022A2CE0022A2
      CE0022A2CE0022A2CE00C2C2C200D7D7D7000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF0000000000008001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000001000000000000807F000000000000
      C0FF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
end
