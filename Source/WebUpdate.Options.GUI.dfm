object FormOptions: TFormOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 305
  ClientWidth = 525
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    Left = 208
    Top = 0
    Width = 317
    Height = 272
    ActivePage = TabSheetFTP
    Align = alClient
    Style = tsButtons
    TabOrder = 0
    object TabSheetMain: TTabSheet
      Caption = 'Main'
      TabVisible = False
      DesignSize = (
        309
        262)
      object LabelFileName: TLabel
        Left = 3
        Top = 11
        Width = 74
        Height = 13
        Caption = 'Base Directory:'
      end
      object LabelChannelFileName: TLabel
        Left = 3
        Top = 59
        Width = 88
        Height = 13
        Caption = 'Channel Filename:'
      end
      object EditChannelFileName: TJvFilenameEdit
        Left = 3
        Top = 75
        Width = 303
        Height = 21
        OnAfterDialog = EditChannelFileNameAfterDialog
        Filter = 'JSON files (*.json)|*.json'
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object EditBaseDirectory: TJvDirectoryEdit
        Left = 3
        Top = 27
        Width = 303
        Height = 21
        DialogKind = dkWin32
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
    end
    object TabSheetFTP: TTabSheet
      Caption = 'Copy / Upload'
      ImageIndex = 1
      TabVisible = False
      object CheckBoxAutoCopyUpload: TCheckBox
        Left = 0
        Top = 0
        Width = 309
        Height = 22
        Align = alTop
        Caption = 'Automatically Copy/Upload on Snapshot'
        TabOrder = 0
      end
      object GroupBoxFTP: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 82
        Width = 303
        Height = 169
        Align = alTop
        Caption = 'FTP'
        TabOrder = 1
        DesignSize = (
          303
          169)
        object LabelPassword: TLabel
          Left = 11
          Top = 115
          Width = 50
          Height = 13
          Caption = 'Password:'
        end
        object LabelUsername: TLabel
          Left = 11
          Top = 67
          Width = 52
          Height = 13
          Caption = 'Username:'
        end
        object LabelServer: TLabel
          Left = 11
          Top = 19
          Width = 36
          Height = 13
          Caption = 'Server:'
        end
        object EditFtpPassword: TEdit
          Left = 11
          Top = 131
          Width = 282
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object EditFtpUsername: TEdit
          Left = 11
          Top = 83
          Width = 282
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
        object EditFtpServer: TEdit
          Left = 11
          Top = 35
          Width = 282
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
        end
      end
      object GroupBoxCopy: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 25
        Width = 303
        Height = 51
        Align = alTop
        Caption = 'Copy'
        TabOrder = 2
        DesignSize = (
          303
          51)
        object CheckBoxCopyTo: TCheckBox
          Left = 11
          Top = 18
          Width = 63
          Height = 23
          Caption = 'Copy to:'
          TabOrder = 0
        end
        object EditCopyPath: TEdit
          Left = 80
          Top = 19
          Width = 213
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
        end
      end
    end
  end
  object TreeOptions: TVirtualStringTree
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 200
    Height = 264
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    TabOrder = 1
    OnChange = TreeOptionsChange
    OnGetText = TreeOptionsGetText
    Columns = <>
  end
  object Panel: TPanel
    Left = 0
    Top = 272
    Width = 525
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object ButtonOK: TButton
      AlignWithMargins = True
      Left = 363
      Top = 4
      Width = 75
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object ButtonCancel: TButton
      AlignWithMargins = True
      Left = 446
      Top = 4
      Width = 75
      Height = 25
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alRight
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
