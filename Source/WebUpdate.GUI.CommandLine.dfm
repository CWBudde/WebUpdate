object FormCommandLine: TFormCommandLine
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Command-line switches'
  ClientHeight = 310
  ClientWidth = 624
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
  object MemoCommandLine: TMemo
    Left = 0
    Top = 0
    Width = 624
    Height = 310
    Align = alClient
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -16
    Font.Name = 'Terminal_Hex'
    Font.Style = []
    Lines.Strings = (
      'Command-line switches'
      '---------------------'
      ''
      'Syntax: %s project.wup command [more commands] [-options]'
      ''
      '  project.wup must be replaced by your project name'
      ''
      'Commands:'
      '---------'
      ''
      '  Snapshot                     (take snapshot)'
      '  Copy                         (copy to path)'
      '  Upload                       (upload to server)'
      ''
      'Options:'
      '---------'
      ''
      
        '  -Channel:"channel name"      (with/without quotes, default: "N' +
        'ightly")'
      
        '  -FtpHost:host                (FTP host name, overrides project' +
        #39's default)'
      
        '  -FtpUser:username            (FTP user name, overrides project' +
        #39's default)'
      
        '  -FtpPassword:password        (FTP password, overrides project'#39 +
        's default)'
      '  -CopyPath:path               (Path of snapshot copies)')
    ParentFont = False
    ReadOnly = True
    TabOrder = 0
  end
end
