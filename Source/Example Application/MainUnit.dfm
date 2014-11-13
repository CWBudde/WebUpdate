object FormExample: TFormExample
  Left = 0
  Top = 0
  Caption = 'Example Application'
  ClientHeight = 128
  ClientWidth = 201
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LabelChannel: TLabel
    Left = 8
    Top = 11
    Width = 43
    Height = 13
    Caption = 'Channel:'
  end
  object ButtonCheckUpdates: TButton
    Left = 8
    Top = 35
    Width = 185
    Height = 25
    Caption = '&Check for Updates'
    Default = True
    TabOrder = 0
    OnClick = ButtonCheckUpdatesClick
  end
  object ButtonExit: TButton
    Left = 8
    Top = 97
    Width = 185
    Height = 25
    Caption = 'E&xit'
    TabOrder = 1
    OnClick = ButtonExitClick
  end
  object ButtonPerformUpdate: TButton
    Left = 8
    Top = 66
    Width = 185
    Height = 25
    Caption = 'Perform &Update'
    TabOrder = 2
    OnClick = ButtonPerformUpdateClick
  end
  object ComboBoxChannels: TComboBox
    Left = 57
    Top = 8
    Width = 136
    Height = 21
    Style = csDropDownList
    TabOrder = 3
    OnChange = ComboBoxChannelsChange
  end
end
