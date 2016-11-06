object Form12: TForm12
  Left = 0
  Top = 0
  Caption = 'Form12'
  ClientHeight = 240
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 193
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Novo Plugin'
    TabOrder = 0
    OnClick = Button1Click
  end
  object MainMenu1: TMainMenu
    Left = 32
    Top = 16
    object Plugins1: TMenuItem
      Caption = 'Plugins'
    end
  end
  object PluginManager1: TPluginManager
    Filename = 'Plugin.ini'
    LocalPluginPath = '{app}\plugins'
    Active = True
    MainMenu = MainMenu1
    MenuItem = Plugins1
    Left = 48
    Top = 88
  end
end
