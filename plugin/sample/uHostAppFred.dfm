object Form25: TForm25
  Left = 0
  Top = 0
  Caption = 'Form25'
  ClientHeight = 547
  ClientWidth = 637
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu1: TMainMenu
    Left = 152
    Top = 40
    object Arquivo1: TMenuItem
      Caption = 'Arquivo'
    end
    object Ferramentas1: TMenuItem
      Caption = 'Ferramentas'
    end
    object Plugins1: TMenuItem
      Caption = 'Plugins'
      object CarregarnovosPlugins1: TMenuItem
        Caption = 'Carregar novos Plugins'
        OnClick = CarregarnovosPlugins1Click
      end
    end
  end
  object PluginManager1: TPluginManager
    Filename = 'Plugin.ini'
    LocalPluginPath = '{app}\Plugins'
    MainMenu = MainMenu1
    MenuItem = Plugins1
    Active = True
    Left = 152
    Top = 96
  end
end
