object Form11: TForm11
  Left = 0
  Top = 0
  Caption = 'Form11'
  ClientHeight = 353
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 505
    Height = 29
    Caption = 'ToolBar1'
    TabOrder = 0
    ExplicitLeft = 280
    ExplicitTop = 80
    ExplicitWidth = 150
  end
  object MainMenu1: TMainMenu
    Left = 24
    Top = 8
    object Plugins1: TMenuItem
      Caption = 'Plugins'
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object InstallPlugins1: TMenuItem
        Caption = 'Install Plugins'
        OnClick = InstallPlugins1Click
      end
    end
  end
end
