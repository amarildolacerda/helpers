object ProgressEvents: TProgressEvents
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Executando......'
  ClientHeight = 264
  ClientWidth = 412
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 406
    Height = 41
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Andamento do Processamento'
    Color = clBtnHighlight
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentBackground = False
    ParentFont = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 223
    Width = 412
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      412
      41)
    object SpeedButton1: TSpeedButton
      Left = 298
      Top = 4
      Width = 108
      Height = 32
      Anchors = [akTop, akRight]
      Caption = 'OK'
      OnClick = SpeedButton1Click
    end
    object lbPosition: TLabel
      Left = 19
      Top = 16
      Width = 3
      Height = 13
    end
    object lblErro: TLabel
      Left = 115
      Top = 6
      Width = 177
      Height = 33
      AutoSize = False
      WordWrap = True
      OnClick = lblErroClick
    end
  end
  object ValueListEditor1: TValueListEditor
    Left = 0
    Top = 47
    Width = 412
    Height = 176
    Align = alClient
    Color = clHotLight
    Strings.Strings = (
      '=')
    TabOrder = 2
    TitleCaptions.Strings = (
      'Atividade'
      'Fase')
    ColWidths = (
      300
      106)
    RowHeights = (
      18
      18)
  end
end
