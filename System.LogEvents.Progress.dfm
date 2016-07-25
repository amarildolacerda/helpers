object ProgressEvents: TProgressEvents
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Executando......'
  ClientHeight = 264
  ClientWidth = 393
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
    Width = 387
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
    Top = 218
    Width = 393
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      393
      46)
    object SpeedButton1: TSpeedButton
      Left = 279
      Top = 4
      Width = 108
      Height = 37
      Anchors = [akTop, akRight]
      Caption = 'OK'
      OnClick = SpeedButton1Click
    end
    object lbPosition: TLabel
      Left = 20
      Top = 17
      Width = 3
      Height = 13
    end
    object lblErro: TLabel
      Left = 115
      Top = 6
      Width = 158
      Height = 33
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      WordWrap = True
      OnClick = lblErroClick
    end
    object lbTimer: TLabel
      Left = 20
      Top = 31
      Width = 3
      Height = 13
    end
    object cbRolarLista: TCheckBox
      Left = 3
      Top = -1
      Width = 97
      Height = 17
      Caption = 'rolar a lista'
      TabOrder = 0
    end
  end
  object ValueListEditor1: TValueListEditor
    Left = 0
    Top = 47
    Width = 393
    Height = 171
    Align = alClient
    BorderStyle = bsNone
    Color = clHighlightText
    DoubleBuffered = True
    FixedColor = clHighlightText
    Options = [goFixedVertLine, goFixedHorzLine, goHorzLine, goColSizing, goAlwaysShowEditor, goThumbTracking]
    ParentDoubleBuffered = False
    Strings.Strings = (
      '=')
    TabOrder = 2
    TitleCaptions.Strings = (
      'Atividade'
      'Fase')
    ExplicitHeight = 176
    ColWidths = (
      300
      91)
    RowHeights = (
      18
      18)
  end
end
