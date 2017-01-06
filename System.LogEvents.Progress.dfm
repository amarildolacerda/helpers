object ProgressEvents: TProgressEvents
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  Caption = 'Executando......'
  ClientHeight = 491
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 423
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
    ExplicitWidth = 387
  end
  object Panel2: TPanel
    Left = 0
    Top = 445
    Width = 429
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    Color = clBtnHighlight
    ParentBackground = False
    TabOrder = 1
    ExplicitTop = 218
    ExplicitWidth = 393
    DesignSize = (
      429
      46)
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 429
      Height = 50
      Align = alTop
      ExplicitLeft = 392
      ExplicitTop = 16
      ExplicitWidth = 50
    end
    object SpeedButton1: TSpeedButton
      Left = 315
      Top = 4
      Width = 108
      Height = 37
      Anchors = [akTop, akRight]
      Caption = 'OK'
      OnClick = SpeedButton1Click
      ExplicitLeft = 279
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
      Width = 194
      Height = 33
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      WordWrap = True
      OnClick = lblErroClick
      ExplicitWidth = 158
    end
    object lbTimer: TLabel
      Left = 20
      Top = 31
      Width = 3
      Height = 13
    end
    object cbRolarLista: TCheckBox
      Left = 3
      Top = 2
      Width = 97
      Height = 17
      Caption = 'rolar a lista'
      TabOrder = 0
      Visible = False
    end
  end
  object ValueListEditor1: TValueListEditor
    Left = 0
    Top = 47
    Width = 429
    Height = 398
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
    ExplicitWidth = 393
    ExplicitHeight = 171
    ColWidths = (
      300
      127)
    RowHeights = (
      18
      18)
  end
end
