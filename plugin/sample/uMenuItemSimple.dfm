object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 70
  ClientWidth = 236
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 135
    Top = 19
    Width = 75
    Height = 25
    Caption = 'Fechar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 23
    Width = 121
    Height = 21
    EditLabel.Width = 61
    EditLabel.Height = 13
    EditLabel.Caption = 'LabeledEdit1'
    TabOrder = 1
  end
end
