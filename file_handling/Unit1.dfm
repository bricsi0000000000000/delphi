object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 85
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 265
    Height = 25
    TabOrder = 0
  end
  object SaveButton: TButton
    Left = 8
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = SaveButtonClick
  end
  object ImportButton: TButton
    Left = 89
    Top = 39
    Width = 75
    Height = 25
    Caption = 'Import'
    TabOrder = 2
    OnClick = ImportButtonClick
  end
end
