object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 66
  ClientWidth = 198
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
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object ComboBox1: TComboBox
    Left = 56
    Top = 5
    Width = 134
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 182
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
end