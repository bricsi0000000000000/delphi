object Factorial: TFactorial
  Left = 0
  Top = 0
  Caption = 'Factorial calculator'
  ClientHeight = 74
  ClientWidth = 207
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 13
    Caption = 'Number:'
  end
  object ResultLabel: TLabel
    Left = 8
    Top = 40
    Width = 3
    Height = 13
  end
  object NumberEdit: TEdit
    Left = 55
    Top = 5
    Width = 121
    Height = 21
    TabOrder = 0
    TextHint = 'Integer'
    OnChange = NumberEditChange
  end
end
