object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Calculate triangle'
  ClientHeight = 116
  ClientWidth = 283
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Hypotenuse'
  end
  object Label2: TLabel
    Left = 8
    Top = 35
    Width = 65
    Height = 13
    Caption = 'Opposite side'
  end
  object Label3: TLabel
    Left = 8
    Top = 62
    Width = 65
    Height = 13
    Caption = 'Adjacent side'
  end
  object PerimeterLabel: TLabel
    Left = 168
    Top = 16
    Width = 57
    Height = 13
    Caption = 'Perimeter: -'
  end
  object AreaLabel: TLabel
    Left = 168
    Top = 35
    Width = 34
    Height = 13
    Caption = 'Area: -'
  end
  object HypotenuseEdit: TEdit
    Left = 79
    Top = 5
    Width = 57
    Height = 21
    TabOrder = 0
  end
  object OppositeSideEdit: TEdit
    Left = 79
    Top = 32
    Width = 57
    Height = 21
    TabOrder = 1
  end
  object AdjacentSideEdit: TEdit
    Left = 79
    Top = 59
    Width = 57
    Height = 21
    TabOrder = 2
  end
  object CalculateButton: TButton
    Left = 8
    Top = 86
    Width = 128
    Height = 25
    Caption = 'Calculate'
    TabOrder = 3
    OnClick = CalculateButtonClick
  end
end
