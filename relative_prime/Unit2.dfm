object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Relative prime'
  ClientHeight = 118
  ClientWidth = 222
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
    Width = 64
    Height = 13
    Caption = 'First number:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 78
    Height = 13
    Caption = 'Second number:'
  end
  object ResultLabel: TLabel
    Left = 8
    Top = 95
    Width = 205
    Height = 13
    Alignment = taCenter
    Caption = '12 and 24 are relative primes'
    Layout = tlCenter
  end
  object SecondNumberEdit: TEdit
    Left = 92
    Top = 37
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object FirstNumberEdit: TEdit
    Left = 92
    Top = 5
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object CalculateButton: TButton
    Left = 9
    Top = 64
    Width = 205
    Height = 25
    Caption = 'Calculate'
    TabOrder = 2
    OnClick = CalculateButtonClick
  end
end
