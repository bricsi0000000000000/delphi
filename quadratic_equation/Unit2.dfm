object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Quadratic equation'
  ClientHeight = 82
  ClientWidth = 231
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
    Left = 47
    Top = 11
    Width = 31
    Height = 13
    Caption = 'x^2 +'
  end
  object Label2: TLabel
    Left = 123
    Top = 11
    Width = 20
    Height = 13
    Caption = 'x + '
  end
  object Label3: TLabel
    Left = 188
    Top = 11
    Width = 17
    Height = 13
    Caption = '= 0'
  end
  object x1Label: TLabel
    Left = 97
    Top = 40
    Width = 30
    Height = 13
    Caption = 'x1 = -'
  end
  object x2Label: TLabel
    Left = 97
    Top = 59
    Width = 30
    Height = 13
    Caption = 'x2 = -'
  end
  object aEdit: TEdit
    Left = 8
    Top = 8
    Width = 33
    Height = 21
    TabOrder = 0
  end
  object bEdit: TEdit
    Left = 84
    Top = 8
    Width = 33
    Height = 21
    TabOrder = 1
  end
  object cEdit: TEdit
    Left = 149
    Top = 8
    Width = 33
    Height = 21
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Calculate'
    TabOrder = 3
    OnClick = Button1Click
  end
end
