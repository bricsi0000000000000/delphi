object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 100
  ClientWidth = 356
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
  object AddNumbersButton: TButton
    Left = 8
    Top = 8
    Width = 340
    Height = 25
    Caption = 'Add numbers'
    TabOrder = 0
    OnClick = AddNumbersButtonClick
  end
  object ResultMemo: TMemo
    Left = 8
    Top = 39
    Width = 340
    Height = 42
    Hint = 'Result'
    Lines.Strings = (
      'ResultMemo')
    ReadOnly = True
    ScrollBars = ssHorizontal
    TabOrder = 1
  end
end
