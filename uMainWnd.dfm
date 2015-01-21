object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Please enter the text below'
  ClientHeight = 213
  ClientWidth = 455
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object MemoSrc: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 449
    Height = 157
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 129
  end
  object ButtonParse: TButton
    Left = 0
    Top = 188
    Width = 455
    Height = 25
    Align = alBottom
    Caption = 'Parse'
    TabOrder = 1
    OnClick = ButtonParseClick
    ExplicitTop = 160
  end
  object ButtonClear: TButton
    Left = 0
    Top = 163
    Width = 455
    Height = 25
    Align = alBottom
    Caption = 'Clear'
    TabOrder = 2
    OnClick = ButtonClearClick
    ExplicitTop = 135
  end
end
