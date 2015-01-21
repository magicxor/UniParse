object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Please enter the text below'
  ClientHeight = 372
  ClientWidth = 531
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LabelAbout: TLabel
    AlignWithMargins = True
    Left = 1
    Top = 358
    Width = 529
    Height = 13
    Margins.Left = 1
    Margins.Top = 5
    Margins.Right = 1
    Margins.Bottom = 1
    Align = alBottom
    Alignment = taCenter
    Caption = 'coded by: github.com/magicxor | mailparser@mail.ru'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 1973790
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    OnClick = LabelAboutClick
    ExplicitWidth = 252
  end
  object MemoSrc: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 525
    Height = 277
    Align = alClient
    TabOrder = 0
  end
  object ButtonParse: TButton
    Left = 0
    Top = 318
    Width = 531
    Height = 35
    Align = alBottom
    Caption = 'Parse'
    TabOrder = 1
    OnClick = ButtonParseClick
  end
  object ButtonClear: TButton
    Left = 0
    Top = 283
    Width = 531
    Height = 35
    Align = alBottom
    Caption = 'Clear'
    TabOrder = 2
    OnClick = ButtonClearClick
  end
end
