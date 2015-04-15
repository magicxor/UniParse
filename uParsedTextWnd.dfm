object FormParsedText: TFormParsedText
  Left = 0
  Top = 0
  Caption = 'Parsed Text'
  ClientHeight = 382
  ClientWidth = 896
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterLR: TSplitter
    Left = 660
    Top = 0
    Height = 287
    Align = alRight
    OnMoved = FormResize
    ExplicitLeft = 656
    ExplicitTop = 120
    ExplicitHeight = 100
  end
  object StringGridParsedText: TStringGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 654
    Height = 281
    Align = alClient
    ColCount = 25
    DefaultColWidth = 24
    FixedCols = 0
    RowCount = 11
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 0
    OnSelectCell = StringGridParsedTextSelectCell
  end
  object MemoSymbolDescription: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 290
    Width = 890
    Height = 89
    Align = alBottom
    ReadOnly = True
    TabOrder = 1
  end
  object StringGridSrcText: TStringGrid
    AlignWithMargins = True
    Left = 666
    Top = 3
    Width = 227
    Height = 281
    Align = alRight
    ColCount = 1
    DefaultColWidth = 200
    FixedCols = 0
    RowCount = 11
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 2
  end
end
