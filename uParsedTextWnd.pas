/// <summary>
/// Displays a parsed text and a description of each symbol.
/// </summary>
unit uParsedTextWnd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  Grids, Vcl.ExtCtrls;

type
  TFormParsedText = class(TForm)
    /// <summary>
    ///   Displays a grid of symbols that contains in a source text.
    /// </summary>
    StringGridParsedText: TStringGrid;
    /// <summary>
    ///   Displays an symbol description for any symbol in text.
    /// </summary>
    MemoSymbolDescription: TMemo;
    /// <summary>
    ///   Displays a line of symbols for each row of StringGridParsedText. <br />
    /// </summary>
    StringGridSrcText: TStringGrid;
    /// <summary>
    ///   Left-right splitter.
    /// </summary>
    SplitterLR: TSplitter;
    /// <summary>
    /// Regroup symbols on form resize.
    /// </summary>
    procedure FormResize(Sender: TObject);
    /// <summary>
    /// Show symbol description on cell select.
    /// </summary>
    procedure StringGridParsedTextSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
  public
    /// <summary>
    /// Set new string for parsing.
    /// </summary>
    function SetTextToParse(textToParse: string): Boolean;
  end;

var
  /// <summary>
  /// This form displays a parsed text and a description of each symbol. <br />
  /// </summary>
  FormParsedText: TFormParsedText;

implementation

{$R *.dfm}

uses Math, uMainWnd, System.Character, System.RegularExpressions;

procedure TFormParsedText.FormResize(Sender: TObject);
// Regroup symbols in the grid
var
  i: Integer;
begin
  // Clear every column
  for i := 0 to StringGridParsedText.ColCount - 1 do
  begin
    StringGridParsedText.Cols[i].Clear;
  end;
  // Calculate an column count
  StringGridParsedText.ColCount :=
    Floor((StringGridParsedText.Width - (GetSystemMetrics(SM_CXVSCROLL) * 3)) /
    StringGridParsedText.DefaultColWidth) - 1;
  StringGridSrcText.DefaultColWidth := StringGridSrcText.Width - 5;
  // Parse a text again
  FormMain.ButtonParseClick(Self);
end;

function TFormParsedText.SetTextToParse(textToParse: string): Boolean;
var
  i, cl, rw: Integer;
  GridRect: TGridRect;
  CanSelect : Boolean;
begin
  try
    // Clear tables
    StringGridParsedText.RowCount := 1;
    StringGridParsedText.Rows[0].Clear;
    StringGridSrcText.Rows[0].Clear;
    // Init
    cl := 0;
    rw := 0;
    GridRect.Left := 0;
    GridRect.Top := 0;
    GridRect.Right := 0;
    GridRect.Bottom := 0;
    CanSelect := True;
    // Calculate an row count
    StringGridParsedText.RowCount := Ceil(textToParse.Length / StringGridParsedText.ColCount);
    StringGridSrcText.RowCount := StringGridParsedText.RowCount;
    // Fill the table
    for i := low(textToParse) to high(textToParse) do
    begin
      StringGridParsedText.Cells[cl, rw] := textToParse[i];
      // Replace all line breaks by space character
      StringGridSrcText.Cells[0, rw] := TRegEx.Replace(StringGridParsedText.Rows[rw].Text,
        '(?s)(\R)', ' ', [roIgnoreCase, roMultiLine]);
      // Loop control
      Inc(cl);
      if cl = StringGridParsedText.ColCount then
      begin
        cl := 0;
        Inc(rw);
      end;
    end;

    StringGridParsedText.Selection := GridRect;
    StringGridParsedTextSelectCell(Self, GridRect.Left, GridRect.Top, CanSelect);

    Result := True;
  except
    Result := False;
  end;
end;

/// <summary>
/// Get a name of an unicode category.
/// </summary>
function GetUCategoryName(AUnicodeCategory: TUnicodeCategory): string;
begin
  case AUnicodeCategory of
    TUnicodeCategory.ucControl:
      Result := 'ucControl';
    TUnicodeCategory.ucFormat:
      Result := 'ucFormat';
    TUnicodeCategory.ucUnassigned:
      Result := 'ucUnassigned';
    TUnicodeCategory.ucPrivateUse:
      Result := 'ucPrivateUse';
    TUnicodeCategory.ucSurrogate:
      Result := 'ucSurrogate';
    TUnicodeCategory.ucLowercaseLetter:
      Result := 'ucLowercaseLetter';
    TUnicodeCategory.ucModifierLetter:
      Result := 'ucModifierLetter';
    TUnicodeCategory.ucOtherLetter:
      Result := 'ucOtherLetter';
    TUnicodeCategory.ucTitlecaseLetter:
      Result := 'ucTitlecaseLetter';
    TUnicodeCategory.ucUppercaseLetter:
      Result := 'ucUppercaseLetter';
    TUnicodeCategory.ucCombiningMark:
      Result := 'ucCombiningMark';
    TUnicodeCategory.ucEnclosingMark:
      Result := 'ucEnclosingMark';
    TUnicodeCategory.ucNonSpacingMark:
      Result := 'ucNonSpacingMark';
    TUnicodeCategory.ucDecimalNumber:
      Result := 'ucDecimalNumber';
    TUnicodeCategory.ucLetterNumber:
      Result := 'ucLetterNumber';
    TUnicodeCategory.ucOtherNumber:
      Result := 'ucOtherNumber';
    TUnicodeCategory.ucConnectPunctuation:
      Result := 'ucConnectPunctuation';
    TUnicodeCategory.ucDashPunctuation:
      Result := 'ucDashPunctuation';
    TUnicodeCategory.ucClosePunctuation:
      Result := 'ucClosePunctuation';
    TUnicodeCategory.ucFinalPunctuation:
      Result := 'ucFinalPunctuation';
    TUnicodeCategory.ucInitialPunctuation:
      Result := 'ucInitialPunctuation';
    TUnicodeCategory.ucOtherPunctuation:
      Result := 'ucOtherPunctuation';
    TUnicodeCategory.ucOpenPunctuation:
      Result := 'ucOpenPunctuation';
    TUnicodeCategory.ucCurrencySymbol:
      Result := 'ucCurrencySymbol';
    TUnicodeCategory.ucModifierSymbol:
      Result := 'ucModifierSymbol';
    TUnicodeCategory.ucMathSymbol:
      Result := 'ucMathSymbol';
    TUnicodeCategory.ucOtherSymbol:
      Result := 'ucOtherSymbol';
    TUnicodeCategory.ucLineSeparator:
      Result := 'ucLineSeparator';
    TUnicodeCategory.ucParagraphSeparator:
      Result := 'ucParagraphSeparator';
    TUnicodeCategory.ucSpaceSeparator:
      Result := 'ucSpaceSeparator';
  else
    Result := 'Unknown';
  end;
end;

procedure TFormParsedText.StringGridParsedTextSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
// Display new symbol description
begin
  MemoSymbolDescription.Clear;
  if Length(StringGridParsedText.Cells[ACol, ARow]) > 0 then
  begin
    MemoSymbolDescription.Lines.Add(StringGridParsedText.Cells[ACol, ARow]);
    MemoSymbolDescription.Lines.Add('Unicode number (DEC): ' +
      IntToStr(Ord(StringGridParsedText.Cells[ACol, ARow][1])));
    MemoSymbolDescription.Lines.Add('Unicode number (HEX): ' +
      IntToHex(Ord(StringGridParsedText.Cells[ACol, ARow][1]), 4));
    MemoSymbolDescription.Lines.Add('Unicode category: ' +
      GetUCategoryName(StringGridParsedText.Cells[ACol, ARow][1].GetUnicodeCategory));
  end;
end;

end.
