unit uParsedTextWnd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Vcl.ExtCtrls;

type
  TFormParsedText = class(TForm)
    StringGridParsedText: TStringGrid;
    MemoParsedText: TMemo;
    StringGridSrcText: TStringGrid;
    SplitterLR: TSplitter;
    procedure FormResize(Sender: TObject);
    procedure StringGridParsedTextSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure SplitterLRMoved(Sender: TObject);
  private
    { Private declarations }
  public
    function SetTextToParse(textToParse: string): Boolean;
  end;

var
  FormParsedText: TFormParsedText;

implementation

{$R *.dfm}

uses Math, uMainWnd, System.Character;

procedure TFormParsedText.FormResize(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to StringGridParsedText.ColCount-1 do
  begin
    StringGridParsedText.Cols[i].Clear;
  end;
  StringGridParsedText.ColCount :=
    Floor((StringGridParsedText.Width - (GetSystemMetrics(SM_CXVSCROLL) * 3)) /
    StringGridParsedText.DefaultColWidth) - 1;
  StringGridSrcText.DefaultColWidth := StringGridSrcText.Width - 5;
  FormMain.ButtonParseClick(Self);
end;

function TFormParsedText.SetTextToParse(textToParse: string): Boolean;
var
  i, cl, rw: Integer;
  GridRect: TGridRect;
  CSel: Boolean;
begin
  try
    // Очищаем таблицу
    StringGridParsedText.RowCount := 1;
    StringGridParsedText.Rows[0].Clear;
    StringGridSrcText.Rows[0].Clear;
    // Заполняем
    cl := 0;
    rw := 0;
    GridRect.Left := 0;
    GridRect.Top := 0;
    GridRect.Right := 0;
    GridRect.Bottom := 0;
    CSel := True;
    //
    StringGridParsedText.RowCount := Ceil(textToParse.Length / StringGridParsedText.ColCount);
    StringGridSrcText.RowCount := StringGridParsedText.RowCount;
    //
    for i := low(textToParse) to high(textToParse) do
    begin
      StringGridParsedText.Cells[cl, rw] := textToParse[i];
      // Тут можно было заменять все невидимые символы с помощью регэкспов, но мне лень:
      StringGridSrcText.Cells[0, rw] := StringReplace(StringGridParsedText.Rows[rw].Text,
        sLineBreak, ' ', [rfReplaceAll, rfIgnoreCase]);
      //---
      Inc(cl);
      if cl = StringGridParsedText.ColCount then
      begin
        cl := 0;
        Inc(rw);
      end;
    end;
    //
    StringGridParsedText.Selection := GridRect;
    StringGridParsedTextSelectCell(Self, GridRect.Left, GridRect.Top, CSel);
    //
    Result := True;
  except
    Result := False;
  end;
end;

procedure TFormParsedText.SplitterLRMoved(Sender: TObject);
begin
  FormResize(Self);
end;

function GetUniCatName(UniCat: TUnicodeCategory): string;
begin
  case UniCat of
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
begin
  MemoParsedText.Clear;
  if Length(StringGridParsedText.Cells[ACol, ARow]) > 0 then
  begin
    MemoParsedText.Lines.Add(StringGridParsedText.Cells[ACol, ARow]);
    MemoParsedText.Lines.Add('Unicode number (DEC): ' +
      IntToStr(Ord(StringGridParsedText.Cells[ACol, ARow][1])));
    MemoParsedText.Lines.Add('Unicode number (HEX): ' +
      IntToHex(Ord(StringGridParsedText.Cells[ACol, ARow][1]), 4));
    MemoParsedText.Lines.Add('Unicode category: ' + GetUniCatName(StringGridParsedText.Cells[ACol,
      ARow][1].GetUnicodeCategory));
  end;
end;

end.
