unit uMainWnd;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    MemoSrc: TMemo;
    ButtonParse: TButton;
    ButtonClear: TButton;
    procedure ButtonParseClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses uParsedTextWnd;

procedure TFormMain.ButtonClearClick(Sender: TObject);
begin
  MemoSrc.Clear;
end;

procedure TFormMain.ButtonParseClick(Sender: TObject);
begin
  if Length(MemoSrc.Text) > 0 then
  begin
    FormParsedText.SetTextToParse(MemoSrc.Text);
    FormParsedText.Show;
  end;
end;

end.
