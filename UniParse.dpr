﻿/// <summary>
///   This application can parse a string into individual characters.
/// </summary>
program UniParse;

uses
  Forms,
  uMainWnd in 'uMainWnd.pas' {FormMain},
  uParsedTextWnd in 'uParsedTextWnd.pas' {FormParsedText};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormParsedText, FormParsedText);
  Application.Run;
end.
