program testMultipage;

uses
  Forms,
  activex,
  fmtestMultipage in 'fmtestMultipage.pas' {Form1},
  Graphic.Multipage in '..\Graphic.Multipage.pas';

{$R *.res}

begin
//  coinitialize(nil);
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
//  couninitialize;
end.
