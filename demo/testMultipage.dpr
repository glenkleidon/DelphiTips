program testMultipage;

uses
  Forms,
  activex,
  fmtestMultipage in 'fmtestMultipage.pas' {Form1},
  MultipageImage in '..\MultiPageImage.pas';

{$R *.res}

begin
//  coinitialize(nil);
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
//  couninitialize;
end.
