program RecordUtilWSDemo;

uses
  Forms,
  fmRecordUtilWSDemo in 'fmRecordUtilWSDemo.pas' {Form1},
  RecordUtils in '..\RecordUtils.pas',
  WebCardsApi in 'WebCardsApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
