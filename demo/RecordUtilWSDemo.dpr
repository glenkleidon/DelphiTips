program RecordUtilWSDemo;

uses
  Forms,
  fmRecordUtilWSDemo in 'fmRecordUtilWSDemo.pas' {ServerForm},
  RecordUtils in '..\RecordUtils.pas',
  WebCardsApi in 'WebCardsApi.pas',
  fmRecordUtilsWSClient in 'fmRecordUtilsWSClient.pas' {PlayerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
