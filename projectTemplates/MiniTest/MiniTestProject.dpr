program MiniTestProject;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  TestCases1 in 'TestCases1.pas',
  MiniTestFramework in 'MiniTestFramework.pas';

begin
  try

    Title('Test Cases For <Project> Units');
    RunTestSets;
    TestSummary;

    if sameText(Paramstr(1),'/p') then ReadLn;

    ExitCode := TotalErroredTests+TotalFailedTests;

  except
    on E: Exception do
	  Begin
      Writeln(
          'Test Framework Exception: ',
          'Test case:' ,CurrentTestCaseName,';',CurrentTestCaseLabel,
          E.Message);
		  ExitCode := 1;
	  end; 
  end;
end.
