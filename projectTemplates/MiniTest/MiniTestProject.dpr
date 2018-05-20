program MiniTestProject;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in 'MiniTestFramework.pas',
  TestCases1 in 'TestCases1.pas';

begin
  try

    NewSet('Example Pass/Fail');
    PrepareSet(Setup);
    AddTestCase('Passing Test Example',  Case_One_Passes);
    AddTestCase('Failing Test Example',  Case_Two_Fails);
    FinaliseSet(TearDown);

    NewSet('Example Exception and Skip');
    AddTestCase('Expected Exception Example',Case_Three_Gets_Error);
    AddTestCase('Skipping Test Example', Case_Four_skips, SKIP);
    FinaliseSet(TearDown);

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
