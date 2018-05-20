program MiniTestProject;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in 'MiniTestFramework.pas',
  TestCases1 in 'TestCases1.pas';

begin
  try

    NewSet('Example Pass Fails');
    PrepareSet(Setup);
    AddTestCase('Passing Test Example',  Test_One_Passes);
    AddTestCase('Failing Test Example',  Test_Two_Fails);
    FinaliseSet(TearDown);

    NewSet('Example Exception and Skip');
    AddTestCase('Expected Exception Example',Test_Three_Gets_Error);
    AddTestCase('Skipping Test Example', Test_Four_skips, SKIP);
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
