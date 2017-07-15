program MiniTestProject;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework,
  TestCases1 in 'TestCases1.pas';

begin
  try

    PrepareSet(Setup);
    AddTestSet('Passing Test Example',  Test_One_Passes);
    AddTestSet('Failing Test Example',  Test_Two_Fails);
    AddTestSet('Expected Exception Example',Test_Three_Gets_Error);
    AddTestSet('Skipping Test Example', Test_Four_skips, SKIP);
    FinaliseSet(TearDown);

    Title('Test Cases For <Project> Units');
    RunTestSets;
    TestSummary;

    if sameText(Paramstr(1),'/p') then ReadLn;

    ExitCode := TotalErroredTestCases+TotalFailedTestCases;


  except
    on E: Exception do
	  Begin
      Writeln(
          'Test Framework Exception: ',
          'Test case:' ,CurrentTestClass,';',CurrentTestCase,
          E.Message);
		  ExitCode := 1;
	  end; 
  end;
end.
