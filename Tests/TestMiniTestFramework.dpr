program TestMiniTestFramework;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas',
  TestCasesMiniTestFramework in 'TestCasesMiniTestFramework.pas';

begin
  try
    Title('Testing the Framework...With the framework!!');

    CheckIsTrue_works_as_expected;
    CheckIsEqual_works_as_Expected;
    Test_Skip_works_as_Expected;

    TestSummary;

    NewSet('Check Framework Set Functions');
    PrepareSet(PrepareCounters);
    AddTestCase('Expected Exception Passes', Test_Expected_Exception_passes);
    AddTestCase('Unexpected Exception Raises Error', Test_Unexpected_Exception_Raises_Error);
    AddtestCase('Set level Skips and ignore skips works as expected', Test_Set_level_Skips_work_as_expected, SKIP);
    FinaliseSet(nil);

    NewSet('Check Framework Results');
    PrepareSet(Nil);
    FinaliseSet(Check_That_Test_Cases_Ran_Correctly);

    RunTestSets;

    TestSummary;

    if sameText(Paramstr(1),'/p') then ReadLn;

    ExitCode := TotalErrors+TotalFailedTests;


  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
