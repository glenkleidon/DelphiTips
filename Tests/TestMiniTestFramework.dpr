program TestMiniTestFramework;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas',
  TestCasesMiniTestFramework in 'TestCasesMiniTestFramework.pas';

begin
  try
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;
    Title('Testing the Framework...With the framework!!');
    (**)

    CheckIsTrue_works_as_expected;
    CheckIsEqual_works_as_Expected;
    Test_Skip_works_as_Expected;

    TestSummary;
    (**)
    NewSet('Check Framework Set Functions');
    PrepareSet(PrepareCounters);
    AddTestCase('Expected Exception Passes', Test_Expected_Exception_passes);
    AddTestCase('Unexpected Exception Raises Error',
      Test_Unexpected_Exception_Raises_Error);
    AddTestCase('Set level Skips and ignore skips works as expected',
      Test_Set_level_Skips_work_as_expected, SKIP);
    AddTestCase('Skip Case works as expected',
      Test_Case_Level_skip_works_as_expected, SKIPCASE);
    FinaliseSet(nil);

    NewSet('Check Framework Results');
    PrepareSet(Nil);
    FinaliseSet(Check_That_Test_Cases_Ran_Correctly);
    RunTestSets;
    TestSummary;
    (**)
    NewSet('Compare Result Display Improvements');
    PrepareSet(Nil);
    (**)
    AddTestCase('LCS check', Test_LCS_returns_Correct_Result);
    AddTestCase('LCSDiff check', Test_LCSDiff_returns_Correct_Result);
    AddTestCase('LCSDifferences check', Test_LCSDiffences_returns_Correct_Result);
    AddTestCase('LCSDifferences With Complext JSON',Test_LCSDiffences_Handles_Complex_JSON_Difference);
    (**)
    AddTestCase('Console Column Testing', Test_Console_Column_Displays_Colums_as_Expected);
    (**)
    AddTestCase('Simple Types',
      Test_Simple_Types_Compare_sensibly);
    (**)
    AddTestCase('Compare Outputs results in easy to read format',
      Test_Difference_compare_easier_to_read);
    AddTestCase('Compare Outputs result in correct mode',
      Test_Difference_compare_uses_correct_mode);
    (**)
    FinaliseSet(Nil);
    RunTestSets;
    TestSummary;

    if sameText(Paramstr(1), '/p') then
      ReadLn;

    ExitCode := TotalErrors + TotalFailedTests;

    (**)
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
