program TestMiniTestFramework;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas',
  TestCasesMiniTestFramework in 'TestCasesMiniTestFramework.pas';

begin
  try
    Title('Testing the Framework...With the framework!!');
    (** )

    CheckIsTrue_works_as_expected;
    CheckIsEqual_works_as_Expected;
    Test_Skip_works_as_Expected;

    TestSummary;
    (** )
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
    (** )
    AddTestCase('Check Find Substitutions',
      Test_Find_Differences_Substituted_works_as_expected);
    AddTestCase('Check Find Omissions',
      Test_Find_Differences_Omitted_Acutal_works_as_expected);
    AddTestCase('Check Find Additions',
      Test_Find_Differences_Additions_Acutal_works_as_expected);
    AddTestCase('Multiple Differences Located',
      Test_Find_multiple_Differences);
    (**)
    AddTestCase('Complex JSON string comparison', Test_Complex_JSON_String);
    (** )
    AddTestCase('Simple Types',
      Test_Simple_Types_Compare_sensibly);
    (**)
    AddTestCase('Compare Outputs results in easy to read format',
      Test_Difference_compare_easier_to_read);
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
