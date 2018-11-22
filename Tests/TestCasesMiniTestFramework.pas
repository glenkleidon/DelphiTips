unit TestCasesMiniTestFramework;

interface

uses MiniTestFramework;

var
  TestingPasses: integer = 0;
  TestingSkips: integer = 0;
  TestingFails: integer = 0;
  TestingErrors: integer = 0;

  TotalPasses: integer = 0;
  TotalSkips: integer = 0;
  TotalFails: integer = 0;
  TotalErrors: integer = 0;

procedure CheckIsTrue_works_as_expected;
Procedure CheckIsEqual_works_as_Expected;
Procedure Test_Skip_works_as_expected;
Procedure PrepareCounters;
Procedure Test_Expected_Exception_passes;
Procedure Test_Unexpected_Exception_Raises_Error;
Procedure Test_Set_level_Skips_work_as_expected;
Procedure Check_That_Test_Cases_Ran_Correctly;
procedure Test_Case_Level_skip_works_as_expected;
Procedure Test_Find_Differences_Substituted_works_as_expected;
Procedure Test_Find_Differences_Omitted_Acutal_works_as_expected;
Procedure Test_Find_Differences_Additions_Acutal_works_as_expected;
Procedure Test_Find_multiple_Differences;
Procedure Test_Complex_JSON_String;
Procedure Test_Simple_Types_Compare_sensibly;

Procedure Test_Difference_compare_easier_to_read;

implementation

uses sysutils;

Procedure Check_That_Test_Cases_Ran_Correctly;
begin
  NewCase('Check Test Cases Ran Correctly');
  NewTest('Has correct # of Tests (including Prep and Finalise)');
  checkIsEqual(8, length(MiniTestCases));
  NewTest('Has correct # of Errored Tests');
  checkIsTrue(TotalErroredTests = TotalErrors + 1);
  NewTest('Has correct # of Skipped Tests');
  checkIsTrue(TotalSkippedTests = TotalSkips + 2);
  // the two above expected to skip
  NewTest('Has correct # of Passed Tests');
  checkIsEqual(TotalPasses + 2, TotalPassedTests);
  // the one above expected to pass
  if (TotalErroredTests = TotalErrors + 1) then
  begin
    Println('   The Error in the run is planned so that "passes"', clMessage);
    TotalErroredTests := 0;
    Inc(TotalPassedTests);
  end
  else
  begin
    Println('   Wrong Number of Errors, only 1 expected!', FOREGROUND_YELLOW);
  end;

  if (TotalSkippedTests = TotalSkips + 2) then
  begin
    Println('   The Skip in the run is planned so that "passes"', clMessage);
    TotalSkippedTests := 0;
    Inc(TotalPassedTests);
  end
  else
  begin
    Println('   Wrong Number of Skips, only 1 expected!', FOREGROUND_YELLOW);
  end;

end;

procedure UpdateCounters;
begin
  TestingPasses := MiniTestFramework.CasePassedTests;
  TestingFails := MiniTestFramework.CaseFailedTests;
  TestingSkips := MiniTestFramework.CaseSkippedTests;
  TestingErrors := MiniTestFramework.CaseErrors;
end;

procedure UpdateTotalCounters;
begin
  TotalPasses := MiniTestFramework.TotalPassedTests;
  TotalFails := MiniTestFramework.TotalFailedTests;
  TotalSkips := MiniTestFramework.TotalSkippedTests;
  TotalErrors := MiniTestFramework.TotalErroredTests;
end;

Procedure PrepareCounters;
begin
  NextTestCase('');
  UpdateTotalCounters;
end;

Procedure Test_Expected_Exception_passes;
begin
  ExpectedException := 'Expected Exception';
  raise exception.create('Expected Exception');
end;

Procedure Test_Unexpected_Exception_Raises_Error;
begin
  ExpectedException := '';
  raise exception.create('Unexpected Expected Exception');
end;

procedure CheckIsTrue_works_as_expected;
begin
  NewCase('Checking checkIsTrue against checkisFalse');

  UpdateCounters;
  NewTest('Checking True is true');
  checkIsTrue(true);
  NewTest('Checking MiniTest Framework counted Set as PASS');
  checkIsFalse((TestingPasses + 1) <> MiniTestFramework.CasePassedTests);
  checkIsFalse(TestingFails <> 0);

  UpdateCounters;
  NewTest('Checking True is false (should fail of course)');
  checkIsTrue(false);
  NewTest('Checking Fails increased by 1 ');
  checkIsFalse((TestingFails + 1) <> MiniTestFramework.CaseFailedTests);
  NewTest('Checking Passes did not change ');
  checkIsFalse(TestingPasses + 1 <> CasePassedTests);
  UpdateCounters;
  // now adjust the counts manually, to "Pass" the expected failure
  if (TestingFails = 1) then
  begin
    Println('   The failure above is expected, so that "passes"', clMessage);
    CaseFailedTests := 0;
    Inc(CasePassedTests);
  end
  else
  begin
    Println('   Too many failures, only 1 expected!', FOREGROUND_YELLOW);
  end;
end;

Procedure CheckIsEqual_works_as_Expected;
begin
  NewCase('Checking CheckIsEqual');

  UpdateCounters;
  NewTest('Checking "TESTTEXT" string');
  checkIsEqual('TESTTEXT', 'TESTTEXT');
  checkIsTrue((TestingPasses + 1) = MiniTestFramework.CasePassedTests);

  UpdateCounters;
  NewTest('Checking "135" Integer ');
  checkIsEqual(135, 130 + 5);
  NewTest('Checking passes increased by 1');
  checkIsTrue((TestingPasses + 1) = MiniTestFramework.CasePassedTests);

  UpdateCounters;
  NewTest('Checking "A" Char ');
  checkIsEqual('A', upcase('a'));
  NewTest('Checking passes increased by 1');
  checkIsTrue((TestingPasses + 1) = MiniTestFramework.CasePassedTests);

  UpdateCounters;
  NewTest('Checking "A" Char <> "B" ');
  checkIsEqual('A', upcase('b'));
  NewTest('Checking failed increased by 1');
  checkIsTrue((TestingFails + 1) = MiniTestFramework.CaseFailedTests);

  UpdateCounters;
  if (TestingFails = 1) then
  begin
    Println('   The failure above is expected, so that "passes"', clMessage);
    CaseFailedTests := 0;
    Inc(CasePassedTests);
  end
  else
  begin
    Println('   Too many failures, only 1 expected!', FOREGROUND_YELLOW);
  end;

end;

Procedure Test_Skip_works_as_expected;
const
  expectedSkips = 4;
begin
  NextTestCase('Checking Cases Skip as expected');

  UpdateCounters;
  NewTest('Checking skip on IsTrue');
  checkIsTrue(false, '', SKIPPED);
  NewTest('Checking skip on IsFalse');
  checkIsFalse(true, '', SKIPPED);
  NewTest('Checking skip on IsEqual');
  checkIsEqual('A', 'A', '', SKIPPED);
  NewTest('Checking skip on NotEqual');
  CheckNotEqual('A', 'B', '', SKIPPED);

  NewTest('Checking Passes Not changed');
  checkIsEqual(CasePassedTests, TestingPasses);

  NewTest('Checking Fails Not changed');
  checkIsEqual(CaseFailedTests, TestingFails);

  NewTest(format('Checking Skipped Counter has increased by %d',
    [expectedSkips]));
  checkIsEqual(TestingSkips + expectedSkips, CaseSkippedTests);

  UpdateCounters;
  if (TestingSkips = expectedSkips) then
  begin
    Println(sysutils.format('The %d skips above are expected, so that "passes"',
      [expectedSkips]), clMessage);
    dec(CaseSkippedTests, expectedSkips);
    Inc(CasePassedTests, expectedSkips);
  end
  else
  begin
    Println('Wrong number of skips!', FOREGROUND_YELLOW);
    CaseSkippedTests := CaseSkippedTests - (expectedSkips - TestingSkips);
    CaseFailedTests := CaseFailedTests + (expectedSkips - TestingSkips);
  end;

end;

Procedure Test_Set_level_Skips_work_as_expected;
begin
  NewTest('Test 4 should Skip (Set Level skip)');
  checkIsEqual('ABC', 'A' + 'B' + 'C', ''); // skipped by set level

  NewTest('Test 4 should Pass (Using Dont Skip)');
  checkIsEqual('ABC', 'A' + 'B' + 'C', '', DONTSKIP);
  // decide to run this one after all

end;

procedure Test_Case_Level_skip_works_as_expected;
begin
  NewTest('TRUE Case that should not be run');
  checkIsTrue(true);

  NewTest('FALSE Case that should not be run');
  checkIsTrue(false);

end;

Procedure Test_Find_Differences_Substituted_works_as_expected;
var
  lDifferences: TDifferences;
begin
  (* *)
  NewTest('Text that is Empty, Length of result should be 0');
  lDifferences := FindDifferences('', '');
  checkIsEqual(0, length(lDifferences));

  (* *)
  NewTest('Text that is the same, Length of result should be 1');
  lDifferences := FindDifferences('ABC', 'ABC');
  checkIsEqual(1, length(lDifferences));
  NewTest('Text that is the same, Should be dtNONE');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtNone);
  NewTest('Check StartPos=0');
  checkIsEqual(0, lDifferences[0].TextStart);
  NewTest('Check Sizes are 0,0,0');
  checkIsEqual(0, lDifferences[0].size);
  checkIsEqual(0, lDifferences[0].TextSize);
  checkIsEqual(0, lDifferences[0].CompareToSize);

  (* *)
  lDifferences := FindDifferences('ABC', 'ABD');
  NewTest('ABC<->ABD, (Diff to end) Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABC<->ABD, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtSubstitution);
  NewTest('ABC<->ABD, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  NewTest('ABC<->ABD Check Sizes 1,1,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(1, lDifferences[0].TextSize);
  checkIsEqual(1, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABC', 'ABDEFGH');
  NewTest('ABC<->ABD, (Diff to end) Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABC<->ABD, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtSubstitution);
  NewTest('ABC<->ABD, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  NewTest('ABC<->ABD Check Sizes 5,1,5');
  checkIsEqual(5, lDifferences[0].size);
  checkIsEqual(1, lDifferences[0].TextSize);
  checkIsEqual(5, lDifferences[0].CompareToSize);

  (* *)
  lDifferences := FindDifferences('ABCD', 'ABXD');
  NewTest('ABCD<->ABXD, (Substitution) Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABCD<->ABXD, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtSubstitution);
  NewTest('ABCD<->ABXD, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  NewTest('ABCD<->ABXD Check Sizes 1,1,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(1, lDifferences[0].TextSize);
  checkIsEqual(1, lDifferences[0].CompareToSize);

  (* *)
  lDifferences := FindDifferences('ABCDEF', 'ABXDEF');
  NewTest('ABCDEF<->ABXDEF, (Substitution) Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABCDEF<->ABXDEF, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtSubstitution);
  NewTest('ABCDEF<->ABXDEF, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  NewTest('ABCDEF<->ABXDEF, Should have Size=1,1,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(1, lDifferences[0].TextSize);
  checkIsEqual(1, lDifferences[0].CompareToSize);
  (* *)

  lDifferences := FindDifferences('ABCDEF', 'ABXXEF');
  NewTest('ABCDEF<->ABXXEF, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtSubstitution);
  NewTest('ABCDEF<->ABXXEF, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  NewTest('ABCDEF<->ABXXEF, Should have Size=2');
  checkIsEqual(2, lDifferences[0].size);
  checkIsEqual(2, lDifferences[0].TextSize);
  checkIsEqual(2, lDifferences[0].CompareToSize);
  (* *)

end;

Procedure Test_Find_Differences_Omitted_Acutal_works_as_expected;
var
  lDifferences: TDifferences;
begin
  (* *)
  lDifferences := FindDifferences('ABC', 'AB');
  NewTest('ABC<->AB, Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABC<->AB, Should be dtCompareTooShort');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareTooShort);
  NewTest('ABC<->AB, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  checkIsEqual(3, lDifferences[0].CompareStart);
  NewTest('ABC<->AB, Should have Sizes=1,0,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(0, lDifferences[0].TextSize);
  checkIsEqual(1, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABC', 'BC');
  NewTest('ABC<->BC, Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABC<->BC, Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareHasOmission);
  NewTest('ABC<->BC, Should have Start=1');
  checkIsEqual(1, lDifferences[0].TextStart);
  checkIsEqual(1, lDifferences[0].CompareStart);
  NewTest('ABCD<->BC, Should have Sizes=1,0,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(1, lDifferences[0].TextSize);
  checkIsEqual(0, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABCD', 'ABD');
  NewTest('ABCD<->ABD, Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareHasOmission);
  NewTest('ABCD<->ABD, Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABCD<->ABD, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  NewTest('ABCD<->ABD, Should have Size=1,1,0');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(1, lDifferences[0].TextSize);
  checkIsEqual(0, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABCDEF', 'ABDEF');
  NewTest('ABCDEF<->ABDEF, Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABCDEF<->ABDEF, Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareHasOmission);
  NewTest('ABCDEF<->ABDEF, Should have Start=3');
  checkIsEqual(3, lDifferences[0].TextStart);
  checkIsEqual(3, lDifferences[0].CompareStart);
  NewTest('ABCDEF<->ABDEF, Should have Size=1,1,0');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(1, lDifferences[0].TextSize);
  checkIsEqual(0, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABCDEF', 'ADEF');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABCDEF<->ABDEF, Should be dtCompareHasOmission');
  NewTest('ABCDEF<->ABDEF, Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareHasOmission);
  NewTest('ABCDEF<->ADEF, Should have Start=2');
  checkIsEqual(2, lDifferences[0].TextStart);
  checkIsEqual(2, lDifferences[0].CompareStart);
  NewTest('ABCDEF<->ADEF, Should have Size=2,2,0');
  checkIsEqual(2, lDifferences[0].size);
  checkIsEqual(2, lDifferences[0].TextSize);
  checkIsEqual(0, lDifferences[0].CompareToSize);
  (* *)

end;

Procedure Test_Find_Differences_Additions_Acutal_works_as_expected;
var
  lDifferences: TDifferences;
begin
  (* *)
  lDifferences := FindDifferences('ABC', 'ABCD');
  NewTest('ABC<->ABCD, Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABC<->ABCD, Should be dtCompareTooLong');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareTooLong);
  NewTest('ABC<->ABCD, Should have Start=4');
  checkIsEqual(4, lDifferences[0].TextStart);
  checkIsEqual(4, lDifferences[0].CompareStart);
  NewTest('ABC<->ABCD, Should have Size=1,0,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(0, lDifferences[0].TextSize);
  checkIsEqual(1, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABCDEF', 'ABCDEFGH');
  NewTest('ABCDEF<->ABCDEFGH, Should be dtCompareTooLong');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareTooLong);
  NewTest('ABCDEF<->ABCDEFGH, Should have Start=7');
  checkIsEqual(7, lDifferences[0].TextStart);
  checkIsEqual(7, lDifferences[0].CompareStart);
  NewTest('ABCDEF<->ABCDEFGH, Should have Size=1,0,1');
  checkIsEqual(2, lDifferences[0].size);
  checkIsEqual(0, lDifferences[0].TextSize);
  checkIsEqual(2, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABC', 'ZABC');
  NewTest('ABC<->ZABC, Length of result should be 1');
  checkIsEqual(1, length(lDifferences));
  NewTest('ABC<->ZABC, Should be dtCompareHasAddition');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareHasAddition);
  NewTest('ABC<->ZABC, Should have Start=1');
  checkIsEqual(1, lDifferences[0].TextStart);
  NewTest('ABCD<->ZABD, Should have Size=1,0,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(0, lDifferences[0].TextSize);
  checkIsEqual(1, lDifferences[0].CompareToSize);
  (* *)
  lDifferences := FindDifferences('ABCDEF', 'ABCXXDEF');
  NewTest('ABCDEF<->ABCXXDEF, Should be dtCompareHasAddition');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareHasAddition);
  NewTest('ABCDEF<->ABCXXDEF, Should have Start=4');
  checkIsEqual(4, lDifferences[0].TextStart);
  NewTest('ABCDEF<->ABCXXDEF, Should have Size=2,0,2');
  checkIsEqual(2, lDifferences[0].size);
  checkIsEqual(0, lDifferences[0].TextSize);
  checkIsEqual(2, lDifferences[0].CompareToSize);
  (* *)

end;

Procedure Test_Find_multiple_Differences;
var
  lDifferences: TDifferences;
begin
  (* *)
  lDifferences := FindDifferences('ABC_ONE_TWO_Three', 'ABCD_ONE_2_Three');
  NewTest('ABC_ONE_TWO_Three<->ABCD_ONE_2_Three, Length of result should be 2');
  checkIsEqual(2, length(lDifferences));

  NewTest('ABC_ONE_TWO_Three<->ABCD_ONE_2_Three, [0] Should be dtCompareHasAddition');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtCompareHasAddition);
  NewTest('ABC_ONE_TWO_Three<->ABCD_ONE_2_Three, [0]  Should have Start=4');
  checkIsEqual(4, lDifferences[0].TextStart);
  checkIsEqual(4, lDifferences[0].CompareStart);
  NewTest('ABC_ONE_TWO_Three<->ABCD_ONE_2_Three, [0]  Should have Size=1,3,1');
  checkIsEqual(1, lDifferences[0].size);
  checkIsEqual(0, lDifferences[0].TextSize);
  checkIsEqual(1, lDifferences[0].CompareToSize);

  NewTest('ABC_ONE_TWO_Three<->ABCD_ONE_2_Three, [1]  Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[1].TypeOfDifference = dtCompareHasOmission);
  NewTest('ABC_ONE_TWO_Three<->ABCD_ONE_2_Three, [1]  Should have Start=9,10');
  checkIsEqual(9, lDifferences[1].TextStart);
  checkIsEqual(10, lDifferences[1].CompareStart);
  NewTest('ABC_ONE_TWO_Three<->ABCD_ONE_2_Three, [1]  Should have Size=1');
  checkIsEqual(3, lDifferences[1].size);
  checkIsEqual(3, lDifferences[1].TextSize);
  checkIsEqual(1, lDifferences[1].CompareToSize);
  (* *)

end;

Procedure Test_Complex_JSON_String;
var
  lExpected, lResult: string;
  lDifferences: TDifferences;
begin

  lExpected :=
    '{"Authentication": {'#9'"Username": "USERX",'#9'"Password": "XXXXXX"},'
    + '"CorrelationId": "30BA96DD-398A-4EED-8696-F9F6B0F88877","RequestStartTime":'
    + '"1899-12-30T00:00:00.000Z","EntityTypeId": 2,"EntityId": ${#Project#ContractId},'
    + '"InterfaceId": 100,"IPAddress": "127.0.0.1","CallerSystemId": 2,"DownloadDocumentInput":'
    + '{'#9'"DocumentTypeCode": 101,'#9'"Version": 0,'#9'"RequestUsername":' +
    ' "OOO000"}}';

  lResult :=
    '{"DownloadDocumentInput":{"DocumentTypeCode":101,"DocumentLocationId":0,"Version":'
    + '0,"RequestUsername":"OOO000","RefCount":0},"Authentication":{"Username":'
    + '"USERX","Password":"XXXXXX","Token":"","RefCount":0},"RequestStartTime"'
    + ':"1899-12-30T00:00:00.000+11:00","CallerSystemId":2,"CorrelationId":' +
    '"30BA96DD-398A-4EED-8696-F9F6B0F88877","EntityId":0,"EntityTypeId":0,' +
    '"InterfaceId":100,"IPAddress":"127.0.0.1","RefCount":0}';
  checkIsEqual(lExpected, lResult);

  lDifferences := FindDifferences(lExpected, lResult);
  NewTest('JSON Comparison, Length of result should be 7');
  checkIsEqual(7, length(lDifferences));

  NewTest('JSON Comparison, [0]  Should be dtDifferent');
  checkIsTrue(lDifferences[0].TypeOfDifference = dtDifferent);
  NewTest('JSON Comparison, [1]  Should be dtDifferent');
  checkIsTrue(lDifferences[1].TypeOfDifference = dtDifferent);
  NewTest('JSON Comparison, [2]  Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[2].TypeOfDifference = dtCompareHasOmission);
  NewTest('JSON Comparison, [3]  Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[3].TypeOfDifference = dtCompareHasOmission);
  NewTest('JSON Comparison, [4]  Should be dtDifferent');
  checkIsTrue(lDifferences[4].TypeOfDifference = dtDifferent);
  NewTest('JSON Comparison, [5]  Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[5].TypeOfDifference = dtCompareHasOmission);
  NewTest('JSON Comparison, [6]  Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[6].TypeOfDifference = dtCompareHasOmission);

  NewTest('JSON Comparison, [0]  Should have Start=3,3');
  checkIsEqual(3, lDifferences[0].TextStart);
  checkIsEqual(3, lDifferences[0].CompareStart);
  NewTest('JSON Comparison, [0]  Should have Size=415,375,415');
  checkIsEqual(415, lDifferences[0].size);
  checkIsEqual(375, lDifferences[0].TextSize);
  checkIsEqual(415, lDifferences[0].CompareToSize);

  NewTest('JSON Comparison, [6]  Should have Start=3,3');
  checkIsEqual(3, lDifferences[6].TextStart);
  checkIsEqual(3, lDifferences[6].CompareStart);
  NewTest('JSON Comparison, [6]  Should have Size=415,375,415');
  checkIsEqual(415, lDifferences[6].size);
  checkIsEqual(375, lDifferences[6].TextSize);
  checkIsEqual(415, lDifferences[6].CompareToSize);

end;

Procedure Test_Difference_compare_easier_to_read;
begin
  (* *)
  NewTest('Compare Short String');
  checkIsEqual('ABC', 'DEF');

  NewTest('Compare Lines without breaks String');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fix jumps over the lazy dog');

  NewTest('Compare Lines without breaks String and multiple differences');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quoKC brown fix jumps iver the lzay dog');

  NewTest('Compare Lines with omission ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The brown fox jumps over the lazy dog');

  NewTest('Compare Lines with Addition ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fox jumps jumps over the lazy dog');

  NewTest('Compare Lines with omission at the front ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'brown fox jumps over the lazy dog');

  NewTest('Compare Lines with omission at the end ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fox jumps over the');

  NewTest('Compare Lines with multiple differences String');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fix jumps over the lasy dog');
  (* *)

  NewTest('Compare MultiLines Results');
  checkIsEqual('The quick brown fox jumps over the lazy dog and'#13#10 +
    ' there are multiple lines to deal with'#13#10#13#10 +
    '<1 Empty Line Above> But still works OK',
    'The quick brown fix jumps over the lazy dog and'#13#10 +
    ' there are multople lines to dwal with'#13#10#13#10 +
    '<1 Empty Line Above> But still works alright');
  (* *)

end;

Procedure Test_Simple_Types_Compare_sensibly;
var
  lErrorCount: integer;
begin

  lErrorCount := CaseFailedTests;
  NewTest('Booleans compare without using difference check');
  checkIsTrue(false);
  checkIsEqual(0, DifferencesFound, 'Did not expect to find differences');
  checkIsFalse(true);
  checkIsEqual(0, DifferencesFound, 'Did not expect to find differences');

  NewTest('Numbers compare as numbers without difference Check');
  checkIsEqual(10, 5);
  checkIsEqual(0, DifferencesFound, 'Did not expect to find differences');
  checkIsEqual(1000, 5.2);
  checkIsEqual(0, DifferencesFound, 'Did not expect to find differences');
  if (CaseFailedTests = lErrorCount + 4) then
  begin
    Println('   The Error in the set is planned so that "passes"', clMessage);
    CaseFailedTests := 0;
    Inc(CasePassedTests, lErrorCount);
  end
  else
  begin
    Println('   Wrong Number of errors, 4 expected!', FOREGROUND_YELLOW);
  end;

end;

end.
