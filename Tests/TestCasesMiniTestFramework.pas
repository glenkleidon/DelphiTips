unit TestCasesMiniTestFramework;

interface
  uses MiniTestFramework;

var TestingPasses: integer = 0;
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
Procedure Test_Find_Differences_Substituted_works_as_expected ;
Procedure Test_Find_Differences_Omitted_Acutal_works_as_expected;

Procedure Test_Difference_compare_easier_to_read;

implementation
uses sysutils;

Procedure Check_That_Test_Cases_Ran_Correctly;
begin
  NewCase('Check Test Cases Ran Correctly');
  NewTest('Has correct # of Tests (including Prep and Finalise)');
  checkIsEqual(8,length(MiniTestCases));
  NewTest('Has correct # of Errored Tests');
  checkIsTrue(TotalErroredTests=TotalErrors+1);
  NewTest('Has correct # of Skipped Tests');
  checkIsTrue(TotalSkippedTests=TotalSkips+2); // the two above expected to skip
  NewTest('Has correct # of Passed Tests');
  checkIsEqual(TotalPasses+2,TotalPassedTests); // the one above expected to pass
  if (TotalErroredTests=TotalErrors+1) then
  begin
   Println('   The Error in the run is planned so that "passes"', clMessage);
   TotalErroredTests := 0;
   Inc(TotalPassedTests);
  end else
  begin
   Println('   Wrong Number of Errors, only 1 expected!', FOREGROUND_YELLOW);
  end;

  if (TotalSkippedTests=TotalSkips+2) then
  begin
   Println('   The Skip in the run is planned so that "passes"', clMessage);
   TotalSkippedTests := 0;
   Inc(TotalPassedTests);
  end else
  begin
   Println('   Wrong Number of Skips, only 1 expected!', FOREGROUND_YELLOW);
  end;

end;

procedure UpdateCounters;
begin
   TestingPasses := MiniTestFramework.CasePassedTests;
   TestingFails  := MiniTestFramework.CaseFailedTests;
   TestingSkips  := MiniTestFramework.CaseSkippedTests;
   TestingErrors  := MiniTestFramework.CaseErrors;
end;

procedure UpdateTotalCounters;
begin
   TotalPasses := MiniTestFramework.TotalPassedTests;
   TotalFails  := MiniTestFramework.TotalFailedTests;
   TotalSkips  := MiniTestFramework.TotalSkippedTests;
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
 newTest('Checking True is true');
 checkisTrue(true);
 newTest('Checking MiniTest Framework counted Set as PASS');
 checkIsFalse((TestingPasses+1)<>MiniTestFramework.CasePassedTests);
 checkIsFalse(TestingFails<>0);

 UpdateCounters;
 newTest('Checking True is false (should fail of course)');
 checkIsTrue(false);
 newTest('Checking Fails increased by 1 ');
 checkIsFalse((TestingFails+1)<>MiniTestFramework.CaseFailedTests);
 newTest('Checking Passes did not change ');
 checkisFalse(TestingPasses+1<>CasePassedTests);
 UpdateCounters;
 // now adjust the counts manually, to "Pass" the expected failure
 if (TestingFails=1) then
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
 newTest('Checking "TESTTEXT" string');
 checkisequal('TESTTEXT','TESTTEXT');
 checkIsTrue((TestingPasses+1)=MiniTestFramework.CasePassedTests);

 UpdateCounters;
 newTest('Checking "135" Integer ');
 checkisequal(135,130+5);
 newTest('Checking passes increased by 1');
 checkIsTrue((TestingPasses+1)=MiniTestFramework.CasePassedTests);

 UpdateCounters;
 newTest('Checking "A" Char ');
 checkisequal('A',upcase('a'));
 newTest('Checking passes increased by 1');
 checkIsTrue((TestingPasses+1)=MiniTestFramework.CasePassedTests);

 UpdateCounters;
 newTest('Checking "A" Char <> "B" ');
 checkisequal('A',upcase('b'));
 newTest('Checking failed increased by 1');
 checkIsTrue((TestingFails+1)=MiniTestFramework.CaseFailedTests);

 UpdateCounters;
  if (TestingFails=1) then
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
const expectedSkips = 4;
begin
 NextTestCase('Checking Cases Skip as expected');

 UpdateCounters;
 newTest('Checking skip on IsTrue');
 CheckisTrue(False,'',SKIPPED);
 newTest('Checking skip on IsFalse');
 CheckisFalse(True,'',SKIPPED);
 newTest('Checking skip on IsEqual');
 CheckIsEqual('A','A','',SKIPPED);
 newTest('Checking skip on NotEqual');
 CheckNotEqual('A','B','',SKIPPED);


 newTest('Checking Passes Not changed');
 CheckIsEqual(CasePassedTests,TestingPasses);

 newTest('Checking Fails Not changed');
 CheckIsEqual(CaseFailedTests,TestingFails);

 newTest(format('Checking Skipped Counter has increased by %d',[Expectedskips]));
 CheckIsEqual(TestingSkips+expectedSkips,CaseSkippedTests);

 UpdateCounters;
  if (TestingSkips=expectedSkips) then
 begin
   Println(sysutils.format('The %d skips above are expected, so that "passes"',[ExpectedSkips]), clMessage);
   dec(CaseSkippedTests,expectedSkips);
   Inc(CasePassedTests,expectedSkips);
 end
 else
 begin
   Println('Wrong number of skips!', FOREGROUND_YELLOW);
   CaseSkippedTests := CaseSkippedTests - (expectedSkips-TestingSkips);
   CaseFailedTests := CaseFailedTests + (expectedSkips-TestingSkips);
 end;

end;

Procedure Test_Set_level_Skips_work_as_expected;
begin
  NewTest('Test 4 should Skip (Set Level skip)');
  checkisEqual('ABC','A'+'B'+'C',''); // skipped by set level

  NewTest('Test 4 should Pass (Using Dont Skip)');
  checkisEqual('ABC','A'+'B'+'C','',DONTSKIP); // decide to run this one after all

end;

procedure Test_Case_Level_skip_works_as_expected;
begin
 NewTest('TRUE Case that should not be run');
 checkIsTrue(true);

 NewTest('FALSE Case that should not be run');
 checkIsTrue(false);

end;

Procedure Test_Find_Differences_Substituted_works_as_expected ;
var lDifferences: TDifferences;
begin
  (**)
  NewTest('Text that is Empty, Length of result should be 0');
  lDifferences := FindDifferences('','');
  checkIsEqual(0,length(lDifferences));
  NewTest('Text that is the same, Length of result should be 1');

  lDifferences := FindDifferences('ABC','ABC');
  checkIsEqual(1,length(lDifferences));
  NewTest('Text that is the same, Should be dtNONE');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtNone);

  (**)
  lDifferences := FindDifferences('ABC','ABD');
  NewTest('ABC<->ABD, Length of result should be 1');
  checkIsEqual(1,length(lDifferences));
  NewTest('ABC<->ABD, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtSubstitution);
  NewTest('ABC<->ABD, Should have Start=3');
  checkIsEqual(3,lDifferences[0].StartPos);

  (**)
  lDifferences := FindDifferences('ABCD','ABXD');
  NewTest('ABCD<->ABXD, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtSubstitution);
  NewTest('ABCD<->ABXD, Should have Start=3');
  checkIsEqual(3,lDifferences[0].StartPos);
  NewTest('ABCD<->ABXD, Should have Size=1');
  checkIsEqual(1,lDifferences[0].size);
  (**)
  lDifferences := FindDifferences('ABCDEF','ABXDEF');
  NewTest('ABCDEF<->ABXDEF, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtSubstitution);
  NewTest('ABCDEF<->ABXDEF, Should have Start=3');
  checkIsEqual(3,lDifferences[0].StartPos);
  NewTest('ABCDEF<->ABXDEF, Should have Size=1');
  checkIsEqual(1,lDifferences[0].Size);
 (**)

  lDifferences := FindDifferences('ABCDEF','ABXXEF');
  NewTest('ABCDEF<->ABXXEF, Should be dtSubstitution');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtSubstitution);
  NewTest('ABCDEF<->ABXXEF, Should have Start=3');
  checkIsEqual(3,lDifferences[0].StartPos);
  NewTest('ABCDEF<->ABXXEF, Should have Size=2');
  checkIsEqual(2,lDifferences[0].Size);
 (**)

end;


Procedure Test_Find_Differences_Omitted_Acutal_works_as_expected;
var lDifferences: TDifferences;
begin
  (**)
  lDifferences := FindDifferences('ABC','AB');
  NewTest('ABC<->AB, Length of result should be 1');
  checkIsEqual(1,length(lDifferences));
  NewTest('ABC<->AB, Should be dtCompareTooShort');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtCompareTooShort);
  NewTest('ABC<->AB, Should have Start=3');
  checkIsEqual(3,lDifferences[0].StartPos);
  (**)
  lDifferences := FindDifferences('ABCD','ABD');
  NewTest('ABCD<->ABD, Should be dtActualHasOmission');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtCompareHasOmission);
  NewTest('ABCD<->ABD, Should have Start=3');
  checkIsEqual(3,lDifferences[0].StartPos);
  NewTest('ABCD<->ABD, Should have Size=1');
  checkIsEqual(1,lDifferences[0].Size);
  (**)
  lDifferences := FindDifferences('ABCDEF','ABDEF');
  NewTest('ABCDEF<->ABDEF, Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtCompareHasOmission);
  NewTest('ABCDEF<->ABDEF, Should have Start=3');
  checkIsEqual(3,lDifferences[0].StartPos);
  NewTest('ABCDEF<->ABDEF, Should have Size=1');
  checkIsEqual(1,lDifferences[0].Size);
  (**)
  lDifferences := FindDifferences('ABCDEF','ADEF');
  NewTest('ABCDEF<->ABDEF, Should be dtCompareHasOmission');
  checkIsTrue(lDifferences[0].TypeOfDifference=dtCompareHasOmission);
  NewTest('ABCDEF<->ADEF, Should have Start=2');
  checkIsEqual(2,lDifferences[0].StartPos);
  NewTest('ABCDEF<->ADEF, Should have Size=2');
  checkIsEqual(2,lDifferences[0].Size);
  (**)

end;

Procedure Test_Difference_compare_easier_to_read;
begin
  NewTest('Compare Short String');
  CheckIsEqual('ABC', 'DEF');


  NewTest('Compare Lines without breaks String');
  CheckIsEqual('The quick brown fox jumps over the lazy dog', 'The quick brown fix jumps over the lazy dog');

  NewTest('Compare Lines with multiple differences String');
  CheckIsEqual('The quick brown fox jumps over the lazy dog', 'The quick brown fix jumps over the lasy dog');


  NewTest('Compare MultiLines Results');
  CheckIsEqual(
    'The quick brown fox jumps over the lazy dog and'#13#10+
    ' there are multiple lines to deal with'#13#10#13#10+
    'But still works OK',
    'The quick brown fix jumps over the lazy dog and'#13#10+
    ' there are multople lines to dwal with'#13#10#13#10+
    'But still works alright'
    )
end;


end.
