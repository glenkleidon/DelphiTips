unit TestCasesMiniTestFramework;

interface
  uses MiniTestFramework;

var TestingPasses: integer = 0;
    TestingSkips: integer = 0;
    TestingFails: integer = 0;


procedure CheckIsTrue_works_as_expected;
Procedure CheckIsEqual_works_as_Expected;
Procedure Test_Skip_works_as_expected;

implementation
uses sysutils;

procedure UpdateCounters;
begin
   TestingPasses := MiniTestFramework.SetPassedTestCases;
   TestingFails  := MiniTestFramework.SetFailedTestCases;
   TestingSkips  := MiniTestFramework.SetSkippedTestCases;
end;

procedure CheckIsTrue_works_as_expected;
begin
 NewTestSet('Checking checkIsTrue against checkisFalse');

 UpdateCounters;
 newTestCase('Checking True is true');
 checkisTrue(true);
 newTestCase('Checking MiniTest Framework counted Set as PASS');
 checkIsFalse((TestingPasses+1)<>MiniTestFramework.SetPassedTestCases);
 checkIsFalse(TestingFails<>0);

 UpdateCounters;
 newTestCase('Checking True is false (should fail of course)');
 checkIsTrue(false);
 newTestCase('Checking Fails increased by 1 ');
 checkIsFalse((TestingFails+1)<>MiniTestFramework.SetFailedTestCases);
 newTestCase('Checking Passes did not change ');
 checkisFalse(TestingPasses+1<>SetPassedTestCases);
 UpdateCounters;
 // now adjust the counts manually, to "Pass" the expected failure
 if (TestingFails=1) then
 begin
   Println('The failure above is expected, so that "passes"', clMessage);
   SetFailedTestCases := 0;
   Inc(SetPassedTestCases);
 end
 else
 begin
   Println('Too many failures, only 1 expected!', FOREGROUND_YELLOW);
 end;
end;

Procedure CheckIsEqual_works_as_Expected;
begin
 NewTestSet('Checking CheckIsEqual');

 UpdateCounters;
 newTestCase('Checking "TESTTEXT" string');
 checkisequal('TESTTEXT','TESTTEXT');
 checkIsTrue((TestingPasses+1)=MiniTestFramework.SetPassedTestCases);

 UpdateCounters;
 newTestCase('Checking "135" Integer ');
 checkisequal(135,130+5);
 newTestCase('Checking passes increased by 1');
 checkIsTrue((TestingPasses+1)=MiniTestFramework.SetPassedTestCases);

 UpdateCounters;
 newTestCase('Checking "A" Char ');
 checkisequal('A',upcase('a'));
 newTestCase('Checking passes increased by 1');
 checkIsTrue((TestingPasses+1)=MiniTestFramework.SetPassedTestCases);

 UpdateCounters;
 newTestCase('Checking "A" Char <> "B" ');
 checkisequal('A',upcase('b'));
 newTestCase('Checking failed increased by 1');
 checkIsTrue((TestingFails+1)=MiniTestFramework.SetFailedTestCases);

 UpdateCounters;
  if (TestingFails=1) then
 begin
   Println('The failure above is expected, so that "passes"', clMessage);
   SetFailedTestCases := 0;
   Inc(SetPassedTestCases);
 end
 else
 begin
   Println('Too many failures, only 1 expected!', FOREGROUND_YELLOW);
 end;

end;

Procedure Test_Skip_works_as_expected;
const expectedSkips = 4;
begin
 NewTestSet('Checking Cases Skip as expeced');

 UpdateCounters;
 newTestCase('Checking skip on IsTrue');
 CheckisTrue(False,'',SKIPPED);
 newTestCase('Checking skip on IsFalse');
 CheckisFalse(True,'',SKIPPED);
 newTestCase('Checking skip on IsEqual');
 CheckIsEqual('A','A','',SKIPPED);
 newTestCase('Checking skip on NotEqual');
 CheckNotEqual('A','B','',SKIPPED);


 newTestCase('Checking Passes Not changed');
 CheckIsEqual(SetPassedTestCases,TestingPasses);

 newTestCase('Checking Fails Not changed');
 CheckIsEqual(SetFailedTestCases,TestingFails);

 newTestCase(format('Checking Skipped Counter has increased by %d',[Expectedskips]));
 CheckIsEqual(TestingSkips+expectedSkips,SetSkippedTestCases);

 UpdateCounters;
  if (TestingSkips=expectedSkips) then
 begin
   Println(sysutils.format('The %d skips above are expected, so that "passes"',[ExpectedSkips]), clMessage);
   dec(SetSkippedTestCases,expectedSkips);
   Inc(SetPassedTestCases,expectedSkips);
 end
 else
 begin
   Println('Wrong number of skips!', FOREGROUND_YELLOW);
   SetSkippedTestCases := SetSkippedTestCases - (expectedSkips-TestingSkips);
   setFailedTestCases := setFailedTestCases + (expectedSkips-TestingSkips);
 end;

end;

end.
