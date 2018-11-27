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

procedure Test_LCS_returns_Correct_Result;
procedure Test_LCSDiff_returns_Correct_Result;
Procedure Test_LCSDiffences_returns_Correct_Result;
Procedure Test_LCSDiffences_Handles_Complex_JSON_Difference;
Procedure Test_Simple_Types_Compare_sensibly;

Procedure Test_Difference_compare_easier_to_read;
Procedure Test_Console_Column_Displays_Colums_as_Expected;

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

procedure Test_LCS_returns_Correct_Result;
var
  lS1, lS2: string;
  lExpected, lResult: string;
begin
  lS1 := '';
  lS2 := '';
  lResult := LCSStr(lS1, lS2);
  lExpected := '';
  NewTest('Empty values return empty');
  checkIsEqual(lExpected, lResult);

  lS1 := 'A';
  lS2 := '';
  lResult := LCSStr(lS1, lS2);
  lExpected := '';
  NewTest('One Empty value return empty');
  checkIsEqual(lExpected, lResult);

  lS1 := 'A';
  lS2 := 'B';
  lResult := LCSStr(lS1, lS2);
  lExpected := '';
  NewTest('Two different chars return empty');
  checkIsEqual(lExpected, lResult);

  lS1 := 'ACD';
  lS2 := 'EFG';
  lResult := LCSStr(lS1, lS2);
  lExpected := '';
  NewTest('Two different strings of the same length return empty');
  checkIsEqual(lExpected, lResult);

  lS1 := 'ACDHIJKLMNOP';
  lS2 := 'EFG';
  lResult := LCSStr(lS1, lS2);
  lExpected := '';
  NewTest('Two strings of the different lengths return empty');
  checkIsEqual(lExpected, lResult);

  lS1 := 'B';
  lS2 := 'B';
  lResult := LCSStr(lS1, lS2);
  lExpected := 'B';
  NewTest('Two same chars return char');
  checkIsEqual(lExpected, lResult);

  lS1 := 'ACD';
  lS2 := 'ECG';
  lResult := LCSStr(lS1, lS2);
  lExpected := 'C';
  NewTest('Two strings of the same length 1 char same returns 1 char');
  checkIsEqual(lExpected, lResult);

  lS1 := 'ACDHEJKLMNOP';
  lS2 := 'EFG';
  lResult := LCSStr(lS1, lS2);
  lExpected := 'E';
  NewTest('Two strings of the differnent lengths with 1 char same returns 1 char');
  checkIsEqual(lExpected, lResult);

  lS2 := 'ACDHEJKLMNOP';
  lS1 := 'AFG';
  lResult := LCSStr(lS1, lS2);
  lExpected := 'A';
  NewTest('Two strings of the differnent lengths with 1 char same returns 1 char');
  checkIsEqual(lExpected, lResult);

end;

procedure Test_LCSDiff_returns_Correct_Result;
var
  lS1, lS2: string;
  lResult: TStringDifference;
begin
  lS1 := '';
  lS2 := '';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Empty values return empty');
  checkIsEqual('', lResult.Same);
  checkIsEqual('', lResult.FirstBefore);
  checkIsEqual('', lResult.FirstAfter);
  checkIsEqual('', lResult.SecondBefore);
  checkIsEqual('', lResult.SecondAfter);

  lS1 := 'A';
  lS2 := '';
  lResult := LCSDiff(lS1, lS2);
  NewTest('One Empty value return Text in Before');
  checkIsEqual('', lResult.Same);
  checkIsEqual('A', lResult.FirstBefore);
  checkIsEqual('', lResult.FirstAfter);
  checkIsEqual('', lResult.SecondBefore);
  checkIsEqual('', lResult.SecondAfter);

  lS1 := 'A';
  lS2 := 'B';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Two different chars Text in Before');
  checkIsEqual('', lResult.Same);
  checkIsEqual('A', lResult.FirstBefore);
  checkIsEqual('', lResult.FirstAfter);
  checkIsEqual('B', lResult.SecondBefore);
  checkIsEqual('', lResult.SecondAfter);

  lS1 := 'ACD';
  lS2 := 'EFG';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Two different strings of the same length return Text in Before');
  checkIsEqual('', lResult.Same);
  checkIsEqual('ACD', lResult.FirstBefore);
  checkIsEqual('', lResult.FirstAfter);
  checkIsEqual('EFG', lResult.SecondBefore);
  checkIsEqual('', lResult.SecondAfter);

  lS1 := 'ACDHIJKLMNOP';
  lS2 := 'EFG';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Two strings of the different lengths return Text in Before');
  checkIsEqual('', lResult.Same);
  checkIsEqual('ACDHIJKLMNOP', lResult.FirstBefore);
  checkIsEqual('', lResult.FirstAfter);
  checkIsEqual('EFG', lResult.SecondBefore);
  checkIsEqual('', lResult.SecondAfter);

  lS1 := 'B';
  lS2 := 'B';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Two same chars return char and nothing in before or after');
  checkIsEqual('B', lResult.Same);
  checkIsEqual('', lResult.FirstBefore);
  checkIsEqual('', lResult.FirstAfter);
  checkIsEqual('', lResult.SecondBefore);
  checkIsEqual('', lResult.SecondAfter);

  lS1 := 'ACD';
  lS2 := 'ECG';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Same length String with 1 char same returns char and text in B,A');
  checkIsEqual('C', lResult.Same);
  checkIsEqual('A', lResult.FirstBefore);
  checkIsEqual('D', lResult.FirstAfter);
  checkIsEqual('E', lResult.SecondBefore);
  checkIsEqual('G', lResult.SecondAfter);

  lS1 := 'ACDHEJKLMNOP';
  lS2 := 'EFG';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Different length strings with 1 char same returns char and FB,FA,SA');
  checkIsEqual('E', lResult.Same);
  checkIsEqual('ACDH', lResult.FirstBefore);
  checkIsEqual('JKLMNOP', lResult.FirstAfter);
  checkIsEqual('', lResult.SecondBefore);
  checkIsEqual('FG', lResult.SecondAfter);

  lS1 := 'AFG';
  lS2 := 'ACDHEJKLMNOP';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Different length strings with first char same returns char and FA,BA');
  checkIsEqual('A', lResult.Same);
  checkIsEqual('', lResult.FirstBefore);
  checkIsEqual('FG', lResult.FirstAfter);
  checkIsEqual('', lResult.SecondBefore);
  checkIsEqual('CDHEJKLMNOP', lResult.SecondAfter);

  lS1 := 'AFABCDEFGG';
  lS2 := '123456ABCDEFG8901234';
  lResult := LCSDiff(lS1, lS2);
  NewTest('Different length strings with first char same returns char and FA,BA');
  checkIsEqual('ABCDEFG', lResult.Same);
  checkIsEqual('AF', lResult.FirstBefore);
  checkIsEqual('G', lResult.FirstAfter);
  checkIsEqual('123456', lResult.SecondBefore);
  checkIsEqual('8901234', lResult.SecondAfter);

end;

Procedure Test_LCSDiffences_returns_Correct_Result;
var
  i: integer;
  lS1, lS2: string;
  lRes: TStringDifference;
  lResult: TStringDifferences;
begin
  (* *)
  NewTest('Compare Short String');
  lS1 := 'ABC';
  lS2 := 'DEF';
  lResult := LCSDifferences(lS1, lS2);
  checkIsEqual(1, length(lResult));
  checkIsEqual('', lResult[0].Same);
  checkIsEqual('ABC', lResult[0].FirstBefore);
  checkIsEqual('', lResult[0].FirstAfter);
  checkIsEqual('DEF', lResult[0].SecondBefore);
  checkIsEqual('', lResult[0].SecondAfter);
  (* *)

  lS1 := 'the fox';
  lS2 := 'the fix';
  lRes := LCSDiff(lS1, lS2);
  checkIsEqual('the f', lRes.Same);
  checkIsEqual('', lRes.FirstBefore);
  checkIsEqual('ox', lRes.FirstAfter);
  checkIsEqual('', lRes.SecondBefore);
  checkIsEqual('ix', lRes.SecondAfter);
  (* *)

  lS1 := 'The quick brown fox jumps over the lazy dog';
  lS2 := 'The quick brown fix jumps over the lazy dog';
  lResult := LCSDifferences(lS1, lS2);
  NewTest('Compare Lines without breaks String - returns 3 rows');
  checkIsEqual(3, length(lResult));
  NewTest('Compare Lines without breaks String[0]');
  checkIsEqual('The quick brown f', lResult[0].Same);
  NewTest('Compare Lines without breaks String[0]-First');
  checkIsEqual('', lResult[0].FirstBefore);
  checkIsEqual('o', lResult[0].FirstAfter);
  checkIsEqual(1, lResult[0].FirstPos);
  NewTest('Compare Lines without breaks String[0]-Second');
  checkIsEqual('', lResult[0].SecondBefore);
  checkIsEqual('i', lResult[0].SecondAfter);
  checkIsEqual(1, lResult[0].SecondPos);

  NewTest('Compare Lines without breaks String[1]');
  checkIsEqual('', lResult[1].Same);
  NewTest('Compare Lines without breaks String[1]-First');
  checkIsEqual('o', lResult[1].FirstBefore);
  checkIsEqual('', lResult[1].FirstAfter);
  checkIsEqual(0, lResult[1].FirstPos);
  NewTest('Compare Lines without breaks String[1]-Second');
  checkIsEqual('i', lResult[1].SecondBefore);
  checkIsEqual('', lResult[1].SecondAfter);
  checkIsEqual(0, lResult[1].SecondPos);

  NewTest('Compare Lines without breaks String[2]');
  checkIsEqual('x jumps over the lazy dog', lResult[2].Same);
  NewTest('Compare Lines without breaks String[2]-First');
  checkIsEqual('The quick brown fo', lResult[2].FirstBefore);
  checkIsEqual('', lResult[2].FirstAfter);
  checkIsEqual(19, lResult[2].FirstPos);
  NewTest('Compare Lines without breaks String[2]-Second');
  checkIsEqual('The quick brown fi', lResult[2].SecondBefore);
  checkIsEqual('', lResult[2].SecondAfter);
  checkIsEqual(19, lResult[2].SecondPos);

end;

Procedure Test_LCSDiffences_Handles_Complex_JSON_Difference;
var
  lS1, lS2: string;
  lResult: TStringDifferences;
  i: integer;
begin
  (* *)
  lS1 := '{"Authentication": {'#1'"Username": "USERX",'#1'"Password": "XXXXXX"},'
    + '"CorrelationId": "30BA96DD-398A-4EED-8696-F9F6B0F88877","RequestStartTime":'
    + '"1899-12-30T00:00:00.000Z","EntityTypeId": 2,"EntityId": ${#Project#ContractId},'
    + '"InterfaceId": 100,"IPAddress": "127.0.0.1","CallerSystemId": 2,"DownloadDocumentInput":'
    + '{'#1'"DocumentTypeCode": 101,'#1'"Version": 0,'#1'"RequestUsername":' +
    ' "OOO000"}}';

  lS2 := '{"DownloadDocumentInput":{"DocumentTypeCode":101,"DocumentLocationId":0,"Version":'
    + '0,"RequestUsername":"OOO000","RefCount":0},"Authentication":{"Username":'
    + '"USERX","Password":"XXXXXX","Token":"","RefCount":0},"RequestStartTime"'
    + ':"1899-12-30T00:00:00.000+11:00","CallerSystemId":2,"CorrelationId":' +
    '"30BA96DD-398A-4EED-8696-F9F6B0F88877","EntityId":0,"EntityTypeId":0,' +
    '"InterfaceId":100,"IPAddress":"127.0.0.1","RefCount":0}';

  checkIsEqual(ls1,ls2);
  lResult := LCSDifferences(lS1, lS2);


  NewTest('Complex JSON - returns 74 rows');
  checkIsEqual(74, length(lResult));
  NewTest('Complex JSON[0]');
  checkIsEqual('{', lResult[0].Same);
  NewTest('Complex JSON[0]-First');
  checkIsEqual('', lResult[0].FirstBefore);
  checkIsEqual('', lResult[0].FirstAfter);
  checkIsEqual(1, lResult[0].FirstPos);
  NewTest('Complex JSON[0]-Second');
  checkIsEqual('', lResult[0].SecondBefore);

  checkIsEqual( '"DownloadDocumentInput":{"DocumentTypeCode":101,"DocumentLocationId":0,"Versi'+
 'on":0,"RequestUsername":"OOO000","RefCount":0},', lResult[0].SecondAfter);
  checkIsEqual(1, lResult[0].SecondPos);

   NewTest('Complex JSON[68]');
    checkIsEqual('', lResult[1].Same);
    NewTest('Complex JSON[68]-First');
    checkIsEqual('questUsername', lResult[68].FirstBefore);
    checkIsEqual(' "OOO000"}}', lResult[68].FirstAfter);
    checkIsEqual(0, lResult[68].FirstPos);
    NewTest('Complex JSON[68]-Second');
    checkIsEqual('i', lResult[68].SecondBefore);
    checkIsEqual('', lResult[68].SecondAfter);
    checkIsEqual(0, lResult[68].SecondPos);

    NewTest('Complex JSON[2]');
    checkIsEqual('x jumps over the lazy dog', lResult[2].Same);
    NewTest('Complex JSON[2]-First');
    checkIsEqual('o', lResult[2].FirstBefore);
    checkIsEqual('', lResult[2].FirstAfter);
    checkIsEqual(19, lResult[2].FirstPos);
    NewTest('Complex JSON[2]-Second');
    checkIsEqual('i', lResult[2].SecondBefore);
    checkIsEqual('', lResult[2].SecondAfter);
    checkIsEqual(19, lResult[2].SecondPos);


end;

Procedure Test_Difference_compare_easier_to_read;
begin
  (**)
  NewTest('Compare Short String');
  checkIsEqual('ABC', 'DEF');

  NewTest('Compare Lines without breaks String');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fix jumps over the lazy dog');

  NewTest('Compare Lines without breaks String and multiple differences');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
               'The quoKC brown fix jumps iver the lzay dog');
 (**)

  NewTest('Compare Lines with omission ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The brown fox jumps over the lazy dog');
  (**)
  NewTest('Compare Lines with Addition ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fox jumps jumps over the lazy dog');
  (**)
  NewTest('Compare Lines with omission at the front ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'brown fox jumps over the lazy dog');

  NewTest('Compare Lines with omission at the end ');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fox jumps over the');

  NewTest('Compare Lines with multiple differences String');
  checkIsEqual('The quick brown fox jumps over the lazy dog',
    'The quick brown fix jumps over the lasy dog');
  (**)

  NewTest('Compare MultiLines Results');
  checkIsEqual('The quick brown fox jumps over the lazy dog and'#13#10 +
    ' there are multiple lines to deal with'#13#10#13#10 +
    '<1 Empty Line Above> But still works OK',
    'The quick brown fix jumps over the lazy dog and'#13#10 +
    ' there are multople lines to dwal with'#13#10#13#10 +
    '<1 Empty Line Above> But still works alright');
  (* *)

end;

Procedure Test_Console_Column_Displays_Colums_as_Expected;
var lResult : TConsoleColumn;
  i: Integer;
begin
    lResult := TConsoleColumn.Create(40, clExpectedText, False);
    lResult.AddText('1234567890123456789012345678901234567890',clActualText);
    lResult.AddText('Text ok'#13#10'And Another',clExpectedText);
    lResult.AddText('more text',clTextDifferent);
    lResult.Finalise(clExpectedText);
    for i := 0 to lResult.LineCount-1 do
    begin
       Print(lResult.Lines[i].Text,lResult.Lines[i].Colour);
       if lResult.Lines[i].EOL then Writeln;
    end;
    Writeln;
end;

end.
