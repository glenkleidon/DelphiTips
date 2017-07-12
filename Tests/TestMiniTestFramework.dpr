program TestMiniTestFramework;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas',
  TestCasesMiniTestFramework in 'TestCasesMiniTestFramework.pas';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    Title('Testing the Framework...With the framework!!');

    CheckIsTrue_works_as_expected;
    CheckIsEqual_works_as_Expected;
    Test_Skip_works_as_expected;




    TestSummary;

    if sameText(Paramstr(1),'/p') then ReadLn;

    ExitCode := TotalErrors+TotalFailedTestCases;


  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
