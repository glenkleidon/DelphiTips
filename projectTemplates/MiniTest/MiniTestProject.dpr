program MiniTestProject;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in 'MiniTestFramework.pas',
  {$IFDEF TESTINSIGHT}
  TestInsight.Dunitm,
  {$ENDIF }
  TestCases1 in 'TestCases1.pas',
  ToolsAPI in 'c:\Program Files (x86)\Embarcadero\Studio\22.0\source\ToolsAPI\ToolsAPI.pas',
  JclDebug in '..\..\..\jcl-JCL-2.7-Build5676\jcl\source\windows\JclDebug.pas';

begin
  try
    {$IFDEF TESTINSIGHT}
     RunRegisteredTests;
    {$ENDIF}
    Title('Test Cases For <Project> Units');
    RunTestSets;
    TestSummary;
    TestingCompleted;

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
