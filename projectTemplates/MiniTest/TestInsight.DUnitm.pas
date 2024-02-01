unit TestInsight.DUnitm;

interface

uses
  System.SysUtils,
  TestInsight.Client,
  DUnitX.TestFramework,
  MiniTestFramework;

type

  TTestInsightDUnitmReporter = Record
  private
    FTests: TArray<string>;
    FOptions: TTestInsightOptions;
    FInitialized: string;
    FClient: ITestInsightClient;
    procedure SetClient(const Value: ITestInsightClient);
    function GetIsInitialized: boolean;
  private
    function TestSelectedIndex(const ACasePath: string): Integer;
    function ShouldReport(const ACase: TTestSet; const ATestName: string):boolean;
  public
    procedure Starting(const ACount:integer);
    procedure Ending;
    procedure Init(AClient: ITestInsightClient);
    procedure CaseBeginning(const ACase: TTestSet);
    procedure TestBeginning(const ACase: TTestSet; const ATestname: string);
    procedure TestExecuted(const ACase: TTestSet;
  const ATestname, AResult, AMessage: string; const ATimeTaken: Extended);
    property Client: ITestInsightClient read FClient write SetClient;
    property IsInitialized: boolean read GetIsInitialized;
    property Options: TTestInsightOptions read FOptions;
    property Tests: TArray<string> read FTests;
  End;


procedure RunRegisteredTests(const baseUrl: string = DefaultUrl);

implementation

uses
  JclDebug;

var
  Insight: TTestInsightDUnitmReporter;


procedure PrintCallStack;
var
  StackList: TJclStackInfoList;
  I: Integer;
begin
  StackList := JclCreateStackList(True, 1, nil); // True to include current function
  try
    for I := 0 to StackList.Count - 1 do
    begin
      with StackList.Items[I] do
      begin
        // Here you can access the CallerAddr, SourceName, LineNumber, etc.
        Writeln(Format('Unit: %s, Method: %s, Line: %d', [UnitName, ProcedureName, LineNumber]));
      end;
    end;
  finally
    StackList.Free;
  end;
end;


procedure RunRegisteredTests(const baseUrl: string = DefaultUrl);
var
  lTest: string;
begin
  println('Test Insight - Running', clMessage);
  Insight.Init(TTestInsightRestClient.Create(baseUrl));
  if length(Insight.Tests)>19 then
    PrintLn('Selected Tests: Many...')
  else
  begin
    for lTest In Insight.Tests do
      println('  '+ lTest);
  end;

  MiniTestFramework.StartingEvent := Insight.Starting;
  MiniTestFramework.EndedEvent := Insight.Ending;
  MiniTestFramework.CaseStartingEvent := Insight.CaseBeginning;
  MiniTestFramework.TestStartingEvent := Insight.TestBeginning;
  MiniTestFramework.TestExecutedEvent := Insight.TestExecuted;

end;


function CasePath(const ACase: TTestSet; const ATest: string): string;
begin
  result := format('%s.%s.%s', [ACase.Title, Acase.SetName, ACase.TestCaseName, ATest]);
  if not ATest.IsEmpty then
    result := format('%s.%s',[Result, ATest]);
end;

{ TTestInsightDUnitmReporter }

procedure TTestInsightDUnitmReporter.CaseBeginning(const ACase: TTestSet);
begin
  TestBeginning(ACase, '');
end;

procedure TTestInsightDUnitmReporter.Ending;
begin
  Client.FinishedTesting;
end;

function TTestInsightDUnitmReporter.GetIsInitialized: boolean;
begin
  result := not FInitialized.IsEmpty;
end;

procedure TTestInsightDUnitmReporter.Init(AClient: ITestInsightClient);
begin
   FInitialized := 'true';
   FClient := AClient;
   FTests := Client.GetTests;
end;

procedure TTestInsightDUnitmReporter.SetClient(const Value: ITestInsightClient);
begin
  FClient := Value;
end;

function TTestInsightDUnitmReporter.ShouldReport(
  const ACase: TTestSet; const ATestName: string): boolean;
begin
   Result := true;
   if (Length(Tests)>0) and (TestSelectedIndex(CasePath(ACase, ATestName))<0) then
     exit;
end;

procedure TTestInsightDUnitmReporter.Starting(const ACount: integer);
begin
  printLn('Test Insight - Started Testing');
  Client.StartedTesting(ACount)
end;

procedure TTestInsightDUnitmReporter.TestExecuted(const ACase: TTestSet;
  const ATestname, AResult, AMessage: string; const ATimeTaken: Extended);
var
  error: ITestError;
  testResult: TTestInsightResult;
  lFullTestname: string;

  function InsightResultType: TResultType;
  var
    i: integer;
  begin
    for i := 0 to 3 do
      if PASS_FAIL[i]=AResult then
      begin
        case i of
           0 : Result := TResultType.Passed;
           1 : Result := TResultType.Skipped;
           2 : Result := TResultType.Failed;
           3 : Result := TResultType.Error;
        end;
      end;
  end;

  begin

  if ACase.TestCaseName.Length=0 then
    exit;

  testResult := TTestInsightResult.Create(
    InsightResultType, ACase.TestCaseName, ACase.SetName );

  lFullTestname := format('%s.%s',[ACase.SetName, ATestname ]);
  testResult.Duration := Round(ATimeTaken*1000);
  testResult.UnitName := ACase.Title;
  testResult.ClassName := ACase.SetName;
  testResult.MethodName := lFullTestName;
  testResult.ExceptionMessage := AMessage;
  testResult.Path := casePath(ACase, ATestName);
  printLn('Test Insight - Test Executed' + testResult.Path, clMessage);

  Client.PostResult(testResult);
end;

function TTestInsightDUnitmReporter.TestSelectedIndex(
  const ACasePath: string): Integer;
var
  lTest: String;
  lSearchStr: string;
  i: integer;
begin
  result := -1;
  lSearchStr := format(' %s ',[ACasePath]);
  for i := 0 to length(Tests) - 1 do
  begin
    lTest := format('%s ',[Tests[i]]);
    if pos(lSearchstr,lTest)>=0 then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TTestInsightDUnitmReporter.TestBeginning(const ACase: TTestSet;
  const ATestname: string);
var
  lActiveTest: TTestInsightResult;
begin
  lactiveTest := TTestInsightResult.Create(
    TResultType.Running, ACase.TestCaseName, ACase.SetName );
  lactiveTest.Duration := 0;
  lactiveTest.UnitName := ACase.Title;
  lactiveTest.ClassName := ACase.SetName;
  lactiveTest.MethodName := ACase.TestCaseName;
  lactiveTest.Path := CasePath(ACase, ATestName);
  printLn('Test Insight - Test Beginning ' + lactiveTest.Path, clMessage);
  Client.PostResult(lactiveTest);
end;

initialization

finalization
  Insight.Client:=nil;

end.
