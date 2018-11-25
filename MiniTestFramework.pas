unit MiniTestFramework;

interface

{$IFDEF VER120} {$DEFINE BEFOREVARIANTS} {$ENDIF}
{$IFDEF VER130} {$DEFINE BEFOREVARIANTS} {$ENDIF}

uses SysUtils, windows
{$IFNDEF BEFOREVARIANTS}
    , Variants
{$ENDIF}
    ;

Const
  FRAMEWORK_VERSION = '2.0.0.1';
  DEFAULT_TOTALS_FORMAT =
    'Run>Sets:%-3d Cases:%-3d Tests:%-4d Passed:%-4d Failed:%-3d Skipped:%-3d Errors:%-3d';
  DEFAULT_SET_FORMAT =
    'Set>Cases:%-4d Tests:%-4d Passed:%-4d Failed:%-4d Skipped:%-4d Errors:%-4d';
  DEFAULT_CASE_FORMAT =
    '  Results> Passed:%-5d Failed:%-5d Skipped:%-5d Errors:%-5d';
  DEFAULT_SET_NAME = 'SET';
  FINAL_SET_NAME = '__';
  EXPECTED_FORMAT_MESSAGE = '   Expected:';
  ACTUAL_FORMAT_MESSAGE = '   Actual  :';
  EXPECTED_ACTUAL_SEPARATOR = #1#13#10;
  EXPECTED_ACTUAL_FORMAT = '%s' + EXPECTED_FORMAT_MESSAGE + '<%s>' +
    EXPECTED_ACTUAL_SEPARATOR + ACTUAL_FORMAT_MESSAGE + '<%s>';

  PASS_FAIL: array [0 .. 3] of string = ('PASS', 'SKIP', 'FAIL', 'ERR ');
  FOREGROUND_DEFAULT = 7;

  DEFAULT_SCREEN_WIDTH = 80;
  FOREGROUND_CYAN = 3;
  FOREGROUND_YELLOW = 6;
  FOREGROUND_PURPLE = 5;
  BACKGROUND_YELLOW = $60;
  BACKGROUND_WHITE = BACKGROUND_BLUE OR BACKGROUND_GREEN OR BACKGROUND_RED;

  clTitle = FOREGROUND_YELLOW;
  clError = FOREGROUND_RED or FOREGROUND_INTENSITY;
  clPass = FOREGROUND_GREEN;
  clMessage = FOREGROUND_CYAN;
  clDefault = FOREGROUND_DEFAULT;
  clSkipped = FOREGROUND_PURPLE;
  // Difference Colours
  clTextDifferent = BACKGROUND_WHITE OR FOREGROUND_RED or FOREGROUND_INTENSITY;
  clExpectedText = FOREGROUND_GREEN OR BACKGROUND_WHITE;
  clActualText = FOREGROUND_BLUE OR BACKGROUND_WHITE;
  clOmission = BACKGROUND_YELLOW;

Type
  TComparitorType = Variant;

  TTestCaseProcedure = Procedure();

  TSkipType = (skipFalse, skipTrue, skipCase);

  TDifferenceType = (dtNone, dtCompareTooLong, dtCompareTooShort, dtDifferent,
    dtCompareHasOmission, dtCompareHasAddition, dtSubstitution);

  TLCSParams = Record
    Length: integer;
    FirstPos: integer;
    SecondPos: integer;
  end;

  TStringDifference = Record
    Same: String;
    FirstBefore: string;
    FirstAfter: string;
    FirstPos: integer;
    SecondBefore: string;
    SecondAfter: string;
    SecondPos: integer;
  end;

  TStringDifferences = Array of TStringDifference;

  TConsoleText = Record
    Text: string;
    Colour: integer;
    EOL: boolean;
  end;

  TConsoleTextArray = Array of TConsoleText;

  TTestSet = Record
    SetName: string;
    Execute: TTestCaseProcedure;
    TestCaseName: string;
    Skip: TSkipType;
    ExpectedException: string;
  end;

const
  SKIPPED = skipTrue;
  Skip = skipTrue; // alternate

var
  MiniTestCases: Array of TTestSet;

  SkippingSet, IgnoreSkip: boolean;
  CreatingSets: boolean = false;

  ExpectedException, ExpectedSetException, LastSetName, CurrentSetName,
    CurrentTestCaseName, CurrentTestCaseLabel, DeferredTestCaseLabel: string;
  TotalPassedTests: integer = 0;
  TotalFailedTests: integer = 0;
  TotalSkippedTests: integer = 0;
  TotalErroredTests: integer = 0;
  TotalCases: integer = 0;
  TotalSets: integer = 0;
  DifferencesFound: integer;

  CasePassedTests, CaseFailedTests, CaseErrors, CaseSkippedTests: integer;
  SetCases, SetPassedTests, SetFailedTests, SetErrors, SetSkippedTests: integer;

  TotalOutputFormat, SetOutputFormat, CaseOutputFormat: string;

Procedure Title(AText: string);

Procedure AddTestSet(ATestCaseName: string; AProcedure: TTestCaseProcedure;
  ASkipped: TSkipType = skipFalse; AExpectedException: string = '');
{$IFNDEF BEFOREVARIANTS}deprecated; {$ENDIF} // wrong naming convention.

Procedure AddTestCase(ATestCaseName: string; AProcedure: TTestCaseProcedure;
  ASkipped: TSkipType = skipFalse; AExpectedException: string = '');
Procedure PrepareSet(AProcedure: TTestCaseProcedure);
Procedure FinaliseSet(AProcedure: TTestCaseProcedure);
Procedure FinalizeSet(AProcedure: TTestCaseProcedure);
Procedure RunTestSets;
Procedure SkipTestCases(ACaseId: integer);

Procedure NewTest(ACase: string; ATestCaseName: string = '');
Procedure NewSet(ASetName: string);
Procedure NewCase(ATestCaseName: string);
Procedure NewTestCase(ACase: string; ATestCaseName: string = '');
{$IFNDEF BEFOREVARIANTS}deprecated; {$ENDIF} // wrong naming convention.
Procedure NextTestSet(ASetName: string);
Procedure NextTestCase(ACaseName: string; ASkipped: TSkipType = skipFalse);
Function CheckIsEqual(AExpected, AResult: TComparitorType;
  AMessage: string = ''; ASkipped: TSkipType = skipFalse): boolean;
Function CheckIsTrue(AResult: boolean; AMessage: string = '';
  ASkipped: TSkipType = skipFalse): boolean;
Function CheckIsFalse(AResult: boolean; AMessage: string = '';
  ASkipped: TSkipType = skipFalse): boolean;
Function CheckNotEqual(AResult1, AResult2: TComparitorType;
  AMessage: string = ''; ASkipped: TSkipType = skipFalse): boolean;
Procedure DeferTestCase(ATestName: string = '');
Procedure DeferredTestSuccess;
Procedure DeferredTestFail;
Procedure DeferredTestException(E: Exception);
Procedure ResumeTestCase;
Procedure ExpectException(AExceptionClassName: string;
  AExpectForSet: boolean = false);
Procedure CheckException(AException: Exception);
Function NotImplemented(AMessage: string = ''): boolean;
Function DontSkip: TSkipType;
Function TotalTests: integer;
Procedure TestSummary;
procedure CaseResults;
Function ConsoleScreenWidth: integer;
Procedure Print(AText: String; AColour: smallint = FOREGROUND_DEFAULT);
Procedure PrintLn(AText: String; AColour: smallint = FOREGROUND_DEFAULT);
Procedure PrintLnCentred(AText: string; AChar: char;
  AColour: smallint = FOREGROUND_DEFAULT);

// Difference Engine
// function lcs(a, b: string): string;
function LCSDiff(AText, ACompareTo: string): TStringDifference;
function LCSDifferences(AText, ACompareTo: string): TStringDifferences;
function lcsStr(a, b: string): string;
Procedure ResetStringDifference(var ADiff: TStringDifference);
procedure DisplayMessage(AMessage: String; AMessageColour: smallint;
  ADataType: integer);

implementation

uses classes;

Const
  NIL_EXCEPTION_CLASSNAME = 'NilException';
  NO_EXCEPTION_EXPECTED = 'No Exceptions';

Type
  TCheckTestType = (cttComparison, cttSkip, cttException);
  TIntArray = array of integer;
  TIntArrayArray = array of TIntArray;

{$IFDEF BEFOREVARIANTS}
  TvarType = integer;
{$ENDIF}

var
  SameTestCounter: integer = 0;
  LastTestCaseLabel: string;
  SetCounter: integer = 0;

  /// ////////  SCREEN MANAGEMENT \\\\\\\\\\\\\\\\\

var
  Screen_width: integer = -1;
  Console_Handle: THandle = 0;

Function CanDisplayCaseName(ACaseName: string): boolean;
begin
  Result := (ACaseName <> '') and (not(CreatingSets));
end;

Function NextSetName: string;
begin
  inc(SetCounter);
  Result := Format(DEFAULT_SET_NAME + ' %u', [SetCounter]);
end;

Function TotalTests: integer;
begin
  Result := TotalPassedTests + TotalFailedTests + TotalSkippedTests +
    TotalErroredTests + CasePassedTests + CaseFailedTests + CaseSkippedTests +
    CaseErrors;
end;

Function ConsoleHandle: THandle;
begin
  if Console_Handle = 0 then
    Console_Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  Result := Console_Handle;
end;

procedure DoubleLine;
begin
  PrintLn(stringofchar('=', ConsoleScreenWidth));
end;

procedure SingleLine;
begin
  PrintLn(stringofchar('-', ConsoleScreenWidth));
end;

Procedure SetTextColour(AColour: smallint);
begin
  SetConsoleTextAttribute(ConsoleHandle, AColour);
end;

Function ConsoleScreenWidth: integer;
var
  lScreenInfo: TConsoleScreenBufferInfo;
begin
  if Screen_width = -1 then
  begin
    try
      GetConsoleScreenBufferInfo(ConsoleHandle, lScreenInfo);
      Screen_width := lScreenInfo.dwSize.X - 2;
    except
      Screen_width := DEFAULT_SCREEN_WIDTH;
    end;
  end;
  Result := Screen_width;
end;

Procedure Print(AText: String; AColour: smallint = FOREGROUND_DEFAULT);
begin
  SetTextColour(AColour);
  Write(AText);
  if AColour <> FOREGROUND_DEFAULT then
    SetTextColour(FOREGROUND_DEFAULT);
end;

Procedure PrintLn(AText: String; AColour: smallint = FOREGROUND_DEFAULT);
begin
  Print(AText + #13#10, AColour);
end;

Procedure PrintLnCentred(AText: string; AChar: char;
  AColour: smallint = FOREGROUND_DEFAULT);
var
  PreSpace, PostSpace, TitleSpace: integer;
begin
  TitleSpace := ConsoleScreenWidth - 4;
  PreSpace := trunc((TitleSpace - Length(AText)) / 2);
  PostSpace := TitleSpace - PreSpace - Length(AText);
  Print(stringofchar(AChar, PreSpace));
  Print('  ' + AText + '  ', AColour);
  PrintLn(stringofchar(AChar, PostSpace));
end;

Procedure AddTestSet(ATestCaseName: string; AProcedure: TTestCaseProcedure;
  ASkipped: TSkipType; AExpectedException: string);
begin
  AddTestCase(ATestCaseName, AProcedure, ASkipped, AExpectedException);
end;

Procedure AddTestCase(ATestCaseName: string; AProcedure: TTestCaseProcedure;
  ASkipped: TSkipType; AExpectedException: string);
var
  l: integer;
begin
  if Length(CurrentSetName) = 0 then
    CurrentSetName := NextSetName;

  l := Length(MiniTestCases);
  SetLength(MiniTestCases, l + 1);
  MiniTestCases[l].SetName := CurrentSetName;
  MiniTestCases[l].Execute := AProcedure;
  MiniTestCases[l].TestCaseName := ATestCaseName;
  MiniTestCases[l].Skip := ASkipped;
  MiniTestCases[l].ExpectedException := AExpectedException;
end;

Procedure PrepareSet(AProcedure: TTestCaseProcedure);
begin
  AddTestCase('', AProcedure);
end;

Procedure FinaliseSet(AProcedure: TTestCaseProcedure);
begin
  AddTestCase('', AProcedure);
  CurrentSetName := '';
  CreatingSets := false;
end;

Procedure FinalizeSet(AProcedure: TTestCaseProcedure);
begin
  // Americaniz(s)ed form
  FinaliseSet(AProcedure);
end;

Procedure Title(AText: string);
var
  lText: string;
begin
  lText := 'DUnitm V-' + FRAMEWORK_VERSION;
  PrintLnCentred(lText, '=', clDefault);
  PrintLnCentred(AText, '=', clTitle);
  DoubleLine;
end;

function CaseHasErrors: smallint;
begin
  Result := 0;
  if CaseFailedTests + CaseErrors > 0 then
    Result := 2
  else if CaseSkippedTests > 0 then
    Result := 1;
end;

function SetHasErrors: smallint;
begin
  Result := 0;
  if SetFailedTests + SetErrors > 0 then
    Result := 2
  else if SetSkippedTests > 0 then
    Result := 1;
end;

function RunHasErrors: smallint;
var
  lSetResult: smallint;
begin
  Result := 0;
  if (TotalFailedTests + TotalErroredTests > 0) then
    Result := 2
  else if TotalSkippedTests > 0 then
    Result := 1;
  if Result = 2 then
    exit;
  lSetResult := CaseHasErrors;
  if lSetResult > Result then
    Result := lSetResult;
end;

Function ResultColour(AHasErrors: smallint): smallint;
var
  lIntesity: byte;
begin
  case AHasErrors AND 3 of
    0:
      Result := clPass;
    1:
      Result := clSkipped;
  else
    Result := clError;
  end;
  lIntesity := AHasErrors and 255 and
    (BACKGROUND_INTENSITY or FOREGROUND_INTENSITY);
  Result := Result or lIntesity;
end;

Function CaseIsEmpty: boolean;
begin
  Result := (CasePassedTests = 0) and (CaseFailedTests = 0) and
    (CaseSkippedTests = 0) and (CaseErrors = 0);
end;

procedure CaseResults;
begin

  if CaseIsEmpty then
    exit;

  PrintLn(Format(CaseOutputFormat, [CasePassedTests, CaseFailedTests,
    CaseSkippedTests, CaseErrors]), ResultColour(CaseHasErrors));

end;

Function SetIsEmpty: boolean;
begin
  Result := (SetPassedTests = 0) and (SetFailedTests = 0) and
    (SetSkippedTests = 0) and (SetErrors = 0);
end;

procedure SetResults;
begin

  if (SetIsEmpty) or (Length(CurrentSetName) = 0) then
    exit;

  PrintLn(Format(SetOutputFormat, [SetCases, SetPassedTests + SetFailedTests +
    SetSkippedTests + SetErrors, SetPassedTests, SetFailedTests,
    SetSkippedTests, SetErrors]), ResultColour(SetHasErrors));

  SingleLine;
end;

Procedure TestSummary;
begin
  NextTestCase('');
  DoubleLine;
  PrintLn(Format(TotalOutputFormat, [TotalSets, TotalCases, TotalTests,
    TotalPassedTests, TotalFailedTests, TotalSkippedTests, TotalErroredTests]),
    ResultColour(RunHasErrors or FOREGROUND_INTENSITY));
  WriteLn('');
end;

Procedure SetHeading(ASetName: string);
var
  lHeading: string;
begin
  if Length(ASetName) = 0 then
    exit;
  lHeading := 'Test Set:' + ASetName;
  PrintLn(lHeading, clTitle);
end;

/// //////// END SCREEN MANAGEMENT \\\\\\\\\\\\\\\\\

/// //////// TEST CASES  \\\\\\\\\\\\\\\\\

Procedure SkipTestCases(ACaseId: integer);
begin
  NewTest(MiniTestCases[ACaseId].TestCaseName);
  CheckIsTrue(false, 'Case Skipped', Skip);
end;

Procedure RunTestSets;
var
  i, l: integer;
begin
  CreatingSets := false;
  l := Length(MiniTestCases);
  if l > 0 then
  begin
    LastSetName := MiniTestCases[0].SetName;
    NextTestSet(LastSetName);
  end;
  SetCases := 0;
  for i := 0 to l - 1 do
    Try
      if not assigned(MiniTestCases[i].Execute) then
        continue;

      if MiniTestCases[i].Skip = skipCase then
      begin
        SkipTestCases(i);
        continue;
      end;

      CurrentSetName := MiniTestCases[i].SetName;
      if MiniTestCases[i].TestCaseName <> '' then
        NextTestCase(MiniTestCases[i].TestCaseName, MiniTestCases[i].Skip);
      ExpectException(MiniTestCases[i].ExpectedException, true);
      MiniTestCases[i].Execute;
      LastSetName := CurrentSetName;
    except
      on E: Exception do
        CheckException(E);
    end;
  // Last Case is done, Need to Update the Final Set Results.
  CurrentSetName := FINAL_SET_NAME;
  NextTestCase('');

  // Destroy the Cases.
  SetLength(MiniTestCases, 0);
  LastSetName := '';
  CurrentSetName := '';
end;

Procedure NextTestSet(ASetName: string);
begin

  SetResults;

  if ASetName = FINAL_SET_NAME then
    ASetName := ''; // Finalising.

  if Length(ASetName) > 0 then
  begin
    SetHeading(ASetName);
    inc(TotalSets);
  end;

  SetPassedTests := 0;
  SetFailedTests := 0;
  SetSkippedTests := 0;
  SetErrors := 0;
  SetCases := 0;

end;

Procedure NextTestCase(ACaseName: string; ASkipped: TSkipType);
var
  lHeading: string;
begin
  CaseResults;

  inc(SetPassedTests, CasePassedTests);
  inc(SetFailedTests, CaseFailedTests);
  inc(SetSkippedTests, CaseSkippedTests);
  inc(SetErrors, CaseErrors);

  if LastSetName <> CurrentSetName then
    NextTestSet(CurrentSetName);

  if CanDisplayCaseName(ACaseName) then
  begin
    lHeading := ' Test Case:' + ACaseName;
    PrintLn(lHeading, clTitle);
    inc(SetCases);
    inc(TotalCases);
  end;

  inc(TotalPassedTests, CasePassedTests);
  inc(TotalFailedTests, CaseFailedTests);
  inc(TotalSkippedTests, CaseSkippedTests);
  inc(TotalErroredTests, CaseErrors);
  SkippingSet := ASkipped <> skipFalse;
  IgnoreSkip := false;
  ExpectedSetException := '';
  CasePassedTests := 0;
  CaseFailedTests := 0;
  CaseSkippedTests := 0;
  CaseErrors := 0;
  SameTestCounter := 0;
  LastTestCaseLabel := '';
  CurrentTestCaseLabel := '';
  CurrentTestCaseName := ACaseName;
  ExpectedException := '';
end;

Procedure ExpectException(AExceptionClassName: string;
  AExpectForSet: boolean = false);
begin
  ExpectedException := AExceptionClassName;
  if AExpectForSet then
    ExpectedSetException := ExpectedException;
end;

function ValueAsString(AValue: TComparitorType): string;
var
  lType: TvarType;
begin
  lType := varType(AValue);
  case lType of
    varEmpty:
      Result := 'Empty';
    varNull:
      Result := 'null';
    varSingle, varDouble, varCurrency:
      Result := FloatToStr(AValue);
    varDate:
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AValue);
    varBoolean:
      If (AValue) Then
        Result := 'True'
      else
        Result := 'False';

    varSmallint, varInteger, varVariant, varByte
{$IFNDEF BEFOREVARIANTS}
      , varInt64, varShortInt, varWord, varLongWord
{$ENDIF}
{$IFDEF UNICODE}
      , varUInt64
{$ENDIF}
      :
      Result := IntToStr(AValue);

{$IFDEF UNICODE}
    varUString,
{$ENDIF}
    varOleStr, varStrArg, varString:
      Result := AValue;

  else
    Result := 'Unsupported Type';
  end;

end;

Function CompareValues(AExpected, AResult: TComparitorType): boolean;
var
  lExpectedType, lResultType: TvarType;
  lExpectedIsInteger, lExpectedIsNumber, lExpectedIsString, lResultIsInteger,
    lResultIsNumber, lResultIsString: boolean;
begin
  Result := false;
  lExpectedType := varType(AExpected);
  lResultType := varType(AResult);

  if (lExpectedType + lResultType = 1) OR (lExpectedType + lResultType = 1) then
  begin
    Result := true;
    exit;
  end;

  if (lExpectedType = lResultType) then
  begin
    Result := (AExpected = AResult);
    exit;
  end;

  lResultIsInteger := lResultType in [varByte, varSmallint, varInteger
{$IFNDEF BEFOREVARIANTS} , varShortInt, varWord, varLongWord, varInt64
{$ENDIF}
    ];
  if (lExpectedType = varBoolean) and (lResultIsInteger) then
  begin
    Result := (AExpected = (AResult = 0));
    exit;
  end;

  lExpectedIsInteger := lExpectedType in [varByte, varSmallint, varInteger
{$IFNDEF BEFOREVARIANTS} , varShortInt, varWord, varLongWord, varInt64
{$ENDIF}
    ];

  if (lExpectedIsInteger and lResultIsInteger) then
  begin
    Result :=
{$IFDEF BEFOREVARIANTS}
      varAsType(AExpected, varInteger) = varAsType(AResult, varInteger);
{$ELSE}
      varAsType(AExpected, varInt64) = varAsType(AResult, varInt64);
{$ENDIF}
    exit;
  end;

  if (lResultType = varBoolean) and (lExpectedIsInteger) then
  begin
    Result := (AExpected = 0) = AResult;
    exit;
  end;

  lExpectedIsNumber := (lExpectedIsInteger) or
    (lExpectedType in [varSingle, varDouble, varCurrency]);
  lResultIsNumber := (lResultIsInteger) or
    (lResultType in [varSingle, varDouble, varCurrency]);

  if (lExpectedIsNumber and lResultIsNumber) then
  begin
    Result := double(AExpected) = double(AResult);
    exit;
  end;

  lExpectedIsString := (lExpectedType = varString) or
    (lExpectedType in [varOleStr, varStrArg]);
  lResultIsString := (lResultType = varString) or
    (lResultType in [varOleStr, varStrArg]);
  if (lExpectedIsString and lResultIsString) then
  begin
    Result := AResult = AExpected;
    exit;
  end;
end;

Function Check(IsEqual: boolean; AExpected, AResult: TComparitorType;
  AMessage: string; ATestType: TCheckTestType): boolean;
var
  lMessage, lCounter: string;
  lResult: integer;
  Outcome: boolean;
  lMessageColour: smallint;
begin
  Result := false;
  lMessageColour := clDefault;
  lResult := 0;
  DifferencesFound := 0;
  try
    case ATestType Of
      cttSkip:
        begin
          lResult := 1;
          if AMessage = '' then
            lMessage := ' Test Skipped'
          else
            lMessage := ' ' + AMessage;
          lMessageColour := clSkipped;
          inc(CaseSkippedTests);
          exit;
        end;
      cttException:
        begin
          lMessageColour := clMessage;
          if AMessage = '' then
            lMessage := ' Exception.'
          else
            lMessage := ' ' + AMessage;
          if IsEqual then
          begin
            lResult := 0;
            inc(CasePassedTests);
          end
          else
          begin
            lResult := 3;
            inc(CaseErrors);
            lMessageColour := clMessage;
            lMessage := Format(EXPECTED_ACTUAL_FORMAT,
              [#13#10, ValueAsString(AExpected), ValueAsString(AResult)])
          end;
        end;
    else // case
      begin
        try
          lMessage := '';
          Outcome := CompareValues(AExpected, AResult);
          lMessageColour := clPass;
          if IsEqual <> Outcome then
          begin
            lResult := 2;
            lMessageColour := clError;
            inc(CaseFailedTests);
            if AMessage = '' then
            begin
              if IsEqual then
                lMessage := Format(EXPECTED_ACTUAL_FORMAT,
                  [#13#10, ValueAsString(AExpected), ValueAsString(AResult)])
              else
                lMessage :=
                  Format('%s   Expected outcomes to differ, but both returned %s%s',
                  [#13#10, ValueAsString(AExpected)]);
            end
            else
              lMessage := #13#10'   ' + AMessage;
            exit;
          end;
          inc(CasePassedTests);
        except
          on E: Exception do
          begin
            if (E.ClassName = ExpectedException) then
            begin
              // At this level, it will only be exceptions
              // for Variant type comparisons
              lResult := 0;
              inc(CasePassedTests);
            end
            else
            begin
              lResult := 2;
              lMessageColour := clMessage;
              lMessage := #13#10'   Illegal Comparison Test Framework: ' +
                E.Message;
              inc(CaseErrors);
            end;
          end;
        end;
      end; // case else
    end; // case
  finally
    if LastTestCaseLabel = CurrentTestCaseLabel then
      inc(SameTestCounter)
    else
      SameTestCounter := 1;
    LastTestCaseLabel := CurrentTestCaseLabel;
    if SameTestCounter = 1 then
      lCounter := ''
    else
      lCounter := '-' + IntToStr(SameTestCounter);
    if CurrentTestCaseLabel = '' then
    begin
      CurrentTestCaseLabel := copy('Test for ' + CurrentTestCaseName, 1,
        ConsoleScreenWidth - 4);
      if lCounter = '' then
      begin
        lCounter := '-1';
        LastTestCaseLabel := CurrentTestCaseLabel;
      end;
      ExpectedException := ExpectedSetException;
      IgnoreSkip := false;
    end;

    Print(Format('  %s-', [PASS_FAIL[lResult]]), lMessageColour);
    Print(Format('%s%s', [CurrentTestCaseLabel, lCounter]));
    DisplayMessage(lMessage, lMessageColour, varType(AResult));
    Result := (lResult = 0);
  end;
end;

function lcsMax(a, b: TLCSParams): TLCSParams;
begin
  if (a.Length > b.Length) then
    Result := a
  else
    Result := b;
end;

Function lcs(X, Y: string): TLCSParams;
var
  lSize, lBestSize, pF, pS, lx, ly, lr, i, j, k: integer;
  procedure SetBest;
  begin
    if (lSize > Result.Length) OR
      ((lSize = Result.Length) and (pS < Result.SecondPos)) then
    begin
      Result.FirstPos := pF;
      Result.SecondPos := pS;
      Result.Length := lSize;
      lSize := 0;
    end;
  end;

begin
  Result.Length := 0;
  Result.FirstPos := 0;
  Result.SecondPos := 0;
  lx := Length(X);
  ly := Length(Y);
  lSize := 0;
  for i := 1 to lx do
  begin
    for j := 1 to ly do
    begin
      if X[i] = Y[j] then
      begin
        pF := i;
        pS := j;
        k := 1;
        lSize := 1;
        while (((i + k) <= lx) and ((j + k) <= ly)) do
        begin
          if X[i + k] = Y[j + k] then
            inc(lSize)
          else
            break;
          inc(k);
        end;
        SetBest;
      end;
    end;
    SetBest;
  end;
end;

function lcsStr(a, b: string): string;
var
  l: TLCSParams;
begin
  l := lcs(a, b);
  Result := copy(a, l.FirstPos, l.Length);
end;

Procedure ResetStringDifference(var ADiff: TStringDifference);
begin
  ADiff.Same := '';
  ADiff.FirstBefore := '';
  ADiff.FirstAfter := '';
  ADiff.FirstPos := 0;
  ADiff.SecondBefore := '';
  ADiff.SecondAfter := '';
  ADiff.SecondPos := 0;
end;

function LCSDiff(AText, ACompareTo: string): TStringDifference;
var
  p, ls: integer;
begin
  ResetStringDifference(Result);
  Result.Same := lcsStr(AText, ACompareTo);
  ls := Length(Result.Same);
  if ls = 0 then
  begin
    // No Same text found.
    Result.FirstBefore := AText;
    Result.SecondBefore := ACompareTo;
    Result.FirstAfter := '';
    Result.SecondAfter := '';
  end
  else
  begin
    // There is a difference
    p := pos(Result.Same, AText);
    if p > 0 then // probably not really possible.
    begin
      Result.FirstBefore := copy(AText, 1, p - 1);
      Result.FirstAfter := copy(AText, p + ls, MAXINT);
      Result.FirstPos := p;
    end;
    p := pos(Result.Same, ACompareTo);
    if p > 0 then
    begin
      Result.SecondBefore := copy(ACompareTo, 1, p - 1);
      Result.SecondAfter := copy(ACompareTo, p + ls, MAXINT);
      Result.SecondPos := p;
    end;
  end;
end;

function LCSDifferences(AText, ACompareTo: string): TStringDifferences;
var
  lDiff: TStringDifference;
  ls, i, p: integer;
  lBeforeDifferences, lAfterDifferences: TStringDifferences;
begin
  if Length(AText) + Length(ACompareTo) = 0 then
    exit;
  lDiff := LCSDiff(AText, ACompareTo);
  ls := Length(lDiff.Same);
  if ls = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := lDiff;
    exit;
  end;

  // Get Differences before and after
  if lDiff.FirstBefore <> lDiff.SecondBefore then
    lBeforeDifferences := LCSDifferences(lDiff.FirstBefore, lDiff.SecondBefore);
  if lDiff.FirstAfter <> lDiff.SecondAfter then
    lAfterDifferences := LCSDifferences(lDiff.FirstAfter, lDiff.SecondAfter);

  // Now Assemble into a single result.
  SetLength(Result, Length(lBeforeDifferences) + Length(lAfterDifferences) + 1);
  p := 0;
  for i := Low(lBeforeDifferences) to High(lBeforeDifferences) do
  begin
    Result[p] := lBeforeDifferences[i];
    inc(p);
  end;
  Result[p] := lDiff;
  inc(p);
  for i := Low(lAfterDifferences) to High(lAfterDifferences) do
  begin
    Result[p] := lAfterDifferences[i];
    inc(p);
  end;
end;


procedure DisplayMessage(AMessage: String; AMessageColour: smallint;
  ADataType: integer);
var
  lExpected, lActual, lFormatStr: string;
  lColTextSize: integer;
  lLeftColumn, lRightColumn: TConsoleTextArray;
  lSingleRow: boolean;

  lStringDifference: TStringDifferences;
  p,i, StartDiff, lExpectedStart, lActualStart, lColumnWidth: integer;

// remove tab and delete
  Procedure EscapeTabs;
  var
    ii: integer;
    lPos: PChar;
  begin
    lPos := @AMessage[1];
    for ii := 1 to Length(AMessage) do
    begin
      case ord(lPos^) of
        9, 8, 127:
          lPos^ := #1;
      end;
      inc(lPos);
    end;
  end;

begin
  p := pos(EXPECTED_ACTUAL_SEPARATOR, AMessage);
  if (AMessageColour <> clError) OR
    ((p < 1) OR (pos(EXPECTED_FORMAT_MESSAGE, AMessage) < 1) OR
    (pos(ACTUAL_FORMAT_MESSAGE, AMessage) < 1)) then
  begin
    PrintLn(AMessage, AMessageColour);
    exit;
  end;

  // Only do the test on string data types
  if NOT((ADataType = varString) OR
{$IFNDEF BEFOREVARIANTS}
    (ADataType = varUString) or (ADataType = varUStrArg) or
{$ENDIF}
    (ADataType in [varStrArg, varOleStr])) then
  begin
    PrintLn(StringReplace(AMessage, #1, '', []), AMessageColour);
    exit;
  end;

  /// Ok, we have errored - look for the reason.
  /// The idea is to do this:
  /// If the lines can be compared above and below each other
  /// ie the width of each is less than the screen, just show
  /// the result under each other, highlingting the difference
  /// Expected :<This is expected>
  /// Actual   :<this is Actual>
  /// ======
  ///
  /// If they WONT fit then do it like this
  /// Expected                          Actual
  /// ================================= =====================================
  /// This is what I expected but this  This is what I EXPECTED but this is w
  /// ========                          ========
  ///
  PrintLn('');

  EscapeTabs;
  lExpectedStart := pos(EXPECTED_FORMAT_MESSAGE, AMessage) +
    Length(EXPECTED_FORMAT_MESSAGE);
  lActualStart := p + Length(ACTUAL_FORMAT_MESSAGE) +
    Length(EXPECTED_ACTUAL_SEPARATOR);

  lExpected := copy(AMessage, lExpectedStart, p - lExpectedStart);
  lActual := copy(AMessage, lActualStart, MAXINT);

  // Does the difference require multiple columns, or will a single line do?
  if (Length(lExpected) < (ConsoleScreenWidth - Length(EXPECTED_FORMAT_MESSAGE)
    - 1)) and (Length(lActual) < (ConsoleScreenWidth -
    Length(ACTUAL_FORMAT_MESSAGE) - 1)) then
  begin
    lSingleRow := true;
    lColumnWidth := (ConsoleScreenWidth - 5)
  end
  else
  begin
    lSingleRow := false;
    lColumnWidth := (ConsoleScreenWidth - 5) div 2;
  end;

  lStringDifference := LCSDifferences(lExpected, lActual);

  for i := Low(lStringDifference) to High(lStringDifference) do
  begin
    StartDiff := lStringDifference[i].SecondPos - lStringDifference[i].FirstPos;
    if Length(lStringDifference[i].Same) > 0 then
    begin
      if (StartDiff > 0) then
        Print(stringofchar(' ', StartDiff), clOmission);
      Print(lStringDifference[i].Same, clError)
    end
    else
      Print(lStringDifference[i].FirstBefore, clTextDifferent);
  end;
  WriteLn;
  for i := Low(lStringDifference) to High(lStringDifference) do
  begin
    StartDiff := lStringDifference[i].SecondPos - lStringDifference[i].FirstPos;
    if Length(lStringDifference[i].Same) > 0 then
    begin
      if (StartDiff < 0) then
        Print(stringofchar(' ', -1*StartDiff), clOmission);
      Print(lStringDifference[i].Same, clError)
    end
    else
    begin
      Print(lStringDifference[i].SecondBefore, clTextDifferent);
    end;
  end;
  WriteLn;

  { lDifferences := FindDifferences(lExpected, lActual);
    DifferencesFound := Length(lDifferences);
    lDifferenceMax := DifferencesFound - 1;
    lSize := 0;
    lExpectedPos := 1;
    lActualPos := 1;
    SetLength(lLeftColumn, 0);
    SetLength(lRightColumn, 0);

    // Add the Content to each column
    for i := 0 to lDifferenceMax do
    begin
    // Left Column Text
    AddTextToColumn(lLeftColumn, copy(lExpected, lExpectedPos,
    lDifferences[i].TextStart - lExpectedPos), clError, lLeftPos);
    AddTextToColumn(lLeftColumn, AddTrailingSpace(copy(lExpected,
    lDifferences[i].TextStart, lDifferences[i].TextSize),
    lDifferences[i].Size), clDifferent, lLeftPos);
    lExpectedPos := lDifferences[i].TextStart + lDifferences[i].TextSize;
    // Right Column text
    AddTextToColumn(lRightColumn, copy(lActual, lActualPos,
    lDifferences[i].CompareStart - lActualPos), clError, lRightPos);
    AddTextToColumn(lRightColumn,
    AddTrailingSpace(copy(lActual, lDifferences[i].CompareStart,
    lDifferences[i].CompareToSize), lDifferences[i].Size), clDifferent,
    lRightPos);
    lActualPos := lDifferences[i].CompareStart + lDifferences[i].CompareToSize;
    end;
    AddTextToColumn(lLeftColumn, copy(lExpected, lExpectedPos, MAXINT), clError,
    lLeftPos);
    AddTextToColumn(lRightColumn, copy(lActual, lActualPos, MAXINT), clError,
    lRightPos);

    // Now output the columns
    lLeftPos := -1;
    lRightPos := -1;
    lLeftFields := Length(lLeftColumn);
    lRightFields := Length(lRightColumn);
    // Ensure last field has EOL
    lLeftColumn[lLeftFields - 1].EOL := true;
    lRightColumn[lRightFields - 1].EOL := true;

    // Output the heading
    if lSingleRow then
    begin
    Print(EXPECTED_FORMAT_MESSAGE, clError);
    end
    else
    begin
    PrintLn(Format('   %s|%s', [AddTrailingSpace(EXPECTED_FORMAT_MESSAGE,
    lColumnWidth), AddTrailingSpace(ACTUAL_FORMAT_MESSAGE, lColumnWidth)]
    ), clError);
    end;

    while (lLeftPos < lLeftFields - 1) or (lRightPos < lRightFields - 1) do
    begin
    // Space at the beginning.
    if not lSingleRow then
    Print('  |', clError);
    OutputLeftColumn;
    OutputRightColumn;
    end;
  }
end;

function TestTypeFromSkip(ASkipped: TSkipType): TCheckTestType;
begin
  if (Not IgnoreSkip) and ((ASkipped = skipTrue) or SkippingSet) then
    Result := cttSkip
  else
    Result := cttComparison;
end;

Function DontSkip: TSkipType;
begin
  IgnoreSkip := true;
  Result := skipFalse;
end;

Function CheckIsEqual(AExpected, AResult: TComparitorType;
  AMessage: string = ''; ASkipped: TSkipType = skipFalse): boolean;
begin
  Result := false;
  try
    Result := Check(true, AExpected, AResult, AMessage,
      TestTypeFromSkip(ASkipped));
  except
    on E: Exception do
      CheckException(E);
  end;
end;

Function CheckNotEqual(AResult1, AResult2: TComparitorType; AMessage: string;
  ASkipped: TSkipType): boolean;
Begin
  Result := false;
  try
    Result := Check(false, AResult1, AResult2, AMessage,
      TestTypeFromSkip(ASkipped));
  except
    on E: Exception do
      CheckException(E);
  end;
end;

Function CheckIsTrue(AResult: boolean; AMessage: string;
  ASkipped: TSkipType): boolean;
begin
  Result := CheckIsEqual(true, AResult, AMessage, ASkipped);
end;

Function CheckIsFalse(AResult: boolean; AMessage: string;
  ASkipped: TSkipType): boolean;
begin
  Result := CheckIsEqual(false, AResult, AMessage, ASkipped);
end;

Procedure CheckException(AException: Exception);
var
  lExpected: string;
  lExceptionClassName: string;
  lExceptionMessage: string;
begin
  lExpected := ExpectedException;
  if (AException = nil) then
  begin
    lExceptionClassName := NIL_EXCEPTION_CLASSNAME;
    lExceptionMessage := NO_EXCEPTION_EXPECTED;
  end
  else
  begin
    lExceptionClassName := AException.ClassName;
    lExceptionMessage := AException.Message;
  end;

  if lExpected = '' then
    lExpected := NO_EXCEPTION_EXPECTED;
  Check((lExceptionClassName = lExpected) or (pos(lExpected, lExceptionMessage)
    > 0), lExpected, lExceptionClassName + ':' + lExceptionMessage, '',
    cttException);
end;

Function NotImplemented(AMessage: string = ''): boolean;
var
  lMessage: string;
begin
  if AMessage = '' then
    lMessage := 'Not Implemented'
  else
    lMessage := AMessage;
  Result := CheckIsTrue(true, lMessage, skipTrue);
end;

Procedure NewSet(ASetName: string);
begin
  CurrentSetName := ASetName;
  CreatingSets := true;
end;

Procedure NewCase(ATestCaseName: string);
begin
  NewTest('', ATestCaseName);
end;

procedure NewTestCase(ACase: string; ATestCaseName: string);
begin
  NewTest(ACase, ATestCaseName);
end;

procedure NewTest(ACase: string; ATestCaseName: string);
begin
  if (ATestCaseName <> '') and
    ((ACase = '') OR (CurrentTestCaseName <> ATestCaseName)) then
    NextTestCase(ATestCaseName);

  if (ACase <> '') then
    CurrentTestCaseLabel := ACase;
  ExpectedException := ExpectedSetException;
  IgnoreSkip := false;
end;

Procedure DeferTestCase(ATestName: string = '');
begin
  if Length(ATestName) = 0 then
    DeferredTestCaseLabel := CurrentTestCaseLabel
  else
    DeferredTestCaseLabel := ATestName;
end;

Procedure ResumeTestCase;
begin
  if Length(DeferredTestCaseLabel) > 0 then
    NewTest(DeferredTestCaseLabel);
  DeferredTestCaseLabel := '';
end;

Procedure DeferredTestSuccess;
begin
  ResumeTestCase;
  CheckIsTrue(true);
end;

Procedure DeferredTestFail;
begin
  ResumeTestCase;
  CheckIsTrue(false);
end;

Procedure DeferredTestException(E: Exception);
begin
  ResumeTestCase;
  CheckException(E);
end;

initialization

{$IFDEF CompilerVersion}
{$IF CompilerVersion >= 20.0}
  system.ReportMemoryLeaksOnShutdown := true;
{$IFEND}
{$ENDIF}
TotalOutputFormat := DEFAULT_TOTALS_FORMAT;
SetOutputFormat := DEFAULT_SET_FORMAT;
CaseOutputFormat := DEFAULT_CASE_FORMAT;

end.
