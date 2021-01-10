unit MiniTestFramework;

interface

{$IFDEF VER120} {$DEFINE BEFOREVARIANTS} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IFDEF VER130} {$DEFINE BEFOREVARIANTS} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IFNDEF BEFOREVARIANTS}
{$IFDEF VER140} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IFDEF VER150} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IFDEF VER160} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IFDEF VER170} {$DEFINE BEFORE_INLINE} {$ENDIF}
{$IF CompilerVersion >= 17.0}
{$DEFINE HAS_INLINE}
{$IFEND}
{$IF CompilerVersion >= 20.0}
{$DEFINE HAS_VARUSTRING}
{$IFEND}
{$IF CompilerVersion >= 23.0}
{$DEFINE HAS_VARUSTRARG}
{$IFEND}
{$ENDIF}

uses SysUtils, windows
{$IFNDEF BEFOREVARIANTS}
    , Variants
{$ENDIF}
    ;

Const
  FRAMEWORK_VERSION = '2.0.0.3';
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
  FOREGROUND_ROSE = 12;
  FOREGROUND_CANARY = 14;
  FOREGROUND_PURPLE = 5;
  FOREGROUNG_BLUE = 9;
  BACKGROUND_YELLOW = $60;
  BACKGROUND_WHITE = BACKGROUND_BLUE OR BACKGROUND_GREEN OR BACKGROUND_RED;

{$IFDEF RG_COLOUR_BLIND}
  clTitle = FOREGROUND_CYAN;
  clError = FOREGROUND_RED or FOREGROUND_INTENSITY;
  clPass = FOREGROUND_YELLOW;
  clMessage = FOREGROUND_CYAN;
  clDefault = FOREGROUND_DEFAULT;
  clSkipped = FOREGROUND_PURPLE;
  // Difference Colours
  clTextDifferent = BACKGROUND_WHITE OR FOREGROUND_RED or FOREGROUND_INTENSITY;
  clExpectedText = FOREGROUND_RED OR BACKGROUND_WHITE;
  clActualText = FOREGROUND_GREEN OR BACKGROUND_WHITE;
  clOmission = BACKGROUND_YELLOW;
{$ELSE}
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
{$ENDIF}

Type
  TComparitorType = Variant;

  TTestCaseProcedure = Procedure();

  TSkipType = (skipFalse, skipTrue, skipCase);
  TCheckTestType = (cttComparison, cttSkip, cttException);

  TDifferenceViewOptions = (dfRow, dfTwoColumn, dfEscapeCRLF);
  TDifferenceViewMode = Set of TDifferenceViewOptions;

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

  IConsoleColumn = Interface
    Function Lines: TConsoleTextArray;
    function GetColumnWidth: integer;
    Procedure Finalise(AColour: integer);
    Procedure AddText(AText: string; AColour: integer);
    Property ColumnWidth: integer read GetColumnWidth;
  End;

  TConsoleColumn = Class(TInterfacedObject, IConsoleColumn)
  private
    fEscapeCRLF: boolean;
    fPos: integer;
    fLines: TConsoleTextArray;
    fColour: integer;
    FColumnWidth: integer;
    function GetColumnWidth: integer;
    Function AddTextReturningOverflow(AText: string; AColour: integer): string;
    function GetCharsRemaining: integer;
  public
    Constructor Create(AWidth: integer; AColour: integer; AEscapeCRLF: boolean);
    Destructor Destroy; override;
    Procedure AddText(AText: string; AColour: integer);
    Procedure PadToEnd(AColour: integer);
    Procedure Finalise(AColour: integer);
    Function LineCount: integer;
    Function Lines: TConsoleTextArray;
  published
    Property ColumnWidth: integer read GetColumnWidth;
    Property CharsRemaining: integer read GetCharsRemaining;
  End;

  TScreenCoordinate = Record
    x: smallint;
    y: smallint;
  end;

  PTestCounts = ^TTestCounts;

  TTestCounts = Record
    Errored: integer;
    Failed: integer;
    Passed: integer;
    Skipped: integer;
  end;

  PTestCaseState = ^TTestCaseState;

  TTestCaseState = Record
    TestsExecuted: integer;
    ExpectedException: string;
    ExpectedSetException: string;
    DeferredExpectedException: string;
    DeferredCaseLabel: string;
    CurrentCaseName: string;
    CurrentCaseLabel: string;
    OutputFormat: string;
    SameTestCounter: integer;
    LastCaseLabel: string;
  end;

  PTestSetState = ^TTestSetState;

  TTestSetState = Record
    LastSetName: string;
    CurrentSetName: string;
    DifferencesFound: integer;
    CasesExecuted: integer;
    CaseCounts: TTestCounts;
    SetCounts: TTestCounts;
    OutputFormat: string;
    DifferenceDisplayMode: TDifferenceViewMode;
    CurrentCaseIndex: integer;
  end;

  TTestSet = Record
    SetName: string;
    Execute: TTestCaseProcedure;
    TestCaseName: string;
    Skip: TSkipType;
    ExpectedException: string;
  end;

  PTestSetArray = ^TTestSetArray;
  TTestSetArray = Array of TTestSet;

  ITestRun = Interface
    // Getters and Setters
    procedure SetTotalCases(const Value: integer);
    procedure SetTotalErroredTests(const Value: integer);
    procedure SetTotalFailedTests(const Value: integer);
    procedure SetTotalPassedTests(const Value: integer);
    procedure SetTotalSets(const Value: integer);
    procedure SetTotalSkippedTests(const Value: integer);
    procedure SetSetCounter(const Value: integer);
    procedure SetDefaultCaseOutputFormat(const Value: string);
    procedure SetDefaultSetOutputFormat(const Value: string);

    function GetTotalCases: integer;
    function GetTotalErroredTests: integer;
    function GetTotalFailedTests: integer;
    function GetTotalPassedTests: integer;
    function GetTotalSets: integer;
    function GetTotalSkippedTests: integer;
    function GetCurrentCase: integer;
    function GetTestSets: TTestSetArray;
    function GetCaseOutputFormat: String;
    function GetCurrentCaseRef: PTestCaseState;
    function GetCurrentSetName: string;
    function GetSetCounter: integer;
    function GetDefaultCaseOutputFormat: string;
    function GetDefaultSetOutputFormat: string;

    // Property Definitions.
    property CurrentCase: integer read GetCurrentCase;
    property TotalPassedTests: integer read GetTotalPassedTests
      write SetTotalPassedTests;
    property TotalFailedTests: integer read GetTotalFailedTests
      write SetTotalFailedTests;
    property TotalSkippedTests: integer read GetTotalSkippedTests
      write SetTotalSkippedTests;
    property TotalErroredTests: integer read GetTotalErroredTests
      write SetTotalErroredTests;
    property TotalCases: integer read GetTotalCases write SetTotalCases;
    property TotalSets: integer read GetTotalSets write SetTotalSets;
    property TestSets: TTestSetArray read GetTestSets;
    property CaseOutputFormat: String read GetCaseOutputFormat;
    property CurrentCaseRef: PTestCaseState read GetCurrentCaseRef;
    property CurrentSetName: string read GetCurrentSetName;
    property SetCounter: integer read GetSetCounter write SetSetCounter;
    property DefaultCaseOutputFormat: string read GetDefaultCaseOutputFormat write SetDefaultCaseOutputFormat;
    property DefaultSetOutputFormat: string read GetDefaultSetOutputFormat write SetDefaultSetOutputFormat;

    // Methods
    procedure ExpectException(AExceptionClassName: string;
      AExpectForSet: boolean = false);
    function Check(IsEqual: boolean; AExpected, AResult: TComparitorType;
      AMessage: string; ATestType: TCheckTestType): boolean;
    Function CheckIsEqual(AExpected, AResult: TComparitorType;
  AMessage: string = ''; ASkipped: TSkipType = skipFalse): boolean;
    function CheckNotEqual(AResult1, AResult2: TComparitorType;
      AMessage: string; ASkipped: TSkipType): boolean;
    function CheckIsTrue(AResult: boolean; AMessage: string = '';
      ASkipped: TSkipType = skipFalse): boolean;
    function CheckIsFalse(AResult: boolean; AMessage: string = '';
      ASkipped: TSkipType = skipFalse): boolean;
    procedure CheckException(AException: Exception);
    Function NotImplemented(AMessage: string = ''): boolean;
    procedure NewSet(ASetName: string);
    procedure NewCase(ATestCaseName: string);
    procedure NewTestCase(ACase, ATestCaseName: string);
    procedure NewTest(ACase: string; ATestCaseName: string = '');
    procedure DeferExpectedException(AExceptionClassName: string);
    Procedure DeferTestCase(ATestName: string = '');
    procedure ResumeTestCase;
    procedure DeferredTestSuccess;
    procedure DeferredTestException(E: Exception);
    procedure DeferredTestFail;
    procedure RunTestSets;
    function NextSetName: string;
    procedure NextTestSet(ASetName: string);
    procedure NextTestCase(ACaseName: string;
      ASkipped: TSkipType = TSkipType.skipFalse);
    procedure SkipTestCases(ACaseId: integer);
    function TotalTests: integer;
    procedure SetDisplayMode(ARow, AColumn: boolean;
      AEscapeCRLF: boolean = false);
    Procedure DisplayModeDefault(AEscapeCRLF: boolean = false);
    Procedure DisplayModeRows(AEscapeCRLF: boolean = false);
    Procedure DisplayModeColumns(AEscapeCRLF: boolean = false);
    Procedure DisplayModeNone(AEscapeCRLF: boolean = false);
    function CaseHasErrors: smallint;
    function CaseIsEmpty: boolean;
    procedure CaseResults;
    procedure FinaliseSet(AProcedure: TTestCaseProcedure);
    procedure FinalizeSet(AProcedure: TTestCaseProcedure);
    procedure PrepareSet(AProcedure: TTestCaseProcedure);
    function RunHasErrors: smallint;
    function SetHasErrors: smallint;
    procedure SetHeading(ASetName: string);
    function SetIsEmpty: boolean;
    procedure SetResults;
    procedure Title(AText: string);
    Procedure AddTestCase(ATestCaseName: string; AProcedure: TTestCaseProcedure;
      ASkipped: TSkipType = TSkipType.skipFalse;
      AExpectedException: string = '');
  end;

  TTestRun = class(TInterfacedObject, ITestRun)
  private
    FCounts: TTestCounts;
    FMiniTestCases: TTestSetArray;
    FSetState: TTestSetState;
    FSetCounter: integer;
    FCasesExecuted: integer;
    FSetsExecuted: integer;
    FSetOutputFormat: string;
    FCaseOutputFormat: string;
    FDefaultCaseOutputFormat: string;
    FDefaultSetOutputFormat: string;
    procedure SetTotalCases(const Value: integer);
    procedure SetTotalErroredTests(const Value: integer);
    procedure SetTotalFailedTests(const Value: integer);
    procedure SetTotalPassedTests(const Value: integer);
    procedure SetTotalSets(const Value: integer);
    procedure SetTotalSkippedTests(const Value: integer);
    procedure SetSetCounter(const Value: integer);
    procedure SetDefaultCaseOutputFormat(const Value: string);
    procedure SetDefaultSetOutputFormat(const Value: string);

    function GetTotalCases: integer;
    function GetTotalErroredTests: integer;
    function GetTotalFailedTests: integer;
    function GetTotalPassedTests: integer;
    function GetTotalSets: integer;
    function GetTotalSkippedTests: integer;
    function GetCurrentCase: integer;
    function GetTestSets: TTestSetArray;
    function GetCaseOutputFormat: String;
    function GetCurrentCaseRef: PTestCaseState;
    function GetCurrentSetName: string;
    function GetSetCounter: integer;
    function GetDefaultCaseOutputFormat: string;
    function GetDefaultSetOutputFormat: string;
  public
    procedure ExpectException(AExceptionClassName: string;
      AExpectForSet: boolean = false);
    function Check(IsEqual: boolean; AExpected, AResult: TComparitorType;
      AMessage: string; ATestType: TCheckTestType): boolean;
    Function CheckIsEqual(AExpected, AResult: TComparitorType;
  AMessage: string = ''; ASkipped: TSkipType = skipFalse): boolean;
    function CheckNotEqual(AResult1, AResult2: TComparitorType;
      AMessage: string; ASkipped: TSkipType): boolean;
    function CheckIsTrue(AResult: boolean; AMessage: string = '';
      ASkipped: TSkipType = skipFalse): boolean;
    function CheckIsFalse(AResult: boolean; AMessage: string = '';
      ASkipped: TSkipType = skipFalse): boolean;
    procedure CheckException(AException: Exception);
    Function NotImplemented(AMessage: string = ''): boolean;
    procedure NewSet(ASetName: string);
    procedure NewCase(ATestCaseName: string);
    procedure NewTestCase(ACase, ATestCaseName: string);
    procedure NewTest(ACase: string; ATestCaseName: string = '');
    procedure DeferExpectedException(AExceptionClassName: string);
    Procedure DeferTestCase(ATestName: string = '');
    procedure ResumeTestCase;
    procedure DeferredTestSuccess;
    procedure DeferredTestException(E: Exception);
    procedure DeferredTestFail;
    procedure RunTestSets;
    function NextSetName: string;
    procedure NextTestSet(ASetName: string);
    procedure NextTestCase(ACaseName: string;
      ASkipped: TSkipType = TSkipType.skipFalse);
    procedure SkipTestCases(ACaseId: integer);
    function TotalTests: integer;
    procedure SetDisplayMode(ARow, AColumn: boolean;
      AEscapeCRLF: boolean = false);
    Procedure DisplayModeDefault(AEscapeCRLF: boolean = false);
    Procedure DisplayModeRows(AEscapeCRLF: boolean = false);
    Procedure DisplayModeColumns(AEscapeCRLF: boolean = false);
    Procedure DisplayModeNone(AEscapeCRLF: boolean = false);
    function CaseHasErrors: smallint;
    function CaseIsEmpty: boolean;
    procedure CaseResults;
    procedure FinaliseSet(AProcedure: TTestCaseProcedure);
    procedure FinalizeSet(AProcedure: TTestCaseProcedure);
    procedure PrepareSet(AProcedure: TTestCaseProcedure);
    function RunHasErrors: smallint;
    function SetHasErrors: smallint;
    procedure SetHeading(ASetName: string);
    function SetIsEmpty: boolean;
    procedure SetResults;
    procedure Title(AText: string);
    Procedure AddTestCase(ATestCaseName: string; AProcedure: TTestCaseProcedure;
      ASkipped: TSkipType = TSkipType.skipFalse;
      AExpectedException: string = '');
    property CurrentCase: integer read GetCurrentCase;
    property TotalPassedTests: integer read GetTotalPassedTests
      write SetTotalPassedTests;
    property TotalFailedTests: integer read GetTotalFailedTests
      write SetTotalFailedTests;
    property TotalSkippedTests: integer read GetTotalSkippedTests
      write SetTotalSkippedTests;
    property TotalErroredTests: integer read GetTotalErroredTests
      write SetTotalErroredTests;
    property TotalCases: integer read GetTotalCases write SetTotalCases;
    property TotalSets: integer read GetTotalSets write SetTotalSets;
    property TestSets: TTestSetArray read GetTestSets;
    property CaseOutputFormat: String read GetCaseOutputFormat;
    property CurrentCaseRef: PTestCaseState read GetCurrentCaseRef;
    property CurrentSetName: string read GetCurrentSetName;
    property SetCounter: integer read GetSetCounter write SetSetCounter;
    property DefaultCaseOutputFormat: string read GetDefaultCaseOutputFormat write SetDefaultCaseOutputFormat;
    property DefaultSetOutputFormat: string read GetDefaultSetOutputFormat write SetDefaultSetOutputFormat;
  end;

const
  Skipped = skipTrue;
  Skip = skipTrue; // alternate

var
  MiniTestCases: TTestSetArray;

  SkippingSet, IgnoreSkip: boolean;
  CreatingSets: boolean = false;
  TotalOutputFormat: string;
  DefaultDifferenceDisplayMode: TDifferenceViewMode = [dfRow, dfTwoColumn];

  DefaultRun: ITestRun;

Procedure InitTestCounts(var ACounts: TTestCounts);

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
Procedure DeferExpectedException(AExceptionClassName: string);
Procedure DeferredTestSuccess;
Procedure DeferredTestFail;
Procedure DeferredTestException(E: Exception);
Procedure ResumeTestCase;
Procedure ExpectException(AExceptionClassName: string;
  AExpectForSet: boolean = false);
Procedure CheckException(AException: Exception);
Function NotImplemented(AMessage: string = ''): boolean;
Function DontSkip: TSkipType;
Function TotalTests(ARun: ITestRun = nil): integer;
Procedure TestSummary(ARun: ITestRun = nil);
procedure CaseResults;
Function ConsoleScreenWidth: integer;
Function ConsoleCursorPosition: TScreenCoordinate;
Function SetConsoleBufferLength(Rows: smallint): boolean;
Procedure DisplayModeDefault(AEscapeCRLF: boolean = false);
Procedure DisplayModeRows(AEscapeCRLF: boolean = false);
Procedure DisplayModeColumns(AEscapeCRLF: boolean = false);
Procedure DisplayModeNone(AEscapeCRLF: boolean = false);

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
function DisplayMessage(AMessage: String; AMessageColour: smallint;
  ADataType: integer; ADifferenceDisplayMode: TDifferenceViewMode): integer;

implementation

uses classes;

Const
  NIL_EXCEPTION_CLASSNAME = 'NilException';
  NO_EXCEPTION_EXPECTED = 'No Exceptions';

Type
  TIntArray = array of integer;
  TIntArrayArray = array of TIntArray;

{$IFDEF BEFOREVARIANTS}
  TvarType = integer;
{$ENDIF}
  /// ////////  SCREEN MANAGEMENT \\\\\\\\\\\\\\\\\

var
  Screen_width: integer = -1;
  Console_Handle: THandle = 0;

Procedure InitTestCounts(var ACounts: TTestCounts);
begin
  with ACounts do
  begin
    Errored := 0;
    Failed := 0;
    Passed := 0;
    Skipped := 0;
  end;
end;

Function CanDisplayCaseName(ACaseName: string): boolean;
begin
  Result := (ACaseName <> '') and (not(CreatingSets));
end;

Function TTestRun.NextSetName: string;
begin
  SetCounter := SetCounter + 1;
  Result := Format(DEFAULT_SET_NAME + ' %u', [SetCounter]);
end;

Function TTestRun.TotalTests: integer;
begin
  Result := TotalPassedTests + TotalFailedTests + TotalSkippedTests +
    TotalErroredTests + FSetState.CaseCounts.Passed +
    FSetState.CaseCounts.Failed + FSetState.CaseCounts.Skipped +
    FSetState.CaseCounts.Errored;
end;

Function ConsoleHandle: THandle;
begin
  if Console_Handle = 0 then
    Console_Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  Result := Console_Handle;
end;

function DoubleLineText: string;
begin
  Result := stringofchar('=', ConsoleScreenWidth);
end;

procedure DoubleLine;
begin
  PrintLn(DoubleLineText);
end;

function SingleLineText: string;
begin
  Result := stringofchar('-', ConsoleScreenWidth);
end;

procedure SingleLine;
begin
  PrintLn(SingleLineText);
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
    if GetConsoleScreenBufferInfo(ConsoleHandle, lScreenInfo) then
      Screen_width := lScreenInfo.dwSize.x - 2
    else
      Screen_width := DEFAULT_SCREEN_WIDTH;
  end;
  Result := Screen_width;
end;

Function SetConsoleBufferLength(Rows: smallint): boolean;
var
  lSize: COORD;
begin
  lSize.x := ConsoleScreenWidth + 2;
  lSize.y := Rows;
  Result := SetConsoleScreenBufferSize(GetStdHandle(STD_OUTPUT_HANDLE), lSize);
end;

Function ConsoleCursorPosition: TScreenCoordinate;
var
  lScreenInfo: TConsoleScreenBufferInfo;
begin
  Result.x := 0;
  Result.y := 0;
  if GetConsoleScreenBufferInfo(ConsoleHandle, lScreenInfo) then
  begin
    Result.x := lScreenInfo.dwCursorPosition.x;
    Result.y := lScreenInfo.dwCursorPosition.y;
    Screen_width := lScreenInfo.dwSize.x - 2;
  end;
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

Procedure CentredLineDimensions(AText: string; var ATitleSpace: integer;
  var APreSpace: integer; var APostSpace: integer);
begin
  ATitleSpace := ConsoleScreenWidth - 4;
  APreSpace := trunc((ATitleSpace - Length(AText)) / 2);
  APostSpace := ATitleSpace - APreSpace - Length(AText);
end;

Function LnCentredText(AText: string; AChar: char;
  AColour: smallint = FOREGROUND_DEFAULT): string;
var
  PreSpace, PostSpace, TitleSpace: integer;
begin
  CentredLineDimensions(AText, TitleSpace, PreSpace, PostSpace);

  Result := stringofchar(AChar, PreSpace) + '  ' + AText + '  ' +
    stringofchar(AChar, PostSpace) + #13#10;
end;

Procedure PrintLnCentred(AText: string; AChar: char;
  AColour: smallint = FOREGROUND_DEFAULT);
var
  PreSpace, PostSpace, TitleSpace: integer;
begin
  CentredLineDimensions(AText, TitleSpace, PreSpace, PostSpace);
  Print(stringofchar(AChar, PreSpace));
  Print('  ' + AText + '  ', AColour);
  PrintLn(stringofchar(AChar, PostSpace));
end;

procedure TTestRun.SetDefaultCaseOutputFormat(const Value: string);
begin
  FDefaultCaseOutputFormat := Value;
end;

procedure TTestRun.SetDefaultSetOutputFormat(const Value: string);
begin
  FDefaultSetOutputFormat := Value;
end;

Procedure TTestRun.SetDisplayMode(ARow, AColumn: boolean;
  AEscapeCRLF: boolean = false);
var
  lMode: TDifferenceViewMode;
begin
  lMode := [];
  if ARow then
    lMode := lMode + [dfRow];
  if AColumn then
    lMode := lMode + [dfTwoColumn];
  if AEscapeCRLF then
    lMode := lMode + [dfEscapeCRLF];
  self.FSetState.DifferenceDisplayMode := lMode;
end;

Procedure TTestRun.DisplayModeDefault(AEscapeCRLF: boolean = false);
begin
  SetDisplayMode(true, true, AEscapeCRLF);
end;

Procedure TTestRun.DisplayModeRows(AEscapeCRLF: boolean = false);
begin
  SetDisplayMode(true, false, AEscapeCRLF);
end;

Procedure TTestRun.DisplayModeColumns(AEscapeCRLF: boolean = false);
begin
  SetDisplayMode(false, true, AEscapeCRLF);
end;

Procedure TTestRun.DisplayModeNone(AEscapeCRLF: boolean = false);
begin
  SetDisplayMode(false, false, AEscapeCRLF);
end;

/// TEST FUNCTIONS

Procedure AddTestSet(ATestCaseName: string; AProcedure: TTestCaseProcedure;
  ASkipped: TSkipType; AExpectedException: string);
begin
  AddTestCase(ATestCaseName, AProcedure, ASkipped, AExpectedException);
end;

Procedure AddTestCase(ATestCaseName: string; AProcedure: TTestCaseProcedure;
  ASkipped: TSkipType; AExpectedException: string);
begin
  DefaultRun.AddTestCase(ATestCaseName, AProcedure, ASkipped,
    AExpectedException);
end;

Procedure PrepareSet(AProcedure: TTestCaseProcedure);
begin
  AddTestCase('', AProcedure);
end;

Procedure TTestRun.FinaliseSet(AProcedure: TTestCaseProcedure);
begin
  AddTestCase('', AProcedure);
  FSetState.CurrentSetName := '';
  CreatingSets := false;
end;

Procedure TTestRun.FinalizeSet(AProcedure: TTestCaseProcedure);
begin
  // Americaniz(s)ed form
  FinaliseSet(AProcedure);
end;

Procedure TTestRun.Title(AText: string);
var
  lText: string;
begin
  lText := 'DUnitm V-' + FRAMEWORK_VERSION;
  PrintLnCentred(lText, '=', clDefault);
  PrintLnCentred(AText, '=', clTitle);
  DoubleLine;
end;

function TTestRun.CaseHasErrors: smallint;
begin
  Result := 0;
  if FSetState.CaseCounts.Failed + FSetState.CaseCounts.Errored > 0 then
    Result := 2
  else if FSetState.CaseCounts.Skipped > 0 then
    Result := 1;
end;

function TTestRun.SetHasErrors: smallint;
begin
  Result := 0;
  if FSetState.SetCounts.Failed + FSetState.SetCounts.Errored > 0 then
    Result := 2
  else if FSetState.SetCounts.Skipped > 0 then
    Result := 1;
end;

function TTestRun.RunHasErrors: smallint;
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

Function TTestRun.CaseIsEmpty: boolean;
begin
  Result := (FSetState.CaseCounts.Passed = 0) and
    (FSetState.CaseCounts.Failed = 0) and (FSetState.CaseCounts.Skipped = 0) and
    (FSetState.CaseCounts.Errored = 0);
end;

procedure TTestRun.CaseResults;
begin

  if CaseIsEmpty then
    exit;

  PrintLn(Format(CaseOutputFormat, [FSetState.CaseCounts.Passed,
    FSetState.CaseCounts.Failed, FSetState.CaseCounts.Skipped,
    FSetState.CaseCounts.Errored]), ResultColour(CaseHasErrors));

end;

Function TTestRun.SetIsEmpty: boolean;
begin
  Result := (FSetState.SetCounts.Passed = 0) and
    (FSetState.SetCounts.Failed = 0) and (FSetState.SetCounts.Skipped = 0) and
    (FSetState.SetCounts.Errored = 0);
end;

procedure TTestRun.SetResults;
begin

  if (SetIsEmpty) or (Length(CurrentSetName) = 0) then
    exit;

  PrintLn(Format(FSetState.OutputFormat, [FSetState.CasesExecuted,
    FSetState.SetCounts.Passed + FSetState.SetCounts.Failed +
    FSetState.SetCounts.Skipped + FSetState.SetCounts.Errored,
    FSetState.SetCounts.Passed, FSetState.SetCounts.Failed,
    FSetState.SetCounts.Skipped, FSetState.SetCounts.Errored]),
    ResultColour(SetHasErrors));

  SingleLine;
end;

procedure TTestRun.SetSetCounter(const Value: integer);
begin
  FSetCounter := Value;
end;

Procedure TestSummary(ARun: ITestRun = nil);
begin
  if ARun = nil then
    ARun := DefaultRun;
  ARun.NextTestCase('');
  DoubleLine;
  PrintLn(Format(TotalOutputFormat, [ARun.TotalSets, ARun.TotalCases,
    ARun.TotalTests, ARun.TotalPassedTests, ARun.TotalFailedTests,
    ARun.TotalSkippedTests, ARun.TotalErroredTests]),
    ResultColour(ARun.RunHasErrors or FOREGROUND_INTENSITY));
  WriteLn('');
end;

Procedure TTestRun.SetHeading(ASetName: string);
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

Procedure TTestRun.SkipTestCases(ACaseId: integer);
begin
  NewTest(FMiniTestCases[ACaseId].TestCaseName);
  CheckIsTrue(false, 'Case Skipped', Skip);
end;

Procedure TTestRun.RunTestSets;
var
  i, l: integer;
begin
  CreatingSets := false;
  l := Length(FMiniTestCases);
  if l > 0 then
  begin
    FSetState.LastSetName := FMiniTestCases[0].SetName;
    NextTestSet(FSetState.LastSetName);
  end;

  for i := 0 to l - 1 do
    Try
      FSetState.CurrentCaseIndex := i;
      if not assigned(FMiniTestCases[i].Execute) then
        continue;
      if FMiniTestCases[i].Skip = skipCase then
      begin
        SkipTestCases(i);
        continue;
      end;

      DisplayModeDefault;
      FSetState.CurrentSetName := FMiniTestCases[i].SetName;
      if FMiniTestCases[i].TestCaseName <> '' then
        NextTestCase(FMiniTestCases[i].TestCaseName, FMiniTestCases[i].Skip);
      ExpectException(FMiniTestCases[i].ExpectedException, true);
      FMiniTestCases[i].Execute;
      FSetState.LastSetName := FSetState.CurrentSetName;
    except
      on E: Exception do
        CheckException(E);
    end;
  // Last Case is done, Need to Update the Final Set Results.
  FSetState.CurrentSetName := FINAL_SET_NAME;
  NextTestCase('');

  // Destroy the Cases.
  SetLength(FMiniTestCases, 0);
  FSetState.LastSetName := '';
  FSetState.CurrentSetName := '';
end;

Procedure TTestRun.NextTestSet(ASetName: string);
begin

  SetResults;

  if ASetName = FINAL_SET_NAME then
    ASetName := ''; // Finalising.

  if Length(ASetName) > 0 then
  begin
    SetHeading(ASetName);
    TotalSets := TotalSets + 1;
  end;

  InitTestCounts(self.FSetState.SetCounts);

end;

Procedure TTestRun.NextTestCase(ACaseName: string; ASkipped: TSkipType);
var
  lHeading: string;
  lStateOfCurrentCase: PTestCaseState;
begin
  lStateOfCurrentCase := lStateOfCurrentCase;
  if lStateOfCurrentCase <> Nil then
    lStateOfCurrentCase.ExpectedSetException := '';

  CaseResults;
  FSetState.SetCounts.Passed := FSetState.SetCounts.Passed +
    FSetState.CaseCounts.Passed;
  FSetState.SetCounts.Failed := FSetState.SetCounts.Failed +
    FSetState.CaseCounts.Failed;
  FSetState.SetCounts.Skipped := FSetState.SetCounts.Skipped +
    FSetState.CaseCounts.Skipped;
  FSetState.SetCounts.Errored := FSetState.SetCounts.Errored +
    FSetState.CaseCounts.Errored;

  if FSetState.LastSetName <> FSetState.CurrentSetName then
    NextTestSet(FSetState.CurrentSetName);

  if CanDisplayCaseName(ACaseName) then
  begin
    lHeading := ' Test Case:' + ACaseName;
    PrintLn(lHeading, clTitle);
    inc(self.FSetState.CasesExecuted);
    TotalCases := TotalCases + 1;
  end;

  TotalPassedTests := TotalPassedTests + FSetState.CaseCounts.Passed;
  TotalFailedTests := TotalFailedTests + FSetState.CaseCounts.Failed;
  TotalSkippedTests := TotalSkippedTests + FSetState.CaseCounts.Skipped;
  TotalErroredTests := TotalErroredTests + FSetState.CaseCounts.Errored;
  SkippingSet := ASkipped <> skipFalse;
  IgnoreSkip := false;

  InitTestCounts(FSetState.CaseCounts);
  lStateOfCurrentCase.SameTestCounter := 0;
  lStateOfCurrentCase.LastCaseLabel := '';
  lStateOfCurrentCase.CurrentCaseLabel := '';
  lStateOfCurrentCase.CurrentCaseName := ACaseName;
  lStateOfCurrentCase.ExpectedException := '';
end;

Procedure TTestRun.ExpectException(AExceptionClassName: string;
  AExpectForSet: boolean = false);
begin
  if CurrentCaseRef = nil then
    exit;
  CurrentCaseRef.ExpectedException := AExceptionClassName;
  if AExpectForSet then
    CurrentCaseRef.ExpectedSetException := CurrentCaseRef.ExpectedException;
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

{$IFDEF HAS_VARUSTRING}
    varUString,
{$IFDEF HAS_VARSTRARG}varUStrArg, {$ENDIF}
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

Function TTestRun.Check(IsEqual: boolean; AExpected, AResult: TComparitorType;
  AMessage: string; ATestType: TCheckTestType): boolean;
var
  lMessage, lCounter: string;
  lResult: integer;
  Outcome: boolean;
  lMessageColour: smallint;
  lDifferencesFound: integer;
begin
  Result := false;
  lMessageColour := clDefault;
  lResult := 0;
  FSetState.DifferencesFound := 0;
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
          inc(FSetState.CaseCounts.Skipped);
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
            inc(FSetState.CaseCounts.Passed);
          end
          else
          begin
            lResult := 3;
            inc(FSetState.CaseCounts.Errored);
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
            inc(FSetState.CaseCounts.Failed);
            if AMessage = '' then
            begin
              if IsEqual then
                lMessage := Format(EXPECTED_ACTUAL_FORMAT,
                  [#13#10, ValueAsString(AExpected), ValueAsString(AResult)])
              else
                lMessage :=
                  Format('%s   Expected outcomes to differ, but both returned <%s>',
                  [#13#10, ValueAsString(AExpected)]);
            end
            else
              lMessage := #13#10'   ' + AMessage;
            exit;
          end;
          inc(FSetState.CaseCounts.Passed);
        except
          on E: Exception do
          begin
            if (E.ClassName = CurrentCaseRef.ExpectedException) then
            begin
              // At this level, it will only be exceptions
              // for Variant type comparisons
              lResult := 0;
              inc(FSetState.CaseCounts.Passed);
            end
            else
            begin
              lResult := 2;
              lMessageColour := clMessage;
              lMessage := #13#10'   Illegal Comparison Test Framework: ' +
                E.Message;
              inc(FSetState.CaseCounts.Errored);
            end;
          end;
        end;
      end; // case else
    end; // case
  finally
    if CurrentCaseRef.LastCaseLabel = CurrentCaseRef.CurrentCaseLabel then
      inc(CurrentCaseRef.SameTestCounter)
    else
      CurrentCaseRef.SameTestCounter := 1;
    CurrentCaseRef.LastCaseLabel := CurrentCaseRef.CurrentCaseLabel;
    if CurrentCaseRef.SameTestCounter = 1 then
      lCounter := ''
    else
      lCounter := '-' + IntToStr(CurrentCaseRef.SameTestCounter);
    if CurrentCaseRef.CurrentCaseLabel = '' then
    begin
      CurrentCaseRef.CurrentCaseLabel :=
        copy('Test for ' + CurrentCaseRef.CurrentCaseName, 1,
        ConsoleScreenWidth - 4);
      if lCounter = '' then
      begin
        lCounter := '-1';
        CurrentCaseRef.LastCaseLabel := CurrentCaseRef.CurrentCaseLabel;
      end;
      CurrentCaseRef.ExpectedException := CurrentCaseRef.ExpectedSetException;
      IgnoreSkip := false;
    end;

    Print(Format('  %s-', [PASS_FAIL[lResult]]), lMessageColour);
    Print(Format('%s%s', [CurrentCaseRef.CurrentCaseLabel, lCounter]));
    lDifferencesFound := DisplayMessage(lMessage, lMessageColour,
      varType(AResult), FSetState.DifferenceDisplayMode);
    if lDifferencesFound > -1 then
      FSetState.DifferencesFound := lDifferencesFound;
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

Function lcs(x, y: string): TLCSParams;
var
  lSize, pF, pS, lx, ly, i, j, k: integer;
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
  lx := Length(x);
  ly := Length(y);
  lSize := 0;
  for i := 1 to lx do
  begin
    for j := 1 to ly do
    begin
      if x[i] = y[j] then
      begin
        pF := i;
        pS := j;
        k := 1;
        lSize := 1;
        while (((i + k) <= lx) and ((j + k) <= ly)) do
        begin
          if x[i + k] = y[j + k] then
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
  lBeforeDifferences := nil;
  lAfterDifferences := nil;

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

Function FinalLengthofDifferences(ADifferences: TStringDifferences): integer;
var
  lStartDiff, i: integer;
begin
  Result := 0;
  for i := Low(ADifferences) to High(ADifferences) do
  begin
    lStartDiff := ADifferences[i].SecondPos - ADifferences[i].FirstPos;
    if Length(ADifferences[i].Same) > 0 then
    begin
      if (lStartDiff > 0) then
        Result := Result + lStartDiff;
      Result := Result + Length(ADifferences[i].Same);
    end
    else
      Result := Result + Length(ADifferences[i].FirstBefore);
  end;

end;

Function AlignDifferencesByRow(ALeftColumn, ARightColumn: TConsoleColumn)
  : TConsoleTextArray;
var
  lSize, i, p: integer;
  Procedure ExtraText(AMessage: string; AIndex: integer);
  begin
    Result[AIndex].Text := AMessage;
    Result[AIndex].Colour := clError;
    Result[AIndex].EOL := false;
  end;

begin
  lSize := ALeftColumn.LineCount;
  if ARightColumn.LineCount > lSize then
    lSize := ARightColumn.LineCount;

  SetLength(Result, (4 * lSize));
  p := 0;
  ExtraText(EXPECTED_FORMAT_MESSAGE, p);
  inc(p);
  lSize := ALeftColumn.LineCount - 1;
  for i := 0 to lSize do
  begin
    Result[p] := ALeftColumn.Lines[i];
    if (i <> lSize) and (Result[p].EOL) then
    begin
      inc(p);
      ExtraText(EXPECTED_FORMAT_MESSAGE, p);
    end;
    inc(p);
  end;

  ExtraText(ACTUAL_FORMAT_MESSAGE, p);
  inc(p);
  lSize := ARightColumn.LineCount - 1;
  for i := 0 to lSize do
  begin
    Result[p] := ARightColumn.Lines[i];
    if (i <> lSize) and (Result[p].EOL) then
    begin
      inc(p);
      ExtraText(ACTUAL_FORMAT_MESSAGE, p);
    end;
    inc(p);
  end;
  SetLength(Result, p);
end;

Function AlignDifferencesByColumn(ALeftColumn, ARightColumn: TConsoleColumn)
  : TConsoleTextArray;
var
  lSize, i, lLeftLine, lRightLine: integer;

  Procedure AddEmptyLine;
  begin
    Result[i].Text := stringofchar(' ', ALeftColumn.ColumnWidth);
    Result[i].Colour := clOmission;
    Result[i].EOL := true;
  end;

begin
  lSize := ALeftColumn.LineCount;
  if ARightColumn.LineCount > lSize then
    lSize := ARightColumn.LineCount;
  SetLength(Result, 4 * lSize);
  lLeftLine := 0;
  lRightLine := 0;
  i := 0;
  while (lLeftLine < ALeftColumn.LineCount) or
    (lRightLine < ARightColumn.LineCount) do
  begin
    // Leading space
    Result[i].Text := '  ';
    Result[i].Colour := clError;
    Result[i].EOL := false;

    // Left Column
    inc(i);
    if lLeftLine >= ALeftColumn.LineCount then
    begin
      AddEmptyLine;
      inc(i);
    end
    else
    begin
      repeat
        Result[i] := ALeftColumn.Lines[lLeftLine];
        Result[i].EOL := false;
        inc(i);
        inc(lLeftLine);
      until (lLeftLine >= ALeftColumn.LineCount) or
        (ALeftColumn.Lines[lLeftLine - 1].EOL);
    end;

    // Inter column space
    Result[i].Text := ' ';
    Result[i].Colour := clDefault;
    Result[i].EOL := false;

    // Right Column
    inc(i);
    if lRightLine >= ARightColumn.LineCount then
    begin
      AddEmptyLine;
      inc(i);
    end
    else
    begin
      repeat
        Result[i] := ARightColumn.Lines[lRightLine];
        inc(i);
        inc(lRightLine);
      until (lRightLine >= ARightColumn.LineCount) or
        (ARightColumn.Lines[lRightLine - 1].EOL);
    end;
    // check for enough working space: probably not needed.
    if Length(Result) - i < lSize then
      SetLength(Result, Length(Result) + lSize);
  end;
  SetLength(Result, i + 1);
end;

Function DifferencesToConsoleArray(ADifferences: TStringDifferences;
  ADifferenceDisplayMode: TDifferenceViewMode): TConsoleTextArray;
var
  i, lColumnWidth, lStartDiff: integer;
  lLeftColumn, lRightColumn: TConsoleColumn;
  lTwoColumn: boolean;
  lLeftPadColour, lRightPadColour: integer;
begin
  lLeftColumn := nil;
  lRightColumn := nil;
  try
    lTwoColumn := false;
    lColumnWidth := ConsoleScreenWidth - 12;
    lLeftPadColour := clError;
    lRightPadColour := clError;
    if dfTwoColumn in ADifferenceDisplayMode then
    begin
      if (not(dfRow in ADifferenceDisplayMode)) or
        (FinalLengthofDifferences(ADifferences) > (ConsoleScreenWidth - 3)) then
      begin
        lColumnWidth := (ConsoleScreenWidth - 3) Div 2;
        lTwoColumn := true;
      end;
    end;
    lLeftColumn := TConsoleColumn.Create(lColumnWidth, clExpectedText,
      dfEscapeCRLF in ADifferenceDisplayMode);
    lRightColumn := TConsoleColumn.Create(lColumnWidth, clActualText,
      dfEscapeCRLF in ADifferenceDisplayMode);
    for i := Low(ADifferences) to High(ADifferences) do
    begin

      lStartDiff := ADifferences[i].SecondPos - ADifferences[i].FirstPos;
      if Length(ADifferences[i].Same) > 0 then
      begin
        if (lStartDiff > 0) then
          lLeftColumn.AddText(stringofchar(' ', lStartDiff), clOmission);
        lLeftColumn.AddText(ADifferences[i].Same, clExpectedText);
      end
      else
        lLeftColumn.AddText(ADifferences[i].FirstBefore, clTextDifferent);

      // Right Column (Actual)
      lStartDiff := ADifferences[i].SecondPos - ADifferences[i].FirstPos;
      if Length(ADifferences[i].Same) > 0 then
      begin
        if (lStartDiff < 0) then
        begin
          lRightColumn.AddText(stringofchar(' ', -1 * lStartDiff), clOmission);
          if (i < High(ADifferences)) then
            lLeftColumn.AddText(stringofchar(' ', -1 * lStartDiff), clOmission);
        end;
        lRightColumn.AddText(ADifferences[i].Same, clActualText);
      end
      else
        lRightColumn.AddText(ADifferences[i].SecondBefore, clTextDifferent);
    end;

    if lTwoColumn then
    begin
      lLeftPadColour := clExpectedText;
      lRightPadColour := clActualText;
    end;

    lLeftColumn.Finalise(lLeftPadColour);
    lRightColumn.Finalise(lRightPadColour);
    if lTwoColumn then
      Result := AlignDifferencesByColumn(lLeftColumn, lRightColumn)
    else
      Result := AlignDifferencesByRow(lLeftColumn, lRightColumn);
  finally
    freeandnil(lLeftColumn);
    freeandnil(lRightColumn);
  end;
end;

function DisplayMessage(AMessage: String; AMessageColour: smallint;
  ADataType: integer; ADifferenceDisplayMode: TDifferenceViewMode): integer;
var
  lExpected, lActual: string;
  p, i, lExpectedStart, lActualStart: integer;
  lStringDifference: TStringDifferences;
  lScreenText: TConsoleTextArray;

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
  if (AMessageColour <> clError) OR (ADifferenceDisplayMode = []) OR
    (ADifferenceDisplayMode = [dfEscapeCRLF]) OR
    ((p < 1) OR (pos(EXPECTED_FORMAT_MESSAGE, AMessage) < 1) OR
    (pos(ACTUAL_FORMAT_MESSAGE, AMessage) < 1)) then
  begin
    Result := 1;
    PrintLn(StringReplace(AMessage, #1, '', [rfReplaceAll]), AMessageColour);
    exit;
  end;

  // Only do the test on string data types
  if NOT((ADataType = varString) OR
{$IFNDEF BEFORE_INLINE}
{$IFDEF HAS_VARUSTRING}
    (ADataType = varUString) OR
{$IFDEF HAS_VARUSTRARG } (ADataType = varUStrArg) OR {$ENDIF}
{$ENDIF}
{$ENDIF}
    (ADataType in [varStrArg, varOleStr])) then
  begin
    PrintLn(StringReplace(AMessage, #1, '', []), AMessageColour);
    exit;
  end;

  /// Ok, we have errored - look for the reason.
  /// dfROW
  /// When dfColumn Always Split into 2
  /// When dfColumn and dfRow then split into 2 when too wide to fit.
  ///
  /// Expected :<This is expected>
  /// Actual   :<this is Actual>
  /// ======
  ///
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

  // Compare Differences
  lExpected := copy(AMessage, lExpectedStart, p - lExpectedStart);
  lActual := copy(AMessage, lActualStart, MAXINT);
  lStringDifference := LCSDifferences(lExpected, lActual);
  Result := Length(lStringDifference);

  // Convert to Console Output
  lScreenText := DifferencesToConsoleArray(lStringDifference,
    ADifferenceDisplayMode);
  for i := low(lScreenText) to High(lScreenText) do
  begin
    Print(lScreenText[i].Text, lScreenText[i].Colour);
    if lScreenText[i].EOL then
      WriteLn;
  end;

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

Function TTestRun.CheckIsEqual(AExpected, AResult: TComparitorType;
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

Function TTestRun.CheckNotEqual(AResult1, AResult2: TComparitorType;
  AMessage: string; ASkipped: TSkipType): boolean;
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

Function TTestRun.CheckIsTrue(AResult: boolean; AMessage: string;
  ASkipped: TSkipType): boolean;
begin
  Result := CheckIsEqual(true, AResult, AMessage, ASkipped);
end;

Function TTestRun.CheckIsFalse(AResult: boolean; AMessage: string;
  ASkipped: TSkipType): boolean;
begin
  Result := CheckIsEqual(false, AResult, AMessage, ASkipped);
end;

Procedure TTestRun.CheckException(AException: Exception);
var
  lExpected: string;
  lExceptionClassName: string;
  lExceptionMessage: string;
begin
  lExpected := CurrentCaseRef.ExpectedException;
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

Function TTestRun.NotImplemented(AMessage: string = ''): boolean;
var
  lMessage: string;
begin
  if AMessage = '' then
    lMessage := 'Not Implemented'
  else
    lMessage := AMessage;
  Result := CheckIsTrue(true, lMessage, skipTrue);
end;

Procedure TTestRun.NewSet(ASetName: string);
begin
  FSetState.CurrentSetName := ASetName;
  CreatingSets := true;
end;

Procedure TTestRun.NewCase(ATestCaseName: string);
begin
  NewTest('', ATestCaseName);
end;

procedure TTestRun.NewTestCase(ACase: string; ATestCaseName: string);
begin
  NewTest(ACase, ATestCaseName);
end;

procedure TTestRun.NewTest(ACase: string; ATestCaseName: string);
begin
  if (ATestCaseName <> '') and
    ((ACase = '') OR (CurrentCaseRef.CurrentCaseName <> ATestCaseName)) then
    NextTestCase(ATestCaseName);

  if (ACase <> '') then
    CurrentCaseRef.CurrentCaseLabel := ACase;
  CurrentCaseRef.ExpectedException := CurrentCaseRef.ExpectedSetException;
  IgnoreSkip := false;
end;

Procedure TTestRun.DeferTestCase(ATestName: string = '');
begin
  if Length(ATestName) = 0 then
    CurrentCaseRef.DeferredCaseLabel := CurrentCaseRef.CurrentCaseLabel
  else
    CurrentCaseRef.DeferredCaseLabel := ATestName;
end;

Procedure TTestRun.DeferExpectedException(AExceptionClassName: string);
begin
  CurrentCaseRef.DeferredExpectedException := AExceptionClassName;
end;

Procedure TTestRun.ResumeTestCase;
begin
  if Length(CurrentCaseRef.DeferredCaseLabel) > 0 then
    NewTest(CurrentCaseRef.DeferredCaseLabel);
  if Length(CurrentCaseRef.DeferredExpectedException) > 0 then
    ExpectException(CurrentCaseRef.DeferredExpectedException);
  CurrentCaseRef.DeferredExpectedException := '';
  CurrentCaseRef.DeferredCaseLabel := '';
end;

Procedure TTestRun.DeferredTestSuccess;
begin
  ResumeTestCase;
  CheckIsTrue(true);
end;

Procedure TTestRun.DeferredTestFail;
begin
  ResumeTestCase;
  CheckIsTrue(false);
end;

Procedure TTestRun.DeferredTestException(E: Exception);
begin
  ResumeTestCase;
  CheckException(E);
end;

{ TConsoleColumn }

procedure TConsoleColumn.AddText(AText: string; AColour: integer);
var
  lOverflow: string;
  lList: TStringlist;
  i: integer;
begin
  lList := TStringlist.Create;
  try
    if self.fEscapeCRLF then
      lList.Text := StringReplace(StringReplace(AText, #10, #17, [rfReplaceAll]
        ), #13, #20, [rfReplaceAll])
    Else
      lList.Text := AText;

    for i := 0 to lList.Count - 1 do
    begin
      lOverflow := lList[i];
      repeat
        lOverflow := AddTextReturningOverflow(lOverflow, AColour);
      until Length(lOverflow) = 0;
      if (not(self.fEscapeCRLF)) and (i < lList.Count - 1) then
      begin
        PadToEnd(AColour);
        self.fLines[self.LineCount - 1].EOL := true;
      end;
    end;

  finally
    freeandnil(lList);
  end;
end;

function TConsoleColumn.AddTextReturningOverflow(AText: string;
  AColour: integer): string;
var
  lPos, lSize: integer;
  lText: string;
begin
  self.fColour := AColour;
  if Length(AText) = 0 then
    exit;

  lText := copy(AText, 1, self.CharsRemaining);
  Result := copy(AText, self.CharsRemaining + 1, MAXINT);
  lSize := Length(lText);
  self.fPos := self.fPos + lSize;

  lPos := Length(self.fLines);
  SetLength(self.fLines, lPos + 1);
  self.fLines[lPos].Text := lText;
  self.fLines[lPos].Colour := AColour;
  Assert(self.fPos <= self.FColumnWidth,
    'Test Framework ERROR. Column larger than expected.');
  self.fLines[lPos].EOL := (self.fPos >= self.FColumnWidth);

  if self.fLines[lPos].EOL then
    self.fPos := 0;

end;

constructor TConsoleColumn.Create(AWidth: integer; AColour: integer;
  AEscapeCRLF: boolean);
begin
  self.FColumnWidth := AWidth;
  self.fColour := -1;
  SetLength(fLines, 0);
  self.fPos := 0;
  self.fEscapeCRLF := AEscapeCRLF;
end;

destructor TConsoleColumn.Destroy;
begin
  SetLength(self.fLines, 0);
  inherited;
end;

procedure TConsoleColumn.Finalise(AColour: integer);
begin
  if self.fPos = 1 then
    exit;
  PadToEnd(AColour);
end;

function TConsoleColumn.GetCharsRemaining: integer;
begin
  Result := self.FColumnWidth - self.fPos;
end;

function TConsoleColumn.GetColumnWidth: integer;
begin
  Result := self.FColumnWidth;
end;

function TConsoleColumn.LineCount: integer;
begin
  Result := Length(self.fLines);
end;

function TConsoleColumn.Lines: TConsoleTextArray;
begin
  Result := self.fLines;
end;

procedure TConsoleColumn.PadToEnd(AColour: integer);
begin
  self.AddTextReturningOverflow(stringofchar(' ', self.CharsRemaining),
    AColour);
end;

{ TTestRun }

procedure TTestRun.AddTestCase(ATestCaseName: string;
  AProcedure: TTestCaseProcedure; ASkipped: TSkipType;
  AExpectedException: string);
var
  l: integer;
begin
  if Length(CurrentSetName) = 0 then
    FSetState.CurrentSetName := NextSetName;
  l := Length(FMiniTestCases);
  SetLength(FMiniTestCases, l + 1);
  FMiniTestCases[l].SetName := CurrentSetName;
  FMiniTestCases[l].Execute := AProcedure;
  FMiniTestCases[l].TestCaseName := ATestCaseName;
  FMiniTestCases[l].Skip := ASkipped;
  FMiniTestCases[l].ExpectedException := AExpectedException;
end;

function TTestRun.GetCaseOutputFormat: String;
begin
  Result := '';
  if CurrentCaseRef = nil then
    exit;
  Result := CurrentCaseRef.OutputFormat;
end;

function TTestRun.GetCurrentCase: integer;
begin
  Result := FSetState.CurrentCaseIndex;
end;

function TTestRun.GetCurrentCaseRef: PTestCaseState;
var
  lIndex: integer;
begin
  Result := nil;
  lIndex := self.CurrentCase;
  if lIndex < 0 then
    exit;
  Result := PTestCaseState(@FMiniTestCases[lIndex]);
end;

function TTestRun.GetCurrentSetName: string;
begin
  Result := self.FSetState.CurrentSetName;
end;

function TTestRun.GetDefaultCaseOutputFormat: string;
begin
  result := FDefaultCaseOutputFormat;
end;

function TTestRun.GetDefaultSetOutputFormat: string;
begin
  result := FDefaultSetOutputFormat;
end;

function TTestRun.GetSetCounter: integer;
begin
  result := self.FSetCounter;
end;

function TTestRun.GetTestSets: TTestSetArray;
begin
  Result := FMiniTestCases;
end;

function TTestRun.GetTotalCases: integer;
begin
  Result := self.FCounts.Errored + self.FCounts.Failed + self.FCounts.Passed +
    self.FCounts.Skipped;
end;

function TTestRun.GetTotalErroredTests: integer;
begin
  Result := FCounts.Errored;
end;

function TTestRun.GetTotalFailedTests: integer;
begin
  Result := FCounts.Failed;
end;

function TTestRun.GetTotalPassedTests: integer;
begin
  Result := FCounts.Passed;
end;

function TTestRun.GetTotalSets: integer;
begin
  Result := length(FMiniTestCases);
end;

function TTestRun.GetTotalSkippedTests: integer;
begin
  Result := FCounts.Skipped;
end;

procedure TTestRun.SetTotalCases(const Value: integer);
begin
  FCasesExecuted := Value;
end;

procedure TTestRun.SetTotalErroredTests(const Value: integer);
begin
  FCounts.Errored := Value;
end;

procedure TTestRun.SetTotalFailedTests(const Value: integer);
begin
  FCounts.Failed := Value;
end;

procedure TTestRun.SetTotalPassedTests(const Value: integer);
begin
  FCounts.Passed := Value;
end;

procedure TTestRun.SetTotalSets(const Value: integer);
begin
  FSetsExecuted := Value;
end;

procedure TTestRun.SetTotalSkippedTests(const Value: integer);
begin
  FCounts.Skipped := Value;
end;

Procedure TTestRun.PrepareSet(AProcedure: TTestCaseProcedure);
begin
  AddTestCase('', AProcedure);
end;

initialization

DefaultRun := TTestRun.Create;

{$IFDEF CompilerVersion}
{$IF CompilerVersion >= 20.0}
system.ReportMemoryLeaksOnShutdown := true;
{$IFEND}
{$ENDIF}
TotalOutputFormat := DEFAULT_TOTALS_FORMAT;
DefaultRun.DefaultSetOutputFormat := DEFAULT_SET_FORMAT;
DefaultRun.DefaultCaseOutputFormat := DEFAULT_CASE_FORMAT;

end.
