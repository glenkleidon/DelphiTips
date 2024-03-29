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
  FRAMEWORK_VERSION = '2.0.0.5';
  DEFAULT_TOTALS_FORMAT =
    'Run>Sets:%-3d Cases:%-3d Tests:%-4d Passed:%-4d Failed:%-3d Skipped:%-3d Errors:%-3d';
  DEFAULT_SET_FORMAT =
    'Set>Cases:%-4d Tests:%-4d Passed:%-4d Failed:%-4d Skipped:%-4d Errors:%-4d';
  DEFAULT_CASE_FORMAT =
    '  Results> Passed:%-5d Failed:%-5d Skipped:%-5d Errors:%-5d';
  METADATA_FORMAT = '%s'#9'%s'#9'%s'#9'%s'#9'%s';
  DEFAULT_SET_NAME = 'SET';
  FINAL_SET_NAME = '__';
  PREPARE_SET_NAME = '__prepare';
  FINALISE_SET_NAME = '__finalise';
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

  TSkipType = (skipFalse, skipTrue, skipCase, skipPrep, skipSet);

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

  TTestSet = Record
    Title: string;
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
  CurrentTitle: string = '';
  ExpectedException, ExpectedSetException, LastSetName, CurrentSetName,
    CurrentTestCaseName, CurrentTestCaseLabel, DeferredExpectedException,
    DeferredTestCaseLabel: string;
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
  DifferenceDisplayMode: TDifferenceViewMode = [dfRow, dfTwoColumn];

  CurrentCaseIndex: integer;
  TestMetaData: boolean = false;
  HasCaseList: boolean = false;
  ProjectInCaseList: boolean = false;

  CaseList: string = '';
  TestProgramName: string = '';

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
procedure NewAssertion(ACase: string; ATestCaseName: string = '');
Procedure NewSet(ASetName: string; ASkipped: TSkipType = skipFalse);
Procedure NewCase(ATestCaseName: string);
Procedure NewTestCase(ACase: string; ATestCaseName: string = '');
{$IFNDEF BEFOREVARIANTS}deprecated; {$ENDIF} // wrong naming convention.
Procedure NextTestSet(ASetName: string);
Procedure NextTestCase(ACaseName: string; ASkipped: TSkipType = skipFalse);
Function CheckIsEqual(AExpected, AResult: TComparitorType;
  AMessage: string = ''; ASkipped: TSkipType = skipFalse): boolean;
Function CheckIsCloseTo(AExpected, AResult: TComparitorType;
  ASignificantDecimalPlaces: byte = 3; AMessage: string = '';
  ASkipped: TSkipType = skipFalse): boolean;
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
Function TotalTests: integer;
Procedure TestSummary;
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
procedure DisplayMessage(AMessage: String; AMessageColour: smallint;
  ADataType: integer);

procedure TestingCompleted;
implementation

uses classes, math;

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
  Result :=
   (ACaseName <> '') and
   (not CreatingSets) and
   (not TestMetaData) and
   (not SameText(ACaseName, PREPARE_SET_NAME)) and
   (not SameText(ACaseName, FINALISE_SET_NAME))
   ;
end;

Function NextSetName: string;
begin
  SkippingSet := false;
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

Procedure SetDisplayMode(ARow, AColumn: boolean; AEscapeCRLF: boolean = false);
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
  DifferenceDisplayMode := lMode;
end;

Procedure DisplayModeDefault(AEscapeCRLF: boolean = false);
begin
  SetDisplayMode(true, true, AEscapeCRLF);
end;

Procedure DisplayModeRows(AEscapeCRLF: boolean = false);
begin
  SetDisplayMode(true, false, AEscapeCRLF);
end;

Procedure DisplayModeColumns(AEscapeCRLF: boolean = false);
begin
  SetDisplayMode(false, true, AEscapeCRLF);
end;

Procedure DisplayModeNone(AEscapeCRLF: boolean = false);
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
var
  l: integer;
begin
  if Length(CurrentSetName) = 0 then
    CurrentSetName := NextSetName;

  l := Length(MiniTestCases);
  SetLength(MiniTestCases, l + 1);
  MiniTestCases[l].Title := CurrentTitle;
  MiniTestCases[l].SetName := CurrentSetName;
  MiniTestCases[l].Execute := AProcedure;
  MiniTestCases[l].TestCaseName := ATestCaseName;
  MiniTestCases[l].Skip := ASkipped;
  if (SkippingSet) then
    MiniTestCases[l].Skip := skipSet;
  MiniTestCases[l].ExpectedException := AExpectedException;
end;

Procedure PrepareSet(AProcedure: TTestCaseProcedure);
begin
  AddTestCase(PREPARE_SET_NAME, AProcedure, skipPrep);
end;

Procedure FinaliseSet(AProcedure: TTestCaseProcedure);
begin
  AddTestCase(FINALISE_SET_NAME, AProcedure, skipPrep);
  CurrentSetName := '';
  CreatingSets := false;
  SkippingSet := false;
end;

Procedure FinalizeSet(AProcedure: TTestCaseProcedure);
begin
  // Americaniz(s)ed form
  FinaliseSet(AProcedure);
end;

Function GetProjectName: string;
begin
  if Length(TestProgramName) = 0 then
    TestProgramName := ChangeFileExt(ExtractFileName(Paramstr(0)), '');
  Result := TestProgramName;
end;

// Return a Unique(ish) Identifier for the specific test case
Function GenerateID(ASet: TTestSet): string;
var
  i, l, p: integer;
  lHash, lTestName: string;
begin
  lHash := copy(GetProjectName + ')(*&^%$#@!', 1, 10);
  lTestName := ASet.SetName + '/' + ASet.TestCaseName;
  l := Length(lTestName);
  for i := 1 to l do
  begin
    p := 1 + (i mod 10);
    lHash[p] := char(ord(lHash[p]) XOR ord(lTestName[i]) XOR (i mod 256));
  end;

  for i := 1 to 10 do
    Result := Result + Format('%.2x', [ord(lHash[i])]);
end;

Function GenerateCaseID(ACaseId: integer): string;
begin
  Result := GenerateID(MiniTestCases[ACaseId]);
end;

Function GenerateSetID(ACaseId: integer): string;
var
  ASet: TTestSet;
begin
  ASet := MiniTestCases[ACaseId];
  ASet.TestCaseName := '-';
  Result := GenerateID(ASet);
end;

function GenerateTitleId(ATitle: String): string;
var
  lTitleSet: TTestSet;
begin
  lTitleSet.SetName := ATitle;
  lTitleSet.TestCaseName := '-';
  Result := GenerateID(lTitleSet);
end;

Function GenerateProjectID(): string;
var
  lProjectSet: TTestSet;
begin
  lProjectSet.SetName := GetProjectName;
  lProjectSet.TestCaseName := '[]';
  Result := GenerateID(lProjectSet);
end;

Procedure OutputMetaData(AId: string; ACase: TTestSet;
  AStatusChar: string = '');
var
  lStatusChar: string;
  lCaseRow: string;
begin
  case ACase.Skip of
    skipFalse:
      lStatusChar := 'E';
    skipTrue:
      lStatusChar := 'D';
    skipCase:
      lStatusChar := 'C';
  end;
  if Length(AStatusChar) > 0 then
    lStatusChar := AStatusChar;
  lCaseRow := Format(METADATA_FORMAT, [lStatusChar, TestProgramName,
    ACase.SetName, ACase.TestCaseName, AId]);
  Writeln(lCaseRow);
end;

Procedure OutputCaseMetaData(ACaseId: integer);
var
  lId: string;
begin
  // probably Init or Finalise
  if (MiniTestCases[ACaseId].Skip = skipPrep) then
    exit;
  lId := GenerateCaseID(ACaseId);
  OutputMetaData(lId, MiniTestCases[ACaseId]);
end;

Procedure OutputSetMetaData(ACaseId: integer);
var
  lId: string;
  lSet: TTestSet;
begin
  lId := GenerateSetID(ACaseId);
  lSet := MiniTestCases[ACaseId];
  lSet.TestCaseName := '';
  OutputMetaData(lId, lSet, 'S');
end;

Procedure OutputTitleMetaData(ATitle: String);
var
  lTitleSet: TTestSet;
  lId: string;
begin
  lTitleSet.SetName := ATitle;
  lTitleSet.TestCaseName := '-';
  lTitleSet.Skip := skipFalse;
  lId := GenerateID(lTitleSet);
  lTitleSet.TestCaseName := '';
  OutputMetaData(lId, lTitleSet, 'T');
end;

Procedure OutputProjectMetaData();
var
  lId: string;
  lSet: TTestSet;
begin
  lId := GenerateProjectID();
  lSet.Skip := skipFalse;
  lSet.SetName := '';
  lSet.TestCaseName := '';
  OutputMetaData(lId, lSet, 'P');
end;

Procedure SetTitleMeta(ATitle: string);
var
  i, l: integer;
begin
  if TestMetaData then
    OutputTitleMetaData(ATitle);
  // retrospectively add the title to all of the cases without a Title.
  l := Length(MiniTestCases) - 1;
  for i := 0 to l do
    if Length(MiniTestCases[i].Title) = 0 then
      MiniTestCases[i].Title := ATitle;

end;

Procedure Title(AText: string);
var
  lText: string;
begin
  CurrentTitle := AText;
  if not TestMetaData then
  begin
    lText := 'DUnitm V-' + FRAMEWORK_VERSION;
    PrintLnCentred(lText, '=', clDefault);
    PrintLnCentred(AText, '=', clTitle);
    DoubleLine;
  end;
  SetTitleMeta(AText);
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

  if CaseIsEmpty or TestMetaData then
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
  if TestMetaData then
    exit;

  if (SetIsEmpty) or (Length(CurrentSetName) = 0) then
    exit;

  PrintLn(Format(SetOutputFormat, [SetCases, SetPassedTests + SetFailedTests +
    SetSkippedTests + SetErrors, SetPassedTests, SetFailedTests,
    SetSkippedTests, SetErrors]), ResultColour(SetHasErrors));

  SingleLine;
end;

Procedure TestSummary;
begin
  if TestMetaData then
    exit;

  NextTestCase('');
  DoubleLine;
  PrintLn(Format(TotalOutputFormat, [TotalSets, TotalCases, TotalTests,
    TotalPassedTests, TotalFailedTests, TotalSkippedTests, TotalErroredTests]),
    ResultColour(RunHasErrors or FOREGROUND_INTENSITY));
  Writeln('');
end;

Procedure SetHeading(ASetName: string);
var
  lHeading: string;
begin
  if (Length(ASetName) = 0) or TestMetaData then
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

Function CaseInCaseList(ACaseId: integer): boolean;
begin
  Result := (Not HasCaseList) or (pos(GenerateCaseID(ACaseId), CaseList) > 0);
end;

Function SetInCaseList(ACaseId: integer): boolean;
begin
  Result := (Not HasCaseList) or (pos(GenerateSetID(ACaseId), CaseList) > 0);
end;

Function TitleInCaseList(ACaseId: integer): boolean;
begin
  Result := Not(HasCaseList) or
    (pos(GenerateTitleId(MiniTestCases[ACaseId].Title), CaseList) > 0);
end;

Procedure RunTestSets;
var
  i, l: integer;
  lWholeCaseInList, lTitleInCaseList, lFirstCase: boolean;
  lTitle: string;
begin
  CurrentSetName := '';
  CreatingSets := false;
  lWholeCaseInList := false;
  lTitleInCaseList := false;
  l := Length(MiniTestCases);
  if l > 0 then
  begin
    LastSetName := MiniTestCases[0].SetName;
    NextTestSet(LastSetName);
    lTitleInCaseList := TitleInCaseList(0);
    lWholeCaseInList := (Not HasCaseList) or ProjectInCaseList or
      SetInCaseList(0) or lTitleInCaseList;
  end;
  lFirstCase := true;
  SetCases := 0;
  for i := 0 to l - 1 do
    Try
      CurrentCaseIndex := i;
      if not assigned(MiniTestCases[i].Execute) then
        continue;

      if TestMetaData then
      begin
        CurrentSetName := MiniTestCases[i].SetName;
        if (lFirstCase) or (CurrentSetName <> LastSetName) then
          OutputSetMetaData(i);
        OutputCaseMetaData(i);
      end
      else
      begin
        DisplayModeDefault;

        if (lTitle <> MiniTestCases[i].Title) then
          lTitleInCaseList := TitleInCaseList(i);
        lTitle := MiniTestCases[i].Title;

        CurrentSetName := MiniTestCases[i].SetName;

        if LastSetName <> CurrentSetName then
        begin
          lWholeCaseInList := (Not HasCaseList) or ProjectInCaseList or
            lTitleInCaseList or SetInCaseList(i);
        end;

        if MiniTestCases[i].Skip = skipSet then
          continue;



        if (MiniTestCases[i].Skip = skipPrep) or (lWholeCaseInList) or
          (HasCaseList and (CaseInCaseList(i))) then
        begin
          if MiniTestCases[i].Skip = skipCase then
          begin
            SkipTestCases(i);
          end
          else
          begin
            if MiniTestCases[i].TestCaseName <> '' then
              NextTestCase(MiniTestCases[i].TestCaseName,
                MiniTestCases[i].Skip);
            ExpectException(MiniTestCases[i].ExpectedException, true);
            MiniTestCases[i].Execute;
          end;
        end; // else not one of the cases requested.
      end;
      lFirstCase := false;
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
var
  lSetName: string;
begin

  SetResults;

  if ASetName = FINAL_SET_NAME then
    ASetName := ''; // Finalising.

  lSetName := ASetName;
  if Length(lSetName) < 0 then
    lSetName := 'Next';

  SetHeading(lSetName);
  inc(TotalSets);

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

Function Check(IsEqual: boolean; AExpected, AResult: TComparitorType;
  AMessage: string; ATestType: TCheckTestType): boolean;
var
  lMessage, lCounter: string;
  lResult: integer;
  Outcome: boolean;
  lMessageColour: smallint;
begin
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
                  Format('%s   Expected outcomes to differ, but both returned <%s>',
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
    if p > 0 then
    // probably not really possible.
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

Function DifferencesToConsoleArray(ADifferences: TStringDifferences)
  : TConsoleTextArray;
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
    if dfTwoColumn in DifferenceDisplayMode then
    begin
      if (not(dfRow in DifferenceDisplayMode)) or
        (FinalLengthofDifferences(ADifferences) > (ConsoleScreenWidth - 3)) then
      begin
        lColumnWidth := (ConsoleScreenWidth - 3) Div 2;
        lTwoColumn := true;
      end;
    end;
    lLeftColumn := TConsoleColumn.Create(lColumnWidth, clExpectedText,
      dfEscapeCRLF in DifferenceDisplayMode);
    lRightColumn := TConsoleColumn.Create(lColumnWidth, clActualText,
      dfEscapeCRLF in DifferenceDisplayMode);
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

procedure DisplayMessage(AMessage: String; AMessageColour: smallint;
  ADataType: integer);
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
  if (AMessageColour <> clError) OR (DifferenceDisplayMode = []) OR
    (DifferenceDisplayMode = [dfEscapeCRLF]) OR
    ((p < 1) OR (pos(EXPECTED_FORMAT_MESSAGE, AMessage) < 1) OR
    (pos(ACTUAL_FORMAT_MESSAGE, AMessage) < 1)) then
  begin
    DifferencesFound := 1;
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
  DifferencesFound := Length(lStringDifference);

  // Convert to Console Output
  lScreenText := DifferencesToConsoleArray(lStringDifference);
  for i := low(lScreenText) to High(lScreenText) do
  begin
    Print(lScreenText[i].Text, lScreenText[i].Colour);
    if lScreenText[i].EOL then
      Writeln;
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

Function CheckIsCloseTo(AExpected, AResult: TComparitorType;
  ASignificantDecimalPlaces: byte = 3; AMessage: string = '';
  ASkipped: TSkipType = skipFalse): boolean;
var
  lExpected, lResult: TComparitorType;
  lFactor : Single;
begin
  lFactor :=  power(10,ASignificantDecimalPlaces);
  lExpected := (trunc(AExpected * lFactor) / lFactor);
  lResult := (trunc(AResult * lFactor) / lFactor);
  result := false;
  try
    result := Check(True, lExpected, lResult, AMessage,
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

Procedure NewSet(ASetName: string; ASkipped: TSkipType = skipFalse);
begin
  CurrentSetName := ASetName;
  CreatingSets := true;
  SkippingSet := ASkipped in [skipTrue, skipSet];
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

procedure NewAssertion(ACase: string; ATestCaseName: string = '');
begin
  NewTest(ACase, ATestCaseName);
end;

Procedure DeferTestCase(ATestName: string = '');
begin
  if Length(ATestName) = 0 then
    DeferredTestCaseLabel := CurrentTestCaseLabel
  else
    DeferredTestCaseLabel := ATestName;
end;

Procedure DeferExpectedException(AExceptionClassName: string);
begin
  DeferredExpectedException := AExceptionClassName;
end;

Procedure ResumeTestCase;
begin
  if Length(DeferredTestCaseLabel) > 0 then
    NewTest(DeferredTestCaseLabel);
  if Length(DeferredExpectedException) > 0 then
    ExpectException(DeferredExpectedException);
  DeferredExpectedException := '';
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

procedure AssignCaseList;
var
  i: integer;
  lParam: string;
begin
  for i := 1 to ParamCount do
  begin
    lParam := Paramstr(i);
    if pos(copy(lParam, 1, 1), '/-') > 0 then
      continue;
    CaseList := CaseList + ' ' + lParam;
  end;
  HasCaseList := Length(CaseList) > 0;
  ProjectInCaseList := pos(GenerateProjectID(), CaseList) > 0;
end;

Procedure AssignTestMetaData;
begin
  TestMetaData := FindCmdLineSwitch('M', ['-', '/'], true);
  if TestMetaData then
    OutputProjectMetaData;
end;

procedure TestingCompleted;
begin
  if lowercase(Paramstr(1)) = '/p' then
    readln;
  ExitCode := (TotalFailedTests + TotalErroredTests);
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
AssignCaseList;
AssignTestMetaData;

end.

