
![](https://github.com/glenkleidon/DelphiTips/blob/master/projectTemplates/MiniTest/MiniTest.ico?raw=true)

## DUnitm - Mini Test Framework [MiniTestFrameWork.pas](https://raw.githubusercontent.com/glenkleidon/DelphiTips/master/MiniTestFramework.pas)

How To videos on YouTube : [Using DUnitm - You Tube](https://www.youtube.com/playlist?list=PL42y13vA83auEzqLTzmkwnQuC6pyOxlQE)

A simple, fully functional XUnit style Test framework allowing Unit testing to be added to any console application.
DUnitm consists of only a single PAS.  A project template is included in the repo to add into Delphi's _New Projects_.

The reason for creating DUnitm (the 'm' stands for 'minimal') is that it can be included in any project without installing anything in the Delphi IDE and will work from the command line with (pretty much) any version of Delphi.  

The output will be something like this:

![Example Output of MiniTest](https://2.bp.blogspot.com/-IUHz-WmzpGo/Wd3_BR_xSHI/AAAAAAAACTM/faWUuduPoWkmwazV4-fv8EBY2WrDZKmdQCLcBGAs/s320/MiniTestFramework.png) 

Each Test is marked as Successful (green), Failed (red), Skipped (purple) or as Exceptions (cyan) and a summary of the output is shown at the end of each case and when all cases have been run.!

### Classes and Cases, Sets and Runs
A **"Test"** is one or more _Assertions_ that you make about the outcome of a function, procedure call or condition (eg IsTrue, IsFalse or IsEqual).  A **"Test Case"** is Procedure that contains one or more tests for related conditions in the unit being tested.  A **"Set"** (a suite in XUnit) is set of Procedures consisting of 1 to _n_ Cases.  The results of a particular execution of your test cases is called a **"Run"**.  You can prepare a run (set up fixtures), and finalise them (tear down fixture) after the tests have been run.

### Why do we need a new Test framework?
DUnitm is great for continuous delivery processes allowing test cases to be run during the build process. This is something that DUnitX also provides.

DUnitm fulfils the role of the _Test framework for bundled packages_. It is ideal for including in a project bundle or component set.    [Project Bundling](https://github.com/glenkleidon/bundles) is a packaging approach I am working towards for Delphi.  This follows the modern application development process using distributed version control systems to make projects portable into new and existing development environments with no IDE or project setup.

The modern approach to delivery of developer components is to use _packages_.  This is NOT the same thing as a delphi component package (.DPK) however.  In this context a project will have a _package file_ which defines what specific versions of components are required for the project to compile. There is usually a way to request those packages when the project is first loaded and to update when ever the package file changes.  In the NodeJs world packages are typically managed by "npm" or "yarn". In front-end JS development "Bower" has been used in the past, but npm and yarn have largely taken over. In the .Net world this is done using "Nuget", however Visual Studio 2015 and 2017 also support NPM and Bower inside the IDE.

In all these cases, a package consists of the code source, and/or ready to use binaries, other packages that the component set requires (package dependencies), and importantly a set of test cases to confirm that the components are working correctly in your environment after you install them.  That is where DUnitm fits in - it provides a Test framework that can be easily bundled inside packages.

NOTE that the "GetIt" Package management tool included with the newest versions of Delphi does NOT meet this criteria. GetIt is an online store for Components. While this is a great tool, it does not provide the same functionality as npm, Yarn or NuGet because the components are installed system wide, not as part of the project.

### Thread Safety.
DUnitm uses static functions and does not create any objects itself and is therefore leak proof.  However as at version 1 of Delphi tips, DUnitm is absolutely **_NOT_ thread safe** due to its use of global counters.  In very large projects there is likely to be great benefit in terms of performance if it were possible to execute all of the required test runs in parallel.  This may add a great time advantage in Integrated Delivery systems.  In fact that can be easily achieved using records in modern delphi, however versions of delphi prior to 2010 have no support of record helpers or record methods making the solution far less elegant. Never-the-less thread safety is on the DUnitm roadmap.


## Example Code 

### Simple Test Run
The easiest way to use the DUnitm is using a simple test run.  Simple test runs allow you to run cases directly: There is no setup or teardown. The disadvantages of using a simple run is that encountering an exception will cause the run to stop without running all the tests unless you manually trap and check the exceptions. Simple test runs also don't support skipping a whole case.

To make a Simple Test run, create a console app, include MiniTestFramework.pas and the Unit/s your are trying to test.  You can add Test Case Procedures directly to your DPR or in a separate unit.
 
``` 
program UnitTestLabelledEnum;
{
  Example of A simple DUnitm Test run
}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas',
  DelphiEnum in '..\..\DelphiEnum.pas';  // unit I want to test

  // Add Test Case Procedures...
  procedure Label_of_static_Type_Passes;
  begin
    ...
  end;
  
  Procedure Without_Labels_Labelled_Enums_match_Static_types;
  begin
   ...
  end;

  Static_Class_Function_Labelled_AsString_returns_labels_as_expected;
  begin
    ...
  end;
// Main Program
begin
 
  Title('Test cases for TLabelledEnum');
  
  Label_of_static_Type_Passes;
  Without_Labels_Labelled_Enums_match_Static_types;
  Static_Class_Function_Labelled_AsString_returns_labels_as_expected;

  TestSummary; 

end;
```
The steps are:
1. Give the Test Run a Title with the **_Title_** function  
2. Call each Test Case Procedure in turn.  
   1. Each Case should include a test or tests added using the **_NewTest_** function
   2. Each Test should have Assertions using **_CheckIsEqual_**, **_CheckIsTrue_**, **_CheckIsFalse_** or **_CheckNotEqual_**
   3. Tests may also use **_ExpectException_** in conjunction with **_CheckException_** (which needs to be caught manually)
   4. The test outcome can be **_SKIPPED_** (using _SKIP_).  This means the actions will still be evaluated, but wont count toward the success or failure tally. (You need to use a standard test set to skip cases evaluation).
3. Display the summary by calling **_TestSummary_**

### Standard Test Projects
The recommended approach for using DUnitm is to create a console app, add MiniTestFramework.pas and a Test Unit to contain your test cases to the uses clause.  This keeps the test cases clean, closely related to the functions they test and more importantly, re-usable.  The Delphi Tips repo contains a Project Template for all versions of delphi.  The template allows you to create a new Unit test framework from the _New_ Menu item in Delphi.

![](https://github.com/glenkleidon/DelphiTips/blob/master/projectTemplates/NewUnitTest-D7.png?raw=true)


The advantages over using simple test runs are that exceptions do not have to be manually managed: the framework will trap the exception for you and peform the evaluation.  Also, in simple test runs, skipping does not actually prevent the actions from being evaluated.  Using SKIP at the test case level, will prevent any of the tests in the case from being run.


### Standard Test Runs 
The Name of the test unit should reflect the name of the unit you are trying to test. Eg when testing _"MyUnit.pas"_ your test case unit should be called something like _"Test_MyUnit.pas"_  You could also create separate units for specific parts of your test unit (say if there are multiple classes in the unit).

Here is an example of a **_standard test run_**.  

```
program UnitTestRecordUtils;
{
  Example of Standard DUnitm Test runs
}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas',
  Test_RecordUtils in '..\..\Test_RecordUtils.pas';  // My Test unit

begin
 
  Title('Test Cases For Tools used for RecordUtils');
  NewSet('Example Test Set');
  PrepareSet(nil);
  AddTestCase('JSON to Value Pairs', JSONTOValuePairs_Works_as_Expected);
  AddTestCase('Is URL Encoding', IsURLEncoding_works_as_expected);
  FinaliseSet(nil);
  RunTestSets;
  TestSummary;
  
  // add more test runs if desired
  Title('Test Cases For Specific Record Utils functions');
  PrepareSet(Setup);
  ...
end;
```
You could of course add a static function to the test unit called something like "AddTests" which makes your test case even more re-usable.

The steps are:
1. Give the Test Run a title with the **_Title_** function  
2. Name the set with **_NewSet_**
3. Perform any setup required for the run by calling **_PrepareSet_** passing in the Setup Procedure (or nothing)
4. Add as many Test Cases as needed by calling **_AddTestCase_** passing in a description and a Test Procedure
5. Skipping a test case is supported.  You can skip the case entrirely using the _SKIPCASE_ option where no tests are run for that case.
6. Tear Down the Run by calling **_FinaliseSet_** passing in the Teardown Procedure (if needed)
7. Execute the Test Run by calling **_RunTestSets_**
8. [Optionally] Create another set by repeating from steps 2
9. Display the Summary by calling **_TestSummary_**

### Test Cases
Test cases typically follow the standard pattern of Describe, Arrange, Act, Assert.  Look at the procedure we want to test in the example below  

**Example 1**
```
program DUnitMExample1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas';

  function SomeFunctionThatReturns1: integer;
  begin
    result := 1;
  end;
```
Here is the Test Case..
```
  Procedure Example_Test_Set_Shows_Expected_Result;
  var Expected, Result : integer;
  begin
   // Set up a new Case and reset case level counters. (Not needed for a Standard run)
   NewCase('Example Test Set');

   // Describe
   NewTest('Checking Good Outcomes');
   // Arrange
   Expected :=1;
   // Act
   Result := SomeFunctionThatReturns1;
   // Assertions
   CheckisTrue(Expected=Result,'Using Is True');
   CheckisFalse(Result=1000,'Using IsFalse');
   CheckisEqual(Expected,Result,'Using IsEqual',SKIPPED); // dont fail on this test

   // Describe
   NewTest('Checking Failed Outcomes');
   // Arrange
   Expected :=2;
   // Act
   Result := SomeFunctionThatReturns1;
   // Assertions
   CheckisTrue(Expected=Result,'Using IsTrue should fail');
   CheckisFalse(Result=1000,'Using IsFalse should pass');
   CheckisEqual(Expected,Result,'Using IsEqual should fail'); // dont fail on this test
  end;
```
And the main program 
```
begin
  try
   Title('Test cases for TLabelledEnum');

   Example_Test_Set_Shows_Expected_Result;

   TestSummary;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
```
![DUnitM Screen Shot](https://www.galkam.com.au/images/opensource/DelphiTips/DUnitExample1ConsoleOutput.PNG)

## DUnitm Interface Specification
The following functions are supported:

### Set Functions
#### Procedure Title(AText: string); 
Specifies a Title for a RUN
+ _AText_: a string to show as the title.

#### Procedure NewSet(ASetName: string);
+ _ASetName_: the name of the set 
Labels the Current Test Run. If this function is omitted in the preparation of a set, the default set name will be applied.

#### Procedure AddTestCase(ATestClass: string; AProcedure : TTestCaseProcedure; ASkipped:TSkipType=skipFalse; AExpectedException: string = ''); 
Adds a Test Case (Procedure) to the current Test Set
+ _ATestClass_: a string name for the Test Case
+ _AProcedure_: the Procedure containing the Tests (Assertions)
+ _ASkipped_: TSkipType, _skipCase_ if the case is to be skipped (default=_skipfalse_). Using _skipTrue_ will still evaluate the actions but will not contribute to the pass/fail tally
+ _AExpectedException_: a string containing the Exception Class Name or any part of the exception message 

#### Procedure AddTestSet(ATestClass: string; AProcedure : TTestCaseProcedure; ASkipped:TSkipType=skipFalse; AExpectedException: string = '');
**_Deprecated_** in Favour of AddTestCase.  The naming of this method was incorrect as it suggested adding a Set to a Run rather than a Case to a Set.

#### Procedure PrepareSet(AProcedure: TTestCaseProcedure);
Adds a Setup Procedure to the current test set.  Note: if no setup is required, PrepareSet should still be called with a "nil" setup procedure
 + AProcedure: Procedure containing any required setup for the Set.
 
#### Procedure FinaliseSet(AProcedure: TTestCaseProcedure); \[Alias: FinalizeSet\]
Adds a tear down to the current test set. _Note: if no tear-down is required, FinaliseSet should still be called with a "nil" setup procedure_
 + AProcedure: Procedure containing any required tear down for the set.
 
#### Procedure RunTestSets;
Executes the Test Run.

#### Procedure TestSummary;
Outputs the Summary for the Current RUN.


### Case Functions
#### Procedure NewCase(ATestCaseName: string);
(For Simple runs) adds a new test case to the run.  Not required for Test sets as the AddTestCase function adds the case name.
+ _ATestCaseName_: The name of the test case as it will appear in the output.

#### Procedure NewTest(ACase: string; ATestCaseName: string = '');
Instructs the Test run to add a new Test to the current Test Case;
+ _ACase_: the Description for the Test.
+ _ATestCaseName_: \[optional\] name of the test case.  (Typically only used internally)

#### Procedure NewTestCase(ACase: string; ATestCaseName: string = '');
**_Deprecated_** in Favour of NewTest. The naming of this function in earlier releases was incorrect as it suggested adding a new case to a set, rather than a new test to a case.

#### Procedure ExpectException(AExceptionClassName: string;AExpectForSet: boolean=false);
Instructs the RUN that the Exception Class name is expected. 

### Assertions
#### Function CheckIsEqual(AExpected, AResult: TComparitorType;  AMessage: string = ''; ASkipped:TSkipType=skipFalse): boolean; 
Assertion that the Expected result matches the Actual Result.
+ _AExpected_ : The expected outcome for the test
+ _AResult_ : The Actual outcome of the test peformed
+ _AMessage_ : Message to display if failed (default shows a comparison of tested values)
+ _ASkipped_ : TSkipType, skipTrue to skip this test (default=skipFalse);

#### Function  CheckIsTrue(AResult: boolean; AMessage: string = ''; ASkipped:TSkipType=skipFalse): boolean;
Assertion that the condition in AResult _IS TRUE_
+ _AResult_ : Any type of Boolean result
+ _AMessage_ : Message to display if failed (default shows a comparison of tested values)
+ _ASkipped_ : TSkipType, skipTrue to skip this test (default=skipFalse);

#### Function  CheckIsFalse(AResult: boolean; AMessage: string = ''; ASkipped:TSkipType=skipFalse): boolean; 
Assertion that the condition in AResult _IS FALSE_
+ _AResult1_ : Any type of supported datatype to be compared with AResul2
+ _AResult2_ : Any type of supported datatype to be compared with AResul1
+ _AMessage_ : Message to display if failed (default shows a comparison of tested values)
+ _ASkipped_ : TSkipType, skipTrue to skip this test (default=skipFalse);
 
#### Function  CheckNotEqual(AResult1, AResult2: TComparitorType;  AMessage: string = ''; ASkipped:TSkipType=skipFalse): boolean;

Assertion that Result 1 _DOES NOT EQUAL_ Result2
+ _AResult1_ : Any type of supported datatype to be compared with AResul2
+ _AResult2_ : Any type of supported datatype to be compared with AResul1
+ _AMessage_ : Message to display if failed (default shows a comparison of tested values)
+ _ASkipped_ : TSkipType, skipTrue to skip this test (default=skipFalse);

#### Procedure CheckException(AException: Exception);
Checks an exception to confirm if it was expected or not.  Note in simply runs, this method can be used in conjunction with _try except on e:exception do_ construct. 
+ _AException_: A Exception object descendant which will be tested against the expected exception for this test.
 
 
### Private Methods

#### Function NotImplemented(AMessage: string=''):boolean;
Checks if a Message is the Not implemented method
+ _AMessage_: \[optional\] A message to describe why the test is not implemented.

#### Function DontSkip:TSkipType;
Instructs the run to _NOT_ skip the next test even if the case is entirely skipped.

#### Function TotalTests: integer;
Calculates the test Totals.

#### procedure CaseResults;
(For Simple runs) Displays the current Test Case results

#### procedure SetResults;
(For Simple runs) Displays the Current Set Results 

#### Procedure NextTestSet(ASetName: string);
Called by the Test Run to Roll the counters over to the next test Set
+ _ASetName_: The name of the current Set


#### Procedure NextTestCase(ACaseName: string; ASkipped:TSkipType=skipFalse);
Called by the test Run to roll the counters over to the next test Case
 + _ACaseName_: The name of the current case
 + _ASkipped_ : boolean, True to skip this test (default=false);

#### Function ConsoleScreenWidth:integer;
Determine the current console screen width for summary output

#### Procedure Print(AText: String; AColour: smallint = FOREGROUND_DEFAULT);
#### Procedure PrintLn(AText: String; AColour: smallint = FOREGROUND_DEFAULT)`
Print functions to output text with the selected screen colour 
 + _AText_: text to be output
 + _AColour_: Colour for the text (colour constants must be used)


