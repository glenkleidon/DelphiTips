program DUnitMExample1;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  MiniTestFramework in '..\MiniTestFramework.pas';

  function SomeFunctionThatReturns1: integer;
  begin
    result := 1;
  end;

  Procedure Example_Test_Set_Shows_Expected_Result;
  var Expected, Result : integer;
  begin
   // Set up a new Case and reset case level counters. (Not needed for a Standard run)
   NewTestSet('Example Test Set');

   // Describe
   NewTestCase('Checking Good Outcomes');
   // Arrange
   Expected :=1;
   // Act
   Result := SomeFunctionThatReturns1;
   // Assertions
   CheckisTrue(Expected=Result,'Using Is True');
   CheckisFalse(Result=1000,'Using IsFalse');
   CheckisEqual(Expected,Result,'Using IsEqual',SKIPPED); // dont fail on this test

   NewTestCase('Checking Failed Outcomes');
   Expected :=2;
   // Act
   Result := SomeFunctionThatReturns1;
   //
   CheckisTrue(Expected=Result,'Using IsTrue should fail');
   CheckisFalse(Result=1000,'Using IsFalse should pass');
   CheckisEqual(Expected,Result,'Using IsEqual should fail'); // dont fail on this test
  end;

begin
  try
   Title('Test cases for TLabelledEnum');

   Example_Test_Set_Shows_Expected_Result;

   TestSummary;

   readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
