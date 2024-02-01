unit TestCases1;

interface
  uses MiniTestFramework
   (* TODO -oUser -cTestCase1 Unit :
   (** )
      Insert Your UNITS here
   (**)
  ;

implementation
 uses sysutils,
   ToolsAPI,
   System.Diagnostics;

 procedure PrintCallStack;
var
  Stack: TStack;
  Frame: TStackFrame;
  I: Integer;
begin
  Stack := TStack.Create;
  try
    for I := 0 to Stack.FrameCount - 1 do
    begin
      Frame := Stack.Frames[I];
      // Format and print the frame information
      // For example, you might print the method name and address
      Writeln(Format('Method: %s, Address: %p', [Frame.MethodName, Frame.Address]));
    end;
  finally
    Stack.Free;
  end;
end;


Procedure Setup;
begin
  {Prepare anything you want here.
   NOTE:
    you can prepare as many setups to the set as you like
    or simply add them inside the set Procedure
  }


Procedure Case_One_Passes;
begin
  NewTest('Case 1 passes as expected');
  checkisEqual('ABC','A'+'B'+'C');
end;

Procedure Case_Two_Fails;
begin
  NewTest('Case 2 is Expected to Fail');
  checkisEqual('ABC','D'+'E'+'F');
end;

Procedure Case_Three_Gets_Error;
var StringVar: variant;
    x : integer;
begin
  NewTest('Case 3 Gets Expected Exception');
  ExpectedException:='EVariantTypeCastError';
  StringVar:='ABC';
  x := 1 + StringVar; // force variant exception.
  PrintLn('This line never prints' + inttostr(x));
end;

Procedure Case_Four_skips;
begin
  NewTest('Case 4 Test 1 should Skip (Set Level skip)');
  checkisEqual('ABC','A'+'B'+'C',''{,SKIP}); // skipped by set level

  NewTest('Case 4 Test 2 should Pass (Using Dont Skip)');
  checkisEqual('ABC','A'+'B'+'C','',DONTSKIP); // decide to run this one after all

end;

Procedure TearDown;
begin
  {Release anything you need to here}
end;

procedure NotMentioned;
begin
  CheckIsTrue(false,'This set should not have been run');
end;

initialization
 NewSet('Skip Set Demo This set will not be executed', skipTrue);
 PrepareSet(nil);
 AddTestCase('This entire set will be ignored', NotMentioned);
 FinaliseSet(nil);

    NewSet('Example Pass/Fail');
    PrepareSet(Setup);
    AddTestCase('Passing Test Example',  Case_One_Passes);
    AddTestCase('Failing Test Example',  Case_Two_Fails);
    FinaliseSet(TearDown);

    NewSet('Example Exception and Skip');
    AddTestCase('Expected Exception Example',Case_Three_Gets_Error);
    AddTestCase('Skipping All Tests Example', Case_Four_skips, SKIP);
    AddTestCase('Skip Case Entirely', Case_four_skips, skipCase);
    FinaliseSet(TearDown);

end.
