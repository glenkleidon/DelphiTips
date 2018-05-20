unit TestCases1;

interface
  uses MiniTestFramework
   (* TODO -oUser -cTestCase1 Unit :
   (** )
      Insert Your UNITS here
   (**)
  ;
Procedure Setup;
Procedure Case_One_Passes;
Procedure Case_Two_Fails;
Procedure Case_Three_Gets_Error;
Procedure Case_Four_skips;
Procedure TearDown;

implementation
 uses sysutils;

Procedure Setup;
begin
  {Prepare anything you want here.
   NOTE:
    you can prepare as many setups to the set as you like
    or simply add them inside the set Procedure
  }
end;


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

end.
