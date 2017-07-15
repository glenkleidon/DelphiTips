unit TestCases1;

interface
  uses MiniTestFramework
   (* TODO -oUser -cTestCase1 Unit :
   (** )
      Insert Your UNITS here
   (**)
  ;
Procedure Setup;
Procedure Test_One_Passes;
Procedure Test_Two_Fails;
Procedure Test_Three_Gets_Error;
Procedure Test_Four_skips;
Procedure TearDown;

implementation

Procedure Setup;
begin
  {Prepare anything you want here.
   NOTE:
    you can prepare as many setups to the set as you like
    or simply add them inside the set Procedure
  }
end;


Procedure Test_One_Passes;
begin
  NewTestCase('Test 1 passes as expected');
  checkisEqual('ABC','A'+'B'+'C');
end;

Procedure Test_Two_Fails;
begin
  NewTestCase('Test 2 is Expected to Fail');
  checkisEqual('ABC','D'+'E'+'F');
end;

Procedure Test_Three_Gets_Error;
var StringVar: variant;
    X: Integer;
begin
  NewTestCase('Test 3 Gets Expected');
  ExpectedException:='EVariantTypeCastError';
  StringVar:='ABC';
  x := 1+StringVar;
end;

Procedure Test_Four_skips;
begin
  NewTestCase('Test 4 Case 1 should Skip (Set Level skip)');
  checkisEqual('ABC','A'+'B'+'C',''{,SKIP}); // skipped by set level

  NewTestCase('Test 4 Case 2 should Pass (Using Dont Skip)');
  checkisEqual('ABC','A'+'B'+'C','',DONTSKIP); // decide to run this one after all

end;

Procedure TearDown;
begin
  {Release anything you need to here}
end;

end.
