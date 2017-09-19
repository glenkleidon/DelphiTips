unit TestCasesRecordUtils;

interface
  uses MiniTestFramework
   (* TODO -oUser -cTestCase1 Unit :
   (**)
      ,RecordUtils
   (**)
  ;
Procedure Setup;
Procedure Record_Clears_as_expected;
Procedure Clone_copies_values_as_expected;
Procedure Parse_Populates_record_as_expected;
Procedure AsValuePairs_Exports_values_as_Expected;
Procedure Implicit_Cast_To_Static_works_as_Expected;
Procedure Implicit_Cast_From_Static_works_as_Expected;
Procedure TearDown;

Type TTestRecordUnitStatictype = Record
        number : integer;
        text   : string;
        bool   : boolean;
        flNum  : single;
End;
     TSerialisableTestStaticType = TRecordSerializer<TTestRecordUnitStaticTYpe>;


implementation

Procedure Setup;
begin
  {Prepare anything you want here.
   NOTE:
    you can prepare as many setups to the set as you like
    or simply add them inside the set Procedure
  }
end;


Procedure Record_Clears_as_expected;
var lResult : TSerialisableTestStaticType;
begin
  NewTestCase('Static Type Clears as Expected');
  lResult.Clear;
  checkisTrue(lResult.Values.number=0);
  checkisTrue(lResult.Values.text='');
  checkisFalse(lResult.Values.bool);
  checkisTrue(lResult.Values.flNum=0);
end;

Procedure Clone_copies_values_as_expected;
var lResult,lClone : TSerialisableTestStaticType;
begin
   lResult.Values.number := 5;
   lResult.Values.text := 'TestValue TEXT';
   lResult.Values.bool := false;
   lResult.Values.flNum := 5.663;
   lClone.clone(lResult.Values);
   checkisEqual(lResult.Values.number,lClone.Values.number);
   checkisEqual(lResult.Values.text,lClone.Values.text);
   checkisEqual(lResult.Values.bool,lClone.Values.bool);
   checkisEqual(lResult.Values.flNum,lClone.Values.flNum);
end;

Procedure Parse_Populates_record_as_expected;
var lExpected: String;
    lResult : TSerialisableTestStaticType;
begin
  lExpected := 'number=5'#13#10+
               'text=TestValue TEXT'#13#10+
               'bool=False'#13#10+
               'flNum=5.663';
  lResult.Parse(lExpected);
  checkisFalse(lResult.Values.bool);
  checkisEqual(lResult.Values.number,5);
  checkisEqual(lResult.Values.text,'TestValue TEXT');
  checkisEqual(trunc(lresult.Values.flNum*1000)/1000,5.663); // double conversion...
end;

Procedure AsValuePairs_Exports_values_as_Expected;
var lResult : TSerialisableTestStaticType;
    lExpected : string;
begin
  lExpected := 'number=5'#13#10+
             'text=TestValue TEXT'#13#10+
             'bool=False'#13#10+
             'flNum=0'#13#10;
   lResult.Values.number := 5;
   lResult.Values.text := 'TestValue TEXT';
   lResult.Values.bool := false;
   lResult.Values.flNum := 0;
   checkisEqual(lExpected,lResult.AsValuePairs);
end;

Procedure Implicit_Cast_To_Static_works_as_Expected;
var lResult : TTestRecordUnitStatictype;
var lSerialisable : TSerialisableTestStaticType;
begin
   lSerialisable.Values.number := 5;
   lSerialisable.Values.text := 'TestValue TEXT';
   lSerialisable.Values.bool := false;
   lSerialisable.Values.flNum := 5.663;
   lResult := lSerialisable;
   checkisEqual(lResult.number,lSerialisable.Values.number);
   checkisEqual(lResult.text,lSerialisable.Values.text);
   checkisEqual(lResult.bool,lSerialisable.Values.bool);
   checkisEqual(lResult.flNum,lSerialisable.Values.flNum);
end;

Procedure Implicit_Cast_From_Static_works_as_Expected;
var lStatic : TTestRecordUnitStatictype;
var lResult : TSerialisableTestStaticType;
begin
   lStatic.number := 5;
   lStatic.text := 'TestValue TEXT';
   lStatic.bool := false;
   lStatic.flNum := 5.663;
   lResult := lStatic;
   checkisEqual(lStatic.number,lResult.Values.number);
   checkisEqual(lStatic.text,lResult.Values.text);
   checkisEqual(lStatic.bool,lResult.Values.bool);
   checkisEqual(lStatic.flNum,lResult.Values.flNum);
end;


Procedure TearDown;
begin
  {Release anything you need to here}
end;

end.
