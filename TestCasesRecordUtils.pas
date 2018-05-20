unit TestCasesRecordUtils;

interface
  uses SysUtils,MiniTestFramework
   (* TODO -oUser -cTestCase1 Unit :
   (**)
      ,RecordUtils
   (**)
  ;
// Tools for RecordUtils
procedure JSONTOValuePairs_Works_as_Expected;
procedure ISURLEncoding_works_as_expected;
// RecordUtils
Procedure Setup;
Procedure Record_Clears_as_expected;
Procedure Clone_copies_values_as_expected;
Procedure Parse_Populates_record_as_expected;
Procedure AsValuePairs_Exports_values_as_Expected;
Procedure Implicit_Cast_To_Static_works_as_Expected;
Procedure Implicit_Cast_From_Static_works_as_Expected;
Procedure Implicit_Cast_To_String_works_as_Expected;
Procedure Implicit_Cast_From_String_works_as_Expected;
Procedure Check_REsult_from_function_contains_correct_pointers;
Procedure AsJSON_works_as_Expected;
Procedure FromJSON_works_as_Expected;
Procedure AsURLEncoded_works_as_Expected;
Procedure FromURLEncoded_works_as_Expected;
Procedure Parse_Array_Works_as_Expected;
Procedure Parse_Array_Update_Works_as_Expected;
Procedure Parse_Array_Exceptions_Detected_as_expected;
Procedure Parse_Array_Update_Exceptions_Detected_as_expected;
Procedure AsValuePairs_Exports_Arrays_as_Expected;
Procedure AsJSON_Exports_Arrays_as_Expected;
Procedure Enums_as_Integer_works_as_Expected;
Procedure SetOf_generates_array_of_types;
Procedure Array_of_Type_parses_back_into_a_set;
Procedure Add_Works_as_expected;

//SerializerObject
procedure Record_stores_a_persistent_object;


Procedure TearDown;

Type
  TCards = (Hart,Spade,Diamond,Club);
  TSetOfSuits = Set of TCards;

  TTestRecord = Record
    number : integer;
    text   : string;
    bool   : boolean;
    flNum  : single;
    card   : TCards;
  End;

  TTestRecordSet = Record
    number : integer;
    text   : string;
    bool   : boolean;
    flNum  : single;
    cards  : TSetOfSuits;
  End;

  TSerialisableRecord = TRecordSerializer<TTestRecord>;
  PSerialisableRecord = ^TSerialisableRecord;
  TSerialisableRecordSet = TRecordSerializer<TTestRecordSet>;

  TSerialisableRecords = Array of TSerialisableRecord;

  ITestRecordSerializer = interface
    Function Serializer : PSerialisableRecord;
  end;

  TSerialisableRecordObject = class(TRecordSerializerObject<TSerialisableRecord>,ITestRecordSerializer)
      public
       Function Serializer : PSerialisableRecord;
  end
   ;




implementation

// JSON Tool Tests
procedure JSONTOValuePairs_Works_as_Expected;
var lExpected, lResult: String;
begin
  lExpected :=
      'ValuePair[0].id=1'#13#10+
      'ValuePair[0].Name=Name'#13#10+
      'ValuePair[0].Object1.One=One'#13#10+
      'ValuePair[0].Object1.Two=Two'#13#10+
      'ValuePair[0].Object1.Three=3'#13#10+
      'ValuePair[0].Object1.ObjectArray[0].id=22'#13#10+
      'ValuePair[0].Object1.ObjectArray[0].Name=Object2-0'#13#10+
      'ValuePair[0].Object1.ObjectArray[1].id=23'#13#10+
      'ValuePair[0].Object1.ObjectArray[1].Name=Object2-1'#13#10+
      'ValuePair[1].id=2'#13#10+
      'ValuePair[1].Name=Name2'#13#10+
      'ValuePair[1].Object1.One=1'#13#10+
      'ValuePair[1].Object1.Two=2'#13#10+
      'ValuePair[1].Object1.Three=Three'#13#10+
      'ValuePair[1].Object1.ObjectArray[0].id=33'#13#10+
      'ValuePair[1].Object1.ObjectArray[0].Name=Object2-3'#13#10+
      'ValuePair[1].Object1.ObjectArray[1].id=34'#13#10+
      'ValuePair[1].Object1.ObjectArray[1].Name=Object2-4'#13#10;

  lResult := JSONToValuePairs(
    '{'#13#10+
    '	"ValuePair": [{'#13#10+
    '		"id": 1,'#13#10+
    '		"Name": "Name",'#13#10+
    '		"Object1": {'#13#10+
    '			"One": "One",'#13#10+
    '			"Two": "Two",'#13#10+
    '			"Three": 3,'#13#10+
    '			"ObjectArray": [{'#13#10+
    '				"id": 22,'#13#10+
    '				"Name": "Object2-0"'#13#10+
    '			},'#13#10+
    '			{'#13#10+
    '				"id": 23,'#13#10+
    '				"Name": "Object2-1"'#13#10+
    '			}]'#13#10+
    '		}'#13#10+
    '	},'#13#10+
    '	{'#13#10+
    '		"id": 2,'#13#10+
    '		"Name": "Name2",'#13#10+
    '		"Object1": {'#13#10+
    '			"One": 1,'#13#10+
    '			"Two": 2,'#13#10+
    '			"Three": "Three",'#13#10+
    '			"ObjectArray": [{'#13#10+
    '				"id": 33,'#13#10+
    '				"Name": "Object2-3"'#13#10+
    '			},'#13#10+
    '			{'#13#10+
    '				"id": 34,'#13#10+
    '				"Name": "Object2-4"'#13#10+
    '			}]'#13#10+
    '		}'#13#10+
    '	}'#13#10+
    '	]'#13#10+
    '}'
  );
  checkisEqual(lExpected,lResult);

end;
// URL Encoding
procedure ISURLEncoding_works_as_expected;
var lURL, lVp: string;
begin
  newTestCase('URL or VP Text has No Ambiguity');
  lURL :=
     'number=5&'+
     'text=TestValue+is+15%25+%26+%0D%0A&'+
     'bool=False&'+
     'flNum=5.663';
  lVP :=
   'number=5'#13#10+
   'text=TestValue TEXT'#13#10+
   'bool=False'#13#10+
   'flNum=5.663';

  checkIsTrue(isURLEncoding(lURL));
  checkIsFalse(isURLEncoding(lVP));

  newTestCase('Check when both & and CR are present');
  lURL := lURL+#13#10;
  lVP  := stringreplace(lVP,'TEXT','& TEXT',[]);
  checkIsTrue(isURLEncoding(lURL),'URL Detect Failed');
  checkIsFalse(isURLEncoding(lVp),'Value Pair Detect Failed');


end;

Procedure Setup;
begin
  {Prepare anything you want here.
   NOTE:
    you can prepare as many setups to the set as you like
    or simply add them inside the set Procedure
  }
end;


Procedure Record_Clears_as_expected;
var lResult : TSerialisableRecord;
begin
  NewTestCase('Static Type Clears as Expected');
  lResult.Clear;
  checkisTrue(lResult.Value.number=0);
  checkisTrue(lResult.Value.text='');
  checkisFalse(lResult.Value.bool);
  checkisTrue(lResult.Value.flNum=0);
end;

Procedure Clone_copies_Values_as_expected;
var lResult,lClone : TSerialisableRecord;
begin
   lResult.Value.number := 5;
   lResult.Value.text := 'TestValue TEXT';
   lResult.Value.bool := false;
   lResult.Value.flNum := 5.663;
   lClone.clone(lResult.Value);
   checkisEqual(lResult.Value.number,lClone.Value.number);
   checkisEqual(lResult.Value.text,lClone.Value.text);
   checkisEqual(lResult.Value.bool,lClone.Value.bool);
   checkisEqual(lResult.Value.flNum,lClone.Value.flNum);
end;

Procedure Parse_Populates_record_as_expected;
var lExpected: String;
    lResult : TSerialisableRecord;
begin
  lExpected := 'number=5'#13#10+
               'text=TestValue TEXT'#13#10+
               'bool=False'#13#10+
               'flNum=5.663'#13#10+
               'card=Hart';
  lResult.Parse(lExpected);
  checkisFalse(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue TEXT',lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...
  checkIsEqual(Hart,lResult.Value.card);
end;

Procedure Implicit_Cast_To_String_works_as_Expected;
var lResult : TSerialisableRecord;
    lExpected : string;
begin
  lExpected := 'number=5'#13#10+
             'text=TestValue TEXT'#13#10+
             'bool=False'#13#10+
             'flNum=0'#13#10+
             'card=Hart'#13#10;
   lResult.Value.number := 5;
   lResult.Value.text := 'TestValue TEXT';
   lResult.Value.bool := false;
   lResult.Value.flNum := 0;
   lResult.Value.card := Hart;
   checkisEqual(lExpected,lResult);

end;

Procedure Implicit_Cast_From_String_works_as_Expected;
var lExpected: String;
    lResult : TSerialisableRecord;
begin
  lExpected := 'number=5'#13#10+
               'text=TestValue TEXT'#13#10+
               'bool=False'#13#10+
               'flNum=5.663'#13#10+
               'card=Spade';
  lResult := lExpected;
  checkisFalse(lResult.Value.bool);
  lResult.Value.bool := Not lResult.Value.bool;
  checkisTrue(lResult.Value.bool);
  checkisEqual(Spade,lResult.Value.card);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue TEXT',lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...

  lExpected :=
     'number=5&'+
     'text=TestValue+is+15%25+%26+%0D%0A&'+
     'bool=False&'+
     'flNum=5.663'#13#10+
     'card=Club';
  lResult := lExpected;
  checkisFalse(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue is 15% & '#13#10,lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...
  checkisEqual(Club,lResult.Value.card);

end;

Procedure AsValuePairs_Exports_Values_as_Expected;
var lResult : TSerialisableRecord;
    lExpected : string;
begin
  lExpected := 'number=5'#13#10+
             'text=TestValue TEXT'#13#10+
             'bool=False'#13#10+
             'flNum=0'#13#10+
             'card=Hart'#13#10;
   lResult.Value.number := 5;
   lResult.Value.text := 'TestValue TEXT';
   lResult.Value.bool := false;
   lResult.Value.flNum := 0;
   lResult.Value.card := Hart;
   checkisEqual(lExpected,lResult.AsValuePairs);
end;

Procedure Implicit_Cast_To_Static_works_as_Expected;
var lResult : TTestRecord;
var lSerialisable : TSerialisableRecord;
begin
   lSerialisable.Value.number := 5;
   lSerialisable.Value.text := 'TestValue TEXT';
   lSerialisable.Value.bool := false;
   lSerialisable.Value.flNum := 5.663;
   lResult := lSerialisable;
   checkisEqual(lSerialisable.Value.number,lResult.number);
   checkisEqual(lSerialisable.Value.text,lResult.text);
   checkisEqual(lResult.bool,lSerialisable.Value.bool);
   checkisEqual(lSerialisable.Value.flNum,lResult.flNum);
end;

Procedure Implicit_Cast_From_Static_works_as_Expected;
var lStatic : TTestRecord;
var lResult : TSerialisableRecord;
begin
   lStatic.number := 5;
   lStatic.text := 'TestValue TEXT';
   lStatic.bool := false;
   lStatic.flNum := 5.663;
   lResult := lStatic;
   checkisEqual(lResult.Value.number,lStatic.number);
   checkisEqual(lResult.Value.text,lStatic.text);
   checkisEqual(lResult.Value.bool,lStatic.bool);
   checkisEqual(lResult.Value.flNum,lStatic.flNum);
end;

Procedure AsJSON_works_as_Expected;
var lExpected: String;
    lRecord : TSerialisableRecord;
    lDblValue: Double;
    lDblValueStr: string;
begin
  lDblValue := 5.0;
  lDblValueStr := FloatToStr(lDblValue);
  lExpected := '{"number":5,'+
                '"text":"TestValue TEXT is \"\"",'+
                '"bool":false,'+
                '"flNum":'+lDblValueStr+','+
                '"card":"Diamond"'+
                '}';
  lRecord.Value.number := 5;
  lRecord.Value.text := 'TestValue TEXT is ""';
  lRecord.Value.bool := false;
  lRecord.Value.flNum := lDblValue;
  lRecord.Value.card := Diamond;
  checkIsEqual(lExpected,lRecord.AsJSON);
end;

Procedure FromJSON_works_as_Expected;
var lExpected: String;
    lResult : TSerialisableRecord;
    lDblValue: Double;
    lDblValueStr: string;
begin
  lDblValue := 5.0;
  lDblValueStr := FloatToStr(lDblValue);
  lExpected := '{"number":5,'+
                '"text":"TestValue TEXT is \"\"",'+
                '"bool":False,'+
                '"flNum":'+lDblValueStr+'}';
  lResult.FromJSON(lExpected);
  checkisFalse(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue TEXT is ""',lResult.Value.text);
  checkisEqual(5,trunc(lresult.Value.flNum*1000)/1000); // double conversion...
end;

function PointerCheck(AText: String): TTestRecord;
var lSerialisable: TSerialisableRecord;
begin
 // This function serves to prove that the data is copied in a sensible way.
 // There are two implicit casts here. The Parse method is called on
 // the Local Variable lSerialisable and then, the Result.Clone method is called
 // to Copy the the value into Result
 lSerialisable := AText;
 Result := lSerialisable;
end;

Procedure Check_Result_from_function_contains_correct_pointers;
var lExpected: String;
    lResult : TSerialisableRecord;
begin
  lExpected := 'number=5'#13#10+
               'text=TestValue TEXT'#13#10+
               'bool=False'#13#10+
               'flNum=5.663';
  lResult := PointerCheck(lExpected);
  checkisFalse(lResult.Value.bool);
  lResult.Value.bool := Not lResult.Value.bool;
  checkisTrue(lResult.Value.bool);

  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue TEXT',lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...

  lExpected :=
     'number=5&'+
     'text=TestValue+is+15%25+%26+%0D%0A&'+
     'bool=False&'+
     'flNum=5.663';
  lResult := lExpected;
  checkisFalse(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue is 15% & '#13#10,lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...
end;


Procedure AsURLEncoded_works_as_Expected;
var lResult : TSerialisableRecord;
    lExpected : string;
begin
  lExpected :=
     'number=5&'+
     'text=TestValue+is+15%25+%26+%0D%0A&'+
     'bool=False&'+
     'flNum=0&'+
     'card=Hart';
   lResult.Value.number := 5;
   lResult.Value.text := 'TestValue is 15% & '#13#10;
   lResult.Value.bool := false;
   lResult.Value.flNum := 0;
   lResult.Value.card := Hart;
   checkisEqual(lExpected,lResult.AsURLEncoded);
end;

Procedure FromURLEncoded_works_as_Expected;
var lExpected: String;
    lResult : TSerialisableRecord;
begin
  lExpected :=
     'number=5&'+
     'text=TestValue+is+15%25+%26+%0D%0A&'+
     'bool=False&'+
     'flNum=5.663';
  lResult.FromURLEncoded(lExpected);
  checkisFalse(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue is 15% & '#13#10,lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...
end;

Procedure Parse_Array_Update_Works_as_Expected;
var lExpected: String;
    lResult : TSerialisableRecord;
    lDblValue: Double;
    lDblValueStr: String;
begin
  lDblValue := 5.663;
  lDblValueStr := floatToStr(lDblValue);

  NewTestCase('2 Arrays, starting at 0 Parse Correctly');
  lExpected := 'number[0]=5'#13#10+
               'text[0]=TestValue TEXT'#13#10+
               'bool[0]=False'#13#10+
               'flNum[0]='+lDblValueStr+#13#10+
               'number[1]=6'#13#10+
               'text[1]=TestValue TEXT 2'#13#10+
               'bool[1]=True'#13#10;
  lDblValue := 300.01;
  lDblValueStr := floatToStr(lDblValue);
  lExpected := lExpected +  'flNum[1]='+lDblValueStr;

  checkisEqual(2,lResult.Parse(lExpected,spmAppend),'Expected 2 Records to be added');
  checkisEqual(2,lResult.Count,'Expected Count to be 2');

  NewTestCase('Array Element 0 has Correct Values');
  checkisFalse(lResult.Values[0].bool);
  checkisEqual(5,lResult.Values[0].number);
  checkisEqual('TestValue TEXT',lResult.Values[0].text);
  checkisEqual(5.663,trunc(lResult.Values[0].flNum*1000)/1000); // double conversion...

  NewTestCase('Array Element 1 has Correct Values');
  checkisTrue(lResult.Values[1].bool);
  checkisEqual(6,lResult.Values[1].number);
  checkisEqual('TestValue TEXT 2',lResult.Values[1].text);
  checkisEqual(300.01,trunc(lResult.Values[1].flNum*1000)/1000); // double conversion...

  NewTestCase('Update Single Row');

  lExpected := 'number[0]=128'#13#10+
               'text[0]=TestValue TEXT 4'#13#10+
               'bool[0]=False'#13#10+
               'flNum[0]=21.2'#13#10;
  checkisEqual(1,lResult.Parse(lExpected),'Expected 1 Records to be added');
  checkisEqual(2,lResult.Count,'Expected Count to be 2');

  NewTestCase('Array Element 0 has Correct Values');
  checkisFalse(lResult.Values[0].bool);
  checkisEqual(128,lResult.Values[0].number);
  checkisEqual('TestValue TEXT 4',lResult.Values[0].text);
  checkisEqual(21.2,trunc(lResult.Values[0].flNum*1000)/1000); // double conversion...

  lExpected := 'number[0]=6'#13#10+
               'text[0]=TestValue TEXT 2'#13#10+
               'bool[0]=True'#13#10+
               'flNum[0]=301'#13#10+
               'number[1]=8'#13#10+
               'text[1]=TestValue TEXT 3'#13#10+
               'bool[1]=False'#13#10+
               'flNum[1]=5.01';
  checkisEqual(2,lResult.Parse(lExpected),'Expected 2 Records to be added');
  checkisEqual(2,lResult.Count,'Expected Count to be 2');

  NewTestCase('Array Element 0 has Correct Values');
  checkisTrue(lResult.Values[0].bool);
  checkisEqual(6,lResult.Values[0].number);
  checkisEqual('TestValue TEXT 2',lResult.Values[0].text);
  checkisEqual(301,trunc(lResult.Values[0].flNum*1000)/1000); // double conversion...

  NewTestCase('Array Element 1 has Correct Values');
  checkisFalse(lResult.Values[1].bool);
  checkisEqual(8,lResult.Values[1].number);
  checkisEqual('TestValue TEXT 3',lResult.Values[1].text);
  checkisEqual(5.01,trunc(lResult.Values[1].flNum*1000)/1000); // double conversion...

  NewTestCase('Update Single index without specifying bounds');
  lExpected := 'number=3'#13#10+
               'text=TestValue TEXT 6'#13#10+
               'bool=False'#13#10+
               'flNum=44.2'#13#10;

  checkisEqual(1,lResult.Parse(lExpected,spmUpdate,1),'Expected 1 Record to be added');
  checkisEqual(2,lResult.Count,'Expected Count to be 2');

  NewTestCase('Array Element 1 has Correct Values');
  checkisFalse(lResult.Values[1].bool);
  checkisEqual(3,lResult.Values[1].number);
  checkisEqual('TestValue TEXT 6',lResult.Values[1].text);
  checkisEqual(44.2,trunc(lResult.Values[1].flNum*1000)/1000); // double conversion...

  NewTestCase('Update Single Index using Index');
  lExpected := 'number=3'#13#10+
               'text=TestValue TEXT 6'#13#10+
               'bool=False'#13#10+
               'flNum=44.2'#13#10;

  checkisEqual(1,lResult.Parse(lExpected,spmAppend,1));
  checkisEqual(3,lResult.Count,'Expected Count to be 3');

  NewTestCase('Array Element 1 has Correct Values');
  checkisFalse(lResult.Values[1].bool);
  checkisEqual(3,lResult.Values[1].number);
  checkisEqual('TestValue TEXT 6',lResult.Values[1].text);
  checkisEqual(44.2,trunc(lResult.Values[1].flNum*1000)/1000); // double conversion...

end;

Procedure Parse_Array_Exceptions_Detected_as_expected;
var lExpected: String;
    lResult : TSerialisableRecord;
    lDblValue: Double;
begin
  NewTestCase('2 Arrays, starting at 0 Parse Correctly');
  lDblValue := 5.663;

  lExpected := 'number[0]=5'#13#10+
               'text[0]=TestValue TEXT'#13#10+
               'bool[0]=False'#13#10+
               'flNum[0]='+floatToStr(lDblValue)+#13#10+
               'number[1]=6'#13#10+
               'text[1]=TestValue TEXT 2'#13#10+
               'bool[1]=True'#13#10;
  lDblValue := 300.01;
  lExpected := lExpected  + 'flNum[1]='+FloatToStr(lDblValue);

  lResult := lExpected;
  checkisequal(lExpected, lResult);

  NewTestCase('Update Single Index using Index');
  lExpected := 'number=3'#13#10+
               'text=TestValue TEXT 6'#13#10+
               'bool=False'#13#10+
               'flNum=44.2'#13#10;

  lResult.Parse(lExpected);
  checkisequal(lExpected, lResult);

end;

Procedure Parse_Array_Works_as_Expected;
var lExpected: String;
    lResult : TSerialisableRecord;
begin
  NewTestCase('Single Array Parses into Values');
  lExpected := 'number[0]=5'#13#10+
               'text[0]=TestValue TEXT'#13#10+
               'bool[0]=False'#13#10+
               'flNum[0]=5.663';
  checkisEqual(1,lResult.Parse(lExpected));
  checkisFalse(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue TEXT',lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...

  NewTestCase('Second Parse Appends to existing Array');
  lExpected := 'number[0]=6'#13#10+
               'text[0]=TestValue TEXT 2'#13#10+
               'bool[0]=True'#13#10+
               'flNum[0]=300.01';
  checkisEqual(1,lResult.Parse(lExpected,spmAppend),'Expected 1 Record to be added');
  checkisEqual(2,lResult.Count,'Expected Count to be 2');
  checkisFalse(lResult.Values[0].bool);
  checkisEqual(5,lResult.Values[0].number);
  checkisEqual('TestValue TEXT',lResult.Values[0].text);
  checkisEqual(5.663,trunc(lResult.Values[0].flNum*1000)/1000); // double conversion...
  checkisTrue(lResult.Values[1].bool);
  checkisEqual(6,lResult.Values[1].number);
  checkisEqual('TestValue TEXT 2',lResult.Values[1].text);
  checkisEqual(300.01,trunc(lResult.Values[1].flNum*1000)/1000); // double conversion...

  NewTestCase('Clear Array Works as Expected');
  lResult.Clear;
  Checkistrue(lResult.Value.number=0);
  Checkisequal(lresult.Count,0);


  NewTestCase('2 Arrays, starting at 0 Parse Correctly');
  lExpected := 'number[0]=5'#13#10+
               'text[0]=TestValue TEXT'#13#10+
               'bool[0]=False'#13#10+
               'flNum[0]=5.663'#13#10+
               'number[1]=6'#13#10+
               'text[1]=TestValue TEXT 2'#13#10+
               'bool[1]=True'#13#10+
               'flNum[1]=300.01';
  checkisEqual(2,lResult.Parse(lExpected,spmAppend),'Expected 2 Records to be added');
  checkisEqual(2,lResult.Count,'Expected Count to be 2');

  NewTestCase('Array Element 0 has Correct Values');
  checkisFalse(lResult.Values[0].bool);
  checkisEqual(5,lResult.Values[0].number);
  checkisEqual('TestValue TEXT',lResult.Values[0].text);
  checkisEqual(5.663,trunc(lResult.Values[0].flNum*1000)/1000); // double conversion...

  NewTestCase('Array Element 1 has Correct Values');
  checkisTrue(lResult.Values[1].bool);
  checkisEqual(6,lResult.Values[1].number);
  checkisEqual('TestValue TEXT 2',lResult.Values[1].text);
  checkisEqual(300.01,trunc(lResult.Values[1].flNum*1000)/1000); // double conversion...

  lResult.Clear;

  NewTestCase('2 Arrays, starting at 0 Parse Correctly');
  lExpected := 'number[0]=5'#13#10+
               'text[0]=TestValue TEXT'#13#10+
               'bool[0]=False'#13#10+
               'flNum[0]=5.663'#13#10+
               'number[1]=6'#13#10+
               'text[1]=TestValue TEXT 2'#13#10+
               'bool[1]=True'#13#10+
               'flNum[1]=300.01';
  checkisEqual(2,lResult.Parse(lExpected),'Expected 2 Records to be added');
  checkisEqual(2,lResult.Count,'Expected Count to be 2');

  NewTestCase('Array Element 0 has Correct Values');
  checkisFalse(lResult.Values[0].bool);
  checkisEqual(5,lResult.Values[0].number);
  checkisEqual('TestValue TEXT',lResult.Values[0].text);
  checkisEqual(5.663,trunc(lResult.Values[0].flNum*1000)/1000); // double conversion...

  NewTestCase('Array Element 1 has Correct Values');
  checkisTrue(lResult.Values[1].bool);
  checkisEqual(6,lResult.Values[1].number);
  checkisEqual('TestValue TEXT 2',lResult.Values[1].text);
  checkisEqual(300.01,trunc(lResult.Values[1].flNum*1000)/1000); // double conversion...

  lResult.Clear;

  NewTestCase('2 Arrays, starting at 0, skipping 1 and 2 Parse Correctly');
  lExpected := 'number[0]=5'#13#10+
               'text[0]=TestValue TEXT'#13#10+
               'bool[0]=False'#13#10+
               'flNum[0]=5.663'#13#10+
               'number[3]=6'#13#10+
               'text[3]=TestValue TEXT 3'#13#10+
               'bool[3]=True'#13#10+
               'flNum[3]=300.01';
  checkisEqual(2,lResult.Parse(lExpected),'Expected 2 Records to be added');
  checkisEqual(4,lResult.Count,'Expected Count to be 2');

  NewTestCase('Array Element 0 has Correct Values');
  checkisFalse(lResult.Values[0].bool);
  checkisEqual(5,lResult.Values[0].number);
  checkisEqual('TestValue TEXT',lResult.Values[0].text);
  checkisEqual(5.663,trunc(lResult.Values[0].flNum*1000)/1000); // double conversion...

  NewTestCase('Array Element 3 has Correct Values');
  checkisTrue(lResult.Values[3].bool);
  checkisEqual(6,lResult.Values[3].number);
  checkisEqual('TestValue TEXT 3',lResult.Values[3].text);
  checkisEqual(300.01,trunc(lResult.Values[3].flNum*1000)/1000); // double conversion...
end;

Procedure AsValuePairs_Exports_Arrays_as_Expected;
var lResult : TSerialisableRecord;
    lExpected, lExpected2 : string;
begin
  NewTestCase('Single Array Exports as array Values');
  lExpected :=
       'number[0]=5'#13#10+
       'text[0]=TestValue TEXT'#13#10+
       'bool[0]=False'#13#10+
       'flNum[0]=103'#13#10+
       'card[0]=Hart'#13#10;

  lExpected2 :=
       'number=333'#13#10+
       'text=TestValue TEXT 2'#13#10+
       'bool=False'#13#10+
       'flNum=105'#13#10+
       'card=Hart'#13#10;
  checkisEqual(1,lResult.Parse(lExpected));
  checkisEqual(lExpected,lResult.AsValuePairs);

  NewTestCase('2 Array Exports as array Values');
  checkisEqual(1,lResult.Parse(lExpected2,spmAppend));
  checkisEqual(lExpected+stringReplace(lExpected2,'=','[1]=',[rfReplaceAll])
                ,lResult.AsValuePairs);

  NewTestCase('2 Array Export Second Element as array Values');
  checkisEqual(stringReplace(lExpected2,'=','[1]=',[rfReplaceAll])
                ,lResult.AsValuePairs(1));
end;

Procedure Parse_Array_Update_Exceptions_Detected_as_expected;
var lResult : TSerialisableRecord;
    lExpected, lExpected2 : string;
begin
  NewTestCase('Single Array Exports as array Values');
  lExpected :=
       'number[0]=5'#13#10+
       'text[0]=TestValue TEXT'#13#10+
       'bool[0]=False'#13#10+
       'flNum[0]=103'#13#10+
       'card[0]=Hart'#13#10;
  lExpected2 :=
       'number[1]=333'#13#10+
       'text[1]=TestValue TEXT 2'#13#10+
       'bool[1]=False'#13#10+
       'flNum[1]=105'#13#10+
       'card[1]=Hart'#13#10;
  checkisEqual(1,lResult.Parse(lExpected));
  checkisEqual(lExpected,lResult.AsValuePairs);

  NewTestCase('2 Array Exports as array Values');
  lResult.Parse(lExpected2,spmUpdate);
end;

Procedure Enums_as_Integer_works_as_Expected;
var lExpected: String;
    lResult : TSerialisableRecord;
begin
  lExpected := 'number=5'#13#10+
               'text=TestValue TEXT'#13#10+
               'bool=0'#13#10+
               'flNum=5.663'#13#10+
               'card=1';
  lResult.Parse(lExpected);
  checkisFalse(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue TEXT',lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...
  checkIsEqual(Spade,lResult.Value.card);
 lExpected := 'number=5'#13#10+
               'text=TestValue TEXT2'#13#10+
               'bool=1'#13#10+
               'flNum=5.663'#13#10+
               'card=3';
  lResult.Parse(lExpected);
  checkisTrue(lResult.Value.bool);
  checkisEqual(5,lResult.Value.number);
  checkisEqual('TestValue TEXT2',lResult.Value.text);
  checkisEqual(5.663,trunc(lresult.Value.flNum*1000)/1000); // double conversion...
  checkIsEqual(Club,lResult.Value.card);
end;

Procedure AsJSON_Exports_Arrays_as_Expected;
var lExpected: String;
    lRecords : TSerialisableRecord;
    lDblValue: Double;
    lDblValueStr: string;
begin
  lDblValue := 5.0;
  lDblValueStr := FloatToStr(lDblValue);
  lExpected := '[{"number":5,'+
                '"text":"TestValue TEXT is \"\"",'+
                '"bool":false,'+
                '"flNum":'+lDblValueStr+','+
                '"card":"Spade"'+
                '},'+
                '{"number":7,'+
                '"text":"TestValue TEXT 2 is \"\"",'+
                '"bool":true,'+
                '"flNum":'+lDblValueStr+','+
                '"card":"Club"'+
                '}]';
  lRecords.FromJSON(lExpected);
  checkisTrue(lRecords.isArray);
  checkisEqual(lRecords.Values[0].number,5);
  checkisEqual(lRecords.Values[0].text,'TestValue TEXT is ""');
  checkisEqual(lRecords.Values[0].bool,false);
  checkisEqual(lRecords.Values[0].flNum,lDblValue);
  checkisEqual(lRecords.Values[0].Card,Spade);
  checkisEqual(lRecords.Values[1].number,7);
  checkisEqual(lRecords.Values[1].text,'TestValue TEXT 2 is ""');
  checkisTrue(lRecords.Values[1].bool);
  checkisEqual(lRecords.Values[1].flNum,lDblValue);
  checkisEqual(lRecords.Values[1].Card,Club);
  checkIsEqual(lExpected,lRecords.AsJSON);
end;

Procedure SetOf_generates_array_of_types;
var lValue : TTestRecordSet;
    lSValue : TSerialisableRecordSet;
    lExpected, lResult : string;
    lDblValue: Double;
    lDblValueStr: string;
begin
   lDblValue := 5.0;
   lDblValueStr := FloatToStr(lDblValue);
   lValue.number := 1;
   lValue.text := 'TestValue TEXT is ""';
   lValue.bool := true;
   lValue.flNum := lDblValue;
   lValue.cards := [Hart,Spade];

   NewTestCase('Add a Single Element');
   lExpected := '{"number":1,'+
                '"text":"TestValue TEXT is \"\"",'+
                '"bool":true,'+
                '"flNum":'+lDblValueStr+','+
                '"cards":["Hart","Spade"]'+
                '}';
   lSValue.Add(lValue);
   checkisEqual(lExpected, lSvalue.AsJSON);

end;

Procedure Array_of_Type_parses_back_into_a_set;
var lValue : TTestRecordSet;
    lSValue,
    lSValue2 : TSerialisableRecordSet;
    lExpected, lResult : string;
    lDblValue: Double;
    lDblValueStr: string;
begin
   lDblValue := 5.0;
   lDblValueStr := FloatToStr(lDblValue);
   lValue.number := 1;
   lValue.text := 'TestValue TEXT is ""';
   lValue.bool := true;
   lValue.flNum := lDblValue;
   //(Hart,Spade,Diamond,Club)
  // lValue.cards := [Hart,Spade,Diamond];
   lValue.cards := [Hart,Club];

   NewTestCase('Add a Single Element as JSON');
   lExpected := '{"number":1,'+
                '"text":"TestValue TEXT is \"\"",'+
                '"bool":true,'+
                '"flNum":'+lDblValueStr+','+
                '"cards":["Hart","Club"]'+
                '}';
   lSValue.Add(lValue);
   checkisEqual(lExpected, lSvalue.AsJSON);
   lSValue2 := lSvalue.AsJSON;
   checkisEqual(lExpected, lSvalue2.asJson);

   NewTestCase('Add a Single Element as Value Pairs');
   lExpected := 'number=1'+#13#10+
                'text=TestValue TEXT is ""'+#13#10+
                'bool=True'+#13#10+
                'flNum='+lDblValueStr+#13#10+
                'cards=[Hart,Club]'#13#10;
   checkisEqual(lExpected, lSvalue.AsValuePairs);
   lSValue2 := lSValue.AsValuePairs;
   checkisEqual(lExpected, lSvalue2.asValuePairs);




end;

Procedure Add_Works_as_expected;
var lValue : TTestRecord;
    lSValue : TSerialisableRecord;
    lExpected, lResult : string;
    lDblValue: Double;
    lDblValueStr: string;
begin
   lDblValue := 5.0;
   lDblValueStr := FloatToStr(lDblValue);
   lValue.number := 1;
   lValue.text := 'TestValue TEXT is ""';
   lValue.bool := true;
   lValue.flNum := lDblValue;
   lValue.card := hart;

   NewTestCase('Add a Single Element');
   lExpected := '{"number":1,'+
                '"text":"TestValue TEXT is \"\"",'+
                '"bool":true,'+
                '"flNum":'+lDblValueStr+','+
                '"card":"Hart"'+
                '}';
   lSValue.Add(lValue);
   checkisEqual(lExpected, lSvalue.AsJSON);

   NewTestCase('Add another Single Element');
   lExpected := '[' + lExpected + ',' + lExpected + ']';
   lSValue.Add(lValue);
   checkisEqual(lExpected, lSvalue.AsJSON);

end;

procedure Record_stores_a_persistent_object;
var lValue : TTestRecord;
    lSValue : TSerialisableRecordObject;
    lExpected, lResult : string;
    lDblValue: Double;
    lDblValueStr: string;
begin
   lDblValue := 5.0;
   lDblValueStr := FloatToStr(lDblValue);
   lValue.number := 1;
   lValue.text := 'TestValue TEXT is ""';
   lValue.bool := true;
   lValue.flNum := lDblValue;
   lValue.card := hart;

   lSValue := TSerialisableRecordObject.Create;

   NewTestCase('Add a Single Element');
   lExpected := '{"number":1,'+
                '"text":"TestValue TEXT is \"\"",'+
                '"bool":true,'+
                '"flNum":'+lDblValueStr+','+
                '"card":"Hart"'+
                '}';
   lSValue.Serializer.Add(lValue);
   checkisEqual(lExpected, lSvalue.Serializer.AsJSON);

   NewTestCase('Add another Single Element');
   lExpected := '[' + lExpected + ',' + lExpected + ']';

   lSValue.Serializer.Add(lValue);
   checkisEqual(lExpected, lSvalue.Serializer.AsJSON);

end;


Procedure TearDown;
begin
  {Release anything you need to here}
end;

{ TSerialisableRecordAsObject }


{ TSerialisableRecordObject }

function TSerialisableRecordObject.Serializer: PSerialisableRecord;
begin
   result := PValue;
end;

end.
