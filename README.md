# DelphiTips
Useful Delphi Units which provide full functionality in a single PAS file.

## Mini Test Framework (MiniTestFrameWork.pas)
A fully functional DUnitX like Test framework.  It is simple to use and only requires the single pas file in a console application.
### Using Mini Test Framework
The following functions are supported:
  + ```Procedure Title(AText: string);```

  + ```Procedure NewTestSet(AClassName: string);```

  + ```Procedure SetTestCase(ACase: string; ATestClassName: string = '');```

  + ```Function  CheckIsEqual(AExpected, AResult: TValue; AMessage: string = ''; ASkipped: boolean=false): boolean;```

  + ```Function  CheckIsTrue(AResult: boolean; AMessage: string = ''; ASkipped: boolean=false): boolean;```

  + ```Function  CheckIsFalse(AResult: boolean; AMessage: string = ''; ASkipped: boolean=false): boolean;```

  + ```Function  CheckNotEqual(AResult1, AResult2: TValue;  AMessage: string = ''; ASkipped: boolean=false): boolean;```

  + ```Function  NotImplemented(AMessage: string=''):boolean;```

  + ```Procedure TestSummary;```

  + ```Procedure ClassResults;```

  + ```Procedure Print(AText: String; AColour: smallint = FOREGROUND_DEFAULT);```

  + ```Procedure PrintLn(AText: String; AColour: smallint = FOREGROUND_DEFAULT);```


### Classes and Cases, Sets and Runs
A "Test" is a check to confirm the outcome of a function, procedure call or condition.  A "Case" can have 1 to _n_ Tests. A "Class" of Tests consists of 1 to _n_ cases. All of the cases in a class is called a "Set".  The results of a particular execution of your test cases is called a "Run"


![Example Output of MiniTest](MiniTestFramework.png?raw=true)

