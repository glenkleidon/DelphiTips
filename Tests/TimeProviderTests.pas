unit TimeProviderTests;

interface
uses
  SysUtils,
  TimeProvider,
  MiniTestFramework;


implementation
uses DateUtils, Math;

var Sut: ITimeProvider;

procedure Setup;
begin
  Sut := TTimeProvider.Create;

end;

procedure TearDown;
begin
  Sut := nil;
end;

function TimeAsString(ATime: TDateTime): string;
begin
  result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ATime);
end;

procedure IncreasesByExpectedAmount;
var
  lExpected, TestDate: TDateTime;
begin
  TestDate := EncodeDate(2000,1,1);

  NewAssertion('Increase by 1 year');
  lExpected := EncodeDate(2001,1,1);
  Sut.ChangeTime(TestDate,true);
  Sut.IncTime(1, tpYears);
  CheckIsEqual(lExpected, Sut.Now);

  NewAssertion('Increase by 1 month');
  lExpected := EncodeDate(2001,2,1);
  Sut.IncTime(1, tpMonths);
  CheckIsEqual(lExpected, Sut.Now);

  NewAssertion('Increase by 2 weeks');
  lExpected := EncodeDate(2001,2,15);
  Sut.IncTime(2, tpWeeks);
  CheckIsEqual(lExpected, Sut.Now);

  NewAssertion('Increase by 3 Days');
  lExpected := EncodeDate(2001,2,18);
  Sut.IncTime(3, tpDays);
  CheckIsEqual(lExpected, Sut.Now);

  NewAssertion('Increase by 4 hours');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,0,0,0);
  Sut.IncTime(4, tpHours);
  CheckIsEqual( TimeAsString(lExpected),TimeAsString(Sut.Now) );

  NewAssertion('Increase by 5 minutes');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,5,0,0);
  Sut.IncTime(5, tpMinutes);
  CheckIsEqual( TimeAsString(lExpected),TimeAsString(Sut.Now) );

  NewAssertion('Increase by 6 Seconds');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,5,6,0);
  Sut.IncTime(6, tpSeconds);
  CheckIsEqual( TimeAsString(lExpected),TimeAsString(Sut.Now) );

  NewAssertion('Increase by 789 MilliSeconds');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,5,6,789);
  Sut.IncTime(789, tpMilliseconds);
  CheckIsEqual( TimeAsString(lExpected),TimeAsString(Sut.Now) );

end;

procedure ChangeTimeSetsCorrectTime;
var
   StartTime, NewTime : TDateTime;

begin
  StartTime := sut.Now;
  NewTest('Create Time should be less than 2 seconds from now');
  CheckIsTrue(Abs(SecondSpan(SysUtils.Now, StartTime))<2);

  NewTest('Changing time to Y2k should should be within 2 seconds of Y2k');
  NewTime := EncodeDate(2000,1,1);
  Sut.ChangeTime(NewTime);
  CheckIsTrue(Abs(SecondSpan(Sut.Now, NewTime))<2);

end;

procedure FixedTimeStaysFixed;
var
   FixedTime : TDateTime;
begin
   FixedTime := EncodeDate(2000, 1, 1);
   Sut.ChangeTime(FixedTime, true);
   Sleep(1000);
   CheckIsEqual(FixedTime,Sut.Now);
   Sleep(1000);
   CheckIsEqual(FixedTime,Sut.Now);
end;

procedure ReturnsRelativeTime;
var
   StartTime, lExpected : TDateTime;
begin
  StartTime := EncodeDate(2000,1,1);
  NewAssertion('set to system');
  Sut.ChangeTime(Now);
  CheckIsCloseTo(SysUtils.Now, sut.Now,8);


  NewAssertion('Altered By 1 year');
  lExpected := EncodeDate(2001,1,1);
  Sut.ChangeTime(StartTime,true);
  CheckIsEqual(lExpected, Sut.TimeIn(1, tpYears));

  NewAssertion('Altered By 1 month');
  lExpected := EncodeDate(2000,2,1);
  CheckIsEqual(lExpected, Sut.TimeIn(1, tpMonths));

  NewAssertion('Altered By 2 weeks');
  lExpected := EncodeDate(2000,1,15);
  CheckIsEqual(lExpected, Sut.TimeIn(2, tpWeeks));

  NewAssertion('Altered By 3 Days');
  lExpected := EncodeDate(2000,1,4);
  CheckIsEqual(lExpected,  Sut.TimeIn(3, tpDays));

  NewAssertion('Altered By 4 hours');
  lExpected := EncodeDate(2000,1,1) + EncodeTime(4,0,0,0);
  CheckIsEqual( TimeAsString(lExpected),
   TimeAsString(Sut.TimeIn(4, tpHours)) );

  NewAssertion('Altered By 5 minutes');
  lExpected := EncodeDate(2000,1,1) + EncodeTime(0,5,0,0);
  CheckIsEqual( TimeAsString(lExpected),
   TimeAsString(Sut.TimeIn(5, tpMinutes)) );

  NewAssertion('Altered By 6 Seconds');
  lExpected := EncodeDate(2000,1,1) + EncodeTime(0,0,6,0);
  CheckIsEqual( TimeAsString(lExpected),
   TimeAsString(Sut.TimeIn(6, tpSeconds)) );

  NewAssertion('Altered By 789 MilliSeconds');
  lExpected := EncodeDate(2000,1,1) + EncodeTime(0,0,0,789);
  CheckIsEqual( TimeAsString(lExpected),
    TimeAsString(Sut.TimeIn(789, tpMilliseconds)) );
end;

initialization

  NewSet('Time Provider');
  PrepareSet(Setup);
  AddTestCase('ChangeTime sets correct time', ChangeTimeSetsCorrectTime);
  AddTestCase('Fixed Time stays fixed', FixedTimeStaysFixed);
  AddTestCase('Fixed Time increases by expected amount', IncreasesByExpectedAmount);
  AddTestCase('TimeIn returns adjusted relative time', ReturnsRelativeTime);

  FinaliseSet(TearDown);


end.
