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

procedure IncreasesByExpectedAmount;
var
  lExpected, TestDate: TDateTime;
  function TimeAsString(ATime: TDateTime): string;
  begin
    result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ATime);
  end;
begin
  TestDate := EncodeDate(2000,1,1);

  NewAssertion('Increase by 1 year');
  lExpected := EncodeDate(2001,1,1);
  Sut.ChangeTime(TestDate,true);
  Sut.IncTime(1, tpYear);
  CheckIsEqual(Sut.Now, lExpected);

  NewAssertion('Increase by 1 month');
  lExpected := EncodeDate(2001,2,1);
  Sut.IncTime(1, tpMonth);
  CheckIsEqual(lExpected, Sut.Now);

  NewAssertion('Increase by 2 weeks');
  lExpected := EncodeDate(2001,2,15);
  Sut.IncTime(2, tpWeek);
  CheckIsEqual(lExpected, Sut.Now);

  NewAssertion('Increase by 3 Days');
  lExpected := EncodeDate(2001,2,18);
  Sut.IncTime(3, tpDay);
  CheckIsEqual(lExpected, Sut.Now);

  NewAssertion('Increase by 4 hours');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,0,0,0);
  Sut.IncTime(4, tpHour);
  CheckIsEqual( TimeAsString(lExpected),TimeAsString(Sut.Now) );

  NewAssertion('Increase by 5 minutes');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,5,0,0);
  Sut.IncTime(5, tpMinute);
  CheckIsEqual( TimeAsString(lExpected),TimeAsString(Sut.Now) );

  NewAssertion('Increase by 6 Seconds');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,5,6,0);
  Sut.IncTime(6, tpSecond);
  CheckIsEqual( TimeAsString(lExpected),TimeAsString(Sut.Now) );

  NewAssertion('Increase by 789 MilliSeconds');
  lExpected := EncodeDate(2001,2,18) + EncodeTime(4,5,6,789);
  Sut.IncTime(789, tpMillisecond);
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

initialization

  NewSet('Time Provider');
  PrepareSet(Setup);
  AddTestCase('ChangeTime sets correct time', ChangeTimeSetsCorrectTime);
  AddTestCase('Fixed Time stays fixed', FixedTimeStaysFixed);
  AddTestCase('Fixed Time increases by expected amount', IncreasesByExpectedAmount);

  FinaliseSet(TearDown);


end.
