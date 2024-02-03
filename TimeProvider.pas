unit TimeProvider;

interface
uses
  SysUtils;

type
  TTimeProviderSpan = (tpMilliseconds, tpSeconds, tpMinutes, tpHours, tpDays,
      tpWeeks, tpMonths, tpYears);

  ITimeProvider = interface
     function Now: TDateTime;
     function GetIsFixed: Boolean;
     property IsFixed: Boolean read GetIsFixed;
     procedure ChangeTime(ANewTime: TDateTime; AFixed: Boolean=false);
     procedure IncTime(AAmount: Integer; ATimeframe:TTimeProviderSpan);
     function TimeIn(AAmount: Integer; ATimeframe: TTimeProviderSpan): TDateTime;
     //TO DO...
     //property TimeZone: Integer read GetTimeZone write SetTimeZone;
     //property DaylightSavingHrs: single read GetDaylightSavingHrs write SetDaylightSavingHrs;
  end;

  TCustomTimeProvider = class(TInterfacedObject, ITimeProvider)
  private
     FOffset : Extended;
     FCreateTime: TDateTime;
     FIsFixed : Boolean;
    function GetIsFixed: Boolean;
  public
     constructor Create; overload;
     constructor Create(ANow:TDateTime; AFixed: Boolean=false); overload;
     // Interface Methods.
     function Now: TDateTime;
     procedure ChangeTime(ANewTime: TDateTime; AFixed: Boolean=false);
     procedure IncTime(AAmount: Integer; ATimeframe:TTimeProviderSpan);
     function TimeIn(AAmount: Integer; ATimeframe: TTimeProviderSpan): TDateTime;
     property IsFixed: Boolean read GetIsFixed;
  end;

  TTimeProvider=TCustomTimeProvider;



implementation
uses DateUtils;

{ TTimeProvider }

procedure TCustomTimeProvider.ChangeTime(ANewTime: TDateTime; AFixed: Boolean);
begin
  FCreateTime := SysUtils.Now;
  FOffSet :=  FCreateTime-ANewTime;
  FIsFixed := AFixed;
end;

constructor TCustomTimeProvider.Create(ANow: TDateTime; AFixed: Boolean);
begin
  ChangeTime(ANow, AFixed);
end;

constructor TCustomTimeProvider.Create;
begin
  FCreateTime := now;
  FOffSet := 0;
  FIsFixed := false;
end;

function TCustomTimeProvider.GetIsFixed: Boolean;
begin
  result := FIsFixed;
end;

procedure TCustomTimeProvider.IncTime(AAmount: Integer;
  ATimeframe: TTimeProviderSpan);
begin
   case ATimeframe of
     tpMilliseconds: ChangeTime(IncMilliSecond(self.Now, AAmount), IsFixed);
     tpSeconds: ChangeTime(IncSecond(self.Now, AAmount), IsFixed);
     tpMinutes: ChangeTime(IncMinute(self.Now, AAmount), IsFixed);
     tpHours: ChangeTime(IncHour(self.Now, AAmount), IsFixed);
     tpDays: ChangeTime(IncDay(self.Now, AAmount), IsFixed);
     tpWeeks: ChangeTime(IncWeek(self.Now, AAmount), IsFixed);
     tpMonths: ChangeTime(IncMonth(self.Now, AAmount), IsFixed);
     tpYears: ChangeTime(IncYear(self.Now, AAmount), IsFixed);
   end;
end;

function TCustomTimeProvider.Now: TDateTime;
begin
   if FIsFixed then
     result := FCreateTime-FOffset
   else
     result := (SysUtils.Now-FOffSet);
end;

function TCustomTimeProvider.TimeIn(AAmount: Integer;
  ATimeframe: TTimeProviderSpan): TDateTime;
begin
   case ATimeframe of
     tpMilliseconds: result := IncMilliSecond(self.Now, AAmount);
     tpSeconds: result := IncSecond(self.Now, AAmount);
     tpMinutes: result := IncMinute(self.Now, AAmount);
     tpHours: result := IncHour(self.Now, AAmount);
     tpDays: result := IncDay(self.Now, AAmount);
     tpWeeks: result := IncWeek(self.Now, AAmount);
     tpMonths: result := IncMonth(self.Now, AAmount);
     tpYears: result := IncYear(self.Now, AAmount);
   end;
end;

end.
