unit TimeProvider;

interface
uses
  SysUtils;

type
  TTimeProviderSpan = (tpMillisecond, tpSecond, tpMinute, tpHour, tpDay,
      tpWeek, tpMonth, tpYear);

  ITimeProvider = interface
     function Now: TDateTime;
     function GetIsFixed: Boolean;
     property IsFixed: Boolean read GetIsFixed;
     procedure ChangeTime(ANewTime: TDateTime; AFixed: Boolean=false);
     procedure IncTime(AAmount: Integer; ATimeframe:TTimeProviderSpan);
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
     property IsFixed: Boolean read GetIsFixed;
  end;

  TTimeProvider=TCustomTimeProvider;



implementation
uses DateUtils;

{ TTimeProvider }

procedure TCustomTimeProvider.ChangeTime(ANewTime: TDateTime; AFixed: Boolean);
begin
  FCreateTime := Now;
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
     tpMillisecond: ChangeTime(IncMilliSecond(self.Now, AAmount), IsFixed);
     tpSecond: ChangeTime(IncSecond(self.Now, AAmount), IsFixed);
     tpMinute: ChangeTime(IncMinute(self.Now, AAmount), IsFixed);
     tpHour: ChangeTime(IncHour(self.Now, AAmount), IsFixed);
     tpDay: ChangeTime(IncDay(self.Now, AAmount), IsFixed);
     tpWeek: ChangeTime(IncWeek(self.Now, AAmount), IsFixed);
     tpMonth: ChangeTime(IncMonth(self.Now, AAmount), IsFixed);
     tpYear: ChangeTime(IncYear(self.Now, AAmount), IsFixed);
   end;
end;

function TCustomTimeProvider.Now: TDateTime;
begin
   if FIsFixed then
     result := FCreateTime-FOffset
   else
     result := (SysUtils.Now-FOffSet);
end;

end.
 