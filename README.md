# DelphiTips
Useful Delphi Units which provide full functionality in a single PAS file.

There are currently 3 units available:
  + DUnitm - A test framework
  + MultiPageImage - Support for Multipage Tif in Standard Delphi TImage.
  + Record Utils - JSON, XML, Value-Pair Type serializer/deserializer for Record types

## DUnitm - Mini Test Framework
The [Mini Test framework](https://github.com/glenkleidon/DelphiTips/wiki/DUnitm---Mini-Test-Framework) is a light-weight unit testing framework which simply requires that you
include the single MiniTestFrameWork.pas in a console app, and start testing your code.
Check out the [Blog here](https://glenkleidon.blogspot.com.au/2017/07/new-mini-test-framework-for-delphi.html) and the [YouTube](https://www.youtube.com/playlist?list=PL42y13vA83auEzqLTzmkwnQuC6pyOxlQE) playlist.

### Added Features: Release 1 
  + Test Run support
  + Set level Skipping
  + Set level Expected Exceptions
  + Project Templates
### Added Features: Release 2
  + Refactoring to correct issues with naming conventions
  + Deprecated functions using incorrect previous naming convention
  + Case level skipping stops cases from actually being evaluated. **
### Breaking changes Release 1 to 2
The _Skip_ parameter type in AddTestCase and Assertions has been changed from _boolean_ to a custom Enum _TSkipType_.  This was required to support case level skipping.  Using the Frameworks _**SKIP**_ or _**SKIPPED**_ constants will continue to work as normal, however this is a breaking change for existing cases where the _Skip_ parameter has been implemented using a BOOLEAN expression.  You need to change to _skipTrue_ or _skipFalse_ in this case.

### Added Features: Release 3
  + Deferred test cases
  + Powerful Difference viewer functions
  
#### Bugfixes and Features
  + Fixed bug for memory leak in the difference viewer 
  + Added support for Red/Green Colour blindness (Changes default colours from Red/Green) 
  + (Added GITFlow branches)

### Added Features: Release 5
  + Test Metadata exportable with the /m switch
  + Running tests by Unique Identifier
  
### Fixes and Features version 5.1
  + Added _CheckIsCloseTo_ Assertions
  + fixed set display issues
  + Added Set level "Ignore" to skip entire Set without executing it.


See the updated wiki!

### Updated Support
Now Supports All versions of Delphi back to Delphi 7 (and probably 4).  This has been tested on:

|Version  |Version     |Version    |Version    |Version     |       |
|---------|-----------:|:---------:|:---------:|:----------:|:------|
|4 &#9744;|2005 &#9744;|XE  &#9745;|XE6 &#9744;|10.0 Seattle|&#9744;|
|5 &#9745;|2006 &#9744;|XE2 &#9745;|XE7 &#9744;|10.1 Berlin |&#9745;|
|6 &#9744;|2007 &#9745;|XE3 &#9744;|XE8 &#9744;|10.2 Tokyo  |&#9745;|
|7 &#9745;|2009 &#9745;|XE4 &#9744;|           |10.3 Rio    |&#9745;|
|8 &#9744;|2010 &#9745;|XE5 &#9745;|           |11.0 Sydney |&#9745;|

If using versions of Delphi 2005 or 2006, You may have trouble with the 
2007 DPROJ file in the Test Unit and the Project Template folders.  If you 
do have trouble, simply remove the DPROJ file and get Delphi to rebuild from the
DPR file.

## MulitPageImage
This unit [MultiPageImage.pas](https://github.com/glenkleidon/DelphiTips/wiki/MultiPageImage---Helper-Methods-to-Add-Multi-Page-support-to-TImage-Component) supports multipage TIF in the standard VCL TImage by simply adding the unit to the project.  This is in the same way as you add JPEG, GIF and PNG support.  The Unit has helper methods for TImage and TPicture which provides TIF multi page support including:
  + PageCount (read only)
  + PageNumber (read or set)
  + LoadFromFile(Filename, PageNo)
  + LoadFromStream(Stream, PageNo)

NOTE: This is MOSTLY not new work: it simply moves a copy of the TWICImage class (from VCL.Graphics) to its own unit and adds the few lines of code required to support multiple pages, and implements the helpers for TImage and TPicture to use the feature.

WARNING! SaveToFile still only has single file support as of September 2018.


### Supported Versions
Graphics.WICMultipage supports all versions of Delphi back to Delphi 2010.  Delphi 2009 also works, but there are a number of minor issues and is dependent on a copy of Delphi 2010 Wincodec unit with some modification. So 2009 is not recommended.  This unit has has been tested on:

|Version  |Version     |Version    |Version    |Version     |       |
|---------|-----------:|:---------:|:---------:|:----------:|:------|
|4 &#9744;|2005 &#9744;|XE  &#9745;|XE6 &#9744;|10.0 Seattle|&#9744;|
|5 &#9744;|2006 &#9744;|XE2 &#9745;|XE7 &#9744;|10.1 Berlin |&#9745;|
|6 &#9744;|2007 &#9746;|XE3 &#9744;|XE8 &#9744;|10.2 Tokyo  |&#9745;|
|7 &#9744;|2009 &#9745;|XE4 &#9744;|Delpi 2010 &#9745;|            |       |
|8 &#9744;|2010 &#9745;|XE5 &#9745;|           |            |       |


## Record Utils - Utils for cloning, clearing, serialising/Deserialising Pascal Record Types

Pascal Records are extremely useful for working with the Parallel Type Library because they are automatically memory managed.  However  not not being classes, they are more difficult to automatically
populate from Streamed Data without using published properties.

The RecordUtils.pas file uses RTTI to automatically serialise and deserialse as value pairs. eg
```
TMyRecordStatus = (mrsNone, mrsBusy, mrsIdle); 
TMyRecord=Record
  id : integer;
  Name: string;
  isNew : boolean;
  Status : TMyRecordStatus; // enum
end;
```

WIll be serialised as 
```
id=1
Name=Test1
isNew=True
Status=mrsBusy
```
### Supported Types
Currently only the Data types Integer, String, Boolean and any Enumerated type are supported. _NESTED Records_ is not supported at this time. 

### Support for Arrays
Yes, arrays are supported.  The Above example as an array would be output as:
```
id[0]=1
Name[0]=Test1
isNew[0]=True
Status[0]=mrsBusy

id[1]=2
Name[1]=Test2
isNew[1]=false
Status[1]=mrsNone
```
The Data Type and Array count can optionally be included in the data in the following way:

_Single Record_
```
RecordType=TMyRecord
```
_Array of Records_
```
RecordType=TMyRecord[]
RecordCount=2
```
The alternative format where each id is prefixed with the data type is also supported for Parsing (but will not be output in this format). Eg

_Single Record_
```
TMyRecord.id=1
TMyRecord.Name=Test1
TMyRecord.isNew=True
TMyRecord.Status=mrsBusy
```
_Array of Records_
```
TMyRecord[0].id=1
TMyRecord[0].Name=Test1
TMyRecord[0].isNew=True
TMyRecord[0].Status=mrsBusy

TMyRecord[1].id=1
TMyRecord[1].Name=Test1
TMyRecord[1].isNew=True
TMyRecord[1].Status=mrsBusy
```


### Line Endings
Internally the string structure is managed as a TStringlist, so the line ending symantics are a consequence of that architecture. The  

The \<CR\> (Carriage Return, ASCII-13) cannot be represented as a single line in the TStringlist (by default) and consequently are always converted to \<LF\> (Line Feed, ASCII-10).  So \<CRLF\> becomes \<LF\>, \<CR\> becomes \<LF\>, \<CRLF\>\<LF\> becomes \<LF\>\<LF\>.  At this stage, to preserve \<CR\>, you must pre-process the string representation using an alternate encoding scheme (eg HTML encoding )

## Time Provider
In modern development, Unit testing is a critical part of the development cycle.  In order that you can control what the system time 
the ITimeProvider interface allows you to Mock the System time without needing to change the date or time of the system.
Supported on all versions of Delphi back to version 5 (and probably 4).

The Time Provider class TTimeProvider implements this interface:
```
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

```

At this stage, `TimeZone` support is not included in the provider although that is an important feature for testing (look out for updates in future releases)


### How to use ITimeProvider
In your Class or Unit reference, pass a reference to an initialized ITimeProvider.  By default set this up so that `Now=System.Now`. 
When testing, you can pass in alternative time providers where the time:
  + flows normally starting from a fixed date and time OR
  + is a constant time.
  
Once initialized, the time provider can be paused at a specific time, restarted from another time or shifted forward or backward by increments.

### Initialize your Provider (ideally from a DI container) 
Typically you would use the TimeProvider in your application by passing it into the constructor.  For your typical application, the 
provider will simply report the normal System time.  For your test cases, you have control over what time the class thinks
the system time is.

```
TMyClass = Class
private 
  FTimeProvider: ITimeProvider;
  function GetTimeProvider: ITimeProvider;
protected
  {$IFNDEF TESTING}
  property TimeProvider: ITimeprovider read GetTimeProvider;
  {$ENDIF}
public
  Constructor Create(ATimeProvider: ITimeProvider=nil);
  function PersonsAge(DOB: TDateTime): Integer;
  {$IFDEF TESTING}
  property TimeProvider: ITimeprovider read GetTimeProvider;
  {$ENDIF}
end;
```

The advantage is that now your class is unit testable and wont be dependent on when (or in the future what timezone) the 
test is run.  Also in your application, the timeprovider is a protected member (or private if you'd rather) and only for testcases
will the TimeProvider be public.

You can simply access the "time" using `TimeProvider.Now` instead of `SysUtils.Now` eg
```
  function TMYClass.PersionsAge(DOB: TDateTime): integer;
  begin
      // fairly bad implementation of Age in years
      result := Integer(trunc( (TimeProvider.Now-DOB)/365.25));
  end;

```

### Constructors
Initialising the way the Time provider behaves Via the Constructors is as follows:
```
  var NormalTime: ITimeProvider := TTimeProvider.Create;
  var TimeStartsAtY2k: ITimeProvider := TTimeProvider.Create(EncodeDate(2000,1,1));
  var TimeIsAlwaysJanTwo2010: ITimeProvider := TTimeProvider.Create(EncodeDate(2010,1,2), True {fixed});
```
_NormalTime_ will operate the same as SysUtils.Now;
_TimeStartsAtY2k_ will work as if the Application (or method call etc) was started on Jan 1st 2000 at midnight.
_TimeIsAlwaysJanTwo2010_ will force the time provider to always report the current time as Jan 2nd 2010 when ever it is called.

### Skipping or reversing time 
This is a powerful way to doing testing.  For example, if you have a time based function, you can test this by following the process here.

```
var Sut: IMyClass;
begin
  // uses the default timeprovider to start with
  Sut := TMyClass.Create(TTimeProvider.Create); 
  
  // Change to specifc date/time
  Sut.TimeProvider.ChangeTime(EncodeDate(2001, 1, 2));
  
  // Arrange your test data
  var OneYearOldPersonDOB := EncodeDate(2000, 1, 1);
  
  NewTest('One year old person should be 1 year old!'); 
  CheckIsEqual(1, Sut.PersonsAge(OneYearOldPersonDOB));
  
  NewTest('In 2 years time, the person will be 3 years old');
  Sut.TimeProvider.IncTime(2, tpYears);  // move time by 2 years.
  CheckIsEqual(3, Sut.PersonsAge(OneYearOldPersonDOB));
end;
```

### Keeping track of another TimeZone
While not fully supported as yet, you can use the TimeProvider to track the time in another timezone.

```
  var MelbourneTime:ITimeProvider := TTimeProvider.Create;
  // Set up Adelaide time which is -30 minutes...
  var AdelaideTime:ITimeProvider := TTimeProvider.Create(
       MelbourneTime.TimeIn(-30,tpMinutes));
  
  ...
  Format('TIME: Melbourne %s   Adelaide: %s', [ 
    FormatDateTime('HH:NN:SS', MelbourneTime.Now),
    FormatDateTime('HH:NN:SS', AdelaideTime.Now),
	]);
```

