# DelphiTips
Useful Delphi Units which provide full functionality in a single PAS file.

## DUnitm - Mini Test Framework
The [Mini Test framework](https://github.com/glenkleidon/DelphiTips/wiki/Mini-Test-Framework) is a light-weight unit testing framework which simply requires that you
include the single MiniTestFrameWork.pas in a console app, and start testing your code.
Check out the [Blog here](https://glenkleidon.blogspot.com.au/2017/07/new-mini-test-framework-for-delphi.html)

### Added Features: 
  + Test Run support
  + Set level Skipping
  + Set level Expected Exceptions
  + Project Templates
  
See the updated wiki!

### Updated Support
Now Supports All versions of Delphi back to Delphi 7 (and probably 4).  This has been tested on:

|Version  |Version     |Version    |Version    |Version     |       |
|---------|-----------:|:---------:|:---------:|:----------:|:------|
|4 &#9744;|2005 &#9744;|XE  &#9745;|XE6 &#9744;|10.0 Seattle|&#9744;|
|5 &#9744;|2006 &#9744;|XE2 &#9745;|XE7 &#9744;|10.1 Berlin |&#9745;|
|6 &#9744;|2007 &#9745;|XE3 &#9744;|XE8 &#9744;|10.2 Tokyo  |&#9744;|
|7 &#9745;|2009 &#9745;|XE4 &#9744;|           |            |       |
|8 &#9744;|2010 &#9745;|XE5 &#9745;|           |            |       |

If using versions of Delphi 2005 or 2006, You may have trouble with the 
2007 DPROJ file in the Test Unit and the Project Template folders.  If you 
do have trouble, simply remove the DPROJ file and get Delphi to rebuild from the
DPR file.

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