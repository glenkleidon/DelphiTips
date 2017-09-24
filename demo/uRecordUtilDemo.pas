unit uRecordUtilDemo;

interface
  uses RecordUtils;

  CONST
    CRLF=#13#10;

  Type
  // Here is my standard record - but IT CAN BE ANY RECORD you previously
  // created - NOTE: Only shallow support is implemented as yet.
  TMyRecordStatus = (mrsNone, mrsBusy, mrsIdle);
  TMyRecord=Record
    id : integer;
    Name: string;
    isNew : boolean;
    Status : TMyRecordStatus; // enum
  end;

  // To Enable all of the Majik, just declare a new generic type and use that
  // It has 2 Important Properties : Values (record of the assigned type)
  //                                 AllValues (Array of records of the type)
  TMyRecordWithUtils = TRecordSerializer<TMyRecord>;



Procedure TestAssign(var AMyRecord: TMyRecord);

implementation

Procedure TestAssign(var AMyRecord: TMyRecord);
var lRecord: TMyRecord;
begin
  lRecord.id := -1;
  lRecord.Name := 'From Test Assign';
  AMyRecord := lRecord;
end;

end.
