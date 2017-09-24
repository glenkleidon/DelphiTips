program RecordUtilsDemo;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  RecordUtils in '..\RecordUtils.pas',
  uRecordUtilDemo in 'uRecordUtilDemo.pas';

  // No create some variables.
  var
  MyRecord : TMyRecord;
  MyRecordWithUtils, MyRecordWithUtils2 : TMyRecordWithUtils;

 

begin
  try
    // 1. Initalise the Default Value.  As you know, the INT and Bool will
    //    be unitialised - lets init them.
    MyRecordWithUtils.Clear;  // DONE!

    // 2. Copy the contents of one Record to another
    //
    // Assign some Value.
    MyRecordWithUtils.Value.Name := 'From Record 1';
    MyRecordWithUtils2.Value.Name := '.......';
    Writeln('Setting Value 1 and 2 separately');
    writeln('1: "',MyRecordWithUtils.Value.Name, '"; 2: "',MyRecordWithUtils2.Value.Name,'"',CRLF);

    // Copy the values (by assignment)
    Writeln(#13#10'Assign Value 2 from Value 1');
    MyRecordWithUtils2 := MyRecordWithUtils;
    writeln('1: "',MyRecordWithUtils.Value.Name, '"; 2: "',MyRecordWithUtils2.Value.Name,'"',CRLF);

    // Now prove they are still different records
    Writeln('Value 2 is a NOT a pointer to Value 1');
    MyRecordWithUtils.Value.id := 1;
    MyRecordWithUtils2.Value.id := 2;
    writeln('1: id is "',MyRecordWithUtils.Value.id, '"; 2: id is "',MyRecordWithUtils2.Value.id,'"',CRLF);

    // Use with a static record
    Writeln('You can still use a pointer though, by re-assigning the VALUES property');
    MyRecordWithUtils2.Value := MyRecord;
    MyRecord.id := 99;
    MyRecord.Name := 'Static Record';
    writeln('2: "',MyRecordWithUtils2.Value.Name,'", ID:',MyRecordWithUtils2.Value.id,
            '"; Static: "',MyRecord.Name,'", ID:',MyRecord.id,'"',CRLF);
    ReadLn;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
