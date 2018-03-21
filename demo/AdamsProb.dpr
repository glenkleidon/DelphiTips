program AdamsProb;

{$APPTYPE CONSOLE}
  {$IFDEF VER210} {$DEFINE PREXE2} {$ENDIF}
  {$IFDEF VER220} {$DEFINE PREXE2} {$ENDIF}

{$R *.res}

uses
  {$IFDEF PREXE2}
    {$MESSAGE FATAL 'Will not compile on this version of Delphi'}
  {$ELSE}
    System.SysUtils,
    System.classes,
    System.Generics.Collections,
  {$ENDIF}
  RecordUtils in '..\RecordUtils.pas',
  AdamsSerializer in 'AdamsSerializer.pas';

  var
    lUserSet : TUserSet;
    MyUserSet : TSUserSet;
    fIUserData : IDataEntity;

    lCustomerSet : TCustomerSet;
    MyCustomerSet : TSCustomerSet;
    fICustomerData : IDataEntity;
    fCustomerData : ICustomerSetSerializer;
    cCustomerData : TCustomerSetSerializerObject;

    fMySets : TMySets;

begin
  try
    System.ReportMemoryLeaksOnShutdown := true;
    TStringlist.create;

    Writeln('Creat the Serializable record directly');
    // User Set
    lUserSet.EntityName := 'UserData';
    lUserSet.RequiredRecord := [udId,udFirstname];
    MyUserSet := lUserSet;

    writeln(MyUserSet.AsJSON);

    // Customer Set
    lCustomerSet.EntityName := 'CustomerData';
    lCustomerSet.RequiredRecord := [cdId,cdName];
    MyCustomerSet := lCustomerSet;

    writeln(MyCustomerSet.AsJSON);

    Writeln('Now use a based Object IASJson Operators');
    /// Technique 1: use a reference to the existing userset
    fIUserData := TUserSetSerializerObject.NewAsReference(MyUserSet);
    writeln(fIUserData.ASJson);

    // Technique 2:
    cCustomerData := TCustomerSetSerializerObject.Create;
  {$IFDEF VER230}
    fCustomerData := ICustomerSetSerializer(cCustomerdata);
    FICustomerData := IDataEntity(cCustomerdata);
  {$ELSE}
      fCustomerData := cCustomerdata as ICustomerSetSerializer;
      FICustomerData := cCustomerdata as IDataEntity;
  {$ENDIF}
    fCustomerData.Serializer.Add(lCustomerSet);
    Writeln('From Serializer interface:',fCustomerData.Serializer.AsJSON);
    Writeln('From IasJSON interface:',fICustomerData.ASJson);


    Writeln('Finally as a array of Identity');
    fMySets := TMysets.Create;
    try
      fMySets.Add(fIUserData);
      fMySets.Add(fICustomerData);

      writeln(fMySets.AsJson);
    finally
      freeandnil(fMySets);
    end;

    readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
