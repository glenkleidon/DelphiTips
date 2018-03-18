unit AdamsSerializer;

interface
  uses RecordUtils, system.Generics.collections;

Type

  TUserDataPoint = (udId, udFirstName, udLastname, udEmail, udMobile);
  TCustomerDataPoint = (cdId, cdName, cdABN, cdAddress);

  TBaseSet<T> = Record
  private
    fRequiredData: T;
    fEntityName: String;
  public
     property RequiredRecord : T read fRequiredData write fRequiredData;
     property EntityName: String read fEntityName Write fEntityName;
  End;

  TUserData = set of TUserDataPoint;
  TUserSet = TBaseSet<TUserData>;
  PUserSetSerializer = ^TSUserSet;
  TSUserSet = TRecordSerializer<TUserSet>;

  TCustomerData=set of TCustomerDataPoint;
  TCustomerSet = TBaseSet<TCustomerData>;
  PCustomerSetSerializer = ^TSCustomerSet;
  TSCustomerSet = TRecordSerializer<TCustomerSet>;

  // Now for 99% of things you only need the above.
  // To create Type Safe, Interfaced objects of any type of serializer
  // and we need 3 interfaces.
  // A bit more planning is required (you could do it easily with RTTI, but
  // that is for another day
  // We want an interface that exposes the serializer's AsJson
  // We can use the ObjectWrapper for this including the interface we want.
  // and to ensure we have type safety, we can ensure Add an extra interface
  // definition to be sure.

   // ASJSON
   IDataEntity = Interface
      function ASJson: String;
      procedure FromJson(AJSON: String);
   End;

   TMySets = Class(TList<IDataEntity>)
      public
        Function AsJson: string;
        procedure FromJson(AJSON: String);
   end;

  // Need an interface for each to ensure type safety (limitation of using records)
   IUserSetSerializer = interface
      Function Serializer :  PUserSetSerializer;
   end;
   // Implemetation of the interface descending the standard SerializerObject
   TUserSetSerializerObject = class(TRecordSerializerObject<TSUserSet>,IUserSetSerializer,IDataEntity)
      public
       Function Serializer : PUserSetSerializer;
       Function ASJson: String;
       Procedure FromJson(AJSON: String);
       Constructor NewAsReference(const ASet : TSUserSet);
  end;

  // Same for CustomerData
  ICustomerSetSerializer = interface
      Function Serializer :  PCustomerSetSerializer;
  end;
  // need to implement this interface by descending the
  TCustomerSetSerializerObject = class(TRecordSerializerObject<TSCustomerSet>,ICustomerSetSerializer,IDataEntity)
      public
       Function Serializer : PCustomerSetSerializer;
       Procedure FromJson(AJSON: String);
       Function ASJson: String;
       Constructor NewAsReference(const ASet : TSCustomerSet);
  end;

implementation
  uses windows;

{ TCustomerSetSerializerObject }

function TCustomerSetSerializerObject.ASJson: String;
begin
  result := self.Serializer.AsJSON;
end;

procedure TCustomerSetSerializerObject.FromJson(AJSON: String);
begin
  self.Serializer.FromJSON(AJSON);
end;

constructor TCustomerSetSerializerObject.NewAsReference(const ASet: TSCustomerSet);
begin
  self.fReference := @ASet;
end;

function TCustomerSetSerializerObject.Serializer: PCustomerSetSerializer;
begin
  // ensure type safety of the pointer.
    Result := PValue;
end;

{ TUserSetSerializerObject }

function TUserSetSerializerObject.ASJson: String;
begin
   result := self.Serializer.AsJSON;
end;


procedure TUserSetSerializerObject.FromJson(AJSON: String);
begin
   self.Serializer.FromJSON(AJSON);
end;

constructor TUserSetSerializerObject.NewAsReference(const ASet: TSUserSet);
begin
   {$message warn 'When creating a TUserSetSerializerObject, ensure that the scope for the referenced TSUserSet Record is more general than the object'};
   self.fReference := @ASet;
end;

function TUserSetSerializerObject.Serializer: PUserSetSerializer;
begin
   result := self.PValue;
end;

{ TMySets }

function TMySets.AsJson: string;
var i : IDataEntity;
    lSeparator: string;
begin
  result := '[';
  lSeparator := '';
  for i in self do
  begin
     result := result + lSeparator + i.ASJson;
     lSeparator := ',';
  end;
  result := Result + ']';
end;

procedure TMySets.FromJson(AJSON: String);
begin
  // not implemented.
end;

end.
