unit RecordUtils;

////////////////////////////////////////////////
// Copyright Glen Kleidon 2017                //
// Part of the Delphi Tips Repo               //
// https://github.com/glenkleidon/DelphiTips  //
////////////////////////////////////////////////

interface
   uses sysUtils, Classes, Rtti, Generics.collections;
   type PStrings = ^TStrings;
   Type TSerializerParseMode = (spmAppend, spmUpdate);
   Type TJSONState = (jsNone, jsInArray, jsInObject, jsInName, jsEndName,
                       jsInQuotedValue, jsInValue, jsInEscape, jsEndValue, jsEndObject, jsEndArray, jsNextElement);
   Type TArrayBounds = record
          Low : integer;
          High : integer;
   end;

   Type TRecordSerializer<T> = Record
     private
       fisArray : string;
       fIndex: integer;
       function getCount: integer;
       function getIsArray: boolean;
       procedure setIsArray(const Value: boolean);
     public
       Values : T;
       AllValues: Array of T;
       Procedure Clear;
       Procedure Clone`(ARecord: T); overload;
       Procedure Clone(ARecord: TRecordSerializer<T>); overload;
       Function Parse(AStrings:TStrings; AMode: TSerializerParseMode=spmUpdate;
                             AIndex: integer=-1): integer; overload;
       Function Parse(AText: String; AMode: TSerializerParseMode = spmUpdate;
                             AIndex: integer=-1): integer; overload;
       Function AsValuePairs(AIndex:integer =-1): string; overload;
       Procedure AsValuePairs(AStrings:TStrings; AIndex:integer =-1); overload;
       Function AsJSON : String; overload;
       Procedure FromJSON(AJSON: string);
       class operator Implicit(ASerializable: TRecordSerializer<T>): T;
       class operator Implicit(ARecord: T): TRecordSerializer<T>;
       class operator Implicit(AText: string): TRecordSerializer<T>;
       class operator Implicit(ARecord: TRecordSerializer<T>): String;
       Property Count : integer read getCount;
       Property isArray: boolean read getIsArray write setIsArray;
   end;

   function IndexedName(AId: string; AIndex: integer=-1): string;
   function ParseStringAsIndex(AId: string; AStrings: TStrings; AIndex: integer=-1): string;
   function AsValuePair(AId: string; AValue: string; AIndex: integer=-1; ALineEnding: string=''):string;
   function AsValuePairRow(AId: string; AValue: string; AIndex: integer=-1): string;
   Function RecordAsValuePairs(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
   Function ParseValuesToRecord(AStrings: TStrings; ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): integer;
   Function GetValuePairHeader(ATypeInfo: Pointer; APValue: Pointer): string;
   Procedure CloneRecord(ATypeInfo: Pointer; APRecordToClone: Pointer; APClonedRecord : Pointer );
   Procedure ClearRecord(ATypeInfo: Pointer; APValue: Pointer);
   Function AsJSONPair(AId: string; AValue: string;AValueQuoted:boolean=true):string;
   Function RecordAsJSON(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
   Procedure ParseJSONtoRecord(AString: String; ATypeInfo: Pointer; APValue: Pointer);
   Function JSONToValuePairs(AJSON:string):String;
   Function GetIndexBounds(AStrings: TStrings): TArrayBounds;


implementation

  uses TypInfo,StrUtils;

Function GetIndexBounds(AStrings: TStrings): TArrayBounds;
var p,q,r: integer;
    v,c : integer;
    lText, lRow: string;
begin
  Result.Low := MaxInt;
  Result.High := -1;
  p := pos('[',AStrings.Text);
  try
    if p<1 then exit;
    for lRow in AStrings do
    begin
      lText := copy(lRow,1,pos('=',lRow)-1);
      p := pos('[',lText);
      q := posex(']',lText,p);
      if (p>0) and (q>0) then
      begin
        // 123456789
        // id[0]   5-3-1 = 1
        val(copy(ltext,p+1,q-p-1),v,c);
        if c>0 then continue;
        if v<result.Low then Result.Low:=v;
        if v>Result.High then Result.High:=v;
      end else exit;
    end;
  finally
    if Result.Low=MaxInt then Result.Low := Result.High;
  end;
end;

Function JSONToValuePairs(AJSON:string):String;
var c : char;
    l,i,index : integer;
    lValue, lName, lRow : string;
    lArrayIndex : TStack<Integer>;
    lStates: TStack<TJSONState>;
    lState : TJSONState;
    lList : TStringlist;
    Procedure JSONParseError(AChar: Char);
    begin
      raise Exception.Create(format('JSON Parse Error %s at %u',[string(AChar), i]));
    end;
    Procedure Reset;
    begin
      lName := '';
      lValue := '';
    end;
    Procedure EndValue;
    var llIndex: string;
    begin
      if index>=0 then llIndex := format('[%s]',[Index]);
      // could populate directly from here!!!!
      lRow := format('%s%s=%s',[lName,llIndex,lValue]);
      lList.Add(lRow);
    end;
begin
   // simple implementation assumes single object, no spacing
   lArrayIndex := TStack<Integer>.Create;
   lStates:= TStack<TJSONState>.Create;
   lList := TStringlist.Create;
   Try
   lStates.Push(TJSONState.jsNone);
   l := length(AJSON);
   for i := 1 to l do
   begin
     c := AJSON[i];
     lState := lStates.peek;
     if (c<>'"') then
     case lState of
        jsInQuotedValue,jsInName :
          begin
            if c='\' then
               lStates.push(jsInEscape)
            else lValue := lValue + c;
            continue;
          end;
        jsInEscape :
          begin
            lValue := lValue + c;
            lStates.Pop;
          end;
     end;
     case c of
       '{':
           case lState of
            jsNone,
            jsNextElement,
            jsInArray:
             begin
               Reset;
               lStates.Push(TjsonState.jsInObject);
             end;
            jsInObject, jsInName,
            jsEndName, jsInValue,
            jsEndObject, jsEndArray: JSONParseError(c);
           end;
       '}':
           case lState of
            jsNone:;
            jsInArray:;
            jsInObject:;
            jsInName:;
            jsEndName:;
            jsInValue:;
            jsEndValue:;
            jsEndObject:;
            jsEndArray:;
            jsNextElement:;
           end;

       '[':
           case lState of
            jsInValue
             begin

             end;
            jsNone, jsInArray,  jsNextElement:
             begin
                lArrayIndex.Push(Index);
                Index := 0;
             end;
            else JSONParseError(c);
           end;
       ']':
           case lState of
            jsEndValue, jsEndObject, jsEndArray:
             begin
                lStates.Pop;
                if lState<>TJSONState.jsInArray then JSONParseError(c);

                Index := -1;
             end;
            else JSONParseError(c);
           end;
       '"':
           case lState of
            jsNone,
            jsInArray,
            jsInObject,
            jsNextElement:
             begin
               Reset;
               lStates.Pop;
               lState := TJSONState.jsInName;
               lStates.Push(lState);
             end;
            jsEndName:
              begin
               lStates.Pop;
               lState := TJSONState.jsInQuotedValue;
               lStates.Push(lState);
              end;
            jsInName:
              begin
               lName := lValue;
               lValue := '';
               lStates.Pop;
               lState := TJSONState.jsEndName;
               lStates.Push(lState);
              end;
            jsInValue:
              begin
               lName := lValue;
               lValue := '';
               lStates.Pop;
               lState := TJSONState.jsEndValue;
               lStates.Push(lState);
              end;
           else JSONParseError(c);
           end;
       ',':
           case lState of
            jsEndValue,
            jsEndArray,
            jsEndObject:
              begin
                lStates.Pop;
                lState := lStates.Peek;
                if lState <> jsInArray then JSONParseError(c);
                lStates.Push(TJSONState.jsNextElement);
                inc(Index);
              end;
            else JSONParseError(c);
           end;
       ':':
           case lState of
            jsInName:
            begin
              lStates.Pop;
              lState := jsEndName;
              lStates.Push(lState);
            end;

            jsInValue, jsInValue
            jsEndName:
            begin
            end;
            jsNone:;
            jsInArray:;
            jsInObject:;
            jsEndValue:;
            jsEndObject:;
            jsEndArray:;
            jsNextElement:;
           end;
     end;
     if State in [jsInValue] then lvalue = lValue + c;
   end;
   finally
     freeandnil(lList);
     freeandnil(lArrayIndex);
     freeandnil(lStates);
   end;


   Result := AJSON;
   result := StringReplace(Result,'{','',[]);
   result := StringReplace(Result,'\"',#1,[rfReplaceAll]);
   result := StringReplace(Result,'\\',#2,[rfReplaceAll]);
   result := StringReplace(Result,'"','',[rfReplaceAll]);
   result := StringReplace(Result,':','=',[rfReplaceAll]);
   result := StringReplace(Result,',',#13#10,[rfReplaceAll]);
   result := StringReplace(Result,#1,'"',[rfReplaceAll]);
   result := StringReplace(Result,#2,'\',[rfReplaceAll]);
   l := length(result)-1;
   Result := copy(Result,1,l);
end;

function IndexedName(AId: string; AIndex: integer=-1): string;
begin
 if (Aindex<0) then Result := AId else Result := format('%s[%u]',[AId, AIndex]);
end;

Function ParseValuesToRecord(AStrings: TStrings; ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): integer;
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lContext: TRTTIContext;
  lText : TStringlist;
  lPrefix : string;
  lSuffix : String;
  lTValue,lTDefaultValue : TValue;
  lValue : string;
  lInt: Int64;
  lSingle: Single;
  lDouble: Double;
  Buffer : Pointer;
  p:integer;
begin
  Result := 0;
  lContext := TRTTIContext.Create;
  ltype := lContext.GetType(ATypeInfo);
  lFields := lType.getFields;
  lPrefix := IndexedName(ltype.Name,AIndex);
  lSuffix := format('[%u]',[AIndex]);
  for lfield in lFields do
  begin
    if lField.FieldType.IsRecord then
    begin
      // Add records here.
    end else
    begin
       lValue := AStrings.values[lField.Name];
       if length(LValue)=0 then lValue := AStrings.values[lField.Name+lSuffix];
       if length(LValue)=0 then lValue := AStrings.values[lPrefix+lField.Name];
       if length(lValue)=0 then continue;
       case lField.FieldType.TypeKind of
         tkEnumeration:
         begin
           lTValue.Make(getEnumValue(lField.FieldType.Handle,lValue),lField.FieldType.Handle,lTvalue);
           lField.setValue(APValue, lTValue);
           inc(result);
         end;
         tkInteger,
         tkInt64:
         begin
            if tryStrToInt64(lValue,lInt) then
                lField.SetValue(APValue,lInt)
            else raise Exception.Create('Invalid Integer Type Cast');
            inc(result);
         end;
         tkFloat:
         begin
            p := pos('.',lValue);
            if (p=0) or (length(lValue)-p<7) then
            begin
              if tryStrToFloat(lValue,lSingle) then
                  lField.SetValue(APValue,lSingle)
              else raise Exception.Create('Invalid Float Single Type Cast');
            end else
            begin
              if tryStrToFloat(lValue,lDouble) then
                  lField.SetValue(APValue,lDouble)
              else raise Exception.Create('Invalid Float Double Type Cast');
            end;
            inc(result);
         end;
       else
       begin
          lField.setValue(APValue, TValue(lValue));
          Inc(Result);
       end;
       end;
    end;
  end;
end;

Function RecordAsValuePairs(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lText : TStringlist;
  lPrefix : string;
  lValue : string;
  lTValue : TValue;
  p:integer;
begin
  ltype := TRTTIContext.Create.GetType(ATypeInfo);
  lFields := lType.getFields;
  lText:= TStringlist.create;
  try
    lPrefix := IndexedName(ltype.Name,AIndex);
    for lfield in lFields do
    begin
      if lField.FieldType.IsRecord then
      begin
        // Add records here.
      end else
      begin
        lValue := AsValuePair(lField.Name,lField.getValue(APValue).ToString, AIndex,#13#10);
        if length(lValue)>0 then lText.Add(lValue);
      end;
    end;
    result := lText.text;
  finally
    freeAndNil(lText);
  end;
end;

Function RecordAsJSON(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lComma : string;
  lValue : string;
  lTValue : TValue;
  lNoQuotes: Boolean;
  lBoolHandle: PTypeInfo;
begin
  Result := '{';
  ltype := TRTTIContext.Create.GetType(ATypeInfo);
  lFields := lType.getFields;
  lComma := '';
  lBoolHandle := typeInfo(Boolean);
  for lfield in lFields do
  begin
    if lField.FieldType.IsRecord then
    begin
      // Add records here.
      continue;
    end;
    lValue := lField.getValue(APValue).ToString;
    if length(lValue)=0 then continue;
    lNoQuotes :=
       ((lField.FieldType.TypeKind in [tkInteger,tkFloat,tkInt64])
         or (lField.FieldType.Handle=lBoolHandle));
    lValue := lComma+AsJsonPair(lField.Name,lValue,not lNoQuotes);
    Result := Result+(lValue);
    lComma := ',';
  end;
  Result := Result + '}';
end;

Procedure ParseJSONtoRecord(AString: String; ATypeInfo: Pointer; APValue: Pointer);
begin
  raise Exception.Create('Not Implemented');
end;


function ParseStringAsIndex(AId: string; AStrings: TStrings; AIndex: integer=-1): string;
var lSuffix : string;
begin
  if AIndex=-1 then lSuffix := '' else lSuffix := format('[%u]',[AIndex]);
  result := AStrings.Values[Aid+lSuffix];
end;

function AsValuePairRow(AId: string; AValue: string; AIndex: integer=-1): string;
begin
  result :=  AsValuePair(AId, AValue, AIndex,#13#10);
end;

function AsValuePair(AId: string; AValue: string; AIndex: integer=-1; ALineEnding: string=''):string;
var lSuffix : string;
begin
  result := '';
  if length(AValue)>0 then
  begin
    if AIndex=-1 then lSuffix := '' else lSuffix := format('[%u]',[AIndex]);
    result := format('%s%s=%s',[AId,lSuffix,
            stringReplace(
              stringreplace(AValue,#13#10,#10,[rfReplaceAll]),
                #13,#10,[rfReplaceAll])]);
  end;
end;

function AsJSONPair(AId: string; AValue: string;AValueQuoted:boolean=true):string;
var lformat : string;
    function JSONEscape(Atext: string): string;
    begin
      result := StringReplace(StringReplace(atext,'\','\\',[rfReplaceAll])
                   ,'"','\"',[rfReplaceAll]);
    end;
begin
  result := '';
  if AValueQuoted then lformat := '"%s":"%s"' else
  begin
    lformat:='"%s":%s';
    AValue := lowercase(AValue);
  end;
  Result := format(lformat,[JSONEscape(AId),JSONEscape(AValue)]);
end;


Procedure ClearRecord(ATypeInfo: Pointer; APValue: Pointer);
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lPrefix : string;
  lValue : TValue;
begin
  ltype := TRTTIContext.Create.GetType(ATypeInfo);
  lFields := lType.getFields;
  for lfield in lFields do
  begin
    if lField.FieldType.IsRecord then
    begin
      // Add client records here.
    end else
    begin
      case lField.FieldType.TypeKind of
        tkInteger, tkFloat, tkInt64: lField.SetValue(APValue,0);
        tkChar,tkWChar : lField.SetValue(APValue,#0);
        tkEnumeration:
        begin
           lValue.Make(0,lField.FieldType.Handle,lValue);
           lField.setValue(APValue, lValue);
        end;
        tkString,tkLString,tkUString : lField.SetValue(APValue,'');
      end; // case;
    end;
  end;
end;

procedure CloneRecord(ATypeInfo: Pointer; APRecordToClone: Pointer; APClonedRecord : Pointer );
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lPrefix : string;
  lValue : TValue;
begin
  ltype := TRTTIContext.Create.GetType(ATypeInfo);
  lFields := lType.getFields;
  for lfield in lFields do
  begin
    if lField.FieldType.IsRecord then
    begin
      // Add client records here.
    end else
    begin
      lValue := lField.getValue(APRecordToClone);
      lField.SetValue(APClonedRecord,lValue);
    end;
  end;
end;

Function GetValuePairHeader(ATypeInfo: Pointer; APValue: Pointer): string;
var
  ltype: TRTTIType;
  lPrefix : string;
  lValue : TValue;
  lDynArray : TRttiDynamicArrayType;
  lArray : TRttiArrayType;
  l : integer;
  lName: string;
  function NameAndLength(AName: string; ACount: integer): string;
  begin
    if Acount>=0 then
      Result := format('%s[]%sRecordCount=%u', [AName,#13#10,ACount])
    else Result := AName+'[]';
  end;
begin
  Result := '';
  ltype := TRTTIContext.Create.GetType(ATypeInfo);
  if not (lType.TypeKind in [tkRecord,tkArray,tkDynArray]) then exit;

  case lType.TypeKind of
    tkArray:
      begin
        lArray := lType as TRttiArrayType;
        if lArray.ElementType.TypeKind<>tkRecord then exit;
        Result := NameAndLength(lArray.ElementType.Name,lArray.TotalElementCount);
      end;
    tkDynArray:
      begin
        lDynArray := lType as TRttiDynamicArrayType;
        if lDynArray.ElementType.TypeKind<>tkRecord then exit;

        lName := lDynArray.ElementType.Name;
        l := -1;
        if APValue<>nil then
        begin
          TValue.Make(ApValue,lDynArray.Handle,lValue);
          l := lValue.GetArrayLength;
        end;
        Result := 'RecordType='+NameAndLength(lName, l);
      end;
  else
     begin
      Result := Result + 'RecordType='+lType.Name;
      exit;
     end;
  end; // case
end;

{ TRecordSerializer<T> }

function TRecordSerializer<T>.AsValuePairs(AIndex:integer =-1): string;
var i,l :integer;
begin
 if (self.Count=0) then
 begin
   result := RecordAsValuePairs(TypeInfo(T), @self.Values, AIndex);
 end else if (AIndex>=0) and (Aindex<Count) then
 begin
   result := RecordAsValuePairs(TypeInfo(T), @self.AllValues[AIndex], AIndex);
 end else
 begin
   l := self.Count-1;
   for i := 0 to l do
   begin
     result := result + RecordAsValuePairs(TypeInfo(T), @self.AllValues[i], i);
   end;
 end;
end;

function TRecordSerializer<T>.AsJSON: String;
var i : Integer;
    lComma: string;
begin
  if Self.isArray then
  begin
    Result := '[';  
    for I := 0 to High(Self.AllValues) do
    begin
      result := Result + lComma + 
                RecordAsJSON(typeInfo(T),@Self.AllValues[i]);
      lComma := ',';
    end;
    result := Result + ']';
  end
  else
    result := RecordAsJSON(typeInfo(T),@Self.Values);
end;

procedure TRecordSerializer<T>.AsValuePairs(AStrings: TStrings; AIndex:integer =-1);
begin
  AStrings.Text := RecordAsValuePairs(TypeInfo(T), @self.Values, AIndex);
end;

procedure TRecordSerializer<T>.Clear;
begin
  clearRecord(TypeInfo(T),@self.Values);
  setlength(AllValues,0);
  findex := -1;
  fIsArray := '';
end;

procedure TRecordSerializer<T>.Clone(ARecord: TRecordSerializer<T>);
begin
  CloneRecord(TypeInfo(T),@Arecord.Values, @Self);
end;

procedure TRecordSerializer<T>.Clone(ARecord: T);
begin
  CloneRecord(TypeInfo(T),@Arecord, @Self.Values);
end;

procedure TRecordSerializer<T>.FromJSON(AJSON: string);
begin
 self.Parse(JSONToValuePairs(AJSON));
end;

function TRecordSerializer<T>.getCount: integer;
begin
  result := length(Self.AllValues);
  if result>0 then fIsArray:='True';
end;

function TRecordSerializer<T>.getIsArray: boolean;
begin
  result := (Count>0) or (fIsArray='True');
end;

class operator TRecordSerializer<T>.Implicit(ARecord: T): TRecordSerializer<T>;
begin
   result.clone(ARecord);
end;

class operator TRecordSerializer<T>.Implicit(ASerializable: TRecordSerializer<T>): T;
begin
  CloneRecord(TypeInfo(T), @ASerializable.Values, @Result);
end;

Function TRecordSerializer<T>.Parse(AText: String; AMode: TSerializerParseMode=spmUpdate;
          AIndex: integer=-1): integer; 
var lList : TStringlist;
begin
  lList := TStringlist.create;
  try
    lList.Text := AText;
    lList.Sorted := true;
    result := Self.Parse(lList,Amode,AIndex);
  finally
    freeandnil(lList);
  end;
end;

procedure TRecordSerializer<T>.setIsArray(const Value: boolean);
begin
  if Value then
  begin
    if self.Count=0 then 
    begin
      setlength(AllValues,1);
      fIndex := 0;
      CloneREcord(TypeInfo(T),@Values,@AllValues[0]);
    end;
    fIsArray:='True';
  end else
  begin
    if (self.Count>0) then Setlength(AllValues,0);
    fIsArray:='';
    fIndex := -1;
  end;
end;

Function TRecordSerializer<T>.Parse(AStrings: TStrings; AMode: TSerializerParseMode=spmUpdate;
             AIndex: integer=-1): integer;  
var lIndex,
    I,SI,MaxID :integer;
    bounds : TArrayBounds;
    lList : TStringlist;
    lIsArray : boolean;
    lOffset : integer;
begin
  Result := 0;
  MaxID := self.Count-1;
  lOffset := 0;

  bounds.high := -1; bounds.low := -1;
  bounds := GetIndexBounds(AStrings);
  
  // should it be an array now considering the data
  lIsArray := // was it previously an Array?
               isArray OR 
               // has arrays but index not specified
              ((AIndex<0) and (bounds.High<>-1)) OR
               // has index Specified AND there are indexes specified
              ((AIndex>=0) and (Bounds.High>=0));

  // We might need to change the Mode if this has just become an array
  if (lIsArray) then
  begin
    // Is an array but no bounds are specified
    if (bounds.high=-1) then
    begin
      if (AMode=spmUpdate) and (AIndex<0) then
      raise Exception.Create('Cannot update records, no index specified');
      
      if AMode=spmAppend then
      begin
        MaxId:=Self.Count;
        setlength(AllValues,MaxID+1);
        if (ParseValuesToRecord(AStrings, TypeInfo(T), @Self.AllValues[MaxId])>0) then
         result := 1;
        exit;
      end;
    end;
    
    // wasnt an array, but now is
    if (self.isArray <> lIsArray) and (AMode=spmUpdate) then
    begin
      AMode := spmAppend;
      lOffset := -1; // need to offset start point by 1
    end;
    //if in Append Mode, but there are no records, need to offset
    if (MaxID<0) and (Amode=spmAppend) then lOffset:=-1;
  end;

             
  self.isArray := lIsArray;

  // is there only 1 element to Set ?
  if ( (not lIsArray) and (Amode=spmUpdate) ) then
  begin
    if (ParseValuesToRecord(AStrings, TypeInfo(T), @Self.Values, AIndex)>0) then
       result := 1;
    exit;
  end;

  if ((lIsArray) and (AIndex>=0) ) then
  begin
    if Bounds.High=-1 then 
    begin
      bounds.Low := AIndex;
      bounds.High := AIndex;
      AIndex := -1;
    end;
    if (MaxId<bounds.high) then
    begin
      if AMode=spmUpdate then
       raise Exception.Create(
        format('Index out of bounds %u',[bounds.high]))
      else SetLength(Self.AllValues,bounds.High)
    end;
    if (ParseValuesToRecord(AStrings, TypeInfo(T), @Self.AllValues[bounds.High], AIndex)>0) then
       result := 1;
    exit;
  end;
  
  // Potentially more than 1 record
  if AMode=spmAppend then SI := self.Count+lOffset else SI:=bounds.Low;
  
  for I := bounds.Low to bounds.High do
  begin
    if (si>Count-1) then
    begin
      if AMode=spmUpdate then raise Exception.Create(
        format('Index out of bounds %u',[si]))
      else SetLength(Self.AllValues,si+1)
    end;
    if ParseValuesToRecord(AStrings,TypeInfo(T),@Self.AllValues[si],i)>0 then
    begin
      inc(result);
    end;
    inc(si);
  end;
  if (Self.count=1) and (Result=1) then
  begin
     findex := 0;
     self.Clone(Self.AllValues[0]);
  end;
end;


class operator TRecordSerializer<T>.Implicit(
  AText: string): TRecordSerializer<T>;
begin
   Result.Parse(AText);
end;

class operator TRecordSerializer<T>.Implicit(
  ARecord: TRecordSerializer<T>): String;
begin
   Result := ARecord.AsValuePairs;
end;

end.