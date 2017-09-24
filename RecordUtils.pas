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
   Type TSerializerEncoding = (seValuePairs, seJSON, seURLEncoding);
   Type TJSONState = (jsNone, jsInArray, jsInObject, jsInName, jsEndName,
                       jsInQuotedValue, jsInValue, jsInEscape, jsEndValue, jsEndObject, jsEndArray, jsNextElement);
   Type TNamedCounter = record
        Name: string;
        index: integer;
        Procedure Clear;
        class Function New: TNamedCounter; static;
   end;

   Type TNamedCounterArray = TArray<TNamedCounter>;
   Type TNamedCounterStack = Record
   private
    fList : TNamedCounterArray;
    function getPeek: TNamedCounter;
    function getIndex: integer;
    function getCurrentName: string;
    function getFormatString: String;
   public
    Function Pop : TNamedCounter;
    Function Push(ANamedIndex:TNamedCounter): Integer;
    Procedure IncCounter;
    Procedure NameCounter(AName: string);
    Function CurrentCounter: Integer;
    Procedure Clear;
    Property Peek : TNamedCounter read getPeek;
    Property CurrentIndex : integer read getIndex;
    Property CurrentName : string read getCurrentName;
    Property FormatString : String read getFormatString;
   end;

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
       procedure setIsArray(const AValue: boolean);
     public
       Value : T;
       Values: Array of T;
       Procedure Clear;
       Procedure Clone(ARecord: T); overload;
       Procedure Clone(ARecord: TRecordSerializer<T>); overload;
       Function Parse(AStrings:TStrings; AMode: TSerializerParseMode=spmUpdate;
                             AIndex: integer=-1; AFormat: TSerializerEncoding=seValuePairs): integer; overload;
       Function Parse(AText: String; AMode: TSerializerParseMode = spmUpdate;
                             AIndex: integer=-1; AFormat: TSerializerEncoding=seValuePairs): integer; overload;
       Function AsValuePairs(AIndex:integer =-1; AFormat: TSerializerEncoding=seValuePairs): string; overload;
       Procedure AsValuePairs(AStrings:TStrings; AIndex:integer =-1; AFormat: TSerializerEncoding=seValuePairs); overload;
       Function AsJSON : String; overload;
       Procedure FromJSON(AJSON: string);
       Function AsURLEncoded : string;
       Procedure FromURLEncoded(AURLEncoded: string);
       class operator Implicit(ASerializable: TRecordSerializer<T>): T;
       class operator Implicit(ARecord: T): TRecordSerializer<T>;
       class operator Implicit(AText: string): TRecordSerializer<T>;
       class operator Implicit(ARecord: TRecordSerializer<T>): String;
       Property Count : integer read getCount;
       Property isArray: boolean read getIsArray write setIsArray;
   end;

   function IndexedName(AId: string; AIndex: integer=-1): string;
   function ParseStringAsIndex(AId: string; AStrings: TStrings; AIndex: integer=-1): string;
   function AsValuePair(AId: string; AValue: string; AIndex: integer=-1; ALineEnding: string=''; AFormat: TSerializerEncoding=seValuePairs):string;
   function AsValuePairRow(AId: string; AValue: string; AIndex: integer=-1; AFormat: TSerializerEncoding=seValuePairs): string;
   Function RecordAsValuePairs(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1; AFormat: TSerializerEncoding=seValuePairs): string;
   Function ParseValuesToRecord(AStrings: TStrings; ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1;
                   AFormat: TSerializerEncoding = seValuePairs): integer;
   Function GetValuePairHeader(ATypeInfo: Pointer; APValue: Pointer): string;
   Procedure CloneRecord(ATypeInfo: Pointer; APRecordToClone: Pointer; APClonedRecord : Pointer );
   Procedure ClearRecord(ATypeInfo: Pointer; APValue: Pointer);
   Function AsJSONPair(AId: string; AValue: string;AValueQuoted:boolean=true):string;
   Function RecordAsJSON(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
   Procedure ParseJSONtoRecord(AString: String; ATypeInfo: Pointer; APValue: Pointer);
   Function JSONToValuePairs(AJSON:string):String;
   Function GetIndexBounds(AStrings: TStrings): TArrayBounds;
   Function isURLEncoding(var AString: string): boolean;


implementation

  uses TypInfo,StrUtils,
     httpApp       // URL Encoding support... NOT UTF Safe!!
     ;

Function isURLEncoding(var AString: string): boolean;
var pAmp,pAmp2, pCR, pCr2, pEqual : integer;
begin
  // to be URL Encoded, it must have & and =
  Result := false;
  pAmp   := pos('&',AString);
  if pAmp<1 then exit;
  result := true;
  pCr    := pos(#13,AString);
  if pCr<1 then exit;

  // Could still be either
  pAmp2  :=posex('&',AString,pAmp+1);
  pCr2   :=posEx(#13,AString,pCr+1);

  if (pCr2<1) and (pAmp2>0) then exit;  // ok almost certainly

  pEqual :=Posex('=',Astring,pAmp+1);
  if (pEqual>0) and (pEqual<pAmp2) then exit;// if its not, its a very good imitation

  result := false;
end;

Function GetIndexBounds(AStrings: TStrings): TArrayBounds;
var p,q: integer;
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
    l,i : integer;
    lValue, lName, lRow : string;
    lIndexList : TNamedCounterStack;
    lStates: TStack<TJSONState>;
    lState : TJSONState;
    lList : TStringlist;
    Procedure JSONParseError(AChar: Char);
    begin
      raise Exception.Create(format('JSON Parse Error character %s at %u',[string(AChar), i]));
    end;
    Procedure Reset;
    begin
      lName := '';
      lValue := '';
    end;
    Procedure EndValue;
    begin
      // could populate directly from here!!!!
      lRow := format( lIndexList.CurrentName+'=%s',[lValue]);
      lList.Add(lRow);
      Reset;
    end;
    function EndInValue : boolean;
    begin
      result := false;
      if lState<>jsInValue then exit;
      lValue := trim(lValue);
      Endvalue;
      result := true;
    end;

begin
   // simple implementation assumes single object, no spacing
   lIndexList.clear;
   lStates:= TStack<TJSONState>.Create;
   lList := TStringlist.Create;
   Try
     lStates.Push(TJSONState.jsNone);
     l := length(AJSON);
     for i := 1 to l do
     begin
       c := AJSON[i];
       lState := lStates.peek;
       if (lState=jsInEscape) or (c<>'"') then
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
              continue;
            end;
       end;
       case c of
         '{':
             case lState of
              jsInValue,
              jsNone,
              jsNextElement,
              jsInArray:
               begin
                 if lState=jsInValue then lStates.pop;
                 Reset;
                 if lStates.peek=jsNextElement then lStates.pop;
                 lStates.Push(TjsonState.jsInObject);
                 lIndexList.Push(TNamedCounter.New);
               end;
              else JSONParseError(c);
             end;
         '}':
             case lState of
              jsInObject, 
              jsInValue, jsEndValue, jsEndArray, jsEndObject:
               begin
                 EndInValue;
                 if lState<>jsInObject then lStates.Pop;
                 // now we should be in inObject
                 lStates.Pop; // pop out of current object
                 lIndexList.Pop;
                 lState := jsEndObject;
                 lStates.Push(lState);
               end;
              else JSONParseError(c);
             end;
         '[':
             case lState of
              jsInValue, jsNone, jsInArray, jsNextElement:
               begin
                  if lState=jsInValue then lStates.pop;
                  lIndexList.Push(TNamedCounter.New);
                  lIndexList.IncCounter;
                  lStates.Push(jsInArray);
               end;
              else JSONParseError(c);
             end;
         ']':
             case lState of
              jsInValue, jsEndValue, jsEndObject, jsEndArray:
               begin
                  EndInValue;
                  lStates.Pop;
                  if not(lStates.peek=jsInArray) then JSONParseError(c);
                  lStates.Pop; // pop out of the current array
                  lIndexList.Pop;
                  lStates.push(jsEndArray);
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
                 if not(lStates.peek in [jsInObject, jsInArray]) then lStates.Pop;
                 lStates.Push(jsInName);
               end;
              jsInValue:
                begin
                 lStates.Pop;
                 lValue := '';
                 lState := jsInQuotedValue;
                 lStates.Push(lState);
                end;
              jsInName:
                begin
                 lName := lValue;
                 lIndexList.NameCounter(lName);
                 lValue := '';
                 lStates.Pop;
                 lStates.Push(jsEndName);
                end;
              jsInQuotedValue:
                begin
                 lStates.Pop;
                 EndValue;
                 lStates.Push(jsEndValue);
                end;
             else JSONParseError(c);
             end;
         ',':
             case lState of
              jsinValue, jsEndValue, jsEndArray, jsEndObject:
                begin
                  EndInValue;
                  lStates.Pop;
                  lState := lStates.Peek;
                  case lState of
                     jsInObject:; //ok, but nothing to do
                     jsInArray : lIndexList.IncCounter;
                  else JSONParseError(c);
                  end;
                  lStates.Push(jsNextElement);
                end;
              else JSONParseError(c);
             end;
         ':':
             case lState of
              jsEndName:
              begin
                lStates.Pop;
                lStates.Push(jsInValue);
              end;
             else JSONParseError(c);
             end;
       end;
       if lState in [jsInValue] then lvalue := lValue + c;
     end;
     lStates.Pop;
     if lStates.Peek<>jsNone then raise Exception.Create('JSON Text Incomplete');

     result := lList.text;
   finally
     freeandnil(lList);
     freeandnil(lStates);
   end;
end;

function IndexedName(AId: string; AIndex: integer=-1): string;
begin
 if (Aindex<0) then Result := AId else Result := format('%s[%u]',[AId, AIndex]);
end;

Function ParseValuesToRecord(AStrings: TStrings; ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1;
    AFormat: TSerializerEncoding = seValuePairs): integer;
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lContext: TRTTIContext;
  lPrefix : string;
  lSuffix : String;
  lTValue : TValue;
  lValue : string;
  lFieldName: string;
  lInt: Int64;
  lSingle: Single;
  lDouble: Double;
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
       lFieldName := lField.Name;
       if AFormat=seURLEncoding then lFieldname := HTTPEncode(lFieldname);
       lValue := AStrings.values[lFieldName];
       if length(LValue)=0 then lValue := AStrings.values[lFieldName+lSuffix];
       if length(LValue)=0 then lValue := AStrings.values[lPrefix+lFieldName];
       if length(lValue)=0 then continue;
       if AFormat=seURLEncoding then lValue := httpDecode(lValue);
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

Function RecordAsValuePairs(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1;
  AFormat: TSerializerEncoding=seValuePairs): string;
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lText : TStringlist;
  lPrefix : string;
  lValue : string;
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
        lValue := AsValuePair(lField.Name,lField.getValue(APValue).ToString, AIndex,#13#10,AFormat);
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

function AsValuePairRow(AId: string; AValue: string; AIndex: integer=-1;
      AFormat: TSerializerEncoding=seValuePairs): string;
begin
  result :=  AsValuePair(AId, AValue, AIndex,#13#10, Aformat);
end;

function AsValuePair(AId: string; AValue: string; AIndex: integer=-1;
           ALineEnding: string=''; AFormat: TSerializerEncoding=seValuePairs):string;
var lSuffix : string;
begin
  result := '';
  if length(AValue)>0 then
  begin
    if AFormat=seURLEncoding then
    begin
      // by default from HTTPApp which should be available in very version of delphi
      AId := HTTPEncode(AId);
      AValue := HTTPEncode(AValue);
    end else
    begin
      AValue :=  stringReplace(
              stringreplace(AValue,#13#10,#10,[rfReplaceAll]),
                #13,#10,[rfReplaceAll]) ;
    end;
    if AIndex=-1 then lSuffix := '' else lSuffix := format('[%u]',[AIndex]);
    result := format('%s%s=%s',[AId,lSuffix,AValue]);
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

function TRecordSerializer<T>.AsValuePairs(AIndex:integer =-1; AFormat: TSerializerEncoding=seValuePairs): string;
var i,l :integer;
begin
 if (self.Count=0) then
 begin
   result := RecordAsValuePairs(TypeInfo(T), @self.Value, AIndex, AFormat);
 end else if (AIndex>=0) and (Aindex<Count) then
 begin
   result := RecordAsValuePairs(TypeInfo(T), @self.Values[AIndex], AIndex, Aformat);
 end else
 begin
   l := self.Count-1;
   for i := 0 to l do
   begin
     result := result + RecordAsValuePairs(TypeInfo(T), @self.Values[i], i, AFormat);
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
    for I := 0 to High(Self.Values) do
    begin
      result := Result + lComma + 
                RecordAsJSON(typeInfo(T),@Self.Values[i]);
      lComma := ',';
    end;
    result := Result + ']';
  end
  else
    result := RecordAsJSON(typeInfo(T),@Self.Value);
end;

function TRecordSerializer<T>.AsURLEncoded: string;
begin
   Result := stringreplace(
          AsValuePairs(-1,seURLEncoding), #13#10, '&', [rfReplaceAll]);
   Result := copy(Result,1,length(Result)-1);
end;

procedure TRecordSerializer<T>.AsValuePairs(AStrings: TStrings; AIndex:integer =-1; AFormat: TSerializerEncoding=seValuePairs);
begin
  AStrings.Text := RecordAsValuePairs(TypeInfo(T), @self.Value, AIndex, AFormat);
end;

procedure TRecordSerializer<T>.Clear;
begin
  clearRecord(TypeInfo(T),@self.Value);
  setlength(Values,0);
  findex := -1;
  fIsArray := '';
end;

procedure TRecordSerializer<T>.Clone(ARecord: TRecordSerializer<T>);
begin
  CloneRecord(TypeInfo(T),@Arecord.Value, @Self);
end;

procedure TRecordSerializer<T>.Clone(ARecord: T);
begin
  CloneRecord(TypeInfo(T),@Arecord, @Self.Value);
end;

procedure TRecordSerializer<T>.FromJSON(AJSON: string);
begin
 self.Parse(JSONToValuePairs(AJSON));
end;

procedure TRecordSerializer<T>.FromURLEncoded(AURLEncoded: string);
begin
  Parse(stringReplace(AURLEncoded,'&',#13#10, [rfReplaceAll]),spmUpdate,-1,seURLEncoding);
end;

function TRecordSerializer<T>.getCount: integer;
begin
  result := length(Self.Values);
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
  CloneRecord(TypeInfo(T), @ASerializable.Value, @Result);
end;

Function TRecordSerializer<T>.Parse(AText: String; AMode: TSerializerParseMode=spmUpdate;
          AIndex: integer=-1; AFormat: TSerializerEncoding=seValuePairs): integer;
var lList : TStringlist;
begin
  lList := TStringlist.create;
  try
    lList.Text := AText;
    lList.Sorted := true;
    result := Self.Parse(lList,Amode,AIndex,AFormat);
  finally
    freeandnil(lList);
  end;
end;

procedure TRecordSerializer<T>.setIsArray(const AValue: boolean);
begin
  if AValue then
  begin
    if self.Count=0 then
    begin
      setlength(Values,1);
      fIndex := 0;
      CloneREcord(TypeInfo(T),@Value,@Values[0]);
    end;
    fIsArray:='True';
  end else
  begin
    if (self.Count>0) then Setlength(Values,0);
    fIsArray:='';
    fIndex := -1;
  end;
end;

Function TRecordSerializer<T>.Parse(AStrings: TStrings; AMode: TSerializerParseMode=spmUpdate;
             AIndex: integer=-1; AFormat: TSerializerEncoding=seValuePairs): integer;
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
        setlength(Values,MaxID+1);
        if (ParseValuesToRecord(AStrings, TypeInfo(T), @Self.Values[MaxId],-1,AFormat)>0) then
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
    if (ParseValuesToRecord(AStrings, TypeInfo(T), @Self.Value, AIndex, AFormat)>0) then
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
      else SetLength(Self.Values,bounds.High)
    end;
    if (ParseValuesToRecord(AStrings, TypeInfo(T), @Self.Values[bounds.High], AIndex, AFormat)>0) then
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
      else SetLength(Self.Values,si+1)
    end;
    if ParseValuesToRecord(AStrings,TypeInfo(T),@Self.Values[si],i, AFormat)>0 then
    begin
      inc(result);
    end;
    inc(si);
  end;
  if (Self.count=1) and (Result=1) then
  begin
     findex := 0;
     self.Clone(Self.Values[0]);
  end;
end;


class operator TRecordSerializer<T>.Implicit(
  AText: string): TRecordSerializer<T>;
begin
   if isURLEncoding(AText) then
     Result.FromURLEncoded(AText)
   else
     Result.Parse(AText);
end;

class operator TRecordSerializer<T>.Implicit(
  ARecord: TRecordSerializer<T>): String;
begin
   Result := ARecord.AsValuePairs;
end;

{ TNamedIndex }

procedure TNamedCounter.Clear;
begin
  self.Name := '';
  self.index := -1;
end;

class function TNamedCounter.New: TNamedCounter;
begin
  result.Clear;
end;

{ TNamedCounterStack }

function TNamedCounterStack.getIndex: integer;
begin
  result := length(fList)-1;
end;

function TNamedCounterStack.getPeek: TNamedCounter;
begin
  Result := fList[CurrentIndex];
end;

procedure TNamedCounterStack.NameCounter(AName: string);
begin
  fList[CurrentIndex].Name := AName;
end;

procedure TNamedCounterStack.IncCounter;
begin
  inc(fList[CurrentIndex].Index); 
end;

function TNamedCounterStack.Pop: TNamedCounter;
var lIndex : integer;
begin
  if length(flist)=0 then raise Exception.Create('Cannot Pop from empty stack');
  lIndex := length(flist)-2;
  setlength(fList,lIndex+1);
  if (lIndex>=0) then Result := fList[lIndex];
end;

function TNamedCounterStack.Push(ANamedIndex: TNamedCounter): Integer;
var lIndex: integer;
begin
  lIndex:= length(Flist);
  setlength(fList,lIndex+1);
  flist[lIndex].Name := ANamedIndex.Name;
  flist[lIndex].index := ANamedIndex.index;
  Result := lIndex;
end;

procedure TNamedCounterStack.Clear;
begin
  Setlength(fList,0);
end;

function TNamedCounterStack.CurrentCounter: Integer;
begin
  result := flist[CurrentIndex].index;
end;

Function TNamedCounterStack.getCurrentName: string;
var lNamedIndex : TNamedCounter;
    i,l: integer;
    lDot,lNameStr,lCountStr : string;
begin
  result := '';
  lDot := '';                              
  l:=length(fList)-1;
  for i := 0 to l do
  begin
    lNamedIndex := fList[i];
    lNameStr := lNamedIndex.Name;
    lCountStr := '';
    if lNamedIndex.index>=0 then lCountStr := format('[%u]',[lNamedIndex.index]); 
    if length(lNamedIndex.Name)>0 then
    begin
      if True then

      Result := Result + format('%s%s%s',
              [lDot,lNameStr,lCountStr]);
    end
    else 
    begin
      if i=0 then
      begin
        if l>0 then fList[1].index := lNamedIndex.index;
        continue;
      end;
      if i=l then lNameStr := '%s';
      Result := Result + format('%s%s',[lNameStr,lCountStr]);
    end;
    lDot := '.';
  end;
end;

function TNamedCounterStack.getFormatString: String;
begin
 //  
end;

end.