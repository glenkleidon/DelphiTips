unit RecordUtils;

////////////////////////////////////////////////
// Copyright Glen Kleidon 2017                //
// Part of the Delphi Tips Repo               //
// https://github.com/glenkleidon/DelphiTips  //
////////////////////////////////////////////////

interface
   uses System.sysUtils, System.Classes, System.Rtti;

   function IndexedName(AId: string; AIndex: integer=-1): string;
   function ParseStringAsIndex(AId: string; AStrings: TStrings; AIndex: integer=-1): string;
   function AsValuePair(Aid: string; AValue: string; AIndex: integer=-1): string;
   function AsValuePairRow(Aid: string; AValue: string; AIndex: integer=-1): string;
   Function RecordAsValuePairs(ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
   Function ParseValuesToRecord(AStrings: TStrings; ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
   procedure CloneRecord(ATypeInfo: Pointer; APRecordToClone: Pointer; APClonedRecord : Pointer );


implementation

  uses System.TypInfo;

function IndexedName(AId: string; AIndex: integer=-1): string;
begin
 if (Aindex<0) then Result := AId else Result := format('%s[%u].',[AId, AIndex]);
end;

Function ParseValuesToRecord(AStrings: TStrings; ATypeInfo: Pointer; APValue: Pointer; AIndex: integer = -1): string;
var
  ltype: TRTTIType;
  lfield: TRttiField;
  lfields: TArray<TRttiField>;
  lContext: TRTTIContext;
  lText : TStringlist;
  lPrefix : string;
  lTValue : TValue;
  lValue : string;
begin
  lContext := TRTTIContext.Create;
  ltype := lContext.GetType(ATypeInfo);
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
         lValue := AStrings.values[lField.Name];
         if LValue = '' then lValue := AStrings.values[lPrefix+lField.Name];
         if lValue = '' then continue;
         if (lField.FieldType.TypeKind = TTypeKind.tkEnumeration) then
         begin
           lTValue := lField.GetValue(APValue);
           lTValue := getEnumValue(lTValue.TypeInfo,lValue);
         end else lField.setValue(APValue, TValue(lValue));
      end;
    end;
    result := lText.text;
  finally
    freeAndNil(lText);
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
        lValue := lField.getValue(APValue).ToString;
        lText.Add(AsValuePair(lField.Name,lValue,AIndex));
      end;
    end;
    result := lText.text;
  finally
    freeAndNil(lText);
  end;
end;

function ParseStringAsIndex(AId: string; AStrings: TStrings; AIndex: integer=-1): string;
var lSuffix : string;
begin
  if AIndex=-1 then lSuffix := '' else lSuffix := format('[%u]',[AIndex]);
  result := AStrings.Values[Aid+lSuffix];
end;

function AsValuePairRow(Aid: string; AValue: string; AIndex: integer=-1): string;
begin
  result :=  AsValuePair(Aid, AValue, AIndex) + #13#10;
end;

function AsValuePair(Aid: string; AValue: string; AIndex: integer=-1):string;
var lSuffix : string;
begin
  if AIndex=-1 then lSuffix := '' else lSuffix := format('[%u]',[AIndex]);
  result := format('%s%s=%s',[AId,lSuffix,
          AValue.replace(#13#10,#10,[rfReplaceAll]).
          replace(#13,#10,[rfReplaceAll])
          ]);
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

end.
