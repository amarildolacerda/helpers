unit System.Json.Helper;

interface

Uses System.Classes, System.SysUtils, System.Rtti, System.TypInfo, System.Json;

type

    TJsonRecord<T:Record> = class
      public
       class function ToJson(O: T): string;
       class procedure FromJson(O:T;AJson:string);
    end;



implementation


class procedure TJsonRecord<T>.FromJson(O: T; AJson: string);
var js:TJsonObject;
    AContext  : TRttiContext;
    AField    : TRttiField;
    ARecord   : TRttiRecordType;
    AFldName  : String;
    AValue    : TValue;
begin
   js:= TJsonObject.ParseJSONValue(AJson) as TJsonObject;
   try
    AContext := TRttiContext.Create;
    try
        ARecord := AContext.GetType(TypeInfo(T)).AsRecord;
        for AField in ARecord.GetFields do
        begin
            AFldName := AField.Name;
            AValue := js.GetValue(AFldName);
            AField.SetValue(@O,AValue);
        end;

    finally
      AContext.free;
    end;

   finally
     js.Free;
   end;
end;

class function TJsonRecord<T>.ToJson(O: T): string;
var
    AContext  : TRttiContext;
    AField    : TRttiField;
    ARecord   : TRttiRecordType;
    AFldName  : String;
    AValue    : TValue;
    ArrFields : TArray<TRttiField>;
    i:integer;
    js:TJsonObject;
begin
    js := TJsonObject.Create;
    AContext := TRttiContext.Create;
    try
        ARecord := AContext.GetType(TypeInfo(T)).AsRecord;
        ArrFields := ARecord.GetFields;
        i := 0;
        for AField in ArrFields do
        begin
            AFldName := AField.Name;
            AValue := AField.GetValue(@O);
            try
            if AValue.IsEmpty then
              js.addPair(AFldName,'NULL')
            else
            case AField.FieldType.TypeKind of
               tkInteger,tkInt64:
                    try
                      js.addPair(AFldName,TJSONNumber.Create(Avalue.AsInt64));
                    except
                      js.addPair(AFldName,TJSONNumber.Create(0));
                    end;
               tkEnumeration:
                      js.addPair(AFldName,TJSONNumber.Create(Avalue.AsInteger));
               tkFloat:
                 begin
                    if AField.FieldType.ToString.Equals('TDateTime') then
                      js.addPair(AFldName, FormatDateTime('yyyy-mm-dd HH:nn:ss',  AValue.AsExtended))
                    else
                    if AField.FieldType.ToString.Equals('TDate') then
                      js.addPair(AFldName, FormatDateTime('yyyy-mm-dd',  AValue.AsExtended))
                    else
                    if AField.FieldType.ToString.Equals('TTime') then
                      js.addPair(AFldName, FormatDateTime('HH:nn:ss',  AValue.AsExtended))
                    else
                      try
                        js.addPair(AFldName,TJSONNumber.Create(Avalue.AsExtended));
                      except
                        js.addPair(AFldName,TJSONNumber.Create(0));
                    end;
                 end
            else
               js.addPair(AFldName,AValue.asString)
            end;
            except
              js.addPair(AFldName,'NULL')
            end;

        end;
        result := js.ToString;
    finally
        js.Free;
        AContext.Free;
    end;

end;


end.
