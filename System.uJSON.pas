unit System.uJson;

interface

{$I Delphi.inc }
{$IFDEF UNICODE}
{$DEFINE XE}
{$ENDIF}

uses System.Classes, System.Types, System.SysUtils, System.JSON,
  RegularExpressions, {RTTI, TypInfo, DBXJson,} DBXJsonReflect;

type

  TJsonType = (jtUnknown, jtObject, jtArray, jtString, jtTrue, jtFalse,
    jtNumber, jtDate, jtDateTime, jtBytes);

  TObjectHelper = class Helper for TObject
  private
  public
    class procedure Using<T>(O:T; Proc: TProc<T>); static;
    procedure Anonimous<T:Class>( Proc: TProc<T> );
    procedure Run<T:Class>(Proc: TProc<T> );
    procedure Queue<T:Class>( Proc:TProc<T>);
    procedure Synchronize<T:Class>( Proc:TProc<T>);

    class function FromJson(AJson: String): TObject; static;
    function Clone: TObject;
    function asJson: string;
  end;

  TJSONObjectHelper = class helper for TJSONObject
  private

  public
{$IFDEF VER270}
    function ToJSON: string;
{$ENDIF}
    class function GetTypeAsString(AType: TJsonType): string; static;
    class function GetJsonType(AJsonValue: TJsonValue): TJsonType; static;
    class function Stringify(so: TJSONObject): string;
    class function Parse(const dados: string): TJSONObject;
    function V(chave: String): variant;
    function S(chave: string): string;
    function I(chave: string): integer;
    function O(chave: string): TJSONObject; overload;
    function O(index: integer): TJSONObject; overload;
    function F(chave: string): Extended;
    function B(chave: string): boolean;
    function A(chave: string): TJSONArray;
    function AsArray: TJSONArray;
    function Contains(chave: string): boolean;
    function asObject: TObject;
    class function FromObject<T>(AObject: T): TJSONObject; overload;
    // class function FromObject(AObject: Pointer): TJSONObject;overload;
    class function FromRecord<T>(rec: T): TJSONObject;
    function addPair(chave: string; value: integer): TJSONObject; overload;
    function addPair(chave: string; value: Double): TJSONObject; overload;
    function addPair(chave: string; value: TDatetime): TJSONObject; overload;
  end;

  TJSONArrayHelper = class helper for TJSONArray
  public
    function Length: integer;
  end;

  TJSONValueHelper = class helper for TJsonValue
  public
{$IFDEF VER270}
    function ToJSON: string;
{$ENDIF}
    function AsArray: TJSONArray;
    function AsPair: TJsonPair;
    function Datatype: TJsonType;
    function asObject: TJSONObject;
  end;

  TJSONPairHelper = class helper for TJsonPair
  public
    function asObject: TJSONObject;
  end;

  IJson = TJSONObject;
  IJSONArray = TJSONArray;

  TJson = TJSONObject;

function ReadJsonString(const dados: string; chave: string): string;
function ReadJsonInteger(const dados: string; chave: string): integer;
function ReadJsonFloat(const dados: string; chave: string): Extended;
// function ReadJsonObject(const dados: string): IJson;
function JSONstringify(so: IJson): string;
function JSONParse(const dados: string): IJson;

function ISODateTimeToString(ADateTime: TDatetime): string;
function ISODateToString(ADate: TDatetime): string;
function ISOTimeToString(ATime: TTime): string;

function ISOStrToDateTime(DateTimeAsString: string): TDatetime;
function ISOStrToDate(DateAsString: string): TDate;
function ISOStrToTime(TimeAsString: string): TTime;

implementation

uses db, System.Rtti, System.TypInfo, System.DateUtils;

var
  LJson: TJson;

class function TJSONObjectHelper.GetTypeAsString(AType: TJsonType): string;
begin
  case AType of
    jtUnknown:
      result := 'Unknown';
    jtString:
      result := 'String';
    jtTrue, jtFalse:
      result := 'Boolean';
    jtNumber:
      result := 'Extended';
    jtDate:
      result := 'TDate';
    jtDateTime:
      result := 'TDateTime';
    jtBytes:
      result := 'Byte';
  end;
end;

class function TJSONObjectHelper.GetJsonType(AJsonValue: TJsonValue): TJsonType;
var
  LJsonString: TJSONString;
begin
  if AJsonValue is TJSONObject then
    result := jtObject
  else if AJsonValue is TJSONArray then
    result := jtArray
  else if (AJsonValue is TJSONNumber) then
    result := jtNumber
  else if AJsonValue is TJSONTrue then
    result := jtTrue
  else if AJsonValue is TJSONFalse then
    result := jtFalse
  else if AJsonValue is TJSONString then
  begin
    LJsonString := (AJsonValue as TJSONString);
    if TRegEx.IsMatch(LJsonString.value,
      '^([0-9]{4})-?(1[0-2]|0[1-9])-?(3[01]|0[1-9]|[12][0-9])(T| )(2[0-3]|[01][0-9]):?([0-5][0-9]):?([0-5][0-9])$')
    then
      result := jtDateTime
    else if TRegEx.IsMatch(LJsonString.value,
      '^([0-9]{4})(-?)(1[0-2]|0[1-9])\2(3[01]|0[1-9]|[12][0-9])$') then
      result := jtDate
    else
      result := jtString
  end
  else
    result := jtUnknown;
end;

function JSONParse(const dados: string): IJson;
begin
  result := TJSONObject.ParseJSONValue(dados) as IJson;
end;

function JSONstringify(so: IJson): string;
begin
  result := so.ToJSON;
end;

function ReadJsonFloat(const dados: string; chave: string): Extended;
var
  I: IJson;
begin
  I := JSONParse(dados);
  try
    I.TryGetValue<Extended>(chave, result);
  finally
    I.Free;
  end;
end;

function ReadJsonString(const dados: string; chave: string): string;
var
  j: TJson;
  I: IJson;
  V: variant;
begin
  j := JSONParse(dados);
  // usar variavel local para não gerar conflito com Multi_threaded application
  try
    j.TryGetValue<variant>(chave, V);
    result := V;
    { case VarTypeToDataType of
      varString: Result := I.S[chave];
      varInt64: Result := IntToStr(I.I[chave]);
      varDouble,varCurrency: Result := FloatToStr(I.F[chave]);
      varBoolean: Result := BoolToStr(  I.B[chave] );
      varDate: Result := DateToStr(I.D[chave]);
      else
      result :=  I.V[chave];
      end; }
  finally
    j.Free;
  end;
end;

(* function ReadJsonObject(const dados: string; chave: string): IJson;
  var
  j: TJson;
  begin
  result := JSONParse(dados);
  { // usar variavel local para não gerar conflito com Multi_threaded application
  try
  result := j.parse(dados);
  finally
  j.Free;
  end;}
  end;
*)
function ReadJsonInteger(const dados: string; chave: string): integer;
var
  j: TJson;
  I: IJson;
begin
  j := JSONParse(dados);
  // usar variavel local para não gerar conflito com Multi_threaded application
  try
    j.TryGetValue<integer>(chave, result);
  finally
    j.Free;
  end;
end;

{$IFNDEF MULTI_THREADED}

function JSON: TJson;
begin
  if not assigned(LJson) then
    LJson := TJson.Create;
  result := LJson;
end;

procedure JSONFree;
begin
  if assigned(LJson) then
    FreeAndNil(LJson);
end;
{$ENDIF}
{ TJSONObjectHelper }

function TJSONObjectHelper.A(chave: string): TJSONArray;
begin
  TryGetValue<TJSONArray>(chave, result);
end;

function TJSONObjectHelper.addPair(chave: string; value: integer): TJSONObject;
begin
  result := addPair(chave, TJSONNumber.Create(value));
end;

function TJSONObjectHelper.addPair(chave: string; value: Double): TJSONObject;
begin
  result := addPair(chave, TJSONNumber.Create(value));
end;

function TJSONObjectHelper.addPair(chave: string; value: TDatetime)
  : TJSONObject;
var
  S: string;
begin
  if trunc(value) <> value then
    S := ISODateTimeToString(value)
  else
    S := ISODateToString(value);
  result := addPair(chave, S);
end;

function TJSONObjectHelper.AsArray: TJSONArray;
begin
  result := TJSONObject.ParseJSONValue(self.ToJSON) as TJSONArray;
end;

function TJSONObjectHelper.B(chave: string): boolean;
begin
  TryGetValue<boolean>(chave, result);
end;

function TJSONObjectHelper.Contains(chave: string): boolean;
var
  LJSONValue: TJsonValue;
begin
  LJSONValue := FindValue(chave);
  result := LJSONValue <> nil;
end;

function TJSONObjectHelper.F(chave: string): Extended;
begin
  result := 0;
  if FindValue(chave) <> nil then
    TryGetValue<Extended>(chave, result);
end;

function TJSONObjectHelper.I(chave: string): integer;
begin
  result := 0;
  if FindValue(chave) <> nil then
    TryGetValue<integer>(chave, result);
end;

function TJSONObjectHelper.O(index: integer): TJSONObject;
var
  pair: TJsonPair;
begin
  result := TJSONObject(get(index));
end;

function TJSONObjectHelper.O(chave: string): TJSONObject;
var
  V: TJsonValue;
begin
  V := GetValue(chave);
  result := V as TJSONObject;
  // TryGetValue<TJSONObject>(chave, result);
end;

class function TJSONObjectHelper.Parse(const dados: string): TJSONObject;
begin
  result := TJSONObject.ParseJSONValue(dados) as TJSONObject;
end;

class function TJSONObjectHelper.FromRecord<T>(rec: T): TJSONObject;
var
  m: TJSONMarshal;
  js: TJsonValue;
begin
  { m := TJSONMarshal.Create;
    try
    js := m.Marshal(AObject);
    result := js as TJSONObject;
    finally
    m.Free;
    end;
  }
  result := TJSONObject.FromObject<T>(rec);
end;

class function TJSONObjectHelper.FromObject<T>(AObject: T): TJSONObject;
var
  typ: TRttiType;
  ctx: TRttiContext;
  field: TRttiField;
  tk: TTypeKind;
  P: Pointer;
  key: String;
  FRecord: TRttiRecordType;
  FMethod: TRttiMethod;
begin
  result := TJSONObject.Create;
  ctx := TRttiContext.Create;
  typ := ctx.GetType(TypeInfo(T));
  P := @AObject;
  for field in typ.GetFields do
  begin
    key := field.Name.ToLower;
    if not(field.Visibility in [mvPublic, mvPublished]) then
      continue;
    tk := field.FieldType.TypeKind;
    case tk of
      tkRecord:
        begin
          { FRecord := ctx.GetType(field.GetValue(P).TypeInfo).AsRecord ;
            FMethod := FRecord.GetMethod('asJson');
            if assigned(FMethod) then
            begin
            result.AddPair(key,fMethod.asJson );
            end; }
        end;
      tkInteger:
        result.addPair(key, TJSONNumber.Create(field.GetValue(P).AsInteger));
      tkFloat:
        begin
          if sametext(field.FieldType.Name, 'TDateTime') then
            result.addPair(TJsonPair.Create(key,
              ISODateTimeToString(field.GetValue(P).asExtended)))
          else if sametext(field.FieldType.Name, 'TDate') then
            result.addPair(TJsonPair.Create(key,
              ISODateToString(field.GetValue(P).asExtended)))
          else if sametext(field.FieldType.Name, 'TTime') then
            result.addPair(TJsonPair.Create(key,
              ISOTimeToString(field.GetValue(P).asExtended)))
          else if sametext(field.FieldType.Name, 'TTimeStamp') then
            result.addPair(TJsonPair.Create(key,
              ISODateTimeToString(field.GetValue(P).asExtended)))
          else
            result.addPair(key, TJSONNumber.Create(field.GetValue(P)
              .asExtended));
        end
    else
      result.addPair(TJsonPair.Create(key, field.GetValue(P).ToString));
    end;
  end;
end;

function TJSONObjectHelper.S(chave: string): string;
begin
  TryGetValue<string>(chave, result);
end;

class function TJSONObjectHelper.Stringify(so: TJSONObject): string;
begin
  result := so.ToJSON;
end;

function TJSONObjectHelper.V(chave: String): variant;
var
  V: string;
begin
  TryGetValue<string>(chave, V);
  result := V;
end;

function TJSONObjectHelper.asObject: TObject;
var
  m: TJSONunMarshal;
begin
  m := TJSONunMarshal.Create;
  try
    result := m.Unmarshal(self);
  finally
    m.Free;
  end;
end;

{$IFDEF VER270}

function TJSONObjectHelper.ToJSON: string;
begin
  result := ToString;
end;
{$ENDIF}
{ TJSONArrayHelper }

function TJSONArrayHelper.Length: integer;
begin
  result := Count;
end;

{ TJSONValueHelper }
{$IFDEF VER270}

function TJSONValueHelper.ToJSON: string;
begin
  result := ToString;
end;
{$ENDIF}
{ TJSONValueHelper }

function TJSONValueHelper.AsArray: TJSONArray;
begin
  result := self as TJSONArray;
end;

function TJSONValueHelper.asObject: TJSONObject;
begin
  result := self as TJSONObject;
end;

function TJSONValueHelper.AsPair: TJsonPair;
begin
  result := TJsonPair(self);
end;

function TJSONValueHelper.Datatype: TJsonType;
begin
  result := TJSONObject.GetJsonType(self);
end;

{ TJSONPairHelper }

function TJSONPairHelper.asObject: TJSONObject;
begin
  result := (self.JsonValue) as TJSONObject;
end;

{ TObjectHelper }

class procedure TObjectHelper.Using<T>(O: T; Proc: TProc<T>);
var obj:TObject;
begin
  try
    Proc(O);
  finally
    freeAndNil(o);
  end;
end;


function TObjectHelper.asJson: string;
var
  j: TJsonValue;
  m: TJSONMarshal;
begin
  m := TJSONMarshal.Create;
  try
    j := m.Marshal(self);
    result := j.ToJSON;
  finally
    m.Free;
  end;
end;

function TObjectHelper.Clone: TObject;
begin
  result := TObject.FromJson(asJson);
end;

class function TObjectHelper.FromJson(AJson: String): TObject;
var
  m: TJSONunMarshal;
  V: TJSONObject;
begin
  m := TJSONunMarshal.Create;
  try
    V := TJSONObject.Parse(AJson);
    result := m.Unmarshal(V);
  finally
    m.Free;
  end;
end;


procedure TObjectHelper.Queue<T>(Proc: TProc<T>);
begin
   TThread.Queue(nil,
       procedure
       begin
             Proc(self);
       end);
end;

procedure TObjectHelper.Run<T>(Proc: TProc<T>);
begin
   TThread.CreateAnonymousThread(
           procedure
              begin
                 proc(self);
              end ).Start;
end;


procedure TObjectHelper.Synchronize<T>(Proc: TProc<T>);
begin
   TThread.Synchronize(nil,
   procedure
   begin
      proc(self);
   end);
end;

procedure TObjectHelper.Anonimous<T>(Proc: TProc<T>);
begin
   Proc(self);
end;


function ISOTimeToString(ATime: TTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  result := FormatDateTime('hh:nn:ss', ATime, fs);
end;

function ISODateToString(ADate: TDatetime): string;
begin
  result := FormatDateTime('YYYY-MM-DD', ADate);
end;

function ISODateTimeToString(ADateTime: TDatetime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function ISOStrToDateTime(DateTimeAsString: string): TDatetime;
begin
  result := EncodeDateTime(StrToInt(Copy(DateTimeAsString, 1, 4)),
    StrToInt(Copy(DateTimeAsString, 6, 2)),
    StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)),
    StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;

function ISOStrToTime(TimeAsString: string): TTime;
begin
  result := EncodeTime(StrToInt(Copy(TimeAsString, 1, 2)),
    StrToInt(Copy(TimeAsString, 4, 2)), StrToInt(Copy(TimeAsString, 7, 2)), 0);
end;

function ISOStrToDate(DateAsString: string): TDate;
begin
  result := EncodeDate(StrToInt(Copy(DateAsString, 1, 4)),
    StrToInt(Copy(DateAsString, 6, 2)), StrToInt(Copy(DateAsString, 9, 2)));
  // , StrToInt
  // (Copy(DateAsString, 12, 2)), StrToInt(Copy(DateAsString, 15, 2)),
  // StrToInt(Copy(DateAsString, 18, 2)), 0);
end;


// function ISODateToStr(const ADate: TDate): String;
// begin
// Result := FormatDateTime('YYYY-MM-DD', ADate);
// end;
//
// function ISOTimeToStr(const ATime: TTime): String;
// begin
// Result := FormatDateTime('HH:nn:ss', ATime);
// end;

initialization

finalization

{$IFNDEF MULTI_THREADED}
  JSONFree;
{$ENDIF}

end.
