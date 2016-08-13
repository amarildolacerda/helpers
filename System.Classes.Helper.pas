{ *************************************************************************** }
{ }
{ }
{ Copyright (C) Amarildo Lacerda }
{ }
{ https://github.com/amarildolacerda }
{ }
{ }
{ *************************************************************************** }
{ }
{ Licensed under the Apache License, Version 2.0 (the "License"); }
{ you may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at }
{ }
{ http://www.apache.org/licenses/LICENSE-2.0 }
{ }
{ Unless required by applicable law or agreed to in writing, software }
{ distributed under the License is distributed on an "AS IS" BASIS, }
{ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{ See the License for the specific language governing permissions and }
{ limitations under the License. }
{ }
{ *************************************************************************** }
unit System.Classes.Helper;

interface

uses System.Classes, System.SysUtils, System.Rtti, System.TypInfo, System.Json;

Type

  IFireEventProc = interface
    ['{BBC08E72-6518-4BF8-8BEE-0A46FD8B351C}']
    procedure SetOnEvent(const Value: TProc<TObject>);
    procedure FireEvent(Sender: TObject);
  end;

  TObjectExt = class(System.TObject)
  private
    FOnFireEvent: TProc<TObject>;
    procedure SetOnFireEvent(const Value: TProc<TObject>);
  public
    procedure FireEvent; overload;
    procedure FireEvent(Sender: TObject); overload;
    property OnFireEvent: TProc<TObject> read FOnFireEvent write SetOnFireEvent;
  end;

  TCustomAttributeClass = class of TCustomAttribute;
  TMemberVisibilitySet = set of TMemberVisibility;

  TObjectHelper = class helper for TObject
  private
    function GetProperties(AName: string): TValue;
    procedure SetProperties(AName: string; const Value: TValue);
    function GetFields(AName: string): TValue;
    procedure SetFields(AName: string; const Value: TValue);
    function GetMethods(AName: String): TRttiMethod;
  public
    // metodos anonimous
    class procedure Using<T>(O: T; Proc: TProc<T>); static;
    class function Anonymous<T: Class>(O: T; Proc: TProc<T>): TObject; static;
    class procedure Run<T: Class>(O: T; Proc: TProc<T>); overload; static;
    class procedure Run(Proc: TProc); overload; static;
    class procedure WaitFor<T: Class>(O: T; Proc: TProc<T>); overload; static;
    class procedure WaitFor(Proc: TProc); overload; static;
    class function Queue<T: Class>(O: T; Proc: TProc<T>): TObject;
      overload; static;
    class procedure Queue(Proc: TProc); overload; static;
    class function Synchronize<T: Class>(O: T; Proc: TProc<T>): TObject;
      overload; static;
    class procedure Synchronize(Proc: TProc); overload; static;


    // JSON
    function ToJson:string; overload;
    function ToJsonObject:TJsonObject;overload;
    procedure FromJson(AJson:string);overload;
    class function FromJson<T:Class, constructor>(AJson:string):T;overload;static;

    // RTTI
    function PropertyCount: Integer;
    function PropertyName(idx: Integer): string;
    property Properties[AName: string]: TValue read GetProperties
      write SetProperties;
    procedure GetPropertiesList(AList: TStrings;
      const AVisibility: TMemberVisibilitySet = [mvPublished, mvPublic]);
    procedure GetPropertiesItems(AList: TStrings;
      const AVisibility: TMemberVisibilitySet = [mvPublished, mvPublic]);

    function FieldsCount: Integer;
    function FieldName(idx: Integer): string;
    property Fields[AName: string]: TValue read GetFields write SetFields;
    procedure GetFieldsList(AList: TStrings;
      const AVisibility: TMemberVisibilitySet = [mvPublic]);

    property Methods[AName: String]: TRttiMethod read GetMethods;
    procedure GetMethodsList(AList: TStrings;
      const AVisibility: TMemberVisibilitySet = [mvPublic]);

    function HasAttribute(aMethod: TRttiMethod;
      attribClass: TCustomAttributeClass): Boolean;

    function InvokeAttribute(attribClass: TCustomAttributeClass;
      params: array of TValue): Boolean;
    function InvokeMethod(AName: string; params: array of TValue): Boolean;

  end;

  TTaskList = class(TThreadList)
  private
    FMaxThread: Integer;
    procedure SetMaxThread(const Value: Integer);
  public
    constructor create;
    procedure DoDestroyThread(Value: TObject);
    procedure Run(Proc: TProc);
    property MaxThread: Integer read FMaxThread write SetMaxThread;
  end;

implementation

uses System.DateUtils,  REST.Json;

class procedure TObjectHelper.Using<T>(O: T; Proc: TProc<T>);
var
  obj: TObject;
begin
  try
    Proc(O);
  finally
    freeAndNil(O);
  end;
end;

class procedure TObjectHelper.WaitFor(Proc: TProc);
begin
  TObject.WaitFor<TObject>(nil,
    procedure(Sender: TObject)
    begin
      Proc;
    end);
end;

class procedure TObjectHelper.WaitFor<T>(O: T; Proc: TProc<T>);
var
  th: TThread;
begin
  th := TThread.CreateAnonymousThread(
    procedure
    begin
      Proc(O);
    end);
  th.Start;
  th.WaitFor;
end;

procedure TObjectExt.FireEvent;
begin
  FireEvent(self);
end;

function TObjectHelper.FieldName(idx: Integer): string;
var
  aCtx: TRttiContext;
begin
  aCtx := TRttiContext.create;
  try
    result := ACtx.GetType(self.ClassType).GetFields[idx].Name;
  finally
    aCtx.Free;
  end;
end;

function TObjectHelper.FieldsCount: Integer;
var
  aCtx: TRttiContext;
begin
  aCtx := TRttiContext.create;
  try
    result := High(aCtx.GetType(self.ClassType).GetFields);
  finally
    aCtx.Free;
  end;
end;

procedure TObjectHelper.FromJson(AJson:string);
var oJs:TJsonObject;
begin
   oJs:=TJsonObject.ParseJSONValue(AJson) as TJSONObject ;
   TJson.JsonToObject(self,oJs);
end;

class function TObjectHelper.FromJson<T>(AJson: string): T;
begin
   result := TJson.JsonToObject<T>(AJson);
end;

function TObjectHelper.GetFields(AName: string): TValue;
var
  aCtx: TRttiContext;
  AField: TRttiField;
begin
  result := nil;
  aCtx := TRttiContext.create;
  try
    AField := aCtx.GetType(self.ClassType).GetField(AName);
    if assigned(AField) then
      result := AField.GetValue(self);
  finally
    aCtx.Free;
  end;
end;

procedure TObjectHelper.GetFieldsList(AList: TStrings;
const AVisibility: TMemberVisibilitySet = [mvPublic]);
var
  aCtx: TRttiContext;
  AFld: TRttiField;
begin
  AList.clear;
  aCtx := TRttiContext.create;
  try
    for AFld in aCtx.GetType(self.ClassType).GetFields do
    begin
      if AFld.Visibility in AVisibility then
        AList.Add(AFld.Name);
    end;
  finally
    aCtx.Free;
  end;
end;

function TObjectHelper.GetMethods(AName: String): TRttiMethod;
var
  aCtx: TRttiContext;
begin
  aCtx := TRttiContext.create;
  try
    result := aCtx.GetType(self.ClassType).GetMethod(AName);
  finally
    // ACtx.Free;
  end;
end;

procedure TObjectHelper.GetMethodsList(AList: TStrings;
const AVisibility: TMemberVisibilitySet = [mvPublic]);
var
  aMethod: TRttiMethod;
  aCtx: TRttiContext;
begin
  AList.clear;
  aCtx := TRttiContext.create;
  try
    for aMethod in aCtx.GetType(self.ClassType).GetMethods do
    begin
      if aMethod.Visibility in AVisibility then
        AList.Add(aMethod.Name);
    end;
  finally
    aCtx.Free;
  end;
end;

function TObjectHelper.GetProperties(AName: string): TValue;
var
  aCtx: TRttiContext;
  aProperty: TRttiProperty;
begin
  result := nil;
  aCtx := TRttiContext.create;
  try
    aProperty := aCtx.GetType(self.ClassType).GetProperty(AName);
    if assigned(aProperty) then
      result := aProperty.GetValue(self);
  finally
    aCtx.Free;
  end;
end;

type
  // Adiciona funções ao TValue
  TValueHelper = record helper for TValue
  private
    function IsNumeric: Boolean;
    function IsFloat: Boolean;
    function AsFloat: Extended;
    function IsBoolean: Boolean;
    function IsDate: Boolean;
    function IsDateTime: Boolean;
    function IsDouble: Boolean;
    function AsDouble: Double;
    function IsInteger: Boolean;
  end;

function TValueHelper.IsNumeric: Boolean;
begin
  Result := Kind in [tkInteger, tkChar, tkEnumeration, tkFloat,
    tkWChar, tkInt64];
end;

function TValueHelper.IsFloat: Boolean;
begin
  Result := Kind = tkFloat;
end;

function TValueHelper.AsFloat: Extended;
begin
  Result := AsType<Extended>;
end;

function TValueHelper.IsBoolean: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Boolean);
end;

function TValueHelper.IsDate: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDate);
end;

function TValueHelper.IsDateTime: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDatetime);
end;

function TValueHelper.IsDouble: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(Double);
end;

function TValueHelper.AsDouble: Double;
begin
  Result := AsType<Double>;
end;

function TValueHelper.IsInteger: Boolean;
begin
  Result := TypeInfo = System.TypeInfo(integer);
end;

function ISODateTimeToString(ADateTime: TDatetime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function ISOStrToDateTime(DateTimeAsString: string): TDatetime;
begin
  Result := EncodeDateTime(StrToInt(Copy(DateTimeAsString, 1, 4)),
    StrToInt(Copy(DateTimeAsString, 6, 2)),
    StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)),
    StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;

procedure TObjectHelper.GetPropertiesItems(AList: TStrings;
  const AVisibility: TMemberVisibilitySet);
var
  aCtx: TRttiContext;
  aProperty: TRttiProperty;
  aRtti: TRttiType;
  AValue: TValue;
begin
  AList.clear;
  aCtx := TRttiContext.Create;
  try
    aRtti := aCtx.GetType(self.ClassType);
    for aProperty in aRtti.GetProperties do
    begin
      if aProperty.Visibility in AVisibility then
      begin
        AValue := aProperty.GetValue(self);
        if AValue.IsDate or AValue.IsDateTime then
          AList.Add(aProperty.Name + '=' + ISODateTimeToString(AValue.AsDouble))
        else if AValue.IsBoolean then
          AList.Add(aProperty.Name + '=' + ord(AValue.AsBoolean).ToString)
        else
          AList.Add(aProperty.Name + '=' + AValue.ToString);
      end;
    end;
  finally
    aCtx.free;
  end;

end;

procedure TObjectHelper.GetPropertiesList(AList: TStrings;
const AVisibility: TMemberVisibilitySet = [mvPublished, mvPublic]);
var
  aCtx: TRttiContext;
  aProperty: TRttiProperty;
  aRtti: TRttiType;
begin

  AList.clear;
  aCtx := TRttiContext.create;
  try
    aRtti := aCtx.GetType(self.ClassType);
    for aProperty in aRtti.GetProperties do
    begin
      if aProperty.Visibility in AVisibility then
        AList.Add(aProperty.Name);
    end;
  finally
    aCtx.Free;
  end;

end;

function TObjectHelper.HasAttribute(aMethod: TRttiMethod;
attribClass: TCustomAttributeClass): Boolean;
var
  attributes: TArray<TCustomAttribute>;
  attrib: TCustomAttribute;
begin
  result := False;
  attributes := aMethod.GetAttributes;
  for attrib in attributes do
    if attrib.InheritsFrom(attribClass) then
      Exit(True);
end;

function TObjectHelper.InvokeAttribute(attribClass: TCustomAttributeClass;
params: array of TValue): Boolean;
var
  aCtx: TRttiContext;
  aMethod: TRttiMethod;
begin
  result := False;
  aCtx := TRttiContext.create;
  try
    for aMethod in aCtx.GetType(self.ClassType).GetMethods do
    begin
      if HasAttribute(aMethod, attribClass) then
      begin
        aMethod.Invoke(self, params);
        result := True;
      end;
    end;
  finally
    aCtx.Free;
  end;

end;

function TObjectHelper.InvokeMethod(AName: string;
params: array of TValue): Boolean;
var
  aMethod: TRttiMethod;
begin
  aMethod := Methods[AName];
  if not assigned(aMethod) then
    Exit(False);
  try
    aMethod.Invoke(self, params);
  finally
  end;
end;

function TObjectHelper.PropertyCount: Integer;
var
  aCtx: TRttiContext;
begin
  aCtx := TRttiContext.create;
  try
    result := High(aCtx.GetType(self.ClassType).GetProperties);
  finally
    aCtx.Free;
  end;
end;

function TObjectHelper.PropertyName(idx: Integer): string;
var
  aCtx: TRttiContext;
begin
  aCtx := TRttiContext.create;
  try
    result := aCtx.GetType(self.ClassType).GetProperties[idx].Name;
  finally
    aCtx.Free;
  end;
end;

class procedure TObjectHelper.Queue(Proc: TProc);
begin
  TThread.Queue(TThread.CurrentThread,
    procedure
    begin
      Proc;
    end);
end;

class function TObjectHelper.Queue<T>(O: T; Proc: TProc<T>): TObject;
begin
  result := O;
  TThread.Queue(TThread.CurrentThread,
    procedure
    begin
      Proc(O);
    end);
end;

class procedure TObjectHelper.Run(Proc: TProc);
begin
  TObject.Run<TObject>(TThread.CurrentThread,
    procedure(Sender: TObject)
    begin
      Proc;
    end);
end;

class procedure TObjectHelper.Run<T>(O: T; Proc: TProc<T>);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      Proc(O);
    end).Start;
end;

procedure TObjectHelper.SetFields(AName: string; const Value: TValue);
var
  AField: TRttiField;
  aCtx: TRttiContext;
begin
  aCtx := TRttiContext.create;
  try
    AField := aCtx.GetType(self.ClassType).GetField(AName);
    if assigned(AField) then
      AField.SetValue(self, Value);
  finally
    aCtx.Free;
  end;
end;

procedure TObjectHelper.SetProperties(AName: string; const Value: TValue);
var
  aProperty: TRttiProperty;
  aCtx: TRttiContext;
begin
  aCtx := TRttiContext.create;
  try
    aProperty := aCtx.GetType(self.ClassType).GetProperty(AName);
    if assigned(aProperty) then
      aProperty.SetValue(self, Value);
  finally
    aCtx.Free;
  end;
end;

class procedure TObjectHelper.Synchronize(Proc: TProc);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      Proc
    end);
end;

class function TObjectHelper.Synchronize<T>(O: T; Proc: TProc<T>): TObject;
begin
  result := O;
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      Proc(O);
    end);
end;

function TObjectHelper.toJson: string;
begin         // System.uJson
   result := TJson.ObjectToJsonString(self);
end;

function TObjectHelper.ToJsonObject: TJsonObject;
begin
   result := TJson.ObjectToJsonObject(self);
end;

class function TObjectHelper.Anonymous<T>(O: T; Proc: TProc<T>): TObject;
begin
  result := O;
  Proc(O);
end;

{ TObject }

procedure TObjectExt.FireEvent(Sender: TObject);
begin
  if assigned(FOnFireEvent) then
    FOnFireEvent(Sender);
end;

procedure TObjectExt.SetOnFireEvent(const Value: TProc<TObject>);
begin
  FOnFireEvent := Value;
end;

{ TThreadedPool }

constructor TTaskList.create;
begin
  inherited;
  FMaxThread := 10;
end;

procedure TTaskList.DoDestroyThread(Value: TObject);
begin
  Remove(Value);
end;

procedure TTaskList.Run(Proc: TProc);
var
  T: TThread;
begin
  T := TThread.CreateAnonymousThread(Proc);
  T.onTerminate := DoDestroyThread;
  Add(T);
  T.Start;
end;

procedure TTaskList.SetMaxThread(const Value: Integer);
begin
  FMaxThread := Value;
end;

end.
