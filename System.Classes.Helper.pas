unit System.Classes.Helper;

interface

uses System.Classes, System.SysUtils, System.Rtti;

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

    // RTTI
    property Properties[AName: string]: TValue read GetProperties
      write SetProperties;
    procedure GetPropertiesList(AList: TStrings);
    property Fields[AName: string]: TValue read GetFields write SetFields;
    property Methods[AName: String]: TRttiMethod read GetMethods;
    function HasAttribute(aMethod: TRttiMethod;
      attribClass: TCustomAttributeClass): Boolean;
    function InvokeAttribute(attribClass: TCustomAttributeClass;
      params: array of TValue): Boolean;
    function InvokeMethod(AName: string; params: array of TValue): Boolean;

  end;

  TTaskList = class(TThreadList)
  private
    FMaxThread: integer;
    procedure SetMaxThread(const Value: integer);
  public
    constructor create;
    procedure DoDestroyThread(Value: TObject);
    procedure Run(Proc: TProc);
    property MaxThread: integer read FMaxThread write SetMaxThread;
  end;

implementation

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

function TObjectHelper.GetFields(AName: string): TValue;
var
  ACtx: TRttiContext;
  AField: TRttiField;
begin
  result := nil;
  ACtx := TRttiContext.create;
  try
    AField := ACtx.GetType(self.ClassType).GetField(AName);
    if assigned(AField) then
      result := AField.GetValue(self);
  finally
    ACtx.Free;
  end;
end;

function TObjectHelper.GetMethods(AName: String): TRttiMethod;
var
  ACtx: TRttiContext;
begin
  ACtx := TRttiContext.create;
  try
    result := ACtx.GetType(self.ClassType).GetMethod(AName);
  finally
    // ACtx.Free;
  end;
end;

function TObjectHelper.GetProperties(AName: string): TValue;
var
  ACtx: TRttiContext;
  aProperty: TRttiProperty;
begin
  result := nil;
  ACtx := TRttiContext.create;
  try
    aProperty := ACtx.GetType(self.ClassType).GetProperty(AName);
    if assigned(aProperty) then
      result := aProperty.GetValue(self);
  finally
    ACtx.Free;
  end;
end;

procedure TObjectHelper.GetPropertiesList(AList: TStrings);
var
  ACtx: TRttiContext;
  aProperty: TRttiProperty;
  aRtti: TRttiType;
begin

  AList.Clear;
  ACtx := TRttiContext.create;
  try
    aRtti := ACtx.GetType(self.ClassType);
    for aProperty in aRtti.GetProperties do
    begin
      AList.Add(aProperty.Name);
    end;
  finally
    ACtx.Free;
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
  ACtx: TRttiContext;
  aMethod: TRttiMethod;
begin
  result := False;
  ACtx := TRttiContext.create;
  try
    for aMethod in ACtx.GetType(self.ClassType).GetMethods do
    begin
      if HasAttribute(aMethod, attribClass) then
      begin
        aMethod.Invoke(self, params);
        result := True;
      end;
    end;
  finally
    ACtx.Free;
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
    aMethod.DisposeOf;
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
  ACtx: TRttiContext;
begin
  ACtx := TRttiContext.create;
  try
    AField := ACtx.GetType(self.ClassType).GetField(AName);
    if assigned(AField) then
      AField.SetValue(self, Value);
  finally
    ACtx.Free;
  end;
end;

procedure TObjectHelper.SetProperties(AName: string; const Value: TValue);
var
  aProperty: TRttiProperty;
  ACtx: TRttiContext;
begin
  ACtx := TRttiContext.create;
  try
    aProperty := aCtx.GetType(self.ClassType).GetProperty(AName);
    if assigned(aProperty) then
      aProperty.SetValue(self, Value);
  finally
    ACtx.Free;
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

procedure TTaskList.SetMaxThread(const Value: integer);
begin
  FMaxThread := Value;
end;

end.
