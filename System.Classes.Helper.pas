unit System.Classes.Helper;

interface

uses System.Classes, System.SysUtils;

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

  TObjectHelper = class helper for TObject
  private
  public
    class procedure Using<T>(O: T; Proc: TProc<T>); static;
    class function Anonimous<T: Class>(O: T; Proc: TProc<T>): TObject; static;
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

class function TObjectHelper.Anonimous<T>(O: T; Proc: TProc<T>): TObject;
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
  t: TThread;
begin
  t := TThread.CreateAnonymousThread(Proc);
  t.onTerminate := DoDestroyThread;
  Add(T);
  t.Start;
end;

procedure TTaskList.SetMaxThread(const Value: integer);
begin
  FMaxThread := Value;
end;

end.
