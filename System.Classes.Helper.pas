unit System.Classes.Helper;

interface

uses System.Classes, System.SysUtils;


Type

  IFireEventProc = interface
    ['{BBC08E72-6518-4BF8-8BEE-0A46FD8B351C}']
    procedure SetOnEvent(const Value: TProc<TObject>);
    procedure FireEvent(Sender:TObject);
  end;

  TObject = class(System.TObject)
  private
    FOnFireEvent: TProc<TObject>;
    procedure SetOnFireEvent(const Value: TProc<TObject>);
    public
      procedure FireEvent;overload;
      procedure FireEvent(Sender:TObject);overload;
      property OnFireEvent:TProc<TObject> read FOnFireEvent write SetOnFireEvent;
   end;

   TObjectHelper = class helper for TObject
   public
      class procedure Using<T>(O:T; Proc: TProc<T>); static;
      procedure Anonimous<T:Class>( Proc: TProc<T> );
      procedure Run<T:Class>(Proc: TProc<T> );
      procedure Queue<T:Class>( Proc:TProc<T>);
      procedure Synchronize<T:Class>( Proc:TProc<T>);
  end;

implementation


class procedure TObjectHelper.Using<T>(O: T; Proc: TProc<T>);
var obj:TObject;
begin
  try
    Proc(O);
  finally
    freeAndNil(o);
  end;
end;


procedure TObject.FireEvent;
begin
   FireEvent(self);
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


{ TObject }

procedure TObject.FireEvent(Sender: TObject);
begin
   if assigned(FOnFireEvent) then
      FonFireEvent(Sender);
end;

procedure TObject.SetOnFireEvent(const Value: TProc<TObject>);
begin
  FOnFireEvent := Value;
end;


end.
