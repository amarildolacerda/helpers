unit System.Classes.Helper;

interface

uses System.Classes, System.SysUtils;


Type

  IFireEventProc = interface
    ['{BBC08E72-6518-4BF8-8BEE-0A46FD8B351C}']
    procedure SetOnEvent(const Value: TProc<TObject>);
    procedure FireEvent(Sender:TObject);
  end;

  TObjectExt = class(System.TObject)
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
      class function Anonimous<T:Class>(O:T; Proc: TProc<T> ):TObject;static;
      class procedure Run<T:Class>(O:T;Proc: TProc<T> );static;
      class function Queue<T:Class>(O:T; Proc:TProc<T>):TObject;static;
      class function Synchronize<T:Class>(O:T; Proc:TProc<T>):TObject;static;
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


procedure TObjectExt.FireEvent;
begin
   FireEvent(self);
end;

class function TObjectHelper.Queue<T>(O:T;Proc: TProc<T>):TObject;
begin
   result := O;
   TThread.Queue(nil,
       procedure
       begin
             Proc(O);
       end);
end;

class procedure TObjectHelper.Run<T>(O:T;Proc: TProc<T>);
begin
   TThread.CreateAnonymousThread(
           procedure
              begin
                 proc(O);
              end ).Start;
end;



class function TObjectHelper.Synchronize<T>(O:T;Proc: TProc<T>):TObject;
begin
   result := O;
   TThread.Synchronize(nil,
   procedure
   begin
      proc(O);
   end);
end;

class function TObjectHelper.Anonimous<T>(O:T;Proc: TProc<T>):TObject;
begin
   result := O;
   Proc(O);
end;


{ TObject }

procedure TObjectExt.FireEvent(Sender: TObject);
begin
   if assigned(FOnFireEvent) then
      FonFireEvent(Sender);
end;

procedure TObjectExt.SetOnFireEvent(const Value: TProc<TObject>);
begin
  FOnFireEvent := Value;
end;


end.
