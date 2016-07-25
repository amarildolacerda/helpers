unit System.ThreadSafe;

interface

uses Classes, {System.contnrs,} System.Generics.Collections;

type

  TObjectLock = class(TObject)
  private
    FLock: TObject;
  public
    constructor create; virtual;
    destructor destroy; override;
    procedure Lock; virtual;
    procedure UnLock; virtual;
  end;

  TThreadSafeList = class(TThreadList)
  end;

  TThreadSafeStringList = class(TObjectLock)
  private
    FList: TStringList;
    function Getitems(idx: integer): string;
    procedure Setitems(idx: integer; const Value: string);
    function GetDelimitedText: string;
    function GetDelimiter: Char;
    procedure SetDelimitedText(const Value: string);
    procedure SetDelimiter(const Value: Char);
    function GetCommaText: string;
    procedure SetCommaText(const Value: string);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const Value: Char);
  public
    constructor create; override;
    destructor destroy; override;
    procedure Clear;
    function Count: integer;
    function IndexOf(AText: string): integer;
    function IndexOfName(AText: string): integer;
    procedure Add(sText: string; ADupl: boolean = true);
    procedure Delete(i: integer);
    function LockList: TStringList;
    procedure UnlockList; inline;
    property items[idx: integer]: string read Getitems write Setitems;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    function text: string;
    property CommaText: string read GetCommaText write SetCommaText;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    procedure AssingTo(str: TStringList);
    procedure AddTo(str: TStringList);
  end;

  TThreadSafeObjectList<T: Class> = class(TObjectLock)
  private
    FList: TObjectList<T>;
    function Getitems(idx: integer): T;
    procedure Setitems(idx: integer; const Value: T);
  protected
    FItemClass: TClass;
  public
    constructor create; overload; override;
    constructor create(item: TClass); overload; virtual;
    destructor destroy; override;
    function LockList: TObjectList<T>;
    procedure UnlockList;
    procedure Clear;
    function Add(Value: T): integer; overload;
    function Count: integer;
    property items[idx: integer]: T read Getitems write Setitems;
    function IndexOf(Value: T): integer;
    procedure Delete(idx: integer);
    function Add: T; overload;
  end;

  TThreadSafeInterfaceList<T: IInterface> = class(TObjectLock)
  private
    FOwned: boolean;
    FList: TList<T>;
    function Getitems(idx: integer): T;
    procedure Setitems(idx: integer; const Value: T);
  public
    procedure Insert(idx: integer; AValue: T);
    function Add(AValue: T): integer;
    function IndexOf(AValue: T): integer;
    procedure Delete(idx: integer);
    constructor create(AOwned: boolean = true); virtual;
    function Count: integer;
    procedure Clear;
    function LockList: TList<T>;
    procedure UnlockList;
    property items[idx: integer]: T read Getitems write Setitems;
  end;

implementation

constructor TThreadSafeStringList.create;
begin
  inherited create;
  FList := TStringList.create;
end;

destructor TThreadSafeStringList.destroy;
begin
  FList.Free;
  inherited;
end;

function TThreadSafeStringList.LockList: TStringList;
begin
  Lock;
  result := FList;
end;

procedure TThreadSafeStringList.UnlockList;
begin
  UnLock;
end;

procedure TThreadSafeStringList.Add(sText: string; ADupl: boolean = true);
begin
  try
    with LockList do
    begin
      if (not ADupl) and (IndexOf(sText) >= 0) then
        Exit;
      Add(sText);
    end;
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.Delete(i: integer);
begin
  try
    LockList.Delete(i);
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.Count: integer;
begin
  try
    result := LockList.Count;
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.Getitems(idx: integer): string;
begin
  try
    result := LockList.Strings[idx];
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.Setitems(idx: integer; const Value: string);
begin
  try
    LockList.Strings[idx] := Value;
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.text: string;
begin
  try
    result := LockList.text;
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.GetDelimitedText: string;
begin
  try
    result := LockList.DelimitedText;
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.GetDelimiter: Char;
begin

  try
    result := LockList.Delimiter;
  finally
    UnlockList;
  end;

end;

procedure TThreadSafeStringList.SetDelimitedText(const Value: string);
begin
  try
    LockList.DelimitedText := Value;
  finally
    UnlockList;
  end;

end;

procedure TThreadSafeStringList.SetDelimiter(const Value: Char);
begin
  try
    LockList.Delimiter := Value;
  finally
    UnlockList;
  end;

end;

function TThreadSafeStringList.GetCommaText: string;
var
  f: TStringList;
  i: integer;
begin
  f := LockList;
  try
    result := '';
    for i := 0 to f.Count - 1 do
    begin
      if i > 0 then
        result := result + ',';
      result := result + QuoteChar + f[i] + QuoteChar;
    end;
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.SetCommaText(const Value: string);
begin
  try
    LockList.CommaText := Value;
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.GetQuoteChar: Char;
begin
  try
    result := LockList.QuoteChar;
  finally
    UnlockList;
  end;

end;

function TThreadSafeStringList.IndexOf(AText: string): integer;
begin
  with LockList do
    try
      result := IndexOf(AText);
    finally
      UnlockList;
    end;
end;

function TThreadSafeStringList.IndexOfName(AText: string): integer;
begin
  try
    result := LockList.IndexOfName(AText);
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.SetQuoteChar(const Value: Char);
begin
  try
    LockList.QuoteChar := Value;
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.AddTo(str: TStringList);
begin
  try
    str.AddStrings(LockList);
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.AssingTo(str: TStringList);
begin
  try
    str.Assign(LockList);
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.Clear;
begin
  try
    LockList.Clear;
  finally
    UnlockList;
  end;
end;

{ TObjectLocked }

constructor TObjectLock.create;
begin
  inherited;
  FLock := TObject.create;
end;

destructor TObjectLock.destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TObjectLock.Lock;
begin
  System.TMonitor.Enter(FLock);
end;

procedure TObjectLock.UnLock;
begin
  System.TMonitor.Exit(FLock);
end;

{ TThreadObjectList }

function TThreadSafeObjectList<T>.Add(Value: T): integer;
begin
  with LockList do
    try
      result := FList.Count;
      FList.Add(Value);
    finally
      UnlockList;
    end;
end;

function TThreadSafeObjectList<T>.Add: T;
begin
  result := T(FItemClass.create);
end;

procedure TThreadSafeObjectList<T>.Clear;
begin
  with LockList do
    try
      Clear;
    finally
      UnlockList;
    end;
end;

function TThreadSafeObjectList<T>.Count: integer;
begin
  with LockList do
    try
      result := FList.Count;
    finally
      UnlockList;
    end;
end;

constructor TThreadSafeObjectList<T>.create(item: TClass);
begin
  create;
  FItemClass := item;
end;

constructor TThreadSafeObjectList<T>.create;
begin
  inherited;
  FList := TObjectList<T>.create;
end;

procedure TThreadSafeObjectList<T>.Delete(idx: integer);
begin
  with LockList do
    try
      Delete(idx);
    finally
      UnlockList;
    end;
end;

destructor TThreadSafeObjectList<T>.destroy;
begin
  FList.Free;
  inherited;
end;

function TThreadSafeObjectList<T>.Getitems(idx: integer): T;
begin
  with LockList do
    try
      result := FList.items[idx];
    finally
      UnlockList;
    end;
end;

function TThreadSafeObjectList<T>.IndexOf(Value: T): integer;
begin
  with LockList do
    try
      result := FList.IndexOf(Value);
    finally
      UnlockList;
    end;
end;

function TThreadSafeObjectList<T>.LockList: TObjectList<T>;
begin
  Lock;
  result := FList;
end;

procedure TThreadSafeObjectList<T>.Setitems(idx: integer; const Value: T);
begin
  with LockList do
    try
      FList.items[idx] := Value;
    finally
      UnlockList;
    end;
end;

procedure TThreadSafeObjectList<T>.UnlockList;
begin
  UnLock;
end;

{ TInterfaceThreadSafeList<T> }

function TThreadSafeInterfaceList<T>.Add(AValue: T): integer;
begin
  result := -1;
  Lock;
  try
    FList.Add(AValue);
    result := FList.Count - 1;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeInterfaceList<T>.Clear;
var
  ob: IInterface;
  i: integer;
begin
  with LockList do
    try
      try
        for i := Count - 1 downto 0 do
        begin
          ob := items[i];
          Delete(i);
          if FOwned then
            ob := nil;
        end;
      finally
        Clear; // garantir que todos os itens foram excluidos... redundante
      end;
    finally
      UnlockList;
    end;
end;

function TThreadSafeInterfaceList<T>.Count: integer;
begin
  with LockList do
    try
      result := Count;
    finally
      UnlockList;
    end;

end;

constructor TThreadSafeInterfaceList<T>.create(AOwned: boolean = true);
begin
  inherited create;
  FOwned := AOwned;
  FList := TList<T>.create;
end;

procedure TThreadSafeInterfaceList<T>.Delete(idx: integer);
var
  ob: T;
begin
  Lock;
  try
    ob := FList.items[idx];
    FList.Delete(idx);
    if FOwned then
      ob := nil;
  finally
    UnLock;
  end;
end;

function TThreadSafeInterfaceList<T>.Getitems(idx: integer): T;
begin
  with LockList do
    try
      result := T(items[idx]);
    finally
      UnlockList;
    end;
end;

function TThreadSafeInterfaceList<T>.LockList: TList<T>;
begin
  Lock;
  result := FList;
end;

procedure TThreadSafeInterfaceList<T>.Setitems(idx: integer; const Value: T);
begin
  with LockList do
    try
      FList.items[idx] := T;
    finally
      UnlockList;
    end;
end;


procedure TThreadSafeInterfaceList<T>.UnlockList;
begin
  UnLock;
end;

function TThreadSafeInterfaceList<T>.IndexOf(AValue: T): integer;
var
  i: integer;
  ob1, ob2: TObject;
begin
  result := -1;
  ob2 := AValue as TObject;
  Lock;
  try
    for i := 0 to FList.Count - 1 do
    begin
      ob1 := FList.items[i] as TObject;
      if ob1 = ob2 then
      begin
        result := i;
        Exit;
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeInterfaceList<T>.Insert(idx: integer; AValue: T);
begin
  Lock;
  try
    FList.Insert(idx, AValue);
  finally
    UnLock;
  end;
end;

end.
