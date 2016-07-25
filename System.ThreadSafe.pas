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

unit System.ThreadSafe;

interface

uses Classes, System.Generics.Collections;

type

  TObjectLock = class(TObject)
  private
    FLock: TObject;
  protected
    procedure Lock; virtual;
    procedure UnLock; virtual;
  public
    constructor create; virtual;
    destructor destroy; override;
  end;

  TThreadSafeList = TThreadList;
  /// Rename only

  TThreadSafeStringList = class(TObjectLock)
  private
    FList: TStringList;
    function Getitems(AIndex: integer): string;
    procedure Setitems(AIndex: integer; const AValue: string);
    function GetDelimitedText: string;
    function GetDelimiter: Char;
    procedure SetDelimitedText(const AValue: string);
    procedure SetDelimiter(const AValue: Char);
    function GetCommaText: string;
    procedure SetCommaText(const AValue: string);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const AValue: Char);
    function GetValues(AName: string): String;
    procedure SetValues(AName: string; const Value: String);
    function GetNames(AIndex: integer): String;
    procedure SetNames(AIndex: integer; const Value: String);
  public
    constructor create; override;
    destructor destroy; override;
    procedure Clear;
    function Count: integer;
    function IndexOf(AText: string): integer;
    function IndexOfName(AText: string): integer;
    procedure Add(AText: string; ADupl: boolean = true);
    procedure Delete(AIndex: integer);
    procedure Remove(AText: string);
    function LockList: TStringList;
    procedure UnlockList; inline;
    property Items[AIndex: integer]: string read Getitems write Setitems;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    function Text: string;
    property CommaText: string read GetCommaText write SetCommaText;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    procedure Assing(AStrings: TStrings);
    procedure AssingTo(AStrings: TStrings);
    procedure AddTo(AStrings: TStrings);
    property Values[AName: string]: String read GetValues write SetValues;
    property Names[AIndex: integer]: String read GetNames write SetNames;
  end;

  TThreadSafeObjectList<T: Class> = class(TObjectLock)
  private
    FList: TObjectList<T>;
    function Getitems(AIndex: integer): T;
    procedure Setitems(AIndex: integer; const AValue: T);
  protected
    FItemClass: TClass;
  public
    constructor create; overload; override;
    constructor create(AClass: TClass); overload; virtual;
    destructor destroy; override;
    function LockList: TObjectList<T>;
    procedure UnlockList;
    procedure Clear;
    function Add(AValue: T): integer; overload;
    function Count: integer;
    property Items[AIndex: integer]: T read Getitems write Setitems;
    function IndexOf(AValue: T): integer;
    procedure Delete(AIndex: integer);
    procedure Remove(AValue: T);
    function Add: T; overload;
  end;

  TThreadSafeInterfaceList<T: IInterface> = class(TObjectLock)
  private
    FOwned: boolean;
    FList: TList<T>;
    function Getitems(AIndex: integer): T;
    procedure Setitems(AIndex: integer; const AValue: T);
  public
    procedure Insert(AIndex: integer; AValue: T);
    function Add(AValue: T): integer;
    function IndexOf(AValue: T): integer;
    procedure Delete(AIndex: integer);
    procedure Remove(AValue: T);
    constructor create(AOwned: boolean = true); virtual;
    function Count: integer;
    procedure Clear;
    function LockList: TList<T>;
    procedure UnlockList;
    property Items[AIndex: integer]: T read Getitems write Setitems;
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

procedure TThreadSafeStringList.Remove(AText: string);
var
  i: integer;
begin
  i := IndexOf(AText);
  if i >= 0 then
    Delete(i);
end;

procedure TThreadSafeStringList.UnlockList;
begin
  UnLock;
end;

procedure TThreadSafeStringList.Add(AText: string; ADupl: boolean = true);
begin
  Lock;
  try
    begin
      if (not ADupl) and (FList.IndexOf(AText) >= 0) then
        Exit;
      FList.Add(AText);
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeStringList.Delete(AIndex: integer);
begin
  try
    LockList.Delete(AIndex);
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

function TThreadSafeStringList.Getitems(AIndex: integer): string;
begin
  try
    result := LockList.Strings[AIndex];
  finally
    UnlockList;
  end;
end;

function TThreadSafeStringList.GetNames(AIndex: integer): String;
begin
  with LockList do
    try
      result := Names[AIndex];
    finally
      UnlockList;
    end;
end;

procedure TThreadSafeStringList.Setitems(AIndex: integer; const AValue: string);
begin
  try
    LockList.Strings[AIndex] := AValue;
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.SetNames(AIndex: integer; const Value: String);
var
  sValue: String;
begin
  Lock;
  try
    sValue := FList.ValueFromIndex[AIndex];
    FList[AIndex] := Value + '=' + sValue;
  finally
    UnLock;
  end;
end;

function TThreadSafeStringList.Text: string;
begin
  try
    result := LockList.Text;
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

procedure TThreadSafeStringList.SetDelimitedText(const AValue: string);
begin
  try
    LockList.DelimitedText := AValue;
  finally
    UnlockList;
  end;

end;

procedure TThreadSafeStringList.SetDelimiter(const AValue: Char);
begin
  try
    LockList.Delimiter := AValue;
  finally
    UnlockList;
  end;

end;

function TThreadSafeStringList.GetCommaText: string;
var
  f: TStrings;
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

procedure TThreadSafeStringList.SetCommaText(const AValue: string);
begin
  try
    LockList.CommaText := AValue;
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

function TThreadSafeStringList.GetValues(AName: string): String;
begin
  with LockList do
    try
      result := Values[AName];
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

procedure TThreadSafeStringList.SetQuoteChar(const AValue: Char);
begin
  try
    LockList.QuoteChar := AValue;
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.SetValues(AName: string; const Value: String);
begin
  with LockList do
    try
      Values[AName] := Value;
    finally
      UnlockList;
    end;
end;

procedure TThreadSafeStringList.AddTo(AStrings: TStrings);
begin
  try
    AStrings.AddStrings(LockList);
  finally
    UnlockList;
  end;
end;

procedure TThreadSafeStringList.Assing(AStrings: TStrings);
begin
  with LockList do
    try
      Assign(AStrings);
    finally
      UnlockList;
    end;
end;

procedure TThreadSafeStringList.AssingTo(AStrings: TStrings);
begin
  try
    AStrings.Assign(LockList);
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
  System.TMonitor.Enter(FLock); // Bloqueia o objeto
end;

procedure TObjectLock.UnLock;
begin
  System.TMonitor.Exit(FLock); // Libera o objeto
end;

{ TThreadObjectList }

function TThreadSafeObjectList<T>.Add(AValue: T): integer;
begin
  with LockList do
    try
      result := FList.Count;
      FList.Add(AValue);
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

constructor TThreadSafeObjectList<T>.create(AClass: TClass);
begin
  create;
  FItemClass := AClass;
end;

constructor TThreadSafeObjectList<T>.create;
begin
  inherited;
  FList := TObjectList<T>.create;
end;

procedure TThreadSafeObjectList<T>.Delete(AIndex: integer);
begin
  with LockList do
    try
      Delete(AIndex);
    finally
      UnlockList;
    end;
end;

destructor TThreadSafeObjectList<T>.destroy;
begin
  FList.Free;
  inherited;
end;

function TThreadSafeObjectList<T>.Getitems(AIndex: integer): T;
begin
  with LockList do
    try
      result := FList.Items[AIndex];
    finally
      UnlockList;
    end;
end;

function TThreadSafeObjectList<T>.IndexOf(AValue: T): integer;
begin
  with LockList do
    try
      result := FList.IndexOf(AValue);
    finally
      UnlockList;
    end;
end;

function TThreadSafeObjectList<T>.LockList: TObjectList<T>;
begin
  Lock;
  result := FList;
end;

procedure TThreadSafeObjectList<T>.Remove(AValue: T);
var
  i: integer;
begin
  i := IndexOf(AValue);
  if i >= 0 then
    Delete(i);
end;

procedure TThreadSafeObjectList<T>.Setitems(AIndex: integer; const AValue: T);
begin
  with LockList do
    try
      FList.Items[AIndex] := AValue;
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
          ob := Items[i];
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

procedure TThreadSafeInterfaceList<T>.Delete(AIndex: integer);
var
  ob: T;
begin
  Lock;
  try
    ob := FList.Items[AIndex];
    FList.Delete(AIndex);
    if FOwned then
      ob := nil;
  finally
    UnLock;
  end;
end;

function TThreadSafeInterfaceList<T>.Getitems(AIndex: integer): T;
begin
  with LockList do
    try
      result := T(Items[AIndex]);
    finally
      UnlockList;
    end;
end;

function TThreadSafeInterfaceList<T>.LockList: TList<T>;
begin
  Lock;
  result := FList;
end;

procedure TThreadSafeInterfaceList<T>.Remove(AValue: T);
var
  i: integer;
begin
  i := IndexOf(AValue);
  if i >= 0 then
    Delete(i);
end;

procedure TThreadSafeInterfaceList<T>.Setitems(AIndex: integer;
  const AValue: T);
begin
  with LockList do
    try
      FList.Items[AIndex] := T;
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
      ob1 := FList.Items[i] as TObject;
      if ob1.Equals(ob2) then
      begin
        result := i;
        Exit;
      end;
    end;
  finally
    UnLock;
  end;
end;

procedure TThreadSafeInterfaceList<T>.Insert(AIndex: integer; AValue: T);
begin
  Lock;
  try
    FList.Insert(AIndex, AValue);
  finally
    UnLock;
  end;
end;

end.
