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

{
  Alterações:
  25/03/16 - Primeira versão publicada - Construção
  25/07/16 - refactoring na interface para acoplar o dataset.. Connector
}

unit Data.QueryIntf;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  System.Rtti, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.ExprFuncs,
  {FireDAC.Phys.SQLiteDef,} FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, Data.DB, FireDAC.Comp.Client;

type

  TDatasetClass = class of TDataset;
  IQuery = interface;
  // TQueryIntf = class;

  TDataStorageRec = record
    class function NewQuery(const Conn: TFDCustomConnection): IQuery;
      overload; static;
    class function NewQuery(): IQuery; overload; static;
    class function NewQuery(const ATable: string; const AFields: String = '*';
      const AWhere: String = ''; const AGroup: string = '';
      const AOrderBy: string = ''; const AJoin: String = ''): IQuery;
      overload; static;
    class function SqlBuilder(const ATable: string; const AFields: String = '*';
      const AWhere: String = ''; const AGroup: String = '';
      const AOrderBy: string = ''; const AJoin: String = ''): string;
      overload; static;
  end;

  IJsonDataset = interface
    ['{535F0BD0-6535-452B-9E84-6A9BFCFCEB91}']
    function ToJson: string;
  end;

  IDataset = interface(IJsonDataset)
    procedure Next;
    procedure Prior;
    procedure Last;
    procedure First;
    function eof: boolean;
    function bof: boolean;
    procedure Insert;
    procedure Append;
    procedure post;
    procedure edit;
    procedure delete;
    function GetActiveQuery: boolean;
    procedure SetActiveQuery(const Value: boolean);
    property Active: boolean read GetActiveQuery Write SetActiveQuery;
    function GetFields: TFields;
    property Fields: TFields read GetFields;
    function RowsAffected: integer;
    function GetCommand: string;
    procedure SetCommand(const ACommand: string);
    property Command: string read GetCommand write SetCommand;
  end;

  IDatasetIntf = interface(IDataset)
    function Open: IQuery;
    function Close: IQuery;
    function StartTransaction: IQuery;
    function Commit: IQuery;
    function Rollback: IQuery;
    function RecordCountInt: integer;
    property RecordCount: integer read RecordCountInt;
  end;

  IQuery = Interface(IDatasetIntf)
    ['{4C9E016E-41A2-42D1-899E-D820FE4FA9C4}']
    function ExecSql(const AScript: string): IQuery;
    function GetParams: TFDParams;
    property Params: TFDParams read GetParams;

    // Enumerator
    function GetEnumerator: IQuery;
    function GetCurrent: TFields;
    property Current: TFields read GetCurrent;
    function MoveNext: boolean;
    procedure Reset;

    // Open
    function Open(const AProc: TProc<TFDParams>): IQuery; overload;
    function DoLoop(const AProc: TProc<TDataset>): IQuery;
    function DoQuery(const AProc: TProc<TDataset>): IQuery;
    function Clone: IQuery;

    // Filters
    function ConnectName(texto: String): IQuery;
    function Where(const texto: string): IQuery;
    function Table(const texto: string): IQuery;
    function Join(const texto: string): IQuery;
    function GroupBy(const texto: string): IQuery;
    function OrderBy(const texto: string): IQuery;
    function AndWhere(const texto: string): IQuery;
    function OrWhere(const texto: string): IQuery;
    function FilterResult(const texto: string): IQuery;
    function FieldNames(const texto: string): IQuery;
    function ParamValue(const nome: string; valor: variant): IQuery;
    function FieldValue(const nome: string; valor: variant): IQuery;

    function GetCmdFields: string;
    function GetCmdJoin: string;
    function GetCmdOrderBy: string;
    function GetCmdTable: string;
    function GetCmdWhere: string;
    procedure SetCmdWhere(const Value: string);
    procedure SetCmdFiedls(const Value: string);
    procedure SetCmdJoin(const Value: string);
    procedure SetCmdOrderBy(const Value: string);
    procedure SetCmdTable(const Value: string);
    function GetCmdGroup: String;
    procedure SetCmdGroup(const Value: String);

    property CmdWhere: string read GetCmdWhere write SetCmdWhere;
    property CmdTable: string read GetCmdTable write SetCmdTable;
    property CmdFields: string read GetCmdFields write SetCmdFiedls;
    property CmdJoin: string read GetCmdJoin write SetCmdJoin;
    property CmdOrderBy: string read GetCmdOrderBy write SetCmdOrderBy;
    property CmdGroup: string read GetCmdGroup write SetCmdGroup;

    function GetDataset: TFDQuery;
    procedure SetDataset(const Value: TFDQuery);
    property Dataset: TFDQuery read GetDataset write SetDataset;

  End;

  (* IDatabase = interface
    ['{09A79486-12B6-4944-9F4C-8CF841D901E5}']
    function Open:IDatabase;
    function Close:IDatabase;
    function ParamValue(sParam:string;value:variant):IDatabase;
    function Driver(ADriver:string):IDatabase;
    function ConnectName(ADBname:string):IDatabase;
    function LoginParam(AUser:string;APass:String):IDatabase;
    end;

    TDatabaseIntf = class(TComponent,IDatabase)
    private
    FDatabase:TFDConnection;
    public

    class function new:IDatabase;
    function Open:IDatabase;virtual;
    function Close:IDatabase;virtual;
    function ParamValue(sParam:string;value:variant):IDatabase;
    function Driver(ADriver:string):IDatabase;
    function ConnectName(ADBname:string):IDatabase;
    function LoginParam(AUser:string;APass:String):IDatabase;
    end;
  *)

   TQueryIntfBase =  TFDQuery;

  TQueryIntf<T:TFDQuery> = class(TComponent, IQuery)
  private
    FFreeOnDestroy: boolean;
    FScrollRow: integer;
    FDataset: TFDQuery;
    FWhere: string;
    FGroup: string;
    FJoin: string;
    FTable: string;
    FFieldNames: string;
    FOrderBy: string;
    procedure SetFieldByName(const AField: string; const AValue: variant);
    function GetActiveQuery: boolean;
    procedure SetActiveQuery(const Value: boolean);
    function GetCmdFields: string;
    function GetCmdJoin: string;
    function GetCmdOrderBy: string;
    function GetCmdTable: string;
    function GetCmdWhere: string;
    procedure SetCmdWhere(const Value: string);
    procedure SetCmdFiedls(const Value: string);
    procedure SetCmdJoin(const Value: string);
    procedure SetCmdOrderBy(const Value: string);
    procedure SetCmdTable(const Value: string);
    procedure Next;
    procedure Prior;
    procedure Last;
    procedure First;
    procedure Insert;
    procedure Append;
    procedure Reset;
    function Clone: IQuery;
    function Commit: IQuery;
    function DoLoop(const AProc: TProc<TDataset>): IQuery;
    function DoQuery(const AProc: TProc<TDataset>): IQuery;
    function ExecSql(const AScript: string): IQuery;
    function Open(const AProc: TProc<TFDParams>): IQuery; overload;
    function Open: IQuery; overload;
    function Rollback: IQuery;
    function StartTransaction: IQuery;
    function MoveBy(Distance: integer): integer;
    procedure post;
    procedure edit;
    procedure delete;
    function GetCmdGroup: String;
    procedure SetCmdGroup(const Value: String);
    procedure SetDataset(Const ADataset: TFDQuery);
    function GetDataset: TFDQuery;
    procedure SetFreeOnDestroy(const Value: boolean);
  protected
  public
    property FreeOnDestroy:boolean read FFreeOnDestroy write SetFreeOnDestroy;
    function GetQuery: T;
    property Dataset: TFDQuery read GetDataset Write SetDataset;
    class function New: IQuery; overload; static;
    class function New(ADataset: T): IQuery; overload; static;
    class function New(const AConnection: TFDCustomConnection): IQuery;
      overload;
    function Close: IQuery; virtual;
    function ConnectName(texto: String): IQuery; virtual;

    function RowsAffected: integer;
    function eof: boolean;
    function bof: boolean;
    function GetCurrent: TFields;
    function MoveNext: boolean;
    function GetCommand: string;
    procedure SetCommand(const ACommand: string);
    procedure SetConnectionIntf(const AConn: TFDCustomConnection);
    function GetConnectionIntf: TFDCustomConnection;

    function GetFields: TFields;
    function GetEnumerator: IQuery;
    function GetParams: TFDParams;
    function ParamValue(const nome: string; valor: variant): IQuery;
    function FieldValue(const nome: string; valor: variant): IQuery;
    function NewQuery(ATable, AFields, AWhere, AGroup, AOrderBy,
      AJoin: String): IQuery;
    constructor create(AOwner: TComponent); overload; override;
    constructor create(); overload;
    destructor Destroy; override;
    function RebuildSql: IQuery;
    function GroupBy(const texto: string): IQuery;

    function Where(const texto: string): IQuery; virtual;
    function Table(const texto: string): IQuery; virtual;
    function FieldNames(const texto: string): IQuery; virtual;
    function Join(const texto: string): IQuery; virtual;
    function OrderBy(const texto: string): IQuery; virtual;
    function AndWhere(const texto: string): IQuery; virtual;
    function OrWhere(const texto: string): IQuery; virtual;
    function FilterResult(const texto: string): IQuery;
    function RecordCountInt: integer;
    function ToJson: string;

    property CmdFields: string read GetCmdFields write SetCmdFiedls;
    property CmdTable: string read GetCmdTable write SetCmdTable;
    property CmdWhere: string read GetCmdWhere write SetCmdWhere;
    property CmdJoin: string read GetCmdJoin write SetCmdJoin;
    property CmdGroup: String read GetCmdGroup write SetCmdGroup;
    property CmdOrderBy: string read GetCmdOrderBy write SetCmdOrderBy;

  end;

implementation

{ TQueyIntf }

function TQueryIntf<T>.Rollback: IQuery;
begin
  TQueryIntfBase(FDataset) .Connection.Rollback;
  result := self;
end;

function TQueryIntf<T>.RowsAffected: integer;
begin
  result := TQueryIntfBase(FDataset).RowsAffected;
end;

function TQueryIntf<T>.AndWhere(const texto: string): IQuery;
begin
  result := self;
  if FWhere <> '' then
    FWhere := FWhere + ' and ';
  FWhere := FWhere + texto;
  RebuildSql;
end;

procedure TQueryIntf<T>.Append;
begin
  TQueryIntfBase(FDataset).Append;
end;

function TQueryIntf<T>.bof: boolean;
begin
  result := TQueryIntfBase(FDataset).bof;
end;

function TQueryIntf<T>.Clone: IQuery;
begin
  result := TQueryIntf<T>.create(nil) as IQuery;
  if result.CmdTable <> '' then
  begin
    result.CmdTable := self.CmdTable;
    result.CmdWhere := self.CmdWhere;
    result.CmdJoin := self.CmdJoin;
    result.CmdFields := self.CmdFields;
    result.CmdOrderBy := self.CmdOrderBy;
    result.CmdGroup := self.CmdGroup;
  end
  else
    result.Dataset.sql.assign(TQueryIntfBase(FDataset).sql);
  result.Dataset.Connection := self.Dataset.Connection;
end;

function TQueryIntf<T>.Close: IQuery;
begin
  result := self;
  TQueryIntfBase(FDataset).Close;
end;

function TQueryIntf<T>.Commit: IQuery;
begin
  result := self;
  Dataset.Connection.Commit;
end;

function TQueryIntf<T>.ConnectName(texto: String): IQuery;
begin
  result := self;
  Dataset.ConnectionName := texto;
end;

constructor TQueryIntf<T>.create();
begin
  inherited create(nil);
  FFreeOnDestroy := true;
  FDataset := T.Create(nil);

  if not supports(FDataset, IQuery) then
  begin
    FreeAndNil(FDataset);
    raise exception.create('Não suporta interface IQuery');
  end;

end;

constructor TQueryIntf<T>.create(AOwner: TComponent);
begin
  inherited;
  FFreeOnDestroy := false;
  if supports(AOwner, IJsonDataset) then
    FDataset := T(AOwner)
  else
  begin
    FDataset := T.create(self);
    if not supports(FDataset, IQuery) then
    begin
      FreeAndNil(FDataset);
      raise exception.create('Não suporta interface IQuery');
    end;
    FFreeOnDestroy := true;
  end;

  FFieldNames := '*';
  FScrollRow := -1;

end;

procedure TQueryIntf<T>.delete;
begin
  FDataset.delete;
end;

destructor TQueryIntf<T>.Destroy;
begin
  if FFreeOnDestroy then
    FreeAndNil(FDataset);
  inherited;
end;

function TQueryIntf<T>.DoLoop(const AProc: TProc<TDataset>): IQuery;
var
  book: TBookmark;
begin
  result := self;
  FDataset.DisableControls;
  try
    book := FDataset.GetBookmark;
    try
      First;
      while not eof do
      begin
        try
          AProc(self.FDataset);
          Next;
        except
          break;
        end;
      end;

    finally
      FDataset.GotoBookmark(book);
      FDataset.FreeBookmark(book);
    end;
  finally
    FDataset.EnableControls;
  end;
end;

function TQueryIntf<T>.DoQuery(const AProc: TProc<TDataset>): IQuery;
begin
  result := self;
  AProc(self.FDataset);
end;

procedure TQueryIntf<T>.edit;
begin
  FDataset.edit;
end;

function TQueryIntf<T>.eof: boolean;
begin
  result := FDataset.eof;
end;

function TQueryIntf<T>.ExecSql(const AScript: string): IQuery;
begin
  result := self;
  FDataset.sql.Text := AScript;
  FDataset.Execute();
end;

function TQueryIntf<T>.FieldNames(const texto: string): IQuery;
begin
  result := self;
  FFieldNames := texto;
  RebuildSql;
end;

function TQueryIntf<T>.FieldValue(const nome: string; valor: variant): IQuery;
var
  fld: TField;
begin
  result := self;
  fld := FDataset.FindField(nome);
  if fld <> nil then
    fld.Value := valor;
end;

function TQueryIntf<T>.FilterResult(const texto: string): IQuery;
begin
  result := self;
  FDataset.Filter := texto;
  FDataset.Filtered := texto <> '';
end;

procedure TQueryIntf<T>.First;
begin
  FDataset.First;
end;

function TQueryIntf<T>.Join(const texto: string): IQuery;
begin
  result := self;
  FJoin := texto;
  RebuildSql;
end;

procedure TQueryIntf<T>.Last;
begin
  FDataset.Last;
end;

function TQueryIntf<T>.GetActiveQuery: boolean;
begin
  result := FDataset.Active;
end;

function TQueryIntf<T>.GetCommand: string;
begin
  result := FDataset.sql.Text;
end;

function TQueryIntf<T>.GetConnectionIntf: TFDCustomConnection;
begin
  result := FDataset.Connection;
end;

function TQueryIntf<T>.GetCurrent: TFields;
begin
  result := FDataset.Fields;
end;

function TQueryIntf<T>.GetDataset: TFDQuery;
begin
  result := T(FDataset);
end;

function TQueryIntf<T>.GetEnumerator: IQuery;
begin
  result := self as IQuery;
end;

function TQueryIntf<T>.GetFields: TFields;
begin
  result := FDataset.Fields;
end;

function TQueryIntf<T>.GetParams: TFDParams;
begin
  result := FDataset.Params;
end;

function TQueryIntf<T>.GetQuery: T;
begin
  result := T(FDataset);
end;

function TQueryIntf<T>.GroupBy(const texto: string): IQuery;
begin
  result := self;
  FGroup := texto;
  RebuildSql;
end;

procedure TQueryIntf<T>.Insert;
begin
  FDataset.Insert;
end;

function TQueryIntf<T>.GetCmdFields: string;
begin
  result := FFieldNames;
end;

function TQueryIntf<T>.GetCmdGroup: String;
begin
  result := FGroup;
end;

function TQueryIntf<T>.GetCmdJoin: string;
begin
  result := FJoin;
end;

function TQueryIntf<T>.GetCmdOrderBy: string;
begin
  result := FOrderBy;
end;

function TQueryIntf<T>.GetCmdTable: string;
begin
  result := FTable;
end;

function TQueryIntf<T>.GetCmdWhere: string;
begin
  result := FWhere;
end;

function TQueryIntf<T>.MoveBy(Distance: integer): integer;
begin
  result := FDataset.MoveBy(Distance);
  if eof then
    FScrollRow := -1;
end;

function TQueryIntf<T>.MoveNext: boolean;
begin
  if FScrollRow >= 0 then
    Next;
  inc(FScrollRow);
  result := not eof;
end;

class function TQueryIntf<T>.New: IQuery;
begin
  result := TQueryIntf<T>.create(nil) as IQuery;
end;

class function TQueryIntf<T>.New(ADataset: T): IQuery;
begin
  result := TQueryIntf<T>.create(ADataset) as IQuery;
end;

class function TQueryIntf<T>.New(const AConnection
  : TFDCustomConnection): IQuery;
begin
  result := New() as IQuery;
  result.Dataset.Connection := AConnection;
end;

function TQueryIntf<T>.NewQuery(ATable, AFields, AWhere, AGroup, AOrderBy,
  AJoin: String): IQuery;
begin
  result := New;
  // TDataStorageRec.NewQuery(ATable, AFields, AWhere,AGroup, AOrderBy, AJoin);
  result.CmdTable := ATable;
  result.CmdWhere := AWhere;
  result.CmdFields := AFields;
  result.CmdJoin := AJoin;
  result.CmdGroup := AGroup;
  result.CmdOrderBy := AOrderBy;
  result.Dataset.Connection := self.Dataset.Connection;
end;

procedure TQueryIntf<T>.Next;
begin
  FDataset.Next;
end;

function TQueryIntf<T>.Open(const AProc: TProc<TFDParams>): IQuery;
begin
  AProc(GetParams);
  result := self;
  FDataset.Open;
end;

function TQueryIntf<T>.OrderBy(const texto: string): IQuery;
begin
  result := self;
  FOrderBy := texto;
  RebuildSql;
end;

function TQueryIntf<T>.OrWhere(const texto: string): IQuery;
begin
  result := self;
  if FWhere <> '' then
    FWhere := FWhere + ' or ';
  FWhere := FWhere + texto;
  RebuildSql;

end;

function TQueryIntf<T>.ParamValue(const nome: string; valor: variant): IQuery;
var
  prm: TFDParam;
begin
  result := self;
  prm := FDataset.FindParam(nome);
  if prm <> nil then
    prm.Value := valor;
end;

procedure TQueryIntf<T>.post;
begin
  FDataset.post;
end;

procedure TQueryIntf<T>.Prior;
begin
  FDataset.Prior;
end;

function TQueryIntf<T>.RebuildSql: IQuery;
begin
  result := self;
  SetCommand(TDataStorageRec.SqlBuilder(FTable, FFieldNames, FWhere, FGroup,
    FOrderBy, FJoin));
end;

function TQueryIntf<T>.RecordCountInt: integer;
begin
  result := FDataset.RecordCount;
end;

procedure TQueryIntf<T>.Reset;
begin

end;

function TQueryIntf<T>.Open: IQuery;
begin
  FDataset.Open;
  FScrollRow := -1;
  result := self as IQuery;
end;

procedure TQueryIntf<T>.SetActiveQuery(const Value: boolean);
begin
  FDataset.Active := Value;
end;

procedure TQueryIntf<T>.SetCommand(const ACommand: string);
begin
  FDataset.sql.Text := ACommand;
end;

procedure TQueryIntf<T>.SetConnectionIntf(const AConn: TFDCustomConnection);
begin
  FDataset.Connection := AConn;
end;

procedure TQueryIntf<T>.SetDataset(Const ADataset: TFDQuery);
begin
  if FFreeOnDestroy and (not ADataset.Equals(FDataset)) then
    FreeAndNil(FDataset);

  FDataset := T(ADataset);
  FFreeOnDestroy := false;
end;

procedure TQueryIntf<T>.SetFieldByName(const AField: string;
  const AValue: variant);
begin
  FDataset.FieldByName(AField).Value := AValue;
end;

procedure TQueryIntf<T>.SetFreeOnDestroy(const Value: boolean);
begin
  FFreeOnDestroy := Value;
end;

procedure TQueryIntf<T>.SetCmdWhere(const Value: string);
begin
  FWhere := Value;
  RebuildSql;
end;

procedure TQueryIntf<T>.SetCmdFiedls(const Value: string);
begin
  FFieldNames := Value;
  RebuildSql;
end;

procedure TQueryIntf<T>.SetCmdGroup(const Value: String);
begin
  FGroup := Value;
  RebuildSql;
end;

procedure TQueryIntf<T>.SetCmdJoin(const Value: string);
begin
  FJoin := Value;
  RebuildSql;
end;

procedure TQueryIntf<T>.SetCmdOrderBy(const Value: string);
begin
  FOrderBy := Value;
  RebuildSql;
end;

procedure TQueryIntf<T>.SetCmdTable(const Value: string);
begin
  FTable := Value;
  RebuildSql;
end;

function TQueryIntf<T>.StartTransaction: IQuery;
begin
  result := self;
  FDataset.Connection.StartTransaction;
end;

function TQueryIntf<T>.Table(const texto: string): IQuery;
begin
  result := self;
  FTable := texto;
  RebuildSql;
end;

function TQueryIntf<T>.ToJson: string;
var
  Intf: IJsonDataset;
begin
  if supports(FDataset, IJsonDataset, Intf) then
    result := (Intf).ToJson
  else
    result := '{ }';
end;

function TQueryIntf<T>.Where(const texto: string): IQuery;
begin
  result := self;
  FWhere := texto;
  RebuildSql;
end;

{ TStoreDataStorage }

class function TDataStorageRec.NewQuery: IQuery;
begin
  result := TQueryIntf<TFDQuery>.create(nil) as IQuery;
end;

class function TDataStorageRec.NewQuery(const Conn
  : TFDCustomConnection): IQuery;
begin
  result := TDataStorageRec.NewQuery();
  result.Dataset.Connection := Conn;
end;

class function TDataStorageRec.NewQuery(const ATable, AFields, AWhere, AGroup,
  AOrderBy, AJoin: String): IQuery;
begin
  result := NewQuery();
  result.Table(ATable).Where(AWhere).OrderBy(AOrderBy).Join(AJoin)
    .FieldNames(AFields);
  result.GroupBy(AGroup);

end;

class function TDataStorageRec.SqlBuilder(const ATable, AFields, AWhere, AGroup,
  AOrderBy, AJoin: String): string;
var
  sql: TStringBuilder;
begin
  sql := TStringBuilder.create;
  try
    if ATable <> '' then
      sql.Append('select ' + AFields + ' from ' + ATable);
    if AJoin <> '' then
      sql.Append(' ' + AJoin);
    if AWhere <> '' then
      sql.Append(' where ' + AWhere);
    if AGroup <> '' then
      sql.Append(' group by ' + AGroup);
    if AOrderBy <> '' then
      sql.Append(' order by ' + AOrderBy);
    result := sql.ToString;
  finally
    sql.Free;
  end;
end;

{ TDatabaseIntf }

(*

  function TDatabaseIntf.ConnectName(ADBname: string): IDatabase;
  begin
  result := self;
  FDatabase.ConnectionName:=ADBname;
  end;

  function TDatabaseIntf.Driver(ADriver: string): IDatabase;
  begin
  result := self;
  inherited driverName := ADriver;
  end;

  function TDatabaseIntf.LoginParam(AUser, APass: String): IDatabase;
  begin
  result := self
  .ParamValue('USER_NAME',AUser)
  .ParamValue('Password',APass);
  end;

  class function TDatabaseIntf.new: IDatabase;
  begin

  result := TDatabaseIntf.Create(nil) as IDatabase;
  end;

  function TDatabaseIntf.IOpen: IDatabase;
  begin
  result := self;
  inherited Open;
  end;

  function TDatabaseIntf.ParamValue(sParam: string; value: variant): IDatabase;
  begin
  result := self;
  Params.Values[sParam] := value;
  end;
*)
end.
