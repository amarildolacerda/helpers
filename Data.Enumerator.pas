{***************************************************************************}
{                                                                           }
{                                                                           }
{           Copyright (C) Amarildo Lacerda                                  }
{                                                                           }
{           https://github.com/amarildolacerda                              }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}


{
  Alterações:
      25/03/16 - Primeira versão publicada - Construção
}

unit Data.Enumerator;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  System.Rtti, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, Data.DB, FireDAC.Comp.Client;

type

  IQuery = interface;

  TDataStorageRec = record
    class function NewQuery(): IQuery; overload; static;
    class function NewQuery(const ATable: string; const AFields: String = '*';
      const AWhere: String = ''; const AOrderBy: string = '';
      const AJoin: String = ''): IQuery; overload; static;
  end;

  //TConnectionIntf = TFDConnection;

  //TParamsIntf = TFDParams;

  //TFieldsIntf = TFields;

  IDataset = interface
    ['{AED88905-4241-4BBD-9035-1112C882CF05}']
    function Open: IQuery;
    procedure Close;
    procedure Next;
    procedure Prior;
    procedure Last;
    procedure First;
    function eof: boolean;
    function bof: boolean;
    procedure insert;
    procedure post;
    procedure edit;
    procedure delete;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function RowsAffected: integer;
    function GetFields: TFields;
    property Fields: TFields read GetFields;
  end;

  IQuery = Interface(IDataset)
    ['{4C9E016E-41A2-42D1-899E-D820FE4FA9C4}']
    procedure ExecSql(const AScript: string);
    procedure SetCommand(const ACommand: string);
    function GetCommand: string;
    procedure SetConnection(const AConn: TFDCustomConnection);
    function GetConnection: TFDCustomConnection;
    property Command: string read GetCommand write SetCommand;
    property Connection: TFDCustomConnection read GetConnection
      write SetConnection;
    function GetParams: TFDParams;
    property Params: TFDParams read GetParams;

    function GetEnumerator: IQuery;
    function GetCurrent: TFields;
    property Current:TFields read GetCurrent;
    function MoveNext: boolean;
    procedure reset;
    function Open( AProc:TProc<TFDParams> ): IQuery;overload;
    function Clone:IQuery;

  End;

  TQueyIntf = class(TFDQuery, IQuery, IDataset)
  private
    procedure SetFieldByName(const AField: string; const AValue: variant);
  public
    FScrollRow:Integer;
    function RowsAffected: integer;
    function eof: boolean;
    function bof: boolean;
    function GetCurrent: TFields;
    function MoveNext: boolean;
    procedure reset;
    procedure ExecSql(const AScript: string); overload;
    function GetCommand: string;
    procedure SetCommand(const ACommand: string);
    procedure SetConnection(const AConn: TFDCustomConnection);
    function GetConnection: TFDCustomConnection;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function Open: IQuery;overload;
    function Open( AProc:TProc<TFDParams> ): IQuery;overload;
    //procedure SetFieldByName(const AField: string; const AValue: variant);

    function GetFields: TFields;
    function GetEnumerator: IQuery;
    function GetParams: TFDParams;
    function NewQuery(const ATable: string=''; const AFields: String = '*';
      const AWhere: String = ''; const AOrderBy: string = '';
      const AJoin: String = ''): IQuery;
    destructor Destroy;override;
    function Clone:IQuery;

  end;

implementation

{ TQueyIntf }

function TQueyIntf.RowsAffected: integer;
begin
  result := inherited RowsAffected;
end;

function TQueyIntf.bof: boolean;
begin
  result := inherited bof;
end;

function TQueyIntf.Clone: IQuery;
begin
   result := TQueyIntf.Create(nil);
   result.Command := self.GetCommand;
   result.Connection := self.GetConnection;
end;

procedure TQueyIntf.Commit;
begin
  Connection.Commit;
end;

destructor TQueyIntf.Destroy;
begin
  FScrollRow := -1;
  inherited;
end;


function TQueyIntf.eof: boolean;
begin
  result := inherited eof;
end;

procedure TQueyIntf.ExecSql(const AScript: string);
begin
  sql.Text := AScript;
  inherited Execute();
end;

function TQueyIntf.GetCommand: string;
begin
  result := sql.Text;
end;

function TQueyIntf.GetConnection: TFDCustomConnection;
begin
  result := inherited Connection;
end;

function TQueyIntf.GetCurrent:TFields;
begin
  result := self.Fields;
end;

function TQueyIntf.GetEnumerator: IQuery;
begin
  result := self as IQuery;
end;

function TQueyIntf.GetFields: TFields;
begin
  result := inherited Fields;
end;

function TQueyIntf.GetParams: TFDParams;
begin
  result := inherited Params;
end;

function TQueyIntf.MoveNext: boolean;
begin
  if FScrollRow>=0 then
     Next;
  inc(FScrollRow);
  result := not eof;
end;

function TQueyIntf.NewQuery(const ATable, AFields, AWhere, AOrderBy,
  AJoin: String): IQuery;
begin
  result := TDataStorageRec.NewQuery(ATable, AFields, AWhere,
    AOrderBy, AJoin);
  result.Connection := self.Connection;
end;

function TQueyIntf.Open(AProc: TProc<TFDParams>): IQuery;
begin
     AProc(GetParams);
     result := open;
end;

function TQueyIntf.Open: IQuery;
begin
  inherited Open;
  FScrollRow := -1;
  result := self as IQuery;
end;

procedure TQueyIntf.reset;
begin
  First;
  FScrollRow := -1;
end;

procedure TQueyIntf.Rollback;
begin
  Connection.Rollback;
end;

procedure TQueyIntf.SetCommand(const ACommand: string);
begin
  sql.Text := ACommand;
end;

procedure TQueyIntf.SetConnection(const AConn: TFDCustomConnection);
begin
  inherited Connection := AConn;
end;

procedure TQueyIntf.SetFieldByName(const AField: string;
  const AValue: variant);
begin
  inherited fieldByName(AField).Value := AValue;
end;

procedure TQueyIntf.StartTransaction;
begin
  Connection.StartTransaction;
end;

{ TStoreDataStorage }

class function TDataStorageRec.NewQuery: IQuery;
begin
  result := TQueyIntf.Create(nil);
end;

class function TDataStorageRec.NewQuery(const ATable, AFields, AWhere,
  AOrderBy, AJoin: String): IQuery;
var
  sql: TStringBuilder;
begin
  sql := TStringBuilder.Create;
  try
    result := NewQuery();
    if ATable<>'' then
      sql.Append('select ' + AFields + ' from ' + ATable);
    if AJoin <> '' then
      sql.Append(' ' + AJoin);
    if AWhere <> '' then
      sql.Append(' where ' + AWhere);
    if AOrderBy <> '' then
      sql.Append(' order by ' + AOrderBy);
    result.Command := sql.ToString;
  finally
    sql.Free;
  end;
end;

end.
