unit System.Query.Fluent;

interface

uses System.Classes, System.SysUtils, Data.DB;

type
  IFluentQuery = Interface
    ['{738FDA90-A6E8-4FB0-A65E-6FCD4DF79F4E}']
    Function Dataset(ADataset: TDataset):IFluentQuery;overload;
    Function Dataset():TDataset;overload;
    function Where(AWhere:String):IFluentQuery;
  End;

  TFluentQueryAttrib = record
      Fields:string;
      Table:String;
      Where:String;
      Join:string;
      GroupBy:string;
      OrderBy:String;
  end;
  TFluentQuery<T: Class> = class(TInterfacedObject, IFluentQuery)
  protected
    FDataset: TDataset;
    FAttrib:TFluentQueryAttrib;
  public
    class function QueryBuilder(AAttrib:TFluentQueryAttrib):string;static;
    class function New: IFluentQuery; static;
    function Dataset(ADataset: TDataset): IFluentQuery;overload;
    Function Dataset():TDataset;overload;
    function Where(AWhere:String):IFluentQuery;
  end;

implementation

uses System.Rtti;

function TFluentQuery<T>.Dataset: TDataset;
begin
    result := FDataset;
end;

class function TFluentQuery<T>.New: IFluentQuery;
var
  q: TFluentQuery<T>;
begin
  q := TFluentQuery<T>.create;
  q.Dataset(TDataset(T).create(nil));
  result := q;
end;

class function TFluentQuery<T>.QueryBuilder(
  AAttrib: TFluentQueryAttrib): string;
begin

end;

function TFluentQuery<T>.Where(AWhere: String): IFluentQuery;
begin
   FAttrib.Where := AWhere;
end;

function TFluentQuery<T>.Dataset(ADataset: TDataset): IFluentQuery;
begin
  FDataset := ADataset;
  result := self;
end;

end.
