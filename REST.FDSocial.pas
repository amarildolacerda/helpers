unit REST.FDSocial;

interface

uses System.Sysutils, System.Classes, System.JSON, IPPeerClient,
  REST.Client, REST.Authenticator.OAuth, REST.Response.Adapter,
  REST.JSON,
  REST.types, idHTTP, IdSSL, IdSSLOpenSSL, REST.Social,
  System.Generics.Collections, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type

  TRESTSocialClientDataset = class(TRESTSocialClient)
  private
    FAdpterRootElement: string;
    FDMemTable1: TFDMemTable;
    FAdpter: TCustomJSONDataSetAdapter;
    function GetDataSet: TDataset;
    function GetRoot: string;
    procedure SetDataSet(const Value: TDataset);
    procedure Setroot(const Value: string);
  protected
    procedure DoResponseTextChanged(sender: TObject);
  public
    constructor create(ow: TComponent); override;
    destructor destroy; override;
    procedure WriteToDataset(ARoot:String; ADataset: TDataset);
    procedure Clear; override;
  published
    // parametros de entrada - antes do GET
    property BaseURL;
    property Resource;
    // parametro usado para obter o Dataset (nome onde esta armazenado o dados do dataset)
    property RootElement: string read GetRoot write Setroot;
    property DataSet: TDataset read GetDataSet write SetDataSet;
  end;

procedure Register;

implementation

uses DateUtils;

procedure Register;
begin
  RegisterComponents('REST Client', [TRESTSocialClientDataset,TRESTSocialClient]);
end;

{
  *************************** TRESTSocialClientDataset ***************************
}
constructor TRESTSocialClientDataset.create(ow: TComponent);
begin
  inherited;
  OnResponseTextChange := DoResponseTextChanged;
  FDMemTable1 := TFDMemTable.create(self);
  with FDMemTable1 do
  begin
    Name := 'MemTableInternal';
    FetchOptions.AssignedValues := [evMode];
    FetchOptions.Mode := fmAll;
    ResourceOptions.AssignedValues := [rvSilentMode];
    ResourceOptions.SilentMode := True;
    UpdateOptions.AssignedValues := [uvCheckRequired];
    UpdateOptions.CheckRequired := false;
  end;

  FAdpter := TCustomJSONDataSetAdapter.create(self);
  FAdpter.Name := 'AdapterDatasetInternal';
  FAdpter.DataSet := FDMemTable1;
end;

destructor TRESTSocialClientDataset.destroy;
begin

  inherited;
end;


procedure TRESTSocialClientDataset.DoResponseTextChanged(sender: TObject);
begin
   WriteToDataset(FAdpterRootElement,FAdpter.Dataset);
end;

procedure TRESTSocialClientDataset.WriteToDataset(ARoot:String;ADataset:TDataset);
var
  r1, r2: TJSONValue;
  r3: TJSonArray;
  p1:TJsonValue;
  ar: TJSonArray;
  sKey: string;
  sPar: string;
  n: integer;
  isArray:boolean;
  isExit:boolean;
  oldData:TDataset;
begin
  if ARoot='' then exit;

  oldData := FAdpter.Dataset;
  try
  if ADataset<>FAdpter.Dataset then
     FAdpter.Dataset := ADataset;
  sKey := ARoot;
  r1 := TJSONObject.ParseJSONValue(ResponseText);
  isArray := false;
  repeat
    isExit:=false;
    sPar := sKey;
    n := pos('.', sKey);
    if n > 0 then
    begin
      sPar := copy(sKey, 1, n - 1);
      sKey := copy(sKey, n + 1, 255);
    end
    else
      sKey := '';

    if isArray then
      for n := 0 to r3.Count-1 do
      begin
         r3.get(n).TryGetValue<TJsonValue>(sPar,r2);
         if r2<>nil then
         begin
           r1 := r2;
           if sKey='' then
           begin
              isExit := true;
              r3 := TJsonArray(r2);
           end;
           break;
         end;
      end;
    if isExit then break;

    r1.TryGetValue<TJsonArray>(sPar, r3);
    if r3=nil then
    begin
       r1.TryGetValue<TJsonValue>(sPar, r2);
       isArray := false;
       if r2=nil then
         break;
       if sKey > '' then
       begin
         r1 := r2;
         continue;
       end else
         r3 := r2 as TJsonArray;
    end else
    begin
        r1 := r3;
        isArray := true;
    end;
  until sKey = '';

  if r3 <> nil then
    FAdpter.UpdateDataSet(r3);

  finally
  if oldData<>FAdpter.Dataset then
     FAdpter.Dataset := oldData;
  end;
end;

procedure TRESTSocialClientDataset.Clear;
begin
  inherited;
  FDMemTable1.Close;
  FDMemTable1.Fields.Clear;
end;

function TRESTSocialClientDataset.GetDataSet: TDataset;
begin
  result := FAdpter.DataSet;
end;

function TRESTSocialClientDataset.GetRoot: string;
begin
  result := FAdpterRootElement;
end;

procedure TRESTSocialClientDataset.SetDataSet(const Value: TDataset);
begin
  FAdpter.DataSet := Value;
end;

procedure TRESTSocialClientDataset.Setroot(const Value: string);
begin
  FAdpterRootElement := Value;
  FAdpter.ClearDataSet;
end;

end.
