unit REST.FDSocial;

interface

uses System.Sysutils, System.Classes, System.JSON, IPPeerClient,
  REST.Client, REST.Authenticator.OAuth, REST.Response.Adapter,
  REST.types, idHTTP, IdSSL, IdSSLOpenSSL, REST.Social,
  System.Generics.Collections, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type

  TRESTSocialClientDataset = class(TRESTSocialClient)
  private
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
    function GetDataset: TDataset;
    procedure SetDataset(const Value: TDataset);
    procedure Setroot(const Value: string);
    function GetRoot: string;
  public
    procedure Clear; override;
    property RootElement: string read GetRoot write Setroot;
    property DataSet: TDataset read GetDataset write SetDataset;
    constructor create(ow: TComponent); override;
    destructor destroy; override;

  end;

implementation

function TRESTSocialClientDataset.GetRoot: string;
begin
  result := RESTResponseDataSetAdapter1.RootElement;
end;

procedure TRESTSocialClientDataset.Clear;
begin
  inherited;

  FDMemTable1.Close;
  FDMemTable1.Fields.Clear;
  RESTResponseDataSetAdapter1.DataSet := FDMemTable1;
  RESTResponseDataSetAdapter1.Response := Response;
  RESTResponseDataSetAdapter1.RootElement := '';

end;

{ TRESTClientDropBoxDataset }

constructor TRESTSocialClientDataset.create(ow: TComponent);
begin
  inherited;
  FDMemTable1 := TFDMemTable.create(self);
  with FDMemTable1 do
  begin
    FetchOptions.AssignedValues := [evMode];
    FetchOptions.Mode := fmAll;
    ResourceOptions.AssignedValues := [rvSilentMode];
    ResourceOptions.SilentMode := True;
    UpdateOptions.AssignedValues := [uvCheckRequired];
    UpdateOptions.CheckRequired := false;
  end;

  RESTResponseDataSetAdapter1 := TRESTResponseDataSetAdapter.create(self);
  RESTResponseDataSetAdapter1.DataSet := FDMemTable1;
  RESTResponseDataSetAdapter1.FieldDefs.Clear;
  RESTResponseDataSetAdapter1.Response := Response;
  RESTResponseDataSetAdapter1.NestedElements := True;

end;

destructor TRESTSocialClientDataset.destroy;
begin

  inherited;
end;

function TRESTSocialClientDataset.GetDataset: TDataset;
begin
  result := RESTResponseDataSetAdapter1.DataSet;
end;

procedure TRESTSocialClientDataset.SetDataset(const Value: TDataset);
begin
  RESTResponseDataSetAdapter1.DataSet := Value;
  if Value = nil then
  begin
    RESTResponseDataSetAdapter1.Response := nil;
    RESTResponseDataSetAdapter1.AutoUpdate := false;
  end
  else
  begin
    RESTResponseDataSetAdapter1.Response := Response;
    RESTResponseDataSetAdapter1.AutoUpdate := True;
  end;
end;

procedure TRESTSocialClientDataset.Setroot(const Value: string);
begin
  RESTResponseDataSetAdapter1.RootElement := Value;

end;

end.
