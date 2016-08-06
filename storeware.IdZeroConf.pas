unit storeware.IdZeroConf;

interface

uses System.Classes, System.SysUtils, IdUDPServer, IdGlobal, IdSocketHandle,
  System.Json;

Type

  TIdZeroConfType = (zctClient, zctServer);
  TIdZeroConfEvent = procedure(sender: TObject; AData: string) of object;

  TIdZeroConfBase = class(TComponent)
  private
    FUDPServer: TIdUDPServer;
    FzeroConfType: TIdZeroConfType;
    FAppDefaultPort: word;
    FServiceName: String;
    FOnResponse: TIdZeroConfEvent;
    FLocalHost, FAppDefaultHost: string;
    FAppDefaultPath: string;
    procedure SetAppDefaultPort(const Value: word);
    procedure SetDefaultPort(const Value: word);
    procedure SetzeroConfType(const Value: TIdZeroConfType);
    function GetDefaultPort: word;
    procedure SetServiceName(const Value: String);
    procedure SetOnResponse(const Value: TIdZeroConfEvent);
    function GetActive: boolean;
    procedure SetActive(const Value: boolean);
    procedure SetAppDefaultHost(const Value: string);
    function GetzeroConfType: TIdZeroConfType;
    function GetOnResponse: TIdZeroConfEvent;
    function GetAppDefaultHost: string;
    function GetAppDefaultPort: word;
    procedure SetAppDefaultPath(const Value: string);
    function getAppDefaultPath: string;
    function GetLocalHost: string;
  protected
    FClientPort: word;
    procedure DoUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes;
      ABinding: TIdSocketHandle);
    procedure Broadcast(AData: string; APort: word);
    function CreatePayload(ACommand: string; AMessage: String): TJsonObject;
  public
    constructor create(AOwner: TComponent); virtual;
    destructor destroy; override;
    property LocalHost: string read GetLocalHost;
  published
    property ServiceName: String read FServiceName write SetServiceName;
    property Active: boolean read GetActive write SetActive;
  end;

  TIdZeroConfServer = class(TIdZeroConfBase)
  public
    constructor create(AOwner: TComponent); override;
  published
    property ServerPort: word read GetDefaultPort write SetDefaultPort;
    property AppDefaultHost: string read GetAppDefaultHost
      write SetAppDefaultHost;
    property AppDefaultPort: word read GetAppDefaultPort
      write SetAppDefaultPort;
    property AppDefaultPath: string read getAppDefaultPath
      write SetAppDefaultPath;
    property OnRequestEvent: TIdZeroConfEvent read GetOnResponse
      write SetOnResponse;
  end;

  TIdZeroConfClient = class(TIdZeroConfBase)
  private
    FServerPort: word;
    procedure SetServerPort(const Value: word);
  public
    // envia para o servidor perguntando onde ele esta.
    procedure Send;
    constructor create(AOwner: TComponent); override;
  published
    // porta do servidor para onde ira enviar a mensagem de broadcast
    property ServerPort: word read FServerPort write SetServerPort;
    // porta para onde o cliente esta escutando para pegar a resposta do servidor
    property ClientPort: word read GetDefaultPort write SetDefaultPort;
    property OnResponseEvent: TIdZeroConfEvent read GetOnResponse
      write SetOnResponse;
  end;

procedure Register;

implementation

{ TIdZeroConf }
uses IdStack;

const
  ID_ZeroConf_Port = 53330;

procedure TIdZeroConfBase.Broadcast(AData: string; APort: word);
begin
  FUDPServer.Broadcast(AData, APort);
end;

constructor TIdZeroConfBase.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  FUDPServer := TIdUDPServer.create(self);
  FUDPServer.OnUDPRead := DoUDPRead;
  FUDPServer.BroadcastEnabled := true;
  FUDPServer.ThreadedEvent := true;
  FServiceName := 'ZeroConf';
end;

function TIdZeroConfBase.CreatePayload(ACommand, AMessage: String): TJsonObject;
begin
  FLocalHost := GStack.LocalAddress;
  result := TJsonObject.create;
  result.AddPair('service', FServiceName);
  result.AddPair('command', ACommand);
  result.AddPair('payload', AMessage);
  result.AddPair('source', FLocalHost);
end;

destructor TIdZeroConfBase.destroy;
begin
  FUDPServer.Free;
  inherited;
end;

function BytesToString(arr: TIdBytes): string;
var
  i: Integer;
begin
  result := '';
  for i := low(arr) to high(arr) do
    result := result + chr(arr[i]);
end;

// system.ujson
procedure TIdZeroConfBase.DoUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  resp, ASource, AMessage: string;
  AComand: string;
  ABase: string;
  AJson: TJsonObject;
  ACmd: TJsonObject;
begin

  AMessage := BytesToString(AData);
  AJson := TJsonObject.ParseJSONValue(AMessage) as TJsonObject;

  if AJson.Values['service'].Value <> FServiceName then
    exit; // nao é o servico esperado

  AComand := AJson.Values['command'].Value;
  AJson.TryGetValue<string>('source', ASource);

  if sameText('ping', AComand) then
  begin
    AJson.TryGetValue<word>('client', FClientPort);
    ACmd := CreatePayload('response',
      formatDateTime('yyyy-mm-dd hh:mm:ss', now));
    try
      if FAppDefaultHost = '' then
        FAppDefaultHost := FLocalHost;
      ACmd.AddPair('host', FAppDefaultHost);
      ACmd.AddPair('port', TJSONNumber.create(FAppDefaultPort));
      ACmd.AddPair('path', FAppDefaultPath);
      FUDPServer.Broadcast(ACmd.ToString, FClientPort, ASource);
    finally
      ACmd.Free;
    end;
  end;

  if assigned(FOnResponse) then
    FOnResponse(self, AMessage);

end;

function TIdZeroConfBase.GetActive: boolean;
begin
  result := FUDPServer.Active;
end;

function TIdZeroConfBase.GetAppDefaultHost: string;
begin
  result := FAppDefaultHost;
end;

function TIdZeroConfBase.getAppDefaultPath: string;
begin
  result := FAppDefaultPath;
end;

function TIdZeroConfBase.GetAppDefaultPort: word;
begin
  result := FAppDefaultPort;
end;

function TIdZeroConfBase.GetDefaultPort: word;
begin
  result := FUDPServer.DefaultPort;
end;

function TIdZeroConfBase.GetLocalHost: string;
begin
  if FLocalHost = '' then
    FLocalHost := GStack.LocalAddress;
  result := FLocalHost;
end;

function TIdZeroConfBase.GetOnResponse: TIdZeroConfEvent;
begin
  result := FOnResponse;
end;

function TIdZeroConfBase.GetzeroConfType: TIdZeroConfType;
begin
  result := FzeroConfType;
end;

procedure TIdZeroConfBase.SetActive(const Value: boolean);
begin
  if FUDPServer.Active <> Value then
    FUDPServer.Active := Value;
end;

procedure TIdZeroConfBase.SetAppDefaultHost(const Value: string);
begin
  FAppDefaultHost := Value;
end;

procedure TIdZeroConfBase.SetAppDefaultPath(const Value: string);
begin
  FAppDefaultPath := Value;
end;

procedure TIdZeroConfBase.SetAppDefaultPort(const Value: word);
begin
  FAppDefaultPort := Value;
end;

procedure TIdZeroConfBase.SetDefaultPort(const Value: word);
begin
  FUDPServer.DefaultPort := Value;
end;

procedure TIdZeroConfBase.SetOnResponse(const Value: TIdZeroConfEvent);
begin
  FOnResponse := Value;
end;

procedure TIdZeroConfBase.SetServiceName(const Value: String);
begin
  FServiceName := Value;
end;

procedure TIdZeroConfBase.SetzeroConfType(const Value: TIdZeroConfType);
begin
  FzeroConfType := Value;
end;

constructor TIdZeroConfClient.create(AOwner: TComponent);
begin
  inherited;
  if (ServerPort = 0) then
  begin
    ServerPort := ID_ZeroConf_Port;
    ClientPort := ID_ZeroConf_Port + 1;
  end;
  SetzeroConfType(zctClient);
end;

procedure TIdZeroConfClient.Send;
var
  AJson: TJsonObject;
begin
  FLocalHost := GStack.LocalAddress;
  AJson := CreatePayload('ping', '');
  try
    AJson.AddPair('client', TJSONNumber.create(ClientPort));
    // passa para o servidor onde o cliente esta escutando
    Broadcast(AJson.ToString, FServerPort); // envia os dados para o servidor
  finally
    AJson.Free;
  end;
end;

procedure TIdZeroConfClient.SetServerPort(const Value: word);
begin
  FServerPort := Value;
end;

{ TIdZeroConfServer }

constructor TIdZeroConfServer.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  if (ServerPort = 0) then
  begin
    ServerPort := ID_ZeroConf_Port;
    FAppDefaultHost := FLocalHost;
    AppDefaultPort := 8080;
    AppDefaultPath := '/rest/datasnap/';
  end;
  SetzeroConfType(zctServer);
end;

procedure Register;
begin
      RegisterComponents('Storeware',[TIdZeroConfServer,TIdZeroConfClient]);
end;

end.
