unit REST.Social;

interface

uses System.Sysutils, System.Classes, System.JSON, IPPeerClient,
  REST.Client, REST.Authenticator.OAuth, REST.Response.Adapter,
  REST.types, idHTTP, IdSSL, IdSSLOpenSSL,
  System.Generics.Collections;

type
  TCustomSocialBase = class
  public
    procedure LoadFromJson(js: TJsonObject); virtual;
    procedure FromJson(aJson: string); virtual;
    procedure LoadFromJsonFile(sFile: string); virtual;
  end;

  TCustomSocialAuthBase = class(TCustomSocialBase)
  private
    FAccessToken: string;
    FClient_Secret: string;
    FClient_ID: string;
    procedure SetAccessToken(const Value: string);
    procedure SetClient_ID(const Value: string);
    procedure SetClient_Secret(const Value: string);
  public
    property AccessToken: string read FAccessToken write SetAccessToken;
    property Client_ID: string read FClient_ID write SetClient_ID;
    property Client_Secret: string read FClient_Secret write SetClient_Secret;
    procedure LoadFromJson(js: TJsonObject); override;
  end;

  TRESTSocialClient = class(TRESTClient)
  private
    FAuth2: TOAuth2Authenticator;
    RESTResponse1: TRESTResponse;
    RESTRequest1: TRESTRequest;
    function send(url: string; AResource: string;
      AMethod: TRESTRequestMethod): string;
    procedure SetAccessToken(const Value: string);
    function GetAccessToken: string;
  public
    StatusCode: integer;
    ResponseText: string;
    function Response: TRESTResponse;
    function Request: TRESTRequest;
    function Auth2: TOAuth2Authenticator;
    property AccessToken: string read GetAccessToken write SetAccessToken;
    procedure Clear; virtual;
    constructor create(ow: TComponent); override;
    destructor destroy; override;
    function Get(url: string; AResource: string = ''): string; virtual;
    function GetStream(AUrl: string; AResource: string; AStream: TStream)
      : integer; virtual;
    function SendStream(AUrl, AResource: string; AStream: TStream)
      : integer; virtual;
    function Post(url: string; AResource: string = ''): string; virtual;
  end;

function Indy_Send_File(ACommand: string;
  AccessToken, AFileName, ADPFileName: string): integer; overload;
function Indy_Download_File(AccessToken: string; const exportLinks: string;
  AStream: System.Classes.TStream; FSSL: boolean): integer;

implementation

function Indy_Download_File(AccessToken: string; const exportLinks: string;
  AStream: System.Classes.TStream; FSSL: boolean): integer;
var
  res: String;
  LidHTTP: TIdHttp;
  link: string;
begin

  LidHTTP := TIdHttp.create(nil);
  try
    // add authorization from stored key
    LidHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' +
      AccessToken;

    // use SSL
    if FSSL then
      LidHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.create(LidHTTP);

    try
      AStream.Position := 0;
      LidHTTP.Get(exportLinks, AStream);
      result := LidHTTP.Response.ResponseCode;
    except
      // on E: Exception do begin
      // rezultat_String.Add ('Eroare ! ' + E.Message);
      // end;
      raise;
    end;

    AStream.Position := 0;
    // Stream.SaveToFile(file_name)

  finally
    LidHTTP.Free;
    // Stream.Free;
  end;
end;

function Indy_Send_File_Stream(AccessToken, AUrl: string; AStream: TStream;
  ASSL: boolean; var AResponseText: string): integer; overload;
var
  LidHTTP: TIdHttp;
begin
  LidHTTP := TIdHttp.create(nil);
  try
    LidHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' +
      AccessToken;
    LidHTTP.Request.ContentType := 'application/octet-stream';
    // LIdHttp.Request.CustomHeaders.Values['Dropbox-API-Arg'] := '{ "path":"/apps/tete.pdf", "mode":"add","autorename": true, "mute": false }';
    if ASSL then
      LidHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.create(LidHTTP);
    AStream.Position := 0;

    try
      LidHTTP.Put(AUrl + '', AStream);
    except
    end;
    AResponseText := LidHTTP.Response.ResponseText;
    result := LidHTTP.ResponseCode;

  finally
    LidHTTP.Free;
  end;
end;

function Indy_Send_File(ACommand: string;
  AccessToken, AFileName, ADPFileName: string): integer; overload;
var
  AStream: TFileStream;
  rsp: string;
  cmd: string;
begin
  cmd := ACommand + ADPFileName;
  AStream := TFileStream.create(AFileName, fmOpenRead);
  try
    result := Indy_Send_File_Stream(AccessToken, cmd, AStream, true, rsp);
  finally
    AStream.Free;
  end;
end;

procedure TCustomSocialBase.FromJson(aJson: string);
var
  js: TJsonObject;
begin
  js := TJsonObject.ParseJSONValue(aJson) as TJsonObject;
  try
    LoadFromJson(js);
  finally
    js.Free;
  end;
end;

procedure TCustomSocialBase.LoadFromJson(js: TJsonObject);
begin
end;

procedure TCustomSocialBase.LoadFromJsonFile(sFile: string);
var
  str: TstringList;
  js: TJsonObject;
begin
  str := TstringList.create;
  try
    str.LoadFromFile(sFile);
    FromJson(str.text);
  finally
    str.Free;
  end;
end;

procedure TCustomSocialAuthBase.LoadFromJson(js: TJsonObject);
begin
  inherited;
  js.TryGetValue<string>('Access_Token', FAccessToken);
  js.TryGetValue<string>('Client_ID', FClient_ID);
  js.TryGetValue<string>('Client_Secret', FClient_Secret);

end;

procedure TCustomSocialAuthBase.SetAccessToken(const Value: string);
begin
  FAccessToken := Value;
end;

procedure TCustomSocialAuthBase.SetClient_ID(const Value: string);
begin
  FClient_ID := Value;
end;

procedure TCustomSocialAuthBase.SetClient_Secret(const Value: string);
begin
  FClient_Secret := Value;
end;

{ TRESTClientDropBox }

function TRESTSocialClient.Auth2: TOAuth2Authenticator;
begin
  result := FAuth2;
end;

constructor TRESTSocialClient.create(ow: TComponent);
begin
  inherited;
  FAuth2 := TOAuth2Authenticator.create(self);

  RESTResponse1 := TRESTResponse.create(self);
  RESTResponse1.ContentType := 'application/json';

  RESTRequest1 := TRESTRequest.create(self);
  RESTRequest1.Client := self;
  RESTRequest1.Response := RESTResponse1;

  Authenticator := FAuth2;

  Accept := 'application/json, text/plain; q=0.9, text/html;q=0.8,';
  AcceptCharset := 'UTF-8, *;q=0.8';
  HandleRedirects := true;
  RaiseExceptionOn500 := false;

end;

destructor TRESTSocialClient.destroy;
begin
  FAuth2.Free;
  inherited;
end;

function TRESTSocialClient.send(url: string; AResource: string;
  AMethod: TRESTRequestMethod): string;
begin
  RESTRequest1.Method := AMethod;
  RESTRequest1.Resource := AResource;
  BaseURL := url;
  RESTRequest1.Execute;
  result := RESTResponse1.Content;
  StatusCode := RESTResponse1.StatusCode;
end;

function TRESTSocialClient.SendStream(AUrl, AResource: string;
  AStream: TStream): integer;
begin
  result := Indy_Send_File_Stream(AccessToken, AUrl + AResource, AStream, true,
    ResponseText);
end;

procedure TRESTSocialClient.SetAccessToken(const Value: string);
begin
  FAuth2.AccessToken := Value;
end;

function TRESTSocialClient.Get(url: string; AResource: string = ''): string;
begin
  result := send(url, AResource, TRESTRequestMethod.rmGET);
end;

function TRESTSocialClient.GetAccessToken: string;
begin
  result := FAuth2.AccessToken;
end;

function TRESTSocialClient.GetStream(AUrl, AResource: string;
  AStream: TStream): integer;
begin
  result := Indy_Download_File(AccessToken, AUrl + AResource, AStream, true);
end;

function TRESTSocialClient.Post(url: string; AResource: string = ''): string;
begin
  result := send(url, AResource, TRESTRequestMethod.rmPost);
end;

procedure TRESTSocialClient.Clear;
begin
  RESTRequest1.Params.Clear;
  RESTRequest1.SynchronizedEvents := false;
end;

function TRESTSocialClient.Request: TRESTRequest;
begin
  result := RESTRequest1;
end;

function TRESTSocialClient.Response: TRESTResponse;
begin
  result := RESTResponse1;
end;

end.
