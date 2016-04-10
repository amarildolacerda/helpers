unit REST.Social;

interface

uses System.Sysutils, System.Classes, System.JSON, IPPeerClient,
  REST.Client, REST.Authenticator.OAuth, REST.Response.Adapter,
  REST.types, idHTTP, IdSSL, IdSSLOpenSSL,
  System.Generics.Collections;

type
  TCustomSocialBase = class(TObject)
  public
    procedure FromJson(aJson: string); virtual;
    procedure LoadFromJson(js: TJsonObject); virtual;
    procedure LoadFromJsonFile(sFile: string); virtual;
  end;

  TCustomSocialAuthBase = class(TCustomSocialBase)
  private
    FAccessToken: string;
    FClient_ID: string;
    FClient_Secret: string;
    procedure SetAccessToken(const Value: string);
    procedure SetClient_ID(const Value: string);
    procedure SetClient_Secret(const Value: string);
  public
    procedure LoadFromJson(js: TJsonObject); override;
  published
    property AccessToken: string read FAccessToken write SetAccessToken;
    property Client_ID: string read FClient_ID write SetClient_ID;
    property Client_Secret: string read FClient_Secret write SetClient_Secret;
  end;

  TRESTSocialClient = class(TRESTClient)
  private
    FResource : string;
    FAuth2: TOAuth2Authenticator;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    FResponseText: string;
    FOnResponseTextChange: TNotifyEvent;
    function GetAccessToken: string;
    function send(url: string; AResource: string;
      AMethod: TRESTRequestMethod): string;
    procedure SetAccessToken(const Value: string);
    function GetResource: string;
    procedure SetResource(const Value: string);
    function GetRequestMethod: TRESTRequestMethod;
    procedure SetRequestMethod(const Value: TRESTRequestMethod);
    procedure SetResponseText(const Value: string);
    procedure SetOnResponseTextChange(const Value: TNotifyEvent);
    function GetParameterByName(AName: string): TRESTRequestParameter;
    procedure SetParameterByName(AName: string;
      const AValue: TRESTRequestParameter);
  protected

  public
    StatusCode: Integer;
    constructor create(ow: TComponent); override;
    destructor destroy; override;
    procedure Clear; virtual;
    function GetAuth2: TOAuth2Authenticator; virtual;
    property ParameterByName[AName: string]: TRESTRequestParameter read GetParameterByName write SetParameterByName;

    // configuração de acesso ao servidor
    property ResponseText: string read FResponseText write SetResponseText;
    function Get(url: string; AResource,AService: string): string; overload; virtual;
    function Get(AService:String=''): string; overload; virtual;
    function GetStream(AUrl: string; AResource: string; AStream: TStream)
      : Integer; virtual;
    function Post(url: string; AResource,AService: string): string; overload; virtual;
    function Post(AService:string): string; overload; virtual;
    function GetRequest: TRESTRequest; virtual;
    function GetResponse: TRESTResponse; virtual;
    function SendStream(AUrl, AResource: string; AStream: TStream)
      : Integer; virtual;
    procedure Execute;
  published
    property BaseURL;
    property Resource: string read GetResource write SetResource;
    property RequestMethod: TRESTRequestMethod read GetRequestMethod
      write SetRequestMethod;
    property OnResponseTextChange: TNotifyEvent read FOnResponseTextChange
      write SetOnResponseTextChange;
    property Request: TRESTRequest read GetRequest;
    property Response: TRESTResponse read GetResponse;
    property AccessToken: string read GetAccessToken write SetAccessToken;
  end;

function Indy_Send_File(ACommand: string;
  AccessToken, AFileName, ADPFileName: string): Integer; overload;
function Indy_Download_File(AccessToken: string; const exportLinks: string;
  AStream: System.Classes.TStream; FSSL: boolean): Integer;

implementation

function Indy_Download_File(AccessToken: string; const exportLinks: string;
  AStream: System.Classes.TStream; FSSL: boolean): Integer;
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
  ASSL: boolean; var AResponseText: string): Integer; overload;
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
  AccessToken, AFileName, ADPFileName: string): Integer; overload;
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

{
  ****************************** TCustomSocialBase *******************************
}
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
  // abstract;
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

{
  **************************** TCustomSocialAuthBase *****************************
}
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

{
  ****************************** TRESTSocialClient *******************************
}
constructor TRESTSocialClient.create(ow: TComponent);
begin
  inherited;
  FAuth2 := TOAuth2Authenticator.create(self);

  RESTResponse1 := TRESTResponse.create(self);
  RESTResponse1.Name := 'ResponseInternal';
  RESTResponse1.ContentType := 'application/json';

  RESTRequest1 := TRESTRequest.create(self);
  RESTRequest1.Name := 'RequestInternal';
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

procedure TRESTSocialClient.Execute;
begin
   RESTRequest1.Execute;
   ResponseText := RESTResponse1.Content;
end;

function TRESTSocialClient.GetAuth2: TOAuth2Authenticator;
begin
  result := FAuth2;
end;

function TRESTSocialClient.GetParameterByName(
  AName: string): TRESTRequestParameter;
begin
  result:= RESTRequest1.Params.ParameterByName(AName);
end;

procedure TRESTSocialClient.Clear;
begin
  RESTRequest1.Params.Clear;
  RESTRequest1.SynchronizedEvents := false;
end;

function TRESTSocialClient.Get(url: string; AResource,AService: string): string;
begin
  result := send(url, AResource+AService, TRESTRequestMethod.rmGET);

end;

function TRESTSocialClient.Get(AService:String=''): string;
begin
  result := Get(BaseURL, Resource, AService);
end;

function TRESTSocialClient.GetAccessToken: string;
begin
  result := FAuth2.AccessToken;
end;

function TRESTSocialClient.GetRequestMethod: TRESTRequestMethod;
begin
  result := RESTRequest1.Method;
end;

function TRESTSocialClient.GetResource: string;
begin
  result := FResource;
end;

function TRESTSocialClient.GetStream(AUrl: string; AResource: string;
  AStream: TStream): Integer;
begin
  result := Indy_Download_File(AccessToken, AUrl + AResource, AStream, true);
end;


function TRESTSocialClient.Post(AService:string): string;
begin
  result := Post(BaseURL, Resource, AService);
end;

function TRESTSocialClient.Post(url: string; AResource,AService: string): string;
begin
  result := send(url, AResource, TRESTRequestMethod.rmPost);
end;

function TRESTSocialClient.GetRequest: TRESTRequest;
begin
  result := RESTRequest1;
end;

function TRESTSocialClient.GetResponse: TRESTResponse;
begin
  result := RESTResponse1;
end;

function TRESTSocialClient.send(url: string; AResource: string;
  AMethod: TRESTRequestMethod): string;
begin
  RESTRequest1.Method := AMethod;
  RESTRequest1.Resource := AResource;
  BaseURL := url;
  RESTRequest1.Execute;
  ResponseText := RESTResponse1.Content;
  result := ResponseText;
  StatusCode := RESTResponse1.StatusCode;
end;

function TRESTSocialClient.SendStream(AUrl, AResource: string;
  AStream: TStream): Integer;
var
  rst: String;
begin
  result := Indy_Send_File_Stream(AccessToken, AUrl + AResource, AStream,
    true, rst);
  ResponseText := rst;
end;

procedure TRESTSocialClient.SetAccessToken(const Value: string);
begin
  FAuth2.AccessToken := Value;
end;

procedure TRESTSocialClient.SetOnResponseTextChange(const Value: TNotifyEvent);
begin
  FOnResponseTextChange := Value;
end;

procedure TRESTSocialClient.SetParameterByName(AName: string;
  const AValue: TRESTRequestParameter);
var lcl:TRESTRequestParameter;
begin
  lcl := RESTRequest1.Params.ParameterByName(AName);
  if assigned(lcl) then
  with lcl do
       begin
            Name := AValue.name;
            Value := AValue.Value;
            Kind := AValue.Kind;
            Options := AValue.Options;
            ContentType := AValue.ContentType;
            DisplayName := AValue.DisplayName;
       end;
end;

procedure TRESTSocialClient.SetRequestMethod(const Value: TRESTRequestMethod);
begin
  RESTRequest1.Method := Value;
end;

procedure TRESTSocialClient.SetResource(const Value: string);
begin
  FResource := Value;
  RESTRequest1.Resource := Value;
end;

procedure TRESTSocialClient.SetResponseText(const Value: string);
begin
  FResponseText := Value;
  if assigned(FOnResponseTextChange) then
    FOnResponseTextChange(self);
end;


end.
