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

unit Social.Dropbox;

interface

uses System.Sysutils, System.Classes, System.JSON, IPPeerClient,
  REST.Client, REST.Authenticator.OAuth, REST.Response.Adapter,
  REST.types, idHTTP, IdSSL, IdSSLOpenSSL,
  System.Generics.Collections, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type

  TDropboxBase = class
  public
    procedure LoadFromJson(js: TJsonObject); virtual;
    procedure FromJson(aJson: string); virtual;
    procedure LoadFromJsonFile(sFile: string); virtual;
  end;

  TDropboxAppInfo = record
  public
  end;

  TDropboxAuthBase = class(TDropboxBase)
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
    property Client_Secret: string read FClient_Secret
      write SetClient_Secret;
    procedure LoadFromJson(js: TJsonObject); override;
  end;

  TRESTClientDropBox = class(TRESTClient)
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
    ResponseText:string;
    property AccessToken: string read GetAccessToken write SetAccessToken;
    procedure prepare; virtual;
    constructor create(ow: TComponent); override;
    destructor destroy; override;
    function Get(url: string; AResource: string = ''): string;virtual;
    function GetStream(AUrl: string; AResource: string;
      AStream: TStream): integer;virtual;
    function SendStream(AUrl: string; AStream: TStream): integer;virtual;
    function Post(url: string; AResource: string = ''): string;virtual;
  end;

  TRESTClientDropBoxDataset = class(TRESTClientDropBox)
  private
    RESTResponseDataSetAdapter1: TRESTResponseDataSetAdapter;
    FDMemTable1: TFDMemTable;
    function GetDataset: TDataset;
    procedure SetDataset(const Value: TDataset);
    procedure Setroot(const Value: string);
    function GetRoot: string;
  public
    procedure prepare; override;
    property RootElement: string read GetRoot write Setroot;
    property DataSet: TDataset read GetDataset write SetDataset;
    constructor create(ow: TComponent); override;
    destructor destroy; override;
    function SendStream(AUrl:string; AStream:TStream):integer;override;

  end;

  TDropboxAccountInfo = Record
    email: string;
    display_name: string;
    uid: string;
  private
    JSON: string;
  public
    procedure FromJson(aJson: string);
    procedure LoadFromJson(js: TJsonObject);
  end;

  TDropboxListFolderItem = class
  public
  private
    Frev: string;
    Fmodified: string;
    Fpath: string;
    Frevision: string;
    Fthumb_exists: string;
    Fread_only: string;
    Ficon: string;
    Fis_dir: string;
    Fsize: string;
    Froot: string;
    Fbytes: string;
    procedure Setbytes(const Value: string);
    procedure Seticon(const Value: string);
    procedure Setis_dir(const Value: string);
    procedure Setmodified(const Value: string);
    procedure Setpath(const Value: string);
    procedure Setread_only(const Value: string);
    procedure Setrev(const Value: string);
    procedure Setrevision(const Value: string);
    procedure Setroot(const Value: string);
    procedure Setsize(const Value: string);
    procedure Setthumb_exists(const Value: string);
  public
    procedure FromJson(js: TJsonValue);

    property rev: string read Frev write Setrev;
    property thumb_exists: string read Fthumb_exists write Setthumb_exists;
    property path: string read Fpath write Setpath;
    property is_dir: string read Fis_dir write Setis_dir;
    property icon: string read Ficon write Seticon;
    property read_only: string read Fread_only write Setread_only;
    property bytes: string read Fbytes write Setbytes;
    property modified: string read Fmodified write Setmodified;
    property size: string read Fsize write Setsize;
    property root: string read Froot write Setroot;
    property revision: string read Frevision write Setrevision;

  end;

  TDropboxListFolderList = class(TObjectList<TDropboxListFolderItem>)
  public
    JSON: string;
    procedure FromJson(aJson: String);
  end;

  TDropbox = class(TComponent)
  private
    FStatusCode: integer;
    FDataset: TDataset;
    FRestClient: TRESTClientDropBoxDataset;
    FAuthBase: TDropboxAuthBase;
    FListFolder: TDropboxListFolderList;
    FAccountInfo: TDropboxAccountInfo;
    procedure SetAuthBase(const Value: TDropboxAuthBase);
    procedure SetListFolder(const Value: TDropboxListFolderList);
    procedure SetAccountInfo(const Value: TDropboxAccountInfo);
  protected
    procedure prepare;
  public
    Command: string;
    Response: string;
    function DataSet: TDataset;
    constructor create(ow: TComponent);
    destructor destroy; override;

    function StatusCode: integer;

    function GetAccountInfo: TDropboxAccountInfo;

    function GetListFolder(APath: string; AContinue: boolean = false): boolean;
    procedure UploadFile(AFileName: string; AFile_To: String);
    procedure DownloadFile(file_path: string; saveToPath: string); overload;
    procedure DownloadFile(file_path: string; AStream: TStream); overload;

    property ListFolder: TDropboxListFolderList read FListFolder
      write SetListFolder;
    property AccountInfo: TDropboxAccountInfo read FAccountInfo
      write SetAccountInfo;

  published
    property AuthBase: TDropboxAuthBase read FAuthBase write SetAuthBase;
  end;

implementation

{ TDropbox }
Uses {System.uJson,} Data.DB.Helper;

const
  authServer = 'https://www.dropbox.com';
  apiServer = 'https://api.dropbox.com';
  fileServer = 'https://content.dropboxapi.com';

const
  url_authorize = authServer + '/1/oauth2/authorize';
  url_token = apiServer + '/1/oauth2/token';
  url_signOut = apiServer + '/1/unlink_access_token';
  url_accountInfo = apiServer + '/1/account/info';
  url_getFile = fileServer + '/1/files/auto';
  url_postFile = fileServer + '/1/files/auto';
  url_putFile = fileServer + '/1/files_put/auto';  ///auto
  url_metadata = apiServer + '/1/metadata/auto';
  url_delta = apiServer + '/1/delta';
  url_revisions = apiServer + '/1/revisions/auto/';
  url_restore = apiServer + '/1/restore/auto/';
  url_search = apiServer + '/1/search/auto/';
  url_shares = apiServer + '/1/shares/auto';
  url_media = apiServer + '/1/media/auto';
  url_copyRef = apiServer + '/1/copy_ref/auto';
  url_thumbnails = fileServer + '/1/thumbnails/auto';
  url_chunkedUpload = fileServer + '/1/chunked_upload';
  url_commitChunkedUpload = fileServer + '/1/commit_chunked_upload/auto';
  url_fileopsCopy = apiServer + '/1/fileops/copy';
  url_fileopsCreateFolder = apiServer + '/1/fileops/create_folder';
  url_fileopsDelete = apiServer + '/1/fileops/delete';
  url_fileopsMove = apiServer + '/1/fileops/move';
  url_list_Folder = apiServer + '/2/files/list_folder/';

function Indy_Send_File(AccessToken, AUrl:string; AStream: TStream;
  ASSL: boolean; var AResponseText:string): integer;
var
  LidHTTP: TIdHttp;
begin
  LidHTTP := TIdHttp.create(nil);
  try
    LidHTTP.Request.CustomHeaders.Values['Authorization'] := 'Bearer ' +      AccessToken;
    LidHTTP.Request.ContentType := 'application/octet-stream';
    //LIdHttp.Request.CustomHeaders.Values['Dropbox-API-Arg'] := '{ "path":"/apps/tete.pdf", "mode":"add","autorename": true, "mute": false }';
    if ASSL then
      LidHTTP.IOHandler := TIdSSLIOHandlerSocketOpenSSL.create(LidHTTP);
    AStream.Position := 0;

    try
    LidHTTP.Put(AUrl + '', AStream);
    except
    end;
    AResponseText := LIdHttp.Response.ResponseText;
    result := LidHTTP.ResponseCode;

  finally
    LidHTTP.Free;
  end;
end;

(*

  procedure XUploadFile(LocalPath, RemotePath: string);
  const
  URL = 'https://api-content.dropbox.com/1/files/%s/%s';
  var
  OAuthRequest: TOAuthRequest;
  HMAC: TOAuthSignatureMethod;
  var
  FileStream: TFileStream;
  Ts: TStringList;
  begin
  FileStream := TFileStream.Create(LocalPath, fmOpenRead or fmShareDenyNone);
  try
  if RemotePath[1] = '/' then
  Delete(RemotePath, 1, 1);

  OAuthRequest := TOAuthRequest.Create('');
  HMAC := TYourOAuthSignatureMethod_HMAC_SHA1.Create;
  try
  { TODO -oYou : OAuthRequest does not have a way to set the HTTP method being used.
  It should support this. You will need to add it. }
  OAuthRequest.HTTPURL := Format(URL, [FRoot, URLEncodeRemotePath(RemotePath)]);
  OAuthRequest.FromConsumerAndToken(FOAuthConsumer, FOAuthToken, '');
  OAuthRequest.Sign_Request(HMAC, FOAuthConsumer, FOAuthToken);

  { TODO -oYou : This will likely fail! I will let you figure out why. :-) OAuth.pas does not
  generate the request string properly for this call if memory serves me correctly.
  I have provided some very basic exception handling that will write the error message
  to C:\DropboxErrorMessage.txt. Look at that message. It will tell you what the problem
  is!

  This will fail for the reason above, but also because OAuth.pas only supports HTTP GET
  method, not PUT which is required here. How do you resolve this? }
  try
  FHTTP.Put(OAuthRequest.HTTPURL + '?' + OAuthRequest.GetString, FileStream);
  except
  on E: EIdHTTPProtocolException do
  begin
  Ts := TStringList.Create;
  try
  Ts.Text := E.ErrorMessage;
  Ts.SaveToFile('C:\DropboxErrorMessage.txt');
  finally
  Ts.Free;
  end;
  end;
  end;
  finally
  HMAC.Free;
  OAuthRequest.Free;
  end;
  finally
  FileStream.Free;
  end;
  end;
*)

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

constructor TDropbox.create(ow: TComponent);
begin
  inherited create(ow);
  FAuthBase := TDropboxAuthBase.create;
  FRestClient := TRESTClientDropBoxDataset.create(self);
  FListFolder := TDropboxListFolderList.create; // (true);
end;

function TDropbox.DataSet: TDataset;
begin
  result := FDataset;
end;

destructor TDropbox.destroy;
begin
  FAuthBase.Free;
  FRestClient.Free;
  FListFolder.Free;
  inherited;
end;

procedure TDropbox.DownloadFile(file_path: string; AStream: TStream);
begin
  Command := url_getFile + file_path;
  prepare;
  FStatusCode := FRestClient.GetStream(Command, '', AStream);
end;

procedure TDropbox.DownloadFile(file_path: string; saveToPath: string);
var
  AStream: TMemoryStream;
  rsp: boolean;
begin
  AStream := TMemoryStream.create;
  try
    DownloadFile(file_path, AStream);
    if StatusCode = 200 then
    begin
      AStream.SaveToFile(saveToPath);
    end;
  finally
    AStream.Free;
  end;
end;

function TDropbox.GetAccountInfo: TDropboxAccountInfo;
var
  sJson: string;
begin
  prepare;
  Command := url_accountInfo;
  Response := FRestClient.Get(Command);
  FStatusCode := FRestClient.StatusCode;
  if StatusCode = 200 then
    FAccountInfo.FromJson(Response);
  result := FAccountInfo;
end;

function TDropbox.GetListFolder(APath: string;
  AContinue: boolean = false): boolean;
begin
  prepare;
  Command := url_metadata + APath + '?list';
  Response := FRestClient.Get(Command);
  FStatusCode := FRestClient.StatusCode;
  if StatusCode = 200 then
  begin
    FRestClient.RootElement := 'contents';
    Response := FRestClient.DataSet.ToJson;
    FListFolder.FromJson(Response);
  end;
end;

procedure TDropbox.prepare;
begin
  FRestClient.prepare;
  FRestClient.FAuth2.AccessToken := FAuthBase.AccessToken;
end;

procedure TDropbox.SetAccountInfo(const Value: TDropboxAccountInfo);
begin
  FAccountInfo := Value;
end;

procedure TDropbox.SetAuthBase(const Value: TDropboxAuthBase);
begin
  FAuthBase := Value;
end;

procedure TDropbox.SetListFolder(const Value: TDropboxListFolderList);
begin
  FListFolder := Value;
end;

function TDropbox.StatusCode: integer;
begin
  result := FStatusCode;
end;

procedure TDropbox.UploadFile(AFileName: string; AFile_To: String);
var
  AStream:TFileStream;
begin
  prepare;
  Command := url_putFile + AFile_To;
  AStream :=TFileStream.Create(AFileName,fmOpenRead);
  try
    FStatusCode := FRestClient.SendStream(Command, AStream);
  finally
    AStream.Free;
  end;
end;

{ TRESTClientDropBox }

constructor TRESTClientDropBox.create(ow: TComponent);
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
  HandleRedirects := True;
  RaiseExceptionOn500 := false;

end;

destructor TRESTClientDropBox.destroy;
begin
  FAuth2.Free;
  inherited;
end;

function TRESTClientDropBox.send(url: string; AResource: string;
  AMethod: TRESTRequestMethod): string;
begin
  RESTRequest1.Method := AMethod;
  BaseURL := url;
  RESTRequest1.Execute;
  result := RESTResponse1.Content;
  StatusCode := RESTResponse1.StatusCode;
end;

function TRESTClientDropBox.SendStream(AUrl: string; AStream: TStream): integer;
begin
  result := Indy_Send_File(AccessToken, AUrl,  AStream, True,ResponseText );
end;

procedure TRESTClientDropBox.SetAccessToken(const Value: string);
begin
  FAuth2.AccessToken := Value;
end;

function TRESTClientDropBox.Get(url: string; AResource: string = ''): string;
begin
  result := send(url, AResource, TRESTRequestMethod.rmGET);
end;

function TRESTClientDropBox.GetAccessToken: string;
begin
  result := FAuth2.AccessToken;
end;

function TRESTClientDropBoxDataset.GetRoot: string;
begin
  result := RESTResponseDataSetAdapter1.RootElement;
end;

procedure TRESTClientDropBoxDataset.prepare;
begin
  inherited;

  FDMemTable1.Close;
  FDMemTable1.Fields.Clear;
  RESTResponseDataSetAdapter1.DataSet := FDMemTable1;
  RESTResponseDataSetAdapter1.Response := RESTResponse1;
  RESTResponseDataSetAdapter1.RootElement := '';

end;

function TRESTClientDropBox.GetStream(AUrl, AResource: string;
  AStream: TStream): integer;
begin
  result := Indy_Download_File(AccessToken, AUrl, AStream, True);
end;

function TRESTClientDropBox.Post(url: string; AResource: string = ''): string;
begin
  result := send(url, AResource, TRESTRequestMethod.rmPost);
end;

procedure TRESTClientDropBox.prepare;
begin
  RESTRequest1.Params.Clear;
  RESTRequest1.SynchronizedEvents := false;
end;

{ TDropboxConfig }

procedure TDropboxBase.FromJson(aJson: string);
var
  js: TJsonObject;
begin
  js := TJsonObject.ParseJSONValue(AJson) as TJsonObject;
  try
    LoadFromJson(js);
  finally
    js.Free;
  end;
end;

procedure TDropboxBase.LoadFromJson(js: TJsonObject);
begin
end;

procedure TDropboxBase.LoadFromJsonFile(sFile: string);
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

{ TDropboxAuthBase }

procedure TDropboxAuthBase.LoadFromJson(js: TJsonObject);
begin
  inherited;
  js.TryGetValue<string>('AccessToken', FAccessToken);
  js.TryGetValue<string>('Client_ID', FClient_ID);
  js.TryGetValue<string>('Client_Secret', FClient_Secret);

end;

procedure TDropboxAuthBase.SetAccessToken(const Value: string);
begin
  FAccessToken := Value;
end;

procedure TDropboxAuthBase.SetClient_ID(const Value: string);
begin
  FClient_ID := Value;
end;

procedure TDropboxAuthBase.SetClient_Secret(const Value: string);
begin
  FClient_Secret := Value;
end;

{ TDropboxAccountInfo }

procedure TDropboxAccountInfo.FromJson(aJson: string);
var
  js: TJsonObject;
begin
  js := TJsonObject.ParseJSONValue(aJson) as TJSONObject;
  try
    LoadFromJson(js);
  finally
    js.Free;
  end;
end;

procedure TDropboxAccountInfo.LoadFromJson(js: TJsonObject);
begin
  inherited;
  JSON := js.ToJson;
  js.TryGetValue<string>('email', email);
  js.TryGetValue<string>('display_name', display_name);
  js.TryGetValue<string>('uid', uid);
end;

{ TDropboxListFolderList }

procedure TDropboxListFolderList.FromJson(aJson: String);
var
  J: TJsonArray;
  A: TJsonValue;
  it: TDropboxListFolderItem;
begin
  JSON := aJson;
  J := TJsonObject.ParseJSONValue(aJson) as TJsonArray;
  for A in J do
  begin
    it := TDropboxListFolderItem.create;
    Add(it);
    it.FromJson(A);
  end;
end;

{ TDropboxListFolderItem }

procedure TDropboxListFolderItem.FromJson(js: TJsonValue);
begin
  js.TryGetValue<string>('rev', Frev);
  js.TryGetValue<string>('thumb_exists', Fthumb_exists);
  js.TryGetValue<string>('path', Fpath);
  js.TryGetValue<string>('is_dir', Fis_dir);
  js.TryGetValue<string>('icon', Ficon);
  js.TryGetValue<string>('read_only', Fread_only);
  js.TryGetValue<string>('bytes', Fbytes);
  js.TryGetValue<string>('modified', Fmodified);
  js.TryGetValue<string>('size', Fsize);
  js.TryGetValue<string>('root', Froot);
  js.TryGetValue<string>('revision', Frevision);
end;

procedure TDropboxListFolderItem.Setbytes(const Value: string);
begin
  Fbytes := Value;
end;

procedure TDropboxListFolderItem.Seticon(const Value: string);
begin
  Ficon := Value;
end;

procedure TDropboxListFolderItem.Setis_dir(const Value: string);
begin
  Fis_dir := Value;
end;

procedure TDropboxListFolderItem.Setmodified(const Value: string);
begin
  Fmodified := Value;
end;

procedure TDropboxListFolderItem.Setpath(const Value: string);
begin
  Fpath := Value;
end;

procedure TDropboxListFolderItem.Setread_only(const Value: string);
begin
  Fread_only := Value;
end;

procedure TDropboxListFolderItem.Setrev(const Value: string);
begin
  Frev := Value;
end;

procedure TDropboxListFolderItem.Setrevision(const Value: string);
begin
  Frevision := Value;
end;

procedure TDropboxListFolderItem.Setroot(const Value: string);
begin
  Froot := Value;
end;

procedure TDropboxListFolderItem.Setsize(const Value: string);
begin
  Fsize := Value;
end;

procedure TDropboxListFolderItem.Setthumb_exists(const Value: string);
begin
  Fthumb_exists := Value;
end;

{ TRESTClientDropBoxDataset }

constructor TRESTClientDropBoxDataset.create(ow: TComponent);
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
  RESTResponseDataSetAdapter1.Response := RESTResponse1;
  RESTResponseDataSetAdapter1.NestedElements := True;

end;

destructor TRESTClientDropBoxDataset.destroy;
begin

  inherited;
end;

function TRESTClientDropBoxDataset.GetDataset: TDataset;
begin
  result := RESTResponseDataSetAdapter1.DataSet;
end;

function TRESTClientDropBoxDataset.SendStream(AUrl: string;
  AStream: TStream): integer;
begin
 result := inherited SendStream(AUrl,AStream);
{  DataSet := nil;
  RestRequest1.Params.Clear;
  RestRequest1.ClearBody;
  ContentType := 'application/octet-stream';
  RestRequest1.Method := TRESTRequestMethod.rmPOST;
  BaseURL := AUrl;
  RestRequest1.AddBody(AStream,TRestContentType.ctAPPLICATION_OCTET_STREAM );
  RestRequest1.Execute;   // Erro
}
end;

procedure TRESTClientDropBoxDataset.SetDataset(const Value: TDataset);
begin
  RESTResponseDataSetAdapter1.DataSet := Value;
  if Value = nil then
  begin
    RESTResponseDataSetAdapter1.Response := nil;
    RESTResponseDataSetAdapter1.AutoUpdate := false;
  end
  else
  begin
    RESTResponseDataSetAdapter1.Response := RESTResponse1;
    RESTResponseDataSetAdapter1.AutoUpdate := true;
  end;
end;

procedure TRESTClientDropBoxDataset.Setroot(const Value: string);
begin
  RESTResponseDataSetAdapter1.RootElement := Value;

end;

end.
