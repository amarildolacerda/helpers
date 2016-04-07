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
  REST.types, idHTTP, IdSSL, IdSSLOpenSSL, REST.Social, REST.FDSocial,
  System.Generics.Collections, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type


  TDropboxAppInfo = record
  public
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
    FRestClient: TRESTSocialClientDataset;
    FAuthBase: TCustomSocialAuthBase;
    FListFolder: TDropboxListFolderList;
    FAccountInfo: TDropboxAccountInfo;
    procedure SetAuthBase(const Value: TCustomSocialAuthBase);
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
    property AuthBase: TCustomSocialAuthBase read FAuthBase write SetAuthBase;
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


constructor TDropbox.create(ow: TComponent);
begin
  inherited create(ow);
  FAuthBase := TCustomSocialAuthBase.create;
  FRestClient := TRESTSocialClientDataset.create(self);
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
  FRestClient.Clear;
  FRestClient.Auth2.AccessToken := FAuthBase.AccessToken;
end;

procedure TDropbox.SetAccountInfo(const Value: TDropboxAccountInfo);
begin
  FAccountInfo := Value;
end;

procedure TDropbox.SetAuthBase(const Value: TCustomSocialAuthBase);
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
    FStatusCode := FRestClient.SendStream(Command,'', AStream);
  finally
    AStream.Free;
  end;
end;



{ TDropboxConfig }


{ TDropboxAuthBase }

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


end.
