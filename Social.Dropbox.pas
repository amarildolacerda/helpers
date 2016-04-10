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
  REST.JSON,
  System.Generics.Collections, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client;

type

  {TDropboxAppInfo = record
  public
  end;}

  TDropboxAccountInfo = class(TPersistent)
  private
    JSON: string;
    Femail: string;
    Fuid: string;
    Fdisplay_name: string;
    procedure Setdisplay_name(const Value: string);
    procedure Setemail(const Value: string);
    procedure Setuid(const Value: string);
  public
    procedure FromJson(aJson: string);
    procedure LoadFromJson(js: TJsonObject);
  published
    property email: string read Femail write Setemail;
    property display_name: string read Fdisplay_name write Setdisplay_name;
    property uid: string read Fuid write Setuid;
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

  TSocialDropbox = class(TComponent)
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
    procedure SetAccessToken(const Value: string);
    function GetAccessToken: string;
  protected
    procedure prepare;
  public
    //Command: string;
    Response: string;
    function DataSet: TDataset;
    constructor create(ow: TComponent);override;
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
    // property RestClient: TRESTSocialClientDataset read FRestClient write FRestClient;
    property AccessToken: string  read GetAccessToken write SetAccessToken;

  end;

procedure Register;

implementation

{ TDropbox }
// Uses Data.DB.Helper;

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
  url_putFile = fileServer + '/1/files_put/auto';
  /// auto
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

procedure Register;
begin
  RegisterClass(TDropboxAccountInfo);
  RegisterComponents('REST Client', [TSocialDropbox]);
end;

constructor TSocialDropbox.create(ow: TComponent);
begin
  inherited create(ow);
  FAccountInfo := TDropboxAccountInfo.create;
  FAuthBase := TCustomSocialAuthBase.create;
  FRestClient := TRESTSocialClientDataset.create(self);
  FRestClient.Name := 'RestClientInternal';
  FListFolder := TDropboxListFolderList.create; // (true);
end;

function TSocialDropbox.DataSet: TDataset;
begin
  result := FDataset;
end;

destructor TSocialDropbox.destroy;
begin
  FAuthBase.Free;
  FRestClient.Free;
  FListFolder.Free;
  FAccountInfo.Free;
  inherited;
end;

procedure TSocialDropbox.DownloadFile(file_path: string; AStream: TStream);
begin
  prepare;
  FStatusCode := FRestClient.GetStream(url_getFile, file_path, AStream);
end;

procedure TSocialDropbox.DownloadFile(file_path: string; saveToPath: string);
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

function TSocialDropbox.GetAccessToken: string;
begin
  result := AuthBase.AccessToken;
end;

function TSocialDropbox.GetAccountInfo: TDropboxAccountInfo;
var
  sJson: string;
begin
  prepare;
  FRestClient.BaseURL := url_accountInfo;
  Response := FRestClient.Get();
  FStatusCode := FRestClient.StatusCode;
  if StatusCode = 200 then
    FAccountInfo.FromJson(Response);
  result := FAccountInfo;
end;

function TSocialDropbox.GetListFolder(APath: string;
  AContinue: boolean = false): boolean;
var FCommand:string;
begin
  prepare;
  FRestClient.BaseURL := url_metadata;
  FRestClient.Resource := APath + '?list';
  Response := FRestClient.Get();
  FStatusCode := FRestClient.StatusCode;
  if StatusCode = 200 then
  begin
    FRestClient.RootElement := 'contents';
    Response := TJson.ObjectToJsonString(FRestClient.DataSet); // .ToJson;
    FListFolder.FromJson(Response);
  end;
end;

procedure TSocialDropbox.prepare;
begin
  FRestClient.Clear;
  FRestClient.AccessToken := FAuthBase.AccessToken;
end;

procedure TSocialDropbox.SetAccessToken(const Value: string);
begin
  FRestClient.AccessToken := value;
  AuthBase.AccessToken := value;
end;

procedure TSocialDropbox.SetAccountInfo(const Value: TDropboxAccountInfo);
begin
  FAccountInfo := Value;
end;

procedure TSocialDropbox.SetAuthBase(const Value: TCustomSocialAuthBase);
begin
  FAuthBase := Value;
end;

procedure TSocialDropbox.SetListFolder(const Value: TDropboxListFolderList);
begin
  FListFolder := Value;
end;

function TSocialDropbox.StatusCode: integer;
begin
  result := FStatusCode;
end;

procedure TSocialDropbox.UploadFile(AFileName: string; AFile_To: String);
var
  AStream: TFileStream;
  FCommand:string;
begin
  prepare;
  FCommand := url_putFile + AFile_To;
  AStream := TFileStream.create(AFileName, fmOpenRead);
  try
    FStatusCode := FRestClient.SendStream(FCommand, '', AStream);
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
  js := TJsonObject.ParseJSONValue(aJson) as TJsonObject;
  try
    LoadFromJson(js);
  finally
    js.Free;
  end;
end;

procedure TDropboxAccountInfo.LoadFromJson(js: TJsonObject);
begin
  inherited;
  JSON := js.ToString;
  js.TryGetValue<string>('email', Femail);
  js.TryGetValue<string>('display_name', Fdisplay_name);
  js.TryGetValue<string>('uid', Fuid);
end;

procedure TDropboxAccountInfo.Setdisplay_name(const Value: string);
begin
  Fdisplay_name := Value;
end;

procedure TDropboxAccountInfo.Setemail(const Value: string);
begin
  Femail := Value;
end;

procedure TDropboxAccountInfo.Setuid(const Value: string);
begin
  Fuid := Value;
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
