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

{
  Amarildo Lacerda
  03/10/2016

  PluginManager é utilizado no Application que consumir um plugin a ser
  utilizado;

}

unit Plugin.Manager;

{$I plugin.inc}

interface

uses
  Windows, System.Classes, System.SysUtils, Plugin.Interf,
  VCL.Menus, System.Generics.Collections;

type
  TPluginInfo = class
  public
    Handle: THandle;
    Plugin: IPluginItems;
    Filename: string;
    constructor Create;
    destructor Destroy; override;
  end;

  IPluginManager = interface
    ['{F5A6F163-06BC-4021-9AF9-905797FC6F4F}']
    function Count: Integer;
    function GetApplication: IPluginApplication;
    procedure SetApplication(Const AApplication: IPluginApplication);
    procedure SetFileName(AFilename: string);
    function GetItem(idx: Integer): IPluginItems;
    function Add(AHandle: THandle; AFilename: string;
      APlugins: IPluginItems): Integer;
    function LoadPlugin(APlugin: string): Integer;
    function LoadPlugins(APlugin: string; AApplication: IPluginApplication)
      : Integer; overload;
    function Open(AApplication: IPluginApplication): Integer; overload;
    function InstallPlugin(APlugin: string): Integer;
    procedure UnInstallPlugin(APlugin: string);
    procedure Connection(const AConnectionString: string);
    procedure User(const AFilial: Integer; const AAppUser: string);
  end;

  TPluginManagerIntf = class(TInterfacedObject, IPluginManager)
  private
    FFilename: string;
    FConnectionString: string;
    FAppUser: string;
    FFilial: Integer;
  protected
    FList: TList<TPluginInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetApplication: IPluginApplication;
    procedure SetApplication(Const AApplication: IPluginApplication);
    procedure SetFileName(AFilename: string);
    function Count: Integer;
    function GetItem(idx: Integer): IPluginItems;
    function Find(APlugin: string): TPluginInfo;
    function IndexOf(APlugin: string): Integer;
    function Add(AHandle: THandle; AFilename: String;
      APlugins: IPluginItems): Integer;
    function LoadPlugin(APlugin: string): Integer;
    function LoadPlugins(AFilename: string; AApplication: IPluginApplication)
      : Integer; overload;

    procedure Connection(const AConnectionString: string);
    procedure User(const AFilial: Integer; const AAppUser: string);
    function Open(AApplication: IPluginApplication): Integer; overload;

    property Filename: string read FFilename write SetFileName;
    function InstallPlugin(APlugin: string): Integer;
    procedure UnInstallPlugin(APlugin: string);
  end;

  TPluginAttributeControl = class
  public
    TypeID: Int64;
    SubTypeID: Int64;
    PluginExecute: IPluginExecute;
  end;

  TPluginApplicationMenuItemEvent = procedure(const AParentMenuItemName,
    ACaption: string; ADoExecute: IPluginMenuItem) of object;
  TPluginApplicationToolbarItemEvent = procedure(const AParentItemName,
    ACaption: string; ADoExecute: IPluginToolbarItem) of object;
  TPluginApplicationControlEvent = procedure(const AType, ASubType: Int64;
    ADoExecute: IPluginControl) of object;

  TPluginManager = class(TComponent, IPluginApplication)
  private
    FonRegisterMenuItem: TPluginApplicationMenuItemEvent;
    FonRegisterToolbarItem: TPluginApplicationToolbarItemEvent;
    FonRegisterControl: TPluginApplicationControlEvent;
    FPlugins: TPluginManagerIntf;
    FAttributeControls: TObjectList<TPluginAttributeControl>;
    FActive: boolean;
    FOnActive: TNotifyEvent;
    procedure SetActive(const Value: boolean);
    procedure SetOnActive(const Value: TNotifyEvent);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetonRegisterMenuItem(const Value
      : TPluginApplicationMenuItemEvent);
    procedure SetonRegisterToolbarItem(const Value
      : TPluginApplicationToolbarItemEvent);
    procedure SetonRegisterControl(const Value: TPluginApplicationControlEvent);
  protected
    // IPluginApplication
    procedure &RegisterMenuItem(const AParentMenuItemName, ACaption: string;
      ADoExecute: IPluginMenuItem);
    procedure &RegisterToolbarItem(const AParentItemName, ACaption: string;
      ADoExecute: IPluginToolbarItem);
    procedure &RegisterAttributeControl(const AType, ASubType: Int64;
      ADoExecute: IPluginControl);
  public
    constructor Create(ow: TComponent); override;
    destructor Destroy; override;

    // default create item
    procedure NewMenuItem(AMainMenu: TMainMenu; ADefaultMenu: TMenuItem;
      const APath, ACaption: string; ADoExecute: IPluginMenuItem;
      AProc: TProc<TObject>);overload;
    procedure NewAttributeControl(const ATypeID, ASubTypeID: Int64;
      ADoExecute: IPluginExecute);overload;
    procedure NewEmbbedControl(AParentHandle: THandle; AControlID: Int64;
      AControlType: Int64 = 0); overload; virtual;
    procedure EmbbedControl(AControlID: Int64; AControlType: Int64;
      AProc: TProc<IPluginExecute>); overload; virtual;

    // jobs
    function LoadPlugins(APlugin: string): Integer; virtual;
    function InstallPlugin(APlugin: string): Integer; virtual;
    procedure UnInstallPlugin(APlugin: string); virtual;

    // list os plugins
    property Plugins: TPluginManagerIntf read FPlugins;
    // list of plugins controls
    property AttributeControls: TObjectList<TPluginAttributeControl>
      read FAttributeControls;

    // operations
    procedure Open(AApp: IPluginApplication);
    procedure Connection(const AConnectionString:String);
    procedure User(const AFilial: Integer; const AAppUser: string);

  published
    // events fire when plugin start
    property onRegisterMenuItem: TPluginApplicationMenuItemEvent
      read FonRegisterMenuItem write SetonRegisterMenuItem;
    property onRegisterToolbarItem: TPluginApplicationToolbarItemEvent
      read FonRegisterToolbarItem write SetonRegisterToolbarItem;
    property onRegisterControl: TPluginApplicationControlEvent
      read FonRegisterControl write SetonRegisterControl;

    // aditional
    property Filename: string read GetFileName write SetFileName;
    property Active: boolean read FActive write SetActive;
    property OnActive: TNotifyEvent read FOnActive write SetOnActive;

  end;

  // extens TMenuItem do implements anonimous and Interfaced menu item;
  TPluginMenuItemInterf = class(TMenuItem)
  protected
    FProc: TProc<TObject>;
    procedure DoClick(Sender: TObject);
  public
    PluginMenuItem: IPluginMenuItem;
    constructor Create(AOwner: TComponent; AProc: TProc<TObject>); overload;
  end;

function GetPluginManager: TPluginManager;

procedure Register;

implementation

uses {$ifndef DLL} plugin.Service,  {$endif}
 IniFiles,{$ifdef USE_INIFILEEx} IniFilesEx,{$endif} System.uDebug, VCL.Menus.Helpers;

var
  LPluginManager: TPluginManager;

procedure Register;
begin
  RegisterComponents('Store', [TPluginManager]);
end;

procedure TPluginManager.NewAttributeControl(const ATypeID,
  ASubTypeID: Int64; ADoExecute: IPluginExecute);
var
  it: TPluginAttributeControl;
begin
  it := TPluginAttributeControl.Create;
  it.TypeID := ATypeID;
  it.SubTypeID := ASubTypeID;
  it.PluginExecute := ADoExecute;
  FAttributeControls.Add(it);
end;

var
  itCount: Integer = 0;

procedure TPluginManager.NewMenuItem(AMainMenu: TMainMenu;
  ADefaultMenu: TMenuItem; const APath, ACaption: string;
  ADoExecute: IPluginMenuItem; AProc: TProc<TObject>);
var
  it: TPluginMenuItemInterf;
  itClient: TMenuItem;
begin
  inc(itCount);
  // procura o menu para mostrar
  itClient := AMainMenu.FindItem(APath);
  if itClient = nil then
    itClient := ADefaultMenu; // se nao encontrou pega um padrao

  if not assigned(AProc) then
    AProc := (
      procedure(Sender: TObject)
      begin
        with TPluginMenuItemInterf(Sender) do
          PluginMenuItem.DoClick(0);
      end);

  // cria o menu
  it := TPluginMenuItemInterf.Create(AMainMenu, AProc);
  it.Name := 'mnPlugin_' + formatDatetime
    ('hhmmsszzz_' + intToStr(itCount), now);
  it.PluginMenuItem := ADoExecute;
  it.Caption := (it.PluginMenuItem as IPluginMenuItem).GetCaption;
  // adiciona o menu na lista
  itClient.Add(it);

end;

function GetPluginManager: TPluginManager;
begin
  result := LPluginManager;
end;

{ TPluginManager }

function TPluginManagerIntf.Add(AHandle: THandle; AFilename: String;
APlugins: IPluginItems): Integer;
var
  p: TPluginInfo;
  I: Integer;
begin
  result := -1;
  p := TPluginInfo.Create;
  p.Handle := AHandle;
  p.Plugin := APlugins;
  p.Filename := AFilename;
  for I := 0 to APlugins.Count - 1 do
  begin
    APlugins.GetItem(I).GetInterface.Connection(FConnectionString);
    APlugins.GetItem(I).GetInterface.User(FFilial, FAppUser);
  end;
  FList.Add(p);
  result := FList.Count - 1;
end;

procedure TPluginManagerIntf.Connection(const AConnectionString: string);
var
  I, n: Integer;
begin
  FConnectionString := AConnectionString;
  for I := 0 to FList.Count - 1 do
    for n := 0 to FList.Items[I].Plugin.Count - 1 do
      FList.Items[I].Plugin.GetItem(n).GetInterface.Connection(FConnectionString);
end;

function TPluginManagerIntf.Count: Integer;
begin
  result := FList.Count;
end;

constructor TPluginManagerIntf.Create;
begin
  inherited;
  FList := TList<TPluginInfo>.Create;
  FFilename := 'Plugin.ini';
end;

destructor TPluginManagerIntf.Destroy;
var
  p: TPluginInfo;
begin
  while FList.Count > 0 do
  begin
    p := FList.Items[FList.Count - 1];
    p.Free;
    FList.Delete(FList.Count - 1);
  end;
  FList.Free;
  inherited;
end;

function TPluginManagerIntf.Find(APlugin: string): TPluginInfo;
var
  I: Integer;
begin
  result := nil;
  I := IndexOf(APlugin);
  if I >= 0 then
    result := FList.Items[I];
end;

function TPluginManagerIntf.GetApplication: IPluginApplication;
begin
  result := PluginApplication;
end;

function TPluginManagerIntf.GetItem(idx: Integer): IPluginItems;
begin
  result := FList.Items[idx].Plugin;
end;

function TPluginManagerIntf.LoadPlugin(APlugin: string): Integer;
var
  F: function(APApplication: IPluginApplication): IPluginItems;
  H: THandle;
  I: Integer;
  itens: IPluginItems;
begin
  result := -1;
  H := LoadLibrary(PWideChar(APlugin));
  if H > 0 then
  begin
    try
      @F := GetProcAddress(H, 'LoadPlugin');
      if assigned(F) then
      begin
        itens := F(PluginApplication);
        result := LPluginManager.FPlugins.Add(H, APlugin, itens);
      end
      else
        raise Exception.Create('Não carregou o plugin');
    except
      FreeLibrary(H);
    end;
  end;
end;

function TPluginManagerIntf.Open(AApplication: IPluginApplication): Integer;
{$ifndef DLL}
var n,i:integer;
    it:IPluginItems;
begin
   it := GetPluginItems;
   if assigned(it) then
   with GetPluginItems do
    for I := 0 to Count-1  do
      GetItem(I).DoStart;
{$else}
begin
{$endif}
  result := LoadPlugins(ExtractFileName(ParamStr(0)), AApplication);
end;

function TPluginManagerIntf.LoadPlugins(AFilename: string;
AApplication: IPluginApplication): Integer;
var
  I, n: Integer;
  F: string;
  achei: boolean;
begin

  if assigned(AApplication) then
    SetApplication(AApplication);
  if AFilename = '' then
    AFilename := FFilename;
  if FFilename = '' then
    FFilename := AFilename;
  result := 0;
  with TIniFile.Create(FFilename) do
    try
      n := 0;
      while readString(AFilename, 'Plugin' + intToStr(n), '*fim*') <> '*fim*' do
      begin
        try
          F := readString(AFilename, 'Plugin' + intToStr(n), '');
          if fileExists(F) then
          begin
            achei := false;
            for I := 0 to FList.Count - 1 do
              if sametext(F, FList.Items[I].Filename) then
              begin // check if repeat the same DLL
                achei := true;
                break;
              end;
            if not achei then
            begin
              LoadPlugin(F);
              inc(result);
            end;
          end;
        except
          on e: Exception do
            DebugLog('Erro LoadPlugins <' + F + '>: ' + e.message);
        end;
        inc(n);
      end;
    finally
      Free;
    end;
end;

function TPluginManagerIntf.IndexOf(APlugin: string): Integer;
var
  I: Integer;
begin
  result := -1;
  for I := 0 to FList.Count - 1 do
    if sametext(APlugin, FList.Items[I].Filename) then
    begin
      result := I;
      exit;
    end;
end;

function TPluginManagerIntf.InstallPlugin(APlugin: string): Integer;
var
  I: Integer;
  app: string;
  info: TPluginInfo;
begin
  result := -1;
  I := IndexOf(APlugin);
  if I >= 0 then
    exit;

  result := LoadPlugin(APlugin);
  if result >= 0 then
  begin
    I := 0;
    app := ExtractFileName(ParamStr(0));
    with TIniFile.Create(FFilename) do
      try
        while readString(app, 'Plugin' + intToStr(I), '') <> '' do
          inc(I);
        WriteString(app, 'Plugin' + intToStr(I), APlugin);
      finally
        Free;
      end;

    info := Find(APlugin);
    if assigned(info) then
    begin
      info.Plugin.Connection(FConnectionString);
      info.Plugin.Install;
    end;

  end;
end;

procedure TPluginManagerIntf.SetApplication(const AApplication
  : IPluginApplication);
begin
  PluginApplication := AApplication;
end;

procedure TPluginManagerIntf.SetFileName(AFilename: string);
begin
  FFilename := AFilename;
end;

procedure TPluginManagerIntf.UnInstallPlugin(APlugin: string);
var
  info: TPluginInfo;
begin
  info := Find(APlugin);
  if assigned(info) then
    info.Plugin.UnInstall;
end;

procedure TPluginManagerIntf.User(const AFilial: Integer;
const AAppUser: string);
var
  I, n: Integer;
begin
  FFilial := AFilial;
  FAppUser := AAppUser;
  for I := 0 to FList.Count - 1 do
    for n := 0 to FList.Items[I].Plugin.Count - 1 do
      FList.Items[I].Plugin.GetItem(n).GetInterface.User(FFilial, FAppUser);

end;

{ TPluginInfo }

constructor TPluginInfo.Create;
begin
  inherited;
  Handle := 0;
end;

destructor TPluginInfo.Destroy;
var
  p: procedure;
begin
  if Handle > 0 then
    try
      @p := GetProcAddress(Handle, 'UnloadPlugin');
      if assigned(p) then
        p;
      Plugin := nil;
      FreeLibrary(Handle);
    except
    end;
  inherited;
end;

{ TPluginMananer }

procedure TPluginManager.Connection(const AConnectionString:string);
begin
  Plugins.Connection(AConnectionString);
end;

constructor TPluginManager.Create(ow: TComponent);
begin
  inherited;
  PluginApplication := self;
  FPlugins := TPluginManagerIntf.Create;
  FAttributeControls := TObjectList<TPluginAttributeControl>.Create;
end;

destructor TPluginManager.Destroy;
var
  I: Integer;
begin
  FAttributeControls.Free;
  FPlugins.Free;
  inherited;
end;

procedure TPluginManager.EmbbedControl(AControlID, AControlType: Int64;
AProc: TProc<IPluginExecute>);
var
  I: Integer;
  intf: IPluginExecute;
begin
  for I := 0 to FAttributeControls.Count - 1 do
    with FAttributeControls.Items[I] do
      if (TypeID = AControlID) and (SubTypeID = AControlType) then
      begin
        intf := PluginExecute;
        AProc(intf);
      end;

end;

function TPluginManager.GetFileName: string;
begin
  result := FPlugins.Filename;
end;

procedure TPluginManager.NewEmbbedControl(AParentHandle: THandle;
AControlID, AControlType: Int64);
var
  I: Integer;
begin
  for I := 0 to FAttributeControls.Count - 1 do
    with FAttributeControls.Items[I] do
      if (TypeID = AControlID) and (SubTypeID = AControlType) then
      begin
        FAttributeControls.Items[I].PluginExecute.Embedded(AParentHandle);
      end;

end;

function TPluginManager.LoadPlugins(APlugin: string): Integer;
begin
  result := FPlugins.LoadPlugin(APlugin);
end;

procedure TPluginManager.Open(AApp: IPluginApplication);
begin
  Plugins.Open(AApp);
end;

function TPluginManager.InstallPlugin(APlugin: string): Integer;
begin
  result := FPlugins.InstallPlugin(APlugin);
end;

procedure TPluginManager.SetActive(const Value: boolean);
begin
  if not assigned(PluginApplication) then
    raise Exception.Create('Não inicializou o objeto PluginApplication');
  if Value then
    if not(csDesigning in ComponentState) then
    begin
      FPlugins.Open(PluginApplication);
      if assigned(FOnActive) then
        FOnActive(self);
    end;
  FActive := Value;

end;

procedure TPluginManager.SetFileName(const Value: string);
begin
  FPlugins.Filename := Value;
end;

procedure TPluginManager.SetOnActive(const Value: TNotifyEvent);
begin
  FOnActive := Value;
end;

procedure TPluginManager.UnInstallPlugin(APlugin: string);
begin
  FPlugins.UnInstallPlugin(APlugin);
end;

procedure TPluginManager.User(const AFilial: Integer; const AAppUser: string);
begin
  Plugins.User(AFilial, AAppUser);
end;

{ TMenuItemInterf }

constructor TPluginMenuItemInterf.Create(AOwner: TComponent;
AProc: TProc<TObject>);
begin
  inherited Create(AOwner);
  FProc := AProc;
  inherited OnClick := DoClick;
end;

procedure TPluginMenuItemInterf.DoClick(Sender: TObject);
begin
  FProc(self);
end;

{ TPluginApplication }

procedure TPluginManager.RegisterAttributeControl(const AType,
  ASubType: Int64; ADoExecute: IPluginControl);
begin
  if assigned(FonRegisterControl) then
    FonRegisterControl(AType, ASubType, ADoExecute);

end;

procedure TPluginManager.RegisterMenuItem(const AParentMenuItemName,
  ACaption: string; ADoExecute: IPluginMenuItem);
begin
  if assigned(FonRegisterMenuItem) then
    FonRegisterMenuItem(AParentMenuItemName, ACaption, ADoExecute);
end;

procedure TPluginManager.RegisterToolbarItem(const AParentItemName,
  ACaption: string; ADoExecute: IPluginToolbarItem);
begin
  if assigned(FonRegisterToolbarItem) then
    FonRegisterToolbarItem(AParentItemName, ACaption, ADoExecute);
end;

procedure TPluginManager.SetonRegisterControl
  (const Value: TPluginApplicationControlEvent);
begin
  FonRegisterControl := Value;
end;

procedure TPluginManager.SetonRegisterMenuItem
  (const Value: TPluginApplicationMenuItemEvent);
begin
  FonRegisterMenuItem := Value;
end;

procedure TPluginManager.SetonRegisterToolbarItem
  (const Value: TPluginApplicationToolbarItemEvent);
begin
  FonRegisterToolbarItem := Value;
end;

initialization

LPluginManager := TPluginManager.Create(nil);

Finalization

FreeAndNil(LPluginManager);
PluginApplication := nil;

end.
