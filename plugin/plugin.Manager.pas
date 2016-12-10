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

unit plugin.Manager;

{$I plugin.inc}

interface

uses

  Windows, System.Classes, System.SysUtils, plugin.Interf,
{$IFDEF FMX} FMX.Menus, {$ELSE} WinAPI.GDIPObj, VCL.Menus, {$ENDIF}
  System.Generics.Collections,
  System.IOUtils;

type
  TPluginInfo = class
  public
    Handle: THandle;
    plugin: IPluginItems;
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
    procedure LoadPluginPath(APath: string);
    function LoadPlugin(APlugin: string): Integer;
    function LoadPlugins(AFilename: string; AApplication: IPluginApplication)
      : Integer; overload;

    procedure Connection(const AConnectionString: string);
    procedure User(const AFilial: Integer; const AAppUser: string);
    function Open(AApplication: IPluginApplication): Integer; overload;
    procedure Sync(const AJson: string);

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

  TPluginBeforeExecuteEvent = procedure(const AExecute: IPluginExecuteBase;
    var AContinue: boolean) of object;

  TPluginApplication = class(TInterfacedObject, IPluginApplication)
    procedure RegisterMenuItem(const AParentMenuItemName, ACaption: string;
      ADoExecute: IPluginMenuItem);
    procedure RegisterToolbarItem(const AParentItemName, ACaption: string;
      ADoExecute: IPluginToolbarItem);
    procedure RegisterAttributeControl(const AType, ASubType: Int64;
      ADoExecute: IPluginControl);
  end;

  TPluginManager = class(TComponent { , IPluginApplication } )
  private
    FonRegisterMenuItem: TPluginApplicationMenuItemEvent;
    FonRegisterToolbarItem: TPluginApplicationToolbarItemEvent;
    FonRegisterControl: TPluginApplicationControlEvent;
    FPlugins: TPluginManagerIntf;
    FAttributeControls: TObjectList<TPluginAttributeControl>;
    FActive: boolean;
    FOnActive: TNotifyEvent;
    FMenuItem: TMenuItem;
    FMainMenu: TMainMenu;
    FonBeforeExecute: TPluginBeforeExecuteEvent;
    FLocalPluginPath: string;
    procedure SetActive(const Value: boolean);
    procedure SetOnActive(const Value: TNotifyEvent);
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure SetonRegisterMenuItem(const Value
      : TPluginApplicationMenuItemEvent);
    procedure SetonRegisterToolbarItem(const Value
      : TPluginApplicationToolbarItemEvent);
    procedure SetonRegisterControl(const Value: TPluginApplicationControlEvent);
    procedure SetMainMenu(const Value: TMainMenu);
    procedure SetMenuItem(const Value: TMenuItem);
    procedure SetonBeforeExecute(const Value: TPluginBeforeExecuteEvent);
    procedure SetLocalPluginPath(const Value: string);
  protected
    // IPluginApplication
    procedure &RegisterMenuItem(const AParentMenuItemName, ACaption: string;
      ADoExecute: IPluginMenuItem);
    procedure &RegisterToolbarItem(const AParentItemName, ACaption: string;
      ADoExecute: IPluginToolbarItem);
    procedure &RegisterAttributeControl(const AType, ASubType: Int64;
      ADoExecute: IPluginControl);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

  public
    constructor Create(ow: TComponent); override;
    destructor Destroy; override;
    procedure Perform(ATypeID: Int64; AMsg: Cardinal; WParam: NativeUInt;
      LParam: NativeUInt);
    procedure SendCommand(ACommand: string; ABody: string);
    function CreateCommand(ACommand: string; ABody: string): string;
    // default create item
    procedure NewMenuItem(AMainMenu: TMainMenu; ADefaultMenu: TMenuItem;
      const APath, ACaption: string; ADoExecute: IPluginMenuItem;
      AProc: TProc<TObject>); overload;
    procedure NewAttributeControl(const ATypeID, ASubTypeID: Int64;
      ADoExecute: IPluginExecute); overload;

    procedure EmbedControl(AParentHandle: THandle; AControlID: Int64;
      AControlType: Int64 = 0); overload; virtual;
    procedure EmbedControl(AControlID: Int64; AControlType: Int64;
      AProc: TProc<IPluginExecute>); overload; virtual;

    // jobs
    procedure LoadPluginPath(APath: string); virtual;
    function LoadPlugins(APlugin: string): Integer; virtual;
    function InstallPlugin(APlugin: string): Integer; virtual;
    procedure UnInstallPlugin(APlugin: string); virtual;
    procedure OpenDialog(AIniFile: string);

    // list os plugins
    property Plugins: TPluginManagerIntf read FPlugins;
    // list of plugins controls
    property AttributeControls: TObjectList<TPluginAttributeControl>
      read FAttributeControls;

    // operations
    procedure Open(AApp: IPluginApplication);
    procedure Connection(const AConnectionString: String);
    procedure User(const AFilial: Integer; const AAppUser: string);
    procedure Sync(const AJson: string);

  published
    // events fire when plugin start

    property onRegisterMenuItem: TPluginApplicationMenuItemEvent
      read FonRegisterMenuItem write SetonRegisterMenuItem;
    property onRegisterToolbarItem: TPluginApplicationToolbarItemEvent
      read FonRegisterToolbarItem write SetonRegisterToolbarItem;
    property onRegisterControl: TPluginApplicationControlEvent
      read FonRegisterControl write SetonRegisterControl;
    property onBeforeExecute: TPluginBeforeExecuteEvent read FonBeforeExecute
      write SetonBeforeExecute;

    // aditional
    property Filename: string read GetFileName write SetFileName;
    property LocalPluginPath: string read FLocalPluginPath
      write SetLocalPluginPath;
    property MainMenu: TMainMenu read FMainMenu write SetMainMenu;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property OnActive: TNotifyEvent read FOnActive write SetOnActive;
    property Active: boolean read FActive write SetActive;
  end;

  // extens TMenuItem to implements anonimous and Interfaced menu item;
  TRecInterfaced = record
    Controller: IPluginMenuItem;
  end;

  TPluginMenuItemInterf = class(TMenuItem)
  protected
    FProc: TProc<TObject>;
    procedure DoClick(Sender: TObject);
  public
    [unsafe]
    PluginMenuItem: TRecInterfaced;
    constructor Create(AOwner: TComponent; AProc: TProc<TObject>); overload;
  end;

function GetPluginManager: TPluginManager;

procedure Register;

implementation

uses
  plugin.Common,
{$IFNDEF DLL} plugin.Service, {$ENDIF}
{$IFNDEF BPL}plugin.FormManager, {$ENDIF}
  IniFiles {$IFDEF USE_INIFILEEx}, IniFilesEx{$ENDIF};

var
  LPluginManager: TPluginManager;

procedure Register;
begin
  RegisterComponents('Store', [TPluginManager]);
end;

procedure TPluginManager.NewAttributeControl(const ATypeID, ASubTypeID: Int64;
  ADoExecute: IPluginExecute);
var
  it: TPluginAttributeControl;
begin
  it := TPluginAttributeControl.Create;
  it.TypeID := ATypeID;
  it.SubTypeID := ASubTypeID;
  it.PluginExecute := ADoExecute;
  FAttributeControls.Add(it);
end;

{$IFDEF FMX}
{$ELSE}

type
  TMenuItemHelper = class helper for TMenuItem
  public
    function FindItem(const aName: string): TMenuItem;
  end;

function TMenuItemHelper.FindItem(const aName: string): TMenuItem;
var
  i: Integer;
begin
  result := nil;
  if sametext(Name, aName) then
  begin
    result := self;
    exit;
  end
  else
    for i := 0 to Count - 1 do
    begin
      result := items[i].FindItem(aName);
      if assigned(result) then
        exit;
    end;
end;

type
  TMainMenuHelper = class helper for TMainMenu
  public
    function FindItem(const aName: string): TMenuItem;
  end;

function TMainMenuHelper.FindItem(const aName: string): TMenuItem;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to items.Count - 1 do
  begin
    result := items[i].FindItem(aName);
    if assigned(result) then
      exit;
  end;
end;

{$ENDIF}

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
  itClient := nil;
{$IFDEF FMX}
{$ELSE}
  // procura o menu para mostrar
  itClient := AMainMenu.FindItem(APath);
{$ENDIF}
  //ADefaultMenu.

  if itClient = nil then
  begin
    itClient := ADefaultMenu; // se nao encontrou pega um padrao
    if itClient.caption='-' then
       itClient := ADefaultMenu.Parent;
  end;

  if not assigned(AProc) then
    AProc := (
      procedure(Sender: TObject)
      var
        AContinue: boolean;
      begin
        with TPluginMenuItemInterf(Sender) do
        begin
          AContinue := true;
          if assigned(FonBeforeExecute) then
            FonBeforeExecute(PluginMenuItem.Controller, AContinue);
          if AContinue then
            PluginMenuItem.Controller.DoClick(0);
        end;
      end);

  // cria o menu
  it := TPluginMenuItemInterf.Create(AMainMenu, AProc);
  it.Name := 'mnPlugin_' + formatDatetime
    ('hhmmsszzz_' + intToStr(itCount), now);
  it.PluginMenuItem.Controller := ADoExecute;
{$IFDEF FMX}
  it.text := (it.PluginMenuItem as IPluginMenuItem).GetCaption;
{$ELSE}
  it.Caption := (it.PluginMenuItem.Controller as IPluginMenuItem).GetCaption;
  itClient.Add(it);
{$ENDIF}
  // adiciona o menu na lista

end;

procedure TPluginManager.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  inherited;
  if (Operation = TOperation.opRemove) then
  begin
    if (AComponent = FMainMenu) then
      FMainMenu := nil;
    if AComponent = FMenuItem then
      FMenuItem := nil;
  end;
end;

var
  LPluginMangerLocal: boolean = false;

function GetPluginManager: TPluginManager;
begin
  if not assigned(LPluginManager) then
  begin
    LPluginManager := TPluginManager.Create(nil);
    LPluginMangerLocal := true;
  end;
  result := LPluginManager;
end;

{ TPluginManager }

function TPluginManagerIntf.Add(AHandle: THandle; AFilename: String;
APlugins: IPluginItems): Integer;
var
  p: TPluginInfo;
  i: Integer;
begin
  result := -1;
  p := TPluginInfo.Create;
  p.Handle := AHandle;
  p.plugin := APlugins;
  p.Filename := AFilename;
  for i := 0 to APlugins.Count - 1 do
  begin
    APlugins.GetItem(i).GetInterface.Connection(FConnectionString);
    APlugins.GetItem(i).GetInterface.User(FFilial, FAppUser);
  end;
  FList.Add(p);
  result := FList.Count - 1;
end;

procedure TPluginManagerIntf.Connection(const AConnectionString: string);
var
  i, n: Integer;
begin
  FConnectionString := AConnectionString;
  for i := 0 to FList.Count - 1 do
    for n := 0 to FList.items[i].plugin.Count - 1 do
      FList.items[i].plugin.GetItem(n).GetInterface.Connection
        (FConnectionString);
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
    try
      p := FList.items[FList.Count - 1];
      p.Free;
    except
    end;
    FList.Delete(FList.Count - 1);
  end;
  FList.Free;
  inherited;
end;

function TPluginManagerIntf.Find(APlugin: string): TPluginInfo;
var
  i: Integer;
begin
  result := nil;
  i := IndexOf(APlugin);
  if i >= 0 then
    result := FList.items[i];
end;

function TPluginManagerIntf.GetApplication: IPluginApplication;
begin
  result := PluginApplication;
end;

function TPluginManagerIntf.GetItem(idx: Integer): IPluginItems;
begin
  result := FList.items[idx].plugin;
end;

function TPluginManagerIntf.LoadPlugin(APlugin: string): Integer;
var
  F: function(APApplication: IPluginApplication): IPluginItems;
  H: THandle;
  i: Integer;
  itens: IPluginItems;
begin
  result := -1;
  H := LoadLibrary(PWideChar(ExpandConstant(APlugin)));
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
      raise;
    end;
  end;
end;

procedure TPluginManagerIntf.LoadPluginPath(APath: string);
var
  LFileMask: string;
  LSearchRec: TSearchRec;
begin
  APath := ExpandConstant(APath);
  LFileMask := TPath.Combine(APath, '*.dll');
  ForceDirectories(ExtractFileDir(LFileMask));
  if FindFirst(LFileMask, faAnyFile, LSearchRec) = 0 then
    try
      repeat
        LoadPlugin(TPath.Combine(APath, LSearchRec.Name));
      until FindNext(LSearchRec) <> 0;
    finally
      findClose(LSearchRec);
    end;
end;

function TPluginManagerIntf.Open(AApplication: IPluginApplication): Integer;
{$IFNDEF DLL}
var
  n, i: Integer;
  it: IPluginItems;
begin
  it := GetPluginItems;
  if assigned(it) then
    with GetPluginItems do
      for i := 0 to Count - 1 do
      begin
        GetItem(i).DoStart;
      end;
{$ELSE}

begin
{$ENDIF}
  result := LoadPlugins(ExtractFileName(ParamStr(0)), AApplication);
end;

function TPluginManagerIntf.LoadPlugins(AFilename: string;
AApplication: IPluginApplication): Integer;
var
  i, n: Integer;
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
            for i := 0 to FList.Count - 1 do
              if sametext(F, FList.items[i].Filename) then
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
            raise Exception.Create('Erro LoadPlugins <' + F + '>: ' +
              e.message);
        end;
        inc(n);
      end;
    finally
      Free;
    end;
end;

function TPluginManagerIntf.IndexOf(APlugin: string): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
    if sametext(APlugin, FList.items[i].Filename) then
    begin
      result := i;
      exit;
    end;
end;

function TPluginManagerIntf.InstallPlugin(APlugin: string): Integer;
var
  i: Integer;
  app: string;
  info: TPluginInfo;
begin
  result := -1;
  i := IndexOf(APlugin);
  if i >= 0 then
    exit;

  result := LoadPlugin(APlugin);
  if result >= 0 then
  begin
    i := 0;
    app := ExtractFileName(ParamStr(0));
    with TIniFile.Create(FFilename) do
      try
        while readString(app, 'Plugin' + intToStr(i), '') <> '' do
          inc(i);
        WriteString(app, 'Plugin' + intToStr(i), APlugin);
      finally
        Free;
      end;

    info := Find(APlugin);
    if assigned(info) then
    begin
      info.plugin.Connection(FConnectionString);
      info.plugin.Install;
    end;

  end;
end;

procedure TPluginManagerIntf.SetApplication(const AApplication
  : IPluginApplication);
begin
  if AApplication <> PluginApplication then
    PluginApplication := AApplication;
end;

procedure TPluginManagerIntf.SetFileName(AFilename: string);
begin
  FFilename := AFilename;
end;

procedure TPluginManagerIntf.Sync(const AJson: string);
  procedure process(AExecute: IPluginExecuteBase; ACommand: string);
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        TThread.Queue(nil,
          procedure
          begin
            try
              AExecute.Sync(ACommand);
            except
            end;
          end);
      end).start;
  end;

var
  i, n: Integer;
begin
  for i := 0 to FList.Count - 1 do
    for n := 0 to FList.items[i].plugin.Count - 1 do
      process(FList.items[i].plugin.GetItem(n).GetInterface, AJson);
end;

procedure TPluginManagerIntf.UnInstallPlugin(APlugin: string);
var
  info: TPluginInfo;
begin
  info := Find(APlugin);
  if assigned(info) then
    info.plugin.UnInstall;
end;

procedure TPluginManagerIntf.User(const AFilial: Integer;
const AAppUser: string);
var
  i, n: Integer;
begin
  FFilial := AFilial;
  FAppUser := AAppUser;
  for i := 0 to FList.Count - 1 do
    for n := 0 to FList.items[i].plugin.Count - 1 do
      FList.items[i].plugin.GetItem(n).GetInterface.User(FFilial, FAppUser);

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
      plugin := nil;
      sleep(100);
      FreeLibrary(Handle);
    except
    end;
  inherited;
end;

{ TPluginMananer }

procedure TPluginManager.Connection(const AConnectionString: string);
begin
  if assigned(FPlugins) then
    FPlugins.Connection(AConnectionString);
end;

constructor TPluginManager.Create(ow: TComponent);
begin
  inherited;
  FLocalPluginPath := '{app}\Plugins';
  if not assigned(LPluginManager) then
    LPluginManager := self;
  FPlugins := TPluginManagerIntf.Create;
  FAttributeControls := TObjectList<TPluginAttributeControl>.Create;
end;

function TPluginManager.CreateCommand(ACommand: string; ABody: string): string;
begin
  if ABody = '' then
    ABody := '""';
  result := '{ "command":"' + ACommand + '", "body":' + ABody + '}';
end;

destructor TPluginManager.Destroy;
var
  i: Integer;
begin
  FAttributeControls.Free;
  FPlugins.Free;
  inherited;
end;

procedure TPluginManager.EmbedControl(AControlID, AControlType: Int64;
AProc: TProc<IPluginExecute>);
var
  i: Integer;
  intf: IPluginExecute;
  AContinue: boolean;
begin
  if assigned(FAttributeControls) then
    for i := 0 to FAttributeControls.Count - 1 do
      with FAttributeControls.items[i] do
        if (TypeID = AControlID) and
          ((SubTypeID = AControlType) or (AControlType = -1)) then
        begin
          AContinue := true;
          if assigned(FonBeforeExecute) then
            FonBeforeExecute(FAttributeControls.items[i].PluginExecute,
              AContinue);
          if AContinue then
          begin
            PluginExecute.Connection(FPlugins.FConnectionString);
            PluginExecute.User(FPlugins.FFilial, FPlugins.FAppUser);
            intf := PluginExecute;
            AProc(intf);
            intf.Sync(CreateCommand('start', ''));
          end;
        end;
end;

function TPluginManager.GetFileName: string;
begin
  if assigned(FPlugins) then
    result := FPlugins.Filename;
end;

procedure TPluginManager.EmbedControl(AParentHandle: THandle;
AControlID, AControlType: Int64);
var
  i: Integer;
  AContinue: boolean;
begin
  if assigned(FAttributeControls) then
    for i := 0 to FAttributeControls.Count - 1 do
      with FAttributeControls.items[i] do
        if (TypeID = AControlID) and (SubTypeID = AControlType) then
        begin
          AContinue := true;
          if assigned(FonBeforeExecute) then
            FonBeforeExecute(FAttributeControls.items[i].PluginExecute,
              AContinue);
          if AContinue then
            with FAttributeControls.items[i] do
            begin
              PluginExecute.Connection(FPlugins.FConnectionString);
              PluginExecute.User(FPlugins.FFilial, FPlugins.FAppUser);
              PluginExecute.Embedded(AParentHandle);
              PluginExecute.Sync(CreateCommand('start', ''));
            end;
        end;
end;

procedure TPluginManager.LoadPluginPath(APath: string);
begin
  if assigned(FPlugins) then
    FPlugins.LoadPluginPath(APath);
end;

function TPluginManager.LoadPlugins(APlugin: string): Integer;
begin
  if assigned(FPlugins) then
    result := FPlugins.LoadPlugin(APlugin);
end;

procedure TPluginManager.Open(AApp: IPluginApplication);
begin
  if assigned(FPlugins) then
    Plugins.Open(AApp);
end;

procedure TPluginManager.OpenDialog(AIniFile: string);
begin
{$IFNDEF BPL}
  if AIniFile = '' then
    AIniFile := Filename;
  // install news plugins
  with PluginFormManagerDlg do
    try
      Filename := AIniFile;
      ShowModal;
    finally
    end;
{$ENDIF}
end;

procedure TPluginManager.Perform(ATypeID: Int64; AMsg: Cardinal;
WParam, LParam: NativeUInt);
var
  i, n: Integer;
  intf: IPluginInfo;
begin
  if assigned(FPlugins) then
    for i := 0 to Plugins.Count - 1 do
      for n := 0 to Plugins.GetItem(i).Count - 1 do
      begin
        intf := Plugins.GetItem(i).GetItem(n);
        if (ATypeID = 0) or (ATypeID = intf.GetTypeID) then
          intf.GetInterface.Perform(AMsg, WParam, LParam);
      end;

end;

function TPluginManager.InstallPlugin(APlugin: string): Integer;
begin
  result := FPlugins.InstallPlugin(APlugin);
end;

procedure TPluginManager.SendCommand(ACommand, ABody: string);
begin
  Sync(CreateCommand(ACommand, ABody));
end;

procedure TPluginManager.SetActive(const Value: boolean);
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Queue(nil,
        procedure
        begin
          if not(csDesigning in ComponentState) then
          begin
            if not assigned(PluginApplication) then
              raise Exception.Create
                ('Não inicializou o objeto PluginApplication');
            if Value then
            begin
              FPlugins.Open(PluginApplication);
              if FLocalPluginPath <> '' then
                FPlugins.LoadPluginPath(FLocalPluginPath);
              if assigned(FOnActive) then
                FOnActive(self);
            end;
          end;
        end);
    end).start;
  FActive := Value;

end;

procedure TPluginManager.SetFileName(const Value: string);
begin
  if assigned(FPlugins) then
    FPlugins.Filename := Value;
end;

procedure TPluginManager.SetLocalPluginPath(const Value: string);
begin
  FLocalPluginPath := Value;
end;

procedure TPluginManager.SetMainMenu(const Value: TMainMenu);
begin
  FMainMenu := Value;
end;

procedure TPluginManager.SetMenuItem(const Value: TMenuItem);
begin
  FMenuItem := Value;
end;

procedure TPluginManager.SetOnActive(const Value: TNotifyEvent);
begin
  FOnActive := Value;
end;

procedure TPluginManager.UnInstallPlugin(APlugin: string);
begin
  if assigned(FPlugins) then
    FPlugins.UnInstallPlugin(APlugin);
end;

procedure TPluginManager.User(const AFilial: Integer; const AAppUser: string);
begin
  if assigned(FPlugins) then
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

procedure TPluginManager.RegisterAttributeControl(const AType, ASubType: Int64;
ADoExecute: IPluginControl);
begin
  if assigned(FonRegisterControl) then
    FonRegisterControl(AType, ASubType, ADoExecute)
  else
    NewAttributeControl(AType, ASubType, ADoExecute);

end;

procedure TPluginManager.RegisterMenuItem(const AParentMenuItemName,
  ACaption: string; ADoExecute: IPluginMenuItem);
begin
  if assigned(FonRegisterMenuItem) then
    FonRegisterMenuItem(AParentMenuItemName, ACaption, ADoExecute)
  else if assigned(FMainMenu) and assigned(FMenuItem) then
    NewMenuItem(FMainMenu, FMenuItem, AParentMenuItemName, ACaption,
      ADoExecute, nil);
end;

procedure TPluginManager.RegisterToolbarItem(const AParentItemName,
  ACaption: string; ADoExecute: IPluginToolbarItem);
begin
  if assigned(FonRegisterToolbarItem) then
    FonRegisterToolbarItem(AParentItemName, ACaption, ADoExecute);
end;

procedure TPluginManager.SetonBeforeExecute(const Value
  : TPluginBeforeExecuteEvent);
begin
  FonBeforeExecute := Value;
end;

procedure TPluginManager.SetonRegisterControl(const Value
  : TPluginApplicationControlEvent);
begin
  FonRegisterControl := Value;
end;

procedure TPluginManager.SetonRegisterMenuItem(const Value
  : TPluginApplicationMenuItemEvent);
begin
  FonRegisterMenuItem := Value;
end;

procedure TPluginManager.SetonRegisterToolbarItem
  (const Value: TPluginApplicationToolbarItemEvent);
begin
  FonRegisterToolbarItem := Value;
end;

procedure TPluginManager.Sync(const AJson: string);
begin
  if assigned(FPlugins) then
    FPlugins.Sync(AJson);

end;

{ TPluginApplication }

procedure TPluginApplication.RegisterAttributeControl(const AType,
  ASubType: Int64; ADoExecute: IPluginControl);
begin
  GetPluginManager.RegisterAttributeControl(AType, ASubType, ADoExecute);
end;

procedure TPluginApplication.RegisterMenuItem(const AParentMenuItemName,
  ACaption: string; ADoExecute: IPluginMenuItem);
begin
  GetPluginManager.RegisterMenuItem(AParentMenuItemName, ACaption, ADoExecute);
end;

procedure TPluginApplication.RegisterToolbarItem(const AParentItemName,
  ACaption: string; ADoExecute: IPluginToolbarItem);
begin
  GetPluginManager.RegisterToolbarItem(AParentItemName, ACaption, ADoExecute);
end;

initialization

PluginApplication := TPluginApplication.Create() as IPluginApplication;

Finalization

if LPluginMangerLocal then
  FreeAndNil(LPluginManager);
PluginApplication := nil;

end.
