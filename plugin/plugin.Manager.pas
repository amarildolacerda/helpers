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
    function LoadPlugins(APath: string; AApplication: IPluginApplication)
      : Integer; overload;
    function LoadPlugins(AApplication: IPluginApplication): Integer; overload;
    function RegisterPlugin(APlugin: string): Integer;
  end;

  TPluginManagerIntf = class(TInterfacedObject, IPluginManager)
  private
    FPath: string;
    FFilename: string;
    procedure SetPath(const Value: string);
  protected
    FList: TList<TPluginInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetApplication: IPluginApplication;
    procedure SetApplication(Const AApplication: IPluginApplication);
    procedure SetFileName(AFilename: string);
    property Path: string read FPath write SetPath;
    function Count: Integer;
    function GetItem(idx: Integer): IPluginItems;
    function Add(AHandle: THandle; AFilename: String;
      APlugins: IPluginItems): Integer;
    function LoadPlugin(APlugin: string): Integer;
    function LoadPlugins(APath: string; AApplication: IPluginApplication)
      : Integer; overload;
    function LoadPlugins(AApplication: IPluginApplication): Integer; overload;
    function RegisterPlugin(APlugin: string): Integer;
  end;

  TPluginAttributeControl = class
  public
    Acesso: Int64;
    PluginExecute: IPluginExecute;
  end;

  TPluginManager = class(TComponent)
  private
    FActive: boolean;
    FOnActive: TNotifyEvent;
    procedure SetPluginPath(const Value: string);
    procedure SetActive(const Value: boolean);
    procedure SetOnActive(const Value: TNotifyEvent);
    function GetPluginPath: string;
  protected
    FPlugins: TPluginManagerIntf;
    FAttributeControls: TObjectList<TPluginAttributeControl>;
  public
    constructor Create(ow: TComponent); override;
    destructor Destroy; override;
    procedure RegisterMenuItem(AMainMenu: TMainMenu; ADefaultMenu: TMenuItem;
      const APath, ACaption: string; ADoExecute: IPluginMenuItem;
      AProc: TProc<TObject>);
    procedure RegisterAttributeControl(const AAcesso: Int64;
      ADoExecute: IPluginExecute);
    function LoadPlugins(APlugin: string): Integer;
    function RegisterPlugin(APlugin: string): Integer;
    property Plugins: TPluginManagerIntf read FPlugins;
    property AttributeControls: TObjectList<TPluginAttributeControl>
      read FAttributeControls;
  published
    property Active: boolean read FActive write SetActive;
    property PluginPath: string read GetPluginPath write SetPluginPath;
    property OnActive: TNotifyEvent read FOnActive write SetOnActive;

  end;

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

uses IniFiles, IniFilesEx, System.uDebug, VCL.Menus.Helpers;

var
  LPluginManager: TPluginManager;

procedure Register;
begin
  RegisterComponents('Store', [TPluginManager]);
end;

procedure TPluginManager.RegisterAttributeControl(const AAcesso: Int64;
  ADoExecute: IPluginExecute);
var
  it: TPluginAttributeControl;
begin
  it := TPluginAttributeControl.Create;
  it.Acesso := AAcesso;
  it.PluginExecute := ADoExecute;
  FAttributeControls.Add(it);
end;

var
  itCount: Integer = 0;

procedure TPluginManager.RegisterMenuItem(AMainMenu: TMainMenu;
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

function LoadPluginService(APlugin: string;
AAppliction: IPluginApplication): Integer;
var
  F: function(APApplication: IPluginApplication): IPluginItems;
  H: THandle;
  i: Integer;
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
        itens := F(AAppliction);
        result := LPluginManager.FPlugins.Add(H, APlugin, itens);
      end
      else
        raise Exception.Create('Não carregou o plugin');
    except
      FreeLibrary(H);
    end;
  end;
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
begin
  result := -1;
  p := TPluginInfo.Create;
  p.Handle := AHandle;
  p.Plugin := APlugins;
  p.Filename := AFilename;
  FList.Add(p);
  result := FList.Count - 1;
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

function TPluginManagerIntf.GetApplication: IPluginApplication;
begin
  result := PluginApplication;
end;

function TPluginManagerIntf.GetItem(idx: Integer): IPluginItems;
begin
  result := FList.Items[idx].Plugin;
end;

function TPluginManagerIntf.LoadPlugin(APlugin: string): Integer;
begin
  result := LoadPluginService(APlugin, PluginApplication);
end;

function TPluginManagerIntf.LoadPlugins(AApplication
  : IPluginApplication): Integer;
begin
  result := LoadPlugins(ExtractFileName(ParamStr(0)), AApplication);
end;

function TPluginManagerIntf.LoadPlugins(APath: string;
AApplication: IPluginApplication): Integer;
var
  i, n: Integer;
  F: string;
  achei: boolean;
begin

  if assigned(AApplication) then
    SetApplication(AApplication);
  if APath = '' then
    APath := FPath;
  result := 0;
  with TIniFile.Create(FFilename) do
    try
      n := 0;
      while readString(APath, 'Plugin' + intToStr(n), '*fim*') <> '*fim*' do
      begin
        try
          F := readString(APath, 'Plugin' + intToStr(n), '');
          if fileExists(F) then
          begin
            achei := false;
            for i := 0 to FList.Count - 1 do
              if sametext(F, FList.Items[i].Filename) then
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

function TPluginManagerIntf.RegisterPlugin(APlugin: string): Integer;
var
  i: Integer;
  app: string;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
    if sametext(APlugin, FList.Items[i].Filename) then
      exit;

  result := LoadPlugin(APlugin);
  if result >= 0 then
  begin
    app := ExtractFileName(ParamStr(0));
    with TIniFile.Create(FFilename) do
      try
        while readString(app, 'Plugin' + intToStr(i), '') <> '' do
          inc(i);
        WriteString(app, 'Plugin' + intToStr(i), APlugin);
      finally
        Free;
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

procedure TPluginManagerIntf.SetPath(const Value: string);
begin
  FPath := Value;
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
      Plugin := nil;
      @p := GetProcAddress(Handle, 'UnloadPlugin');
      if assigned(p) then
        p;
      FreeLibrary(Handle);
    except
    end;
  inherited;
end;

{ TPluginMananer }

constructor TPluginManager.Create(ow: TComponent);
begin
  inherited;
  FPlugins := TPluginManagerIntf.Create;
  FAttributeControls := TObjectList<TPluginAttributeControl>.Create;
end;

destructor TPluginManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FAttributeControls.Count - 1 do
    FAttributeControls.Items[i].PluginExecute := nil;
  FAttributeControls.Free;
  FPlugins.Free;
  inherited;
end;

function TPluginManager.GetPluginPath: string;
begin
  result := FPlugins.Path;
end;

function TPluginManager.LoadPlugins(APlugin: string): Integer;
begin
  result := FPlugins.LoadPlugin(APlugin);
end;

function TPluginManager.RegisterPlugin(APlugin: string): Integer;
begin
  result := FPlugins.RegisterPlugin(APlugin);
end;

procedure TPluginManager.SetActive(const Value: boolean);
begin
  FActive := Value;
  if Value and assigned(FOnActive) then
    FOnActive(self);
end;

procedure TPluginManager.SetOnActive(const Value: TNotifyEvent);
begin
  FOnActive := Value;
end;

procedure TPluginManager.SetPluginPath(const Value: string);
begin
  FPlugins.Path := Value;
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

initialization

LPluginManager := TPluginManager.Create(nil);

Finalization

FreeAndNil(LPluginManager);
PluginApplication := nil;

end.
