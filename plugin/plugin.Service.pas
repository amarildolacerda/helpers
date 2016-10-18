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

  pluginService - Utilizado no plugin a ser consumido pelo Manager.

}

{$D+}
unit Plugin.Service;

interface

uses WinApi.Windows, VCL.Forms, VCL.Controls, Plugin.Interf, System.Generics.collections;

Type
  // List of plugins
  TPluginItemsInterfacedClass = class of TPluginItemsInterfaced;

  TPluginItemsInterfaced = class(TInterfacedObject, IPluginItems)
  protected
    FItems: TList<IPluginInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    function GetItem(idx: integer): IPluginInfo;
    procedure Add(APlugin: IPluginInfo);
    procedure Install; virtual;
    procedure UnInstall; virtual;

  end;

  // plugin base
  TPluginService = class(TInterfacedObject, IPluginInfo)
  private
    FTypeID:Int64;
  protected
    FForm: TForm;
    FOwned: boolean;
    procedure SetTypeID(const Value: Int64);virtual;
    function GetTypeID: Int64; virtual;
  public
    constructor Create; overload;
    function GetAuthor: string; virtual;
    procedure DoStart; virtual;
    function GetInterface: IPluginExecuteBase; virtual;
    function PluginName: string; virtual;
    procedure Embedded(const AParent: THandle); virtual;
    function CanClose: boolean; virtual;
    property TypeID:Int64 read GetTypeID write SetTypeID;
  end;

  // register one plugin to list of plugins
procedure RegisterPlugin(AInfo: IPluginInfo);
procedure RegisterPluginClass(AClass: TPluginItemsInterfacedClass);

// exported plugins from DLL
// return list of plugins in DLL
function LoadPlugin(AAplication: IPluginApplication): IPluginItems;
// exported unload plugins
procedure UnloadPlugin;

function GetPluginItems: TPluginItemsInterfaced;

implementation

uses System.Classes, System.SysUtils;

var
  LPlugin: TPluginItemsInterfaced;
  PluginService: IPluginInfo;
  LPluginClass: TPluginItemsInterfacedClass;

procedure RegisterPluginClass(AClass: TPluginItemsInterfacedClass);
begin
  LPluginClass := AClass;
end;

function GetPluginItems: TPluginItemsInterfaced;
begin
  result := LPlugin;
end;

{ TPluginService<T> }
function TPluginService.GetAuthor: string;
begin
  result := 'storeware';
end;

function TPluginService.GetInterface: IPluginExecuteBase;
begin
  if Supports(FForm, IPluginExecuteBase) then
    result := FForm as IPluginExecuteBase;
end;

procedure TPluginService.DoStart;
begin

end;

function TPluginService.PluginName: string;
begin
  if assigned(FForm) then
    result := FForm.Caption;
end;

procedure TPluginService.SetTypeID(const Value: Int64);
begin
  FTypeID := Value;
end;

function TPluginService.GetTypeID: Int64;
begin
  result := FTypeID;
end;

function TPluginService.CanClose: boolean;
begin
  result := true;
  if not assigned(FForm) then
    exit;
  if Supports(FForm, IPluginExecuteBase) then
    result := (FForm as IPluginExecuteBase).CanClose
  else
  begin
    result := FForm.CloseQuery;
  end;
end;

constructor TPluginService.Create;
begin
  inherited;
  PluginService := self;
end;

procedure TPluginService.Embedded(const AParent: THandle);
begin
  if not assigned(FForm) then
    exit;

  winapi.windows.SetParent(FForm.Handle,AParent);
  FForm.BorderStyle := bsNone;
  FForm.Align := alClient;
  FForm.Show;

end;

function LoadPlugin(AAplication: IPluginApplication): IPluginItems;
var
  i: integer;
begin
  PluginApplication := AAplication;
  result := LPlugin;
  for i := 0 to result.Count - 1 do
    result.GetItem(i).DoStart;
end;

procedure UnloadPlugin;
begin
  LPlugin := nil;
  // PluginApplication := nil;
end;

procedure RegisterPlugin(AInfo: IPluginInfo);
begin
  if not assigned(LPlugin) then
    LPlugin := LPluginClass.Create;
  LPlugin.Add(AInfo);
end;

{ TPluginInterfaced }

procedure TPluginItemsInterfaced.Add(APlugin: IPluginInfo);
begin
  FItems.Add(APlugin);
end;

function TPluginItemsInterfaced.Count: integer;
begin
  result := FItems.Count;
end;

constructor TPluginItemsInterfaced.Create;
begin
  inherited;
  FItems := TList<IPluginInfo>.Create;
end;

destructor TPluginItemsInterfaced.Destroy;
var
  i: IPluginInfo;
begin
  while FItems.Count > 0 do
  begin
    try
      i := FItems.Items[0];
      FItems.delete(0);
      i := nil;
    except
    end;
  end;
  FItems.Free;
  inherited;
end;

function TPluginItemsInterfaced.GetItem(idx: integer): IPluginInfo;
begin
  result := FItems.Items[idx];
end;

procedure TPluginItemsInterfaced.Install;
begin

end;

procedure TPluginItemsInterfaced.UnInstall;
begin

end;

exports LoadPlugin, UnloadPlugin;

initialization
   RegisterPluginClass(TPluginItemsInterfaced);

end.
