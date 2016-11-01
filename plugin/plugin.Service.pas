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

uses WinApi.Windows, System.Classes, System.SysUtils,
{$IFDEF FMX} FMX.Forms, FMX.Controls, System.UITypes, {$ELSE} VCL.Forms,
  VCL.Controls, {$ENDIF}
  Plugin.Interf,
  System.Generics.collections;

Type
  // List of plugins
  TPluginItemsInterfacedClass = class of TPluginItemsInterfaced;

  TPluginItemsInterfaced = class(TInterfacedObject, IPluginItems)
  protected
    FItems: TList<IPluginInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Connection(const AConnectionString: string); virtual;
    function Count: integer;
    function GetItem(idx: integer): IPluginInfo;
    procedure Add(APlugin: IPluginInfo);
    procedure Install; virtual;
    procedure UnInstall; virtual;
  end;

  // plugin base
  TPluginService = class(TComponent, IPluginInfo)
  private
    FTypeID: Int64;
    FParentHandle: THandle;
  protected
    FForm: TForm;
    FOwned: boolean;
    function GetTypeID: Int64; virtual;
    procedure SetTypeID(const Value: Int64); virtual;
    procedure Perform(AMsg: Cardinal; WParam: NativeUInt; LParam: NativeUInt);

  public
    constructor Create; overload;
    destructor Destroy; override;
    function GetAuthor: string; virtual;
    procedure DoStart; virtual;
    function GetInterface: IPluginExecuteBase; virtual;
    function PluginName: string; virtual;
    procedure Embedded(const AParent: THandle); virtual;
    function CanClose: boolean; virtual;
  published
    property TypeID: Int64 read GetTypeID write SetTypeID;
  end;


  // register one plugin to list of plugins
procedure RegisterPlugin(AInfo: IPluginInfo);
procedure RegisterPluginClass(AClass: TPluginItemsInterfacedClass);

// exported plugins from DLL
// return list of plugins in DLL
function LoadPlugin(AAplication: IPluginApplication): IPluginItems;
// exported unload plugins
procedure UnloadPlugin;

function GetPluginItems: IPluginItems;

procedure Register;

var
  PluginExitProc: TProc;
  PluginEnterProc: TProc;

implementation

var
  LPlugin: IPluginItems; 
  LPluginClass: TPluginItemsInterfacedClass;

procedure Register;
begin
  RegisterComponents('Store', [TPluginService]);
end;


procedure RegisterPluginClass(AClass: TPluginItemsInterfacedClass);
begin
  LPluginClass := AClass;
end;

function GetPluginItems: IPluginItems;
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

destructor TPluginService.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

procedure TPluginService.DoStart;
begin

end;

function TPluginService.PluginName: string;
begin
  if assigned(FForm) then
    result := FForm.Caption
  else
    result := self.Name;
end;

procedure TPluginService.Perform(AMsg: Cardinal; WParam, LParam: NativeUInt);
var
  WindRect: TRect;
begin
  if assigned(FForm) then
  begin
    if AMsg = SW_MAXIMIZE then
    begin
      GetWindowRect(FParentHandle, WindRect);
      FForm.Height := WindRect.Height;
      FForm.Width := WindRect.Width;
    end
    else
{$IFDEF FMX}
{$ELSE}
      FForm.Perform(AMsg, WParam, LParam);
{$ENDIF}
  end;
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
  inherited create(nil);
  // PluginService := self;
end;

procedure TPluginService.Embedded(const AParent: THandle);
begin
  if not assigned(FForm) then
    exit;
  FParentHandle := AParent;
  FForm.Left := 0;
  FForm.Top := 0;
  FForm.BorderIcons := [];
{$IFDEF FMX}
  FForm.WindowState := TWindowState.wsMaximized;
  FForm.Show;
{$ELSE}
  WinApi.Windows.SetParent(FForm.Handle, AParent);
  FForm.BorderStyle := bsNone;
  FForm.Align := alClient;
  FForm.Show;
  ShowWindowAsync(FForm.Handle, SW_MAXIMIZE);
{$ENDIF}

end;

function LoadPlugin(AAplication: IPluginApplication): IPluginItems;
var
  i: integer;
begin
  PluginApplication := AAplication;
  result := LPlugin;
  for i := 0 to result.Count - 1 do
    result.GetItem(i).DoStart;
  if assigned(PluginEnterProc) then
    PluginEnterProc;

end;

procedure UnloadPlugin;
var i:integer;
begin
{$IFDEF DLL}
{$ELSE}
{$ENDIF}
  if assigned(PluginExitProc) then
    PluginExitProc;
  Application.ProcessMessages;
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

procedure TPluginItemsInterfaced.Connection(const AConnectionString: string);
begin

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
      i := nil;
      FItems.delete(0);
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

finalization

{$IFDEF DLL}
  // LPlugin := nil;
{$ENDIF}

end.
