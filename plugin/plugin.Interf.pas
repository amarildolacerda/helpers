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

unit Plugin.Interf;

interface

uses  {$ifdef FMX} FMX.Controls {$else}  VCL.Controls {$endif};

type

  {
    DLL - implements some plugins
    |--IPluginItems
    |----------IPluginInfo 1
    |-----------------IPluginMenuItem
    |----------IPluginInfo 2
    |-----------------IPluginMenuItem
    |----------IPluginInfo 3
    |-----------------IPluginExecute
    |----------IPluginInfo 4
    |-----------------IPluginToolbarItem

    Host (main application) - Implements IPluginApplication

  }


  IPluginExecuteBase=interface;

  // ---------------------------------------------------------------------------
  // Plugin Controls
  // ---------------------------------------------------------------------------

  // with plugin implements an interface
  IPluginInfo = interface
    ['{1BBC2D91-BF8A-4976-9EF5-201A518D62A3}']
    procedure DoStart;
    function PluginName: string;
    function GetTypeID: Int64;
    function GetAuthor: string;
    function GetInterface: IPluginExecuteBase; // retorna a interface que implementa.
  end;

  // list of plugins interface in the same DLL
  // a DLL can implement a lot  IPluginInfo (interfaces)
  // return by DLL function LoadPlugin
  IPluginItems = interface
    ['{756E63BE-4C02-46AF-85AD-87BDB657201F}']
    function Count: integer;
    function GetItem(idx: integer): IPluginInfo;
    procedure Connection(const AConnectionString: string);
    procedure Add( AInfo:IPluginInfo);

    procedure Install;
    procedure UnInstall;
  end;


  // ---------------------------------------------------------------------------
  // Interfaces Plugins implments
  // ---------------------------------------------------------------------------

  IPluginExecuteBase = interface
    ['{4F0858A2-A92A-4705-9E74-5C4BEBB68F02}']
  //  function GetHandle:THandle;
  //  procedure SetHandle(AHandle:THandle);
    function GetCaption: string;
    function GetTypeID: Int64;
    procedure SetTypeID(const ATypeID:Int64);
    procedure Connection(const AConnectionString:string);
    procedure SetParams( AJsonParams:String );
    procedure User(const AFilial: integer; const AAppUser: string);
    procedure Sync(const AJson: string);
    procedure Perform(AMsg:Cardinal;WParam:NativeUInt;LParam:NativeUInt);
    // {"control":xxx,...."operation":"open"}  // TPluginOperation = (open,close,edit,insert,post,delete)
    function CanClose: Boolean;
  end;

  IPluginExecute = interface(IPluginExecuteBase)
    ['{73D9055C-56B3-4ADC-98E6-4F5F0B2D930F}']
    procedure Execute(const AModal: Boolean);
    procedure Embedded(const AParent: THandle);
  end;

  IPluginControl = interface(IPluginExecute)
    ['{84B1D051-D13D-4D72-BE63-26757FED98AB}']
    function GetSubTypeID: Int64;
  end;

  IPluginMenuItem = interface(IPluginExecuteBase)
    ['{7188528B-A818-4739-9FA4-F3A383305C56}']
    function GetPath: string;
    procedure DoClick(const AParent: THandle);
    procedure Embedded(const AParent: THandle);
    function CanClose: Boolean;
    function GetResourceName:String;
  end;

  IPluginToolbarItem = interface(IPluginMenuItem)
  end;

  // ----------------------------------------------------------------------------
  // Host interface  (Main Application)
  // ----------------------------------------------------------------------------
  IPluginApplication = interface
    ['{6ED989EA-E8B5-4435-A0BC-33685CFE7EEB}']
    procedure RegisterMenuItem(const AParentMenuItemName, ACaption: string;
      ADoExecute: IPluginMenuItem);
    procedure RegisterToolbarItem(const AParentItemName, ACaption: string;
      ADoExecute: IPluginToolbarItem);
    procedure RegisterAttributeControl(const AType,ASubType: Int64;
      ADoExecute: IPluginControl);
  end;

var
  PluginApplication: IPluginApplication; // both plugin and host can set value

implementation


initialization

finalization
  PluginApplication := nil;

end.
