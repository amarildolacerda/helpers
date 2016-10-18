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

unit Plugin.Forms;

interface

uses WinApi.windows, VCL.Forms, VCL.Controls, System.classes, System.SysUtils,
  Plugin.Service, Plugin.Interf;

type

  TPluginExecuteService = class(TPluginService, IPluginExecute)
  protected
    FHandle:THandle;
    FIndex:integer;
  protected
    destructor Destroy; override;
    procedure SetForm(const Value: TForm); virtual;
    function GetForm(AParent: THandle): TForm; virtual;
    function GetCaption: string; virtual;

    function GetHandle:THandle;
    procedure SetHandle(AHandle:THandle);

    function PluginIdent: int64; virtual;
    procedure Connect(AAlias: string; AUser: string; APass: string); virtual;
    procedure User(AFilial: integer; AAppUser: string); virtual;
    procedure Sync(AJson: string); virtual;
    procedure Execute(AModal: boolean); virtual;

    function GetAuthor: string; virtual;
    function GetName: string; virtual;
    function GetVersion: string; virtual;
    function GetDescription: string; virtual;
  public
  end;

  TPluginFormService = class(TPluginExecuteService)
  protected
    FCaption: string;
    FFormClass: TFormClass;
    procedure Init;
  public
    constructor Create(AFormClass: TFormClass; ACaption: String); virtual;
    procedure Execute(AModal: boolean); override;
    procedure Embedded(const AParent: THandle); override;
    procedure DoStart; override;
  end;


implementation

uses System.classes.Helper, System.Rtti;

procedure TPluginExecuteService.Connect(AAlias, AUser, APass: string);
begin
  if not assigned(FForm) then
    exit;
  if Supports(FForm, IPluginExecuteBase) then
    (FForm as IPluginExecuteBase).Connect(AAlias, AUser, APass)
  else
  begin
    FForm.Properties['Aliasname'] := AAlias;
    FForm.Properties['UserName'] := AUser;
    FForm.Properties['Password'] := APass;
  end;
end;

destructor TPluginExecuteService.Destroy;
begin
  if FOwned then
    FreeAndNil(FForm);
  inherited;
end;

procedure TPluginExecuteService.Execute(AModal: boolean);
begin
  if not assigned(FForm) then
    exit;
  if AModal then
    FForm.ShowModal
  else
    FForm.Show;
end;

function TPluginExecuteService.GetAuthor: string;
begin
  result := 'Storeware';
end;

function TPluginExecuteService.GetCaption: string;
begin
  if assigned(FForm) then
    result := FForm.Caption
  else
    result := '';
end;

function TPluginExecuteService.GetDescription: string;
begin
  result := 'Plugin';
end;

function TPluginExecuteService.GetForm(AParent: THandle): TForm;
begin
  result := FForm;
  if AParent > 0 then
    WinApi.windows.SetParent(FForm.Handle, AParent);
end;

function TPluginExecuteService.GetHandle: THandle;
begin
   result := FHandle;
end;

function TPluginExecuteService.GetName: string;
begin
  result := 'Storeware';
end;

function TPluginExecuteService.GetVersion: string;
begin
  result := '01.00';
end;

function TPluginExecuteService.PluginIdent: int64;
begin
  result := 0;
end;

procedure TPluginExecuteService.SetForm(const Value: TForm);
begin
  FOwned := false;
  if assigned(FForm) then
    FreeAndNil(FForm);
  FForm := Value;
end;

procedure TPluginExecuteService.SetHandle(AHandle: THandle);
begin
   FHandle := AHandle;
end;

procedure TPluginExecuteService.Sync(AJson: string);
begin
  if not assigned(FForm) then
    exit;
  if Supports(FForm, IPluginExecuteBase) then
    (FForm as IPluginExecuteBase).Sync(AJson)
  else
  begin
    FForm.InvokeMethod('Sync', [AJson]);
  end;
end;

procedure TPluginExecuteService.User(AFilial: integer; AAppUser: string);
begin
  if not assigned(FForm) then
    exit;
  if Supports(FForm, IPluginExecuteBase) then
    (FForm as IPluginExecuteBase).User(AFilial, AAppUser)
  else
  begin
    FForm.Properties['Filial'] := AFilial;
    FForm.Properties['Usuario'] := AAppUser;
  end;
end;

{ TPluginFormService }

constructor TPluginFormService.Create(AFormClass: TFormClass; ACaption: String);
begin
  inherited Create;
  FFormClass := AFormClass;
  FCaption := ACaption;
end;

procedure TPluginFormService.DoStart;
begin
  inherited;
end;

procedure TPluginFormService.Embedded(const AParent: THandle);
begin
  Init;
  inherited;
end;

procedure TPluginFormService.Execute(AModal: boolean);
begin
  Init;
  inherited;

end;

procedure TPluginFormService.Init;
begin
  FreeAndNil(FForm);
  SetForm(FFormClass.Create(nil));
  FForm.Caption := FCaption;
  FOwned := true;

end;

end.
