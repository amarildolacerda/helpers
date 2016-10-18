unit Plugin.MenuItem;

interface

uses System.classes, WinApi.Windows, System.SysUtils, VCL.Forms, VCL.Controls,
  Plugin.Service, Plugin.Forms,
  Plugin.Interf;

type


  TPluginMenuItemBase = class(TPluginExecuteService, IPluginMenuItem)
  public
    function GetResourceName:String; virtual;
    function GetCaption: string; virtual;
    function GetPath: string; virtual;
    procedure DoClick(const AParent: THandle); virtual;
  end;


  TPluginMenuItemService = class(TPluginMenuItemBase)
  private
    FCaption: String;
    FFormClass: TFormClass;
    procedure init;
    procedure Embedded(const AParent: THandle); override;
  public
    constructor Create(AFormClass: TFormClass; ACaption: String); virtual;
    procedure DoStart; override;
    function GetInterface: IPluginExecuteBase; override;
    function GetCaption: string; override;
    procedure DoClick(const AParent: THandle); override;
  end;

implementation


{ TPluginFormMenuService }

procedure TPluginMenuItemBase.DoClick(const AParent: THandle);
var
  LForm: TForm;
begin
  LForm := GetForm(AParent);
  if LForm = nil then
    exit;
  LForm.ShowModal;
end;

function TPluginMenuItemBase.GetResourceName:String;
begin
  result := '';
end;

function TPluginMenuItemBase.GetCaption: string;
begin
  result := 'Menu item ' + extractFileName(ParamStr(0));
end;

function TPluginMenuItemBase.GetPath: string;
begin
  result := '';
end;


{ TPluginMenuItemService<T> }

constructor TPluginMenuItemService.Create(AFormClass: TFormClass;
  ACaption: String);
begin
  inherited Create;
  FCaption := ACaption;
  FFormClass := AFormClass;
end;

procedure TPluginMenuItemService.DoClick(const AParent: THandle);
begin
  init;
  inherited;
end;

procedure TPluginMenuItemService.DoStart;
begin
  inherited;
  PluginApplication.RegisterMenuItem(  '', GetCaption, self);
end;

procedure TPluginMenuItemService.Embedded(const AParent: THandle);
begin
  init;
  inherited;
end;

function TPluginMenuItemService.GetCaption: string;
begin
  result := FCaption;
end;

function TPluginMenuItemService.GetInterface: IPluginExecuteBase;
begin
  result := self as IPluginMenuitem;
end;

procedure TPluginMenuItemService.init;
begin
  FreeAndNil(FForm);
  SetForm(FFormClass.Create(nil));
  FForm.Caption := FCaption;
  FOwned := true;

end;

end.
