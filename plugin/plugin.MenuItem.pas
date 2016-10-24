unit Plugin.MenuItem;

interface

uses System.classes, WinApi.Windows, System.SysUtils,
 {$ifdef FMX} FMX.Forms, FMX.Controls,{$else} VCL.Forms, VCL.Controls,{$endif}
  Plugin.Service, Plugin.Forms,
  Plugin.Interf;

type

  TPluginMenuItemBase = class(TPluginExecuteService, IPluginMenuItem)
  public
    function GetResourceName: String; virtual;
    function GetCaption: string; virtual;
    function GetPath: string; virtual;
    procedure DoClick(const AParent: THandle); virtual;
  end;

  TPluginMenuItemService = class(TPluginMenuItemBase)
  private
    FFormClass: TFormClass;
  protected
    FMenuItemName: string;
    FCaption: String;
    procedure init;Virtual;
    procedure Embedded(const AParent: THandle); override;
  public
    constructor Create(AFormClass: TFormClass; AMenuItemName: string;
      ATypeID: Int64; ACaption: String); virtual;
    class function New(AFormClass: TFormClass; AMenuItemName: string;
      ATypeID: Int64; ACaption: String): IPluginMenuItem;
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

function TPluginMenuItemBase.GetResourceName: String;
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
  AMenuItemName: string; ATypeID: Int64; ACaption: String);
begin
  inherited Create;
  FMenuItemName := AMenuItemName;
  FCaption := ACaption;
  FFormClass := AFormClass;
  TypeID := ATypeID;
end;

procedure TPluginMenuItemService.DoClick(const AParent: THandle);
begin
  init;
  inherited;
end;

procedure TPluginMenuItemService.DoStart;
begin
  inherited;
  PluginApplication.RegisterMenuItem(FMenuItemName, GetCaption, self);
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
  result := self as IPluginMenuItem;
end;

procedure TPluginMenuItemService.init;
begin
  FreeAndNil(FForm);
  SetForm(FFormClass.Create(nil));
  FForm.Caption := FCaption;
  FOwned := true;

end;

class function TPluginMenuItemService.New(AFormClass: TFormClass;
  AMenuItemName: string; ATypeID: Int64; ACaption: String): IPluginMenuItem;
var
  dlg: TPluginMenuItemService;
begin
  dlg := TPluginMenuItemService.Create(AFormClass, AMenuItemName, ATypeID,
    ACaption);
  result := dlg;
end;

end.
