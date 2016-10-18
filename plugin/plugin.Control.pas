unit plugin.Control;

interface

uses Forms, plugin.Interf, plugin.Forms;

type

  TPluginControlService = class(TPluginFormService, IPluginControl)
  private
    FAccess: int64;
  public
    procedure DoStart; override;
    function GetAccess: int64; virtual;
    constructor create(AFormClass: TFormClass; AAccess: int64;
      ACaption: string);
    function GetInterface: IPluginExecuteBase; override;
  end;

implementation

{ TPluginControlService }

constructor TPluginControlService.create(AFormClass: TFormClass; AAccess: int64;
  ACaption: string);
begin
  inherited create(AFormClass, ACaption);
  FAccess := AAccess;
end;

procedure TPluginControlService.DoStart;
begin
  PluginApplication.RegisterAttributeControl(GetAccess, self);
end;

function TPluginControlService.GetAccess: int64;
begin
  result := FAccess;
end;

function TPluginControlService.GetInterface: IPluginExecuteBase;
begin
  result := self as IPluginControl;
end;

end.
