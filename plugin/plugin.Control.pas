unit plugin.Control;

interface

uses {$ifdef FMX} FMX.Forms,{$else} VCL.Forms,{$endif}plugin.Service, plugin.Interf, plugin.Forms;

type

  TPluginControlService = class(TPluginFormService, IPluginControl)
  public
    procedure DoStart; override;
    constructor create(AFormClass: TFormClass; ATypeID, ASubTypeID: int64;
      ACaption: string);
    function GetInterface: IPluginExecuteBase; override;
  end;

implementation

{ TPluginControlService }

constructor TPluginControlService.create(AFormClass: TFormClass; ATypeID,ASubTypeID: int64;
  ACaption: string);
begin
  inherited create(AFormClass, ACaption);
  TypeID := ATypeID;
  subTypeID := ASubTypeID;

end;

procedure TPluginControlService.DoStart;
begin
  PluginApplication.RegisterAttributeControl(GetTypeID, GetSubTypeID, self);
end;


function TPluginControlService.GetInterface: IPluginExecuteBase;
begin
  result := self as IPluginControl;
end;

end.
