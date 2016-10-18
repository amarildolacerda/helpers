unit plugin.ToolBar;

interface

uses plugin.Interf, plugin.MenuItem;

type
  TPluginToolbarService = class(TPluginMenuItemService, IPluginToolbarItem)
  public
    procedure DoStart; override;
    function GetInterface: IPluginExecuteBase; override;
  end;

implementation

{ TPluginToolbarService }

procedure TPluginToolbarService.DoStart;
begin
  PluginApplication.RegisterToolbarItem('',GetCaption,self);
end;

function TPluginToolbarService.GetInterface: IPluginExecuteBase;
begin
  result := self as IPluginToolbarItem;
end;

end.
