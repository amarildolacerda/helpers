program Host_PluginManagerCmp;

uses
  Vcl.Forms,
  Unit12_UsandoComponentPluginManager in 'Unit12_UsandoComponentPluginManager.pas' {Form12};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
