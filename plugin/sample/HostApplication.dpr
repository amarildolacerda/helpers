program HostApplication;

uses
  Vcl.Forms,
  wMainMenu in 'wMainMenu.pas' {Form11},
  plugin.ToolBar in '..\plugin.ToolBar.pas',
  plugin.Manager in '..\plugin.Manager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
