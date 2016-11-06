program HostApplication;

uses
  Vcl.Forms,
  plugin.Manager,
  wMainMenu in 'wMainMenu.pas' {Form11};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
