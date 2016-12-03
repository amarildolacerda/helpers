program HostAppFred;

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Vcl.Forms,
  uHostAppFred in 'uHostAppFred.pas' {Form25};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm25, Form25);
  Application.Run;
end.
