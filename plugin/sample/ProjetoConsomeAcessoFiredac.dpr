program ProjetoConsomeAcessoFiredac;

uses
  Vcl.Forms,
  uConsomeAcessoFiredac in 'uConsomeAcessoFiredac.pas' {Form24};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm24, Form24);
  Application.Run;
end.
