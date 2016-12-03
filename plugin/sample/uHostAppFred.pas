unit uHostAppFred;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, plugin.Manager, Vcl.Menus;

type
  TForm25 = class(TForm)
    MainMenu1: TMainMenu;
    Arquivo1: TMenuItem;
    Ferramentas1: TMenuItem;
    Plugins1: TMenuItem;
    CarregarnovosPlugins1: TMenuItem;
    PluginManager1: TPluginManager;
    procedure CarregarnovosPlugins1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

{$R *.dfm}

procedure TForm25.CarregarnovosPlugins1Click(Sender: TObject);
begin
  GetPluginManager.OpenDialog('');
end;

procedure TForm25.FormCreate(Sender: TObject);
begin
end;

end.
