unit Unit12_UsandoComponentPluginManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, plugin.interf, plugin.Manager,
  Vcl.Menus,
  Vcl.StdCtrls;

type
  TForm12 = class(TForm)
    MainMenu1: TMainMenu;
    Plugins1: TMenuItem;
    PluginManager1: TPluginManager;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form12: TForm12;

implementation

{$R *.dfm}

procedure TForm12.Button1Click(Sender: TObject);
begin
  PluginManager1.OpenDialog(PluginManager1.filename);
end;

end.
