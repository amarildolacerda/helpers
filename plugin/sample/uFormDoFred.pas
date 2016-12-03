unit uFormDoFred;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.FileCtrl;

type
  TForm4 = class(TForm)
    DirectoryListBox1: TDirectoryListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}


uses plugin.Service, plugin.MenuItem, plugin.Toolbar;

initialization

// plugin 1
RegisterPlugin(TPluginMenuItemService.Create(TForm4,'',0, 'Navegador de pastas do Fred'));


finalization




end.
