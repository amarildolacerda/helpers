unit uExecutePluignSimple;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList;

type
  TForm2 = class(TForm)
    ImageList1: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses plugin.Service, plugin.MenuItem, plugin.Toolbar;

type
  TPluginToolbarServiceEx = class(TPluginToolbarService)
  public
    function GetResourceName: String; override;
  end;

{ TPluginToolbarServiceEx }

function TPluginToolbarServiceEx.GetResourceName: String;
begin
   result := 'bitmap';
end;

initialization

// plugin 1
RegisterPlugin(TPluginMenuItemService.Create(TForm2,'',0, 'Menu base de sample2'));

// plugin 2  (can be another form)
RegisterPlugin(TPluginToolbarServiceEx.Create(TForm2,'',0, 'XxxxX'));

finalization

end.
