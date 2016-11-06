unit uMenuItemSimple;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  plugin.Interf, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, dxGDIPlusClasses, QuickRpt, QRCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    LabeledEdit1: TLabeledEdit;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses  pngimage, plugin.Service, plugin.MenuItem, plugin.Control;

procedure TForm1.Button1Click(Sender: TObject);
begin
  close;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
   if fileExists('embedded.png') then
      Image1.Picture.LoadFromFile('embedded.png');
end;

initialization

RegisterPlugin(TPluginMenuItemService.Create(TForm1,'',1, 'Menu base de sample'));
RegisterPlugin(TPluginControlService.create(TForm1,1,0,'my control embbedded'));


finalization

end.
