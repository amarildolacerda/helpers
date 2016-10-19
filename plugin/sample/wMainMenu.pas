unit wMainMenu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, plugin.Interf,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ToolWin, Vcl.ComCtrls,
  Vcl.Buttons, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm11 = class(TForm, IPluginApplication)
    MainMenu1: TMainMenu;
    Plugins1: TMenuItem;
    InstallPlugins1: TMenuItem;
    ools1: TMenuItem;
    ToolBar1: TToolBar;
    Panel1: TPanel;
    Button1: TButton;
    procedure InstallPlugins1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure RegisterMenuItem(const APath, ACaption: string;
      ADoExecute: IPluginMenuItem);
    procedure RegisterToolbarItem(const APath, ACaption: string;
      ADoExecute: IPluginToolbarItem);
    procedure RegisterAttributeControl(const ATypeID,ASubTypeID: Int64;
      ADoExecute: IPluginControl);

  public
    { Public declarations }
    function GetPluginIni:string;
  end;

var
  Form11: TForm11;

implementation

{$R *.dfm}

uses plugin.FormManager, plugin.Manager;

procedure TForm11.Button1Click(Sender: TObject);
begin
 // procura um plugin que se encaixa nos parametros indicados.
 // se estiver usando Um TPageControl, pode por cada plugin em uma aba.
 GetPluginManager.NewEmbbedControl(Panel1.Handle,1,0);
end;

procedure TForm11.FormShow(Sender: TObject);
begin
  // load plugins
  GetPluginManager.Plugins.SetFileName(GetPluginIni);
  GetPluginManager.Plugins.Open(self);
end;

function TForm11.GetPluginIni: string;
begin
  result := ExtractFilePath( ParamStr(0) )+'plugin.ini';
end;

procedure TForm11.InstallPlugins1Click(Sender: TObject);
begin
  // install news plugins
  with PluginFormManagerDlg do
    try
      Filename := GetPluginIni;
      ShowModal;
    finally
    end;
end;

procedure TForm11.RegisterAttributeControl(const ATypeID,ASubTypeID: Int64;
      ADoExecute: IPluginControl);
begin
  GetPluginManager.NewAttributeControl(ATypeID,ASubTypeID,ADoExecute);
end;

procedure TForm11.RegisterMenuItem(const APath, ACaption: string;
  ADoExecute: IPluginMenuItem);
begin
  GetPluginManager.newMenuItem(MainMenu1, Plugins1, APath, ACaption,
    ADoExecute, nil);
   { another option:....
     GetPluginManager.RegisterMenuItem(MainMenu1, Plugins1, APath, ACaption,
    ADoExecute,
    procedure(Sender: TObject)
    var
      item: TPluginMenuItemInterf;
    begin
      item := TPluginMenuItemInterf(Sender);
      item.PluginMenuItem.DoClick(0);
    end);}
end;

type
   // hack TToolButton
   TToolButtonEx = class(TSpeedButton)
     public
       Data:IPluginToolBarItem;
       constructor create(ow:TComponent);override;
       procedure DoClick(sender:TObject);
   end;
{ TToolButtonEx }
constructor TToolButtonEx.create(ow: TComponent);
begin
  inherited;
  OnClick := DoClick;
end;

procedure TToolButtonEx.DoClick(sender: TObject);
begin
  data.DoClick(0);
end;

procedure TForm11.RegisterToolbarItem(const APath, ACaption: string;
ADoExecute: IPluginToolbarItem);
var tb : TToolButtonEx;
    rn:string;
    hd:THandle;
begin
   tb := TToolButtonEx.Create(toolbar1);
   tb.caption := ACaption;
 {  rn := ADoExecute.GetResourceName;   // como carregar uma imagem da DLL.
   if rn<>'' then
   begin
      hd := ADoExecute.GetHandle;
      if hd>0 then
        tb.Glyph.LoadFromResourceName( hd ,rn);
   end;
  } tb.data := ADoExecute;
   ToolBar1.InsertControl(tb);
end;


end.
