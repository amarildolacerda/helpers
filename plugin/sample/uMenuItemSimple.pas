unit uMenuItemSimple;

interface

uses
  VCL.Forms, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Controls, System.Classes, dxGDIPlusClasses;

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

uses
       Data.DB, FireDAC.Stan.Param, FireDAC.Stan.Intf, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Stan.Option,
  FireDAC.DApt.Intf, System.Generics.Collections,
  FireDAC.Comp.ScriptCommands,
  FireDAC.Comp.DataMove,
  FireDAC.Comp.Script,
  IniFiles,IniFilesEx,
    FireDAC.Phys.SQLiteVDataSet,
  System.uDebug, // DONE -oAL : refatorar, para não ter dependencia da uDebug
  FireDAC.Stan.Util,
  FireDAC.Stan.ExprFuncs,
  Registry,
  FireDAC.Phys.ODBCBase, FireDAC.Phys.ODBC,
  FireDAC.Phys.Oracle, FireDAC.Phys.MySQL,
  FireDAC.UI.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.DApt, FireDAC.Phys.IBBase,
  FireDAC.Phys.FB, FireDAC.Phys.MSSQL,
  FireDAC.Moni.RemoteClient,
  FireDAC.Moni.Custom,
  System.IOUtils,
  FireDAC.Phys.SQLite,
  data.fireTables,
     pngimage, plugin.Service, plugin.MenuItem, plugin.Control;

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
