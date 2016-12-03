unit uConsomeAcessoFiredac;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm24 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure dummy;
  end;

var
  Form24: TForm24;

implementation

{$R *.dfm}
// modelo estatico  - ou seja, depende que a DLL esteja no disco
// function SomarNumero( const numero:integer; adicionar:integer):integer ; external 'AcessoFiredac.dll' ;

type
  TSomarNumeroFunc = function(const numero: integer;
    adicionar: integer): integer;

procedure TForm24.Button1Click(Sender: TObject);
begin
  // Panel1.Caption :=   SomarNumero( strToIntDef(edit1.Text,0)   , strToIntDef(edit2.Text,0)    ).ToString ;

  dummy;
end;



// consumindo a DLL com carga dinamica
procedure TForm24.dummy;
var
  h: THandle;
  func: TSomarNumeroFunc;
begin
  // SomarNumero( strToIntDef(edit1.Text,0)   , strToIntDef(edit2.Text,0)    ).ToString ;
  h := LoadLibrary('AcessoFiredac.dll');
  try
    if h > 0 then
    begin
      @func := GetProcAddress(h, 'SomarNumero');
      if assigned(func) then
      begin
        Panel1.caption := func(strToIntDef(Edit1.text, 0),
          strToIntDef(Edit2.text, 0)).ToString;
      end;
    end;
  finally
    FreeLibrary(h);
  end;
end;

end.
