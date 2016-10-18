unit System.ConfigList;

interface

uses SysUtils, Classes,System.Generics.collections,
     {$ifdef FMX} FMX.Edit {$endif};


const
   sConfigList_Configurou = 'Configurou';

type


  TConfigListItem = class
    public
       Nome:String;
       Valor:Variant;
       ValorDefault:Variant;
       Control : TComponent;
  end;

  TConfigListItemClassOf = class of TConfigListItem;

  TConfigList = Class(TObjectList<TConfigListItem>)
  private
    FFileName: string;
    FSessao: String;
    FItemClass: TConfigListItemClassOf;
    procedure SetFileName(const Value: string);
    function GetItem(ANome: String): string;
    procedure SetItem(ANome: String; const Value: string);
    procedure SetSessao(const Value: String);

  public
     function ItemClass:TConfigListItemClassOf;virtual;
     function add:TConfigListItem;
     constructor Create(AItemClass:TConfigListItemClassOf; AFileName:string='');
     procedure Gravar;
     procedure Carregar;
     property FileName:string read FFileName write SetFileName;
     property Sessao:String read FSessao write SetSessao;
     Property Item[ ANome:String ]:string read GetItem write SetItem;

  end;

implementation

{ TConfigList<T> }
uses IniFiles, IniFilesEx;

function TConfigList.add: TConfigListItem;
begin
    result := ItemClass.Create;
    inherited add(result);
end;

procedure TConfigList.Carregar;
var it:TConfigListItem;
    s:string;
begin
    with TIniFile.create(FFileName) do
    try
      for it in self do
      begin
        if it.control.InheritsFrom(TEdit) then
           with TEdit(it.control) do
           begin
                s := text;
                text := ReadString('Config',it.nome,s);
           end;
      end;
    finally
      free;
    end;

end;


constructor TConfigList.create(AItemClass:TConfigListItemClassOf; AFileName:string='');
begin
    inherited create;
    FItemClass := TConfigListItem;
    if AItemClass<>nil then
       FItemClass := AItemClass;
    FSessao := 'Config';
    FFileName := AFileName;

end;

function TConfigList.GetItem(ANome: String): string;
var it:TConfigListItem;
begin
   for it in self do
     if sametext(it.nome,ANome) then
     begin
        if it.control.InheritsFrom(TEdit) then
           with TEdit(it.control) do
                result := text;
        exit;
     end;
end;

procedure TConfigList.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TConfigList.SetItem(ANome: String; const Value: string);
var it:TConfigListItem;
begin
   for it in self do
     if sametext(it.nome,ANome) then
     begin
        if it.control.InheritsFrom(TEdit) then
           with TEdit(it.control) do
                text := Value;
        exit;
     end;
end;

procedure TConfigList.SetSessao(const Value: String);
begin
  FSessao := Value;
end;

procedure TConfigList.Gravar;
var it:TConfigListItem;
begin
    item[sConfigList_Configurou] :=DatetimeToStr(now);
    with TIniFile.create(FFilename) do
    try
      for it in self do
      begin
        if it.control.InheritsFrom(TEdit) then
           with TEdit(it.control) do
                writeString('Config',it.nome,text);
      end;
    finally
      free;
    end;
end;


function TConfigList.ItemClass: TConfigListItemClassOf;
begin
    result := FItemClass;
end;

end.
