{ *************************************************************************** }
{ }
{ }
{ Copyright (C) Amarildo Lacerda }
{ }
{ https://github.com/amarildolacerda }
{ }
{ }
{ *************************************************************************** }
{ }
{ Licensed under the Apache License, Version 2.0 (the "License"); }
{ you may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at }
{ }
{ http://www.apache.org/licenses/LICENSE-2.0 }
{ }
{ Unless required by applicable law or agreed to in writing, software }
{ distributed under the License is distributed on an "AS IS" BASIS, }
{ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{ See the License for the specific language governing permissions and }
{ limitations under the License. }
{ }
{ *************************************************************************** }

unit plugin.FormManager;

interface

{$I plugin.inc}

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ValEdit, Vcl.ExtCtrls,
  Vcl.StdCtrls, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.DBGrids;

type
  TPluginFormManagerDlg = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    FDMemTable1: TFDMemTable;
    FDMemTable1Item: TStringField;
    FDMemTable1Plugin: TStringField;
    Button2: TButton;
    Button3: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FFilename: string;
    procedure SetFilename(const Value: string);
    procedure CarregarLista;
    procedure SalvarLista;
    procedure Reordena;
    { Private declarations }
  public
    { Public declarations }
    property Filename: string read FFilename write SetFilename;
  end;

function PluginFormManagerDlg: TPluginFormManagerDlg;

implementation

{$R *.dfm}

uses IniFiles, {$IFDEF USE_INIFILEEx} IniFilesEx, {$ENDIF}
  plugin.Manager;

var
  FFormDlg: TPluginFormManagerDlg;

function PluginFormManagerDlg: TPluginFormManagerDlg;
begin
  if not assigned(FFormDlg) then
    FFormDlg := TPluginFormManagerDlg.Create(nil);
  result := FFormDlg;
end;

procedure TPluginFormManagerDlg.Button1Click(Sender: TObject);
var
  appDir: string;
  dlg: TOpenDialog;
begin
  appDir := GetCurrentDir;
  dlg := TOpenDialog.Create(self);
  try
    dlg.InitialDir := appDir;
    dlg.Filter := ' DLL de Plugin (*.dll)|*.dll';
    if dlg.execute then
    begin
      //if GetPluginManager.InstallPlugin(dlg.Filename) >= 0 then
      begin
        CarregarLista;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

procedure TPluginFormManagerDlg.FormCreate(Sender: TObject);
begin
  FFilename := 'Plugin.ini';
end;

procedure TPluginFormManagerDlg.FormShow(Sender: TObject);
begin
  CarregarLista;
end;

procedure TPluginFormManagerDlg.Reordena;
var
  i: integer;
begin
  FDMemTable1.first;
  i := 0;
  while FDMemTable1.Eof = false do
  begin
    FDMemTable1.edit;
    FDMemTable1Item.Value := 'Plugin' + IntToStr(i);
    FDMemTable1.Post;
    FDMemTable1.next;
    inc(i);
  end;
end;

procedure TPluginFormManagerDlg.SalvarLista;
var
  app, key: string;
  n: integer;
begin
  app := ExtractFileName(ParamStr(0));
  with TIniFile.Create(FFilename) do
    try
      EraseSection(app);
      n := 0;
      FDMemTable1.first;
      while FDMemTable1.Eof = false do
      begin
        key := 'Plugin' + IntToStr(n);
        WriteString(app, key, FDMemTable1Plugin.Value);
        inc(n);
        FDMemTable1.next;
      end;
    finally
      Free;
    end;
  CarregarLista;
end;

procedure TPluginFormManagerDlg.SetFilename(const Value: string);
begin
  FFilename := Value;
end;

procedure TPluginFormManagerDlg.Button2Click(Sender: TObject);
begin
  SalvarLista;
  close;
end;

procedure TPluginFormManagerDlg.Button3Click(Sender: TObject);
begin
  FDMemTable1.Delete;
  Reordena;
end;

procedure TPluginFormManagerDlg.CarregarLista;
var
  str: TStringList;
  app: String;
  key: string;
  n, i: integer;
  s: string;
begin
  FDMemTable1.EmptyDataSet;
  str := TStringList.Create;
  try
    app := ExtractFileName(ParamStr(0));
    with TIniFile.Create(FFilename) do
      try
        ReadSection(app, str);
        n := 0;
        for i := 0 to str.count - 1 do
        begin
          key := 'Plugin' + IntToStr(i);
          s := ReadString(app, key, 'fim');
          if s <> 'fim' then
          begin
            FDMemTable1.append;
            FDMemTable1Item.Value := key;
            FDMemTable1Plugin.Value := s;
            FDMemTable1.Post;
          end;
        end;
      finally
        Free;
      end;
    Reordena;
  finally
    str.Free;
  end;
end;

initialization

finalization

if assigned(FFormDlg) then
  FreeAndNil(FFormDlg);

end.
