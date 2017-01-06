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

unit System.LogEvents;

interface

Uses {$IFDEF FMX} FMX.Forms, {$ELSE} VCL.Forms, {$ENDIF} System.Classes,
  System.Generics.Collections, System.SysUtils,
  System.SyncObjs, System.LogEvents.Progress;

type

  TLogListItem = class
  private
    FonErro: TLogListEvent;
    Finstancia: TObject;
    FIdent: Integer;
    FOnProgress: TLogProgressEvent;
    procedure SetonErro(const Value: TLogListEvent);
    procedure Setinstancia(const Value: TObject);
    procedure SetIdent(const Value: Integer);
    procedure SetOnProgress(const Value: TLogProgressEvent);
  published
  public
    property Ident: Integer read FIdent write SetIdent;
    property instancia: TObject read Finstancia write Setinstancia;
    property OnErro: TLogListEvent read FonErro write SetonErro;
    property OnProgress: TLogProgressEvent read FOnProgress write SetOnProgress;
  end;

  TLogListItems = class(TObjectList<TLogListItem>)
  private
    FLock: TCriticalSection;
    FBuffer: AnsiString;
    FMostraDataHora: boolean;
    FEnabled: boolean;
    FSyncronized: boolean;
    procedure SetMostraDataHora(const Value: boolean);
    procedure SetEnabled(const Value: boolean);
    procedure DoProcessProc(sender: TObject; identific: Integer;
      ATipo: TLogEventType; msg: string; APosition: double = 0;
      nInteracoes: Integer = 1);
    procedure SetSyncronized(const Value: boolean);
  public
    property Enabled: boolean read FEnabled write SetEnabled;
    property Syncronized: boolean read FSyncronized write SetSyncronized;
    property MostraDataHora: boolean read FMostraDataHora
      write SetMostraDataHora;

    procedure register(sender: TObject; event: TLogListEvent;
      identific: Integer); overload;
    procedure register(sender: TObject; event: TLogProgressEvent;
      identific: Integer); overload;
    procedure unregister(sender: TObject);
    procedure DoErro(sender: TObject; identific: Integer; s: string;
      nInteracoes: Integer = 1);
    procedure DoMsg(sender: TObject; identific: Integer; s: string;
      nInteracoes: Integer = 1);
    procedure DoProgress(sender: TObject; identific: Integer;
      ATipo: TLogEventType; msg: string; APosition: double = 0;
      nInteracoes: Integer = 1);
    procedure SetMax(FValue: Integer);
    procedure Log(texto: string);
    constructor create();
    destructor Destroy; override;
    procedure Run(proc: TProc);
  end;

var
  LogEvents: TLogListItems;

implementation

{$IF CompilerVersion>28.0}

uses System.Threading;
{$ENDIF}

procedure TLogListItem.SetIdent(const Value: Integer);
begin
  FIdent := Value;
end;

procedure TLogListItem.Setinstancia(const Value: TObject);
begin
  Finstancia := Value;
end;

procedure TLogListItem.SetonErro(const Value: TLogListEvent);
begin
  FonErro := Value;
end;

procedure TLogListItem.SetOnProgress(const Value: TLogProgressEvent);
begin
  FOnProgress := Value;
end;

{ TLogListItems }

constructor TLogListItems.create();
begin
  inherited create;
  FSyncronized := true;
  FMostraDataHora := true;
  FLock := TCriticalSection.create;
  Enabled := true;
end;

destructor TLogListItems.Destroy;
begin
  freeAndNil(FLock);
  inherited;
end;

procedure TLogListItems.DoErro(sender: TObject; identific: Integer; s: string;
  nInteracoes: Integer = 1);
begin // compatibilidade
  DoMsg(sender, identific, s, nInteracoes);
end;

procedure TLogListItems.DoMsg(sender: TObject; identific: Integer; s: string;
  nInteracoes: Integer);
var
  it: TLogListItem;
  i, x: Integer;
  h, buf: AnsiString;
begin

  Run(
    procedure
    var
      i: Integer;
    begin

      h := '';
      try
        if not Enabled then
          exit;
        buf := FBuffer;
        FBuffer := '';
        x := 0;
        if FMostraDataHora then
          h := FormatDateTime('DD/MM/YY hh:mm:ss', now) + ' ';
        FBuffer := '';
        for i := 0 to Count - 1 do
        begin
          it := TLogListItem(items[i]);
          if (identific = 0) or (it.Ident = identific) then
          begin
            if assigned(it.FonErro) then
            begin
              it.FonErro(sender, h + s + buf);
              inc(x);
            end;
          end;

          if nInteracoes = 0 then
            continue;
          if x >= nInteracoes then
            break;

        end;
      finally
      end;

    end);

end;

procedure TLogListItems.DoProcessProc(sender: TObject; identific: Integer;
ATipo: TLogEventType; msg: string; APosition: double = 0;
nInteracoes: Integer = 1);
var
  i, x: Integer;
  it: TLogListItem;
begin
  try
    if not Enabled then
      exit;
    FBuffer := '';
    x := 0;
    for i := 0 to Count - 1 do
    begin
      it := TLogListItem(items[i]);
      if (identific = 0) or (it.Ident = identific) then
      begin
        if assigned(it.OnProgress) then
        begin
          it.FOnProgress(sender, ATipo, msg, APosition);
          inc(x);
        end;
      end;

      if nInteracoes = 0 then
        continue;
      if x >= nInteracoes then
        break;

    end;
  finally
  end;

end;

procedure TLogListItems.DoProgress(sender: TObject; identific: Integer;
ATipo: TLogEventType; msg: string; APosition: double = 0;
nInteracoes: Integer = 1);
begin

  Run(
    procedure
    begin
      DoProcessProc(sender, identific, ATipo, msg, APosition, nInteracoes);
    end);

end;

procedure TLogListItems.Log(texto: string);
begin
  Run(
    procedure
    begin
      DoErro(self, 0, texto, 1);
    end);
end;

procedure TLogListItems.register(sender: TObject; event: TLogListEvent;
identific: Integer);
var
  it: TLogListItem;
begin
  it := TLogListItem.create;
  Add(it);
  it.instancia := sender;
  it.OnErro := event;
  it.Ident := identific;
end;

procedure TLogListItems.register(sender: TObject; event: TLogProgressEvent;
identific: Integer);
var
  it: TLogListItem;
begin
  it := TLogListItem.create;
  Add(it);
  it.instancia := sender;
  it.OnProgress := event;
  it.Ident := identific;
end;

procedure TLogListItems.Run(proc: TProc);
begin
  if not FSyncronized then
  begin
    proc;
    exit;
  end
  else

{$IF CompilerVersion>28.0}
    TTask.create(
      procedure
      begin
        try
          TThread.Queue(nil,
            procedure
            begin
              proc;
            end);
        except
          on e: exception do
            LogEvents.DoErro(nil, 0, e.message);
        end;
      end).start;

{$ELSE}
    TThread.CreateAnonymousThread(
      procedure
      begin
        try
          TThread.Queue(nil,
            procedure
            begin
              proc;
            end);
        except
          on e: exception do
            LogEvents.DoErro(nil, 0, e.message);
        end;
      end).start;
{$ENDIF}
end;

procedure TLogListItems.SetEnabled(const Value: boolean);
begin
  FEnabled := Value;
end;

procedure TLogListItems.SetMax(FValue: Integer);
begin

end;

procedure TLogListItems.SetMostraDataHora(const Value: boolean);
begin
  FMostraDataHora := Value;
end;

procedure TLogListItems.SetSyncronized(const Value: boolean);
begin
  FSyncronized := Value;
end;

procedure TLogListItems.unregister(sender: TObject);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    if TLogListItem(items[i]).instancia = sender then
      delete(i);
end;

initialization

LogEvents := TLogListItems.create;

finalization

LogEvents.free;

end.
