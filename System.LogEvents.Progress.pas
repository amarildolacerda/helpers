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

{
  19/07/2016 + incluido para limpara da lista itens concluidos... para liberar recursos
  + incluido metodo clear... que limpa a lista.
  +
  12/07/2016 * trocado TTask por TTHread para compatibilidade com versões anteriores do XE - Amarildo Lacerda
}

unit System.LogEvents.Progress;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.SyncObjs,
  System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ValEdit, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls, System.Diagnostics;

type

  TLogEventType = (etCreating, etWaiting, etStarting, etPreparing, etPrepared,
    etLoading, etCalc, etWorking, etSaving, etEnding, etFinished, etCanceled,
    etAllFinished);

const
  TLogEventTypeNames: array [0 .. 12] of string = ('0%', 'na fila', 'iniciando',
    'preparando', 'pronto', 'carregando', 'calculando...', 'executando',
    'guardando', 'quase lá', '100%', 'Cancelado', 'Encerrado');

type
  TLogListEvent = procedure(sender: TObject; msg: string) of object;
  TLogProgressEvent = procedure(sender: TObject; ATipo: TLogEventType;
    msg: string; APosition: double) of object;

  IProgressEvents = interface
    ['{291F6DA5-5606-412B-9CD6-76A4E25A9C95}']
    procedure SetCanCancel(const Value: Boolean);
    function GetCanCancel: Boolean;
    procedure SetText(const Value: string);
    function GetText: string;
    procedure SetMaxThreads(const Value: Integer);
    function GetMaxThreads: Integer;
    procedure SetMax(const Value: Integer);
    function GetMax: Integer;
    procedure Execute(const ACaption: String; AIdentProcess: Integer = 0);
    procedure WaitFor(const ASleep: Integer = 100);
    procedure WaitForAll(const ASleep: Integer = 100);
    procedure SetTimeout(const Value: Integer);
    procedure SetMaxFinished(const Value: Integer);
    function GetMaxFinished: Integer;
    function GetTimeout: Integer;
    function Count: Integer;
    procedure Add(sender: TObject; AMsg: string; proc: TProc<TObject>);
      overload;
    procedure Add(AIndex: Integer; AMsg: string; proc: TProc<Integer>);
      overload;
    procedure Add(AValue: string; AMsg: string; proc: TProc<string>); overload;
    procedure Add(AMsg: string; proc: TProc); overload;
    function Terminated: Boolean;
    procedure DoProgress(sender: TObject; identific: Integer;
      ATipo: TLogEventType; msg: string; APosition: double = 0;
      nInteracoes: Integer = 1);
    procedure DoWorking(sender: TObject; AMsg: string);
    procedure DoAllFinished(sender: TObject);
    procedure Restart;
    procedure Close;
    procedure Clear;
    property Text: string read GetText write SetText;
    property CanCancel: Boolean read GetCanCancel write SetCanCancel;
    property Max: Integer read GetMax write SetMax;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property MaxFinished: Integer read GetMaxFinished write SetMaxFinished;
  end;

  TProgressEvents = class(TForm, IProgressEvents)
    Panel1: TPanel;
    Panel2: TPanel;
    ValueListEditor1: TValueListEditor;
    SpeedButton1: TSpeedButton;
    lbPosition: TLabel;
    lblErro: TLabel;
    lbTimer: TLabel;
    cbRolarLista: TCheckBox;
    Bevel1: TBevel;
    procedure FormCreate(sender: TObject);
    procedure FormDestroy(sender: TObject);
    procedure SpeedButton1Click(sender: TObject);
    procedure FormCloseQuery(sender: TObject; var CanClose: Boolean);
    procedure lblErroClick(sender: TObject);
    procedure FormClose(sender: TObject; var Action: TCloseAction);
  private
    FStopwatch: TStopwatch;
    FTerminated, FAutoDeleteFinished: Boolean;
    FInited: Boolean;
    FSLock: TCriticalSection;
    FLocal: Integer;
    FMax: Integer;
    FPosition: Integer;
    FMaxThreads: Integer;
    FCanCancel: Boolean;
    FTimeout: Integer;
    FMaxFinished: Integer;
    { Private declarations }
    procedure SetTerminated(const Value: Boolean);
    procedure DoProgressEvent(sender: TObject; ATipo: TLogEventType;
      msg: string; APosition: double);
    procedure DoErro(sender: TObject; msg: string);
    procedure SetMax(const Value: Integer);
    function GetMax: Integer;
    procedure UpdatePosition;
    procedure incPos(x: Integer);
    procedure CheckThreads;
    procedure SetMaxThreads(const Value: Integer);
    function GetMaxThreads: Integer;
    procedure lock;
    procedure Unlock;
    procedure SetText(const Value: string);
    function GetText: string;
    procedure WaitForAll(const ASleep: Integer = 100);
    procedure SetCanCancel(const Value: Boolean);
    function GetCanCancel: Boolean;
    procedure CheckMax(AInc: Integer);
    procedure Run(proc: TProc);
    procedure SetTimeout(const Value: Integer);
    procedure SetMaxFinished(const Value: Integer);
    function GetMaxFinished: Integer;
    function GetTimeout: Integer;
  public
    { Public declarations }
    procedure Execute(const ACaption: String; AIdentProcess: Integer = 0);
    property Max: Integer read GetMax write SetMax;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property MaxFinished: Integer read GetMaxFinished write SetMaxFinished;
    class function new: IProgressEvents; static;
    procedure DoProgress(sender: TObject; identific: Integer;
      ATipo: TLogEventType; msg: string; APosition: double = 0;
      nInteracoes: Integer = 1);
    procedure DoWorking(sender: TObject; AMsg: string);
    procedure DoAllFinished(sender: TObject);
    procedure Add(sender: TObject; AMsg: string; proc: TProc<TObject>);
      overload;
    procedure Add(AIndex: Integer; AMsg: string; proc: TProc<Integer>);
      overload;
    procedure Add(AValue, AMsg: string; proc: TProc<string>); overload;
    procedure Add(AMsg: string; proc: TProc); overload;
    procedure WaitFor(const ASleep: Integer = 100);
    procedure Restart;
    procedure Clear;
    function Count: Integer;
    function Terminated: Boolean;
    property Text: string read GetText write SetText;
    property CanCancel: Boolean read GetCanCancel write SetCanCancel;
  end;

implementation

{$R *.dfm}

uses {$IF CompilerVersion>28} System.Threading, {$ENDIF} System.LogEvents;

procedure TProgressEvents.Add(AIndex: Integer; AMsg: string;
  proc: TProc<Integer>);
begin
  if not FInited then
    Execute('');
  lock;
  try
    DoProgressEvent(self, etCreating, AMsg, 0);
    InterlockedIncrement(FLocal);
    if FMaxThreads <= 0 then
    begin // nao usa Thread...
      DoProgressEvent(self, etStarting, AMsg, 0);
      proc(AIndex);
      DoProgressEvent(self, etFinished, AMsg, 0);
    end
    else
    begin
      Run(
        procedure
        begin
          DoProgressEvent(self, etStarting, AMsg, 0);
          proc(AIndex);
          DoProgressEvent(self, etFinished, AMsg, 0);
        end);
    end;
  finally
    Unlock;
  end;
  CheckThreads;
end;

procedure TProgressEvents.Add(AValue: String; AMsg: string;
proc: TProc<String>);
begin
  if not FInited then
    Execute('');
  lock;
  try
    DoProgressEvent(self, etCreating, AMsg, 0);
    InterlockedIncrement(FLocal);
    DoProgressEvent(self, etStarting, AMsg, 0);
    if FMaxThreads <= 1 then
    begin
      proc(AValue);
      DoProgressEvent(self, etFinished, AMsg, 0);
    end
    else
      Run(
        procedure
        begin
          proc(AValue);
          DoProgressEvent(self, etFinished, AMsg, 0);
        end);
  finally
    Unlock;
  end;
  CheckThreads;
end;

procedure TProgressEvents.Add(sender: TObject; AMsg: string;
proc: TProc<TObject>);
begin
  if not FInited then
    Execute('');
  lock;
  try
    DoProgressEvent(self, etCreating, AMsg, 0);
    InterlockedIncrement(FLocal);
    DoProgressEvent(self, etStarting, AMsg, 0);
    if FMaxThreads <= 1 then
    begin
      proc(sender);
      DoProgressEvent(self, etFinished, AMsg, 0);
    end
    else
      Run(
        procedure
        begin
          proc(sender);
          DoProgressEvent(self, etFinished, AMsg, 0);
        end);
  finally
    Unlock;
  end;
  CheckThreads;
end;

procedure TProgressEvents.Add(AMsg: string; proc: TProc);
begin
  self.Add(self, AMsg,
    procedure(sender: TObject)
    begin
      proc;
    end);
end;

procedure TProgressEvents.CheckMax(AInc: Integer);
begin
  if FPosition >= FMax then
    FMax := FPosition + AInc;
end;

procedure TProgressEvents.CheckThreads;
begin
  if Count >= FMaxThreads then
    WaitFor(FTimeout);

end;

procedure TProgressEvents.Clear;
begin
  Restart;
end;

function TProgressEvents.Count: Integer;
begin
  lock;
  try
    result := (FLocal - FPosition);
  finally
    Unlock;
  end;
end;

procedure TProgressEvents.DoAllFinished(sender: TObject);
begin
  WaitForAll(FTimeout);
  try
    DoProgress(sender, 0, etAllFinished, '');
    SetTerminated(true);
  except
  end;
end;

procedure TProgressEvents.DoErro(sender: TObject; msg: string);
begin
  lblErro.caption := msg;
end;

procedure TProgressEvents.DoProgress(sender: TObject; identific: Integer;
ATipo: TLogEventType; msg: string; APosition: double; nInteracoes: Integer);
begin
  if Terminated then
    exit;
  LogEvents.DoProgress(sender, identific, ATipo, msg, APosition, nInteracoes);
end;

procedure TProgressEvents.Run(proc: TProc);
begin
{$IF CompilerVersion>28}
  TTask.create(
    procedure
    begin
      proc;
    end).start;
{$ELSE}
  TThread.CreateAnonymousThread(
    procedure
    begin
      proc;
    end).start;
{$ENDIF}
end;

procedure TProgressEvents.DoProgressEvent(sender: TObject; ATipo: TLogEventType;
msg: string; APosition: double);
begin
  if not FInited then
  begin
    SpeedButton1.tag := ord(FCanCancel);
    Execute('');
  end;
  Run(
    procedure
    begin
      if TThread.CurrentThread.CheckTerminated then
      begin
        SetTerminated(true);
        exit;
      end;
      TThread.Queue(nil,
        procedure
        var
          i: Integer;
          sFinished, sCanceled: string;
        begin
          try
            if Terminated then
              exit;
            lock;
            try
              if ATipo = etAllFinished then
              begin
                SpeedButton1.Enabled := true;
                SpeedButton1.tag := 0;
                WaitForAll(FTimeout);
                SpeedButton1.caption := 'OK';
                FMaxFinished := 0;
                UpdatePosition;
              end
              else
              begin
                if msg = '' then
                  exit;
                ValueListEditor1.FindRow(msg, i);
                if i < 0 then
                begin
                  if not(ATipo in [etCreating, etWaiting]) then
                    exit;
                  // if ATipo = etFinished then
                  // exit; // foi retirado da lista... (item velhoo)...

                  CheckMax(1);
                  i := ValueListEditor1.InsertRow(msg, '...', true);
                  if cbRolarLista.checked then
                    ValueListEditor1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
                  UpdatePosition;
                end;
                sFinished := TLogEventTypeNames[ord(etFinished)];
                sCanceled := TLogEventTypeNames[ord(etCanceled)];
                case ATipo of
                  etCanceled:
                    if (ValueListEditor1.Cells[1, i] <> sCanceled) and
                      (ValueListEditor1.Cells[1, i] <> sFinished) then
                    begin
                      ValueListEditor1.Cells[1, i] := sCanceled;
                      incPos(1);
                    end;
                  etFinished:
                    if ValueListEditor1.Cells[1, i] <> sFinished then
                    begin
                      ValueListEditor1.Cells[1, i] := sFinished;
                      incPos(1);
                    end;
                else
                  if ValueListEditor1.Cells[1, i] <> sFinished then
                    ValueListEditor1.Cells[1, i] := TLogEventTypeNames
                      [ord(ATipo)];
                end;
              end;
            finally
              Unlock;
            end;
          except
          end;
        end);
    end);

end;

procedure TProgressEvents.DoWorking(sender: TObject; AMsg: string);
begin
  DoProgress(sender, 0, etWorking, AMsg);
end;

procedure TProgressEvents.Execute(const ACaption: String;
AIdentProcess: Integer = 0);
begin

  if AIdentProcess > 0 then
  begin
    LogEvents.unregister(self);
    LogEvents.register(self, DoProgressEvent, AIdentProcess);
    LogEvents.register(self, DoErro, AIdentProcess);
  end;

  Clear;
  FStopwatch := TStopwatch.StartNew;

  if ACaption <> '' then
    caption := ACaption;

  if FCanCancel then
  begin
    SpeedButton1.caption := 'Cancelar';
    SpeedButton1.Enabled := true;
  end;

  Show;
  FInited := true;

end;

procedure TProgressEvents.FormClose(sender: TObject; var Action: TCloseAction);
begin

  SetTerminated(true);
  WaitFor(FTimeout);
  Action := TCloseAction.caFree;
end;

procedure TProgressEvents.FormCloseQuery(sender: TObject;
var CanClose: Boolean);
begin
  SetTerminated(true);
  CanClose := true;
end;

procedure TProgressEvents.FormCreate(sender: TObject);
begin
  FMaxFinished := 5;
  FTimeout := 100;
  FStopwatch := TStopwatch.StartNew;
  FAutoDeleteFinished := true;
  FMaxThreads := TThread.ProcessorCount * 2;
  FTerminated := false;
  ValueListEditor1.Strings.Clear;
  FInited := false;
  FSLock := TCriticalSection.create;
  FMax := 0;
  LogEvents.MostraDataHora := false;
  LogEvents.register(self, DoProgressEvent, 0);
  LogEvents.register(self, DoErro, 0);
  SpeedButton1.Enabled := false;
end;

procedure TProgressEvents.FormDestroy(sender: TObject);
begin
  LogEvents.unregister(self);
  FSLock.free;
end;

function TProgressEvents.GetCanCancel: Boolean;
begin
  result := FCanCancel;
end;

function TProgressEvents.GetMax: Integer;
begin
  result := FMax;
end;

function TProgressEvents.GetMaxFinished: Integer;
begin
  result := FMaxFinished;
end;

function TProgressEvents.GetMaxThreads: Integer;
begin
  result := FMaxThreads;
end;

function TProgressEvents.GetText: string;
begin
  lock;
  try
    result := Panel1.caption;
  finally
    Unlock;
  end;
end;

function TProgressEvents.GetTimeout: Integer;
begin
  result := FTimeout;
end;

procedure TProgressEvents.incPos(x: Integer);
begin
  FPosition := FPosition + x;
  UpdatePosition;
  if FAutoDeleteFinished = false then
    exit;
  Run(
    procedure
    begin
      TThread.Queue(nil,
        procedure
        var
          i, n: Integer;
        begin
          try
            if Terminated then
              exit;
            lock;
            try
              // apaga os concluidos mais velhos para liberar recursos
              n := 0;
              for i := ValueListEditor1.RowCount - 1 downto 0 do
              begin
                if Terminated then
                  exit;
                try
                  if (TLogEventTypeNames[ord(etFinished)
                    ] = ValueListEditor1.Values[ValueListEditor1.Keys[i]]) or
                    (TLogEventTypeNames[ord(etCanceled)
                    ] = ValueListEditor1.Values[ValueListEditor1.Keys[i]]) then
                  begin
                    inc(n);
                    if n > FMaxFinished then
                    begin
                      ValueListEditor1.DeleteRow(i);
                    end;
                  end;
                except
                end;
              end;
            finally
              Unlock;
            end;
          except
          end;
        end);
    end);
end;

procedure TProgressEvents.lblErroClick(sender: TObject);
begin
  showMessage(lblErro.caption);
end;

procedure TProgressEvents.lock;
begin
  FSLock.Acquire;
end;

class function TProgressEvents.new: IProgressEvents;
begin
  result := TProgressEvents.create(nil);
  result.Clear;
end;

procedure TProgressEvents.Restart;
var
  i: Integer;
begin
  lock;
  try
    FStopwatch := TStopwatch.StartNew;
    ValueListEditor1.Strings.Clear;
    FMax := 0;
    FPosition := 0;
    FLocal := 0;
    UpdatePosition;
  finally
    Unlock;
  end;
end;

procedure TProgressEvents.SetCanCancel(const Value: Boolean);
begin
  FCanCancel := Value;
end;

procedure TProgressEvents.SetMax(const Value: Integer);
begin
  TThread.Queue(nil,
    procedure
    begin
      lock;
      try
        FMax := Value;
      finally
        Unlock;
      end;
      UpdatePosition;
    end);
end;

procedure TProgressEvents.SetMaxFinished(const Value: Integer);
begin
  FMaxFinished := Value;
end;

procedure TProgressEvents.SetMaxThreads(const Value: Integer);
begin
  FMaxThreads := Value;
end;

procedure TProgressEvents.SetTerminated(const Value: Boolean);
begin
  lock;
  try
    FTerminated := true;
  finally
    Unlock;
  end;
end;

procedure TProgressEvents.SetText(const Value: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      lock;
      try
        Panel1.caption := Value;
      finally
        Unlock;
      end;
    end);
end;

procedure TProgressEvents.SetTimeout(const Value: Integer);
begin
  FTimeout := Value;
end;

procedure TProgressEvents.SpeedButton1Click(sender: TObject);
begin
  SetTerminated(true);
  WaitForAll(FTimeout);
  Close;
end;

function TProgressEvents.Terminated: Boolean;
begin
  lock;
  try
    result := FTerminated;
    if result then; // como parar a TASK
  finally
    Unlock;
  end;
end;

procedure TProgressEvents.Unlock;
begin
  FSLock.Release;
end;

procedure TProgressEvents.UpdatePosition;

begin
  Run(
    procedure
    begin
      TThread.Queue(nil,
        procedure
        var
          t: Integer;
          n: Integer;
          x: double;
        begin
          try
            if Terminated then
              exit;
            lock;
            try
              lbPosition.caption := '';
              n := ValueListEditor1.Strings.Count;
              t := FMax;
              if (t = 0) or (t < n) then
              begin
                t := n;
                FMax := n;
              end;
              lbPosition.caption := intToStr(FPosition) + ' de ' + intToStr(t);
              x := FStopwatch.Elapsed.TotalDays;
              lbTimer.caption := TimeToStr(x);
            finally
              Unlock;
            end;
          except
          end;
        end);
    end);
end;

procedure TProgressEvents.WaitFor(const ASleep: Integer);
begin
  try
    if FMaxThreads > 0 then
      while Count >= FMaxThreads do
      begin
        TThread.sleep(ASleep);
        Application.ProcessMessages;
        if Terminated then
          break;
      end;
  except
  end;
end;

procedure TProgressEvents.WaitForAll(const ASleep: Integer);
var
  tick: int64;
begin
  try
    tick := TThread.GetTickCount;
    while Count > 0 do
    begin
      Application.ProcessMessages;
      if Terminated then
        break;
      if (TThread.GetTickCount - tick) > ASleep then
        break;
    end;
  except
  end;
end;

end.
