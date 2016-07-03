{***************************************************************************}
{                                                                           }
{                                                                           }
{           Copyright (C) Amarildo Lacerda                                  }
{                                                                           }
{           https://github.com/amarildolacerda                              }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}


unit System.LogEvents.Progress;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,System.SyncObjs,
  System.Generics.Collections,
  System.LogEvents,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.ValEdit, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls;

type


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
    property Max: Integer read GetMax write SetMax;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    procedure execute(const ACaption: String);
    procedure WaitFor(const ASleep: Integer = 100);
    procedure WaitForAll(const ASleep: Integer = 100);
    function Count: Integer;
    procedure Add(sender: TObject; AMsg: string; proc: TProc<TObject>);
      overload;
    procedure Add(AIndex: Integer; AMsg: string; proc: TProc<Integer>);
      overload;
    procedure Add(AValue: string; AMsg: string; proc: TProc<string>); overload;
    property Text: string read GetText write SetText;
    property CanCancel:Boolean read GetCanCancel write SetCanCancel;
    function Terminated: boolean;
  end;


  TProgressEvents = class(TForm, IProgressEvents)
    Panel1: TPanel;
    Panel2: TPanel;
    ValueListEditor1: TValueListEditor;
    SpeedButton1: TSpeedButton;
    lbPosition: TLabel;
    lblErro: TLabel;
    procedure FormCreate(sender: TObject);
    procedure FormDestroy(sender: TObject);
    procedure SpeedButton1Click(sender: TObject);
    procedure FormCloseQuery(sender: TObject; var CanClose: boolean);
    procedure lblErroClick(Sender: TObject);
  private
    FTerminated: boolean;
    FInited: boolean;
    FSLock: TCriticalSection;
    FLocal: Integer;
    FMax: Integer;
    FPosision: Integer;
    FMaxThreads: Integer;
    FCanCancel: Boolean;
    { Private declarations }
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
  public
    { Public declarations }
    procedure execute(const ACaption: String);
    property Max: Integer read GetMax write SetMax;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    class function new: IProgressEvents; static;
    procedure Add(sender: TObject; AMsg: string; proc: TProc<TObject>);
      overload;
    procedure Add(AIndex: Integer; AMsg: string; proc: TProc<Integer>);
      overload;
    procedure Add(AValue, AMsg: string; proc: TProc<string>); overload;
    procedure WaitFor(const ASleep: Integer = 100);
    function Count: Integer;
    function Terminated: boolean;
    property Text: string read GetText write SetText;
    property CanCancel:Boolean read GetCanCancel write SetCanCancel;
  end;

implementation

{$R *.dfm}

uses System.Threading;

procedure TProgressEvents.Add(AIndex: Integer; AMsg: string;
  proc: TProc<Integer>);
begin
  if not FInited then
    execute('');
  lock;
  try
    DoProgressEvent(self, etCreating, AMsg, 0);
    InterlockedIncrement(FLocal);
    if FMaxThreads <= 0 then
    begin    // nao usa Thread...
      DoProgressEvent(self, etStarting, AMsg, 0);
      proc(AIndex);
      DoProgressEvent(self, etFinished, AMsg, 0);
    end
    else
    begin
      DoProgressEvent(self, etWaiting, AMsg, 0);
      TTask.create(
        procedure
        begin
          DoProgressEvent(self, etStarting, AMsg, 0);
          proc(AIndex);
          DoProgressEvent(self, etFinished, AMsg, 0);
        end).start;
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
    execute('');
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
      TTask.create(
        procedure
        begin
          proc(AValue);
          DoProgressEvent(self, etFinished, AMsg, 0);
        end).start;
  finally
    Unlock;
  end;
  CheckThreads;
end;

procedure TProgressEvents.Add(sender: TObject; AMsg: string;
proc: TProc<TObject>);
begin
  if not FInited then
    execute('');
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
      TTask.create(
        procedure
        begin
          proc(sender);
          DoProgressEvent(self, etFinished, AMsg, 0);
        end).start;
  finally
    Unlock;
  end;
  CheckThreads;
end;

procedure TProgressEvents.CheckThreads;
begin
  if Count > FMaxThreads then
    WaitFor(100);

end;

function TProgressEvents.Count: Integer;
begin
  lock;
  try
    result := (FLocal - FPosision);
  finally
    Unlock;
  end;
end;

procedure TProgressEvents.DoErro(sender: TObject; msg: string);
begin
  lblErro.caption := msg;
end;

procedure TProgressEvents.DoProgressEvent(sender: TObject; ATipo: TLogEventType;
msg: string; APosition: double);
var
  i: Integer;
begin
  if not FInited then
  begin
    SpeedButton1.tag := ord(FCanCancel);
    execute('');
  end;
  TThread.Queue(nil,
    procedure
    begin
      if ATipo = etAllFinished then
      begin
        SpeedButton1.Enabled := true;
        SpeedButton1.tag := 0;
        WaitForAll(100);
        SpeedButton1.caption := 'OK';
      end
      else
      begin
        i := ValueListEditor1.Strings.IndexOfName(msg);
        if i >= 0 then
        begin
          case ATipo of
            etCreating:
              ValueListEditor1.Strings.Values[msg] := 'Criando..';
            etWaiting:
              ValueListEditor1.Strings.Values[msg] := 'na fila';
            etStarting:
              ValueListEditor1.Strings.Values[msg] := 'Iniciado..';
            etWorking:
              begin
                ValueListEditor1.Strings.Values[msg] := 'Em andamento';
              end;
            etFinished:
              begin
                ValueListEditor1.Strings.Values[msg] := 'Concluído ';
                incPos(1);
              end
          end;
        end
        else
        begin
          ValueListEditor1.Strings.AddPair(msg, '...');
          ValueListEditor1.Perform(WM_VSCROLL, SB_BOTTOM, 0);
          UpdatePosition;
        end;
      end;
    end);
end;

procedure TProgressEvents.execute(const ACaption: String);
begin
  if ACaption <> '' then
    caption := ACaption;

  if FCanCancel then
  begin
     SpeedButton1.Caption := 'Cancelar';
     SpeedButton1.Enabled := true;
  end;

  show;
  FInited := true;
end;

procedure TProgressEvents.FormCloseQuery(sender: TObject;
var CanClose: boolean);
begin
  FTerminated := true;
end;

procedure TProgressEvents.FormCreate(sender: TObject);
begin
  FMaxThreads := 5;
  FTerminated := false;
  ValueListEditor1.Strings.clear;
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
   result := FCanCancel ;
end;

function TProgressEvents.GetMax: Integer;
begin
  result := FMax;
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

procedure TProgressEvents.incPos(x: Integer);
begin
  FPosision := FPosision + x;
  UpdatePosition;
end;

procedure TProgressEvents.lblErroClick(Sender: TObject);
begin
    showMessage(lblErro.Caption);
end;

procedure TProgressEvents.lock;
begin
  FSLock.Acquire;
end;

class function TProgressEvents.new: IProgressEvents;
begin
  result := TProgressEvents.create(nil);
end;

procedure TProgressEvents.SetCanCancel(const Value: Boolean);
begin
  FCanCancel := Value;
end;

procedure TProgressEvents.SetMax(const Value: Integer);
begin
  FMax := Value;
  UpdatePosition;
end;

procedure TProgressEvents.SetMaxThreads(const Value: Integer);
begin
  FMaxThreads := Value;
end;

procedure TProgressEvents.SetText(const Value: string);
begin
  TThread.Synchronize(nil,
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

procedure TProgressEvents.SpeedButton1Click(sender: TObject);
begin
  FTerminated := true;
  close;
end;

function TProgressEvents.Terminated: boolean;
begin
  lock;
  try
    result := FTerminated;
    if result then
       ; // como parar a TASK
  finally
    Unlock;
  end;
end;

procedure TProgressEvents.Unlock;
begin
  FSLock.Release;
end;

procedure TProgressEvents.UpdatePosition;
var
  t: Integer;
begin
  TThread.Queue(nil,
    procedure
    begin
      lbPosition.caption := '';
      t := FMax;
      if t = 0 then
        t := ValueListEditor1.Strings.Count;
      lbPosition.caption := intToStr(FPosision) + ' de ' + intToStr(t);
    end);
end;

procedure TProgressEvents.WaitFor(const ASleep: Integer);
begin
  while Count > FMaxThreads do
  begin
    TThread.Sleep(ASleep);
    Application.ProcessMessages;
    if Terminated then
      break;
  end;
end;

procedure TProgressEvents.WaitForAll(const ASleep: Integer);
begin
  while Count > 0 do
  begin
    TThread.Sleep(ASleep);
    Application.ProcessMessages;
    if Terminated then
      break;
  end;
end;


end.
