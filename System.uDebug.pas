unit System.uDebug;

{
  manter aqui somente funções e debug..
  objetivo
}

interface

{$I delphi.inc}

uses
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
{$IFDEF FMX}
  FMX.Dialogs,
  FMX.Forms,
{$ELSE}
  VCL.Dialogs,
  VCL.Forms,
{$ENDIF}
  System.IOUtils,
  System.Classes,
  System.Sysutils,
  System.Rtti;

type
  TDebugOptions = record
    db_params_in: boolean;
    db_params_out: boolean;
    db_params_prepare: boolean;
    db_params_unprepare: boolean;
    db_execute: boolean;
    db_dbparams: boolean;
    db_transaction: boolean;
    messages: boolean;
  end;

  TActionOnApplicationError = (actErrContinue, actErrHalt, actErrTerminate);

procedure DebugLog(ATexto: string; AComando: string = ''; AArquivo: string = '';
  nomeRotina: string = ''; AForcar: boolean = false);
//procedure WriteLogError(const ATexto: string);  // nao usar.. substituir por ErrorLog(
procedure ErrorLog(ATexto: string);
procedure WriteLog(AArquivo: string; ATexto: string; AComando: string = '');
procedure WriteLogText(AArquivo: string; ATexto: string;
  revisaArquivoDestino: boolean = True);
procedure DoAppExceptionEvent(Sender: TObject; E: Exception;
  AShow: boolean = True);

function DebugOn: boolean;
procedure SetDebugOn(ligar: boolean);
procedure SuspendDebugOn(susp: boolean);

procedure SetDebugFileName(nome: string);
function GetDebugFileName(): string;
function LogsFilesDir(s: string = ''): string;
procedure eCheckTrue(b: boolean; texto: string);

var
  DebugOptions: TDebugOptions;
  ActionOnApplicationError: TActionOnApplicationError;

implementation

// Calixto - nao usar o Forms para dll aumenta muito o tamanho
uses {$IFDEF MSWINDOWS} DllFunctions, {$ENDIF}
  System.iniFiles, System.IniFilesEx, SyncObjs, System.Threading;

var
  FNoLog: boolean;
  FIsDebugActive: boolean = false;
  LErrorLogPath: String;
  LogCount: integer = 1;
  FDebugFileName: string = '';
  FLockEscrita, FLockEntrada: TCriticalSection;
  LEncerrou: boolean;
  FSuspended: boolean = false;

procedure eCheckTrue(b: boolean; texto: string);
begin
  if not b then
    raise Exception.Create(texto);
end;

function AppDir: String;
begin
  result := ExtractFilePath(ParamStr(0));
end;

function GetTempDir: string;
var
  s: array [0 .. 255] of char;
begin
{$IFDEF MSWINDOWS}
  GetTempPath(255, s);
  result := s;
{$ELSE}
  result := TPath.GetDocumentsPath;
{$ENDIF}
end;

function CurDir: string;
begin
  result := GetCurrentDir;
end;

function LogsFilesDir(s: string = ''): string;
var
  n: integer;

begin
  if s <> '' then
  begin
    n := length(s);
    if s[n] = '\' then
      s := copy(s, 1, n - 1);
    LErrorLogPath := s;
    try
      if not DirectoryExists(LErrorLogPath) then
        mkDir(LErrorLogPath);
    except
      LErrorLogPath := GetTempDir;
    end;
  end;
  if LErrorLogPath = '' then
  begin
    LErrorLogPath := CurDir + '\Logs';
    if pos('system', LErrorLogPath) > 0 then
      LErrorLogPath := AppDir + '\Logs';
  end;
  result := LErrorLogPath;
end;

procedure SuspendDebugOn(susp: boolean);
begin
  FSuspended := susp;
end;

procedure SetDebugFileName(nome: string);
begin
  FDebugFileName := nome;
{$IFNDEF BPL}
  add_LogFileConfig(nome);
{$ENDIF}
  if LErrorLogPath = '' then
    LErrorLogPath := ExtractFilePath(nome);

end;

function GetDebugFileName(): string;
begin
  result := FDebugFileName;
end;

function DebugOn: boolean;
begin
  result := FIsDebugActive;
end;

procedure SetDebugOn(ligar: boolean);
begin
  FIsDebugActive := ligar;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure SlashDir(var ADiretorio: string);
var
  Ld: String;
begin
  if ADiretorio = '' then
    ADiretorio := PathDelim
  else if ADiretorio[length(ADiretorio)] <> PathDelim then
  begin
    Ld := ADiretorio + PathDelim;
    ADiretorio := Ld;
  end;
end;

{$WARN SYMBOL_DEPRECATED ON}

function SlashDirEx(ADiretorio: string): string;
begin
  result := ADiretorio;
  SlashDir(result);
end;

function GetNomeArquivoDebug: string;
begin
  result := format('Debug_%s.log', [formatDateTime('ddmmyyyy', date)]);
end;

procedure WriteLogText(AArquivo: string; ATexto: string;
  revisaArquivoDestino: boolean = True);
var
  Lf: textfile;
  Ls: string;

begin
  try
    FLockEscrita.Acquire;
    try
      if LEncerrou then
        exit;
      // grava dados de texto no disco.... para usar somente para log  usar a função  DebugLog
      if revisaArquivoDestino then
      begin

        Ls := formatDateTime('DDMMYYYY', date); // DDMMYYYY(date);
        AArquivo := StringReplace(AArquivo, Ls,
          { YYYYMMDD } formatDateTime('YYYYMMDD', date), [rfReplaceAll]);
        // troca a data para ordem crescente no disco.
        try
          Ls := LErrorLogPath;
          if ExtractFileName(AArquivo) = AArquivo then
            if Ls[1] <> '\' then
              AArquivo := SlashDirEx(Ls) + AArquivo;
        except
        end;

      end;

      try
        ForceDirectories(ExtractFilePath(AArquivo));
      except
      end;

      AssignFile(Lf, AArquivo);
      try
{$I-}
        Append(Lf);
{$I+}
        if IOResult <> 0 then // se o arquivo nao existe, criar um novo;
          Rewrite(Lf);
        WriteLn(Lf, ATexto);
      finally
        CloseFile(Lf);
      end;
    finally
      FLockEscrita.Release;
    end;

  except
    if GetNomeArquivoDebug <> ExtractFileName(AArquivo) then
      /// evitar loop
      ErrorLog('Erro em: ' + AArquivo + ': ' + ATexto);
  end;

end;

function LowS(s: string): integer;
begin
  result := low(s); // pega base 0 quando for o caso
end;

function HighS(s: String): integer;
begin
  result := high(s);
end;

// Procura a posição de pelo menos um item da lista se esta presente no texto (como POS() - para um lista separado por virgula/ponto-virgula)
// Retorno:   0 - não encontrou;  maior 0 -> indica a posição onde se inicia;
// exemplo:   n := PosA( 'windows,system', 'c:\windows\system32'); -> retorna a posição encontrada para a palavra "windows"
function PosA(lista: String; texto: string): integer;
var
  i: integer;
  s: string;
begin
  result := 0;
  lista := lowercase(lista);
  texto := lowercase(texto);
  s := '';
  for i := LowS(lista) to HighS(lista) do
  begin
    case lista[i] of
      ',', ';':
        begin
          result := pos(s, texto);
          if result > 0 then
            exit;
          s := '';
        end;
    else
      s := s + lista[i];
    end;
  end;
  if s <> '' then
    result := pos(s, texto);
end;

var
  LUltimoErro: String;

procedure WriteLogError(
  const ATexto: string);
begin
    if LUltimoErro <> ATexto then
    begin
      try
        WriteLogText('Erros_' + formatDateTime('yyyymmdd', date) + '.log',
          ExtractFileName(ParamStr(0)) + ' ' + ATexto);
        OutputDebugString({$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(ExtractFileName(ParamStr(0)) + ':' + ATexto));
      except
      end;
      LUltimoErro := ATexto;
    end;
end;


procedure WriteLog(AArquivo: string; ATexto: string; AComando: string = '');
var
  ArqVazio: boolean;
begin
  FLockEntrada.Acquire;
  try

    if LEncerrou then
      exit;
    // if FNoLog then exit;
    ArqVazio := false;
    if AArquivo = '' then
    begin
      ArqVazio := True;
      AArquivo := ExtractFileName(ParamStr(0));
      AArquivo := copy(AArquivo, 1, pos('.', AArquivo) - 1) + '_' +
        formatDateTime('yyyymmdd', date) + '.log';
    end;

    if LErrorLogPath = '' then
    begin
      { DONE -oAL -cImportante :
        Quando Utiliza Serviços, o diretorio CURDIR padrão é o c:\windows\system32.
        Para caso de serviços, indicar o diretório de LOG ao entrar no aplicativo. }
      LErrorLogPath := CurDir + '\logs';
      if PosA('windows,system', LErrorLogPath) > 0 then
        LErrorLogPath := ExtractFilePath(ParamStr(0)) + 'Logs';
    end;

    if ExtractFileName(AArquivo) = AArquivo then
      AArquivo := LErrorLogPath + '\' + AArquivo;
    // controle em diretorio diferente os arquivos de logs

    if AComando = '' then
      AComando := 'Vendor : Logs';
    AComando := '  ' + AComando;

    if ((ArqVazio) and (pos('ERRO', ATexto) > 0)) then
      WriteLogError(ATexto)
    else
      try
        WriteLogText(AArquivo, copy(intToStr(LogCount) + '        ', 1, 8) +
          formatDateTime('hh:nn:ss.zzz', Now) + AComando + ' - ' + ATexto);
{$IFDEF MSWINDOWS}
        if DebugOn and (not FNoLog) then
          OutputDebugString({$IFDEF UNICODE}PWideChar{$ELSE}PAnsiChar{$ENDIF}(ExtractFileName(ParamStr(0)) + ':' + ATexto));
        InterlockedIncrement(LogCount);
{$ELSE}
          inc(LogCount);
{$ENDIF}
      except
      end;
  finally
    FLockEntrada.Release
  end;
end;


procedure ErrorLog(ATexto: string);
begin
  FLockEntrada.Acquire;
  try
  WriteLogError(copy(intToStr(LogCount) + '        ', 1, 8) +
    formatDateTime('hh:nn:ss.zzz', Now) + ' Error - ' + ATexto + #13#10 +
    '--------------------------');
  finally
    FLockEntrada.Release;
  end;
end;

procedure DebugLog(ATexto: string; AComando: string = ''; AArquivo: string = '';
  nomeRotina: string = ''; AForcar: boolean = false);
begin // so grava quando estiver com DEBUG.ON   ativo

  if LEncerrou then
    exit;

  if (not FIsDebugActive) or (FSuspended) then
  begin
    exit;
  end;

  if AComando = '' then
    AComando := 'FD';
  AComando := AComando + ' : Logs';

  if AArquivo = '' then
    AArquivo := FDebugFileName;

  if AArquivo = '' then
    AArquivo := GetNomeArquivoDebug;

  if not AForcar then
    if DebugOptions.db_params_out = false then
    begin
      if (pos('Data Out', ATexto) > 0) or (pos('_fetch', ATexto) > 0) then
        exit;
    end;

  if nomeRotina <> '' then
    TThread.NameThreadForDebugging(nomeRotina);

  WriteLog(AArquivo, ATexto, AComando);

end;

function GetRTTILog(snd: TObject; LNivel: integer): string;
var
  LNome: string;
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LVar: TValue;
  txt: String;
  LNivelLocal: integer;
begin
  dec(LNivel);
  if LNivel < 0 then
    exit;
  if not assigned(snd) then
  begin
    result :='';// 'ClassName: NIL';
    exit;
  end;
  LContext := TRttiContext.Create;
  try
    LType := LContext.GetType(snd.ClassType);
    result := 'ClassName: ' + LType.QualifiedName+ #13#10;
    for LProp in LType.GetProperties do
    begin
      try
        LVar := LProp.GetValue(snd);
        txt := LVar.AsString;
        if txt <> '' then
          result := result + LProp.Name + ': ' + txt + #13#10;
        if LVar.IsObject then
        begin
          txt := GetRTTILog(LVar.AsObject, LNivel);
          if txt<>'' then
             result := result + #13#10 + txt;
        end;
      except
      end;
    end;
  finally
    LContext.Free;
  end;
end;

procedure DoAppExceptionEvent(Sender: TObject; E: Exception;
  AShow: boolean = True);
var
  s: string;
  sbase: string;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      try
        try
          sbase := E.Message;
          try
            s := '';
            if assigned(Sender) then
            begin
              s := GetRTTILog(Sender, 2);
            end;


            s := s + ' Message: ' + E.Message;

            s := s+#13#10+'---StackTrace----'#13#10+ e.StackTrace;

            if ActionOnApplicationError = actErrHalt then
               s := s+ #13#10+'Halt';
            if ActionOnApplicationError = actErrTerminate then
               s := s+ #13#10+'Terminate';

            ErrorLog(s);
          except
            on ee: Exception do
              ErrorLog(ee.Message);
          end;
{$IFNDEF SERVICO}
          if AShow then
          begin
            Application.ShowException(E);
          end;
{$ENDIF}
        finally
          if ActionOnApplicationError = actErrHalt then
            halt(0);

          if ActionOnApplicationError = actErrTerminate then
             Application.Terminate;

        end;
      except
      end;
    end);
end;

var
  LDebugOn: string;
  LAppName: string;
  LApagarArquivo: boolean;

initialization

ActionOnApplicationError := actErrContinue;
FLockEscrita := TCriticalSection.Create;
FLockEntrada := TCriticalSection.Create;
LDebugOn := ExtractFilePath(ParamStr(0)) + 'debug.on';

FIsDebugActive := FileExists(LDebugOn) or
  FindCmdLineSwitch('D', ['-', '\', '/'], True);
FNoLog := FindCmdLineSwitch('-D', ['-', '\', '/'], True);
LEncerrou := false;

{$IFDEF MSWINDOWS}
System.ReportMemoryLeaksOnShutdown := isDelphiRunning and FIsDebugActive;
System.NeverSleepOnMMThreadContention := True;
{$ENDIF}
FDebugFileName := lowercase(GetCurrentDir);
if PosA('system,window', FDebugFileName) > 0 then
  FDebugFileName := ExtractFileDir(ParamStr(0));

// FDebugFileName := FDebugFileName + '\logs\Debug_' + formatDateTime('yyyymmdd', date) + '.log';
LAppName := ExtractFileName(ParamStr(0));
LAppName := ChangeFileExt(LAppName, '');
FDebugFileName := FDebugFileName + '\logs\' + LAppName + '_Debug_' +
  formatDateTime('yyyymmdd', date) + '.log';

LogsFilesDir(ExtractFilePath(FDebugFileName));

{$IFNDEF BPL}
add_LogFileConfig(FDebugFileName);
LApagarArquivo := not FileExists('debug.on');
{$ENDIF}
if FIsDebugActive then
begin
  with TIniFile.Create(LDebugOn) do
    try
      if SectionExists('Options') = false then
      begin
        WriteBool('Options', 'Active', True);
        WriteBool('Database', 'ParamsIN', True);
        WriteBool('Database', 'ParamsOUT', false);
        WriteBool('Database', 'Prepare', True);
        WriteBool('Database', 'Unprepare', True);
        WriteBool('Database', 'Execute', True);
        WriteBool('Database', 'DbParams', false);
        WriteBool('Database', 'Transaction', false);
        WriteBool('Options', 'Messages', false);
      end;

      DebugOptions.db_params_in := readBool('Database', 'ParamsIN', True);
      DebugOptions.db_params_out := readBool('Database', 'ParamsOUT', false);
      DebugOptions.db_params_prepare := readBool('Database', 'Prepare', True);
      DebugOptions.db_params_unprepare :=
        readBool('Database', 'Unprepare', True);
      DebugOptions.db_execute := readBool('Database', 'Execute', True);
      DebugOptions.db_dbparams := readBool('Database', 'DbParams', false);
      DebugOptions.db_transaction := readBool('Database', 'Transaction', false);

      DebugOptions.messages := readBool('Database', 'messages', false);

      FIsDebugActive := readBool('Options', 'Active', false);

      if FIsDebugActive then
      begin
        DebugLog('Debug Ligado. (' + FDebugFileName + ') App: ' + ParamStr(0));
        DebugLog('-----------------------------------------------------');
      end;

    finally
      Free;
    end;
end;

Finalization

LEncerrou := True;
FLockEscrita.Free;
FLockEntrada.Free;
if LApagarArquivo then
  deletefile('debug.on');

end.
