{ ******************************************************* }
{ }
{ CodeGear Delphi Runtime Library }
{ }
{ Copyright (c) 1995-2007 CodeGear }
{ }
{ ******************************************************* }

unit System.IniFilesEx;

{$R-,T-,H+,X+}

interface

{$I delphi.inc}
{$I PDVCustomDefine.pas}
{$IFNDEF MSWINDOWS}
{$UNDEF CRIA_LOGS}
{$ENDIF}

uses SysUtils, Classes,
{$IFDEF BPL} VCL.Dialogs, {$ENDIF}
  IniFiles;

type

  TCustomIniFileHelper = class Helper for {$IFDEF MSWINDOWS} TCustomIniFile
{$ELSE} TMemIniFile {$ENDIF}
    private class

var
  FFileNameFull: string;
  FChanged: boolean;
private
  procedure SetFileNameFull(const Value: string);
  function GetFileNameFull: string;
  procedure SetChanged(const Value: boolean);
  function GetChanged: boolean;
public
  constructor Create(const AFileName: string);
  property FileNameFull: string read GetFileNameFull write SetFileNameFull;
  function ReadStringDefault(const Section, Ident, Default: string): string;
  function ReadIntegerDefault(const Section, Ident: string;
    Default: integer): integer;
  function GetValuesCount(Section: string): integer;
  property Changed: boolean read GetChanged write SetChanged;
  function ToJson: string;
  procedure FromJson(AJson: string);
  procedure WriteObject(const ASection: string; AObj: TObject);
  procedure ReadObject(const ASection: string; AObj: TObject);
  end;

  TMobiIniFile = class(TIniFile)public constructor Create(sFile: string);
  end;

  function GetIniFilesDir(): string;
  function GetConfigFilesDir(sFile: string; usuario: string = ''): string;
  procedure SetIniFilesDir(sDir: string);
  procedure add_LogFileConfig(sFile: string);
  function GetAppIniFileName(): string;

var
  PlacementUserCod: string;

implementation

uses
  System.Threading, System.SyncObjs,
  System.Json, DllFunctions, System.DateUtils,
  System.Rtti, System.Classes.helper, System.TypInfo, System.Rtti.helper,
{$IFDEF MSWINDOWS}
  Windows, uRegistry_Functions,
{$ENDIF}
{$IFDEF VCL}
  uDebug, RTLConsts, Registry;
{$ELSE}
System.IOUtils;
{$ENDIF}

var
  FIniDirectory: String;

const
{$IFDEF MSWINDOWS}
  barraPath = '\';
{$ELSE}
  barraPath = PathDelim;
{$ENDIF}
  { TCustomIniFile }       // uJson     db.helper


function ISODateTimeToString(ADateTime: TDatetime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function ISOStrToDateTime(DateTimeAsString: string): TDatetime;
begin
  result := EncodeDateTime(StrToInt(Copy(DateTimeAsString, 1, 4)),
    StrToInt(Copy(DateTimeAsString, 6, 2)),
    StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)),
    StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;


procedure TCustomIniFileHelper.FromJson(AJson: string);
var
  LJson: TJsonObject;
  LSecao: TJsonArray;
  Chave: string;
  item: string;
  Value: string;
  it: TJSONPair;
  itValue: TJsonValue;
  pair: TJSONPair;
  LItem: TJsonObject;
  i: integer;
begin
  LJson := TJsonObject.ParseJSONValue(AJson) as TJsonObject;
  for it in LJson do
  begin
    Chave := it.JsonString.Value;
    LJson.TryGetValue<TJsonArray>(Chave, LSecao);
    for itValue in LSecao do
    begin
      LItem := itValue as TJsonObject;
      for pair in LItem do
        writeString(Chave, pair.JsonString.Value, pair.JsonValue.Value);
    end;
  end;
end;

function TCustomIniFileHelper.GetChanged: boolean;
begin
  result := FChanged;
end;

function TCustomIniFileHelper.GetFileNameFull: string;
begin
  result := FFileNameFull;
end;

function TCustomIniFileHelper.GetValuesCount(Section: string): integer;
var
  str: TStringList;
begin
  str := TStringList.Create;
  try
    ReadSectionValues(Section, str);
    result := str.count;
  finally
    str.free;
  end;
end;

function TCustomIniFileHelper.ToJson: string;
var
  i, n: integer;
  sec: TStringList;
  it: TStringList;
  s: string;
begin
  sec := TStringList.Create;
  it := TStringList.Create;
  try
    ReadSections(sec);
    result := '';
    for i := 0 to sec.count - 1 do
    begin
      ReadSection(sec[i], it);
      s := '"' + sec[i] + '": [{';
      for n := 0 to it.count - 1 do
      begin
        if n > 0 then
          s := s + ', ';
        s := s + '"' + it[n] + '": "' + ReadString(sec[i], it[n], '') + '"';
      end;
      s := s + '}]';
      if result <> '' then
        result := result + ', ';
      result := result + s;
    end;
    result := '{' + result + '}';
  finally
    it.free;
    sec.free;
  end;

end;


procedure TCustomIniFileHelper.WriteObject(const ASection: string;
  AObj: TObject);
var
  aCtx: TRttiContext;
  AFld: TRttiProperty;
  AValue: TValue;
begin
  aCtx := TRttiContext.Create;
  try
    for AFld in aCtx.GetType(AObj.ClassType).GetProperties do
    begin
      if AFld.Visibility in [mvPublic] then
      begin
        AValue := AFld.GetValue(AObj);
        if AValue.IsDate or AValue.IsDateTime then
          WriteString(ASection, AFld.Name, ISODateTimeToString(AValue.AsDouble))
        else if AValue.IsBoolean then
          WriteBool(ASection, AFld.Name, AValue.AsBoolean)
        else if AValue.IsInteger then
          WriteInteger(ASection, AFld.Name, AValue.AsInteger)
        else if AValue.IsFloat or AValue.IsNumeric then
          WriteFloat(ASection, AFld.Name, AValue.AsFloat)
        else
          WriteString(ASection, AFld.Name, AValue.ToString);
      end;
    end;
  finally
    aCtx.free;
  end;
end;

procedure TCustomIniFileHelper.ReadObject(const ASection: string;
  AObj: TObject);
var
  aCtx: TRttiContext;
  AFld: TRttiProperty;
  AValue, ABase: TValue;
begin
  aCtx := TRttiContext.Create;
  try
    for AFld in aCtx.GetType(AObj.ClassType).GetProperties do
    begin
      if AFld.Visibility in [mvPublic] then
      begin
        ABase := AFld.GetValue(AObj);
        AValue := AFld.GetValue(AObj);
        if ABase.IsDate or ABase.IsDateTime then
          AValue := ISOStrToDateTime(ReadString(ASection, AFld.Name,
            ISODateTimeToString(ABase.AsDouble)))
        else if ABase.IsBoolean then
          AValue := ReadBool(ASection, AFld.Name, ABase.AsBoolean)
        else if ABase.IsInteger then
          AValue := ReadInteger(ASection, AFld.Name, ABase.AsInteger)
        else if ABase.IsFloat or ABase.IsNumeric then
          AValue := ReadFloat(ASection, AFld.Name, ABase.AsFloat)
        else
          AValue := ReadString(ASection, AFld.Name, ABase.asString);
        AFld.SetValue(AObj, AValue);
      end;
    end;
  finally
    aCtx.free;
  end;
end;

function TCustomIniFileHelper.ReadIntegerDefault(const Section, Ident: string;
  Default: integer): integer;
begin
  result := strToIntDef(ReadStringDefault(Section, Ident,
    intToStr(Default)), 0);
end;


function TCustomIniFileHelper.ReadStringDefault(const Section, Ident,
  Default: string): string;
begin
  result := ReadString(Section, Ident, '');
  if result = '' then
  begin
    try
      writeString(Section, Ident, Default);
    except
    end;
    FChanged := true;
    result := Default;
  end;
end;

function GetAppIniFileName(): string;
begin
  result := ExtractFileName(ParamStr(0));
  result := copy(result, 1, pos('.', result) - 1) + '.ini';
end;

function AppDir(): string;
begin
  result := ExtractFilePath(ParamStr(0));
end;

function GetWinDir: string;
var
  LDir: array [0 .. 255] of Char;
begin
{$IFDEF VCL}
  GetWindowsDirectory(LDir, 255);
  result := StrPas(LDir);
{$ELSE}
  result := TPath.GetSharedDocumentsPath;
{$ENDIF}
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure SlashDir(var ADiretorio: string);
// var
// LD: String;
begin
  if ADiretorio = '' then
    ADiretorio := barraPath
  else if ADiretorio[Length(ADiretorio)] <> barraPath then
  begin
    ADiretorio := ADiretorio + barraPath;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}

procedure SetIniFilesDir(sDir: string);
begin
  SlashDir(sDir);
  FIniDirectory := sDir;
  try
    if not DirectoryExists(FIniDirectory) then
      mkDir(FIniDirectory);
  except
  end;

end;

function GetConfigFilesDir(sFile: string; usuario: string = ''): string;
var
  i: integer;
  wf: string;
begin
  if usuario = '' then
    usuario := PlacementUserCod;

  result := ExtractFileName(sFile);
  i := pos('.', result);
  if i > 1 then
    result := copy(result, 1, i - 1);

  wf := System.IniFilesEx.GetIniFilesDir + 'usuario' + usuario + barraPath;
  if not DirectoryExists(wf) then
    ForceDirectories(wf);
  result := wf + result + '.config';

  if (not fileExists(result)) then
    if fileExists(GetWinDir + barraPath + ExtractFileName(sFile)) then
    begin
      wf := GetWinDir + barraPath + ExtractFileName(sFile);
      { if FileExists(wf) then    // nao precisa gravar a posicao antiga.
        CopyFile(pchar(wf),pchar(result),false); }  // nao precisa...
    end;
  add_LogFileConfig(result);
end;

{$IFNDEF MSWINDOWS}

function GetProgramFilesDir: string;
begin
  result := TPath.GetDocumentsPath;
end;
{$ENDIF}

function GetIniFilesDir(): string;
begin
  if FIniDirectory = '' then
  begin
{$IFDEF BPL}
    // FIniDirectory := GetTempDir;
{$ELSE}
    FIniDirectory := GetProgramFilesDir + barraPath + 'Store' + barraPath +
      'Config' + barraPath;
{$ENDIF}
    try
      if not DirectoryExists(FIniDirectory) then
        ForceDirectories(FIniDirectory);
    except
    end;
  end;
  result := FIniDirectory;
end;

constructor TCustomIniFileHelper.Create(const AFileName: string);
var
  s: string;
  winFileName, tmp: String;
  i: integer;
begin
{$IFDEF BPL}
  FFileNameFull := AFileName;
{$ELSE}
  FChanged := true; // reavaliar - usado no IniFileList;
  tmp := AFileName;

{$IFDEF MSWINDOWS}
  i := pos(barraPath + 'store' + barraPath + 'config' + barraPath,
    lowerCase(tmp));
  if i > 0 then
  begin
    if not fileExists(tmp) then
    begin
      i := pos(barraPath + 'usuario', AFileName);
      if i > 0 then
        tmp := FIniDirectory + copy(tmp, i + 1, 255)
      else
        tmp := FIniDirectory + ExtractFileName(AFileName); // limpa
    end;
  end;

  // FFileName := tmp;
  s := ExtractFileName(tmp);
  if tmp <> s then // checa se ja foi definido o diretorio padrao do arquivo
    FFileNameFull := tmp // ja foi definido o diretorio
  else
  begin

    if sametext(ExtractFileExt(AFileName), '.config') then
    begin
      FFileNameFull := GetConfigFilesDir(AFileName);
    end
    else
    begin
      if FIniDirectory = '' then
        GetIniFilesDir();
      if fileExists(FIniDirectory + AFileName) then
        FFileNameFull := FIniDirectory + AFileName
        // checa se tem o arquivo no diretorio de inicio exec
      else if fileExists(AppDir + AFileName) then
        // procurar primeiro no diretorio local.
        FFileNameFull := AppDir + AFileName
        // checa se existe o arquivo no diretorio do EXE
      else
      begin
        winFileName := GetWinDir + barraPath + s;
        if fileExists(winFileName) then
        begin
          try // migra as configuraoces para a pasta config.
            tmp := FIniDirectory + ExtractFileName(winFileName);
            CopyFile(pchar(winFileName), pchar(tmp), false);
            FFileNameFull := tmp;
            winFileName := '';
            // Estava saindo e nao carregava o inherited do inifile
            // ocorria erro para ler ou gravar - Calixto
            // exit;
          except
          end;
        end;
        if (AFileName = s) and (not fileExists(winFileName)) then
          FFileNameFull := FIniDirectory + AFileName
          // se nao existe o arquivo... criar o arquivo localmente
        else
          FFileNameFull := AFileName;
      end;
    end;
  end;

  add_LogFileConfig(FFileNameFull);
{$ELSE}
  FFileNameFull := tmp;
  s := ExtractFileName(tmp);
  if sametext(tmp, s) then
    FFileNameFull := TPath.Combine(TPath.GetDocumentsPath, tmp);
{$ENDIF}
{$ENDIF}
{$IFDEF BPL}
  FFileNameFull := 'c:\fontes\' + ExtractFileName(FFileNameFull);
  try
    inherited Create(FFileNameFull);
  except
    // SetDebugOn(true);
    // DebugLog(FFileNameFull);
    showMessage(FFileNameFull);
  end;
{$ELSE}
  inherited Create(FFileNameFull);

{$ENDIF}
end;

procedure TCustomIniFileHelper.SetChanged(const Value: boolean);
begin
  FChanged := Value;
end;

procedure TCustomIniFileHelper.SetFileNameFull(const Value: string);
begin
  FFileNameFull := Value;
end;

var
  LLogFileConfig: TStringList;
  LEncerrou: boolean = false;
  LLock: tCriticalSection;

procedure add_LogFileConfig(sFile: string);
begin
  if LEncerrou then
    exit;
  if not assigned(LLogFileConfig) then
    LLogFileConfig := TStringList.Create;
  try
    System.TMonitor.Enter(LLogFileConfig);
    if LLogFileConfig.IndexOf(sFile) < 0 then
      LLogFileConfig.add(sFile);
  finally
    System.TMonitor.exit(LLogFileConfig);
  end;
end;
{$IFDEF XE}

function lowS(s: string): integer;
begin
  result := low(s);
end;

function highS(s: string): integer;
begin
  result := high(s);
end;
{$ENDIF}

// Procura a posição de pelo menos um item da lista se esta presente no texto (como POS() - para um lista separado por virgula/ponto-virgula)
// Retorno:   0 - não encontrou;  maior 0 -> indica a posição onde se inicia;
// exemplo:   n := PosA( 'windows,system', 'c:\windows\system32'); -> retorna a posição encontrada para a palavra "windows"
function PosA(lista: String; texto: String): integer;
var
  i: integer;
  s: string;
begin
  result := -1;
  lista := lowerCase(lista);
  texto := lowerCase(texto);
  s := '';
  for i := lowS(lista) to highS(lista) do
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
  LDir: string;

  { TMobiIniFile }

constructor TMobiIniFile.Create(sFile: string);
begin

{$IFDEF MSWINDOWS}
  inherited Create(sFile);
{$ELSE}
  inherited Create(TPath.Combine(TPath.GetDocumentsPath,
    ExtractFileName(sFile)));
{$ENDIF}
end;

initialization

// LLogFileConfig := TStringList.Create;
LLock := tCriticalSection.Create; // compatiblidade com XE6
FIniDirectory := '';
PlacementUserCod := '0';

finalization

{$IFDEF CRIA_LOGS}
try
  LDir := lowerCase(GetCurrentDir + barraPath + 'Logs');
  if PosA('system,windows', LDir) > 0 then
    LDir := ExtractFilePath(ParamStr(0)) + barraPath + 'Logs';
  if not DirectoryExists(LDir) then
    ForceDirectories(LDir);
  if assigned(LLogFileConfig) then
  begin
    LLogFileConfig.SaveToFile(LDir + barraPath + 'ArquivosCarregados.txt');
  end;
except
end;
{$ENDIF}
freeAndNil(LLogFileConfig);
LEncerrou := true;
{$IFDEF VCL}
// DebugLog('Encerrou: IniFilesEx');
{$ENDIF}
freeAndNil(LLock);

end.
