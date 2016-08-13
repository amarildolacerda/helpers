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


unit System.JsonFiles;

interface

uses System.Classes, System.SysUtils, System.Rtti, System.Json, Rest.Json,
  System.iniFiles;

type
  TMemJsonFiles = class
  private
    FEncoding: TEncoding;
    FFileName: string;
    FJson: TJsonObject;
    FAutoSave: boolean;
    FModified: boolean;
    procedure SetAutoSave(const Value: boolean);
    procedure SetModified(const Value: boolean);
    procedure WriteValue(const Section, Ident: string; Value: TValue);
    function ReadValue(const Section, Ident: string;
  Default: Variant): Variant;
  protected
    procedure LoadValues; virtual;
  public

    constructor Create(AFilename: string); overload; virtual;
    constructor Create(const AFilename: string; const AEncoding: TEncoding);
      overload; virtual;

    destructor Destroy; override;
    property FileName: string read FFileName;
    procedure ReadSection(const Section: string; Strings: TStrings); overload;
    function ReadSection(const Section: string): TJsonObject; overload;
    function ReadSectionJsonValue(const Section: TJsonObject; Ident: string)
      : TJsonValue;

    procedure ReadSections(Strings: TStrings); virtual;
    procedure ReadSectionValues(const Section: string;
      Strings: TStrings); virtual;

    procedure WriteString(const Section, Ident: string; Value: string); virtual;
    procedure WriteInteger(const Section, Ident: string;
      Value: Integer); virtual;
    procedure WriteDateTime(const Section, Ident: string;
      Value: TDateTime); virtual;
    procedure WriteBool(const Section, Ident: string; Value: boolean); virtual;
    procedure WriteFloat(const Section, Ident: string; Value: double); virtual;
    function ReadString(const Section, Ident, Default: string): string; virtual;
    function ReadInteger(const Section, Ident: string; Default: Integer)
      : Integer; virtual;
    function ReadBool(const Section, Ident: string; Default: boolean)
      : boolean; virtual;
    function ReadDatetime(const Section, Ident: string; Default: TDateTime)
      : TDateTime; virtual;
    function ReadFloat(const Section, Ident: string; Default: double)
      : double; virtual;

    procedure Clear;
    procedure UpdateFile; virtual;
    property Modified: boolean read FModified write SetModified;
    property AutoSave: boolean read FAutoSave write SetAutoSave;
    function ToJson: string;
    procedure FromJson(AJson: string);
  published
  end;

  TJsonFiles = class(TMemJsonFiles)
  end;

implementation

uses System.DateUtils;
{ TMemJsonFiles }

type
  TJsonObjectHelper = class helper for TJsonObject
  public
    function Find(Section: string): TJsonValue;
  end;

  TValueHelper = record helper for TValue
  private
    function IsNumeric: boolean;
    function IsInteger: boolean;
    function IsDate: boolean;
    function IsDateTime: boolean;
    function IsBoolean: boolean;
  end;

function ISODateTimeToString(ADateTime: TDateTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime, fs);
end;

function ISOStrToDateTime(DateTimeAsString: string): TDateTime;
begin
  Result := EncodeDateTime(StrToInt(Copy(DateTimeAsString, 1, 4)),
    StrToInt(Copy(DateTimeAsString, 6, 2)),
    StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)),
    StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;

function TValueHelper.IsNumeric: boolean;
begin
  Result := Kind in [tkInteger, tkChar, tkEnumeration, tkFloat,
    tkWChar, tkInt64];
end;

function TValueHelper.IsBoolean: boolean;
begin
  Result := TypeInfo = System.TypeInfo(boolean);
end;

function TValueHelper.IsInteger: boolean;
begin
  Result := TypeInfo = System.TypeInfo(Integer);
end;

function TValueHelper.IsDate: boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDate);
end;

function TValueHelper.IsDateTime: boolean;
begin
  Result := TypeInfo = System.TypeInfo(TDateTime);
end;

procedure TMemJsonFiles.Clear;
begin
  FreeAndNil(FJson);
  FJson := TJsonObject.Create;
end;

constructor TMemJsonFiles.Create(AFilename: string);
begin
  Create(AFilename, nil);
end;

constructor TMemJsonFiles.Create(const AFilename: string;
  const AEncoding: TEncoding);
begin
  inherited Create;
  FAutoSave := true;
  FJson := TJsonObject.Create;
  FEncoding := AEncoding;
  FFileName := AFilename;
  LoadValues;

end;

destructor TMemJsonFiles.Destroy;
begin
  if AutoSave and Modified then
    UpdateFile;
  FJson.Free;
  inherited;
end;

procedure TMemJsonFiles.FromJson(AJson: string);
begin
  FreeAndNil(FJson);
  FJson := TJsonObject.ParseJSONValue(AJson) as TJsonObject;
  FModified := false;
end;

procedure TMemJsonFiles.LoadValues;
var
  Size: Integer;
  Buffer: TBytes;
  Stream: TFileStream;
begin
  try
    if (FileName <> '') and FileExists(FileName) then
    begin
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        // Load file into buffer and detect encoding
        Size := Stream.Size - Stream.Position;
        SetLength(Buffer, Size);
        Stream.Read(Buffer[0], Size);
        Size := TEncoding.GetBufferEncoding(Buffer, FEncoding);

        // Load strings from buffer
        FromJson(FEncoding.GetString(Buffer, Size, Length(Buffer) - Size));
      finally
        Stream.Free;
      end;
    end
    else
      Clear;
  finally
    Modified := false;
  end;
end;

procedure TMemJsonFiles.SetAutoSave(const Value: boolean);
begin
  FAutoSave := Value;
end;

procedure TMemJsonFiles.SetModified(const Value: boolean);
begin
  FModified := Value;
end;

function TMemJsonFiles.ToJson: string;
begin
  Result := FJson.ToJson;
end;

procedure TMemJsonFiles.UpdateFile;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    List.Text := ToJson;
    List.SaveToFile(FFileName, FEncoding);
  finally
    List.Free;
  end;
  Modified := false;
end;

procedure TMemJsonFiles.ReadSection(const Section: string; Strings: TStrings);
var
  sec: TJsonObject;
  v: TJsonPair;
begin
  Strings.Clear;
  sec := ReadSection(Section);
  if not assigned(sec) then
    exit;
  for v in sec do
  begin
    Strings.Add(v.JsonString.Value);
  end;

end;

function TMemJsonFiles.ReadBool(const Section, Ident: string;
  Default: boolean): boolean;
var
  j: TJsonObject;
begin
  // result := ReadValue(Section,Ident,Default);

end;

function TMemJsonFiles.ReadDatetime(const Section, Ident: string;
  Default: TDateTime): TDateTime;
var
  v: variant;
begin
  Result := Default;
  v := ReadValue(Section, Ident, ISODateTimeToString(Default));
  Result := ISOStrToDateTime(v);
end;

function TMemJsonFiles.ReadFloat(const Section, Ident: string;
  Default: double): double;
var
  v: Variant;
begin
  Result := Default;
  v := ReadValue(Section, Ident, Default);
 Result := StrToFloatDef(v, 0);
end;

function TMemJsonFiles.ReadInteger(const Section, Ident: string;
  Default: Integer): Integer;
var
  v: Variant;
begin
  v := ReadValue(Section, Ident, Default);
  Result := StrToIntDef(v, 0);
end;

function TMemJsonFiles.ReadSection(const Section: string): TJsonObject;
var
  v: TJsonValue;
begin
  Result := nil;
  v := nil;
  FJson.TryGetValue<TJsonValue>(Section, v);
  if assigned(v) then
    Result := v as TJsonObject;
end;

procedure TMemJsonFiles.ReadSections(Strings: TStrings);
var
  v: TJsonPair;
begin
  Strings.Clear;
  for v in FJson do
  begin
    Strings.Add(v.JsonString.Value);
  end;
end;

function TMemJsonFiles.ReadSectionJsonValue(const Section: TJsonObject;
  Ident: string): TJsonValue;
begin
  Result := nil;
  Section.TryGetValue<TJsonValue>(Ident, Result);
end;

procedure TMemJsonFiles.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  v: TJsonPair;
  j: TJsonObject;
begin
  Strings.Clear;
  j := ReadSection(Section);
  if not assigned(j) then
    exit;
  for v in j do
  begin
    Strings.Add(v.JsonString.Value + '=' + v.JsonValue.Value);
  end;
end;

function TMemJsonFiles.ReadString(const Section, Ident,
  Default: string): string;
var
  v: Variant;
begin
  result := Default;
  v := ReadValue(Section, Ident, Default);
    Result := v;
end;

function TMemJsonFiles.ReadValue(const Section, Ident: string;
  Default: Variant): Variant;
var
  j: TJsonObject;
  v: TJsonValue;
begin
  result := Default;
  j := ReadSection(Section);
  if not assigned(j) then
    exit;

  v := j.Find(Ident);
  if not assigned(v) then
    exit;
  Result := v.Value;

end;

procedure TMemJsonFiles.WriteBool(const Section, Ident: string; Value: boolean);
begin
  WriteValue(Section, Ident, TJSONBool.Create(Value).ToString);
end;

procedure TMemJsonFiles.WriteDateTime(const Section, Ident: string;
  Value: TDateTime);
begin
  WriteValue(Section, Ident, ISODateTimeToString(Value));
end;

procedure TMemJsonFiles.WriteFloat(const Section, Ident: string; Value: double);
begin
  WriteValue(Section, Ident, TJSONNumber.Create(Value).ToString);
end;

procedure TMemJsonFiles.WriteInteger(const Section, Ident: string;
  Value: Integer);
begin
  WriteValue(Section, Ident, Value);
end;

procedure TMemJsonFiles.WriteString(const Section, Ident: string;
  Value: string);
begin
  WriteValue(Section, Ident, Value);
end;

procedure TMemJsonFiles.WriteValue(const Section, Ident: string; Value: TValue);
var
  AArray: TJsonObject;
  AValue: TJsonValue;
  procedure Add;
  begin
    if Value.IsInteger then
      AArray.AddPair(Ident, TJSONNumber.Create(Value.AsInteger))
    else if Value.IsDate or Value.IsDateTime then
      AArray.AddPair(Ident, ISODateTimeToString(Value.AsExtended))
    else if Value.IsNumeric then
      AArray.AddPair(Ident, TJSONNumber.Create(Value.AsExtended))
    else if Value.IsBoolean then
      AArray.AddPair(Ident, TJSONBool.Create(Value.AsBoolean))
    else
      AArray.AddPair(Ident, Value.AsString)
  end;

begin
  AArray := ReadSection(Section);
  if not assigned(AArray) then
  begin
    AArray := TJsonObject.Create;
    FJson.AddPair(Section, AArray)
  end;

  AValue := ReadSectionJsonValue(AArray, Ident);
  if not assigned(AValue) then
    Add
  else
  begin
    AArray.RemovePair(Ident);
    Add;
  end;
  FModified := true;
end;

{ TJsonObjectHelper }

function TJsonObjectHelper.Find(Section: string): TJsonValue;
begin
  Result := FindValue(Section);
end;

end.
