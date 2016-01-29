{
  Autor: Amarildo Lacerda
  Data: 28/01/2016
  Alteraçoes: 
      28/01/16 - Primeira versão publicada
      29/01/16 - correção DoLoopEvent;

}

unit Data.DB.Helper;

interface

uses Classes, SysUtils, DB;

type

  TDatasetHelper = class helper for TDataset
  private
    procedure JsonToRecord(sJson: string; AAppend: boolean);
  public
    procedure OpenAnonimous(proc: TProc);
    procedure Run(AProc:TProc<TDataset>);
    procedure Append(AEvent: TProc<TDataset>); overload;
    procedure Append(AEvent: TProc); overload;
    procedure Post(AEvent: TProc); overload;
    procedure Post(AEvent: TProc<TDataset>); overload;
    function ToJson(): String;
    procedure ChangeAllValuesTo(AFieldName: string; AValue: Variant); overload;
    procedure ChangeAllValuesTo(AFieldName: string; AValue: Variant;
      AConfirm: TFunc<boolean>); overload;
    procedure AppendFromJson(sJson: string);
    procedure CopyFromJson(sJosn: string);
    procedure DoLoopEvent(AEvent: TProc); overload;
    procedure DoLoopEvent(AEvent: TProc<TDataset>); overload;
    procedure DoEventIf(AFieldName: string; AValue: Variant;
      AEvent: TProc); overload;
    procedure DoEventIf(AFieldName: string; AValue: Variant;
      AEvent: TProc<TDataset>); overload;
    procedure Value(AField:string;AValue:variant);overload;
    function Value(AField:string):Variant;overload;
    procedure AppendRecords(ADataSet:TDataset);
    procedure FieldMask(fld:String;mask:string);
    procedure FieldTitle(AFld:string;ATitle:string);

  end;

  TFieldsHelper = class helper for TFields
  public
    function ToJson(): string;
    procedure FormJson(AJson:string);
  end;


implementation

uses System.uJson, System.JSON, System.DateUtils,
     SqlTimSt,   FmtBcd,
     Soap.EncdDecd;

procedure TDatasetHelper.ChangeAllValuesTo(AFieldName: string; AValue: Variant);
begin
  ChangeAllValuesTo(AFieldName, AValue, function: boolean
    begin
      result := true;
    end);
end;

procedure TDatasetHelper.Append(AEvent: TProc<TDataset>);
begin
  Append;
  AEvent(self);
end;

procedure TDatasetHelper.Append(AEvent: TProc);
begin
  Append(procedure(ds: TDataset)
    begin
      AEvent;
    end);
end;

procedure TDatasetHelper.AppendFromJson(sJson: string);
begin
  JsonToRecord(sJson, true);
end;


procedure TDatasetHelper.OpenAnonimous(proc: TProc);
begin
   open;
   if active and assigned(proc) then
      proc;
end;

procedure TDatasetHelper.AppendRecords(ADataSet: TDataset);
begin
    while ADataset.Eof=false do
    begin
        append;
        CopyFields(ADataset);
        Post;
    end;
end;

procedure TDatasetHelper.ChangeAllValuesTo(AFieldName: string; AValue: Variant;
AConfirm: TFunc<boolean>);
var
  book: TBookMark;
  fld: TField;
begin
  fld := FindField(AFieldName);
  if fld = nil then
    exit;
  book := GetBookmark;
  DisableControls;
  try
    first;
    while eof = false do
    begin
      if AConfirm then
      begin
        if fld.Calculated then
          FieldByName(AFieldName).Value := AValue
        else if FieldByName(AFieldName).Value <> AValue then
        begin
          Edit;
          FieldByName(AFieldName).Value := AValue;
          Post;
        end;
      end;
      next;
    end;
  finally
    if BookmarkValid(book) then
      GotoBookmark(book);
    FreeBookmark(book);
    EnableControls;
  end;
end;

procedure TDatasetHelper.CopyFromJson(sJosn: string);
begin
  JsonToRecord(sJosn, false);
end;

procedure TDatasetHelper.DoLoopEvent(AEvent: TProc<TDataset>);
var
  book: TBookMark;
begin
  book := GetBookmark;
  try
    DisableControls;
    first;
    while eof = false do
    begin
      AEvent(self);
      next;
    end;
  finally
    GotoBookmark(book);
    FreeBookmark(book);
    EnableControls;
  end;
end;

procedure TDatasetHelper.FieldTitle(AFld, ATitle: string);
var f:TField;
begin
    f := findField(AFld);
    if not assigned(f) then exit;
    f.DisplayLabel := ATitle;
end;

procedure TDatasetHelper.FieldMask(fld, mask: string);
var f : TField;
begin
    f := FindField(fld);
    if not assigned(f) then exit;
    case f.DataType of
      ftFloat,ftCurrency:
        TFloatField(f).DisplayFormat := mask;
       ftDate:
          TDateField(f).DisplayFormat := mask;
       ftDateTime:
          TDateTimeField(f).DisplayFormat := mask;
       ftString:
       begin
          TStringField(f).DisplayLabel := mask;
          TStringField(f).EditMask := mask;
       end;
    end;
end;


procedure TDatasetHelper.DoEventIf(AFieldName: string; AValue: Variant;
AEvent: TProc);
begin
  DoEventIf(AFieldName, AValue, procedure(ds: TDataset)
    begin
      AEvent;
    end);
end;

procedure TDatasetHelper.DoLoopEvent(AEvent: TProc);
begin
  DoLoopEvent(procedure(ds: TDataset)
    begin
      AEvent;
    end);
end;


procedure TDatasetHelper.DoEventIf(AFieldName: string; AValue: Variant;
AEvent: TProc<TDataset>);
var
  book: TBookMark;
  fld: TField;
begin
  if not assigned(AEvent) then
    exit;
  fld := FindField(AFieldName);
  if fld = nil then
    exit;
  book := GetBookmark;
  DisableControls;
  try
    first;
    while eof = false do
    begin
      if FieldByName(AFieldName).Value = AValue then
        AEvent(self) // quando exclui uma linha, ja salta para o item seguinte.
                     // o next fica por conta da rotina que chamou;
      else
        next; // nao excluir nada, apontar para o proximo
    end;
  finally
    if BookmarkValid(book) and (eof <> bof) then
    begin
      GotoBookmark(book);
    end;
    FreeBookmark(book);
    EnableControls;
  end;
end;

procedure TDatasetHelper.Run(AProc: TProc<TDataset>);
begin
   TThread.CreateAnonymousThread(
     procedure begin
        AProc(self);
     end
   ).Start;
end;

procedure TDatasetHelper.JsonToRecord(sJson: string; AAppend: boolean);
var
  o, k: IJson;
  A: TJSONArray;
  j: integer;
  fld: TField;
  i: integer;
begin
  o := TJSON.Parse(sJson);

  if o.Contains('result') = false then
    raise exception.Create('Não possui tag "result" no json');

  o.TryGetValue<TJSONArray>('result', A);

  if not AAppend then
  begin
    if A.Length > 1 then
      raise exception.Create
        ('Muitas linhas para substituir do json (permitido 1)');
  end;

  for j := 0 to A.Length - 1 do
  begin
    k := A.Get(j) as TJsonObject;
    i := 0;
    for fld in Fields do
    begin
      if k.Contains(lowercase(fld.FieldName)) then
      begin
        if not(state in [dsEdit, dsInsert]) then
          if AAppend then
            Append
          else
            Edit;
        fld.Value := k.V(lowercase(fld.FieldName));
        inc(i);
      end;
    end;
    if state in [dsEdit, dsInsert] then
      Post;
  end;
end;

procedure TDatasetHelper.Post(AEvent: TProc);
begin
  Post(procedure(ds: TDataset)
    begin
      AEvent;
    end);
end;

procedure TDatasetHelper.Post(AEvent: TProc<TDataset>);
begin
  AEvent(self);
  Post;
end;

function TDatasetHelper.ToJson: String;
var
  book: TBookMark;
  lst: TStringList;
begin
  result := '[]';
  book := GetBookmark;
  try
    lst := TStringList.Create;
    try
      lst.Delimiter := ',';
      DisableControls;
      while eof = false do
      begin
        lst.Add(Fields.ToJson());
        next;
      end;
      result := '[' + lst.DelimitedText + ']';
    finally
      lst.Free;
    end;
  finally
    EnableControls;
    GotoBookmark(book);
    FreeBookmark(book);
  end;

end;

procedure TDatasetHelper.Value(AField: string; AValue: variant);
begin
     FieldBYName(AField).Value := AValue;
end;

function TDatasetHelper.Value(AField: string): Variant;
begin
    result := FieldByName(AField).Value;
end;

{ TFieldsHelper }

procedure TFieldsHelper.FormJson(AJson: string);
var j:TJSONObject;
    v:TJsonValue;
    jp: TJSONPair;
    it:TField;
    key : string;
    fs: TFormatSettings;
    MS: TMemoryStream;
    SS: TStringStream;

begin
    j := TJSONObject.Parse(AJson);
    try
    for it in self do
    begin
      key := lowerCase(it.FieldName);
    jp := j.Get(key);
    if Assigned(jp) then
      if not(jp.JsonValue is TJSONNull) then
        v := j.Get(key).JsonValue;
    if not Assigned(v) then
    begin
      it.Clear;
      Continue;
    end;

        case it.DataType of
      TFieldType.ftInteger, TFieldType.ftAutoInc, TFieldType.ftSmallint, TFieldType.ftShortint:
        begin
          it.AsInteger := (v as TJSONNumber).AsInt;
        end;
      TFieldType.ftLargeint:
        begin
          it.AsLargeInt := (v as TJSONNumber).AsInt64;
        end;
      TFieldType.ftSingle, TFieldType.ftFloat:
        begin
          it.AsFloat := (v as TJSONNumber).AsDouble;
        end;
      ftString, ftWideString, ftMemo, ftWideMemo:
        begin
          it.AsString := (v as TJSONString).Value;
        end;
      TFieldType.ftDate:
        begin
          it.AsDateTime := ISOStrToDate((v as TJSONString).Value);
        end;
      TFieldType.ftDateTime:
        begin
          it.AsDateTime := ISOStrToDateTime((v as TJSONString).Value);
        end;
      TFieldType.ftTimeStamp:
        begin
          it.AsSQLTimeStamp := StrToSQLTimeStamp((v as TJSONString).Value);
        end;
      TFieldType.ftCurrency:
        begin
          fs.DecimalSeparator := '.';
{$IF CompilerVersion <= 27}
          it.AsCurrency := StrToCurr((v as TJSONString).Value, fs);
{$ELSE} // Delphi XE7 introduces method "ToJSON" to fix some old bugs...
          it.AsCurrency := StrToCurr((v as TJSONNumber).ToJSON, fs);
{$ENDIF}
        end;
      TFieldType.ftFMTBcd:
        begin
          it.AsBcd := DoubleToBcd((v as TJSONNumber).AsDouble);
        end;
      TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
        begin
          MS := TMemoryStream.Create;
          try
            SS := TStringStream.Create((v as TJSONString).Value, TEncoding.ASCII);
            try
              DecodeStream(SS, MS);
              MS.Position := 0;
              TBlobField(it).LoadFromStream(MS);
            finally
              SS.Free;
            end;
          finally
            MS.Free;
          end;
        end;
      // else
      // raise EMapperException.Create('Cannot find type for field ' + key);
    end;


    end;

    finally
      FreeAndNil(j);
    end;
end;

function TFieldsHelper.ToJson: string;
var
  I: Integer;
  key: string;
  ts: TSQLTimeStamp;
  MS: TMemoryStream;
  SS: TStringStream;
  AJSONObject:TJsonObject;
  it:TField;
begin
  result := '';
  AJSONObject := TJSONObject.Create;
  try
  for it in self do
  begin
    key := LowerCase(it.FieldName);
    if it.IsNull then
    begin
      AJSONObject.AddPair(key, TJSONNull.Create);
      Continue;
    end;

    case it.DataType of
      TFieldType.ftInteger, TFieldType.ftAutoInc, TFieldType.ftSmallint, TFieldType.ftShortint:
        AJSONObject.AddPair(key, TJSONNumber.Create(it.AsInteger));
      TFieldType.ftLargeint:
        begin
          AJSONObject.AddPair(key, TJSONNumber.Create(it.AsLargeInt));
        end;
      TFieldType.ftSingle, TFieldType.ftFloat:
        AJSONObject.AddPair(key, TJSONNumber.Create(it.AsFloat));
      ftWideString, ftMemo, ftWideMemo:
        AJSONObject.AddPair(key, it.AsWideString);
      ftString:
        AJSONObject.AddPair(key, it.AsString);
      TFieldType.ftDate:
        AJSONObject.AddPair(key, ISODateToString(it.AsDateTime));
      TFieldType.ftDateTime:
            AJSONObject.AddPair(key, ISODateTimeToString(it.AsDateTime));
      TFieldType.ftTimeStamp:
         begin
            ts := it.AsSQLTimeStamp;
            AJSONObject.AddPair(key, SQLTimeStampToStr('yyyy-mm-dd hh:nn:ss', ts));
        end;
      TFieldType.ftCurrency:
            AJSONObject.AddPair(key, TJSONNumber.Create(it.AsCurrency));
      TFieldType.ftBCD, TFieldType.ftFMTBcd:
            AJSONObject.AddPair(key, TJSONNumber.Create(BcdToDouble(it.AsBcd)));
      TFieldType.ftGraphic, TFieldType.ftBlob, TFieldType.ftStream:
          begin
            MS := TMemoryStream.Create;
            try
              TBlobField(it).SaveToStream(MS);
              MS.Position := 0;
              SS := TStringStream.Create('', TEncoding.ASCII);
              try
                EncodeStream(MS, SS);
                SS.Position := 0;
                AJSONObject.AddPair(key, SS.DataString);
              finally
                SS.Free;
              end;
            finally
              MS.Free;
            end;
        end;
     end;

      // else
      // raise EMapperException.Create('Cannot find type for field ' + key);
  end;
   result := AJSONObject.toJson;
  finally
    freeAndNil(AJSONObject);
  end;
end;




end.
