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


unit Data.JvObjectDataSet;

interface

uses System.Classes, JvMemoryDataSet, System.Rtti, Data.DB,
  System.SysUtils, System.Generics.Collections, System.Contnrs;

type


  TJvObjectListEvent = procedure(sender:TObject; Action: TListNotification) of object;

  TJvObjectListEventing = class(TObjectList)
  private
    FOnAddEvent: TJvObjectListEvent;
    procedure SetOnAddEvent(const Value: TJvObjectListEvent);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
     public
        property OnNotifyEvent:TJvObjectListEvent read FOnAddEvent write SetOnAddEvent;
  end;


  TJvObjectDataSet = class(TJvMemoryData)

  private

    FNotifyControls: integer;
    FObjectClass: TClass;
    FStringMax: integer;
    FObjectList: TJvObjectListEventing;
    FObjectListOwned: Boolean;
    FObjectClassName: string;
    procedure InternalInitFieldDefsObjectClass;
    procedure InternalDelete; override;
    procedure InternalAddRecord(Buffer: TJvRecordBuffer;
      Append: Boolean); override;
    procedure InternalInsert; override;
    procedure InternalPost; override;
    procedure InternalEdit; override;
    procedure DoAfterEdit; override;
    procedure DoAddToObjectListEvent(Sender: TObject;
      action: TListNotification);

    procedure SetObjectClass(const Value: TClass);
    procedure SetStringMax(const Value: integer);
    procedure InternalSetToRecord(Buffer: TRecBuf); overload; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer;
      NativeFormat: Boolean); overload; override;
    procedure FieldToObject(LRow: integer);
    procedure ObjectToField(LRow: integer);
    procedure SetOwnsObjects(const Value: Boolean);
    function GetOwnsObjects: Boolean;
    procedure SetObjectClassName(const Value: string);
    procedure SetObjectList(const Value: TJvObjectListEventing);

  public
    constructor create(AOwner: TComponent); overload; override;
    constructor create(AOwnder: TComponent; AClass: TClass); overload;
    destructor destroy; override;
    procedure DisableListControls;
    procedure EnableListControls;
    procedure Reopen;
  published
    property ObjectClass: TClass read FObjectClass write SetObjectClass;
    property ObjectList: TJvObjectListEventing read FObjectList
      write SetObjectList;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
    property ObjectClassName: string read FObjectClassName
      write SetObjectClassName;
    property StringWidth: integer read FStringMax write SetStringMax;

  end;


  TObjectDataSet = TJvObjectDataSet;

implementation

constructor TJvObjectDataSet.create(AOwner: TComponent);
begin
  inherited;
  FStringMax := 255;
  FObjectList := TJvObjectListEventing.create;
  FObjectListOwned := true;
  FObjectList.OnNotifyEvent := DoAddToObjectListEvent;
end;

constructor TJvObjectDataSet.create(AOwnder: TComponent; AClass: TClass);
begin
  create(AOwnder);
  ObjectClass := AClass;
end;

destructor TJvObjectDataSet.destroy;
begin
  FObjectList.OnNotifyEvent := nil;
  if FObjectListOwned then
    FreeAndNil(FObjectList);
  inherited;
end;

procedure TJvObjectDataSet.DoAfterEdit;
var
  LRow: integer;
begin
  inherited;
  LRow := GetRecNo - 1;
  if (LRow >= 0) and (LRow < FObjectList.Count) then
    ObjectToField(LRow);

end;

procedure TJvObjectDataSet.DoAddToObjectListEvent(Sender: TObject;
  action: TListNotification);
var
  LRow: integer;
begin
  if (FNotifyControls > 0) then
    exit;

  if state in dsEditModes then
    post;
  DisableListControls;
  try
    insert;
    LRow := GetRecNo - 1;
    if (LRow >= 0) and (LRow < FObjectList.Count) then
      FieldToObject(LRow);
  finally
    EnableListControls;
  end;

end;

procedure TJvObjectDataSet.FieldToObject(LRow: integer);
var
  LContext: TRttiContext;
  obj: TObject;
  LTypes: TRttiType;
  LProp: TRttiProperty;
  fld: TField;
  LVal: TValue;
begin
  LContext := TRttiContext.create;
  try
    obj := FObjectList.Items[LRow];
    LTypes := LContext.GetType(obj.ClassType);
    for LProp in LTypes.GetProperties do
    begin
      fld := Fields.FindField(LProp.Name);
      if fld <> nil then
      begin
        LVal := TValue.From<variant>(fld.Value);
        LProp.SetValue(obj, LVal);
      end;
    end;
  finally
    LContext.Free;
  end;
end;

function TJvObjectDataSet.GetOwnsObjects: Boolean;
begin
  result := FObjectList.OwnsObjects;
end;

procedure TJvObjectDataSet.InternalAddRecord(Buffer: TJvRecordBuffer;
  Append: Boolean);
begin
  inherited;
end;

procedure TJvObjectDataSet.InternalDelete;
var
  LRow: integer;
begin
  LRow := GetRecNo - 1;
  inherited;

  if (LRow >= 0) and (LRow < FObjectList.Count) then
    FObjectList.Delete(LRow);

end;

procedure TJvObjectDataSet.InternalEdit;
begin
  inherited;
end;

procedure TJvObjectDataSet.InternalInitFieldDefsObjectClass;
var
  LContext: TRttiContext;
  LRttiType: TRttiType;
  LRttiProp: TRttiProperty;
  FType: TFieldType;
  FSize: integer;
begin
  LContext := TRttiContext.create();
  try
    LRttiType := LContext.GetType(FObjectClass);
    for LRttiProp in LRttiType.GetProperties do
    begin
      FType := ftString;
      FSize := FStringMax;

      case LRttiProp.PropertyType.TypeKind of
        tkInteger, tkInt64:
          begin
            FType := ftInteger;
            FSize := 0;
          end;
        tkFloat:
          begin
            FSize := 0;
            if LRttiProp.PropertyType.Name.Equals('TDateTime') then
              FType := ftDateTime
            else if LRttiProp.PropertyType.Name.Equals('TDate') then
              FType := ftDate
            else if LRttiProp.PropertyType.Name.Equals('TTime') then
              FType := ftTime
            else if LRttiProp.PropertyType.Name.Equals('Currency') then
              FType := ftCurrency
            else
              FType := ftFloat;
          end;
        tkVariant:
          FType := ftVariant;
      end;
      fieldDefs.Add(LRttiProp.Name, FType, FSize);
    end;
  finally
    LContext.Free;
  end;

end;

procedure TJvObjectDataSet.InternalInsert;
var
  LRow: integer;
begin
  inherited;
  if FNotifyControls = 0 then
  begin
    DisableListControls;
    try
      FObjectList.Add(FObjectClass.create);
      LRow := GetRecNo - 1;
      if (LRow >= 0) and (LRow < FObjectList.Count) then
        FieldToObject(LRow);
    finally
      EnableListControls;
    end;
  end;
end;

procedure TJvObjectDataSet.InternalPost;
var
  LRow: integer;
begin
  inherited;
  LRow := GetRecNo - 1;
  if (LRow >= 0) and (LRow < FObjectList.Count) then
  begin
    FieldToObject(LRow);
  end;
end;

procedure TJvObjectDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
  inherited InternalSetToRecord(Buffer);
end;

procedure TJvObjectDataSet.ObjectToField(LRow: integer);
var
  LNome: string;
  obj: TObject;
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LVar: TValue;
  fld: TField;
begin
  if (LRow >= 0) and (LRow < FObjectList.Count) then
  begin
    LContext := TRttiContext.create;
    try
      obj := FObjectList.Items[LRow];
      LType := LContext.GetType(obj.ClassType);
      for LProp in LType.GetProperties do
      begin
        fld := Fields.FindField(LProp.Name);
        if fld <> nil then
        begin
          LVar := LProp.GetValue(obj);
          fld.Value := LVar.AsVariant;
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

procedure TJvObjectDataSet.Reopen;
begin
  close;
  open;
end;

procedure TJvObjectDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer;
  NativeFormat: Boolean);
begin
  inherited;
end;

procedure TJvObjectDataSet.SetObjectClass(const Value: TClass);
begin
  FObjectClass := Value;
  InternalInitFieldDefsObjectClass;
end;

procedure TJvObjectDataSet.SetObjectClassName(const Value: string);
begin
  FObjectClassName := Value;
  ObjectClass := FindClass(Value);
end;

procedure TJvObjectDataSet.SetObjectList(const Value: TJvObjectListEventing);
begin
  FObjectList.OnNotifyEvent := nil;
  FreeAndNil(FObjectList);
  FObjectList := Value;
  FObjectList.OnNotifyEvent := DoAddToObjectListEvent;
  FObjectListOwned := false;
end;

procedure TJvObjectDataSet.SetOwnsObjects(const Value: Boolean);
begin
  FObjectList.OwnsObjects := Value;
end;

procedure TJvObjectDataSet.SetStringMax(const Value: integer);
begin
  FStringMax := Value;
end;

{ TObjectListChangeEvent }

procedure TJvObjectDataSet.DisableListControls;
begin
  inc(FNotifyControls);
end;

procedure TJvObjectDataSet.EnableListControls;
begin
  dec(FNotifyControls);
  if FNotifyControls < 0 then
    FNotifyControls := 0;
end;


{ TObjectListEventing }

procedure TJvObjectListEventing.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if assigned(FOnAddEvent) then
     FOnAddEvent(self,Action);
end;

procedure TJvObjectListEventing.SetOnAddEvent(const Value: TJvObjectListEvent);
begin
  FOnAddEvent := Value;
end;


end.
