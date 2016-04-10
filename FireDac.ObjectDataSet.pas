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

unit FireDac.ObjectDataSet;

interface

uses System.Classes, System.Rtti, Data.DB,
  System.Generics.Collections, System.TypInfo,
  FireDac.Stan.Intf, FireDac.Stan.Option,
  FireDac.Stan.Param, FireDac.Stan.Error, FireDac.DatS, FireDac.Phys.Intf,
  FireDac.DApt.Intf, FireDac.Comp.DataSet, FireDac.Comp.Client,
  System.SysUtils,  System.Contnrs;

type

  TObjectListEvent = procedure(sender: TObject; Action: TListNotification)
    of object;

  TObjectListEventing = class(TObjectList)
  private
    FOnAddEvent: TObjectListEvent;
    procedure SetOnAddEvent(const Value: TObjectListEvent);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    property OnNotifyEvent: TObjectListEvent read FOnAddEvent
      write SetOnAddEvent;
  end;

  TObjectDataSet = class(TFDMemTable)
  private
    FActiveRow:integer;
    FNotifyControls: integer;
    FObjectClass: TClass;
    FStringMax: integer;
    FObjectList: TObjectListEventing;
    FObjectListOwned: Boolean;
    FObjectClassName: string;
    procedure InternalInitFieldDefsObjectClass;
    procedure InternalDelete; override;
    procedure DoBeforeInsert;override;
    procedure InternalInsert; override;
    procedure InternalPost; override;
    procedure InternalEdit; override;
    procedure DoAfterEdit; override;
    procedure DoAfterInsert;override;
    procedure InternalClose;override;
    procedure DoAddToObjectListEvent(sender: TObject;
      Action: TListNotification);
    function GetRecNo: Integer; override;


    procedure SetObjectClass(const Value: TClass);
    procedure SetStringMax(const Value: integer);
    procedure InternalSetToRecord(Buffer: TRecBuf); overload; override;

    procedure SetOwnsObjects(const Value: Boolean);
    function GetOwnsObjects: Boolean;
    procedure SetObjectClassName(const Value: string);
    procedure SetObjectList(const Value: TObjectListEventing);

  public
    constructor create(AOwner: TComponent); overload; override;
    constructor create(AOwnder: TComponent; AClass: TClass); overload;
    destructor destroy; override;
    procedure FieldToObject(LRow: integer);overload;
    procedure FieldToObject(Obj:TObject);overload;
    procedure ObjectToField(LRow: integer);overload;
    procedure ObjectToField(Obj:TObject);overload;
    procedure DisableListControls;
    procedure EnableListControls;
    procedure Reopen;
  published
    procedure LoadFromList( AList:TList );overload;virtual;
    procedure SaveToList( AList:TList );overload;virtual;
    property ObjectClass: TClass read FObjectClass write SetObjectClass;
    property ObjectList: TObjectListEventing read FObjectList
      write SetObjectList;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
    property ObjectClassName: string read FObjectClassName
      write SetObjectClassName;
    property StringWidth: integer read FStringMax write SetStringMax;

  end;


  TGenericObjectDataset<T: Class> = class(TObjectDataset)
  private
    function GetItems(idx: Integer): T;
    procedure SetItems(idx: Integer; const Value: T);
    public
      constructor create(AOwner:TComponent);override;
      procedure LoadFromList( AList:TList<T> );overload;
      procedure SaveToList( AList:TList<T> );overload;
      function Count:integer;
      property Items[idx:Integer]:T  read GetItems write SetItems;
  end;


implementation

constructor TObjectDataSet.create(AOwner: TComponent);
begin
  inherited;
  FStringMax := 255;
  FObjectList := TObjectListEventing.create(true);
  FObjectListOwned := true;
  FObjectList.OnNotifyEvent := DoAddToObjectListEvent;
end;

constructor TObjectDataSet.create(AOwnder: TComponent; AClass: TClass);
begin
  create(AOwnder);
  ObjectClass := AClass;
end;

destructor TObjectDataSet.destroy;
begin
  FObjectList.OnNotifyEvent := nil;
  if FObjectListOwned then
    FreeAndNil(FObjectList);
  inherited;
end;

procedure TObjectDataSet.DoAfterEdit;
var
  LRow: integer;
begin
  inherited;
  LRow := GetRecNo - 1;
  if (LRow >= 0) and (LRow < FObjectList.Count) then
    ObjectToField(LRow);

end;

procedure TObjectDataSet.DoAfterInsert;
begin
  inherited;
end;

procedure TObjectDataSet.DoBeforeInsert;
begin
  last;
  inherited;
end;

procedure TObjectDataSet.DoAddToObjectListEvent(sender: TObject;
  Action: TListNotification);
var
  LRow: integer;
begin
  if (FNotifyControls > 0) then
    exit;

  case Action of
    lnAdded:
      begin
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
    lnExtracted:
      ;
    lnDeleted:
      ;
  end;

end;

procedure TObjectDataSet.FieldToObject(LRow: integer);
var obj:TObject;
begin
    obj := FObjectList.Items[LRow];
    FieldToObject(obj);
end;

function TObjectDataSet.GetOwnsObjects: Boolean;
begin
  result := FObjectList.OwnsObjects;
end;

function TObjectDataSet.GetRecNo: Integer;
begin
   if state in [dsInsert] then
     result := FActiveRow +1
   else
     result := inherited GetRecNo;
end;

procedure TObjectDataSet.InternalClose;
begin
  inherited;
  if assigned(FObjectList) then
     FObjectList.Clear;
end;

procedure TObjectDataSet.InternalDelete;
var
  LRow: integer;
begin
  LRow := GetRecNo - 1;
  inherited;

  if (LRow >= 0) and (LRow < FObjectList.Count) then
    FObjectList.Delete(LRow);

end;

procedure TObjectDataSet.InternalEdit;
begin
  inherited;
end;

procedure TObjectDataSet.InternalInitFieldDefsObjectClass;
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

procedure TObjectDataSet.InternalInsert;
begin
  inherited;
  if FNotifyControls = 0 then
  begin
    DisableListControls;
    try
      FObjectList.Add(FObjectClass.create);
      FActiveRow := FObjectList.Count-1;
      if (FActiveRow >= 0) and (FActiveRow < FObjectList.Count) then
         FieldToObject(FActiveRow);
    finally
      EnableListControls;
    end;
  end;
end;

procedure TObjectDataSet.InternalPost;
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

procedure TObjectDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
  inherited InternalSetToRecord(Buffer);
end;

procedure TObjectDataSet.LoadFromList(AList: TList);
  var obj:TObject;
      nObj:TObject;
      i:integer;
begin
   if AList.Count=0 then exit;  // nao tem nada para ler
   try
   try
   if active then
      EmptyDataSet;
   close;
   FieldDefs.Clear;
   obj := AList.Items[0];
   ObjectClass := obj.ClassType;
   open;
   for I := 0 to AList.Count-1 do
     begin
       obj := AList.Items[i];
       append;
       ObjectToField(obj);
       Post;
     end;
   finally
   end;
   finally
      Resync([]);
   end;
end;



procedure TObjectDataSet.ObjectToField(Obj: TObject);
var
  LNome: string;
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LVar: TValue;
  fld: TField;
begin
    LContext := TRttiContext.create;
    try
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

procedure TObjectDataSet.ObjectToField(LRow: integer);
var obj:TObject;
begin
  if (LRow >= 0) and (LRow < FObjectList.Count) then
  begin
      obj := FObjectList.Items[LRow];
      ObjectToField(obj);
  end;
end;

procedure TObjectDataSet.Reopen;
begin
  close;
  open;
end;

procedure TObjectDataSet.SaveToList(AList: TList);
var i:integer;
   obj:TObject;
   book:TBookmark;
   OldRow:Integer;
begin
   DisableControls;
   try
   oldRow := GetRecNo;
   AList.Clear;
   first;
   while eof=false do
   begin
     obj := FObjectClass.Create;
     FieldToObject(obj);
     AList.Add(obj);
     next;
   end;
   finally
     SetRecNo(oldRow);
     EnableControls;
   end;
end;

procedure TObjectDataSet.SetObjectClass(const Value: TClass);
begin
  FObjectClass := Value;
  InternalInitFieldDefsObjectClass;
end;

procedure TObjectDataSet.SetObjectClassName(const Value: string);
begin
  FObjectClassName := Value;
  ObjectClass := FindClass(Value);
end;

procedure TObjectDataSet.SetObjectList(const Value: TObjectListEventing);
begin
  FObjectList.OnNotifyEvent := nil;
  FreeAndNil(FObjectList);
  FObjectList := Value;
  FObjectList.OnNotifyEvent := DoAddToObjectListEvent;
  FObjectListOwned := false;
end;

procedure TObjectDataSet.SetOwnsObjects(const Value: Boolean);
begin
  FObjectList.OwnsObjects := Value;
end;

procedure TObjectDataSet.SetStringMax(const Value: integer);
begin
  FStringMax := Value;
end;

{ TObjectListChangeEvent }

procedure TObjectDataSet.DisableListControls;
begin
  inc(FNotifyControls);
end;

procedure TObjectDataSet.EnableListControls;
begin
  dec(FNotifyControls);
  if FNotifyControls < 0 then
    FNotifyControls := 0;
end;

procedure TObjectDataSet.FieldToObject(Obj: TObject);
var
  LContext: TRttiContext;
  LTypes: TRttiType;
  LProp: TRttiProperty;
  fld: TField;
  LVal: TValue;
begin
  LContext := TRttiContext.create;
  try
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

{ TObjectListEventing }

procedure TObjectListEventing.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if assigned(FOnAddEvent) then
    FOnAddEvent(self, Action);
end;

procedure TObjectListEventing.SetOnAddEvent(const Value: TObjectListEvent);
begin
  FOnAddEvent := Value;
end;

{ TGenericObjectDataset<T> }

function TGenericObjectDataset<T>.Count: integer;
begin
   result := FObjectList.Count;
end;


constructor TGenericObjectDataset<T>.create(AOwner: TComponent);
begin
  inherited;
  ObjectClass := T;
end;

function TGenericObjectDataset<T>.GetItems(idx: Integer): T;
var LVal:TValue;
    obj:TObject;
    ct:TClass;
begin
   obj := FObjectList.Items[idx];
   result := T(obj);
end;

procedure TGenericObjectDataset<T>.LoadFromList(AList: TList<T>);
  var obj:TObject;
      nObj:TObject;
      i:integer;
begin
   if AList.Count=0 then exit;  // nao tem nada para ler
   try
   try
   if active then
      EmptyDataSet;
   close;
   FieldDefs.Clear;
   obj := AList.Items[0];
   ObjectClass := obj.ClassType;
   open;
   for I := 0 to AList.Count-1 do
     begin
       obj := AList.Items[i];
       append;
       ObjectToField(obj);
       Post;
     end;
   finally
   end;
   finally
      Resync([]);
   end;
end;

procedure TGenericObjectDataset<T>.SaveToList(AList: TList<T>);
var obj:TObject;
    oldRow:Integer;
begin
    AList.clear;
    oldRow := GetRecNo;
    DisableControls;
    try
    first;
    while eof=false do
    begin
      obj := FObjectClass.Create;
      FieldToObject(obj);
      AList.Add(obj);
      next;
    end;
    finally
      SetRecNo(oldRow);
      EnableControls;
    end;
end;

procedure TGenericObjectDataset<T>.SetItems(idx: Integer; const Value: T);
begin
    FObjectList.SetItem(idx,Value);
end;

end.
