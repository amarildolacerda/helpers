{***************************************************************************}
{                                                                           }
{           VSoft.Fluent.JSON                                               }
{                                                                           }
{           Copyright (C) 2011 Vincent Parrett                              }
{                                                                           }
{           http://www.finalbuilder.com                                     }
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

unit System.JSON.Fluent;

interface

type
  IFluentJSONBuilder = interface
  ['{9574F82E-B81D-49B9-AA04-60EB87C60E8B}']
    function AddObject : IFluentJSONBuilder;overload;
    function AddObject(const name : string) : IFluentJSONBuilder;overload;
    function AddNull(const name : string) : IFluentJSONBuilder;
    function AddString(const name : string; const value : string) : IFluentJSONBuilder;overload;
    function AddString(const value : string) : IFluentJSONBuilder;overload;
    function AddNumber(const name : string; const value : integer) : IFluentJSONBuilder;overload;
    function AddNumber(const name : string; const value : Double) : IFluentJSONBuilder;overload;
    function AddNumber(const value : integer) : IFluentJSONBuilder;overload;
    function AddNumber(const value : Double; const formatStr : string) : IFluentJSONBuilder;overload;
    function AddArray(const name : string) : IFluentJSONBuilder;overload;
    function AddArray : IFluentJSONBuilder;overload;
    function Up : IFluentJSONBuilder;
    function Mark : IFluentJSONBuilder;
    function Return : IFluentJSONBuilder;
    function ToString : string;
  end;

  //factory class
  TFluentJSON = class
    class function CreateJSONBuilder : IFluentJSONBuilder;
  end;

implementation

uses
  Generics.Collections,
  SysUtils;

type
  TJSONElementType = (etObject,etArray,etString,etInteger,etDouble,etBoolean,etNull);


  TJSONElement = class
  public
    Parent      : TJSONElement;
    ElementType : TJSONElementType;
    Members : TList<TJSONElement>;
    Name    : string;
    FormatStr: string;
    StringValue : string;
    Value: record
      case TJSONElementType of
        etBoolean: (BoolValue: boolean);
        etDouble: (DoubleValue: double);
        etInteger: (IntegerValue: Int64);
      end;

    function GetIndentLevel  : integer;
    function GetIndentString : string;
    constructor Create(const AElementType : TJSONElementType; const formatString : string = '');
    destructor Destroy;override;
    function JSONEscapeString(const value : string) : string;
    function ToString : string;override;
  end;


  TFluentJSONBuilder = class(TInterfacedObject,IFluentJSONBuilder)
  private
    FObjects : TList<TJSONElement>;
    FStack   : TStack<TJSONElement>;
    FCurrentElement : TJSONElement;
    FMarkedObjects  : TList<TJSONElement>;
  protected
    function AddObject : IFluentJSONBuilder;overload;
    function AddObject(const name : string) : IFluentJSONBuilder;overload;
    function AddNull(const name : string) : IFluentJSONBuilder;
    function AddString(const name : string; const value : string) : IFluentJSONBuilder;overload;
    function AddString(const value : string) : IFluentJSONBuilder;overload;
    function AddNumber(const name : string; const value : integer) : IFluentJSONBuilder;overload;
    function AddNumber(const name : string; const value : Double) : IFluentJSONBuilder;overload;
    function AddNumber(const value : integer) : IFluentJSONBuilder;overload;
    function AddNumber(const value : Double; const formatStr : string) : IFluentJSONBuilder;overload;
    function AddArray(const name : string) : IFluentJSONBuilder;overload;
    function AddArray : IFluentJSONBuilder;overload;
    function Up : IFluentJSONBuilder;
    function Mark : IFluentJSONBuilder;
    function Return : IFluentJSONBuilder;
    function ToString : string;override;
  public
    constructor Create;
    destructor Destroy;override;
  end;

{ TFluentJSON }

class function TFluentJSON.CreateJSONBuilder: IFluentJSONBuilder;
begin
  result := TFluentJSONBuilder.Create;
end;

{ TFluentJSONBuilder }

function TFluentJSONBuilder.AddArray(const name: string): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etObject,etArray]);
  newElement := TJSONElement.Create(etArray);
  newElement.Name := name;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
  begin
    FCurrentElement.Members.Add(newElement);
    FStack.Push(FCurrentElement);
  end;
  FCurrentElement := newElement;
  result := Self;
end;



function TFluentJSONBuilder.AddNumber(const name: string; const value: Double): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etObject,etArray]);
  newElement := TJSONElement.Create(etDouble);
  newElement.Name := name;
  newElement.Value.DoubleValue := value;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
    FCurrentElement.Members.Add(newElement);
  result := Self;
end;

function TFluentJSONBuilder.AddObject: IFluentJSONBuilder;
begin
  result := AddObject('');
end;

function TFluentJSONBuilder.AddNumber(const name : string; const value: Integer): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etObject,etArray]);
  newElement := TJSONElement.Create(etInteger);
  newElement.Name := name;
  newElement.Value.IntegerValue := value;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
    FCurrentElement.Members.Add(newElement);
  result := Self;
end;

function TFluentJSONBuilder.AddNumber(const value: integer): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etArray]);
  newElement := TJSONElement.Create(etInteger);
  newElement.Name := '';
  newElement.Value.IntegerValue := value;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
    FCurrentElement.Members.Add(newElement);
  result := Self;
end;

function TFluentJSONBuilder.AddArray: IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etArray]);
  newElement := TJSONElement.Create(etArray);
  newElement.Name := '';
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
  begin
    FCurrentElement.Members.Add(newElement);
    FStack.Push(FCurrentElement);
  end;
  FCurrentElement := newElement;
  result := Self;
end;

function TFluentJSONBuilder.AddNull(const name: string): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etObject,etArray]);
  newElement := TJSONElement.Create(etNull);
  newElement.Name := name;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
    FCurrentElement.Members.Add(newElement);
  result := Self;
end;

function TFluentJSONBuilder.AddNumber(const value: Double; const formatStr: string): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etArray]);
  newElement := TJSONElement.Create(etInteger);
  newElement.Name := '';
  newElement.Value.DoubleValue := value;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
    FCurrentElement.Members.Add(newElement);
  result := Self;
end;

function TFluentJSONBuilder.AddObject(const name: string): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  newElement := TJSONElement.Create(etObject);
  newElement.Parent := FCurrentElement;
  newElement.Name := name;
  if FCurrentElement = nil then
    FObjects.Add(newElement)
  else
  begin
    FCurrentElement.Members.Add(newElement);
    FStack.Push(FCurrentElement);
  end;
  FCurrentElement := newElement;
  result := Self;
end;

function TFluentJSONBuilder.AddString(const value: string): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etArray]);
  newElement := TJSONElement.Create(etString);
  newElement.Name := '';
  newElement.StringValue := value;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
    FCurrentElement.Members.Add(newElement);
  result := Self;
end;

function TFluentJSONBuilder.AddString(const name, value: string): IFluentJSONBuilder;
var
  newElement : TJSONElement;
begin
  Assert(FCurrentElement <> nil);
  Assert(FCurrentElement.ElementType in [etObject,etArray]);
  newElement := TJSONElement.Create(etString);
  newElement.Name := name;
  newElement.StringValue := value;
  newElement.Parent := FCurrentElement;
  if FCurrentElement <> nil then
    FCurrentElement.Members.Add(newElement);
  result := Self;
end;

constructor TFluentJSONBuilder.Create;
begin
  FObjects := TList<TJSONElement>.Create;
  FStack   := TStack<TJSONElement>.Create;
  FMarkedObjects  := TList<TJSONElement>.Create;
  FCurrentElement := nil;
end;

destructor TFluentJSONBuilder.Destroy;
var
  element : TJSONElement;
begin
  for element in FObjects do
  begin
    element.Free;
  end;
  FObjects.Free;
  FStack.Free;
  inherited;
end;

function TFluentJSONBuilder.Mark: IFluentJSONBuilder;
begin
  result := Self;
  Assert(FCurrentElement <> nil);
  FMarkedObjects.Add(FCurrentElement);
end;

function TFluentJSONBuilder.Return: IFluentJSONBuilder;
begin
  result := Self;
  Assert(FMarkedObjects.Count > 0);
  FCurrentElement := FMarkedObjects.Last;
end;

function TFluentJSONBuilder.ToString: string;
var
  element : TJSONElement;
begin
  for element in FObjects do
    result := result + element.ToString;
end;

function TFluentJSONBuilder.Up: IFluentJSONBuilder;
begin
  if FStack.Count > 0 then
    FCurrentElement := FStack.Pop
  else
    FCurrentElement := nil;
  result := Self;

end;

{ TJSONElement }

constructor TJSONElement.Create(const AElementType: TJSONElementType; const formatString : string = '');
begin
  ElementType := AElementType;
  if ElementType in [etObject,etArray] then
    Members := TList<TJSONElement>.Create
  else
    Members := nil;
  formatStr := formatString;
end;

destructor TJSONElement.Destroy;
var
  element : TJSONElement;
begin
  if Members <> nil then
  begin
    for element in Members do
    begin
      element.Free;
    end;
    Members.Free;
  end;
  inherited;
end;

function TJSONElement.GetIndentLevel: integer;
var
  parentElement : TJSONElement;
begin
  result := 0;
  parentElement := Self.Parent;
  while parentElement <> nil do
  begin
    Inc(result);
    parentElement := parentElement.Parent;
  end;
end;

function TJSONElement.GetIndentString: string;
var
  count : integer;
begin
  count := GetIndentLevel * 2;
  if count > 0 then
    result := StringOfChar(' ',count);
end;

function TJSONElement.JSONEscapeString(const value: string): string;
var
  c : Char;
  i : integer;
  count : integer;
begin
  result := '';
  count := Length(value);
  for i := 1 to count do
  begin
    c := value[i];
    case c of
      '"' : result := result + '\"';
      '\' : result := result + '\\';
      '/' : result := result + '\/';
      #8 : result := result + '\b';
      #9 : result := result + '\t';
      #10 : result := result + '\n';
      #12 : result := result + '\f';
      #13 : result := result + '\r';
    else
    //TODO : Deal with unicode characters properly!
      result := result + c;
    end;
  end;
end;

function TJSONElement.ToString: string;
var
  member : TJSONElement;
  i      : integer;
  sIndent : string;
begin
  sIndent := GetIndentString;
  result := '';
  case ElementType of
    etObject:
    begin
       if Parent <> nil then
         result := sIndent +  '"' + JSONEscapeString(Self.Name) + '":';

       result := result + '{';
       if Members.Count > 0 then
       begin
        result := result + #13#10;
        for i := 0 to Self.Members.Count - 1 do
        begin
          member := Self.Members[i];
          result := result + member.ToString;
          if i < Self.Members.Count - 1 then
            result := result + ',';
          result := result + #13#10;
        end;
       end;
       result := result + sIndent + '}';
    end;
    etArray:
    begin
       if Parent <> nil then
       begin
          case parent.ElementType of
            etObject: result := result + sIndent + '"' + JSONEscapeString(Self.Name) + '":[';
            etArray: result := result + sIndent + '[';
          end;
       end;
       if Members.Count > 0 then
       begin
        for i := 0 to Members.Count - 1 do
        begin
          member := Members[i];
          if i > 0 then
            result := result + ',' ;
          result := result + member.ToString;
        end;
       end;
       result := result + ']';
    end;
    etString:
    begin
      if ((Self.Parent <> nil) and (Self.Parent.ElementType = etArray)) or (Self.Name = '') then
        result := '"' + JSONEscapeString(Self.StringValue) + '"'
      else
        result := sIndent + '"' + JSONEscapeString(Self.Name) + '":"' + JSONEscapeString(Self.StringValue) + '"';
    end;
    etInteger:
    begin
      if ((Self.Parent <> nil) and (Self.Parent.ElementType = etArray)) or (Self.Name = '') then
        result := IntToStr(Self.Value.IntegerValue)
      else
        result := sIndent + '"' + JSONEscapeString(Self.Name) + '":' + IntToStr(Self.Value.IntegerValue);
    end;
    etDouble:
    begin
      if ((Self.Parent <> nil) and (Self.Parent.ElementType = etArray)) or (Self.Name = '') then
        result := FloatToStr(Self.Value.DoubleValue)
      else
        result := sIndent + '"' + JSONEscapeString(Self.Name) + '":' + FloatToStr(Self.Value.DoubleValue);
    end;
    etBoolean:
    begin
      if ((Self.Parent <> nil) and (Self.Parent.ElementType = etArray)) or (Self.Name = '') then
        result := LowerCase(BoolToStr(Self.Value.BoolValue,true))
      else
        result := sIndent + '"' + JSONEscapeString(Self.Name) + '":' + LowerCase(BoolToStr(Self.Value.BoolValue,true));
    end;
    etNull :
    begin
      if ((Self.Parent <> nil) and (Self.Parent.ElementType = etArray)) or (Self.Name = '') then
        result := 'null'
      else
        result := sIndent + '"' + JSONEscapeString(Self.Name) + '":null';
    end;
  end;
end;

end.

