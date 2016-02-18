{***************************************************************************}
{                                                                           }
{           Records Helper                                                  }
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

unit System.SysUtils.Helper;

interface

uses System.SysUtils, System.Types, System.DateUtils;


type
     TDoubleHelper = record helper for double
     public
         function Add( n:double) : double;
         function ToString:String;overload;
         function ToString(AFormat:string):string;overload;
         function ToInteger:Integer;
         function ToDateTime:TDateTime;
         function ToISODatetime :string;
         procedure FromISODatetime( DateTimeAsString:string );
         function ToTime:TTime;
         function ToISOTime:String;
         procedure FromISOTime(ATime:String);
         procedure FromString( AString:String;ADef:double);overload;
         procedure FromString( AString:String);overload;
     end;

     TStringHelper = record helper for string
     public
         function ToFloat:Double;
         function ToDateTime:TDatetime;
         function ToInteger:Integer;
         procedure FromFloat( Value:Double );overload;
         procedure FromFloat(AFormat:string;Value:Double);overload;
     end;


implementation



function ISOTimeToString(ATime: TTime): string;
var
  fs: TFormatSettings;
begin
  fs.TimeSeparator := ':';
  Result := FormatDateTime('hh:nn:ss', ATime, fs);
end;

function ISODateToString(ADate: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DD', ADate);
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
    StrToInt(Copy(DateTimeAsString, 6, 2)), StrToInt(Copy(DateTimeAsString, 9, 2)),
    StrToInt(Copy(DateTimeAsString, 12, 2)), StrToInt(Copy(DateTimeAsString, 15, 2)),
    StrToInt(Copy(DateTimeAsString, 18, 2)), 0);
end;

function ISOStrToTime(TimeAsString: string): TTime;
begin
  Result := EncodeTime(StrToInt(Copy(TimeAsString, 1, 2)), StrToInt(Copy(TimeAsString, 4, 2)),
    StrToInt(Copy(TimeAsString, 7, 2)), 0);
end;

function ISOStrToDate(DateAsString: string): TDate;
begin
  Result := EncodeDate(StrToInt(Copy(DateAsString, 1, 4)), StrToInt(Copy(DateAsString, 6, 2)),
    StrToInt(Copy(DateAsString, 9, 2)));
end;


{ TDoubleHelper }

function TDoubleHelper.Add(n: double): double;
begin
  self := self + n;
  result := self;
end;

procedure TDoubleHelper.fromISODatetime(DateTimeAsString: string);
begin
    self :=  ISOStrToDateTime(DateTimeAsString);
end;

procedure TDoubleHelper.FromISOTime(ATime: String);
begin
   self := ISOStrToTime(ATime);
end;

procedure TDoubleHelper.FromString(AString: String);
begin
   self := StrToFloat(AString);
end;

procedure TDoubleHelper.FromString(AString: String; ADef:double);
begin
   self := StrToFloatDef(AString,ADef);
end;

function TDoubleHelper.ToDateTime: TDateTime;
begin
   result := self;
end;

function TDoubleHelper.ToInteger: Integer;
begin
  result := Round(self);
end;

function TDoubleHelper.toISODatetime: string;
begin
   result := ISODateTimeToString(self)
end;

function TDoubleHelper.ToISOTime: String;
begin
   result := ISOTimeToString( TTime(self) );
end;

function TDoubleHelper.ToString(AFormat: string): string;
begin
  result := System.SysUtils.FormatFloat(AFormat,self);
end;

function TDoubleHelper.ToString: String;
begin
   result := FloatToStr(self);
end;

function TDoubleHelper.ToTime: TTime;
begin
   result := TTime(self);
end;

{ TStringHelper }

procedure TStringHelper.FromFloat(Value: Double);
begin
  self := FloatToStr(Value);
end;

procedure TStringHelper.FromFloat(AFormat: string; Value: Double);
begin
    self := FormatFloat(AFormat,Value)
end;

function TStringHelper.toDateTime: TDatetime;
begin
  result := StrToDateTime(self);
end;

function TStringHelper.ToFloat: Double;
begin
   result := StrToFloat(self);
end;

function TStringHelper.ToInteger: Integer;
begin
   result := StrToInt(self);
end;


end.
