{
   Amarildo Lacerda  (09/02/2016)
   Usando Record Helper

}
unit System.SysUtils.Helpers;

interface

uses System.SysUtils;


type
     TDoubleHelper = record helper for double
     public
         function Add( n:double) : double;
         function ToString:String;
         function ToInteger:Integer;
         function ToDateTime:TDateTime;
         function ToISODatetime :string;
         procedure FromISODatetime( DateTimeAsString:string );
         function ToTime:TTime;
         function ToISOTime:String;
         procedure FromISOTime(ATime:String);
     end;

     TStringHelper = record helper for string
     public
         function ToFloat:Double;
         function ToDateTime:TDatetime;
         function ToInteger:Integer;
     end;



implementation


uses System.DateUtils;



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

function TDoubleHelper.ToString: String;
begin
   result := FloatToStr(self);
end;

function TDoubleHelper.ToTime: TTime;
begin
   result := TTime(self);
end;

{ TStringHelper }

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
