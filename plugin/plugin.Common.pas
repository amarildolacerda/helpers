unit plugin.Common;

interface

Uses System.Classes, System.SysUtils;

function ExpandConstant(const AConst: string): string; inline;
// like Inno Setup

implementation

function changeConstant(const AValue, msk: string): string; inline;
begin
  result := AValue;
  if sameText('{app}', msk) then
    result := StringReplace(result, msk, ExtractFileDir(ParamStr(0)),
      [rfReplaceAll]);

  if sameText('{home}', msk) then
    result := StringReplace(result, msk, GetHomePath, [rfReplaceAll]);

  if sameText('{curdir}', msk) or sametext('{src}',msk) then
    result := StringReplace(result, msk, GetCurrentDir, [rfReplaceAll]);


end;

function ExpandConstant(const AConst: string): string;
var
  aToken: string;
  b: boolean;
  n, I: integer;
begin
  result := AConst;
  n := pos('{', result);
  b := false;
  if n > 0 then
    for I := n to High(AConst) do
    begin
      case AConst[I] of
        '{':
          begin
            b := true;
            aToken := '{';
          end;
        '}':
          begin
            b := false;
            aToken := aToken + '}';
            result := changeConstant(result, aToken);
            aToken := '';
          end;
      else
        if b then
          aToken := aToken + AConst[I];
      end;
    end;
  result := ExpandFileName(result);
end;

end.
