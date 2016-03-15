{***************************************************************************}
{                                                                           }
{           VCL.Menus.Helpers                                                  }
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


unit VCL.Menus.Helpers;

interface

uses VCL.Menus, System.SysUtils, System.Classes;

Type

  TMenuItemAnonimous = class(TMenuItem)
  private
    FProc: TProc;
    procedure DoClick(Sender: TObject);
  public
    constructor create(AOwner: TComponent; AProc: TProc);
  end;

  TMenuItemHelper = class helper for TMenuItem
  public
    function CreateAnonimous(ACaption: string; AProc: TProc)
      : TMenuItemAnonimous; overload;
    function CreateAnonimous(AProc: TProc): TMenuItemAnonimous; overload;
  end;

implementation

function TMenuItemHelper.CreateAnonimous(AProc: TProc): TMenuItemAnonimous;
begin
  result := TMenuItemAnonimous.create(self, AProc);
  Add(result);
end;

function TMenuItemHelper.CreateAnonimous(ACaption: string; AProc: TProc)
  : TMenuItemAnonimous;
begin
  result := CreateAnonimous(AProc);
  result.Caption := ACaption;
end;

procedure TMenuItemAnonimous.DoClick(Sender: TObject);
begin
  if assigned(FProc) then
    FProc;
end;

constructor TMenuItemAnonimous.create(AOwner: TComponent; AProc: TProc);
begin
  inherited create(AOwner);
  OnClick := DoClick;
  FProc := AProc;
end;

end.
