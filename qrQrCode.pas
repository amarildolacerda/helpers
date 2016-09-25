
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


{
  25/09/2016
  AL - Torna publico procedure para QrCodeToCanvas



}


unit qrQrCode;

{$define usa_acbr}

interface

uses System.Classes, System.SysUtils, Vcl.Graphics, Vcl.ExtCtrls, QuickRpt,
  Data.DB, QRCtrls;

Type

  TqrQRCodeBase = class(TQRImage)
  private
    FText: string;
    procedure SetText(const Value: string);
    procedure Update; override;
  public
    property Text: string read FText write SetText;
  published
  end;


  TqrQRCode = class(TqrQRCodeBase)
    published
      property Text;
  end;

  TqrDBQRCode = class(TqrQRCodeBase)
  private
    FDataset: TDataset;
    FDatafield: string;
    procedure SetDatafield(const Value: string);
    procedure SetDataset(const Value: TDataset);
    procedure CreateQrCode;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Print(OfsX, OfsY: integer); override;

  published
    property Datafield: string read FDatafield write SetDatafield;
    property Dataset: TDataset read FDataset write SetDataset;
  end;

procedure QrCodeToCanvas(AWidth, AHeight: integer; ATexto: String;
  ACanvas: TCanvas);


procedure Register;

implementation

uses {$IFDEF usa_acbr} ACBrDelphiZXingQRCode {$ELSE} DelphiZXingQRCode
{$ENDIF};
// se nao usa ACBR, pode usar o component original :   https://github.com/debenu/DelphiZXingQRCode/

procedure Register;
begin
  RegisterComponents('QReport', [TqrQRCode, TqrDBQRCode]);
end;

procedure QrCodeToCanvas(AWidth, AHeight: integer; ATexto: String;
  ACanvas: TCanvas);
var
  bitmap: TBitmap;
  qr: TDelphiZXingQRCode;
  r: integer;
  c: integer;
  scala: Double;
begin
  bitmap := TBitmap.create;
  try
    qr := TDelphiZXingQRCode.create;
    try
      qr.Data := ATexto;

      // ajuta o tamanho do bitmap para o tamanho do qrcode
      bitmap.SetSize(qr.Rows, qr.Columns);

      // copia o qrcode para o bitmap
      for r := 0 to qr.Rows - 1 do
        for c := 0 to qr.Columns - 1 do
          if qr.IsBlack[r, c] then
            bitmap.Canvas.Pixels[c, r] := clBlack
          else
            bitmap.Canvas.Pixels[c, r] := clWhite;

      // prepara para redimensionar o qrcode para o tamanho do canvas
      if (AWidth < bitmap.Height) then
      begin
        scala := AWidth / bitmap.Width;
      end
      else
      begin
        scala := AHeight / bitmap.Height;
      end;

      // transfere o bitmap para a imagem
      ACanvas.StretchDraw(Rect(0, 0, Trunc(scala * bitmap.Width),
        Trunc(scala * bitmap.Height)), bitmap);

    finally
      qr.Free;
    end;
  finally
    bitmap.Free;
  end;
end;

procedure TqrQRCodeBase.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Update;
  end;
end;

procedure TqrQRCodeBase.Update;
begin
  if Text <> '' then
  begin
    if (not Stretch) and (Picture.Graphic<>nil) then
      Picture.Graphic.SetSize(Width, Height);
    QrCodeToCanvas(Width, Height, Text, self.Canvas);
  end;
end;

{ TqrDBQRCode }

procedure TqrDBQRCode.Notification(AComponent: TComponent;
  Operation: TOperation);
begin

  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Dataset) then
    Dataset := nil;

end;



procedure TqrDBQRCode.Print(OfsX, OfsY: integer);
begin
  CreateQrCode;
  inherited;

end;

procedure TqrDBQRCode.CreateQrCode;
var
  FField: TField;
begin
  if (assigned(FDataset)) and (FDatafield <> '') then
  begin
    FField := FDataset.FindField(FDatafield);
    if assigned(FField) then
    begin
      if FField is TBlobField then
        Picture.Assign(FField)
      else
      begin
        Text := FField.asString;
      end;
    end;
  end;
end;

procedure TqrDBQRCode.SetDatafield(const Value: string);
begin
  FDatafield := Value;
end;

procedure TqrDBQRCode.SetDataset(const Value: TDataset);
begin
  FDataset := Value;
end;


end.
