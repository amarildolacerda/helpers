unit REST.SocialReg;

interface

Uses System.Classes, REST.Social, REST.FDSocial,
     DesignIntf, DesignEditors;


type
  TRESTSocialCompEditor = class (TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;



procedure Register;


implementation


procedure Register;
begin
  RegisterComponentEditor (TRESTSocialClient, TRESTSocialCompEditor);
  RegisterComponents('REST Client', [TRESTSocialClientDataset,TRESTSocialClient]);
end;



{ TRESTSocialCompEditor }

procedure TRESTSocialCompEditor.Edit;
begin
  inherited;

end;

procedure TRESTSocialCompEditor.ExecuteVerb(Index: Integer);
begin
   case Index of
    0: with Component as TRESTSocialClient do
         Execute;
    1: with Component as TRESTSocialClient do
        clear;
  end;

end;

function TRESTSocialCompEditor.GetVerb(Index: Integer): string;
begin
 case Index of
    0: Result := 'Execute';
    1: Result := 'Clear Dataset';
  end;
end;

function TRESTSocialCompEditor.GetVerbCount: Integer;
begin
   result := 2;
end;

end.
