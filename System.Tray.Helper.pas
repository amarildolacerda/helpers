unit System.Tray.Helper;

interface

uses System.Classes, System.SysUtils, JvTrayIcon, System.Classes.Helper,
  vcl.Forms, WinApi.Windows,
  System.SysUtils.Helper, vcl.Menus.helpers, vcl.Menus;

type
  IJvTrayIcon = Interface
    ['{BF0C33CF-49C8-4A21-BCB1-A352CDA29CB6}']
  End;

  TJvTrayIconHelper = class(TJvTrayIcon, IJvTrayIcon)
  private
    FPopUp: TPopupmenu;
    FForm: TForm;
    FOldOnShow: TNotifyEvent;
    FClosing: boolean;
    procedure DoOnShow(sender: TObject);
    procedure SetForm(const AForm: TForm);
    procedure SetClosing(const Value: boolean);
  public
    class function new: IJvTrayIcon;
    constructor create(AOwner: TComponent); override;
    function createItem(ACaption: string; AProc: TProc; AIdx: integer = -1)
      : TMenuItem;
    property Closing: boolean read FClosing write SetClosing;
  end;


  function CreateTrayIcon(AForm:TForm):TJvTrayIconHelper;

implementation

function CreateTrayIcon(AForm:TForm):TJvTrayIconHelper;
begin
    result :=TJvTrayIconHelper.Create(AForm);
end;
{ TJvTrayIconHelper }

constructor TJvTrayIconHelper.create(AOwner: TComponent);
begin
  inherited;
  FPopUp := TPopupmenu.create(self);
  if assigned(AOwner) and AOwner.InheritsFrom(TForm) then
  begin
    SetForm(TForm(AOwner));
  end;
  FPopUp.AutoPopup := true;
  PopupMenu := FPopUp;
  SetWindowLong(Application.Handle,GWL_EXSTYLE,WS_EX_TOOLWINDOW);
end;

function TJvTrayIconHelper.createItem(ACaption: string; AProc: TProc;
  AIdx: integer): TMenuItem;
begin
  FPopUp.CreateItemAnonimous(ACaption, AProc, AIdx);
end;




procedure TJvTrayIconHelper.DoOnShow(sender: TObject);
begin
  if assigned(FOldOnShow) then
    FOldOnShow(sender);
  Active := true;
  if assigned(FForm) then
  begin
    FForm.WindowState := wsMinimized;

  end;
  ShowWindow(Application.Handle, SW_HIDE);


end;

class function TJvTrayIconHelper.new: IJvTrayIcon;
begin
  result := TJvTrayIconHelper.create(nil);
end;

procedure TJvTrayIconHelper.SetClosing(const Value: boolean);
begin
  FClosing := Value;
end;

procedure TJvTrayIconHelper.SetForm(const AForm: TForm);
begin
  if FForm = AForm then
    exit;
  if assigned(FForm) then
  begin // desligar
    FPopUp.Items.Clear;
    FForm.OnShow := FOldOnShow;
    FForm := nil;
  end;

  FForm := AForm;
  FOldOnShow := FForm.OnShow;
  FForm.OnShow := DoOnShow;

  FPopUp.CreateItemAnonimous('&Abrir',
    procedure
    begin
      if assigned(FForm) then
      begin
        FForm.WindowState := wsNormal;
        FForm.Show;
      end;
    end);

  FPopUp.CreateItemAnonimous('&Minimizar',
    procedure
    begin
      if assigned(FForm) then
        FForm.WindowState := wsMinimized;
    end);

  FPopUp.AddSeparator();

  FPopUp.CreateItemAnonimous('&Sair',
    procedure
    begin
      FClosing := true;
      if assigned(FForm) then
         FForm.Close;
    end);


end;

end.
