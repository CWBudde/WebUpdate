unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  WebUpdate.Classes.WebUpdate;

type
  TFormExample = class(TForm)
    ButtonCheckUpdates: TButton;
    ButtonExit: TButton;
    ButtonPerformUpdate: TButton;
    LabelChannel: TLabel;
    ComboBoxChannels: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCheckUpdatesClick(Sender: TObject);
    procedure ButtonExitClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonPerformUpdateClick(Sender: TObject);
  private
    FWebUpdate: TWebUpdate;
  end;

var
  FormExample: TFormExample;

implementation

{$R *.dfm}

{ TFormExample }

procedure TFormExample.FormCreate(Sender: TObject);
begin
  FWebUpdate := TWebUpdate.Create;
  FWebUpdate.BaseURL := 'http://www.savioursofsoul.de/Christian/WebUpdate/';
end;

procedure TFormExample.FormDestroy(Sender: TObject);
begin
  FWebUpdate.Free;
end;

procedure TFormExample.FormShow(Sender: TObject);
var
  Index: Integer;
begin
  FWebUpdate.GetChannels(ComboBoxChannels.Items);
  FWebUpdate.GetLocalChannelInformation;
  Index := ComboBoxChannels.Items.IndexOf(FWebUpdate.ChannelName);
  if Index >= 0 then
    ComboBoxChannels.ItemIndex := Index
  else
    ComboBoxChannels.ItemIndex := 0;
end;

procedure TFormExample.ButtonCheckUpdatesClick(Sender: TObject);
begin
  if FWebUpdate.CheckForUpdate then
  begin
    ButtonPerformUpdate.Default := True;
    ButtonPerformUpdate.SetFocus;
    if MessageDlg('A new update is available!'#13#10#13#10'Update now?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      ButtonPerformUpdateClick(Self);
  end
  else
    MessageDlg('No update is available!', mtInformation, [mbOK], 0);
end;

procedure TFormExample.ButtonPerformUpdateClick(Sender: TObject);
begin
  FWebUpdate.PerformUpdate;
end;

procedure TFormExample.ButtonExitClick(Sender: TObject);
begin
  Close;
end;

end.
