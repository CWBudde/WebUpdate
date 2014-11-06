unit WebUpdate.GUI.About;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormAbout = class(TForm)
    RadioButtonLicenseMIT: TRadioButton;
    LabelDualLicenses: TLabel;
    RadioButtonLicenseLGPL: TRadioButton;
    MemoLGPL: TMemo;
    MemoMIT: TMemo;
    PanelHeader: TPanel;
    ImageHeader: TImage;
    LabelSubTitle: TLabel;
    LabelTitle: TLabel;
    Label1: TLabel;
    procedure RadioButtonLicenseMITClick(Sender: TObject);
    procedure RadioButtonLicenseLGPLClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

implementation

{$R *.dfm}

procedure TFormAbout.RadioButtonLicenseLGPLClick(Sender: TObject);
begin
  MemoLGPL.BringToFront;
end;

procedure TFormAbout.RadioButtonLicenseMITClick(Sender: TObject);
begin
  MemoMIT.BringToFront;
end;

end.

