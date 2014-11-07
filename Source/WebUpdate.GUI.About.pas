unit WebUpdate.GUI.About;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TFormAbout = class(TForm)
    ImageHeader: TImage;
    LabelAnd: TLabel;
    LabelComma1: TLabel;
    LabelComma2: TLabel;
    LabelCopyright: TLabel;
    LabelDualLicenses: TLabel;
    LabelDWS: TLabel;
    LabelIndy: TLabel;
    LabelJEDI: TLabel;
    LabelSubTitle: TLabel;
    LabelTitle: TLabel;
    LabelVirtualTreeview: TLabel;
    MemoLGPL: TMemo;
    MemoMIT: TMemo;
    PanelHeader: TPanel;
    RadioButtonLicenseLGPL: TRadioButton;
    RadioButtonLicenseMIT: TRadioButton;
    procedure LabelDWSClick(Sender: TObject);
    procedure LabelIndyClick(Sender: TObject);
    procedure LabelJEDIClick(Sender: TObject);
    procedure LabelVirtualTreeviewClick(Sender: TObject);
    procedure RadioButtonLicenseLGPLClick(Sender: TObject);
    procedure RadioButtonLicenseMITClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  WinApi.ShellApi;

procedure OpenLink(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWDEFAULT)
end;

procedure TFormAbout.LabelDWSClick(Sender: TObject);
begin
  OpenLink('http://www.delphitools.info/dwscript/');
end;

procedure TFormAbout.LabelIndyClick(Sender: TObject);
begin
  OpenLink('http://www.indyproject.org/');
end;

procedure TFormAbout.LabelJEDIClick(Sender: TObject);
begin
  OpenLink('http://www.delphi-jedi.org/');
end;

procedure TFormAbout.LabelVirtualTreeviewClick(Sender: TObject);
begin
  OpenLink('http://www.jam-software.com/virtual-treeview/');
end;

procedure TFormAbout.RadioButtonLicenseLGPLClick(Sender: TObject);
begin
  MemoLGPL.BringToFront;
end;

procedure TFormAbout.RadioButtonLicenseMITClick(Sender: TObject);
begin
  MemoMIT.BringToFront;
end;

end.
