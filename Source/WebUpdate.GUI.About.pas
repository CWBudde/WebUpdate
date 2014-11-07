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

procedure TFormAbout.LabelDWSClick(Sender: TObject);
begin
  ShellExecuteW(0, 'open', PChar('http://www.delphitools.info/dwscript/'), nil,
    nil, SW_SHOWDEFAULT)
end;

procedure TFormAbout.LabelIndyClick(Sender: TObject);
begin
  ShellExecuteW(0, 'open', PChar('http://www.indyproject.org/'), nil, nil,
    SW_SHOWDEFAULT)
end;

procedure TFormAbout.LabelJEDIClick(Sender: TObject);
begin
  ShellExecuteW(0, 'open', PChar('http://www.delphi-jedi.org/'), nil, nil,
    SW_SHOWDEFAULT)
end;

procedure TFormAbout.LabelVirtualTreeviewClick(Sender: TObject);
begin
  ShellExecuteW(0, 'open', PChar('http://www.jam-software.com/virtual-treeview/'),
    nil, nil, SW_SHOWDEFAULT)
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

