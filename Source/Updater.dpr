program Updater;

uses
  Vcl.Forms,
  WebUpdate.JSON.Channel in 'WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in 'WebUpdate.JSON.Channels.pas',
  WebUpdate.Updater in 'WebUpdate.Updater.pas' {FormWebUpdate},
  WebUpdate.MD5 in 'WebUpdate.MD5.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWebUpdate, FormWebUpdate);
  Application.Run;
end.

