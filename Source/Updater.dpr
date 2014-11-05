program Updater;

uses
  Vcl.Forms,
  WebUpdate.Channel.JSON in 'WebUpdate.Channel.JSON.pas',
  WebUpdate.Channels.JSON in 'WebUpdate.Channels.JSON.pas',
  WebUpdate.Updater in 'WebUpdate.Updater.pas' {FormWebUpdate},
  WebUpdate.MD5 in 'WebUpdate.MD5.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWebUpdate, FormWebUpdate);
  Application.Run;
end.

