program UpdaterWizard;

uses
  Vcl.Forms,
  WebUpdate.Classes.Updater in '..\Common\WebUpdate.Classes.Updater.pas',
  WebUpdate.JSON.Serializer in '..\Common\WebUpdate.JSON.Serializer.pas',
  WebUpdate.JSON.Channel in '..\Common\WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in '..\Common\WebUpdate.JSON.Channels.pas',
  WebUpdate.GUI.Updater in 'WebUpdate.GUI.Updater.pas' {FormWebUpdate},
  WebUpdate.MD5 in '..\Common\WebUpdate.MD5.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWebUpdate, FormWebUpdate);
  Application.Run;
end.

