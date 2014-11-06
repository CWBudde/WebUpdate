program WebUpdateTool;

uses
  Vcl.Forms,
  WebUpdate.GUI.About in 'WebUpdate.GUI.About.pas' {FormAbout},
  WebUpdate.GUI.AuthorTool in 'WebUpdate.GUI.AuthorTool.pas' {FormWebUpdateTool},
  WebUpdate.GUI.Options in 'WebUpdate.GUI.Options.pas' {FormOptions},
  WebUpdate.GUI.CommandLine in 'WebUpdate.GUI.CommandLine.pas' {FormCommandLine},
  WebUpdate.JSON.Channel in 'WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in 'WebUpdate.JSON.Channels.pas',
  WebUpdate.JSON.Preferences in 'WebUpdate.JSON.Preferences.pas',
  WebUpdate.JSON.Project in 'WebUpdate.JSON.Project.pas',
  WebUpdate.JSON.Serializer in 'WebUpdate.JSON.Serializer.pas',
  WebUpdate.MD5 in 'WebUpdate.MD5.pas',
  WebUpdate.WebUpdate in 'WebUpdate.WebUpdate.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWebUpdateTool, FormWebUpdateTool);
  if not Application.Terminated then
    Application.Run;
end.

