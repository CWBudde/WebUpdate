program AuthoringTool;

uses
  Vcl.Forms,
  WebUpdate.Classes.WebUpdate in '..\Common\WebUpdate.Classes.WebUpdate.pas',
  WebUpdate.GUI.About in 'WebUpdate.GUI.About.pas' {FormAbout},
  WebUpdate.GUI.AuthorTool in 'WebUpdate.GUI.AuthorTool.pas' {FormWebUpdateTool},
  WebUpdate.GUI.Options in 'WebUpdate.GUI.Options.pas' {FormOptions},
  WebUpdate.GUI.CommandLine in 'WebUpdate.GUI.CommandLine.pas' {FormCommandLine},
  WebUpdate.JSON.Channel in '..\Common\WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in '..\Common\WebUpdate.JSON.Channels.pas',
  WebUpdate.JSON.Preferences in 'WebUpdate.JSON.Preferences.pas',
  WebUpdate.JSON.Project in '..\Common\WebUpdate.JSON.Project.pas',
  WebUpdate.JSON.Serializer in '..\Common\WebUpdate.JSON.Serializer.pas',
  WebUpdate.Tools in '..\Common\WebUpdate.Tools.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWebUpdateTool, FormWebUpdateTool);
  Application.Run;
end.

