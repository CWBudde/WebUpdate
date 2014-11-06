program WebUpdateTool;

{$DEFINE SupportsConsole}

uses
  Vcl.Forms,
  WebUpdate.Channel.JSON in 'WebUpdate.Channel.JSON.pas',
  WebUpdate.Channels.JSON in 'WebUpdate.Channels.JSON.pas',
  WebUpdate.JSON.Serializer in 'WebUpdate.JSON.Serializer.pas',
  WebUpdate.MD5 in 'WebUpdate.MD5.pas',
  WebUpdate.Project.JSON in 'WebUpdate.Project.JSON.pas',
  WebUpdate.Preferences.JSON in 'WebUpdate.Preferences.JSON.pas',
  WebUpdate.AuthorTool in 'WebUpdate.AuthorTool.pas' {FormWebUpdateTool},
  WebUpdate.Options.GUI in 'WebUpdate.Options.GUI.pas' {FormOptions},
  WebUpdate.WebUpdate in 'WebUpdate.WebUpdate.pas',
  WebUpdate.GUI.About in 'WebUpdate.GUI.About.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormWebUpdateTool, FormWebUpdateTool);
  Application.CreateForm(TForm1, Form1);
  if not Application.Terminated then
    Application.Run;
end.

