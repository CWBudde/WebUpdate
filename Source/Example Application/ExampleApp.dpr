program ExampleApp;

uses
  Vcl.Forms,
  WebUpdate.JSON.Channel in '..\Common\WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in '..\Common\WebUpdate.JSON.Channels.pas',
  WebUpdate.JSON.Serializer in '..\Common\WebUpdate.JSON.Serializer.pas',
  WebUpdate.Classes.WebUpdate in '..\Common\WebUpdate.Classes.WebUpdate.pas',
  MainUnit in 'MainUnit.pas' {FormExample};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormExample, FormExample);
  Application.Run;
end.

