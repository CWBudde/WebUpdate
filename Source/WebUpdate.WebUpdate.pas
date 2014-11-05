unit WebUpdate.WebUpdate;

interface

uses
  System.SysUtils, WebUpdate.Channels.JSON,
  WebUpdate.Channel.JSON;

type
  TWebUpdate = class
  private
    FAutoGet: Boolean;
    FBaseURL: string;
    FChannelName: string;
    FChannels: TWebUpdateChannels;
    FChannelsFile: TFileName;
    FDownloadUpdater: Boolean;
    FLastModified: TDateTime;
    FUpdaterFileName: TFileName;
    procedure SetChannelName(const Value: string);
  public
    constructor Create; overload;
    constructor Create(BaseURL: string; ChannelsFileName: TFileName); overload;
    destructor Destroy; override;

    function CheckForUpdate: Boolean;
    procedure GetCurrentChannelInformation;
    procedure GetChannelsInformation;
    procedure PerformUpdate;
    procedure PerformDownloadUpdater;

    property AutoGetCurrentChannelInformation: Boolean read FAutoGet write FAutoGet;
    property BaseURL: string read FBaseURL write FBaseURL;
    property ChannelName: string read FChannelName write SetChannelName;
    property ChannelsFileName: TFileName read FChannelsFile write FChannelsFile;
    property LastModified: TDateTime read FLastModified;
    property DownloadUpdater: Boolean read FDownloadUpdater write FDownloadUpdater;
    property UpdaterFileName: TFileName read FUpdaterFileName write FUpdaterFileName;
  end;

implementation

uses
  System.StrUtils, WinApi.ShellApi, WinApi.Windows, Vcl.Forms, IdHTTP;

resourcestring
  RStrBaseURL = 'http://www.savioursofsoul.de/Christian/WebUpdate/';
  RStrBaseFile = 'Channels.json';

{ TWebUpdate }

constructor TWebUpdate.Create;
begin
  FAutoGet := False;
  FBaseURL := RStrBaseURL;
  FChannelsFile := RStrBaseFile;
  FChannelName := 'Stable';
  FChannels := TWebUpdateChannels.Create;
  FUpdaterFileName := 'Updater.exe';
end;

constructor TWebUpdate.Create(BaseURL: string; ChannelsFileName: TFileName);
begin
  Create;
  FBaseURL := BaseURL;
  FChannelsFile := ChannelsFileName;
end;

destructor TWebUpdate.Destroy;
begin
  FChannels.Free;
  inherited;
end;

procedure TWebUpdate.PerformDownloadUpdater;
var
  Http: TIdHTTP;
  Text: string;
  Success: Boolean;
begin
  // get channels file (JSON) text
  if StartsText('http://', FBaseURL) or StartsText('https://', FBaseURL) then
  begin
    Http := TIdHTTP.Create(nil);
    try
      try
        // get text from URI
        Text := Http.Get(FBaseURL + FUpdaterFileName);

        // check if text is available (might need check for JSON format)
        Success := Text <> '';
      except
        Success := False;
      end;

      // ignore errors here!
      if not Success then
        Exit;
    finally
      Http.Free;
    end;
  end
  else
    raise Exception.Create('Protocol not supported');
end;

procedure TWebUpdate.GetChannelsInformation;
var
  Http: TIdHTTP;
  Text: string;
  Success: Boolean;
begin
  // get channels file (JSON) text
  if StartsText('http://', FBaseURL) or StartsText('https://', FBaseURL) then
  begin
    Http := TIdHTTP.Create(nil);
    try
      try
        // get text from URI
        Text := Http.Get(FBaseURL + FChannelsFile);

        // check if text is available (might need check for JSON format)
        Success := Text <> '';
      except
        Success := False;
      end;

      // ignore errors here!
      if not Success then
        Exit;
    finally
      Http.Free;
    end;
  end
  else
    raise Exception.Create('Protocol not supported');

  FChannels.LoadFromString(Text);
end;

procedure TWebUpdate.GetCurrentChannelInformation;
var
  LocalPath: TFileName;
  CurrentSetup: TWebUpdateChannelSetup;
begin
  // check if a local file is present otherwise exit
  LocalPath := ExtractFilePath(ParamStr(0));
  if not FileExists(LocalPath + 'WebUpdate.json') then
    Exit;

  CurrentSetup := TWebUpdateChannelSetup.Create;
  try
    CurrentSetup.LoadFromFile(LocalPath + 'WebUpdate.json');

    FChannelName := CurrentSetup.ChannelName;
  finally
    CurrentSetup.Free;
  end;
end;

function TWebUpdate.CheckForUpdate: Boolean;
var
  ChannelItem: TWebUpdateChannelItem;
begin
  Result := False;

  // eventually get information about current channel
  if FAutoGet then
    GetCurrentChannelInformation;

  // locate current channel
  for ChannelItem in FChannels.Items do
    if ChannelItem.Name = FChannelName then
      if ChannelItem.Modified > FLastModified then
        Exit(True);
end;

procedure TWebUpdate.PerformUpdate;
var
  CurrentPath: string;
begin
  // eventually download updater
  if FDownloadUpdater then
    PerformDownloadUpdater;

  CurrentPath := ExtractFilePath(ParamStr(0));
  if FileExists(CurrentPath + FUpdaterFileName) then
  begin
    ShellExecute(Application.Handle, 'open', PChar(CurrentPath + FUpdaterFileName),
      nil, PChar(CurrentPath), SW_SHOW);
    Application.Terminate;
  end;
end;

procedure TWebUpdate.SetChannelName(const Value: string);
begin
  if FChannelName <> Value then
  begin
    FChannelName := Value;
    FLastModified := 0;
  end;
end;

end.
