unit WebUpdate.Package.Component;

interface

uses
  System.Classes, System.SysUtils, WebUpdate.Classes.Updater,
  WebUpdate.Classes.WebUpdate;

type
  TWebUpdateComponent = class(TComponent)
  private
    FWebUpdate: TWebUpdate;
    function GetBaseURL: string;
    function GetChannelName: string;
    function GetDownloadUpdater: Boolean;
    procedure SetBaseURL(const Value: string);
    procedure SetChannelName(const Value: string);
    procedure SetDownloadUpdater(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CheckForUpdate: Boolean;
    procedure GetLocalChannelInformation;
    procedure GetChannelsInformationFromServer;
    procedure PerformUpdate;
    procedure PerformDownloadUpdater;

    property WebUpdate: TWebUpdate read FWebUpdate;
  published
    property BaseURL: string read GetBaseURL write SetBaseURL;
    property ChannelName: string read GetChannelName write SetChannelName;
    property DownloadUpdater: Boolean read GetDownloadUpdater write SetDownloadUpdater default False;
  end;

implementation

resourcestring
  RStrURLNotSpecified = 'URL not specified!';

{ TWebUpdateComponent }

constructor TWebUpdateComponent.Create(AOwner: TComponent);
begin
  inherited;
  FWebUpdate := TWebUpdate.Create;
  FWebUpdate.BaseURL := '';
end;

destructor TWebUpdateComponent.Destroy;
begin
  FWebUpdate.Free;
  inherited;
end;

function TWebUpdateComponent.CheckForUpdate: Boolean;
begin
  Result := FWebUpdate.CheckForUpdate;
end;

function TWebUpdateComponent.GetBaseURL: string;
begin
  Result := FWebUpdate.BaseURL;
end;

function TWebUpdateComponent.GetChannelName: string;
begin
  Result := FWebUpdate.ChannelName;
end;

procedure TWebUpdateComponent.GetChannelsInformationFromServer;
begin
  FWebUpdate.GetChannelsInformationFromServer;
end;

procedure TWebUpdateComponent.GetLocalChannelInformation;
begin
  FWebUpdate.GetLocalChannelInformation
end;

function TWebUpdateComponent.GetDownloadUpdater: Boolean;
begin
  Result := FWebUpdate.DownloadUpdater;
end;

procedure TWebUpdateComponent.PerformDownloadUpdater;
begin
  if FWebUpdate.BaseURL = '' then
    raise Exception.Create(RStrURLNotSpecified);

  FWebUpdate.PerformDownloadUpdater;
end;

procedure TWebUpdateComponent.PerformUpdate;
begin
  if FWebUpdate.BaseURL = '' then
    raise Exception.Create(RStrURLNotSpecified);

  FWebUpdate.PerformUpdate;
end;

procedure TWebUpdateComponent.SetBaseURL(const Value: string);
begin
  FWebUpdate.BaseURL := Value;
end;

procedure TWebUpdateComponent.SetChannelName(const Value: string);
begin
  FWebUpdate.ChannelName := Value;
end;

procedure TWebUpdateComponent.SetDownloadUpdater(const Value: Boolean);
begin
  FWebUpdate.DownloadUpdater := Value;
end;

end.
