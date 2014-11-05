unit WebUpdate.Project.JSON;

interface

uses
  System.SysUtils, dwsJSON, WebUpdate.JSON.Serializer;

type
  TWebUpdateProject = class(TJsonSerializer)
  type
    TFTPOptions = class(TJsonSerializer)
    private
      FPassword: string;
      FUsername: string;
      FServer: string;
    protected
      procedure Read(Root: TdwsJSONObject); override;
      procedure Write(Root: TdwsJSONObject); override;
    public
      constructor Create;

      property Server: string read FServer write FServer;
      property Username: string read FUsername write FUsername;
      property Password: string read FPassword write FPassword;
    end;

    TCopyOptions = class(TJsonSerializer)
    private
      FPath: string;
      FEnabled: Boolean;
    protected
      procedure Read(Root: TdwsJSONObject); override;
      procedure Write(Root: TdwsJSONObject); override;
    public
      constructor Create;

      property Path: string read FPath write FPath;
      property Enabled: Boolean read FEnabled write FEnabled;
    end;

  private
    FAutoCopyUpload: Boolean;
    FBaseDirectory: string;
    FChannelsFilename: TFileName;
    FCopyOptions: TCopyOptions;
    FCurrentChannel: string;
    FFtpOptions: TFTPOptions;
    function GetFullChannelsFilename: TFileName;
    function GetBasePath: string;
    function GetChannelsPath: string;
  protected
    procedure Read(Root: TdwsJSONObject); override;
    procedure Write(Root: TdwsJSONObject); override;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetID: string; override;

    property AutoCopyUpload: Boolean read FAutoCopyUpload write FAutoCopyUpload;
    property BaseDirectory: string read FBaseDirectory write FBaseDirectory;
    property BasePath: string read GetBasePath;
    property ChannelsFilename: TFileName read FChannelsFilename write FChannelsFilename;
    property ChannelsPath: string read GetChannelsPath;
    property Copy: TCopyOptions read FCopyOptions;
    property CurrentChannel: string read FCurrentChannel write FCurrentChannel;
    property FullChannelsFilename: TFileName read GetFullChannelsFilename;
    property FTP: TFTPOptions read FFtpOptions;
  end;

implementation

{ TWebUpdateProject.TFTPOptions }

constructor TWebUpdateProject.TFTPOptions.Create;
begin
  FServer := '';
  FUsername := '';
  FPassword := '';
end;

procedure TWebUpdateProject.TFTPOptions.Read(Root: TdwsJSONObject);
var
  Value: TdwsJSONValue;
begin
  inherited;

  Value := Root.Items['Server'];
  if Assigned(Value) then
    FServer := Value.AsString;

  Value := Root.Items['Username'];
  if Assigned(Value) then
    FUsername := Value.AsString;

  Value := Root.Items['Password'];
  if Assigned(Value) then
    FPassword := Value.AsString;
end;

procedure TWebUpdateProject.TFTPOptions.Write(Root: TdwsJSONObject);
begin
  inherited;

  if FServer <> '' then
    Root.AddValue('Server').AsString := FServer;

  if FUsername <> '' then
    Root.AddValue('Username').AsString := FUsername;

  if FPassword <> '' then
    Root.AddValue('Password').AsString := FPassword;
end;


{ TWebUpdateProject.TCopyOptions }

constructor TWebUpdateProject.TCopyOptions.Create;
begin
  FPath := '';
  FEnabled := False;
end;

procedure TWebUpdateProject.TCopyOptions.Read(Root: TdwsJSONObject);
var
  Value: TdwsJSONValue;
begin
  inherited;

  Value := Root.Items['Path'];
  if Assigned(Value) then
    FPath := Value.AsString;

  Value := Root.Items['Enabled'];
  if Assigned(Value) then
    FEnabled := Value.AsBoolean;
end;

procedure TWebUpdateProject.TCopyOptions.Write(Root: TdwsJSONObject);
begin
  inherited;

  if FPath <> '' then
    Root.AddValue('Path').AsString := FPath;

  if FEnabled then
    Root.AddValue('Enabled').AsBoolean := FEnabled;
end;


{ TWebUpdateProject }

constructor TWebUpdateProject.Create;
begin
  FFtpOptions := TFTPOptions.Create;
  FCopyOptions := TCopyOptions.Create;
  FCurrentChannel := 'Nightly';
end;

destructor TWebUpdateProject.Destroy;
begin
  FCopyOptions.Free;
  FFtpOptions.Free;
  inherited;
end;

function TWebUpdateProject.GetBasePath: string;
begin
  Result := IncludeTrailingPathDelimiter(FBaseDirectory);
end;

function TWebUpdateProject.GetChannelsPath: string;
begin
  Result := ExtractFilePath(FChannelsFilename);
end;

function TWebUpdateProject.GetFullChannelsFilename: TFileName;
begin
  if (FBaseDirectory <> '') then
    if FChannelsFilename <> '' then
      Result := IncludeTrailingPathDelimiter(FBaseDirectory) + FChannelsFilename
    else
      Result := FChannelsFilename
  else
    Result := '';
end;

class function TWebUpdateProject.GetID: string;
begin
  Result := 'Project';
end;

procedure TWebUpdateProject.Read(Root: TdwsJSONObject);
var
  Value: TdwsJSONValue;
begin
  inherited;

  Value := Root.Items['BaseDirectory'];
  if Assigned(Value) then
    FBaseDirectory := Value.AsString;

  Value := Root.Items['ChannelFilename'];
  if Assigned(Value) then
    FChannelsFilename := Value.AsString;

  Value := Root.Items['CurrentChannel'];
  if Assigned(Value) then
    FCurrentChannel := Value.AsString;

  Value := Root.Items['AutoCopyUpload'];
  if Assigned(Value) then
    FAutoCopyUpload := Value.AsBoolean;

  Value := Root.Items['FTP'];
  if Value is TdwsJSONObject then
    FFtpOptions.Read(TdwsJSONObject(Value));

  Value := Root.Items['Copy'];
  if Value is TdwsJSONObject then
    FCopyOptions.Read(TdwsJSONObject(Value));
end;

procedure TWebUpdateProject.Write(Root: TdwsJSONObject);
var
  ObjVal: TdwsJSONObject;
begin
  inherited;

  if FBaseDirectory <> '' then
    Root.AddValue('BaseDirectory').AsString := FBaseDirectory;

  if FChannelsFilename <> '' then
    Root.AddValue('ChannelFilename').AsString := FChannelsFilename;

  if FCurrentChannel <> '' then
    Root.AddValue('CurrentChannel').AsString := FCurrentChannel;

  // write FTP options (and add if it contains any data)
  ObjVal := TdwsJSONObject.Create;
  try
    FFtpOptions.Write(ObjVal);
    if ObjVal.ElementCount > 0 then
      Root.Add('FTP', ObjVal.Clone);
  finally
    ObjVal.Free;
  end;

  // write copy options (and add if it contains any data)
  ObjVal := TdwsJSONObject.Create;
  try
    FCopyOptions.Write(ObjVal);
    if ObjVal.ElementCount > 0 then
      Root.Add('Copy', ObjVal.Clone);
  finally
    ObjVal.Free;
  end;
end;

end.
