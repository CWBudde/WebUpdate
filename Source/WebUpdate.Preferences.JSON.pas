unit WebUpdate.Preferences.JSON;

interface

uses
  System.SysUtils, dwsJSON, WebUpdate.JSON.Serializer;

type
  TWebUpdatePreferences = class(TJsonSerializer)
  private
    FFileName: TFileName;
    FRecentProject: TFileName;
    FTop: Integer;
    FLeft: Integer;
    procedure Load;
    procedure Save;
  protected
    procedure Read(Root: TdwsJSONObject); override;
    procedure Write(Root: TdwsJSONObject); override;
  public
    constructor Create(FileName: TFileName);
    destructor Destroy; override;

    class function GetID: string; override;

    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property RecentProject: TFileName read FRecentProject write FRecentProject;
  end;

implementation

uses
  dwsXPlatform;

{ TWebUpdatePreferences }

constructor TWebUpdatePreferences.Create(FileName: TFileName);
begin
  FFileName := FileName;
  if FileExists(FFileName) then
    Load;
end;

destructor TWebUpdatePreferences.Destroy;
begin
  Save;
  inherited;
end;

class function TWebUpdatePreferences.GetID: string;
begin
  Result := 'Preferences';
end;

procedure TWebUpdatePreferences.Load;
begin
  LoadFromFile(FFileName);
end;

procedure TWebUpdatePreferences.Save;
begin
  SaveToFile(FFileName);
end;

procedure TWebUpdatePreferences.Read(Root: TdwsJSONObject);
var
  Value: TdwsJSONValue;
begin
  inherited;

  Value := Root.Items['Left'];
  if Assigned(Value) then
    FLeft := Value.AsInteger;
  Value := Root.Items['Top'];
  if Assigned(Value) then
    FTop := Value.AsInteger;

  // read recent project
  Value := Root.Items['RecentProject'];
  if Assigned(Value) then
    FRecentProject := Value.AsString;
end;

procedure TWebUpdatePreferences.Write(Root: TdwsJSONObject);
begin
  inherited;

  Root.AddValue('Left').AsInteger := FLeft;
  Root.AddValue('Top').AsInteger := FTop;

  if FRecentProject <> '' then
    Root.AddValue('RecentProject').AsString := FRecentProject;
end;

end.
