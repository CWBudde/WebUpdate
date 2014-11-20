unit WebUpdate.JSON.Channel;

interface

uses
  System.SysUtils, System.Types, System.Generics.Collections, dwsJSON,
  WebUpdate.JSON.Serializer;

type
  TWebUpdateFileAction = (iaAdd, iaDelete);

  TWebUpdateFileItem = class
  private
    FFileName: TFileName;
    FFileSize: Integer;
    FModified: TDateTime;
    FMD5Hash: string;
    FAction: TWebUpdateFileAction;
  public
    constructor Create;

    property Action: TWebUpdateFileAction read FAction write FAction;
    property FileName: TFileName read FFileName write FFileName;
    property FileSize: Integer read FFileSize write FFileSize;
    property Modified: TDateTime read FModified write FModified;
    property MD5Hash: string read FMD5Hash write FMD5Hash;
  end;
  TWebUpdateFileItems = TList<TWebUpdateFileItem>;

  TWebUpdateChannelSetup = class(TJsonSerializer)
  private
    FItems: TWebUpdateFileItems;
    FModified: TDateTime;
    FChannelName: string;
    FAppName: TFileName;
    FPreUpdateScript: string;
    FPostUpdateScript: string;
  protected
    procedure Read(Root: TdwsJSONObject); override;
    procedure Write(Root: TdwsJSONObject); override;
  public
    constructor Create;
    class function GetID: string; override;

    property Modified: TDateTime read FModified write FModified;
    property Items: TWebUpdateFileItems read FItems;
    property AppName: TFileName read FAppName write FAppName;
    property PreUpdateScript: string read FPreUpdateScript write FPreUpdateScript;
    property PostUpdateScript: string read FPostUpdateScript write FPostUpdateScript;
    property ChannelName: string read FChannelName write FChannelName;
  end;

implementation

uses
  System.StrUtils;

{ TWebUpdateFileItem }

constructor TWebUpdateFileItem.Create;
begin
  FFileName := '';
  FFileSize := 0;
  FModified := 0;
  FMD5Hash := '';
  FAction := iaAdd;
end;


{ TWebUpdateChannelSetup }

constructor TWebUpdateChannelSetup.Create;
begin
  inherited;
  FItems := TWebUpdateFileItems.Create;
  FAppName := '';
  FModified := 0;
end;

class function TWebUpdateChannelSetup.GetID: string;
begin
  Result := 'Channel';
end;

procedure TWebUpdateChannelSetup.Read(Root: TdwsJSONObject);
var
  Value: TdwsJSONValue;
  FileValue: TdwsJSONObject;
  Files: TdwsJSONArray;
  Index: Integer;
  Item: TWebUpdateFileItem;
begin
  inherited;

  // get channel name
  Value := Root.Items['Name'];
  if Assigned(Value) then
    FChannelName := Value.AsString;

  // get modification date
  Value := Root.Items['Modified'];
  if Assigned(Value) then
    FModified := ISO8601ToDateTime(Value.AsString);

  // get pre-build script
  Value := Root.Items['PreUpdateScript'];
  if Assigned(Value) then
    FPreUpdateScript := Value.AsString;

  // get files
  Value := Root.Items['Files'];
  if not (Value is TdwsJSONArray) then
    raise Exception.Create('Array expected!');

  // clear existing items
  FItems.Clear;

  Files := TdwsJSONArray(Value);
  for Index := 0 to Files.ElementCount - 1 do
  begin
    Value := Files.Elements[Index];
    if not (Value is TdwsJSONObject) then
      raise Exception.Create('Object expected!');

    // get file item
    FileValue := TdwsJSONObject(Value);
    Item := TWebUpdateFileItem.Create;

    // get file name
    Value := FileValue.Items['FileName'];
    if not Assigned(Value) then
      raise Exception.Create('FileName not specified!');
    Item.FileName := Value.AsString;

    // get file size
    Value := FileValue.Items['Size'];
    if Assigned(Value) then
      Item.FileSize := Value.AsInteger;

    // get modification date/time
    Value := FileValue.Items['Modified'];
    if Assigned(Value) then
      Item.Modified := ISO8601ToDateTime(Value.AsString);

    // get MD5 hash
    Value := FileValue.Items['MD5'];
    if Assigned(Value) then
      Item.MD5Hash := Value.AsString;

    // get action
    Value := FileValue.Items['Action'];
    if Assigned(Value) then
      if SameText(Value.AsString, 'delete') then
        Item.Action := iaDelete
      else if SameText(Value.AsString, 'add') then
        Item.Action := iaAdd
      else
        raise Exception.Create('Unknown action');

    // add item to file items
    FItems.Add(Item);
  end;

  // get post-build script
  Value := Root.Items['PostUpdateScript'];
  if Assigned(Value) then
    FPostUpdateScript := Value.AsString;

  // get application name
  Value := Root.Items['AppName'];
  if Assigned(Value) then
    FAppName := Value.AsString;
end;

procedure TWebUpdateChannelSetup.Write(Root: TdwsJSONObject);
var
  Files: TdwsJSONArray;
  FileValue: TdwsJSONObject;
  Item: TWebUpdateFileItem;
begin
  inherited;

  // write modification data
  Root.AddValue('Modified').AsString := DateTimeToISO8601(FModified);

  // eventually write channel name (if present)
  if FChannelName <> '' then
    Root.AddValue('Name').AsString := FChannelName;

  // get pre-build script
  if FPreUpdateScript <> '' then
    Root.AddValue('PreUpdateScript').AsString := FPreUpdateScript;

  // create files array
  Files := Root.AddArray('Files');

  for Item in FItems do
  begin
    FileValue := Files.AddObject;
    FileValue.AddValue('FileName').AsString := Item.FileName;

    // eventually store file size
    if Item.FileSize <> 0 then
      FileValue.AddValue('Size').AsInteger := Item.FileSize;

    // eventually store file modification time stamp
    if Item.Modified <> 0 then
      FileValue.AddValue('Modified').AsString := DateTimeToISO8601(Item.Modified);

    // eventually store MD5 hash
    if Item.MD5Hash <> '' then
      FileValue.AddValue('MD5').AsString := Item.MD5Hash;

    // eventually store action (default is 'add')
    case Item.Action of
      iaDelete:
        FileValue.AddValue('Action').AsString := 'Delete';
    end;
  end;

  // get post-build script
  if FPostUpdateScript <> '' then
    Root.AddValue('PostUpdateScript').AsString := FPostUpdateScript;

  // write app name
  if FAppName <> '' then
    Root.AddValue('AppName').AsString := FAppName;
end;

end.
