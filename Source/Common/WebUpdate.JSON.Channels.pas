unit WebUpdate.JSON.Channels;

interface

uses
  System.SysUtils, System.Generics.Collections, dwsJSON,
  WebUpdate.JSON.Serializer;

type
  TWebUpdateChannelItem = class
    Name: string;
    FileName: TFileName;
    Modified: TDateTime;
    MD5: string;
  end;
  TWebUpdateChannelItems = TList<TWebUpdateChannelItem>;

  TWebUpdateChannels = class(TJsonSerializer)
  private
    FItems: TWebUpdateChannelItems;
  protected
    procedure Read(Root: TdwsJSONObject); override;
    procedure Write(Root: TdwsJSONObject); override;
  public
    constructor Create;
    class function GetID: string; override;

    property Items: TWebUpdateChannelItems read FItems;
  end;

implementation

{ TWebUpdateChannels }

constructor TWebUpdateChannels.Create;
begin
  inherited;
  FItems := TWebUpdateChannelItems.Create;
end;

class function TWebUpdateChannels.GetID: string;
begin
  Result := 'Channels';
end;

procedure TWebUpdateChannels.Read(Root: TdwsJSONObject);
var
  Value: TdwsJSONValue;
  Files: TdwsJSONArray;
  Index: Integer;
  Item: TWebUpdateChannelItem;
begin
  inherited;

  Value := Root.Items['Files'];
  if not (Value is TdwsJSONArray) then
    raise Exception.Create('Array expected!');

  // clear existing items
  FItems.Clear;

  Files := TdwsJSONArray(Value);
  for Index := 0 to Files.ElementCount - 1 do
  begin
    Item := TWebUpdateChannelItem.Create;

    // get name
    Value := Files.Elements[Index].Items['Name'];
    Item.Name := Value.AsString;

    // get filename
    Value := Files.Elements[Index].Items['FileName'];
    if Assigned(Value) then
      Item.FileName := Value.AsString
    else
      Item.FileName := Item.Name + '.json';

    // get file modified
    Value := Files.Elements[Index].Items['Modified'];
    if Assigned(Value) then
      Item.Modified := ISO8601ToDateTime(Value.AsString)
    else
      Item.Modified := 0;

    // get MD5 hash
    Value := Files.Elements[Index].Items['MD5'];
    if Assigned(Value) then
      Item.MD5 := Value.AsString;

    FItems.Add(Item);
  end;
end;

procedure TWebUpdateChannels.Write(Root: TdwsJSONObject);
var
  Value: TdwsJSONObject;
  Files: TdwsJSONArray;
  Item: TWebUpdateChannelItem;
begin
  inherited;

  Root.AddValue('Modified').AsString := DateTimeToISO8601(Now);
  Files := Root.AddArray('Files');
  for Item in FItems do
  begin
    Value := TdwsJSONObject(Files.AddObject);
    Value.AddValue('Name').AsString := Item.Name;

    // eventually store file name (if not identical to Name + '.json')
    if not (Item.FileName = Item.Name + '.json') then
      if not (Item.FileName = '') then
        Value.AddValue('FileName').AsString := Item.FileName;

    // eventually store file modification date
    if Item.Modified > 0 then
      Value.AddValue('Modified').AsString := DateTimeToISO8601(Item.Modified);

    // eventually store MD5 hash
    if Item.MD5 <> '' then
      Value.AddValue('MD5').AsString := Item.MD5;
  end;
end;

end.
