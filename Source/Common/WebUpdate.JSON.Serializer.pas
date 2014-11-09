unit WebUpdate.JSON.Serializer;

interface

uses
  System.SysUtils, dwsJSON;

type
  EJsonSerializer = class(Exception);

  TJsonSerializer = class
  protected
    procedure Read(Root: TdwsJSONObject); virtual;
    procedure Write(Root: TdwsJSONObject); virtual;
  public
    class function GetID: string; virtual;

    procedure LoadFromFile(const FileName: TFileName);
    procedure SaveToFile(const FileName: TFileName);

    procedure LoadFromString(const Text: string);
    function SaveToString: string;
  end;

function DateTimeToISO8601(DateTime: TDateTime): string;
function ISO8601ToDateTime(const Value: string): TDateTime;

implementation

uses
  dwsXPlatform;

function DateTimeToISO8601(DateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd"' + 'T' + '"hh":"mm":"ss.zzz', DateTime);
end;

function ISO8601ToDateTime(const Value: string): TDateTime;

  function Str2Time(s: string): TDateTime;
  var
    Hour, Minute, MSec, Second: Word;
    Temp: Integer;
  begin
    s := Trim(s);
    if s = '' then
      Result := 0
    else
    begin
      Temp := Pos(':', s);
      Hour := StrToInt(Copy(s, 1, Temp - 1));
      Delete(s, 1, Temp);
      Temp := Pos(':', s);
      Minute := StrToInt(Copy(s, 1, Temp - 1));
      Delete(s, 1, Temp);
      Temp := Pos('.', s);
      if Temp > 0 then
      begin
        MSec := StrToInt(Copy(s, Temp + 1, Length(s) - Temp));
        Delete(s, Temp, Length(s) - Temp + 1);
      end
      else
        MSec := 0;
      Second := StrToInt(s);
      Result := EncodeTime(Hour, Minute, Second, MSec);
    end;
  end;

var
  Day, Month, Year: Word;
  Temp: Integer;
  DateStr, TimeStr: string;
begin
  // check for a value (At all)
  if Value = '' then
    Exit(0);

  Temp := Pos ('T', Value);
  // detect all known date/time formats
  if (Temp = 0) and (Pos('-', Value) > 0) then
    Temp := Length(Value) + 1;
  DateStr := Trim(Copy(Value, 1, Temp - 1));
  TimeStr := Trim(Copy(Value, Temp + 1, Length(Value) - Temp));
  Result := 0;
  if DateStr <> '' then
  begin
    Temp := Pos ('-', DateStr);
    Year := StrToInt(Copy(DateStr, 1, Temp - 1));
    Delete(DateStr, 1, Temp);
    Temp := Pos ('-', DateStr);
    Month := StrToInt(Copy(DateStr, 1, Temp - 1));
    Day := StrToInt(Copy(DateStr, Temp + 1, Length(DateStr) - Temp));
    Result := EncodeDate(Year, Month, Day);
  end;
  Result := Result + Frac(Str2Time(TimeStr));
end;


{ TJsonSerializer }

class function TJsonSerializer.GetID: string;
begin
  Result := '';
end;

procedure TJsonSerializer.LoadFromFile(const FileName: TFileName);
var
  Root: TdwsJSONValue;
begin
  // check if file exists
  if not FileExists(FileName) then
    raise Exception.CreateFmt('File %s does not exist!', [FileName]);

  // parse JSON file
  Root := TdwsJSONValue.ParseFile(FileName);

  // ensure the JSON file is an object
  if not (Root is TdwsJSONObject) then
    raise EJsonSerializer.Create('Object expected');

  Read(TdwsJSONObject(Root));
end;

procedure TJsonSerializer.SaveToFile(const FileName: TFileName);
var
  Root: TdwsJSONObject;
begin
  Root := TdwsJSONObject.Create;
  Write(Root);

  SaveTextToUTF8File(FileName, Root.ToBeautifiedString);
end;

procedure TJsonSerializer.LoadFromString(const Text: string);
var
  Root: TdwsJSONValue;
begin
  Root := TdwsJSONValue.ParseString(Text);

  if not (Root is TdwsJSONObject) then
    raise EJsonSerializer.Create('Object expected');

  Read(TdwsJSONObject(Root));
end;

function TJsonSerializer.SaveToString: string;
var
  Root: TdwsJSONObject;
begin
  Root := TdwsJSONObject.Create;
  Write(Root);

  Result := Root.ToBeautifiedString;
end;

procedure TJsonSerializer.Read(Root: TdwsJSONObject);
begin
  // eventually check ID
  if GetID <> '' then
    if not (Root.Items['ID'].AsString = GetID) then
      raise EJsonSerializer.Create('ID mismatch');
end;

procedure TJsonSerializer.Write(Root: TdwsJSONObject);
begin
  // eventually set ID
  if GetID <> '' then
    Root.AddValue('ID').AsString := GetID;
end;

end.
