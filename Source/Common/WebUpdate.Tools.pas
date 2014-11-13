unit WebUpdate.Tools;

interface

uses
  {$IFDEF MSWINDOWS} Winapi.Windows, {$ENDIF}
  System.Classes, System.SysUtils, IdHashMessageDigest, IdGlobal;

{$IFDEF MSWINDOWS}
function FileTimeToDateTime(Time: TFileTime): TDateTime;
{$ENDIF}
function MD5(const FileName: string): string; overload;
function MD5(Stream: TStream): string; overload;

function WebToLocalFileName(const WebFileName: string): string;
function LocalToWebFileName(const LocalFileName: string): string;

implementation

{$IFDEF MSWINDOWS}
function FileTimeToDateTime(Time: TFileTime): TDateTime;

  function InternalEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond: Word): TDateTime;
  var
    LTime: TDateTime;
    Success: Boolean;
  begin
    Result := 0;
    Success := TryEncodeDate(AYear, AMonth, ADay, Result);
    if Success then
    begin
      Success := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
      if Success then
        if Result >= 0 then
          Result := Result + LTime
        else
          Result := Result - LTime
    end;
  end;

var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result := 0;
  FileTimeToLocalFileTime(Time, LFileTime);

  if FileTimeToSystemTime(LFileTime, SysTime) then
    with SysTime do
    begin
      Result := InternalEncodeDateTime(wYear, wMonth, wDay, wHour, wMinute,
        wSecond, wMilliseconds);
    end;
end;
{$ENDIF}

function MD5(Stream: TStream): string;
var
  IdMD5: TIdHashMessageDigest5;
begin
  IdMD5 := TIdHashMessageDigest5.Create;
  try
    Result := IdMD5.HashStreamAsHex(Stream);
  finally
    IdMD5.Free;
  end;
end;

function MD5(const FileName: string): string;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := MD5(FS);
  finally
    FS.Free;
  end;
end;

function WebToLocalFileName(const WebFileName: string): string;
begin
  Result := StringReplace(WebFileName, '/', '\', [rfReplaceAll]);
end;

function LocalToWebFileName(const LocalFileName: string): string;
begin
  Result := StringReplace(LocalFileName, '\', '/', [rfReplaceAll]);
end;

end.
