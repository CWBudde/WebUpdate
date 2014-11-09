unit WebUpdate.MD5;

interface

uses
  System.Classes, System.SysUtils, IdHashMessageDigest, IdGlobal;

function MD5(const FileName: string): Integer; overload;
function MD5(Stream: TStream): Integer; overload;

implementation

function MD5(Stream: TStream): Integer;
var
  IdMD5: TIdHashMessageDigest5;
begin
  IdMD5 := TIdHashMessageDigest5.Create;
  try
    Result := BytesToLongInt(IdMD5.HashStream(Stream));
  finally
    IdMD5.Free;
  end;
end;

function MD5(const FileName: string): Integer;
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

end.
