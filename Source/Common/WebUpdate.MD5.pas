unit WebUpdate.MD5;

interface

uses
  System.Classes, System.SysUtils, IdHashMessageDigest, IdGlobal;

function MD5(const FileName: string): string; overload;
function MD5(Stream: TStream): string; overload;

implementation

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

end.
