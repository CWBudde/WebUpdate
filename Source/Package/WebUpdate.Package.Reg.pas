unit WebUpdate.Package.Reg;

interface

uses
  System.Classes, WebUpdate.Package.Component;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('WebUpdate', [TWebUpdateComponent]);
end;    

end.
