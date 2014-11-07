unit WebUpdate.GUI.CommandLine;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormCommandLine = class(TForm)
    MemoCommandLine: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

implementation

uses
  System.Types;

{$R *.dfm}

procedure TFormCommandLine.FormCreate(Sender: TObject);
begin
  MemoCommandLine.Lines[3] := Format(MemoCommandLine.Lines[3],
    [ExtractFileName(ParamStr(0))]);
  MemoCommandLine.Lines[26] := Format(MemoCommandLine.Lines[26],
    [ExtractFileName(ParamStr(0))]);
  MemoCommandLine.CaretPos := Point(0, MemoCommandLine.Lines.Count);
end;

end.

