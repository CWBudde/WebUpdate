program Updater;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
  WinApi.Windows,
  WinApi.TlHelp32,
  WebUpdate.Classes.Updater in '..\Common\WebUpdate.Classes.Updater.pas',
  WebUpdate.JSON.Serializer in '..\Common\WebUpdate.JSON.Serializer.pas',
  WebUpdate.JSON.Channel in '..\Common\WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in '..\Common\WebUpdate.JSON.Channels.pas',
  WebUpdate.MD5 in '..\Common\WebUpdate.MD5.pas';

resourcestring
  RStrMD5MismatchUpdate = 'MD5 mismatch, update might be corrupt!';
  RStrUnknownOption = 'Unknown option: %s!';
  RStrUnknownCommand = 'Unknown command: %s';
  RStrChannelDefinitionError = 'Could not load channel definition!';
  RStrSetupLoadError = 'Could not load setup!';

var
  WebUpdater: TWebUpdater;
  MainAppExecutable: string;
  MainAppWindowCaption: string;

procedure KillProcess(ProcessID: Integer);
var
  ProcessHandle: THandle;
  ExitCode: Integer;
begin
  // get process handle
  ProcessHandle := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_VM_OPERATION
    or PROCESS_VM_WRITE or PROCESS_VM_READ or PROCESS_TERMINATE, False,
    ProcessID);

  // eventually close process
  if (ProcessHandle > 0) then
    try
      ExitCode := 0;
      GetExitCodeProcess(ProcessHandle, DWORD(ExitCode));
      TerminateProcess(ProcessHandle, ExitCode);
    finally
      CloseHandle(ProcessHandle);
    end;
end;

function GetProcessIDForExecutable(ExecutableFileName: string): Cardinal;
var
  ContinueLoop: Boolean;
  SnapshotHandle: THandle;
  ProcessEntry32: TProcessEntry32;
  Found: Boolean;
begin
  // initialization
  ProcessEntry32.dwSize := SizeOf(TProcessEntry32);
  Found := False;

  // get snapshot
  SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapshotHandle > 0 then
    try
      ContinueLoop := Process32First(SnapshotHandle, ProcessEntry32);
      while Integer(ContinueLoop) <> 0 do
      begin
        if SameText(ExtractFileName(ProcessEntry32.szExeFile), ExecutableFileName) then
        begin
          Found := True;
          Break;
        end;
        ContinueLoop := Process32Next(SnapshotHandle, ProcessEntry32);
      end;
      if not Found then
        Exit(0);
    finally
      CloseHandle(SnapshotHandle);
    end;

  Result := ProcessEntry32.th32ProcessID;
end;

function GetProcessIDForWindow(Caption: string): Cardinal;
var
  WinHwnd: HWND;
begin
  Result := 0;

  // check for opened main application
  WinHwnd := FindWindow(nil, PWideChar(Caption));
  if not (IsWindow(WinHwnd)) then
    Exit;

  // get process ID
  GetWindowThreadProcessID(WinHwnd, @Result);
end;

procedure CloseMainApplication;
var
  ProcessID: Integer;
  Counter: Integer;
begin
  Counter := 0;
  repeat
    ProcessID := 0;
    if MainAppExecutable <> '' then
      ProcessID := GetProcessIDForExecutable(MainAppExecutable)
    else if MainAppWindowCaption <> '' then
      ProcessID := GetProcessIDForWindow(MainAppWindowCaption);

    // check if process has been found
    if ProcessID = 0 then
      Exit;

    Sleep(100);
    Inc(Counter);
  until Counter >= 50;

  KillProcess(ProcessID);
end;

procedure ScanParameters;
var
  Index, Delay: Integer;
  Text: string;
  EqPos: Integer;
begin
  Delay := 99;
  for Index := 1 to ParamCount do
  begin
    Text := ParamStr(Index);
    if Text[1] = '-' then
    begin
      // remove options identifier and get colon pos
      Delete(Text, 1, 1);
      EqPos := Pos('=', Text);

      if StartsText('u=', Text) then
        WebUpdater.BaseURL := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('f=', Text) then
        WebUpdater.ChannelsFileName := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('c=', Text) then
        WebUpdater.ChannelName := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('d=', Text) then
        Delay := StrToInt(Copy(Text, EqPos, Length(Text) - EqPos))
      else if StartsText('l=', Text) then
        WebUpdater.LocalChannelFileName := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('e=', Text) then
        MainAppExecutable := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('w=', Text) then
        MainAppWindowCaption := Copy(Text, EqPos, Length(Text) - EqPos)
      else
      begin
        WriteLn(Format(RStrUnknownOption, [Text]));
        Exit;
      end;
    end
    else
      WriteLn(Format(RStrUnknownCommand, [Text]));
  end;

  // check for command-line calling
  if ParamCount >= 1 then
  begin
    Sleep(1 + Delay);

    // close main application
    CloseMainApplication;

    // perform web update
    WebUpdater.PerformWebUpdate;
  end
  else
  begin
    // Write usage
  end;
end;

begin
  try
    WebUpdater := TWebUpdater.Create;
    try
      ScanParameters;
    finally
      WebUpdater.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
