program Updater;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils,
{$IFDEF MSWINDOWS}
  WinApi.Windows,
  WinApi.TlHelp32,
{$ENDIF}
  WebUpdate.Classes.Updater in '..\Common\WebUpdate.Classes.Updater.pas',
  WebUpdate.JSON.Serializer in '..\Common\WebUpdate.JSON.Serializer.pas',
  WebUpdate.JSON.Channel in '..\Common\WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in '..\Common\WebUpdate.JSON.Channels.pas',
  WebUpdate.MD5 in '..\Common\WebUpdate.MD5.pas';

resourcestring
  RStrMD5MismatchUpdate = 'Error: MD5 mismatch, update might be corrupt!';
  RStrUnknownOption = 'Error: Unknown option: %s!';
  RStrUnknownCommand = 'Error: Unknown command: %s';
  RStrChannelDefinitionError = 'Error: Could not load channel definition!';
  RStrSetupLoadError = 'Error: Could not load setup!';
  RStrNoURLSpecified = 'Error: No URL specified!';

var
  Delay: Integer;
  WebUpdater: TWebUpdater;
  MainAppExecutable: string;
  MainAppWindowCaption: string;

{$IFDEF MSWINDOWS}
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

    Sleep(1 + Delay);
    Inc(Counter);
  until Counter >= 10;

  KillProcess(ProcessID);
end;
{$ENDIF}

procedure WriteUsage;
begin
  WriteLn('Syntax: Updater.exe [-options]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('--------');
  WriteLn('');
  WriteLn('  -u=URL                       (Base URL for JSON files)');
  WriteLn('  -c=Channel                   (Update Channel, default is ''Stable'')');
  WriteLn('  -f=FileName (Channels)       (Filename of channels definition file)');
  WriteLn('  -d=Delay (Integer)           (Time in milliseconds before updating starts)');
  WriteLn('  -l=FileName                  (Local filename of current setup)');
  WriteLn('  -e=ExeFileName               (Name of main application executable)');
  WriteLn('  -w=WindowCaption             (Caption of main application window)');
  WriteLn('');
  WriteLn('Example:');
  WriteLn('--------');
  WriteLn('');
  WriteLn('  Updater.exe -c=Nightly');
  WriteLn('');
end;

procedure ScanParameters;
var
  Index: Integer;
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
{$IFDEF MSWINDOWS}
      else if StartsText('e=', Text) then
        MainAppExecutable := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('w=', Text) then
        MainAppWindowCaption := Copy(Text, EqPos, Length(Text) - EqPos)
{$ENDIF}
      else
      begin
        WriteLn(Format(RStrUnknownOption, [Text]));
        WriteLn('');
        WriteUsage;
        Halt(100);
      end;
    end
    else
      WriteLn(Format(RStrUnknownCommand, [Text]));
        WriteLn('');
        WriteUsage;
        Halt(101);
  end;

  if WebUpdater.BaseURL = '' then
  begin
    WriteLn(RStrNoURLSpecified);
    WriteLn('');
    WriteUsage;
    Halt(102);
  end;

  if WebUpdater.ChannelsFileName = '' then
  begin
    WriteLn('Error: No file name for channels definition specified!');
    WriteLn('');
    WriteUsage;
    Halt(103);
  end;
end;

begin
  try
    WebUpdater := TWebUpdater.Create;
    try
      ScanParameters;

      // give main application some time to close
      Sleep(1 + Delay);

{$IFDEF MSWINDOWS}
      // close main application
      CloseMainApplication;
{$ENDIF}

      // perform web update
      WebUpdater.PerformWebUpdate;
    finally
      WebUpdater.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
