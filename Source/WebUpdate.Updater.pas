unit WebUpdate.Updater;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, WebUpdate.JSON.Channels, WebUpdate.JSON.Channel,
  WebUpdate.Classes.Updater;

type
  TFormWebUpdate = class(TForm)
    ButtonAbort: TButton;
    ButtonCancel: TButton;
    ButtonClose: TButton;
    ButtonStart: TButton;
    CheckBoxStartApplication: TCheckBox;
    ComboBoxChannels: TComboBox;
    ImageHeader: TImage;
    LabelCurrentFile: TLabel;
    LabelHeader: TLabel;
    LabelRemainingTime: TLabel;
    LabelSelectChannel: TLabel;
    LabelSummary: TLabel;
    LabelTotalStatus: TLabel;
    PageControl: TPageControl;
    PanelHeader: TPanel;
    ProgressBarCurrent: TProgressBar;
    ProgressBarTotal: TProgressBar;
    RadioButtonAlternative: TRadioButton;
    RadioButtonStable: TRadioButton;
    TabProgress: TTabSheet;
    TabSelectChannel: TTabSheet;
    TabSummary: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure RadioButtonChannelClick(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FWebUpdater: TWebUpdater;
    FCommandLine: Boolean;
    FMainAppCaption: string;

    procedure ProgressEventHandler(Sender: TObject; Progress: Integer; ByteCount: Integer);
    procedure FileNameProgressEventHandler(Sender: TObject; const FileName: TFileName);
    procedure DoneEventHandler(Sender: TObject);
    procedure MD5MismatchHandler(Sender: TObject; const FileName: TFileName; var Ignore: Boolean);
  public
    function CheckMainApplication: Boolean;
    procedure ScanCommandLineParameters;
    procedure PerformWebUpdate;
  end;

var
  FormWebUpdate: TFormWebUpdate;

implementation

{$R *.dfm}

uses
  System.StrUtils, WinAPI.ShellAPI, dwsUtils, WebUpdate.MD5;

resourcestring
  RStrMainApplicationCaption = 'Web Update Tool';

resourcestring
  RStrChannelDefinitionError = 'Could not load channel definition!';
  RStrSetupLoadError = 'Could not load setup!';

{ TFormWebUpdate }

procedure TFormWebUpdate.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := TabSelectChannel;

  FMainAppCaption := RStrMainApplicationCaption;

  // create WebUpdater
  FWebUpdater := TWebUpdater.Create;
  FWebUpdater.OnProgress := ProgressEventHandler;
  FWebUpdater.OnFileNameProgress := FileNameProgressEventHandler;
  FWebUpdater.OnDone := DoneEventHandler;
  FWebUpdater.OnMD5Mismatch := MD5MismatchHandler;

  ScanCommandLineParameters;
end;

procedure TFormWebUpdate.FormDestroy(Sender: TObject);
begin
  FWebUpdater.Free;
end;

procedure TFormWebUpdate.ButtonAbortClick(Sender: TObject);
begin
  Abort;
  Close;
end;

procedure TFormWebUpdate.ButtonCloseClick(Sender: TObject);
var
  AppName: TFileName;
begin
  // now eventually execute specified application
  if CheckBoxStartApplication.Visible and CheckBoxStartApplication.Checked then
  begin
    AppName := FWebUpdater.MainAppFileName;
    if AppName <> '' then
      ShellExecute(Application.Handle, 'open', PChar(AppName), nil,
        PChar(ExtractFileDir(AppName)), SW_SHOW);
  end;

  Close;
end;

procedure TFormWebUpdate.ButtonStartClick(Sender: TObject);
begin
  if RadioButtonStable.Checked then
    FWebUpdater.ChannelName := 'Stable'
  else
    FWebUpdater.ChannelName := ComboBoxChannels.Text;

  // first check if main application is still running
  if CheckMainApplication then
    Exit;

  // actually perform web update
  FWebUpdater.PerformWebUpdate;

  // switch pages to progress tab
  PageControl.ActivePage := TabProgress;
  Update;
end;

procedure TFormWebUpdate.FileNameProgressEventHandler(Sender: TObject;
  const FileName: TFileName);
begin
  LabelCurrentFile.Caption := 'Current File: ' + FileName;
end;

procedure TFormWebUpdate.PerformWebUpdate;
begin
  // first check if main application is still running
  if CheckMainApplication then
    Exit;

  // now perform web update
  FWebUpdater.PerformWebUpdate;
end;

procedure TFormWebUpdate.ProgressEventHandler(Sender: TObject;
  Progress: Integer; ByteCount: Integer);
begin
  ProgressBarCurrent.Position := Progress;
  ProgressBarTotal.Position := ProgressBarTotal.Position + ByteCount;
end;

procedure TFormWebUpdate.DoneEventHandler(Sender: TObject);
begin
  // allow starting the application, if an (existing!) app is specified
  CheckBoxStartApplication.Visible := FileExists(FWebUpdater.MainAppFileName);
  CheckBoxStartApplication.Checked := CheckBoxStartApplication.Visible;

  // switch pages to summary tab
  PageControl.ActivePage := TabSummary;
end;

procedure TFormWebUpdate.MD5MismatchHandler(Sender: TObject;
  const FileName: TFileName; var Ignore: Boolean);
begin
  case MessageDlg('MD5 mismatch, update might be corrupt!',
    mtWarning, [mbAbort, mbIgnore], 0) of
    mrAbort:
      Ignore := False;
    mrIgnore:
      Ignore := True;
  end;
end;

procedure TFormWebUpdate.RadioButtonChannelClick(Sender: TObject);
begin
  ComboBoxChannels.Visible := RadioButtonAlternative.Checked;
end;

procedure TFormWebUpdate.ScanCommandLineParameters;
var
  Index: Integer;
  Text: string;
  EqPos: Integer;
  ChannelNames: TStringList;
begin
  FCommandLine := ParamCount >= 1;

  for Index := 1 to ParamCount do
  begin
    Text := ParamStr(Index);
    if Text[1] = '-' then
    begin
      // remove options identifier and get colon pos
      Delete(Text, 1, 1);
      EqPos := Pos('=', Text);

      if StartsText('u=', Text) then
        FWebUpdater.BaseURL := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('f=', Text) then
        FWebUpdater.ChannelsFileName := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('c=', Text) then
        FWebUpdater.ChannelName := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('l=', Text) then
        FWebUpdater.LocalFileName := Copy(Text, EqPos, Length(Text) - EqPos)
      else
      begin
        MessageDlg(Format('Unknown option: %s!', [Text]), mtError, [mbOK], 0);
        Exit;
      end;
    end
    else
      MessageDlg(Format('Unknown command: %s', [Text]), mtError, [mbOK], 0);
  end;

  // check for command-line calling
  if FCommandLine then
  begin
    PerformWebUpdate;

    // switch pages to progress tab
    PageControl.ActivePage := TabProgress;
    Update;
  end
  else
  begin
    ChannelNames := TStringList.Create;
    try
      FWebUpdater.GetChannelNames(ChannelNames);
      for Index := 0 to ChannelNames.Count - 1 do
        if not SameText(ChannelNames[Index], 'Stable') then
          ComboBoxChannels.Items.Add(ChannelNames[Index]);
      ComboBoxChannels.ItemIndex := 0;
    finally
      ChannelNames.Free;
    end;
  end;
end;

function TFormWebUpdate.CheckMainApplication: Boolean;
var
  ProcessHandle: THandle;
  WinHwnd: HWND;
  ProcessID, ExitCode: Integer;
begin
  // check whether to perform check this at all?
  if FMainAppCaption = '' then
    Exit(False);

  repeat
    // check for opened main application
    WinHwnd := FindWindow(nil, PWideChar(FMainAppCaption));
    if not (IsWindow(WinHwnd)) then
      Exit(False);

    // show dialog
    case MessageDlg('Main application is already running!' + #13#10#13#10 +
      'Force closing the main application?', mtWarning, [mbYes, mbAbort,
      mbRetry, mbIgnore], 0) of
      mrYes:
        Break;
      mrAbort:
        Exit(True);
      mrIgnore:
        Exit(False);
    end;
    Break;
  until False;

  // get process ID
  ProcessID := 0;
  GetWindowThreadProcessID(WinHwnd, @ProcessID);

  // get process handle
  ProcessHandle := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_VM_OPERATION
    or PROCESS_VM_WRITE or PROCESS_VM_READ or PROCESS_TERMINATE, False, ProcessID);

  // eventually close process
  if (ProcessHandle > 0) then
  begin
    ExitCode := 0;
    GetExitCodeProcess(ProcessHandle, DWORD(ExitCode));
    TerminateProcess(ProcessHandle, ExitCode);
    CloseHandle(ProcessHandle);
  end;
  Result := False;
end;

end.
