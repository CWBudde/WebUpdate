unit WebUpdate.GUI.Updater;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, WebUpdate.JSON.Channels, WebUpdate.JSON.Channel,
  WebUpdate.Classes.Updater, VirtualTrees, Vcl.ImgList;

type
  TNodeFileItem = record
    FileItem: TFileItem;
  end;
  PNodeFileItem = ^TNodeFileItem;

  TFormWebUpdate = class(TForm)
    ButtonClose: TButton;
    ButtonNext: TButton;
    CheckBoxStartApplication: TCheckBox;
    ComboBoxChannels: TComboBox;
    ImageHeader: TImage;
    ImageList: TImageList;
    LabelCurrentFile: TLabel;
    LabelFileList: TLabel;
    LabelHeader: TLabel;
    LabelRemainingTime: TLabel;
    LabelSelectChannel: TLabel;
    LabelSummary: TLabel;
    LabelTotalStatus: TLabel;
    PageControl: TPageControl;
    PanelControl: TPanel;
    PanelHeader: TPanel;
    ProgressBarCurrent: TProgressBar;
    ProgressBarTotal: TProgressBar;
    RadioButtonAlternative: TRadioButton;
    RadioButtonStable: TRadioButton;
    TabFileList: TTabSheet;
    TabProgress: TTabSheet;
    TabSelectChannel: TTabSheet;
    TabSummary: TTabSheet;
    TreeFiles: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
    procedure ButtonFinishClick(Sender: TObject);
    procedure ButtonNextClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure RadioButtonChannelClick(Sender: TObject);
    procedure TabProgressShow(Sender: TObject);
    procedure TabSelectChannelShow(Sender: TObject);
    procedure TabFileListShow(Sender: TObject);
    procedure TabSummaryShow(Sender: TObject);
    procedure TreeFilesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeFilesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
  private
    FWebUpdater: TWebUpdater;
    FCommandLine: Boolean;
    FMainAppWindowCaption: string;
    FMainAppExecutable: string;
    FDelay: Integer;
    FVerbose: Boolean;

    procedure ProgressEventHandler(Sender: TObject; Progress: Integer; ByteCount: Integer);
    procedure FileNameProgressEventHandler(Sender: TObject; const FileName: TFileName);
    procedure WebUpdateCompleteEventHandler(Sender: TObject);
    procedure MD5MismatchHandler(Sender: TObject; const FileName: TFileName; var Ignore: Boolean);
  public
    procedure ScanCommandLineParameters;
    procedure PerformWebUpdate;
    function CloseMainApplication: Boolean;
  end;

var
  FormWebUpdate: TFormWebUpdate;

implementation

{$R *.dfm}

uses
  System.StrUtils, WinAPI.ShellAPI, WinAPI.TlHelp32, dwsUtils, WebUpdate.MD5;

resourcestring
  RStrMD5MismatchUpdate = 'MD5 mismatch, update might be corrupt!';
  RStrUnknownOption = 'Unknown option: %s!';
  RStrUnknownCommand = 'Unknown command: %s';
  RStrChannelDefinitionError = 'Could not load channel definition!';
  RStrSetupLoadError = 'Could not load setup!';

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
        if UnicodeSameText(ExtractFileName(ProcessEntry32.szExeFile), ExecutableFileName) then
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


{ TFormWebUpdate }

procedure TFormWebUpdate.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := TabSelectChannel;
  TreeFiles.NodeDataSize := SizeOf(TNodeFileItem);

  FMainAppWindowCaption := '';
  FMainAppExecutable := '';
  FVerbose := False;

  // create WebUpdater
  FWebUpdater := TWebUpdater.Create;
  FWebUpdater.OnProgress := ProgressEventHandler;
  FWebUpdater.OnFileNameProgress := FileNameProgressEventHandler;
  FWebUpdater.OnDone := WebUpdateCompleteEventHandler;
  FWebUpdater.OnMD5Mismatch := MD5MismatchHandler;

  ScanCommandLineParameters;
end;

procedure TFormWebUpdate.FormDestroy(Sender: TObject);
begin
  FWebUpdater.Free;
end;

procedure TFormWebUpdate.ButtonAbortClick(Sender: TObject);
begin
  if Assigned(FWebUpdater) then
    FWebUpdater.Abort;
  Close;
end;

procedure TFormWebUpdate.ButtonFinishClick(Sender: TObject);
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

procedure TFormWebUpdate.ButtonNextClick(Sender: TObject);
begin
  if RadioButtonStable.Checked then
    FWebUpdater.ChannelName := 'Stable'
  else
    FWebUpdater.ChannelName := ComboBoxChannels.Text;

  // switch pages to file list / progress tab
  if FVerbose then
    PageControl.ActivePage := TabFileList
  else
  begin
    PerformWebUpdate;

    PageControl.ActivePage := TabProgress;
    Update;
  end;
end;

procedure TFormWebUpdate.ButtonStartClick(Sender: TObject);
begin
  PerformWebUpdate;

  // switch pages to progress tab
  PageControl.ActivePage := TabProgress;
  Update;
end;

procedure TFormWebUpdate.FileNameProgressEventHandler(Sender: TObject;
  const FileName: TFileName);
begin
  LabelCurrentFile.Caption := 'Current File: ' + FileName;
end;

procedure TFormWebUpdate.ProgressEventHandler(Sender: TObject;
  Progress: Integer; ByteCount: Integer);
begin
  ProgressBarCurrent.Position := Progress;
  ProgressBarTotal.Position := ProgressBarTotal.Position + ByteCount;
end;

procedure TFormWebUpdate.WebUpdateCompleteEventHandler(Sender: TObject);
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
  case MessageDlg(RStrMD5MismatchUpdate,
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
      else if StartsText('d=', Text) then
        FDelay := StrToInt(Copy(Text, EqPos, Length(Text) - EqPos))
      else if StartsText('l=', Text) then
        FWebUpdater.LocalChannelFileName := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('e=', Text) then
        FMainAppExecutable := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('w=', Text) then
        FMainAppWindowCaption := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('verbose', Text) then
        FVerbose := True
      else
      begin
        MessageDlg(Format(RStrUnknownOption, [Text]), mtError, [mbOK], 0);
        Exit;
      end;
    end
    else
      MessageDlg(Format(RStrUnknownCommand, [Text]), mtError, [mbOK], 0);
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

procedure TFormWebUpdate.TabProgressShow(Sender: TObject);
begin
  ProgressBarTotal.Max := FWebUpdater.TotalBytes;

  ButtonNext.Visible := False;
  ButtonClose.Caption := '&Abort';
  ButtonClose.OnClick := ButtonAbortClick;
end;

procedure TFormWebUpdate.TabSelectChannelShow(Sender: TObject);
begin
  ButtonNext.Visible := True;
  if FVerbose then
    ButtonNext.Caption := '&Next >'
  else
    ButtonNext.Caption := '&Start >';
  ButtonNext.OnClick := ButtonNextClick;
  ButtonClose.Caption := '&Abort';
  ButtonClose.OnClick := ButtonAbortClick;
end;

procedure TFormWebUpdate.TabFileListShow(Sender: TObject);
var
  FileList: TFileItemList;
  FileItem: TFileItem;
  Node: PVirtualNode;
  NodeData: PNodeFileItem;
begin
  ButtonNext.Visible := True;
  ButtonNext.Caption := '&Start >';
  ButtonNext.OnClick := ButtonStartClick;
  ButtonClose.Caption := '&Abort';
  ButtonClose.OnClick := ButtonAbortClick;

  TreeFiles.BeginUpdate;
  TreeFiles.Clear;
  try
    FileList := FWebUpdater.FileItemList;
    for FileItem in FileList do
    begin
      Node := TreeFiles.AddChild(TreeFiles.RootNode);
      NodeData := TreeFiles.GetNodeData(Node);
      NodeData^.FileItem := FileItem;
    end;
  finally
    TreeFiles.EndUpdate
  end;
end;

procedure TFormWebUpdate.TabSummaryShow(Sender: TObject);
begin
  ButtonNext.Visible := False;
  ButtonClose.Caption := '&Finish';
  ButtonClose.OnClick := ButtonFinishClick;
end;

procedure TFormWebUpdate.TreeFilesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PNodeFileItem;
begin
  // Ignore overlay and other columns
  if (Kind = ikOverlay) or (Column <> 0) then
    Exit;

  NodeData := TreeFiles.GetNodeData(Node);

  case NodeData.FileItem.Action of
    faAdd:
      ImageIndex := 0;
    faChange:
      ImageIndex := 1;
    faDelete:
      ImageIndex := 2;
    faVerify:
      ImageIndex := 3;
  end;
end;

procedure TFormWebUpdate.TreeFilesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PNodeFileItem;
begin
  CellText := '';
  NodeData := TreeFiles.GetNodeData(Node);
  case Column of
    0:
      CellText := NodeData^.FileItem.FileName;
    1:
      if NodeData^.FileItem.Modified > 0 then
        CellText := DateTimeToStr(NodeData^.FileItem.Modified);
    2:
      if NodeData^.FileItem.FileSize > 0 then
        CellText := IntToStr(NodeData^.FileItem.FileSize);
  end;
end;

function TFormWebUpdate.CloseMainApplication: Boolean;
var
  ProcessID: Integer;
  Counter: Integer;
begin
  Result := False;
  Counter := 0;
  repeat
    ProcessID := 0;
    if FMainAppExecutable <> '' then
      ProcessID := GetProcessIDForExecutable(FMainAppExecutable)
    else if FMainAppWindowCaption <> '' then
      ProcessID := GetProcessIDForWindow(FMainAppWindowCaption);

    // check if process has been found
    if ProcessID = 0 then
      Exit(True);

    // show dialog
    case MessageDlg('Main application is already running!' + #13#10#13#10 +
      'Force closing the main application?', mtWarning, [mbYes, mbAbort,
      mbRetry, mbIgnore], 0) of
      mrRetry:
        Sleep(1 + FDelay);
      mrYes:
        KillProcess(ProcessID);
      mrAbort:
        Exit(False);
      mrIgnore:
        Exit(True);
    end;
    Inc(Counter);
  until Counter >= 10;
end;

procedure TFormWebUpdate.PerformWebUpdate;
begin
  // give other applications time to close
  Sleep(1 + FDelay);

  // first check if main application is still running
  if CloseMainApplication then
    FWebUpdater.PerformWebUpdate;
end;

end.
