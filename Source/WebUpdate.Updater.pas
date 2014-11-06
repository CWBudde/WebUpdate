unit WebUpdate.Updater;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, IdHTTP, IdComponent, dwsComp, dwsExprs, dwsErrors,
  WebUpdate.JSON.Channels, WebUpdate.JSON.Channel;

type
  TFileDownloadThread = class(TThread)
  type
    TProgressEvent = procedure (Sender: TObject; Progress: Integer; ByteCount: Integer) of object;
    TFileChangedEvent = procedure (Sender: TObject; const FileName: TFileName) of object;
    TScriptErrorsEvent = procedure (Sender: TObject; MessageList: TdwsMessageList) of object;
  private
    FFilesToAdd: TWebUpdateFileItems;
    FFilesToDelete: TWebUpdateFileItems;
    FHttp: TIdHttp;
    FLocalPath: string;
    FBasePath: string;
    FPostScript: string;
    FPreScript: string;
    FLastWorkCount: Integer;
    FOnFileChanged: TFileChangedEvent;
    FOnProgress: TProgressEvent;
    FOnDone: TNotifyEvent;
    FOnScriptErrors: TScriptErrorsEvent;
    procedure HttpWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  protected
    procedure RunScript(SourceCode: string);
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property FilesToAdd: TWebUpdateFileItems read FFilesToAdd;
    property FilesToDelete: TWebUpdateFileItems read FFilesToDelete;
    property BasePath: string read FBasePath write FBasePath;
    property LocalPath: string read FLocalPath write FLocalPath;

    property PreScript: string read FPreScript write FPreScript;
    property PostScript: string read FPostScript write FPostScript;

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnFileChanged: TFileChangedEvent read FOnFileChanged write FOnFileChanged;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnScriptErrors: TScriptErrorsEvent read FOnScriptErrors write FOnScriptErrors;
  end;

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
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure RadioButtonChannelClick(Sender: TObject);
    procedure ButtonAbortClick(Sender: TObject);
  private
    FChannels: TWebUpdateChannels;
    FCurrentSetup: TWebUpdateChannelSetup;
    FNewSetup: TWebUpdateChannelSetup;
    FThread: TFileDownloadThread;

    FChannelBasePath: string;
    FLocalBasePath: string;
    FMainAppCaption: string;

    procedure SelectChannel;

    procedure ProgressEventHandler(Sender: TObject; Progress: Integer; ByteCount: Integer);
    procedure FileChangedEventHandler(Sender: TObject; const FileName: TFileName);
    procedure DoneEventHandler(Sender: TObject);
  public
    procedure PerformWebUpdate;
    function CheckMainApplication: Boolean;

    procedure LoadChannelsFromURI(const URI: string);
    procedure LoadChannelsFromString(const Text: string);
    procedure LoadSetupFromURI(const URI: string);

    procedure ScanParameters;
  end;

var
  FormWebUpdate: TFormWebUpdate;

implementation

{$R *.dfm}

uses
  dwsUtils, ShellAPI, WebUpdate.MD5;

resourcestring
  RStrBaseURL = 'http://www.savioursofsoul.de/Christian/WebUpdate/';
  RStrBaseFile = 'Channels.json';
  RStrMainApplicationCaption = 'Web Update Tool';

resourcestring
  RStrChannelDefinitionError = 'Could not load channel definition!';
  RStrSetupLoadError = 'Could not load setup!';

{ TFileDownloadThread }

constructor TFileDownloadThread.Create;
begin
  FFilesToAdd := TWebUpdateFileItems.Create;
  FFilesToDelete := TWebUpdateFileItems.Create;

  FPreScript := '';
  FPostScript := '';
  FLastWorkCount := 0;

  inherited Create(False);
end;

destructor TFileDownloadThread.Destroy;
begin
  FFilesToAdd.Free;
  inherited;
end;

procedure TFileDownloadThread.HttpWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  ContentLength: Int64;
  Encoding: string;
begin
  // check whether a HTTP client is assigned
  if not Assigned(FHttp) then
    Exit;

  // check whether a progress event is assigned
  if not Assigned(FOnProgress) then
    Exit;

  Encoding := FHttp.Response.TransferEncoding;
  ContentLength := FHttp.Response.ContentLength;

  if (Pos('chunked', LowerCase(Encoding)) = 0) and (ContentLength > 0) then
  begin
    Synchronize(procedure
    begin
      FOnProgress(Self, 100 * AWorkCount div ContentLength, AWorkCount - FLastWorkCount);
    end);
    FLastWorkCount := AWorkCount;
  end;
end;

procedure TFileDownloadThread.RunScript(SourceCode: string);
var
  CompiledProgram: IdwsProgram;
begin
  with TDelphiWebScript.Create(nil) do
  try
    CompiledProgram := Compile(SourceCode);
    if CompiledProgram.Msgs.HasErrors then
    begin
      if Assigned(OnScriptErrors) then
        OnScriptErrors(Self, CompiledProgram.Msgs);

      Exit;
    end;

    // now execute script
    CompiledProgram.Execute;
  finally
    Free;
  end;
end;

procedure TFileDownloadThread.Execute;
var
  MS: TMemoryStream;
  Item: TWebUpdateFileItem;
  Hash: Integer;
begin
  // eventually run pre script
  if FPreScript <> '' then
    RunScript(FPreScript);

  FHttp := TIdHTTP.Create(nil);
  try
    FHttp.OnWork := HttpWork;
    MS := TMemoryStream.Create;
    try
      for Item in FFilesToAdd do
      begin
        // eventually call 'file changed' event
        if Assigned(FOnFileChanged) then
          Synchronize(procedure
          begin
            FOnFileChanged(Self, Item.FileName);
          end);

        // clear buffer / reset last work count
        MS.Clear;
        FLastWorkCount := 0;

        // check if terminated
        if Terminated then
          Exit;

        // download file
        FHttp.Get(FBasePath + Item.FileName, MS);

        // check if terminated
        if Terminated then
          Exit;

        // eventually check MD5 hash
        if Item.MD5Hash <> 0 then
        begin
          Hash := MD5(MS);
          if Hash <> Item.MD5Hash then
            case MessageDlg('MD5 mismatch, update might be corrupt!',
              mtWarning, [mbAbort, mbIgnore], 0) of
              mrAbort:
                Exit;
            end;
        end;

        // save downloaded file
        MS.SaveToFile(FLocalPath + StringReplace(Item.FileName, '/', '\', [rfReplaceAll]));

        // check if terminated
        if Terminated then
          Exit;
      end;
    finally
      MS.Free;
    end;
  finally
    FreeAndNil(FHttp);
  end;

  // check if terminated
  if Terminated then
    Exit;

  // eventually run post script
  if FPostScript <> '' then
    RunScript(FPostScript);

  // check if terminated
  if Terminated then
    Exit;

  // now delete files no longer needed
  for Item in FFilesToDelete do
    DeleteFile(FLocalPath + StringReplace(Item.FileName, '/', '\', [rfReplaceAll]));

  // check if terminated
  if Terminated then
    Exit;

  // eventually call 'done' event
  if Assigned(FOnDone) then
    Synchronize(procedure
    begin
      FOnDone(Self);
    end);
end;


{ TFormWebUpdate }

procedure TFormWebUpdate.FormCreate(Sender: TObject);
begin
  FChannelBasePath := '';
  FLocalBasePath := ExtractFilePath(ParamStr(0));
  FMainAppCaption := RStrMainApplicationCaption;

  FChannels := TWebUpdateChannels.Create;
  FNewSetup := TWebUpdateChannelSetup.Create;

  PageControl.ActivePage := TabSelectChannel;

  ScanParameters;
end;

procedure TFormWebUpdate.FormDestroy(Sender: TObject);
begin
  FChannels.Free;
  FCurrentSetup.Free;
  FNewSetup.Free;
  FreeAndNil(FThread);
end;

procedure TFormWebUpdate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;
end;

procedure TFormWebUpdate.ButtonAbortClick(Sender: TObject);
begin
  if Assigned(FThread) then
    FThread.Terminate;

  Close;
end;

procedure TFormWebUpdate.ButtonCloseClick(Sender: TObject);
begin
  // now eventually execute specified application
  if CheckBoxStartApplication.Visible and CheckBoxStartApplication.Checked then
    ShellExecute(Application.Handle, 'open', PChar(FLocalBasePath +
      StringReplace(FNewSetup.AppName, '/', '\', [rfReplaceAll])), nil,
      PChar(FLocalBasePath), SW_SHOW);

  Close;
end;

procedure TFormWebUpdate.ButtonStartClick(Sender: TObject);
begin
  SelectChannel;

  PerformWebUpdate;

  // switch pages to progress tab
  PageControl.ActivePage := TabProgress;
  Update;
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
end;

procedure TFormWebUpdate.FileChangedEventHandler(Sender: TObject;
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

procedure TFormWebUpdate.DoneEventHandler(Sender: TObject);
begin
  FNewSetup.SaveToFile(FLocalBasePath + 'WebUpdate.json');

  FThread := nil;

  // allow starting the application, if an (existing!) app is specified
  CheckBoxStartApplication.Visible := FileExists(
    FLocalBasePath + StringReplace(FNewSetup.AppName, '/', '\', [rfReplaceAll]));
  CheckBoxStartApplication.Checked := CheckBoxStartApplication.Visible;

  // switch pages to summary tab
  PageControl.ActivePage := TabSummary;
end;

procedure TFormWebUpdate.RadioButtonChannelClick(Sender: TObject);
begin
  ComboBoxChannels.Visible := RadioButtonAlternative.Checked;
end;

procedure TFormWebUpdate.SelectChannel;
var
  Item: TWebUpdateChannelItem;
  ChannelName: string;
begin
  if RadioButtonStable.Checked then
    ChannelName := 'Stable'
  else
    ChannelName := ComboBoxChannels.Text;

  FChannelBasePath := RStrBaseURL + ChannelName + '/';

  for Item in FChannels.Items do
    if SameText(Item.Name, ChannelName) then
    begin
      LoadSetupFromURI(FChannelBasePath + Item.FileName);
      Exit;
    end;
end;

procedure TFormWebUpdate.ScanParameters;
var
  Index: Integer;
begin
  for Index := 1 to ParamCount do
  begin

  end;

  LoadChannelsFromURI(RStrBaseURL + RStrBaseFile);

  if FileExists(FLocalBasePath + 'WebUpdate.json') then
  begin
    FCurrentSetup := TWebUpdateChannelSetup.Create;
    FCurrentSetup.LoadFromFile(FLocalBasePath + 'WebUpdate.json');
  end;
end;

procedure TFormWebUpdate.PerformWebUpdate;
var
  TotalBytes: Integer;

  procedure AddFileItem(Item: TWebUpdateFileItem);
  begin
    FThread.FilesToAdd.Add(Item);
    TotalBytes := TotalBytes + Item.FileSize;
  end;

var
  Item, CurrentItem: TWebUpdateFileItem;
  ItemFlag: Boolean;
begin
  if CheckMainApplication then
    Exit;

  // ensure if a new setup is assigned
  if not Assigned(FNewSetup) then
    raise Exception.Create('No new setup is available!');

  FThread := TFileDownloadThread.Create;

  TotalBytes := 0;
  if Assigned(FCurrentSetup) then
    for Item in FNewSetup.Items do
    begin
      // assume the item should be added to the files list
      ItemFlag := True;

      // now check items in current setup to see if this must be replaced
      for CurrentItem in FCurrentSetup.Items do
      begin
        if UnicodeSameText(CurrentItem.FileName, Item.FileName) then
        begin
          ItemFlag := CurrentItem.Modified <> Item.Modified;
          Break;
        end;
      end;

      // eventually add the item and increase the total number of bytes
      if ItemFlag then
        AddFileItem(Item);
    end
  else
    for Item in FNewSetup.Items do
      AddFileItem(Item);

  // add files to delete list (if not part of the "FilesToAdd" list)
  if Assigned(FCurrentSetup) then
    for CurrentItem in FCurrentSetup.Items do
    begin
      // assume file shall be deleted
      ItemFlag := True;

      // check if file is among the add list
      for Item in FThread.FilesToAdd do
        if UnicodeSameText(CurrentItem.FileName, Item.FileName) then
        begin
          ItemFlag := False;
          Break;
        end;

      // add file to delete list
      if ItemFlag then
        FThread.FilesToDelete.Add(CurrentItem);
    end;

  ProgressBarTotal.Max := TotalBytes;

  // specify event handlers
  FThread.OnProgress := ProgressEventHandler;
  FThread.OnFileChanged := FileChangedEventHandler;
  FThread.OnDone := DoneEventHandler;

  // specify paths
  FThread.BasePath := FChannelBasePath;
  FThread.LocalPath := FLocalBasePath;

  FThread.Suspended := False;
end;

procedure TFormWebUpdate.LoadChannelsFromURI(const URI: string);
var
  Http: TIdHTTP;
  Text: string;
  Success: Boolean;
begin
  Http := TIdHTTP.Create(nil);
  try
    repeat
      try
        // get text from URI
        Text := Http.Get(URI);

        // check if text is available (might need check for JSON format)
        Success := Text <> '';
      except
        Success := False;
      end;

      // handle errors
      if not Success then
        case MessageDlg(RStrChannelDefinitionError,
          mtError, [mbAbort, mbRetry], 0) of
          mrAbort:
            begin
              Application.Terminate;
              Exit;
            end;
        end;

    until Success;
  finally
    Http.Free;
  end;

  // load channels from string
  LoadChannelsFromString(Text);
end;

procedure TFormWebUpdate.LoadChannelsFromString(const Text: string);
var
  Item: TWebUpdateChannelItem;
begin
  // load channel definition
  FChannels.LoadFromString(Text);

  // update alternate channel selection
  ComboBoxChannels.Items.Clear;
  for Item in FChannels.Items do
  begin
    if not SameText(Item.Name, 'Stable') then
      ComboBoxChannels.Items.Add(Item.Name);
  end;
  ComboBoxChannels.ItemIndex := 0;
end;

procedure TFormWebUpdate.LoadSetupFromURI(const URI: string);
var
  Http: TIdHTTP;
  Text: string;
  Success: Boolean;
begin
  Http := TIdHTTP.Create(nil);
  try
    repeat
      try
        // get text from URI
        Text := Http.Get(URI);

        // check if text is available (might need check for JSON format)
        Success := Text <> '';
      except
        Success := False;
      end;

      // handle errors
      if not Success then
        case MessageDlg(RStrSetupLoadError, mtError, [mbAbort, mbRetry], 0) of
          mrAbort:
            begin
              Application.Terminate;
              Exit;
            end;
        end;
    until Success;
  finally
    Http.Free;
  end;

  // now load setup from string
  FNewSetup.LoadFromString(Text);
end;

end.
