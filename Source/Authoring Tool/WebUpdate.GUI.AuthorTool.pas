unit WebUpdate.GUI.AuthorTool;

interface

// TODO: Add command line commands

uses
  System.StrUtils, System.Types, System.SysUtils, System.Classes,
  Winapi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Menus, Vcl.ActnList,
  Vcl.StdActns, Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls,
  IdComponent, VirtualTrees, WebUpdate.JSON.Preferences, WebUpdate.JSON.Project,
  WebUpdate.JSON.Channels, WebUpdate.JSON.Channel, WebUpdate.Classes.WebUpdate;

type
  TChannelItem = record
    Name: string;
    FileName: TFileName;
    Modified: TDateTime;
  end;
  PChannelItem = ^TChannelItem;

  TFileItem = record
    FileName: string;
    Hash: Cardinal;
    WebFileName: string;
    Caption: string;
    Modified: TDateTime;
    Size: Integer;
  end;
  PFileItem = ^TFileItem;

  TCheckUpdateThread = class(TThread)
  private
    FWebUpdate: TWebUpdate;
  public
    constructor Create(WebUpdate: TWebUpdate);
    procedure Execute; override;
    procedure Update;
  end;

  TFormWebUpdateTool = class(TForm)
    ActionAddChannel: TAction;
    ActionCheckUpdate: TAction;
    ActionClearAll: TAction;
    ActionCommandLine: TAction;
    ActionCopyUpload: TAction;
    ActionDeleteChannel: TAction;
    ActionDocumentation: TAction;
    ActionFileExit: TFileExit;
    ActionFileOpen: TFileOpen;
    ActionFileOptions: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TFileSaveAs;
    ActionHelpAbout: TAction;
    ActionList: TActionList;
    ActionScanFiles: TAction;
    ActionTakeSnapshot: TAction;
    ActionUpdate: TAction;
    ActionViewChannel: TAction;
    Images: TImageList;
    MainMenu: TMainMenu;
    MenuItemCheckAll: TMenuItem;
    MenuItemCheckForUpdates: TMenuItem;
    MenuItemCheckNone: TMenuItem;
    MenuItemClearAll: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    MenuItemHelpCommandlineSwitches: TMenuItem;
    MenuItemHelpDocumentation: TMenuItem;
    MenuItemProject: TMenuItem;
    MenuItemProjectAddChannel: TMenuItem;
    MenuItemProjectCopyUpload: TMenuItem;
    MenuItemProjectDeleteChannel: TMenuItem;
    MenuItemProjectOptions: TMenuItem;
    MenuItemProjectScanFiles: TMenuItem;
    MenuItemProjectSnapshot: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemsViewChannelFiles: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemToolsUpdate: TMenuItem;
    MenuItemUpdateAlpha: TMenuItem;
    MenuItemUpdateBeta: TMenuItem;
    MenuItemUpdateNightly: TMenuItem;
    MenuItemUpdateStable: TMenuItem;
    MenuItemView: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    PanelChannels: TPanel;
    PanelFiles: TPanel;
    PopupMenu: TPopupMenu;
    ProgressBar: TProgressBar;
    MenuItemScanDirectoriesFiles: TMenuItem;
    Separator1: TToolButton;
    Separator2: TToolButton;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    ToolBarChannels: TToolBar;
    ToolButtonChannelsAdd: TToolButton;
    ToolButtonChannelsDelete: TToolButton;
    ToolButtonChannelsStore: TToolButton;
    ToolButtonCopyUpload: TToolButton;
    ToolButtonScanFiles: TToolButton;
    TreeChannels: TVirtualStringTree;
    TreeFileList: TVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ActionAddChannelExecute(Sender: TObject);
    procedure ActionCheckUpdateExecute(Sender: TObject);
    procedure ActionCopyUploadExecute(Sender: TObject);
    procedure ActionDeleteChannelExecute(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileOptionsExecute(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionScanFilesExecute(Sender: TObject);
    procedure ActionTakeSnapshotExecute(Sender: TObject);
    procedure ActionUpdateExecute(Sender: TObject);
    procedure ActionViewChannelExecute(Sender: TObject);
    procedure MenuItemCheckAllClick(Sender: TObject);
    procedure MenuItemCheckNoneClick(Sender: TObject);
    procedure MenuItemUpdateAlphaClick(Sender: TObject);
    procedure MenuItemUpdateBetaClick(Sender: TObject);
    procedure MenuItemUpdateNightlyClick(Sender: TObject);
    procedure MenuItemUpdateStableClick(Sender: TObject);
    procedure TreeChannelsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure TreeFileListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeChannelsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeChannelsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeFileListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure TreeFileListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeFileListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure TreeFileListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionCommandLineExecute(Sender: TObject);
    procedure ActionDocumentationExecute(Sender: TObject);
    procedure TreeChannelsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure TreeChannelsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    FProject: TWebUpdateProject;
    FPreferences: TWebUpdatePreferences;
    FChannels: TWebUpdateChannels;
    FAppName: string;
    FWebUpdate: TWebUpdate;
    FProjectModified: Boolean;
    FCheckUpdateThread: TCheckUpdateThread;

    function ScanParameters: Boolean;

    procedure ClearStatus;
    procedure WriteStatus(Text: string);

    function LocateNode(RootNode: PVirtualNode; Caption: string): PVirtualNode;
    function CreateNode(FileStrings: TStringDynArray): PVirtualNode;
    function GetCurrentChannelNodeData: PChannelItem;

    procedure CollectFileProgressEventHandler(const Directory: string; var SkipScan: Boolean);

    procedure StatusEventHandler(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: String);
    procedure WorkBeginEventHandler(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure WorkEndEventHandler(Sender: TObject; AWorkMode: TWorkMode);
    procedure WorkEventHandler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure SetCurrentChannelName(const Value: string);
    function GetCurrentChannelName: string;
  protected
    procedure SetupDefaultChannels;
    procedure LoadChannels;
    procedure SaveChannels;
  public
    procedure ScanDirectory;
    procedure TakeSnapshot;
    procedure UploadSnapshot;
    procedure CopySnapshot;

    property Project: TWebUpdateProject read FProject;
    property CurrentChannelName: string read GetCurrentChannelName write SetCurrentChannelName;
  end;

var
  FormWebUpdateTool: TFormWebUpdateTool;

implementation

{$R *.dfm}

uses
  WinApi.ShellApi, Winapi.CommCtrl,
  IdHTTP, IdFtp, dwsUtils, dwsXPlatform, dwsJSON,
  WebUpdate.GUI.Options, WebUpdate.GUI.About, WebUpdate.GUI.CommandLine,
  WebUpdate.JSON.Serializer, WebUpdate.Tools;

const
  CBaseURL = 'https://raw.githubusercontent.com/CWBudde/WebUpdate/master/Binaries/WebUpdate/';

resourcestring
  RStrFileNotFound = 'File %s not found';
  RStrNoFileSelected = 'No file selected for update!';
  RStrSavingChannelSetup = 'Saving channel setup...';

{ TCheckUpdateThread }

constructor TCheckUpdateThread.Create(WebUpdate: TWebUpdate);
begin
  FWebUpdate := WebUpdate;
  inherited Create;
end;

procedure TCheckUpdateThread.Execute;
begin
  FWebUpdate.GetChannelsInformationFromServer;

  // check for termination
  if Terminated then
    Exit;

  if FWebUpdate.CheckForUpdate then
  begin
    // check for termination
    if Terminated then
      Exit;

    Synchronize(Update);
  end;
end;

procedure TCheckUpdateThread.Update;
begin
  if MessageDlg('A new update is available!' + #13#10#13#10 +
    'Update now?', mtInformation, [mbYes, mbNo], 0) = mrYes then
    FWebUpdate.PerformUpdate;
end;


{ TFormWebUpdateTool }

procedure TFormWebUpdateTool.FormCreate(Sender: TObject);
var
  ProgressBarStyle: Integer;
  PanelRect: TRect;
begin
  // specify node data sizes
  TreeChannels.NodeDataSize := SizeOf(TChannelItem);
  TreeFileList.NodeDataSize := SizeOf(TFileItem);

  // create preferences
  FPreferences := TWebUpdatePreferences.Create(ExtractFilePath(ParamStr(0)) +
    'Preferences.json');

  // create project
  FProject := TWebUpdateProject.Create;

  // create channels
  FChannels := TWebUpdateChannels.Create;

  ProgressBar.Parent := StatusBar;
  SendMessage(StatusBar.Handle, SB_GETRECT, 0, Integer(@PanelRect));
  with PanelRect do
    ProgressBar.SetBounds(Left, Top, Right - Left, Bottom - Top);

  ProgressBarStyle := GetWindowLong(ProgressBar.Handle, GWL_EXSTYLE);
  ProgressBarStyle := ProgressBarStyle - WS_EX_STATICEDGE;
  SetWindowLong(ProgressBar.Handle, GWL_EXSTYLE, ProgressBarStyle);
end;

procedure TFormWebUpdateTool.FormDestroy(Sender: TObject);
begin
  if Assigned(FCheckUpdateThread) then
  begin
    FCheckUpdateThread.Terminate;
    FCheckUpdateThread.WaitFor;
    FreeAndNil(FCheckUpdateThread);
  end;

  FWebUpdate.Free;
  FChannels.Free;
  FProject.Free;
  FPreferences.Free;
end;

procedure TFormWebUpdateTool.FormShow(Sender: TObject);
begin
  Left := FPreferences.Left;
  Top := FPreferences.Top;

  // eventually load or setup project
  if FileExists(FPreferences.RecentProject) then
    FProject.LoadFromFile(FPreferences.RecentProject)
  else
  begin
    // set default values
    FProject.BaseDirectory := ExtractFileDir(ParamStr(0));
    FProject.ChannelsFilename := 'WebUpdate\Channels.json';
  end;

  // load/setup channels
  if FileExists(Project.FullChannelsFilename) then
    LoadChannels
  else
    SetupDefaultChannels;

  if ScanParameters then
    Exit;

  ActionCopyUpload.Visible := not Project.AutoCopyUpload;
  ActionViewChannel.Checked := FPreferences.ViewFiles;
  ActionViewChannelExecute(Sender);

  // create & setup self update
  FWebUpdate := TWebUpdate.Create;
  FWebUpdate.BaseURL := CBaseURL;
  FWebUpdate.LocalChannelFileName := ChangeFileExt(ParamStr(0), '.json');
  FWebUpdate.GetLocalChannelInformation;
  FCheckUpdateThread := TCheckUpdateThread.Create(FWebUpdate);
end;

procedure TFormWebUpdateTool.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FPreferences.Left := Left;
  FPreferences.Top := Top;
  FPreferences.ViewFiles := ActionViewChannel.Checked;
end;

procedure TFormWebUpdateTool.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FProjectModified then
    CanClose := MessageDlg('Project has been modified, but not saved yet!' +
      #13#10#13#10 + 'Do you really want to close the application without saving?',
      mtInformation, [mbYes, mbNo], 0) = mrYes;
end;

function TFormWebUpdateTool.GetCurrentChannelName: string;
begin
  Result := FProject.ChannelName;
end;

function TFormWebUpdateTool.GetCurrentChannelNodeData: PChannelItem;
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
begin
  Result := nil;

  // get FCurrentChannel node
  for Node in TreeChannels.Nodes do
  begin
    NodeData := TreeChannels.GetNodeData(Node);
    if SameText(NodeData^.Name, FProject.ChannelName) then
      Exit(TreeChannels.GetNodeData(Node));
  end;

  // get currently checked node
  for Node in TreeChannels.CheckedNodes do
    Exit(TreeChannels.GetNodeData(Node));
end;

procedure TFormWebUpdateTool.ActionAddChannelExecute(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
begin
  Node := TreeChannels.AddChild(TreeChannels.RootNode);
  Node.CheckType := ctRadioButton;
  NodeData := TreeChannels.GetNodeData(Node);
  NodeData^.Name := 'Unknown';
  NodeData^.FileName := NodeData^.Name + '.json';
  FileAge(NodeData^.FileName, NodeData^.Modified);
end;

procedure TFormWebUpdateTool.ActionDeleteChannelExecute(Sender: TObject);
begin
  if Assigned(TreeChannels.FocusedNode) then
    TreeChannels.DeleteNode(TreeChannels.FocusedNode);
end;

procedure TFormWebUpdateTool.ActionDocumentationExecute(Sender: TObject);
begin
  ShellExecute(Application.Handle, 'open', PChar(ExtractFilePath(ParamStr(0)) +
    'Documentation.pdf'), nil, PChar(ExtractFilePath(ParamStr(0))), SW_SHOW);
end;

procedure TFormWebUpdateTool.ActionCommandLineExecute(Sender: TObject);
begin
  with TFormCommandLine.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormWebUpdateTool.ActionCopyUploadExecute(Sender: TObject);
begin
  UploadSnapshot;
  CopySnapshot;
end;

procedure TFormWebUpdateTool.ActionCheckUpdateExecute(Sender: TObject);
begin
  FWebUpdate.GetChannelsInformationFromServer;
  if FWebUpdate.CheckForUpdate then
    if MessageDlg('A new update is available!' + #13#10#13#10 +
      'Update now?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      FWebUpdate.PerformUpdate;
end;

procedure TFormWebUpdateTool.ActionFileOpenAccept(Sender: TObject);
var
  FileName: TFileName;
begin
  Assert(Sender is TFileOpen);

  // get file name and exit if not exists
  FileName := TFileOpen(Sender).Dialog.FileName;
  if not FileExists(FileName) then
    Exit;

  FProject.LoadFromFile(FileName);
  FPreferences.RecentProject := FileName;
end;

procedure TFormWebUpdateTool.ActionFileSaveAsAccept(Sender: TObject);
var
  FileName: TFileName;
begin
  Assert(Sender is TFileSaveAs);
  FileName := TFileSaveAs(Sender).Dialog.FileName;
  FProject.SaveToFile(FileName);
  FProjectModified := False;
  FPreferences.RecentProject := FileName;
end;

procedure TFormWebUpdateTool.ActionFileSaveExecute(Sender: TObject);
begin
  if FPreferences.RecentProject <> '' then
  begin
    FProject.SaveToFile(FPreferences.RecentProject);
    FProjectModified := False;
  end;
end;

procedure TFormWebUpdateTool.ActionHelpAboutExecute(Sender: TObject);
begin
  with TFormAbout.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFormWebUpdateTool.ActionTakeSnapshotExecute(Sender: TObject);
begin
  TakeSnapshot;
end;

procedure TFormWebUpdateTool.ActionUpdateExecute(Sender: TObject);
begin
  FWebUpdate.PerformUpdate;
end;

procedure TFormWebUpdateTool.ActionViewChannelExecute(Sender: TObject);
begin
  if ActionViewChannel.Checked then
  begin
    TreeFileList.Visible := True;
    PanelChannels.Align := alTop;
    PanelChannels.Height := 128;
    Splitter.Visible := True;
    Splitter.Top := PanelChannels.Height;
  end
  else
  begin
    TreeFileList.Visible := False;
    Splitter.Visible := False;
    PanelChannels.Align := alClient;
  end;
end;

procedure TFormWebUpdateTool.ActionFileOptionsExecute(Sender: TObject);
begin
  with TFormOptions.Create(Self) do
  try
    EditBaseDirectory.Text := Project.BaseDirectory;
    EditChannelFileName.Text :=  Project.ChannelsFilename;
    EditFtpServer.Text := Project.FTP.Server;
    EditFtpUsername.Text := Project.FTP.Username;
    EditFtpPassword.Text := Project.FTP.Password;
    CheckBoxAutoCopyUpload.Checked := Project.AutoCopyUpload;
    CheckBoxCopyTo.Checked := Project.Copy.Enabled;
    CheckBoxMD5.Checked := Project.UseMD5;
    EditCopyPath.Text := Project.Copy.Path;

    if ShowModal = mrOk then
    begin
      Project.BaseDirectory := EditBaseDirectory.Text;
      Project.ChannelsFilename := EditChannelFileName.Text;
      Project.FTP.Server := EditFtpServer.Text;
      Project.FTP.Username := EditFtpUsername.Text;
      Project.FTP.Password := EditFtpPassword.Text;
      Project.AutoCopyUpload := CheckBoxAutoCopyUpload.Checked;
      Project.Copy.Enabled := CheckBoxCopyTo.Checked;
      Project.Copy.Path := EditCopyPath.Text;
      FProjectModified := True;

      ActionCopyUpload.Visible := not Project.AutoCopyUpload;
    end;
  finally
    Free;
  end;
end;

procedure TFormWebUpdateTool.ActionScanFilesExecute(Sender: TObject);
begin
  ScanDirectory;
end;

procedure TFormWebUpdateTool.CollectFileProgressEventHandler(
  const Directory: String; var SkipScan: Boolean);
begin
  WriteStatus('Scanning: ' + Directory);

  if Pos('.', Directory) > 0 then
    SkipScan := True;
end;

function TFormWebUpdateTool.LocateNode(RootNode: PVirtualNode; Caption: string): PVirtualNode;
var
  Node: PVirtualNode;
  NodeData: PFileItem;
  Hash: Cardinal;
begin
  Hash := SimpleStringHash(Caption);
  for Node in TreeFileList.ChildNodes(RootNode) do
  begin
    NodeData := TreeFileList.GetNodeData(Node);
    if NodeData.Hash = Hash then
      if UnicodeSameText(NodeData.Caption, Caption) then
        Exit(Node);
  end;

  // node not found -> create!
  Result := TreeFileList.AddChild(RootNode);
  Result.CheckType := ctCheckBox;
  NodeData := TreeFileList.GetNodeData(Result);
  NodeData^.Caption := Caption;
  NodeData^.Hash := Hash;
end;

procedure TFormWebUpdateTool.MenuItemCheckAllClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  for Node in TreeFileList.Nodes do
    TreeFileList.CheckState[Node] := csCheckedNormal;
end;

procedure TFormWebUpdateTool.MenuItemCheckNoneClick(Sender: TObject);
var
  Node: PVirtualNode;
begin
  for Node in TreeFileList.Nodes do
    TreeFileList.CheckState[Node] := csUncheckedNormal;
end;

procedure TFormWebUpdateTool.MenuItemUpdateAlphaClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  FWebUpdate.ChannelName := 'Alpha';
end;

procedure TFormWebUpdateTool.MenuItemUpdateBetaClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  FWebUpdate.ChannelName := 'Beta';
end;

procedure TFormWebUpdateTool.MenuItemUpdateNightlyClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  FWebUpdate.ChannelName := 'Nightly';
end;

procedure TFormWebUpdateTool.MenuItemUpdateStableClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := True;
  FWebUpdate.ChannelName := 'Stable';
end;

function TFormWebUpdateTool.CreateNode(FileStrings: System.Types.TStringDynArray): PVirtualNode;
var
  Level: Integer;
begin
  Result := TreeFileList.RootNode;
  for Level := 0 to High(FileStrings) do
    Result := LocateNode(Result, FileStrings[Level]);
end;

procedure TFormWebUpdateTool.ScanDirectory;
var
  BaseDirectory: string;
  FileStrings: System.Types.TStringDynArray;
  FileList: TStringList;
  FileName: TFileName;
  WebFileName: string;
  Node: PVirtualNode;
  NodeData: PFileItem;
  BasePath: string;
  Fad: TWin32FileAttributeData;
begin
  // get base directory (eventually expanding relative paths)
  BaseDirectory := Project.BaseDirectory;
  if IsRelativePath(BaseDirectory) then
    BaseDirectory := ExtractFilePath(ParamStr(0)) + BaseDirectory
  else if BaseDirectory = '' then
    BaseDirectory := ExtractFileDir(ParamStr(0));

  TreeFileList.BeginUpdate;
  try
    FileList := TStringList.Create;
    try
      CollectFiles(BaseDirectory, '*.*', FileList, True, CollectFileProgressEventHandler);
      BasePath := IncludeTrailingPathDelimiter(BaseDirectory);

      WriteStatus('Building tree...');

      for FileName in FileList do
      begin
        WebFileName := ExtractRelativePath(BasePath, FileName);
        FileStrings := SplitString(WebFileName, '\');

        Node := CreateNode(FileStrings);
        NodeData := TreeFileList.GetNodeData(Node);
        NodeData^.FileName := FileName;
        NodeData^.WebFileName := LocalToWebFileName(WebFileName);
        NodeData^.Caption := FileStrings[High(FileStrings)];
        NodeData^.Hash := SimpleStringHash(NodeData^.Caption);

        if not GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @Fad) then
          RaiseLastOSError;

        NodeData^.Size := Fad.nFileSizeLow;
        NodeData^.Modified := FileTimeToDateTime(Fad.ftLastWriteTime);
      end;
    finally
      FileList.Free;
    end;

    WriteStatus('Sorting tree...');

    TreeFileList.SortTree(0, sdAscending);
  finally
    TreeFileList.EndUpdate;
  end;

  ClearStatus;
end;

function TFormWebUpdateTool.ScanParameters: Boolean;
type
  TWebUpdateCommand = (wuSnapshot, wuUpload, wuCopy);
  TWebUpdateCommands = set of TWebUpdateCommand;
var
  ParamIndex, CharIndex: Integer;
  Commands: TWebUpdateCommands;
  Text: string;
  EqPos: Integer;
begin
  Result := False;

  // check for parameters
  if ParamCount = 0 then
    Exit;

  // check if project file exists
  if not FileExists(ParamStr(1)) then
  begin
    MessageDlg(Format('File %s does not exist!', [ParamStr(1)]), mtError,
      [mbOK], 0);
    Exit;
  end;

  // now load project
  FProject.LoadFromFile(ParamStr(1));

  // check whether a command is specified
  if ParamCount = 1 then
  begin
    MessageDlg('No command specified!', mtError, [mbOK], 0);
    Exit;
  end;

  Commands := [];
  for ParamIndex := 2 to ParamCount do
  begin
    Text := ParamStr(ParamIndex);
    if Text[1] = '-' then
    begin
      // remove options identifier and get colon pos
      Delete(Text, 1, 1);
      EqPos := Pos('=', Text);

      if StartsText('Channel:', Text) then
        CurrentChannelName := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('FtpHost:', Text) then
        Project.FTP.Server := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('FtpUser:', Text) then
        Project.FTP.Username := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('FtpPassword:', Text) then
        Project.FTP.Password := Copy(Text, EqPos, Length(Text) - EqPos)
      else if StartsText('CopyPath:', Text) then
        Project.Copy.Path := Copy(Text, EqPos, Length(Text) - EqPos)
      else
      begin
        MessageDlg(Format('Unknown option: %s!', [Text]), mtError, [mbOK], 0);
        Exit;
      end;
    end
    else
    begin
      if UnicodeSameText(Text, 'Snapshot') then
        Commands := Commands + [wuSnapshot]
      else if UnicodeSameText(Text, 'Copy') then
        Commands := Commands + [wuCopy]
      else if UnicodeSameText(Text, 'Upload') then
        Commands := Commands + [wuUpload]
      else
        for CharIndex := 1 to Length(Text) do
          case Text[CharIndex] of
            's', 'S':
              Commands := Commands + [wuSnapshot];
            'c', 'C':
              Commands := Commands + [wuCopy];
            'u', 'U':
              Commands := Commands + [wuUpload];
            else
              begin
                MessageDlg(Format('Unknown command %s', [Text[CharIndex]]),
                  mtError, [mbOK], 0);
                Exit;
              end;
          end;
    end;
  end;

  // run minimized
  WindowState := wsMinimized;
  Result := True;

  if wuSnapshot in Commands then
    TakeSnapshot;
  if wuCopy in Commands then
    CopySnapshot;
  if wuUpload in Commands then
    UploadSnapshot;

  Application.Terminate;
end;

procedure TFormWebUpdateTool.LoadChannels;
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
  ChannelItem: TWebUpdateChannelItem;
  FileName: string;
begin
  // get channels file name
  FileName := Project.FullChannelsFilename;
  if IsRelativePath(FileName) then
    FileName := ExtractFilePath(ParamStr(0)) + FileName;

  // load channels from file
  FChannels.LoadFromFile(FileName);

  // clear channel tree
  TreeChannels.Clear;

  TreeChannels.BeginUpdate;
  try
    // enumerate channel item
    for ChannelItem in FChannels.Items do
    begin
      Node := TreeChannels.AddChild(TreeChannels.RootNode);
      Node.CheckType := ctRadioButton;
      NodeData := TreeChannels.GetNodeData(Node);

      NodeData^.Name := ChannelItem.Name;
      NodeData^.FileName := ChannelItem.FileName;
      NodeData^.Modified := ChannelItem.Modified;

      // update check state
      if SameText(ChannelItem.Name, FProject.ChannelName) then
        TreeChannels.CheckState[Node] := csCheckedNormal;
    end;
  finally
    TreeChannels.EndUpdate;
  end;
end;

procedure TFormWebUpdateTool.SaveChannels;
var
  Node: PVirtualNode;
  NodeChannelData: PChannelItem;
  ChannelItem: TWebUpdateChannelItem;
begin
  // now save channels file
  WriteStatus('Save channels file...');

  // clear existing channels
  FChannels.Items.Clear;

  // enumerate nodes
  TreeChannels.BeginUpdate;
  try
    for Node in TreeChannels.Nodes do
    begin
      NodeChannelData := TreeChannels.GetNodeData(Node);

      ChannelItem := TWebUpdateChannelItem.Create;
      ChannelItem.Name := NodeChannelData^.Name;
      ChannelItem.FileName := NodeChannelData^.FileName;
      ChannelItem.Modified := NodeChannelData^.Modified;

      // eventually create MD5 checksum
      if Project.UseMD5 and FileExists(ChannelItem.FileName) then
        ChannelItem.MD5 := MD5(ChannelItem.FileName);

      FChannels.Items.Add(ChannelItem);
    end;
  finally
    TreeChannels.EndUpdate;
  end;
  FChannels.SaveToFile(FormWebUpdateTool.Project.FullChannelsFilename);
end;

procedure TFormWebUpdateTool.SetCurrentChannelName(const Value: string);
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
begin
  if FProject.ChannelName <> Value then
  begin
    // check node with name
    for Node in TreeChannels.Nodes do
    begin
      NodeData := TreeChannels.GetNodeData(Node);
      if SameText(NodeData^.Name, Value) then
        TreeChannels.CheckState[Node] := csCheckedNormal
      else
        TreeChannels.CheckState[Node] := csUncheckedNormal;
    end;

    FProject.ChannelName := Value;
  end;
end;

procedure TFormWebUpdateTool.SetupDefaultChannels;
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
  Index: Integer;
const
  CChannelNames: array [0 .. 3] of string = ('Stable', 'Beta', 'Alpha',
    'Nightly');
begin
  for Index := Low(CChannelNames) to High(CChannelNames) do
  begin
    Node := TreeChannels.AddChild(TreeChannels.RootNode);
    Node.CheckType := ctRadioButton;
    NodeData := TreeChannels.GetNodeData(Node);

    NodeData^.Name := CChannelNames[Index];
    NodeData^.FileName := NodeData^.Name + '.json';
    NodeData^.Modified := 0;

    // update check state
    if SameText(NodeData^.Name, FProject.ChannelName) then
      TreeChannels.CheckState[Node] := csCheckedNormal;
  end;
end;

procedure TFormWebUpdateTool.TakeSnapshot;
var
  Node: PVirtualNode;
  NodeData: PFileItem;
  NodeChannelData: PChannelItem;
  Item: TWebUpdateFileItem;
  Fad: TWin32FileAttributeData;
  LastModified: TDateTime;
  FileName: TFileName;
  ChannelSetup: TWebUpdateChannelSetup;
begin
  // check if any files are selected at all
  if TreeFileList.CheckedCount = 0 then
    if MessageDlg('No file selected for update!', mtError, [mbOK], 0) = mrOk then
      Exit;

  // update status
  WriteStatus('Taking snapshot...');

  // update status
  NodeChannelData := GetCurrentChannelNodeData;
  if not Assigned(NodeChannelData) then
    Exit;

  // get filename for currently selected channel
  FileName := Project.ChannelsPath + ExtractFileName(
    WebToLocalFileName(NodeChannelData^.FileName));

  // create selected channels
  ChannelSetup := TWebUpdateChannelSetup.Create;
  try
    // store current channel name
    ChannelSetup.AppName := FAppName;
    ChannelSetup.ChannelName := FProject.ChannelName;
    LastModified := 0;
    for Node in TreeFileList.CheckedNodes do
    begin
      NodeData := TreeFileList.GetNodeData(Node);
      if NodeData^.WebFileName = '' then
        Continue;

      // get file attribute
      if not GetFileAttributesEx(PChar(NodeData^.FileName), GetFileExInfoStandard, @Fad) then
        RaiseLastOSError;

      // update file attributes
      NodeData^.Size := Fad.nFileSizeLow;
      NodeData^.Modified := FileTimeToDateTime(Fad.ftLastWriteTime);
      TreeFileList.RepaintNode(Node);

      // create (& update) file item
      Item := TWebUpdateFileItem.Create;
      Item.FileName := NodeData^.WebFileName;
      Item.Modified := NodeData^.Modified;
      Item.FileSize := NodeData^.Size;
      if Project.UseMD5 then
        Item.MD5Hash := MD5(NodeData.FileName)
      else
        Item.MD5Hash := '';

      if NodeData^.Modified > LastModified then
        LastModified := NodeData^.Modified;

      // add item to file items list
      ChannelSetup.Items.Add(Item);
    end;
    ChannelSetup.Modified := LastModified;
    NodeChannelData^.Modified := LastModified;

    // save channel setup
    ChannelSetup.SaveToFile(FileName);
  finally
    ChannelSetup.Free;
  end;

  SaveChannels;
  TreeChannels.Invalidate;

  // eventually copy to path and upload to a server
  if Project.AutoCopyUpload then
  begin
    if Project.Copy.Enabled then
      CopySnapshot;

    if Project.FTP.Server <> '' then
      UploadSnapshot;
  end;

  ClearStatus;
end;

procedure TFormWebUpdateTool.TreeChannelsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Assigned(Node) then
    TreeChannels.CheckState[Node] := csCheckedNormal;
end;

procedure TFormWebUpdateTool.TreeChannelsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NewNode: PVirtualNode;
  FileStrings: System.Types.TStringDynArray;
  FileName, WebFileName, RealFileName: string;
  Item: TWebUpdateFileItem;
  ChannelSetup: TWebUpdateChannelSetup;
  ChannelNodeData: PChannelItem;
  NodeData: PFileItem;
begin
  if not Assigned(Node) then
    Exit;

  ChannelNodeData := TreeChannels.GetNodeData(Node);
  FProject.ChannelName := ChannelNodeData^.Name;

  // get channel file name
  FileName := ExtractFileName(WebToLocalFileName(ChannelNodeData^.FileName));
  FileName := Project.ChannelsPath + FileName;

  // check if file exists
  if not FileExists(FileName) then
    Exit;

  // create selected channels
  ChannelSetup := TWebUpdateChannelSetup.Create;
  try
    // load setup from file
    ChannelSetup.LoadFromFile(FileName);

    // store current channel name
    if ChannelSetup.ChannelName <> '' then
      FProject.ChannelName := ChannelSetup.ChannelName;
    FAppName := ChannelSetup.AppName;

    TreeFileList.BeginUpdate;
    try
      for Item in ChannelSetup.Items do
      begin
        WebFileName := Item.FileName;
        FileStrings := SplitString(WebFileName, '/');
        RealFileName := Project.BasePath + WebToLocalFileName(WebFileName);

        // eventually fix relative path
        if IsRelativePath(RealFileName) then
          RealFileName := ExtractFilePath(ParamStr(0)) + RealFileName;

        if FileExists(RealFileName) then
        begin
          NewNode := CreateNode(FileStrings);
          NewNode.CheckState := csCheckedNormal;
          NodeData := TreeFileList.GetNodeData(NewNode);
          NodeData^.FileName := RealFileName;
          NodeData^.WebFileName := WebFileName;
          NodeData^.Caption := FileStrings[High(FileStrings)];
          NodeData^.Hash := SimpleStringHash(NodeData^.Caption);
          NodeData^.Modified := Item.Modified;
          NodeData^.Size := Item.FileSize;
       end else
         if MessageDlg(Format(RStrFileNotFound, [RealFileName]), mtWarning,
           [mbIgnore, mbAbort], 0) = mrAbort then
           Exit;
      end;
    finally
      TreeFileList.EndUpdate;
    end;
  finally
    ChannelSetup.Free;
  end;
end;

procedure TFormWebUpdateTool.TreeChannelsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PChannelItem;
begin
  NodeData := TreeChannels.GetNodeData(Node);
  Finalize(NodeData^);
end;

procedure TFormWebUpdateTool.TreeChannelsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PChannelItem;
begin
  CellText := '';
  NodeData := TreeChannels.GetNodeData(Node);
  case Column of
    0:
      CellText := NodeData^.Name;
    1:
      CellText := NodeData^.FileName;
    2:
      if NodeData^.Modified > 0 then
        CellText := DateTimeToStr(NodeData^.Modified);
  end;
end;

procedure TFormWebUpdateTool.TreeChannelsNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  NodeData: PChannelItem;
begin
  NodeData := TreeChannels.GetNodeData(Node);
  case Column of
    0:
      NodeData^.Name := NewText;
    1:
      NodeData^.FileName := NewText;
  end;
end;

procedure TFormWebUpdateTool.TreeFileListChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  SubNode: PVirtualNode;
begin
  for SubNode in TreeFileList.ChildNodes(Node) do
    TreeFileList.CheckState[SubNode] := TreeFileList.CheckState[Node];
end;

procedure TFormWebUpdateTool.TreeFileListCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  NodeData: array [0..1] of PFileItem;
begin
  NodeData[0] := TreeFileList.GetNodeData(Node1);
  NodeData[1] := TreeFileList.GetNodeData(Node2);

  Result := AnsiCompareStr(NodeData[0].Caption, NodeData[1].Caption);
  if NodeData[0].FileName = '' then
    Result := Result - 2;
  if NodeData[1].FileName = '' then
    Result := Result + 2;
end;

procedure TFormWebUpdateTool.TreeFileListFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PFileItem;
begin
  NodeData := TreeFileList.GetNodeData(Node);
  Finalize(NodeData^);
end;

procedure TFormWebUpdateTool.TreeFileListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  NodeData: PFileItem;
begin
  if Column = 0 then
    case Kind of
      ikNormal, ikSelected:
      begin
        NodeData := TreeFileList.GetNodeData(Node);
        if NodeData.FileName <> '' then
          ImageIndex := 17
        else if TreeFileList.Expanded[Node] then
          ImageIndex := 16
        else
          ImageIndex := 15;
      end;
    end;
end;

function FormatByteSize(const ByteSize: Int64): string;
const
  CkB = 1000; // kilobyte
  CMB = 1000 * CkB; // megabyte
  CGB = 1000 * CMB; // gigabyte
begin
  if ByteSize > CGB then
    Result := FormatFloat('#.### GB', ByteSize / CGB)
  else if ByteSize > CMB then
    Result := FormatFloat('#.### MB', ByteSize / CMB)
  else if ByteSize > CkB then
    Result := FormatFloat('#.### kB', ByteSize / CkB)
  else
    Result := FormatFloat('#.### Bytes', ByteSize);
end;

procedure TFormWebUpdateTool.TreeFileListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: PFileItem;
begin
  CellText := '';
  NodeData := TreeFileList.GetNodeData(Node);
  case Column of
    0:
      CellText := NodeData^.Caption;
    1:
      if NodeData^.FileName <> '' then
        CellText := DateTimeToStr(NodeData^.Modified);
    2:
      if NodeData^.FileName <> '' then
        CellText := FormatByteSize(NodeData^.Size);
  end;
end;

procedure TFormWebUpdateTool.CopySnapshot;
var
  Path: string;
  ChannelSetup: TWebUpdateChannelSetup;
  FileItem: TWebUpdateFileItem;
  ChannelName, ChannelPath: string;
  RealFileName, ChannelFileName, DestFileName: TFileName;
  NodeData: PChannelItem;
begin
  Path := IncludeTrailingPathDelimiter(Project.Copy.Path);
  if IsRelativePath(Path) then
    Path := Project.BasePath + Path;

  // get currently checked channel node
  NodeData := GetCurrentChannelNodeData;
  if not Assigned(NodeData) then
    Exit;

  ChannelName := NodeData^.Name;
  ChannelFileName := Project.ChannelsPath +
    ExtractFileName(WebToLocalFileName(NodeData^.FileName));
  ChannelPath := ExtractFilePath(WebToLocalFileName(NodeData^.FileName));

  // upload files
  ChannelSetup := TWebUpdateChannelSetup.Create;
  try
    // load channel setup
    ChannelSetup.LoadFromFile(ChannelFileName);

    // ensure the date is set identical in both JSON files
    if ChannelSetup.Modified <> NodeData^.Modified then
      if MessageDlg('Time stamp mismatch. Continue?', mtWarning, [mbYes, mbNo], 0) = mrNo then
        Exit;

    for FileItem in ChannelSetup.Items do
    begin
      WriteStatus('Copying file ' + FileItem.FileName + '...');

      RealFileName := Project.BasePath + WebToLocalFileName(FileItem.FileName);

      // copy file
      DestFileName := ExpandFileName(Path + ChannelPath + FileItem.FileName);
      ForceDirectories(ExtractFileDir(DestFileName));
      CopyFile(PChar(RealFileName), PChar(DestFileName), False);

      // set file date/time according to the JSON file
      FileSetDate(DestFileName, DateTimeToFileDate(FileItem.Modified));
    end;
  finally
    ChannelSetup.Free;
  end;

  // copy channel setup
  WriteStatus('Copying channel setup...');
  DestFileName := ExpandFileName(Path + NodeData^.FileName);
  ForceDirectories(ExtractFileDir(DestFileName));
  CopyFile(PChar(ChannelFileName), PChar(DestFileName), False);

  // set file date/time according to the JSON file
  FileSetDate(DestFileName, DateTimeToFileDate(NodeData^.Modified));

  // copy channel file
  WriteStatus('Copying channels list...');
  DestFileName := ExpandFileName(Path + ExtractFileName(FProject.ChannelsFilename));
  CopyFile(PChar(FProject.FullChannelsFilename), PChar(DestFileName), False);

  ClearStatus;
end;

procedure TFormWebUpdateTool.UploadSnapshot;
var
  ChannelSetup: TWebUpdateChannelSetup;
  FileItem: TWebUpdateFileItem;
  ChannelName: string;
  RealFileName, ChannelFileName, ChannelWebName: TFileName;
  NodeData: PChannelItem;
begin
  // only continue if an FTP server is supplied
  if Project.FTP.Server = '' then
    Exit;

  // get currently checked channel node
  NodeData := GetCurrentChannelNodeData;
  if not Assigned(NodeData) then
    Exit;

  ChannelName := NodeData^.Name;
  ChannelFileName := Project.ChannelsPath +
    ExtractFileName(WebToLocalFileName(NodeData^.FileName));
  ChannelWebName := ChannelName + '/' + NodeData^.FileName;

  with TIdFTP.Create(nil) do
  try
    OnWork := WorkEventHandler;
    OnWorkBegin := WorkBeginEventHandler;
    OnWorkEnd := WorkEndEventHandler;
    OnStatus := StatusEventHandler;
    Host := Project.FTP.Server;
    Username := Project.FTP.Username;
    Password := Project.FTP.Password;
    Connect;
    try
      // upload files
      ChannelSetup := TWebUpdateChannelSetup.Create;
      try
        // load channel setup
        ChannelSetup.LoadFromFile(ChannelFileName);

        // ensure the date is set identical in both JSON files
        if ChannelSetup.Modified <> NodeData^.Modified then
          if MessageDlg('Time stamp mismatch. Continue?', mtWarning, [mbYes, mbNo], 0) = mrNo then
            Exit;

        for FileItem in ChannelSetup.Items do
        begin
          WriteStatus('Uploading: ' + FileItem.FileName);

          // upload file
          RealFileName := Project.BasePath + WebToLocalFileName(FileItem.FileName);
          Put(RealFileName, ChannelName + '/' + FileItem.FileName);

          // now try to update time stamp
          try
            SetModTime(ChannelName + '/' + FileItem.FileName, FileItem.Modified);
          except end;
        end;
      finally
        ChannelSetup.Free;
      end;

      WriteStatus('Uploading channel setup...');

      // upload channel setup
      Put(ChannelFileName, ChannelWebName);
      try
        SetModTime(ChannelWebName, NodeData^.Modified);
      except end;

      WriteStatus('Uploading channels list...');

      // upload channel file
      Put(FProject.FullChannelsFilename, ExtractFileName(FProject.ChannelsFilename));

      ClearStatus;
    finally
      Disconnect;
    end;
  finally
    Free;
  end;
end;

procedure TFormWebUpdateTool.StatusEventHandler(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: String);
begin
  StatusBar.Panels[1].Text := AStatusText;
end;

procedure TFormWebUpdateTool.WorkBeginEventHandler(Sender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  ProgressBar.Max := AWorkCountMax;
end;

procedure TFormWebUpdateTool.WorkEndEventHandler(Sender: TObject; AWorkMode: TWorkMode);
begin
  ProgressBar.Position := 0;
end;

procedure TFormWebUpdateTool.WorkEventHandler(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  ProgressBar.Position := AWorkCount;
  Application.ProcessMessages;
end;

procedure TFormWebUpdateTool.ClearStatus;
begin
  StatusBar.Panels[1].Text := '';
end;

procedure TFormWebUpdateTool.WriteStatus(Text: string);
begin
  StatusBar.Panels[1].Text := Text;
  Application.ProcessMessages;
end;

end.
