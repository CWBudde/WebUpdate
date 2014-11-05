unit WebUpdate.AuthorTool;

interface

uses
  System.StrUtils, System.Types, System.SysUtils, System.Classes,
  Winapi.Windows, Winapi.Messages, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Menus, Vcl.ActnList,
  Vcl.StdActns, Vcl.ImgList, Vcl.StdCtrls, Vcl.ExtCtrls,
  IdComponent, VirtualTrees, WebUpdate.Preferences.JSON, WebUpdate.WebUpdate,
  WebUpdate.Project.JSON, WebUpdate.Channels.JSON, WebUpdate.Channel.JSON;

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

  TFormWebUpdateTool = class(TForm)
    ActionAddChannel: TAction;
    ActionScanFiles: TAction;
    ActionDeleteChannel: TAction;
    ActionCopyUpload: TAction;
    ActionCheckUpdate: TAction;
    ActionList: TActionList;
    ActionFileExit: TFileExit;
    ActionFileOpen: TFileOpen;
    ActionFileOptions: TAction;
    ActionFileSave: TAction;
    ActionFileSaveAs: TFileSaveAs;
    ActionTakeSnapshot: TAction;
    ActionUpdate: TAction;
    CheckForUpdateTimer: TTimer;
    Images: TImageList;
    MainMenu: TMainMenu;
    MenuItemCheckAll: TMenuItem;
    MenuItemCheckForUpdates: TMenuItem;
    MenuItemCheckNone: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemFileOpen: TMenuItem;
    MenuItemFileSave: TMenuItem;
    MenuItemProjectOptions: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemTools: TMenuItem;
    MenuItemToolsUpdate: TMenuItem;
    MenuItemUpdateAlpha: TMenuItem;
    MenuItemUpdateBeta: TMenuItem;
    MenuItemUpdateNightly: TMenuItem;
    MenuItemUpdateStable: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    PanelChannels: TPanel;
    PanelFiles: TPanel;
    PopupMenu: TPopupMenu;
    ScanDirectoriesandFiles1: TMenuItem;
    Separator1: TToolButton;
    Splitter: TSplitter;
    StatusBar: TStatusBar;
    ToolBarChannels: TToolBar;
    ToolButton4: TToolButton;
    ToolButtonChannelsAdd: TToolButton;
    ToolButtonChannelsDelete: TToolButton;
    ToolButtonChannelsStore: TToolButton;
    TreeChannels: TVirtualStringTree;
    TreeFileList: TVirtualStringTree;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    MenuItemView: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ActionAddChannelExecute(Sender: TObject);
    procedure ActionScanFilesExecute(Sender: TObject);
    procedure ActionDeleteChannelExecute(Sender: TObject);
    procedure ActionChannelsRecallExecute(Sender: TObject);
    procedure ActionChannelsStoreExecute(Sender: TObject);
    procedure ActionCopyUploadExecute(Sender: TObject);
    procedure ActionCheckUpdateExecute(Sender: TObject);
    procedure ActionFileOpenAccept(Sender: TObject);
    procedure ActionFileOptionsExecute(Sender: TObject);
    procedure ActionFileSaveAsAccept(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionTakeSnapshotExecute(Sender: TObject);
    procedure ActionUpdateExecute(Sender: TObject);
    procedure CheckForUpdateTimerTimer(Sender: TObject);
    procedure MenuItemCheckAllClick(Sender: TObject);
    procedure MenuItemCheckNoneClick(Sender: TObject);
    procedure MenuItemUpdateAlphaClick(Sender: TObject);
    procedure MenuItemUpdateBetaClick(Sender: TObject);
    procedure MenuItemUpdateNightlyClick(Sender: TObject);
    procedure MenuItemUpdateStableClick(Sender: TObject);
    procedure TreeFileListChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeChannelsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
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
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FProject: TWebUpdateProject;
    FPreferences: TWebUpdatePreferences;
    FChannels: TWebUpdateChannels;
    FCurrentChannel: string;
    FAppName: string;
    FWebUpdate: TWebUpdate;
    FProjectModified: Boolean;

    procedure ClearStatus;
    procedure WriteStatus(Text: string);

    function LocateNode(RootNode: PVirtualNode; Caption: string): PVirtualNode;
    function CreateNode(FileStrings: TStringDynArray): PVirtualNode;
    function GetCurrentChannelNodeData: PChannelItem;

    procedure CollectFileProgressEventHandler(const Directory: string; var SkipScan: Boolean);

    procedure SetupDefaultChannels;
    procedure LoadChannels;
    procedure SaveChannels;

    procedure UploadSnapshot;
    procedure CopySnapshot;

    procedure WorkEventHandler(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    procedure LoadChannelSetup(const ChannelSetup: TWebUpdateChannelSetup);
    procedure SaveChannelSetup(const ChannelSetup: TWebUpdateChannelSetup);

    procedure TakeSnapshot;

    procedure ScanDirectory(const BaseDirectory: string);

    property Project: TWebUpdateProject read FProject;
  end;

var
  FormWebUpdateTool: TFormWebUpdateTool;

implementation

{$R *.dfm}

uses
  IdHTTP, IdFtp, dwsUtils, dwsXPlatform, dwsJSON,
  WebUpdate.Options.GUI, WebUpdate.JSON.Serializer, WebUpdate.MD5,
  ShellApi;

resourcestring
  RStrFileNotFound = 'File %s not found';
  RStrNoFileSelected = 'No file selected for update!';
  RStrSavingChannelSetup = 'Saving channel setup...';

function FileTimeToDateTime(Time: TFileTime): TDateTime;

  function InternalEncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
    AMilliSecond: Word): TDateTime;
  var
    LTime: TDateTime;
    Success: Boolean;
  begin
    Result := 0;
    Success := TryEncodeDate(AYear, AMonth, ADay, Result);
    if Success then
    begin
      Success := TryEncodeTime(AHour, AMinute, ASecond, AMilliSecond, LTime);
      if Success then
        if Result >= 0 then
          Result := Result + LTime
        else
          Result := Result - LTime
    end;
  end;

var
  LFileTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result := 0;
  FileTimeToLocalFileTime(Time, LFileTime);

  if FileTimeToSystemTime(LFileTime, SysTime) then
    with SysTime do
    begin
      Result := InternalEncodeDateTime(wYear, wMonth, wDay, wHour, wMinute,
        wSecond, wMilliseconds);
    end;
end;


{ TFormWebUpdateTool }

procedure TFormWebUpdateTool.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FProjectModified then
    CanClose := MessageDlg('Project has been modified, but not saved yet!' +
      #13#10#13#10 + 'Do you really want to close the application without saving?',
      mtInformation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TFormWebUpdateTool.FormCreate(Sender: TObject);
begin
  // specify node data sizes
  TreeChannels.NodeDataSize := SizeOf(TChannelItem);
  TreeFileList.NodeDataSize := SizeOf(TFileItem);

  // create preferences
  FPreferences := TWebUpdatePreferences.Create(ExtractFilePath(ParamStr(0)) +
    'Preferences.json');

  // create project
  FProject := TWebUpdateProject.Create;

  // eventually load or setup project
  if FileExists(FPreferences.RecentProject) then
    FProject.LoadFromFile(FPreferences.RecentProject)
  else
  begin
    // set default values
    FProject.BaseDirectory := ExtractFileDir(ParamStr(0));
    FProject.ChannelsFilename := 'WebUpdate\Channels.json';
  end;

  // create channels
  FChannels := TWebUpdateChannels.Create;

  SetupDefaultChannels;

  FWebUpdate := TWebUpdate.Create;
  FWebUpdate.GetCurrentChannelInformation;
end;

procedure TFormWebUpdateTool.FormDestroy(Sender: TObject);
begin
  FWebUpdate.Free;
  FChannels.Free;
  FProject.Free;
  FPreferences.Free;
end;

procedure TFormWebUpdateTool.FormShow(Sender: TObject);
begin
  LoadChannels;
end;

function TFormWebUpdateTool.GetCurrentChannelNodeData: PChannelItem;
var
  Node: PVirtualNode;
begin
  Result := nil;

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

procedure TFormWebUpdateTool.ActionChannelsRecallExecute(Sender: TObject);
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
  ChannelFileName: TFileName;
  ChannelSetup: TWebUpdateChannelSetup;
begin
  LoadChannels;

  TreeChannels.BeginUpdate;
  try
    for Node in TreeChannels.Nodes do
    begin
      NodeData := TreeChannels.GetNodeData(Node);
      ChannelFileName := Project.ChannelsPath +
        {NodeData^.Name + '\' + } NodeData^.FileName;

      // now check for file date
      if FileExists(ChannelFileName) then
      begin
        ChannelSetup := TWebUpdateChannelSetup.Create;
        try
          ChannelSetup.LoadFromFile(ChannelFileName);
          if NodeData^.Modified <> ChannelSetup.Modified then
            if MessageDlg(Format('The setup for channel %s has changed!',
              [NodeData^.Name])+#13#10#13#10 + 'Update channel list?',
              mtConfirmation, [mbYes, mbNo], 0) = mrYes then
              NodeData^.Modified := ChannelSetup.Modified;
        finally
          ChannelSetup.Free;
        end;
      end;
    end;
  finally
    TreeChannels.EndUpdate;
  end;
end;

procedure TFormWebUpdateTool.ActionChannelsStoreExecute(Sender: TObject);
begin
  SaveChannels;
end;

procedure TFormWebUpdateTool.ActionCopyUploadExecute(Sender: TObject);
begin
//  UploadSnapshot;
  CopySnapshot;
end;

procedure TFormWebUpdateTool.ActionCheckUpdateExecute(Sender: TObject);
begin
  FWebUpdate.GetCurrentChannelInformation;
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

procedure TFormWebUpdateTool.ActionTakeSnapshotExecute(Sender: TObject);
begin
  TakeSnapshot;
end;

procedure TFormWebUpdateTool.ActionUpdateExecute(Sender: TObject);
begin
  FWebUpdate.PerformUpdate;
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
    end;
  finally
    Free;
  end;
end;

procedure TFormWebUpdateTool.ActionScanFilesExecute(Sender: TObject);
begin
  ScanDirectory(Project.BaseDirectory);
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

function TFormWebUpdateTool.CreateNode(FileStrings: TStringDynArray): PVirtualNode;
var
  Level: Integer;
begin
  Result := TreeFileList.RootNode;
  for Level := 0 to High(FileStrings) do
    Result := LocateNode(Result, FileStrings[Level]);
end;

procedure TFormWebUpdateTool.ScanDirectory(const BaseDirectory: string);
var
  FileStrings: TStringDynArray;
  FileList: TStringList;
  FileName: TFileName;
  WebFileName: string;
  Node: PVirtualNode;
  NodeData: PFileItem;
  BasePath: string;
  Fad: TWin32FileAttributeData;
begin
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
        NodeData^.WebFileName := StringReplace(WebFileName, '\', '/', [rfReplaceAll]);
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

procedure TFormWebUpdateTool.LoadChannelSetup(const ChannelSetup: TWebUpdateChannelSetup);
var
  Node: PVirtualNode;
  NodeData: PFileItem;
  FileStrings: TStringDynArray;
  WebFileName, RealFileName: string;
  Item: TWebUpdateFileItem;
begin
  // store current channel name
  if ChannelSetup.ChannelName <> '' then
    FCurrentChannel := ChannelSetup.ChannelName;
  FAppName := ChannelSetup.AppName;

  TreeFileList.BeginUpdate;
  try
    for Item in ChannelSetup.Items do
    begin
      WebFileName := Item.FileName;
      FileStrings := SplitString(WebFileName, '/');
      RealFileName := Project.BasePath + StringReplace(
        WebFileName, '/', '\', [rfReplaceAll]);

      if FileExists(RealFileName) then
      begin
        Node := CreateNode(FileStrings);
        Node.CheckState := csCheckedNormal;
        NodeData := TreeFileList.GetNodeData(Node);
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
end;

procedure TFormWebUpdateTool.SaveChannelSetup(const ChannelSetup: TWebUpdateChannelSetup);
var
  Node: PVirtualNode;
  NodeData: PFileItem;
  Item: TWebUpdateFileItem;
  LastModified: TDateTime;
begin
  // check if any files are selected at all
  if TreeFileList.CheckedCount = 0 then
    if MessageDlg(RStrNoFileSelected, mtError, [mbOK], 0) = mrOk then
      Exit;

  // update status
  WriteStatus(RStrSavingChannelSetup);

  // store current channel name
  ChannelSetup.AppName := FAppName;
  ChannelSetup.ChannelName := FCurrentChannel;
  LastModified := 0;
  for Node in TreeFileList.CheckedNodes do
  begin
    NodeData := TreeFileList.GetNodeData(Node);
    if NodeData^.WebFileName = '' then
      Continue;

    // create (& update) file item
    Item := TWebUpdateFileItem.Create;
    Item.FileName := NodeData^.WebFileName;
    Item.Modified := NodeData^.Modified;
    Item.FileSize := NodeData^.Size;
    Item.MD5Hash := MD5(NodeData.FileName);

    if NodeData^.Modified > LastModified then
      LastModified := NodeData^.Modified;

    // add item to file items list
    ChannelSetup.Items.Add(Item);
  end;
  ChannelSetup.Modified := LastModified;

  ClearStatus;
end;

procedure TFormWebUpdateTool.LoadChannels;
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
  ChannelFileName: TFileName;
  ChannelItem: TWebUpdateChannelItem;
begin
  // get channel file name and check for existence
  ChannelFileName := Project.FullChannelsFilename;
  if not FileExists(ChannelFileName) then
    Exit;

  // now load channels from file
  FChannels.LoadFromFile(ChannelFileName);

  // clear channel tree
  TreeChannels.Clear;

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
    if SameText(ChannelItem.Name, FProject.CurrentChannel) then
      TreeChannels.CheckState[Node] := csCheckedNormal;
  end;
end;

procedure TFormWebUpdateTool.SaveChannels;
var
  Node: PVirtualNode;
  NodeData: PChannelItem;
  ChannelSetup: TWebUpdateChannelSetup;
  ChannelItem: TWebUpdateChannelItem;
begin
  // clear existing channels
  FChannels.Items.Clear;

  // enumerate nodes
  TreeChannels.BeginUpdate;
  try
    for Node in TreeChannels.Nodes do
    begin
      NodeData := TreeChannels.GetNodeData(Node);

      // eventually save current channel
      if Node.CheckState = csCheckedNormal then
      begin
        ChannelSetup := TWebUpdateChannelSetup.Create;
        try
          // first save current channel setup
          SaveChannelSetup(ChannelSetup);

          ChannelSetup.SaveToFile(Project.ChannelsPath + {NodeData^.Name + '\' + } NodeData^.FileName);
          NodeData^.Modified := ChannelSetup.Modified;
        finally
          ChannelSetup.Free;
        end;
      end;

      ChannelItem := TWebUpdateChannelItem.Create;
      ChannelItem.Name := NodeData^.Name;
      ChannelItem.FileName := NodeData^.FileName;
      ChannelItem.Modified := NodeData^.Modified;

      FChannels.Items.Add(ChannelItem);
    end;
  finally
    TreeChannels.EndUpdate;
  end;

  FChannels.SaveToFile(FormWebUpdateTool.Project.FullChannelsFilename);
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
    if SameText(NodeData^.Name, FProject.CurrentChannel) then
      TreeChannels.CheckState[Node] := csCheckedNormal;
  end;
end;

procedure TFormWebUpdateTool.TakeSnapshot;
var
  Node: PVirtualNode;
  NodeData: PFileItem;
  NodeChannelData: PChannelItem;
  Item: TWebUpdateFileItem;
  ChannelItem: TWebUpdateChannelItem;
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
  FileName := Project.ChannelsPath + //NodeChannelData^.Name + '\' +
    NodeChannelData^.FileName;

  // create selected channels
  ChannelSetup := TWebUpdateChannelSetup.Create;
  try
    // store current channel name
    ChannelSetup.AppName := FAppName;
    ChannelSetup.ChannelName := FCurrentChannel;
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

      // create (& update) file item
      Item := TWebUpdateFileItem.Create;
      Item.FileName := NodeData^.WebFileName;
      Item.Modified := NodeData^.Modified;
      Item.FileSize := NodeData^.Size;
      Item.MD5Hash := MD5(NodeData.FileName);

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

      FChannels.Items.Add(ChannelItem);
    end;
  finally
    TreeChannels.EndUpdate;
  end;
  FChannels.SaveToFile(FormWebUpdateTool.Project.FullChannelsFilename);

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

procedure TFormWebUpdateTool.TreeChannelsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: PChannelItem;
  FileName: TFileName;
  ChannelSetup: TWebUpdateChannelSetup;
begin
  if not Assigned(Node) then
    Exit;

  NodeData := TreeChannels.GetNodeData(Node);
  FCurrentChannel := NodeData^.Name;

  FileName := Project.ChannelsPath + {NodeData^.Name + '\' + } NodeData^.FileName;

  // check if file exists
  if not FileExists(FileName) then
    Exit;

  // create selected channels
  ChannelSetup := TWebUpdateChannelSetup.Create;
  try
    // load setup from file
    ChannelSetup.LoadFromFile(FileName);

    LoadChannelSetup(ChannelSetup);
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
          ImageIndex := 14
        else if TreeFileList.Expanded[Node] then
          ImageIndex := 13
        else
          ImageIndex := 12;
      end;
    end;
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
  end;
end;

procedure TFormWebUpdateTool.UploadSnapshot;
var
  ChannelSetup: TWebUpdateChannelSetup;
  FileItem: TWebUpdateFileItem;
  ChannelName: string;
  RealFileName, ChannelFileName, ChannelWebName: TFileName;
  NodeData: PChannelItem;
begin
  // get currently checked channel node
  NodeData := GetCurrentChannelNodeData;
  if not Assigned(NodeData) then
    Exit;

  ChannelName := NodeData^.Name;
  ChannelFileName := Project.ChannelsPath + {ChannelName + '\' + }
    NodeData^.FileName;
  ChannelWebName := NodeData^.Name + '/' + NodeData^.FileName;

  with TIdFTP.Create(nil) do
  try
    OnWork := WorkEventHandler;
    Username := Project.FTP.Username;
    Password := Project.FTP.Password;
    Host := Project.FTP.Server;
    Connect;
    try

      // upload files
      ChannelSetup := TWebUpdateChannelSetup.Create;
      try
        ChannelSetup.LoadFromFile(ChannelFileName);
        for FileItem in ChannelSetup.Items do
        begin
          WriteStatus('Uploading: ' + FileItem.FileName);

          RealFileName := Project.BasePath + StringReplace(
            FileItem.FileName, '/', '\', [rfReplaceAll]);
          Put(RealFileName, ChannelName + '/' + FileItem.FileName);
        end;
      finally
        ChannelSetup.Free;
      end;

      WriteStatus('Uploading channel setup...');

      // upload channel setup
      Put(ChannelFileName, ChannelWebName);

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

procedure TFormWebUpdateTool.CopySnapshot;
var
  Path: string;
  ChannelSetup: TWebUpdateChannelSetup;
  FileItem: TWebUpdateFileItem;
  ChannelName: string;
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
  ChannelFileName := Project.ChannelsPath + // ChannelName + '\' +
    NodeData^.FileName;

  // upload files
  ChannelSetup := TWebUpdateChannelSetup.Create;
  try
    ChannelSetup.LoadFromFile(ChannelFileName);
    for FileItem in ChannelSetup.Items do
    begin
      WriteStatus('Copying file ' + FileItem.FileName + '...');

      RealFileName := Project.BasePath + StringReplace(
        FileItem.FileName, '/', '\', [rfReplaceAll]);

      DestFileName := ExpandFileName(Path + ChannelName + '\' + FileItem.FileName);
      ForceDirectories(ExtractFileDir(DestFileName));
      CopyFile(PWideChar(RealFileName), PWideChar(DestFileName), True);
    end;
  finally
    ChannelSetup.Free;
  end;

  WriteStatus('Uploading channel setup...');

  // upload channel setup
  DestFileName := ExpandFileName(Path + ChannelName + '\' + NodeData^.FileName);
  ForceDirectories(ExtractFileDir(DestFileName));
  CopyFile(PWideChar(ChannelFileName), PWideChar(DestFileName), True);

  WriteStatus('Uploading channels list...');

  // upload channel file
  DestFileName := ExpandFileName(Path + ExtractFileName(FProject.ChannelsFilename));
  CopyFile(PWideChar(FProject.FullChannelsFilename), PWideChar(DestFileName), True);

  ClearStatus;
end;

procedure TFormWebUpdateTool.ClearStatus;
begin
  StatusBar.Panels[0].Text := '';
end;

procedure TFormWebUpdateTool.WorkEventHandler(ASender: TObject;
  AWorkMode: TWorkMode; AWorkCount: Int64);
begin

(*
  Encoding := TIdFTP(ASender).Response.TransferEncoding;
  ContentLength := TIdFTP(ASender).Response.ContentLength;

  if (Pos('chunked', LowerCase(Encoding)) = 0) and (ContentLength > 0) then
  begin
    Synchronize(procedure
    begin
      FOnProgress(Self, 100 * AWorkCount div ContentLength, AWorkCount - FLastWorkCount);
    end);
  end;
*)
end;

procedure TFormWebUpdateTool.WriteStatus(Text: string);
begin
  StatusBar.Panels[0].Text := Text;
  Application.ProcessMessages;
end;

procedure TFormWebUpdateTool.CheckForUpdateTimerTimer(Sender: TObject);
begin
  CheckForUpdateTimer.Enabled := False;
  ActionCheckUpdateExecute(Sender);
end;

end.
