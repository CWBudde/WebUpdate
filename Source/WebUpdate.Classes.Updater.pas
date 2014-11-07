unit WebUpdate.Classes.Updater;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  IdHTTP, IdComponent, dwsComp, dwsExprs, dwsErrors,
  WebUpdate.JSON.Channels, WebUpdate.JSON.Channel;

type
  EHttpDownload = class(Exception);

  TFileItem = class
  type
    TFileAction = (faAdd, faChange, faDelete);
  private
    FFileName: TFileName;
    FMD5Hash: Integer;
    FAction: TFileAction;
  public
    constructor Create(const FileName: TFileName; MD5Hash: Integer);

    property FileName: TFileName read FFileName write FFileName;
    property MD5Hash: Integer read FMD5Hash write FMD5Hash;
    property Action: TFileAction read FAction write FAction;
  end;
  TFileItemList = TList<TFileItem>;

  TUpdaterThread = class(TThread)
  type
    TProgressEvent = procedure (Sender: TObject; Progress: Integer; ByteCount: Integer) of object;
    TFileChangedEvent = procedure (Sender: TObject; const FileName: TFileName) of object;
    TMD5MismatchEvent = procedure (Sender: TObject; const FileName: TFileName; var Ignore: Boolean) of object;
    TScriptErrorsEvent = procedure (Sender: TObject; const MessageList: TdwsMessageList) of object;
  private
    FFilesToAdd: TWebUpdateFileItems;
    FFilesToDelete: TWebUpdateFileItems;
    FFiles: TFileItemList;
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
    FOnMD5Mismatch: TMD5MismatchEvent;
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
    property OnMD5Mismatch: TMD5MismatchEvent read FOnMD5Mismatch write FOnMD5Mismatch;
    property OnScriptErrors: TScriptErrorsEvent read FOnScriptErrors write FOnScriptErrors;
  end;

  TWebUpdater = class
  private
    FBaseURL: string;
    FChannelName: string;
    FChannels: TWebUpdateChannels;
    FChannelsFileName: TFileName;
    FLocalBasePath: string;
    FLocalFileName: TFileName;
    FNewSetup: TWebUpdateChannelSetup;
    FThread: TUpdaterThread;

    FOnFileChanged: TUpdaterThread.TFileChangedEvent;
    FOnScriptErrors: TUpdaterThread.TScriptErrorsEvent;
    FOnDone: TNotifyEvent;
    FOnMD5Mismatch: TUpdaterThread.TMD5MismatchEvent;
    FOnProgress: TUpdaterThread.TProgressEvent;

    procedure MD5MismatchHandler(Sender: TObject; const FileName: TFileName; var Ignore: Boolean);
    procedure ProgressEventHandler(Sender: TObject; Progress: Integer; ByteCount: Integer);
    procedure FileChangedEventHandler(Sender: TObject; const FileName: TFileName);
    procedure ScriptErrorsEventHandler(Sender: TObject; const MessageList: TdwsMessageList);
    procedure DoneEventHandler(Sender: TObject);
    function GetChannelBasePath: string;
    function GetMainAppFileName: TFileName;
  protected
    procedure LoadSetupFromFile(const FileName: TFileName);
    procedure LoadChannels;
    property ChannelBasePath: string read GetChannelBasePath;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Abort;
    procedure PerformWebUpdate;
    procedure GetChannelNames(const ChannelNames: TStringList);

    property BaseURL: string read FBaseURL write FBaseURL;
    property ChannelName: string read FChannelName write FChannelName;
    property Channels: TWebUpdateChannels read FChannels;
    property ChannelsFileName: TFileName read FChannelsFileName write FChannelsFileName;
    property LocalFileName: TFileName read FLocalFileName write FLocalFileName;
    property MainAppFileName: TFileName read GetMainAppFileName;

    property OnProgress: TUpdaterThread.TProgressEvent read FOnProgress write FOnProgress;
    property OnFileChanged: TUpdaterThread.TFileChangedEvent read FOnFileChanged write FOnFileChanged;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnScriptErrors: TUpdaterThread.TScriptErrorsEvent read FOnScriptErrors write FOnScriptErrors;
    property OnMD5Mismatch: TUpdaterThread.TMD5MismatchEvent read FOnMD5Mismatch write FOnMD5Mismatch;
  end;

implementation

uses
  dwsUtils, WebUpdate.MD5;

resourcestring
  RStrBaseURL = 'http://www.savioursofsoul.de/Christian/WebUpdate/';
  RStrChannelsFile = 'Channels.json';
  RStrLocalFile = 'WebUpdate.json';

{ TFileItem }

constructor TFileItem.Create(const FileName: TFileName; MD5Hash: Integer);
begin
  FFileName := FileName;
  FMD5Hash := MD5Hash;
  FAction := faAdd;
end;


{ TUpdaterThread }

constructor TUpdaterThread.Create;
begin
  FFilesToAdd := TWebUpdateFileItems.Create;
  FFilesToDelete := TWebUpdateFileItems.Create;

//  FFiles := TFileItemList.Create;

  FPreScript := '';
  FPostScript := '';
  FLastWorkCount := 0;

  inherited Create(False);
end;

destructor TUpdaterThread.Destroy;
begin
  FFilesToAdd.Free;
  FFilesToDelete.Free;

  FFiles.Free;
  inherited;
end;

procedure TUpdaterThread.HttpWork(ASender: TObject; AWorkMode: TWorkMode;
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

procedure TUpdaterThread.RunScript(SourceCode: string);
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

procedure TUpdaterThread.Execute;
var
  MS: TMemoryStream;
  Item: TWebUpdateFileItem;
  Hash: Integer;
  IgnoreMD5Error: Boolean;
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
          begin
            IgnoreMD5Error := False;
            if Assigned(FOnMD5Mismatch) then
              FOnMD5Mismatch(Self, Item.FileName, IgnoreMD5Error);
            if not IgnoreMD5Error then
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


{ TWebUpdater }

constructor TWebUpdater.Create;
begin
  FChannels := TWebUpdateChannels.Create;
  FNewSetup := TWebUpdateChannelSetup.Create;

  // specify default values
  FBaseURL := RStrBaseURL;
  FChannelsFileName := RStrChannelsFile;
  FLocalFileName := RStrLocalFile;
  FLocalBasePath := ExtractFilePath(ParamStr(0));
end;

destructor TWebUpdater.Destroy;
begin
  // eventually terminate file download thread
  if Assigned(FThread) then
    FThread.Terminate;

  // free objects
  FChannels.Free;
  FNewSetup.Free;
  if Assigned(FThread) then
    FThread.WaitFor;
  FThread.Free;

  inherited;
end;

procedure TWebUpdater.Abort;
begin
  if Assigned(FThread) then
    FThread.Terminate;
end;

procedure TWebUpdater.DoneEventHandler(Sender: TObject);
begin
  // save new setup
  FNewSetup.SaveToFile(FLocalBasePath + 'WebUpdate.json');

  FThread := nil;

  if Assigned(FOnDone) then
    FOnDone(Sender);
end;

procedure TWebUpdater.FileChangedEventHandler(Sender: TObject;
  const FileName: TFileName);
begin
  if Assigned(OnFileChanged) then
    OnFileChanged(Sender, FileName);
end;

procedure TWebUpdater.ScriptErrorsEventHandler(Sender: TObject;
  const MessageList: TdwsMessageList);
begin
  if Assigned(OnScriptErrors) then
    OnScriptErrors(Sender, MessageList);
end;

function TWebUpdater.GetChannelBasePath: string;
begin
  Result :=FBaseURL + ChannelName + '/';
end;

procedure TWebUpdater.GetChannelNames(const ChannelNames: TStringList);
var
  Item: TWebUpdateChannelItem;
begin
  // load channels setup from URI
  LoadChannels;

  // get channel names
  ChannelNames.Clear;
  for Item in Channels.Items do
    ChannelNames.Add(Item.Name);
end;

function TWebUpdater.GetMainAppFileName: TFileName;
begin
  Result := '';
  if FNewSetup.AppName <> '' then
    Result := FLocalBasePath + StringReplace(FNewSetup.AppName, '/', '\',
      [rfReplaceAll]);
end;

procedure TWebUpdater.PerformWebUpdate;
var
  TotalBytes: Integer;

  procedure AddFileItem(Item: TWebUpdateFileItem);
  begin
    FThread.FilesToAdd.Add(Item);
    TotalBytes := TotalBytes + Item.FileSize;
  end;

var
  ChannelItem: TWebUpdateChannelItem;
  Item, CurrentItem: TWebUpdateFileItem;
  LocalSetup: TWebUpdateChannelSetup;
  ItemFlag: Boolean;
begin
  LoadChannels;

  for ChannelItem in FChannels.Items do
    if UnicodeSameText(ChannelItem.Name, ChannelName) then
    begin
      LoadSetupFromFile(ChannelItem.FileName);
      Break;
    end;

  FThread := TUpdaterThread.Create;

  TotalBytes := 0;

  // eventually load existing setup
  if FileExists(FLocalBasePath + FLocalFileName) then
  begin
    LocalSetup := TWebUpdateChannelSetup.Create;
    try
      LocalSetup.LoadFromFile(FLocalBasePath + FLocalFileName);

      for Item in FNewSetup.Items do
      begin
        // assume the item should be added to the files list
        ItemFlag := True;

        // now check items in current setup to see if this must be replaced
        for CurrentItem in LocalSetup.Items do
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
      end;

      // add files to delete list (if not part of the "FilesToAdd" list)
      for CurrentItem in LocalSetup.Items do
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
    finally
      LocalSetup.Free;
    end;
  end
  else
  begin
    // add all files
    for Item in FNewSetup.Items do
      AddFileItem(Item);
  end;


//  ProgressBarTotal.Max := TotalBytes;


  // specify event handlers
  FThread.OnProgress := ProgressEventHandler;
  FThread.OnFileChanged := FileChangedEventHandler;
  FThread.OnDone := DoneEventHandler;
  FThread.OnScriptErrors := ScriptErrorsEventHandler;
  FThread.OnMD5Mismatch := MD5MismatchHandler;

  // specify paths
  FThread.BasePath := ChannelBasePath;
  FThread.LocalPath := FLocalBasePath;

  FThread.Suspended := False;
end;

procedure TWebUpdater.LoadSetupFromFile(const FileName: TFileName);
var
  Http: TIdHTTP;
  Text: string;
  Success: Boolean;
begin
  Http := TIdHTTP.Create(nil);
  try
    try
      // get text from URI
      Text := Http.Get(ChannelBasePath + FileName);

      // check if text is available (might need check for JSON format)
      Success := Text <> '';
    except
      Success := False;
    end;

    // handle errors
    if not Success then
      raise EHttpDownload.CreateFmt('Error downloading from URL %s',
        [BaseURL + ChannelsFileName]);
  finally
    Http.Free;
  end;

  // now load setup from string
  FNewSetup.LoadFromString(Text);
end;

procedure TWebUpdater.MD5MismatchHandler(Sender: TObject;
  const FileName: TFileName; var Ignore: Boolean);
begin
  // event redirection
  if Assigned(FOnMD5Mismatch) then
    FOnMD5Mismatch(Sender, FileName, Ignore);
end;

procedure TWebUpdater.ProgressEventHandler(Sender: TObject; Progress,
  ByteCount: Integer);
begin
  // event redirection
  if Assigned(OnProgress) then
    OnProgress(Sender, Progress, ByteCount);
end;

procedure TWebUpdater.LoadChannels;
var
  Http: TIdHTTP;
  Text: string;
  Success: Boolean;
begin
  Http := TIdHTTP.Create(nil);
  try
    try
      // get text from URL
      Text := Http.Get(BaseURL + ChannelsFileName);

      // check if text is available (might need check for JSON format)
      Success := Text <> '';
    except
      Success := False;
    end;

    // handle errors
    if not Success then
      raise EHttpDownload.CreateFmt('Error downloading from URL %s',
        [BaseURL + ChannelsFileName]);
  finally
    Http.Free;
  end;

  // load channels from string
  FChannels.LoadFromString(Text);
end;

end.
