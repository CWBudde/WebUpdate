unit WebUpdate.Classes.Updater;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  IdHTTP, IdComponent,
  dwsComp, dwsExprs, dwsErrors, WebUpdate.JSON.Channels, WebUpdate.JSON.Channel;

type
  EHttpDownload = class(Exception);

  TFileItem = class
  type
    TFileAction = (faAdd, faChange, faDelete, faVerify);
  private
    FFileName: TFileName;
    FLocalFileName: TFileName;
    FMD5Hash: Integer;
    FAction: TFileAction;
    FModified: TDateTime;
    FFileSize: Integer;
    procedure SetFileName(const Value: TFileName);
  protected
    procedure UpdateLocalFileName;
  public
    constructor Create(const FileName: TFileName; MD5Hash: Integer = 0;
      FileSize: Integer = 0; Modified: TDateTime = 0);

    property FileName: TFileName read FFileName write SetFileName;
    property LocalFileName: TFileName read FLocalFileName;
    property MD5Hash: Integer read FMD5Hash write FMD5Hash;
    property Action: TFileAction read FAction write FAction;
    property Modified: TDateTime read FModified write FModified;
    property FileSize: Integer read FFileSize write FFileSize;
  end;

  TFileItemList = class(TList<TFileItem>)
  public
    function LocateItemByFileName(const FileName: TFileName): TFileItem;
  end;

  TUpdaterThread = class(TThread)
  type
    TProgressEvent = procedure (Sender: TObject; Progress: Integer; ByteCount: Integer) of object;
    TFileNameProgressEvent = procedure (Sender: TObject; const FileName: TFileName) of object;
    TMD5MismatchEvent = procedure (Sender: TObject; const FileName: TFileName; var Ignore: Boolean) of object;
    TScriptErrorsEvent = procedure (Sender: TObject; const MessageList: TdwsMessageList) of object;
  private
    FFiles: TFileItemList;
    FHttp: TIdHttp;
    FLocalPath: string;
    FBasePath: string;
    FPostScript: string;
    FPreScript: string;
    FLastWorkCount: Integer;
    FOnFileNameProgress: TFileNameProgressEvent;
    FOnProgress: TProgressEvent;
    FOnDone: TNotifyEvent;
    FOnScriptErrors: TScriptErrorsEvent;
    FOnMD5Mismatch: TMD5MismatchEvent;
    procedure HttpWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  protected
    procedure RunScript(SourceCode: string);
    procedure Execute; override;
  public
    constructor Create(const Files: TFileItemList); reintroduce;

    property Files: TFileItemList read FFiles;
    property BasePath: string read FBasePath write FBasePath;
    property LocalPath: string read FLocalPath write FLocalPath;

    property PreScript: string read FPreScript write FPreScript;
    property PostScript: string read FPostScript write FPostScript;

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnFileNameProgress: TFileNameProgressEvent read FOnFileNameProgress write FOnFileNameProgress;
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
    FLocalChannelFileName: TFileName;
    FNewSetup: TWebUpdateChannelSetup;
    FThread: TUpdaterThread;
    FFileItemListCache: TFileItemList;
    FTotalSize: Int64;

    FOnFileNameProgress: TUpdaterThread.TFileNameProgressEvent;
    FOnScriptErrors: TUpdaterThread.TScriptErrorsEvent;
    FOnDone: TNotifyEvent;
    FOnMD5Mismatch: TUpdaterThread.TMD5MismatchEvent;
    FOnProgress: TUpdaterThread.TProgressEvent;

    function GetChannelBasePath: string;
    function GetFileItemList: TFileItemList;
    function GetMainAppFileName: TFileName;
    procedure SetChannelName(const Value: string);
    procedure SetBaseURL(Value: string);
    procedure SetChannelsFileName(const Value: TFileName);
    procedure SetLocalChannelFileName(const Value: TFileName);
    function GetTotalSize: Int64;
  protected
    procedure MD5MismatchHandler(Sender: TObject; const FileName: TFileName; var Ignore: Boolean);
    procedure ProgressEventHandler(Sender: TObject; Progress: Integer; ByteCount: Integer);
    procedure FileChangedEventHandler(Sender: TObject; const FileName: TFileName);
    procedure ScriptErrorsEventHandler(Sender: TObject; const MessageList: TdwsMessageList);
    procedure DoneEventHandler(Sender: TObject);

    procedure BuildFileListCache;
    procedure ResetFileListCache;
    procedure LoadSetupFromFile(const FileName: TFileName);
    procedure LoadChannels;

    property ChannelBasePath: string read GetChannelBasePath;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Abort;
    procedure PerformWebUpdate;
    procedure GetChannelNames(const ChannelNames: TStringList);

    property BaseURL: string read FBaseURL write SetBaseURL;
    property ChannelName: string read FChannelName write SetChannelName;
    property Channels: TWebUpdateChannels read FChannels;
    property ChannelsFileName: TFileName read FChannelsFileName write SetChannelsFileName;
    property LocalChannelFileName: TFileName read FLocalChannelFileName write SetLocalChannelFileName;
    property MainAppFileName: TFileName read GetMainAppFileName;
    property FileItemList: TFileItemList read GetFileItemList;
    property TotalBytes: Int64 read GetTotalSize;

    property OnProgress: TUpdaterThread.TProgressEvent read FOnProgress write FOnProgress;
    property OnFileNameProgress: TUpdaterThread.TFileNameProgressEvent read FOnFileNameProgress write FOnFileNameProgress;
    property OnDone: TNotifyEvent read FOnDone write FOnDone;
    property OnScriptErrors: TUpdaterThread.TScriptErrorsEvent read FOnScriptErrors write FOnScriptErrors;
    property OnMD5Mismatch: TUpdaterThread.TMD5MismatchEvent read FOnMD5Mismatch write FOnMD5Mismatch;
  end;

implementation

uses
  dwsUtils, IdSSLOpenSSL, WebUpdate.MD5;

{ TFileItem }

constructor TFileItem.Create(const FileName: TFileName; MD5Hash: Integer = 0;
  FileSize: Integer = 0; Modified: TDateTime = 0);
begin
  FFileName := FileName;
  FMD5Hash := MD5Hash;
  FFileSize := FileSize;
  FModified := Modified;
  FAction := faAdd;
  UpdateLocalFileName;
end;

procedure TFileItem.SetFileName(const Value: TFileName);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    UpdateLocalFileName;
  end;
end;

procedure TFileItem.UpdateLocalFileName;
begin
  FLocalFileName := StringReplace(FFileName, '/', '\', [rfReplaceAll]);
end;


{ TFileItemList }

function TFileItemList.LocateItemByFileName(
  const FileName: TFileName): TFileItem;
var
  Index: Integer;
begin
  Result := nil;
  for Index := 0 to Count - 1 do
    if UnicodeSameText(Items[Index].FileName, FileName) then
      Exit(Items[Index]);
end;


{ TUpdaterThread }

constructor TUpdaterThread.Create(const Files: TFileItemList);
begin
  FFiles := Files;

  FPreScript := '';
  FPostScript := '';
  FLastWorkCount := 0;

  inherited Create(False);
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
  Item: TFileItem;
  Hash: Integer;
  IgnoreMD5Error: Boolean;
begin
  // eventually run pre script
  if FPreScript <> '' then
    RunScript(FPreScript);

  FHttp := TIdHTTP.Create(nil);
  try
    // eventually create SSL IO handler
    if FileExists(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll') and
      FileExists(ExtractFilePath(ParamStr(0)) + 'libeay32.dll') then
      FHttp.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

    FHttp.OnWork := HttpWork;
    MS := TMemoryStream.Create;
    try
      for Item in FFiles do
      begin
        // eventually call 'file changed' event
        if Assigned(FOnFileNameProgress) then
          Synchronize(procedure
          begin
            FOnFileNameProgress(Self, Item.FileName);
          end);

        // eventually delete file and continue with next file
        if Item.Action = faDelete then
        begin
          DeleteFile(FLocalPath + Item.LocalFileName);
          Continue;
        end;

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
        MS.SaveToFile(FLocalPath + Item.LocalFileName);

        // eventually update modification date/time
        if Item.Modified > 0 then
          FileSetDate(FLocalPath + Item.LocalFileName, DateTimeToFileDate(Item.Modified));

        // check if terminated
        if Terminated then
          Exit;
      end;
    finally
      MS.Free;
    end;
  finally
    FHttp.IOHandler.Free;
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

procedure TWebUpdater.BuildFileListCache;
var
  ChannelItem: TWebUpdateChannelItem;
  Item: TWebUpdateFileItem;
  FileItem: TFileItem;
  LocalSetup: TWebUpdateChannelSetup;
begin
  LoadChannels;

  for ChannelItem in FChannels.Items do
    if UnicodeSameText(ChannelItem.Name, ChannelName) then
    begin
      LoadSetupFromFile(ChannelItem.FileName);
      Break;
    end;

  FTotalSize := 0;

  // eventually load existing setup
  if FileExists(FLocalChannelFileName) then
  begin
    LocalSetup := TWebUpdateChannelSetup.Create;
    try
      LocalSetup.LoadFromFile(FLocalChannelFileName);

      // assume deletion of all files currently present
      for Item in LocalSetup.Items do
      begin
        FileItem := TFileItem.Create(Item.FileName, Item.MD5Hash, Item.FileSize,
          Item.Modified);
        FileItem.Action := faDelete;
        FFileItemListCache.Add(FileItem);
      end;
    finally
      LocalSetup.Free;
    end;
  end;

  // add all files (and eventually mark as
  for Item in FNewSetup.Items do
  begin
    // now check if file is already present (from local setup)
    FileItem := FFileItemListCache.LocateItemByFileName(Item.FileName);

    if Assigned(FileItem) then
    begin
      // check if file is marked for explicit deletion
      if Item.Action = iaDelete then
      begin
        FileItem.Action := faDelete;

        // set MD5 hash, size and modification date to 0 => always delete!
        FileItem.MD5Hash := 0;
        FileItem.Modified := 0;
        FileItem.FileSize := 0;

        Continue;
      end;

      // check if file is (supposed to be) identical to previous version
      if (FileItem.Modified = Item.Modified) and
        (FileItem.FileSize = Item.FileSize) and
        (FileItem.MD5Hash = Item.MD5Hash) then
        FileItem.Action := faVerify
      else
      begin
        // set action to 'change' and update properties
        FileItem.Action := faChange;
        FileItem.MD5Hash := Item.MD5Hash;
        FileItem.Modified := Item.Modified;
        FileItem.FileSize := Item.FileSize;
      end;
    end
    else
    begin
      FileItem := TFileItem.Create(Item.FileName, Item.MD5Hash, Item.FileSize,
        Item.Modified);
      FFileItemListCache.Add(FileItem);

      // check if the item action is to delete the file
      if Item.Action = iaDelete then
      begin
        FileItem.Action := faDelete;
        Continue;
      end;
    end;

    // inc total file size (if item is about to be added
    if Item.Action <> iaDelete then
      FTotalSize := FTotalSize + Item.FileSize;
  end;
end;

constructor TWebUpdater.Create;
begin
  FChannels := TWebUpdateChannels.Create;
  FNewSetup := TWebUpdateChannelSetup.Create;

  // specify default values
  FBaseURL := '';
  FChannelsFileName := 'Channels.json';
  FLocalChannelFileName := ExtractFilePath(ParamStr(0)) + 'WebUpdate.json';
end;

destructor TWebUpdater.Destroy;
begin
  // eventually terminate file download thread
  if Assigned(FThread) then
    FThread.Terminate;

  // free objects
  FFileItemListCache.Free;
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
  ResetFileListCache;
end;

procedure TWebUpdater.DoneEventHandler(Sender: TObject);
begin
  // save new setup
  FNewSetup.SaveToFile(FLocalChannelFileName);

  FThread := nil;

  if Assigned(FOnDone) then
    FOnDone(Sender);
end;

procedure TWebUpdater.FileChangedEventHandler(Sender: TObject;
  const FileName: TFileName);
begin
  if Assigned(OnFileNameProgress) then
    OnFileNameProgress(Sender, FileName);
end;

procedure TWebUpdater.ScriptErrorsEventHandler(Sender: TObject;
  const MessageList: TdwsMessageList);
begin
  if Assigned(OnScriptErrors) then
    OnScriptErrors(Sender, MessageList);
end;

procedure TWebUpdater.SetBaseURL(Value: string);
begin
  if not StrEndsWith(Value, '/') then
    Value := Value + '/';

  if FBaseURL <> Value then
  begin
    FBaseURL := Value;

    ResetFileListCache;
  end;
end;

procedure TWebUpdater.SetChannelName(const Value: string);
begin
  if FChannelName <> Value then
  begin
    FChannelName := Value;
    ResetFileListCache;
  end;
end;

procedure TWebUpdater.SetChannelsFileName(const Value: TFileName);
begin
  if FChannelsFileName <> Value then
  begin
    FChannelsFileName := Value;
    ResetFileListCache;
  end;
end;

procedure TWebUpdater.SetLocalChannelFileName(const Value: TFileName);
begin
  if FLocalChannelFileName <> Value then
  begin
    FLocalChannelFileName := Value;
    ResetFileListCache;
  end;
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

function TWebUpdater.GetFileItemList: TFileItemList;
begin
  // eventually create file item list (if not already created)
  if Assigned(FFileItemListCache) then
    Exit(FFileItemListCache);

  FFileItemListCache := TFileItemList.Create;
  BuildFileListCache;
  Result := FFileItemListCache;
end;

function TWebUpdater.GetMainAppFileName: TFileName;
begin
  Result := '';
  if FNewSetup.AppName <> '' then
  begin
    // get application name
    Result := StringReplace(FNewSetup.AppName, '/', '\', [rfReplaceAll]);

    // now add local path information
    Result := ExtractFilePath(FLocalChannelFileName) + Result;
  end;
end;

function TWebUpdater.GetTotalSize: Int64;
begin
  GetFileItemList;
  Result := FTotalSize;
end;

procedure TWebUpdater.PerformWebUpdate;
begin
  FThread := TUpdaterThread.Create(FileItemList);

  // specify event handlers
  FThread.OnProgress := ProgressEventHandler;
  FThread.OnFileNameProgress := FileChangedEventHandler;
  FThread.OnDone := DoneEventHandler;
  FThread.OnScriptErrors := ScriptErrorsEventHandler;
  FThread.OnMD5Mismatch := MD5MismatchHandler;

  // specify paths
  FThread.BasePath := ChannelBasePath;
  FThread.LocalPath := ExtractFilePath(FLocalChannelFileName);

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
      // eventually create SSL IO handler
      if FileExists(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll') and
        FileExists(ExtractFilePath(ParamStr(0)) + 'libeay32.dll') then
        Http.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

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
    Http.IOHandler.Free;
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

procedure TWebUpdater.ResetFileListCache;
begin
  FreeAndNil(FFileItemListCache);
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
      // eventually create SSL IO handler
      if FileExists(ExtractFilePath(ParamStr(0)) + 'ssleay32.dll') and
        FileExists(ExtractFilePath(ParamStr(0)) + 'libeay32.dll') then
        Http.IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

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
    Http.IOHandler.Free;
    Http.Free;
  end;

  // load channels from string
  FChannels.LoadFromString(Text);
end;

end.
