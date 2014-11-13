program SnapshotTool;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.IOUtils,
  System.SysUtils,
  System.StrUtils,
  WinApi.Windows,
  WebUpdate.Classes.WebUpdate in '..\Common\WebUpdate.Classes.WebUpdate.pas',
  WebUpdate.JSON.Channel in '..\Common\WebUpdate.JSON.Channel.pas',
  WebUpdate.JSON.Channels in '..\Common\WebUpdate.JSON.Channels.pas',
  WebUpdate.JSON.Project in '..\Common\WebUpdate.JSON.Project.pas',
  WebUpdate.JSON.Serializer in '..\Common\WebUpdate.JSON.Serializer.pas',
  WebUpdate.Tools in '..\Common\WebUpdate.Tools.pas';

procedure WriteUsage;
begin
  WriteLn(Format('Syntax: %s project.wup command [more commands] [-options]',
    [ExtractFileName(ParamStr(0))]));
  WriteLn('');
  WriteLn('  project.wup must be replaced by your project name');
  WriteLn('');
  WriteLn('Commands:');
  WriteLn('---------');
  WriteLn('');
  WriteLn('  s or S or Snapshot           (take snapshot)');
  WriteLn('  c or C or Copy               (copy to path)');
  WriteLn('  u or U or Upload             (upload to server)');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('---------');
  WriteLn('');
  WriteLn('  -Channel="channel name"      (with/without quotes, default: "Nightly")');
  WriteLn('  -FtpHost=host                (FTP host name, overrides project''s default)');
  WriteLn('  -FtpUser=username            (FTP user name, overrides project''s default)');
  WriteLn('  -FtpPassword=password        (FTP password, overrides project''s default)');
  WriteLn('  -CopyPath=path               (Path of snapshot copies)');
  WriteLn('');
  WriteLn('Example:');
  WriteLn('--------');
  WriteLn('');
  WriteLn(Format('  %s project.wup scu -Channel=Beta',
    [ExtractFileName(ParamStr(0))]));
  WriteLn('');
end;

procedure UploadSnapshot(Project: TWebUpdateProject);
begin

end;

procedure CopySnapshot(Project: TWebUpdateProject);
var
  Path: string;
  ChannelSetup: TWebUpdateChannelSetup;
  FileItem: TWebUpdateFileItem;
  RealFileName, ChannelFileName, DestFileName: TFileName;
begin
  Path := IncludeTrailingPathDelimiter(Project.Copy.Path);
  if IsRelativePath(Path) then
    Path := Project.BasePath + Path;

  ChannelFileName := Project.ChannelsPath + Project.ChannelName + '.json';

  // upload files
  ChannelSetup := TWebUpdateChannelSetup.Create;
  try
    // load channel setup
    ChannelSetup.LoadFromFile(ChannelFileName);

    for FileItem in ChannelSetup.Items do
    begin
      WriteLn('Copying file ', FileItem.FileName, '...');

      RealFileName := Project.BasePath + WebToLocalFileName(FileItem.FileName);

      // copy file
      DestFileName := ExpandFileName(Path + Project.ChannelName + '\' + FileItem.FileName);
      ForceDirectories(ExtractFileDir(DestFileName));
      TFile.Copy(RealFileName, DestFileName, True);

      // set file date/time according to the JSON file
      FileSetDate(DestFileName, DateTimeToFileDate(FileItem.Modified));
    end;

    // copy channel setup
    WriteLn('Copying channel setup...');
    DestFileName := ExpandFileName(Path + Project.ChannelName + '\' +
      Project.ChannelName + '.json');
    ForceDirectories(ExtractFileDir(DestFileName));
    TFile.Copy(ChannelFileName, DestFileName, True);

    // set file date/time according to the JSON file
    FileSetDate(DestFileName, DateTimeToFileDate(ChannelSetup.Modified));
  finally
    ChannelSetup.Free;
  end;

  // copy channel file
  WriteLn('Copying channels list...');
  DestFileName := ExpandFileName(Path + ExtractFileName(Project.ChannelsFilename));
  TFile.Copy(Project.FullChannelsFilename, DestFileName, True);
end;

procedure TakeSnapshot(Project: TWebUpdateProject);
var
  Item: TWebUpdateFileItem;
  ChannelItem: TWebUpdateChannelItem;
  Fad: TWin32FileAttributeData;
  LastModified: TDateTime;
  FileName: TFileName;
  Channels: TWebUpdateChannels;
  ChannelSetup: TWebUpdateChannelSetup;
begin
  // update status
  WriteLn('Taking snapshot...');

  Channels := TWebUpdateChannels.Create;
  try
    // load
    Channels.LoadFromFile(Project.ChannelsPath + Project.ChannelsFilename);
    ChannelItem := Channels.GetItemForChannel(Project.ChannelName);

    // create selected channels
    ChannelSetup := TWebUpdateChannelSetup.Create;
    try
      // get filename for currently selected channel
      FileName := Project.ChannelsPath + Project.ChannelName + '\' +
        Project.ChannelName + '.json';

      ChannelSetup.LoadFromFile(FileName);

      // store current channel name
      ChannelSetup.AppName := Project.ApplicationName;
      ChannelSetup.ChannelName := Project.ChannelName;
      LastModified := 0;
      for Item in ChannelSetup.Items do
      begin
        // get file attribute
        if not GetFileAttributesEx(PChar(Item.FileName), GetFileExInfoStandard, @Fad) then
          RaiseLastOSError;

        // create (& update) file item
        Item.Modified := FileTimeToDateTime(Fad.ftLastWriteTime);
        Item.FileSize := Fad.nFileSizeLow;
        if Project.UseMD5 then
          Item.MD5Hash := MD5(Item.FileName)
        else
          Item.MD5Hash := '';

        if Item.Modified > LastModified then
          LastModified := Item.Modified;

        // add item to file items list
        ChannelSetup.Items.Add(Item);
      end;

      ChannelSetup.Modified := LastModified;
      ChannelItem.Modified := LastModified;

      // save channel setup
      ChannelSetup.SaveToFile(FileName);
    finally
      ChannelSetup.Free;
    end;

    // save channels to file
    Channels.SaveToFile(Project.ChannelsPath + Project.ChannelsFilename);
  finally
    Channels.Free;
  end;
end;

type
  TWebUpdateCommand = (wuSnapshot, wuUpload, wuCopy);
  TWebUpdateCommands = set of TWebUpdateCommand;
var
  Project: TWebUpdateProject;
  ParamIndex, CharIndex: Integer;
  Commands: TWebUpdateCommands;
  Text: string;
  EqPos: Integer;
begin
  try
    // check for parameters
    if ParamCount = 0 then
    begin
      WriteUsage;
      Halt(100);
    end;

    // check if project file exists
    if not FileExists(ParamStr(1)) then
    begin
      WriteLn('File %s does not exist!', ParamStr(1));
      Halt(101);
    end;

    Project := TWebUpdateProject.Create;
    try
      // now load project
      Project.LoadFromFile(ParamStr(1));

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
            Project.ChannelName := Copy(Text, EqPos, Length(Text) - EqPos)
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
            WriteLn('Unknown option: %s!', Text);
            Halt(102);
          end;
        end
        else
        begin
          if SameText(Text, 'Snapshot') then
            Commands := Commands + [wuSnapshot]
          else if SameText(Text, 'Copy') then
            Commands := Commands + [wuCopy]
          else if SameText(Text, 'Upload') then
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
                    WriteLn('Unknown command %s', Text[CharIndex]);
                    Halt(103);
                  end;
              end;
        end;
      end;

      if Project.AutoCopyUpload then
      begin
        if Project.Copy.Enabled then
          Commands := Commands + [wuCopy];

        if Project.FTP.Server <> '' then
          Commands := Commands + [wuUpload];
      end;

      if wuSnapshot in Commands then
        TakeSnapshot(Project);

      // eventually copy to path
      if wuCopy in Commands then
        CopySnapshot(Project);

      // eventually upload to a server
      if wuUpload in Commands then
        UploadSnapshot(Project);
    finally
      Project.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
