WebUpdate
=========

WebUpdate is a bunch of code that can be included into any application to add some simple JSON based WebUpdate. The contained authoring tool allows to create snapshots with a single click. These snapshots can be copied to a dedicated location or uploaded to an FTP server. This can also be done automatically when taking a snapshot. Once uploaded the WebUpdate only needs a simple HTTP connection in order to perform a web update.

The tool was created after thinking about how a modern, lightweight web update tool could work without the need of an extra server (only file serving is required). It is not yet used and thus mostly untested beyond the used in the tool itself.

In order to use this tool in your project there are a few prerequisites. First the source code is licensed under a dual license of MPL or LGPL, which means you can use this either under the conditions of MPL or under the conditions of LGPL. Both have advantages and disadvantages, however the essence of these libraries is that you mention the use of this library and to allow integration of changes to the original project. If you need to license this under a different license, feel free to contact me.

Beyond the license, there are at least 4 dependencies to other libraries namely:
* [DWS](http://www.delphitools.info/dwscript/)
* [Virtual Treeview](http://www.jam-software.com/virtual-treeview/)
* [JEDI](http://www.delphi-jedi.org/)
* [Indy](http://www.indyproject.org/)

Please make sure you have these libraries accessible from your Delphi environment. So far all modern Delphi XE versions are supported. Older versions might work as well, but probably need some refactoring in order to run out of the box.

Command-line switches (Authoring tool)
--------------------------------------

In case you want to automate the authoring tool, you can use the following command-line switches. In fact the tool will still be a GUI-tool, but with a hidden user interface, so don't expect any output of the tool. Following, all commands and options are listed: 

    Syntax: AuthoringTool.exe project.wup command [more commands] [-options]
    
      project.wup must be replaced by your project name
    
    Commands:
    ---------
    
      s or S or Snapshot           (take snapshot)
      c or C or Copy               (copy to path)
      u or U or Upload             (upload to server)
    
    Options:
    ---------
    
      -Channel="channel name"      (with/without quotes, default: "Nightly")
      -FtpHost=host                (FTP host name, overrides project's default)
      -FtpUser=username            (FTP user name, overrides project's default)
      -FtpPassword=password        (FTP password, overrides project's default)
      -CopyPath=path               (Path of snapshot copies)
    
    Example:
    --------
    
      AuthoringTool.exe project.wup scu -Channel=Beta


Command-line switches (Updater)
-------------------------------

While the 'Updater' tool can be started as stand-alone tool, it is supposed to work as a helper for a main application. It is required because a running application can't replace itself.

    Syntax: Updater.exe [-options]
    
    Options:
    --------
    
      -u=URL                       (Base URL for JSON files)
      -c=Channel                   (Update Channel, default is 'Stable')
      -f=FileName (Channels)       (Filename of channels definition file)
      -d=Delay (Integer)           (Time in milliseconds before updating starts)
      -l=FileName                  (Local filename of current setup)
      -e=ExeFileName               (Name of main application executable)
      -w=WindowCaption             (Caption of main application window)
      -v                           (Verbose)

    Example:
    --------
    
      Updater.exe -c=Nightly
