WebUpdate
=========

WebUpdate is a bunch of code that can be included into any application to add some simple JSON based WebUpdate. The contained authoring tool allows to create snapshots with a single click. These snapshots can be copied to a dedicated location or uploaded to an FTP server. This can also be done automatically when taking a snapshot. Once uploaded the WebUpdate only needs a simple HTTP connection in order to perform a web update.

The tool was created after thinking about how a modern, lightweight web update tool could work without the need of an extra server (only file serving is required). It is not yet used and thus mostly untested beyond the used in the tool itself.

In order to use this tool in your project there are a few prerequisites. First the source code is licensed under a dual license of MPL or LGPL, which means you can use this either under the conditions of MPL or under the conditions of LGPL. Both have advantages and disadvantages, however the essence of these libraries is that you mention the use of this library and to allow integration of changes to the original project. If you need to license this under a different license, feel free to contact me.

Beyond the license, there are at least 4 dependencies to other libraries namely:
* DWS
* Virtual Treeview
* JEDI
* Indy

Please make sure you have these libraries accessible from your Delphi environment. So far all modern Delphi XE versions are supported. Older versions might work as well, but probably need some refactoring in order to run out of the box.