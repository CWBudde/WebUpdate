unit WebUpdate.Options.GUI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Mask,
  VirtualTrees, JvExMask, JvToolEdit;

type
  TOptionsItem = record
    TabSheet: TTabSheet;
  end;
  POptionsItem = ^TOptionsItem;

  TFormOptions = class(TForm)
    ButtonCancel: TButton;
    ButtonOK: TButton;
    LabelFileName: TLabel;
    PageControl: TPageControl;
    Panel: TPanel;
    TabSheetMain: TTabSheet;
    TabSheetFTP: TTabSheet;
    TreeOptions: TVirtualStringTree;
    LabelChannelFileName: TLabel;
    EditChannelFileName: TJvFilenameEdit;
    EditBaseDirectory: TJvDirectoryEdit;
    CheckBoxAutoCopyUpload: TCheckBox;
    GroupBoxFTP: TGroupBox;
    EditFtpPassword: TEdit;
    LabelPassword: TLabel;
    EditFtpUsername: TEdit;
    LabelUsername: TLabel;
    EditFtpServer: TEdit;
    LabelServer: TLabel;
    GroupBoxCopy: TGroupBox;
    CheckBoxCopyTo: TCheckBox;
    EditCopyPath: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure TreeOptionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeOptionsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure EditChannelFileNameAfterDialog(Sender: TObject; var AName: string;
      var AAction: Boolean);
  private
    procedure SetupTree;
  end;

implementation

{$R *.dfm}

{ TFormOptions }

procedure TFormOptions.EditChannelFileNameAfterDialog(Sender: TObject;
  var AName: string; var AAction: Boolean);
begin
  // try to make the path relative
  AName := ExtractRelativePath(IncludeTrailingPathDelimiter(
    EditBaseDirectory.Directory), AName);
end;

procedure TFormOptions.FormCreate(Sender: TObject);
begin
  TreeOptions.NodeDataSize := SizeOf(TOptionsItem);
  SetupTree;
  PageControl.ActivePageIndex := 0;
end;

procedure TFormOptions.TreeOptionsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  NodeData: POptionsItem;
begin
  if Assigned(Node) then
  begin
    NodeData := TreeOptions.GetNodeData(Node);
    PageControl.ActivePage := NodeData^.TabSheet;
  end;
end;

procedure TFormOptions.TreeOptionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  NodeData: POptionsItem;
begin
  NodeData := TreeOptions.GetNodeData(Node);
  CellText := NodeData^.TabSheet.Caption;
end;

procedure TFormOptions.SetupTree;
var
  Node: PVirtualNode;
  NodeData: POptionsItem;
  Index: Integer;
begin
  // build tree
  for Index := 0 to PageControl.PageCount - 1 do
  begin
    Node := TreeOptions.AddChild(TreeOptions.RootNode);
    NodeData := TreeOptions.GetNodeData(Node);
    NodeData^.TabSheet := PageControl.Pages[Index];
  end;
end;

end.
