unit main;

interface

uses
{$IFNDEF VER130BELOW}
  Types,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ZAbstractRODataset, ZAbstractDataset, ZDataset, ZConnection,
  StdCtrls, ExtCtrls, Buttons, ComCtrls, Grids, DBGrids, Mask, DBCtrls,
  ExtDlgs, ZSqlMetadata, ZSqlMonitor, ZDbcLogging;

type
  TMainForm = class(TForm)
    ZConnection: TZConnection;
    ZPeople: TZQuery;
    Panel1: TPanel;
    Label1: TLabel;
    ZProtocol: TComboBox;
    Label2: TLabel;
    ZPassword: TEdit;
    Label3: TLabel;
    ZDatabase: TEdit;
    Label4: TLabel;
    SpeedButton1: TSpeedButton;
    PageControl1: TPageControl;
    SpeedButton2: TSpeedButton;
    TabSheet1: TTabSheet;
    DBGrid: TDBGrid;
    DSPeople: TDataSource;
    DBNavigator1: TDBNavigator;
    DBId: TDBEdit;
    ZPeoplep_id: TSmallintField;
    ZPeoplep_name: TStringField;
    ZPeoplep_begin_work: TTimeField;
    ZPeoplep_end_work: TTimeField;
    ZPeoplep_picture: TBlobField;
    ZPeoplep_resume: TMemoField;
    ZPeoplep_redundant: TSmallintField;
    DBName: TDBEdit;
    DBBeginWorkTime: TDBEdit;
    DBResume: TDBMemo;
    DBPicture: TDBImage;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    DBEndWorkTime: TDBEdit;
    Label9: TLabel;
    Label10: TLabel;
    LoadImageBtn: TButton;
    LoadResumeBtn: TButton;
    ZDepartment: TZReadOnlyQuery;
    DSDepartment: TDataSource;
    DBDepartment: TDBLookupComboBox;
    Label11: TLabel;
    ZPeoplep_dep_id: TSmallintField;
    ZHost: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    ZUername: TEdit;
    ZPeopledeprtment: TStringField;
    TabSheet2: TTabSheet;
    ZCargo: TZQuery;
    DSCargo: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator2: TDBNavigator;
    Label14: TLabel;
    ZPort: TEdit;
    SpeedButton3: TSpeedButton;
    TabSheet3: TTabSheet;
    ZSQLMetadata: TZSQLMetadata;
    DSSQLMetadata: TDataSource;
    Panel2: TPanel;
    DBGrid2: TDBGrid;
    ZDBInfoType: TComboBox;
    ZSQLMonitor: TZSQLMonitor;
    TabSheet4: TTabSheet;
    ZLogList: TMemo;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure LoadResumeBtnClick(Sender: TObject);
    procedure LoadImageBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure ZDBInfoTypeChange(Sender: TObject);
    procedure ZSQLMonitorTrace(Sender: TObject; Event: TZLoggingEvent;
      var LogTrace: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses ZClasses, ZDbcIntfs, ZDbcDBLib, ZCompatibility;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
var
 ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(self);
  try
    ODialog.Filter := 'Firebird Database|*.fdb|Interbase Database|*.gdb';
    if ODialog.Execute then
    begin
      ZDatabase.Text := ODialog.FileName;
      if ZConnection.Connected then
        ZConnection.Disconnect;
    end;
  finally
    ODialog.Free;
  end;
end;

procedure TMainForm.SpeedButton2Click(Sender: TObject);
begin
  with ZConnection do
  begin
    User := ZUername.Text;
    Password := ZPassword.Text;
    Protocol := ZProtocol.Text;
    HostName := ZHost.Text;
    if ZPort.Text <> '(default)' then
      Port := StrToInt(ZPort.Text);
    Database := ZDatabase.Text;

    Connect;
    ZCargo.Active := True;
    ZDepartment.Active := True;
    ZPeople.Active := True;
  end;
end;

procedure TMainForm.LoadResumeBtnClick(Sender: TObject);
var
 ODialog: TOpenDialog;
begin
  ODialog := TOpenDialog.Create(self);
  try
    ODialog.Filter := 'Text documents|*.txt';
    if ODialog.Execute then
      DBResume.Lines.LoadFromFile(ODialog.FileName);
  finally
    ODialog.Free;
  end;
end;

procedure TMainForm.LoadImageBtnClick(Sender: TObject);
var
 ODialog: TOpenPictureDialog;
begin
  ODialog := TOpenPictureDialog.Create(self);
  try
    ODialog.Filter := 'Images|*.bmp';
    if ODialog.Execute then
      DBPicture.Picture.LoadFromFile(ODialog.FileName);
  finally
    ODialog.Free;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  ZProtocol.Clear;
  Drivers := DriverManager.GetDrivers;
    for I := 0 to Drivers.Count - 1 do
  begin
    Protocols := (Drivers.Items[I] as IZDriver).GetSupportedProtocols;
    for J := 0 to High(Protocols) do
      ZProtocol.Items.Add(Protocols[J]);
  end;
  ZProtocol.Sorted := True;
end;

procedure TMainForm.SpeedButton3Click(Sender: TObject);
begin
  ZConnection.Disconnect;
  ZLogList.Lines.Clear;
end;

procedure TMainForm.ZDBInfoTypeChange(Sender: TObject);
var
  I: integer;
begin
  ZSQLMetadata.Close;
  if 'Procedures' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdProcedures;
  if 'ProcedureColumns' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdProcedureColumns;
  if 'Tables' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdTables;
  if 'Schemas' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdSchemas;
  if 'Catalogs' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdCatalogs;
  if 'TableTypes' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdTableTypes;
  if 'Columns' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdColumns;
  if 'ColumnPrivileges' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdColumnPrivileges;
  if 'TablePrivileges' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdTablePrivileges;
  if 'BestRowIdentifier' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdBestRowIdentifier;
  if 'VersionColumns' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdVersionColumns;
  if 'PrimaryKeys' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdPrimaryKeys;
  if 'ImportedKeys' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdImportedKeys;
  if 'ExportedKeys' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdExportedKeys;
  if 'CrossReference' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdCrossReference;
  if 'TypeInfo' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdTypeInfo;
  if 'IndexInfo' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdIndexInfo;
  if 'UserDefinedTypes' = ZDBInfoType.Text then
    ZSQLMetadata.MetadataType := mdUserDefinedTypes;
  if ZConnection.Connected then
    ZSQLMetadata.Open;

  for I := 0 to DBGrid2.Columns.Count - 1 do
    if DBGrid2.Columns[I].Width > 150 then
      DBGrid2.Columns[I].Width := 150;
end;

procedure TMainForm.ZSQLMonitorTrace(Sender: TObject;
  Event: TZLoggingEvent; var LogTrace: Boolean);
begin
  ZLogList.Lines.Add(Event.AsString);
  ZLogList.Lines.Add('');
end;

end.
