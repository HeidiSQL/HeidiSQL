unit SynDBExplorerExportTables;

interface

uses
  Windows, Messages, SysUtils, CheckLst, Controls, StdCtrls, Classes, Graphics,
  Forms, ExtCtrls, SQLite3UILogin,
  SynZip, SynCommons, SynDB, SynDBSQLite3;

type
  TDBExportTablesForm = class(TForm)
    BtnExport: TButton;
    BtnCancel: TButton;
    GroupWhere: TGroupBox;
    ChkUseStandardCollations: TCheckBox;
    ChkNoBlobExport: TCheckBox;
    ChkZipDBFile: TCheckBox;
  protected
    fProps: TSQLDBConnectionProperties;
    fEdits: array of TLabeledEdit;
    procedure SetTableNames(const Value: TStrings);
  public
    class function ExportTables(aTableNames: TStrings; aProps: TSQLDBConnectionProperties; const aDestFileName: TFileName): integer; overload;
    class function ExportTables(aListBox: TListBox; aProps: TSQLDBConnectionProperties; const aDestFileName: TFileName): integer; overload;
    property Props: TSQLDBConnectionProperties read fProps write fProps;
  end;

var
  DBExportTablesForm: TDBExportTablesForm;

implementation

{$R *.dfm}

resourcestring
  sTableExportWhereHint = 'e.g. "ID>1000" or "RowNum<=500"';

  
{ TDBExportTablesForm }

class function TDBExportTablesForm.ExportTables(aTableNames: TStrings;
  aProps: TSQLDBConnectionProperties; const aDestFileName: TFileName): integer;
var DB: TSQLDBSQLite3ConnectionProperties;
    Conn: TSQLDBSQLite3Connection;
    Rows: TSQLDBStatement;
    Table,SQL: RawUTF8;
    Fields: TSQLDBColumnDefineDynArray;
    ExcludeTypes: TSQLDBFieldTypes;
    ZipFileName: TFileName;
    Tmp: TForm;
    TmpPanel: TPanel;
    i: integer;
begin
  result := 0;
  if aDestFileName<>'' then
  with TDBExportTablesForm.Create(Application) do
  try
    SetTableNames(aTableNames);
    ActiveControl := BtnExport;
    if ShowModal<>mrOk then
      exit;
    if ChkNoBlobExport.Checked then
      ExcludeTypes := [ftBlob] else
      ExcludeTypes := [];
    DeleteFile(aDestFileName);
    Tmp := CreateTempForm(BtnExport.Caption,@TmpPanel,true);
    try
      DB := TSQLDBSQLite3ConnectionProperties.Create(StringToUTF8(aDestFileName),'','','');
      try
        DB.UseMormotCollations := not ChkUseStandardCollations.Checked;
        Conn := DB.MainConnection as TSQLDBSQLite3Connection;
        Conn.Connect;
        try
          Conn.DB.ExecuteAll('PRAGMA journal_mode=MEMORY;PRAGMA journal_size_limit=16777216;'+
            'PRAGMA synchronous=OFF;');
          // Conn.DB.SetWALMode(true); // slower in WAL mode for huge data :(
          assert(aTableNames.Count=length(fEdits));
          for i := 0 to aTableNames.Count-1 do begin
            TmpPanel.Caption := aTableNames[i];
            Application.ProcessMessages;
            Table := StringToUTF8(aTableNames[i]);
            aProps.GetFields(Table,Fields);
            SQL := Trim(StringToUTF8(fEdits[i].Text));
            if SQL<>'' then
              SQL := ' where '+SQL;
            SQL := aProps.SQLSelectAll(Table,Fields,ExcludeTypes)+SQL;
            Rows := aProps.NewThreadSafeStatement;
            try
              Rows.Execute(SQL,true);
              inc(result,RowsToSQLite3(Conn,Table,Rows,true));
            finally
              Rows.Free;
            end;
          end;
        except
          on E: Exception do
            ShowMessage(E.Message,true);
        end;
      finally
        DB.Free;
      end;
      if ChkZipDBFile.Checked then begin
        ZipFileName := ChangeFileExt(aDestFileName,'.zip');
        TmpPanel.Caption := ExtractFileName(ZipFileName);
        Application.ProcessMessages;
        with TZipWrite.Create(ZipFileName) do
        try
          AddDeflated(aDestFileName,true);
        finally
          Free;
        end;
        DeleteFile(aDestFileName);
      end;
    finally
      Screen.Cursor := crDefault;
      Tmp.Free;
    end;
  finally
    Free;
  end;
end;

class function TDBExportTablesForm.ExportTables(aListBox: TListBox;
  aProps: TSQLDBConnectionProperties; const aDestFileName: TFileName): integer;
var T: TStringList;
    i: integer;
begin
  result := 0;
  if (aListBox=nil) or (aListBox.SelCount=0) then
    exit;
  T := TStringList.Create;
  try
    for i := 0 to aListBox.Count-1 do
      if aListBox.Selected[i] then
        T.Add(aListBox.Items[i]);
    result := ExportTables(T,aProps,aDestFileName);
  finally
    T.Free;
  end;
end;

procedure TDBExportTablesForm.SetTableNames(const Value: TStrings);
var E: TLabeledEdit;
    max,x,y,n,i,h: integer;
begin
  if Value=nil then
    n := 0 else
    n := Value.Count;
  SetLength(fEdits,n);
  max := Screen.Height;
  if n*32+132>max then
    h := 24 else
    h := 32;
  x := 160;
  n := n*h+24;
  if n+148>max then
    n := max-164;
  GroupWhere.Height := n;
  ClientHeight := n+132;
  y := 24;
  for i := 0 to high(fEdits) do begin
    E := TLabeledEdit.Create(self);
    E.Parent := GroupWhere;
    E.LabelPosition := lpLeft;
    E.SetBounds(x,y,180,22);
    E.EditLabel.Caption := Value[i];
    E.ShowHint := true;
    E.Hint := sTableExportWhereHint;
    inc(y,h);
    if (h=24) and (y>=n-24) and (i<>high(fEdits)) then
    if x<>160 then break else begin
      y := 24;
      inc(x,340);
      Width := 740;
    end;
  end;
end;

end.
