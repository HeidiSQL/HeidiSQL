unit insertfiles_progress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, mysql;

type
  TfrmInsertFilesProgress = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ProcessFiles(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    canceled : Boolean;
  public
    { Public declarations }
  end;

var
  frmInsertFilesProgress: TfrmInsertFilesProgress;


implementation

uses insertfiles, main, childwin, helpers;


{$R *.DFM}

procedure TfrmInsertFilesProgress.Button1Click(Sender: TObject);
begin
  canceled := true;
  close;
end;

procedure TfrmInsertFilesProgress.ProcessFiles(Sender: TObject);

procedure die(msg : String);
begin
  Screen.Cursor := crDefault;
  raise exception.Create(msg);
end;

var
  i,j: Integer;
  Handl: THandle;
  Size: DWORD;
  Bytes: DWORD;
  Raw: pChar;
  Escaped: pChar;
  Query: pChar;
  SQL, value, filename : String;
  y,m,d,h,mi,s,ms : Word;
begin
  Timer1.Enabled := false;
  screen.Cursor := crHourglass;
  ProgressBar1.Max := frmInsertFiles.ListViewFiles.Items.Count;

  TRY

  with frmInsertFiles do begin
    for i:=0 to ListViewFiles.Items.Count-1 do begin
      if self.canceled then break;
      self.Label4.Caption := inttostr(i+1)+' of ' + inttostr(ListViewFiles.Items.Count);
      filename := ListViewFiles.Items[i].Caption;
      self.Label5.Caption := mince(filename, 30) + ' ('+ListViewFiles.Items[i].SubItems[0]+' KB)';
      Application.ProcessMessages;
      Handl := CreateFile(Pchar(filename), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
      if Handl = INVALID_HANDLE_VALUE then
        die('Error reading file:'#13#10+filename);
      self.Label6.caption := 'Reading file data ...';
      Application.ProcessMessages;
      Size := GetFileSize(Handl, nil);
      GetMem(Raw, Size);
      if not ReadFile(Handl, Raw^, Size, Bytes, nil) or (Bytes <> Size) then
        die('Error reading file:'#13#10+filename);
      CloseHandle(Handl);
      Handl := INVALID_HANDLE_VALUE;
      with TMDIChild(Mainform.ActiveMDIChild) do begin
        Label6.caption := 'Escaping file data ...';
        Application.ProcessMessages;
        GetMem(Escaped, Size * 2 + 1);
        mysql_real_escape_string(mysql, Escaped, Raw, Size);
        self.Label6.caption := 'Inserting data ...';
        Application.ProcessMessages;
        SQL := 'INSERT INTO '+mainform.mask(ComboBoxDBs.Text)+'.'+mainform.mask(ComboBoxTables.Text) + ' (';
        for j:=0 to length(cols)-1 do begin
          if cols[j].Name = ComboBoxColumns.Text then continue;
          SQL := SQL + mainform.mask(cols[j].Name) + ', ';
        end;
        SQL := SQL + mainform.mask(ComboBoxColumns.Text)+') VALUES (';
        for j:=0 to length(cols)-1 do begin
          if cols[j].Name = ComboBoxColumns.Text then continue;
          Value := cols[j].Value;
          if pos('%', Value) > 0 then begin
            Value := stringreplace(Value, '%filesize%', inttostr(size), [rfReplaceAll]);
            Value := stringreplace(Value, '%filename%', ExtractFileName(filename), [rfReplaceAll]);
            Value := stringreplace(Value, '%filepath%', ExtractFilePath(filename), [rfReplaceAll]);
            DecodeDate(FileDateToDateTime(FileAge(filename)), y, m, d);
            DecodeTime(FileDateToDateTime(FileAge(filename)), h, mi, s, ms);
            Value := stringreplace(Value, '%filedate%', Format('%.4d-%.2d-%.2d', [y,m,d]), [rfReplaceAll]);
            Value := stringreplace(Value, '%filedatetime%', Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,mi,s]), [rfReplaceAll]);
            Value := stringreplace(Value, '%filetime%', Format('%.2d:%.2d:%.2d', [h,mi,s]), [rfReplaceAll]);
          end;
          if cols[j].Quote then Value := '"' + escape_string(Value) + '"';
          SQL := SQL + Value + ', ';
        end;
        SQL := SQL + '"';
        GetMem(Query, Size * 2 + 1 + length(SQL)+2);
        StrCopy(StrECopy(StrECopy(Query, pchar(SQL)), Escaped), '")');
        ExecQuery(Query);
//        if mysql_query(Mysql, Query) <> 0 then
//          die('Error while executing query'+query);
        self.Label6.caption := 'Freeing memory ...';
        Application.ProcessMessages;
        freemem(Raw);
        freemem(Escaped);
        freemem(Query);
      end;
      ProgressBar1.StepIt;
      Application.ProcessMessages;
    end;
  end;

  FINALLY
    screen.Cursor := crDefault;
    close;
  END;
end;

procedure TfrmInsertFilesProgress.FormShow(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  Label4.Caption := '';
  Label5.Caption := '';
  Label6.Caption := '';
  Canceled := false;
  Timer1.Enabled := true;
end;


end.
