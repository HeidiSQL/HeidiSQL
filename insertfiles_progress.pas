unit insertfiles_progress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Db;

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
  Size: DWORD;
  value, filename : String;
  y,m,d,h,mi,s,ms : Word;
  FileStream : TFileStream;
begin
  Timer1.Enabled := false;
  screen.Cursor := crHourglass;
  ProgressBar1.Max := frmInsertFiles.ListViewFiles.Items.Count;

  TRY

  with frmInsertFiles do begin
    for i:=0 to ListViewFiles.Items.Count-1 do
    begin
      if self.canceled then break;
      self.Label4.Caption := inttostr(i+1)+' of ' + inttostr(ListViewFiles.Items.Count);
      filename := ListViewFiles.Items[i].Caption;
      self.Label5.Caption := mince(filename, 30) + ' ('+ListViewFiles.Items[i].SubItems[0]+' KB)';
      Application.ProcessMessages;
      with TMDIChild(Mainform.ActiveMDIChild) do
      begin
        ZQuery3.ParamCheck := true;
        ZQuery3.SQL.Clear;
        ZQuery3.SQL.Add( 'INSERT INTO '+mainform.mask(ComboBoxDBs.Text)+'.'+mainform.mask(ComboBoxTables.Text) +
          ' (' + mainform.mask(ComboBoxColumns.Text) );
        self.Label6.caption := 'Inserting data ...';
        Application.ProcessMessages;
        for j:=0 to length(cols)-1 do
        begin
          if cols[j].Name = ComboBoxColumns.Text then
            continue;
          ZQuery3.SQL.Add( ', ' + mainform.mask(cols[j].Name) );
        end;
        ZQuery3.SQL.Add( ') VALUES (:STREAM,' );

        for j:=0 to length(cols)-1 do
        begin
          if cols[j].Name = ComboBoxColumns.Text then
            continue;
          Value := cols[j].Value;
          if pos('%', Value) > 0 then
          begin
            Value := stringreplace(Value, '%filesize%', inttostr(size), [rfReplaceAll]);
            Value := stringreplace(Value, '%filename%', ExtractFileName(filename), [rfReplaceAll]);
            Value := stringreplace(Value, '%filepath%', ExtractFilePath(filename), [rfReplaceAll]);
            DecodeDate(FileDateToDateTime(FileAge(filename)), y, m, d);
            DecodeTime(FileDateToDateTime(FileAge(filename)), h, mi, s, ms);
            Value := stringreplace(Value, '%filedate%', Format('%.4d-%.2d-%.2d', [y,m,d]), [rfReplaceAll]);
            Value := stringreplace(Value, '%filedatetime%', Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,mi,s]), [rfReplaceAll]);
            Value := stringreplace(Value, '%filetime%', Format('%.2d:%.2d:%.2d', [h,mi,s]), [rfReplaceAll]);
          end;
          if cols[j].Quote then
            Value := '"' + escape_string(Value) + '"';
          ZQuery3.SQL.Add( Value );
        end;
        ZQuery3.SQL.Add( ')' );
        try
          self.Label6.caption := 'Reading file ...';
          Application.ProcessMessages;
          FileStream := TFileStream.Create( filename, fmShareDenyWrite );
          ZQuery3.Params.Clear;
          ZQuery3.Params.CreateParam( ftBlob, 'STREAM', ptInput );
          ZQuery3.ParamByName('STREAM').LoadfromStream( FileStream, ftBlob );
        except
          FileStream.Free;
          MessageDlg( 'Error reading file:'#13#10+filename, mtError, [mbOK], 0 );
          break;
        end;
        ZQuery3.ExecSQL;
        self.Label6.caption := 'Freeing memory ...';
        Application.ProcessMessages;
        FileStream.Free;
      end;
      ProgressBar1.StepIt;
      Application.ProcessMessages;
    end;
  end;

  FINALLY
    TMDIChild(Mainform.ActiveMDIChild).ZQuery3.ParamCheck := false;
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
