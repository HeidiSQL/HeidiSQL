unit insertfiles_progress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Db, ZDataset;

type
  TfrmInsertFilesProgress = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    lblNumber: TLabel;
    lblFilename: TLabel;
    lblOperation: TLabel;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ProcessFiles(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FInsertFilesForm : Pointer;
    canceled : Boolean;
  public
    property InsertFilesForm : Pointer read FInsertFilesForm write FInsertFilesForm;
  end;


implementation

uses main, childwin, helpers,insertfiles,
  HeidiComp;

{$I const.inc}
{$R *.DFM}

procedure TfrmInsertFilesProgress.Button1Click(Sender: TObject);
begin
  canceled := true;
end;

procedure TfrmInsertFilesProgress.ProcessFiles(Sender: TObject);

  procedure die(msg : String);
  begin
    Screen.Cursor := crDefault;
    raise exception.Create(msg);
  end;

var
  i, j: Integer;
  value, filename: String;
  dt: TDateTime;
  y, m, d, h, mi, s, ms: Word;
  FileStream: TFileStream;
  zq: TDeferDataSet;
begin
  Timer1.Enabled := false;
  screen.Cursor := crHourglass;
  ProgressBar1.Max := TfrmInsertFiles(FInsertFilesForm).ListViewFiles.Items.Count;
  zq := TDeferDataSet.Create(nil, MainForm.ChildWin.RunAsyncPost);
  zq.Connection := MainForm.ChildWin.Conn.MysqlConn;

  TRY

  with TfrmInsertFiles(FInsertFilesForm) do
    begin
    for i:=0 to ListViewFiles.Items.Count-1 do
    begin
      if self.canceled then break;
      lblNumber.Caption := inttostr(i+1)+' of ' + inttostr(ListViewFiles.Items.Count);
      lblNumber.Repaint;
      filename := ListViewFiles.Items[i].Caption;
      lblFilename.Caption := mince(filename, 30) + ' ('+FormatNumber(ListViewFiles.Items[i].SubItems[0])+' KB)';
      lblFilename.Repaint;
      zq.ParamCheck := true;
      zq.SQL.Clear;
      zq.SQL.Add( 'INSERT INTO '+mainform.mask(ComboBoxDBs.Text)+'.'+mainform.mask(ComboBoxTables.Text) +
        ' (' + mainform.mask(ComboBoxColumns.Text) );
      lblOperation.caption := 'Inserting data ...';
      lblOperation.Repaint;
      for j:=0 to length(cols)-1 do
      begin
        if cols[j].Name = ComboBoxColumns.Text then
          continue;
        zq.SQL.Add( ', ' + mainform.mask(cols[j].Name) );
      end;
      zq.SQL.Add( ') VALUES (:STREAM, ' );

      for j:=0 to length(cols)-1 do
      begin
        if cols[j].Name = ComboBoxColumns.Text then
          continue;
        Value := cols[j].Value;
        if pos('%', Value) > 0 then
        begin
          //Value := stringreplace(Value, '%filesize%', inttostr(size), [rfReplaceAll]);
          Value := stringreplace(Value, '%filename%', ExtractFileName(filename), [rfReplaceAll]);
          Value := stringreplace(Value, '%filepath%', ExtractFilePath(filename), [rfReplaceAll]);
          FileAge(filename, dt);
          DecodeDate(dt, y, m, d);
          DecodeTime(dt, h, mi, s, ms);
          Value := stringreplace(Value, '%filedate%', Format('%.4d-%.2d-%.2d', [y,m,d]), [rfReplaceAll]);
          Value := stringreplace(Value, '%filedatetime%', Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,mi,s]), [rfReplaceAll]);
          Value := stringreplace(Value, '%filetime%', Format('%.2d:%.2d:%.2d', [h,mi,s]), [rfReplaceAll]);
        end;
        if cols[j].Quote then
          Value := esc(Value);
        zq.SQL.Add( Value + ', ' );
      end;
      // Strip last komma + space + CR + LF
      zq.SQL.Text := copy( zq.SQL.Text, 1, length(zq.SQL.Text)-4 );
      zq.SQL.Add( ')' );
      try
        lblOperation.caption := 'Reading file ...';
        lblOperation.Repaint;
        FileStream := TFileStream.Create( filename, fmShareDenyWrite );
        try
          zq.Params.Clear;
          zq.Params.CreateParam( ftBlob, 'STREAM', ptInput );
          zq.ParamByName('STREAM').LoadfromStream( FileStream, ftBlob );
        finally
          FileStream.Free;
        end;
      except
        MessageDlg( 'Error reading file:' + CRLF + filename, mtError, [mbOK], 0 );
        break;
      end;
      zq.ExecSql;
      lblOperation.caption := 'Freeing memory ...';
      lblOperation.Repaint;
      ProgressBar1.StepIt;
      ProgressBar1.Repaint;
    end;
  end;

  FINALLY
    FreeAndNil(zq);
    screen.Cursor := crDefault;
    Close();
  END;
end;

procedure TfrmInsertFilesProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmInsertFilesProgress.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
end;

procedure TfrmInsertFilesProgress.FormShow(Sender: TObject);
begin
  ProgressBar1.Position := 0;
  lblNumber.Caption := '';
  lblFilename.Caption := '';
  lblOperation.Caption := '';
  Canceled := false;
  Timer1.Enabled := true;
end;


end.
