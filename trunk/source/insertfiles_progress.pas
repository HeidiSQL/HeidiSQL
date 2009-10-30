unit insertfiles_progress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, mysql_connection;

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

uses main, helpers,insertfiles;

{$I const.inc}
{$R *.DFM}

procedure TfrmInsertFilesProgress.Button1Click(Sender: TObject);
begin
  canceled := true;
end;

procedure TfrmInsertFilesProgress.ProcessFiles(Sender: TObject);
const
  ChunkSize = 131072;

var
  i, j: Integer;
  value, filename: String;
  dt: TDateTime;
  y, m, d, h, mi, s, ms: Word;
  FileStream: TFileStream;
  readBuf: String;
  bytesRead: Integer;
  sql, data: WideString;
begin
  Timer1.Enabled := false;
  screen.Cursor := crHourglass;
  ProgressBar1.Max := TfrmInsertFiles(FInsertFilesForm).ListViewFiles.Items.Count;

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
      sql := 'INSERT INTO '+mainform.mask(ComboBoxDBs.Text)+'.'+mainform.mask(ComboBoxTables.Text) +
        ' (' + mainform.mask(ComboBoxColumns.Text);
      lblOperation.caption := 'Inserting data ...';
      lblOperation.Repaint;
      for j:=0 to length(cols)-1 do
      begin
        if cols[j].Name = ComboBoxColumns.Text then
          continue;
        sql := sql + ', ' + mainform.mask(cols[j].Name);
      end;
      try
        lblOperation.caption := 'Reading file ...';
        lblOperation.Repaint;
        FileStream := TFileStream.Create( filename, fmShareDenyWrite );
        try
          data := '_binary 0x';
          while FileStream.Position < FileStream.Size do begin
            // Read characters from file.
            // Data is imported as-is (byte for byte).
            // How the server interprets the data is decided by the
            // character set on the column that is imported into.
            // Set the character set on the column before importing the file.
            //
            // TODO: Indicate this character set on the GUI.
            //
            SetLength(readBuf, ChunkSize div SizeOf(Char));
            bytesRead := FileStream.Read(PChar(readBuf)^, ChunkSize);
            SetLength(readBuf, bytesRead div SizeOf(Char));
            data := data + BinToWideHex(readBuf);
          end;
        finally
          FileStream.Free;
        end;
      except
        MessageDlg( 'Error reading file:' + CRLF + filename, mtError, [mbOK], 0 );
        break;
      end;
      sql := sql + ') VALUES ('+data+', ';

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
        sql := sql + Value + ', ';
      end;
      // Strip last comma + space
      sql := copy(sql, 1, length(sql)-2);
      sql := sql + ')';
      Mainform.Connection.Query(sql);
      lblOperation.caption := 'Freeing memory ...';
      lblOperation.Repaint;
      ProgressBar1.StepIt;
      ProgressBar1.Repaint;
    end;
  end;

  FINALLY
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
