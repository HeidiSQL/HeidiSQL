unit insertfiles_progress;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, mysql_connection;

type
  TfrmInsertFilesProgress = class(TForm)
    pbReadingFiles: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnCancel: TButton;
    lblNumber: TLabel;
    lblFilename: TLabel;
    lblOperation: TLabel;
    timerStartReading: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure ProcessFiles(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Canceled : Boolean;
  end;


implementation

uses main, helpers, insertfiles;

{$I const.inc}
{$R *.DFM}

procedure TfrmInsertFilesProgress.btnCancelClick(Sender: TObject);
begin
  Canceled := true;
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
  Caller: TfrmInsertFiles;
begin
  timerStartReading.Enabled := false;
  Screen.Cursor := crHourglass;
  Caller := Owner as TfrmInsertFiles;
  pbReadingFiles.Max := Caller.ListViewFiles.Items.Count;

  try
    for i:=0 to Caller.ListViewFiles.Items.Count-1 do begin
      if Canceled then
        break;
      lblNumber.Caption := inttostr(i+1)+' of ' + inttostr(Caller.ListViewFiles.Items.Count);
      lblNumber.Repaint;
      filename := Caller.ListViewFiles.Items[i].Caption;
      lblFilename.Caption := mince(filename, 30) + ' ('+FormatNumber(Caller.ListViewFiles.Items[i].SubItems[0])+' KB)';
      lblFilename.Repaint;
      sql := 'INSERT INTO '+mainform.mask(Caller.ComboBoxDBs.Text)+'.'+mainform.mask(Caller.ComboBoxTables.Text) +
        ' (' + mainform.mask(Caller.ComboBoxColumns.Text);
      lblOperation.caption := 'Inserting data ...';
      lblOperation.Repaint;
      for j:=0 to length(Caller.cols)-1 do begin
        if Caller.cols[j].Name = Caller.ComboBoxColumns.Text then
          Continue;
        sql := sql + ', ' + mainform.mask(Caller.cols[j].Name);
      end;
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
      sql := sql + ') VALUES ('+data+', ';

      for j:=0 to Length(Caller.cols)-1 do begin
        if Caller.cols[j].Name = Caller.ComboBoxColumns.Text then
          Continue;
        Value := Caller.cols[j].Value;
        if pos('%', Value) > 0 then begin
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
        if Caller.cols[j].Quote then
          Value := esc(Value);
        sql := sql + Value + ', ';
      end;
      // Strip last comma + space
      sql := copy(sql, 1, length(sql)-2);
      sql := sql + ')';
      Mainform.Connection.Query(sql);
      lblOperation.Repaint;
      pbReadingFiles.StepIt;
      pbReadingFiles.Repaint;
    end;
    Screen.Cursor := crDefault;
    Close;
  except
    on E:Exception do begin
      Screen.Cursor := crDefault;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
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
  pbReadingFiles.Position := 0;
  lblNumber.Caption := '';
  lblFilename.Caption := '';
  lblOperation.Caption := '';
  Canceled := false;
  timerStartReading.Enabled := true;
end;


end.
