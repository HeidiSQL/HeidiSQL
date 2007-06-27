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
    lblNumber: TLabel;
    lblFilename: TLabel;
    lblOperation: TLabel;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ProcessFiles(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FInsertFilesForm : Pointer;
    canceled : Boolean;
  public
    property InsertFilesForm : Pointer read FInsertFilesForm write FInsertFilesForm;
  end;


implementation

uses main, childwin, helpers,insertfiles;


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
  value, filename : String;
  y,m,d,h,mi,s,ms : Word;
  FileStream : TFileStream;
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
      with TMDIChild(Mainform.ActiveMDIChild) do
      begin
        ZQuery3.ParamCheck := true;
        ZQuery3.SQL.Clear;
        ZQuery3.SQL.Add( 'INSERT INTO '+mainform.mask(ComboBoxDBs.Text)+'.'+mainform.mask(ComboBoxTables.Text) +
          ' (' + mainform.mask(ComboBoxColumns.Text) );
        lblOperation.caption := 'Inserting data ...';
        lblOperation.Repaint;
        for j:=0 to length(cols)-1 do
        begin
          if cols[j].Name = ComboBoxColumns.Text then
            continue;
          ZQuery3.SQL.Add( ', ' + mainform.mask(cols[j].Name) );
        end;
        ZQuery3.SQL.Add( ') VALUES (:STREAM, ' );

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
            DecodeDate(FileDateToDateTime(FileAge(filename)), y, m, d);
            DecodeTime(FileDateToDateTime(FileAge(filename)), h, mi, s, ms);
            Value := stringreplace(Value, '%filedate%', Format('%.4d-%.2d-%.2d', [y,m,d]), [rfReplaceAll]);
            Value := stringreplace(Value, '%filedatetime%', Format('%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [y,m,d,h,mi,s]), [rfReplaceAll]);
            Value := stringreplace(Value, '%filetime%', Format('%.2d:%.2d:%.2d', [h,mi,s]), [rfReplaceAll]);
          end;
          if cols[j].Quote then
            Value := '"' + escape_string(Value) + '"';
          ZQuery3.SQL.Add( Value + ', ' );
        end;
        // Strip last komma + space + CR + LF
        ZQuery3.SQL.Text := copy( ZQuery3.SQL.Text, 1, length(ZQuery3.SQL.Text)-4 ); 
        ZQuery3.SQL.Add( ')' );
        try
          lblOperation.caption := 'Reading file ...';
          lblOperation.Repaint;
          FileStream := TFileStream.Create( filename, fmShareDenyWrite );
          try
            ZQuery3.Params.Clear;
            ZQuery3.Params.CreateParam( ftBlob, 'STREAM', ptInput );
            ZQuery3.ParamByName('STREAM').LoadfromStream( FileStream, ftBlob );
          finally
            FileStream.Free;
          end;
        except
          MessageDlg( 'Error reading file:'#13#10+filename, mtError, [mbOK], 0 );
          break;
        end;
        ZQuery3.ExecSQL;
        lblOperation.caption := 'Freeing memory ...';
        lblOperation.Repaint;
      end;
      ProgressBar1.StepIt;
      ProgressBar1.Repaint;
    end;
  end;

  FINALLY
    TMDIChild(Mainform.ActiveMDIChild).ZQuery3.ParamCheck := false;
    screen.Cursor := crDefault;
    Close();
  END;
end;

procedure TfrmInsertFilesProgress.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
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
