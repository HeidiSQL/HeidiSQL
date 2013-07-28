unit runsqlfile;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, ComCtrls, StdCtrls, gnugettext;

type
  TRunSQLFileForm = class(TForm)
    btnClose: TButton;
    lblFilenameValue: TLabel;
    lblFilenameName: TLabel;
    lblQueryName: TLabel;
    lblPositionName: TLabel;
    lblPositionValue: TLabel;
    prbarRun: TProgressBar;
    memoQueryValue: TMemo;
    lblQueryCountName: TLabel;
    lblQueryCountValue: TLabel;
    lblTimeName: TLabel;
    lblTimeValue: TLabel;
    lblAffectedRowsName: TLabel;
    lblAffectedRowsValue: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Running : Boolean;
  public
    { Public declarations }
    SQLFileName : String;
    FileEncoding: TEncoding;
  end;


implementation

uses
  helpers,
  main;

{$R *.dfm}



{**
  Startup - run the sql file
}
procedure TRunSQLFileForm.FormActivate(Sender: TObject);
var
  Stream: TFileStream;
  lines, lines_remaining: String;
  filesize, querycount, rowsaffected: Int64;
  starttime: Cardinal;
  Queries: TSQLBatch;
  i: Integer;
begin
  if Running then
    abort;
  Running := True;

  // Initialize various variables
  lblFilenameValue.Caption := SQLFileName;
  lblTimeValue.Caption := FormatTimeNumber(0, True);
  memoQueryValue.Lines.Clear;
  lines := '';
  querycount := 0;
  rowsaffected := 0;
  lines_remaining := '';
  starttime := GetTickCount;
  Queries := TSQLBatch.Create;

  Screen.Cursor := crHourGlass;

  try
    // Start file operations
    filesize := _GetFileSize( SQLFileName );

    OpenTextfile(SQLFileName, Stream, FileEncoding);
    lblPositionValue.Caption := FormatNumber( Stream.Position ) + ' / ' + FormatNumber( filesize );
    Repaint;

    while Stream.Position < Stream.Size do
    begin
      // Read lines from SQL file until buffer reaches a limit of some MB
      // This strategy performs vastly better than looping through each line
      lines := ReadTextfileChunk(Stream, FileEncoding, 20*SIZE_MB);

      // Display position in file
      lblPositionValue.Caption := FormatByteNumber( Stream.Position ) + ' / ' + FormatByteNumber( filesize );

      // Time
      lblTimeValue.Caption := FormatTimeNumber((GetTickCount - starttime) DIV 1000, True);

      // Step progressbar's position
      prbarRun.Position := Trunc( prbarRun.Max / filesize * Stream.Position );

      Repaint;

      // Split buffer into single queries
      Queries.SQL := lines_remaining + lines;
      lines := '';
      lines_remaining := '';

      // Execute detected queries
      for i:=0 to Queries.Count-1 do
      begin
        // Last line has to be processed in next loop if end of file is not reached
        if (i = Queries.Count-1) and (Stream.Position < Stream.Size) then
        begin
          lines_remaining := Queries[i].SQL;
          break;
        end;

        // Count queries
        querycount := querycount + 1;
        lblQueryCountValue.Caption := FormatNumber( querycount );

        // Display part of query
        memoQueryValue.Text := sstr(Queries[i].SQL, 100 );

        // Time
        lblTimeValue.Caption := FormatTimeNumber((GetTickCount - starttime) DIV 1000, True);

        // Execute single query and display affected rows
        try
          Mainform.ActiveConnection.Query(Queries[i].SQL);
          rowsaffected := rowsaffected + Mainform.ActiveConnection.RowsAffected;
          lblAffectedRowsValue.Caption := FormatNumber( rowsaffected );
        except
          on E:Exception do
            ErrorDialog(E.Message);
        end;
        Application.ProcessMessages;
      end;
    end;
    Queries.Free;
    Stream.Free;
  except
    on E:EFileStreamError do
    begin
      ErrorDialog(f_('Error while reading file "%s"', [SQLFileName]), E.Message);
      Mainform.AddOrRemoveFromQueryLoadHistory(SQLFileName, False, True);
      Mainform.FillPopupQueryLoad;
    end;
  end;
  Screen.Cursor := crDefault;
  btnClose.Enabled := True;
end;


procedure TRunSQLFileForm.FormCreate(Sender: TObject);
begin
  InheritFont(Font);
  TranslateComponent(Self);
end;

end.
