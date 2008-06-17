unit runsqlfile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

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
  private
    { Private declarations }
    SQLFileName : String;
    Running : Boolean;
  public
    { Public declarations }
  end;

  function RunSQLFileWindow(AOwner: TComponent; SQLFileName: String): Boolean;

implementation

uses
  helpers,
  main,
  WideStrings;

{$R *.dfm}


{**
  Create form on demand
  @param TComponent Owner of form (should be calling form)
  @return Boolean Form closed using modalresult mrOK
}
function RunSQLFileWindow(AOwner: TComponent; SQLFileName: String): Boolean;
var
  f : TRunSQLFileForm;
begin
  f := TRunSQLFileForm.Create(AOwner);
  f.SQLFileName := SQLFileName;
  Result := (f.ShowModal=mrOK);
  FreeAndNil(f);
end;


{**
  Startup - run the sql file
}
procedure TRunSQLFileForm.FormActivate(Sender: TObject);
var
  f                   : Textfile;
  tmpstr,
  lines               : String;
  filesize,
  fileoffset,
  querycount,
  rowsaffected        : Int64;
  lastRepaintTime,
  time,
  starttime           : Cardinal;
  SQL                 : TWideStringList;
  i                   : Integer;
  lines_remaining     : String;
begin
  if Running then
    abort;
  Running := True;

  // Initialize various variables
  lblFilenameValue.Caption := mince( SQLFileName, 35 );
  lblTimeValue.Caption := FormatTimeNumber( 0 );
  memoQueryValue.Lines.Clear;
  lines := '';
  fileoffset := 0;
  querycount := 0;
  lastRepaintTime := 0;
  rowsaffected := 0;
  lines_remaining := '';
  starttime := GetTickCount;

  Screen.Cursor := crHourGlass;

  try
    // Start file operations
    filesize := _GetFileSize( SQLFileName );

    lblPositionValue.Caption := FormatNumber( fileoffset ) + ' / ' + FormatNumber( filesize );
    Repaint;

    AssignFile( f, SQLFileName );
    Reset( f );
    while not eof( f ) do
    begin
      // Read lines from SQL file until buffer reaches a limit of some MB
      // This strategy performs vastly better than looping through each line
      while ( not eof( f ) ) and ( Length(lines) < LOAD_SIZE ) do
      begin
        Readln( f, tmpstr );
        // Append line to buffer
        lines := lines + tmpstr + CRLF;

        // Calculate current position in file
        fileoffset := fileoffset + Length(tmpstr) + 2;

        // Avoids memory leak
        tmpstr := '';

        time := GetTickCount;
        if (time - lastRepaintTime > 200) or eof(f) or ( Length(lines) >= LOAD_SIZE ) then
        begin
          // Display position in file
          lblPositionValue.Caption := FormatByteNumber( fileoffset ) + ' / ' + FormatByteNumber( filesize );

          // Time
          lblTimeValue.Caption := FormatTimeNumber( (time - starttime) DIV 1000 );

          // Step progressbar's position
          prbarRun.Position := Trunc( prbarRun.Max / filesize * fileoffset );

          // Remember last repaint-time
          lastRepaintTime := time;
          Repaint;
        end;
      end;

      // Split buffer into single queries
      SQL := parseSQL( lines_remaining + lines, ';' );
      lines := '';
      lines_remaining := '';

      // Execute detected queries
      for i := 0 to SQL.Count - 1 do
      begin
        // Last line has to be processed in next loop if end of file is not reached
        if (i = SQL.Count-1) and (not eof(f)) then
        begin
          lines_remaining := SQL[i];
          break;
        end;

        // Count queries
        querycount := querycount + 1;
        lblQueryCountValue.Caption := FormatNumber( querycount );

        // Display part of query
        memoQueryValue.Text := sstr( SQL[i], 100 );

        // Time
        lblTimeValue.Caption := FormatTimeNumber( (GetTickCount - starttime) DIV 1000 );

        // Execute single query and display affected rows
        rowsaffected := rowsaffected + Mainform.Childwin.ExecUpdateQuery( SQL[i], True, False );
        lblAffectedRowsValue.Caption := FormatNumber( rowsaffected );

        Repaint;
      end;
      SQL.Free;

    end;
    CloseFile( f );
  except
    on E: Exception do
    begin
      MessageDLG( 'Error while reading file ' + SQLFileName + ':' + CRLF + CRLF + E.Message, mtError, [mbOK], 0);
      Mainform.Childwin.AddOrRemoveFromQueryLoadHistory( SQLFileName, false );
      Mainform.Childwin.FillPopupQueryLoad;
    end;
  end;
  Screen.Cursor := crDefault;
  btnClose.Enabled := True;
end;


end.
