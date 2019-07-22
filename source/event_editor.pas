unit event_editor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynEdit, SynMemo, SynRegExpr, ComCtrls, ExtCtrls, WideStrUtils,
  apphelpers, dbconnection, dbstructures, gnugettext;

type
  TFrame = TDBObjectEditor;
  TfrmEventEditor = class(TFrame)
    SynMemoBody: TSynMemo;
    btnHelp: TButton;
    btnDiscard: TButton;
    btnSave: TButton;
    lblBody: TLabel;
    PageControlMain: TPageControl;
    tabSettings: TTabSheet;
    tabScheduling: TTabSheet;
    tabCREATEcode: TTabSheet;
    tabALTERcode: TTabSheet;
    lblName: TLabel;
    editName: TEdit;
    lblComment: TLabel;
    chkDropAfterExpiration: TCheckBox;
    editComment: TEdit;
    grpState: TRadioGroup;
    radioOnce: TRadioButton;
    radioEvery: TRadioButton;
    dateOnce: TDateTimePicker;
    timeOnce: TDateTimePicker;
    editEveryQuantity: TEdit;
    udEveryQuantity: TUpDown;
    comboEveryInterval: TComboBox;
    chkStarts: TCheckBox;
    chkEnds: TCheckBox;
    dateStarts: TDateTimePicker;
    timeStarts: TDateTimePicker;
    timeEnds: TDateTimePicker;
    dateEnds: TDateTimePicker;
    SynMemoCREATEcode: TSynMemo;
    SynMemoALTERcode: TSynMemo;
    comboDefiner: TComboBox;
    lblDefiner: TLabel;
    procedure Modification(Sender: TObject);
    procedure radioScheduleClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnDiscardClick(Sender: TObject);
    procedure PageControlMainChange(Sender: TObject);
    procedure chkStartsEndsClick(Sender: TObject);
    procedure comboEveryIntervalChange(Sender: TObject);
    procedure comboDefinerDropDown(Sender: TObject);
  private
    { Private declarations }
    AlterCodeValid, CreateCodeValid: Boolean;
    function ComposeCreateStatement: String;
    function ComposeAlterStatement: String;
    function ComposeStatement(CreateOrAlter, ObjName: String): String;
    procedure UpdateSQLcode;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(Obj: TDBObject); override;
    function ApplyModifications: TModalResult; override;
  end;

implementation

uses main;

{$R *.dfm}


constructor TfrmEventEditor.Create(AOwner: TComponent);
begin
  inherited;
  Mainform.SynCompletionProposal.AddEditor(SynMemoBody);
  comboEveryInterval.Items := Explode('|', 'YEAR|QUARTER|MONTH|DAY|HOUR|MINUTE|WEEK|SECOND|YEAR_MONTH|'+
    'DAY_HOUR|DAY_MINUTE|DAY_SECOND|HOUR_MINUTE|HOUR_SECOND|MINUTE_SECOND');
  grpState.Items := Explode('|', 'Enable|Disable|Disable on slave');
end;


destructor TfrmEventEditor.Destroy;
begin
  // Store GUI setup? Nothing yet.
  inherited;
end;


procedure TfrmEventEditor.Init(Obj: TDBObject);
var
  CreateCode, DateExpr: String;
  rx: TRegExpr;
  d: TDateTime;
  i: Integer;
begin
  inherited;
  editName.Clear;
  editComment.Clear;
  comboDefiner.Text := '';
  comboDefiner.TextHint := f_('Current user (%s)', [Obj.Connection.CurrentUserHostCombination]);
  comboDefiner.Hint := f_('Leave empty for current user (%s)', [Obj.Connection.CurrentUserHostCombination]);
  chkDropAfterExpiration.Checked := True;
  radioEvery.Checked := True;
  grpState.ItemIndex := 0;
  SynMemoBody.Text := 'BEGIN'+CRLF+CRLF+'END';
  dateOnce.Date := Now;
  timeOnce.Time := Now;
  dateStarts.Date := Now;
  timeStarts.Time := Now;
  dateEnds.Date := Now;
  timeEnds.Time := Now;
  udEveryQuantity.Position := 1;
  comboEveryInterval.ItemIndex := comboEveryInterval.Items.IndexOf('DAY');
  tabALTERcode.TabVisible := False;
  if DBObject.Name <> '' then begin
    // Edit mode
    tabALTERcode.TabVisible := True;
    editName.Text := DBObject.Name;
    // CREATE DEFINER=`root`@`127.0.0.1` EVENT `eventb`
    // ON SCHEDULE EVERY 1 DAY STARTS '2010-04-08 08:05:04' ENDS '2010-04-30 01:01:28'
    // ON COMPLETION NOT PRESERVE
    // ENABLE
    // DO BEGIN END
    CreateCode := DBObject.CreateCode;
    DateExpr := '[''"]([^''"]+)[''"]';
    rx := TRegExpr.Create;
    rx.ModifierI := True;

    rx.Expression := '\bDEFINER\s*=\s*(\S+)\s';
    if rx.Exec(CreateCode) then
      comboDefiner.Text := Obj.Connection.DequoteIdent(rx.Match[1], '@')
    else
      comboDefiner.Text := '';

    rx.Expression := '\bON\s+SCHEDULE\s+(EVERY|AT)\s+((\d+|''[^'']+'')\s+(\S+)(\s+STARTS\s+'+DateExpr+')?(\s+ENDS\s+'+DateExpr+')?|'+DateExpr+')';
    if rx.Exec(CreateCode) then begin
      if UpperCase(rx.Match[1]) = 'AT' then begin
        radioOnce.Checked := True;
        d := MainForm.ActiveConnection.ParseDateTime(WideDequotedStr(rx.Match[3], ''''));
        dateOnce.DateTime := d;
        timeOnce.DateTime := d;
      end else begin
        radioEvery.Checked := True;
        comboEveryInterval.ItemIndex := comboEveryInterval.Items.IndexOf(rx.Match[4]);
        comboEveryIntervalChange(Self);
        if udEveryQuantity.Enabled then
          udEveryQuantity.Position := MakeInt(rx.Match[3])
        else
          editEveryQuantity.Text := WideDequotedStr(rx.Match[3], '''');
        chkStarts.Checked := rx.MatchLen[5] > 0;
        if chkStarts.Checked then begin
          d := MainForm.ActiveConnection.ParseDateTime(rx.Match[6]);
          dateStarts.DateTime := d;
          timeStarts.DateTime := d;
        end;
        chkEnds.Checked := rx.MatchLen[7] > 0;
        if chkEnds.Checked then begin
          d := MainForm.ActiveConnection.ParseDateTime(rx.Match[8]);
          dateEnds.DateTime := d;
          timeEnds.DateTime := d;
        end;
      end;
      Delete(CreateCode, 1, rx.MatchPos[0]+rx.MatchLen[0]);
    end;

    rx.Expression := '^ON\s+COMPLETION(\s+NOT)?\s+PRESERVE\b';
    if rx.Exec(CreateCode) then begin
      chkDropAfterExpiration.Checked := rx.MatchLen[1] > 0;
      Delete(CreateCode, 1, rx.MatchPos[0]+rx.MatchLen[0]);
    end;

    for i:=grpState.Items.Count-1 downto 0 do begin
      if Pos(UpperCase(grpState.Items[i]), UpperCase(CreateCode)) = 1 then begin
        grpState.ItemIndex := i;
        Delete(CreateCode, 1, Length(grpState.Items[i]));
        break;
      end;
    end;

    rx.ModifierG := False;
    rx.Expression := '\bCOMMENT\s+''((.+)[^''])''';
    if rx.Exec(CreateCode) then begin
      editComment.Text := StringReplace(rx.Match[1], '''''', '''', [rfReplaceAll]);
      Delete(CreateCode, rx.MatchPos[0], rx.MatchLen[0]);
    end;

    rx.ModifierG := True;
    rx.Expression := '\bDO\s+(.*)';
    if rx.Exec(CreateCode) then
      SynMemoBody.Text := rx.Match[1];

    rx.Free;

  end;
  radioScheduleClick(Self);
  Modified := False;
  btnSave.Enabled := Modified;
  btnDiscard.Enabled := Modified;
  Mainform.ShowStatusMsg;
  Screen.Cursor := crDefault;
end;


procedure TfrmEventEditor.Modification(Sender: TObject);
begin
  Modified := True;
  btnSave.Enabled := Modified and (editName.Text <> '');
  btnDiscard.Enabled := Modified;
  CreateCodeValid := False;
  AlterCodeValid := False;
  UpdateSQLcode;
end;


procedure TfrmEventEditor.btnSaveClick(Sender: TObject);
begin
  ApplyModifications;
end;


function TfrmEventEditor.ApplyModifications: TModalResult;
var
  sql: String;
begin
  // Create or alter table
  Result := mrOk;
  if DBObject.Name = '' then
    sql := ComposeCreateStatement
  else
    sql := ComposeAlterStatement;
  try
    MainForm.ActiveConnection.Query(sql);
    DBObject.Name := editName.Text;
    DBObject.CreateCode := '';
    tabALTERcode.TabVisible := DBObject.Name <> '';
    Mainform.UpdateEditorTab;
    Mainform.RefreshTree(DBObject);
    Modified := False;
    btnSave.Enabled := Modified;
    btnDiscard.Enabled := Modified;
    AlterCodeValid := False;
    CreateCodeValid := False;
    UpdateSQLcode;
  except
    on E:EDbError do begin
      ErrorDialog(E.Message);
      Result := mrAbort;
    end;
  end;
end;


function TfrmEventEditor.ComposeCreateStatement: String;
begin
  Result := ComposeStatement('CREATE', editName.Text);
end;


function TfrmEventEditor.ComposeAlterStatement: String;
begin
  Result := ComposeStatement('ALTER', DBObject.Name);
end;


function TfrmEventEditor.ComposeStatement(CreateOrAlter, ObjName: String): String;
var
  d: TDateTime;
  Quantity: String;
begin
  // Return CREATE EVENT statement
  Result := CreateOrAlter + ' ';
  if comboDefiner.Text <> '' then
    Result := Result + 'DEFINER='+DBObject.Connection.QuoteIdent(comboDefiner.Text, True, '@')+' ';
  Result := Result + 'EVENT ' + DBObject.Connection.QuoteIdent(ObjName) + CRLF + #9 + 'ON SCHEDULE' + CRLF + #9#9;
  if radioOnce.Checked then begin
    d := dateOnce.DateTime;
    ReplaceTime(d, timeOnce.DateTime);
    Result := Result + 'AT ' + esc(DateTimeToStr(d)) + CRLF;
  end else begin
    if udEveryQuantity.Enabled then
      Quantity := IntToStr(udEveryQuantity.Position)
    else
      Quantity := esc(editEveryQuantity.Text);
    Result := Result + 'EVERY ' + Quantity + ' ' + comboEveryInterval.Text;
    if chkStarts.Checked then begin
      d := dateStarts.DateTime;
      ReplaceTime(d, timeStarts.DateTime);
      Result := Result + ' STARTS ' + esc(DateTimeToStr(d));
    end;
    if chkEnds.Checked then begin
      d := dateEnds.DateTime;
      ReplaceTime(d, timeEnds.DateTime);
      Result := Result + ' ENDS ' + esc(DateTimeToStr(d));
    end;
    Result := Result + CRLF;
  end;

  if chkDropAfterExpiration.Checked then
    Result := Result + #9 + 'ON COMPLETION NOT PRESERVE'
  else
    Result := Result + #9 + 'ON COMPLETION PRESERVE';
  if (DBObject.Name <> '') and (DBObject.Name <> editName.Text) then
    Result := Result + CRLF + #9 + 'RENAME TO ' + DBObject.Connection.QuoteIdent(editName.Text);
  Result := Result + CRLF + #9 + UpperCase(grpState.Items[grpState.ItemIndex]);
  Result := Result + CRLF + #9 + 'COMMENT ' + esc(editComment.Text);
  Result := Result + CRLF + #9 + 'DO ' + SynMemoBody.Text;
end;


procedure TfrmEventEditor.PageControlMainChange(Sender: TObject);
begin
  UpdateSQLcode;
end;


procedure TfrmEventEditor.UpdateSQLcode;
var
  OldTopLine: Integer;
begin
  if (PageControlMain.ActivePage = tabALTERCode) and (not AlterCodeValid) then begin
    SynMemoALTERcode.BeginUpdate;
    OldTopLine := SynMemoALTERcode.TopLine;
    SynMemoALTERcode.Text := ComposeAlterStatement;
    SynMemoALTERcode.TopLine := OldTopLine;
    SynMemoALTERcode.EndUpdate;
    AlterCodeValid := True;
  end else if (PageControlMain.ActivePage = tabCREATECode) and (not CreateCodeValid) then begin
    SynMemoCREATEcode.BeginUpdate;
    OldTopLine := SynMemoCREATEcode.TopLine;
    SynMemoCREATEcode.Text := ComposeCreateStatement;
    SynMemoCREATEcode.TopLine := OldTopLine;
    SynMemoCREATEcode.EndUpdate;
    CreateCodeValid := True;
  end;
end;


procedure TfrmEventEditor.radioScheduleClick(Sender: TObject);
var
  IsOnce: Boolean;
begin
  IsOnce := radioOnce.Checked;
  dateOnce.Enabled := IsOnce;
  timeOnce.Enabled := IsOnce;
  editEveryQuantity.Enabled := not IsOnce;
  udEveryQuantity.Enabled := not IsOnce;
  comboEveryInterval.Enabled := not IsOnce;
  comboEveryIntervalChange(Sender);
  chkStarts.Enabled := not IsOnce;
  chkEnds.Enabled := not IsOnce;
  chkStartsEndsClick(Sender);
  Modification(Sender);
end;


procedure TfrmEventEditor.chkStartsEndsClick(Sender: TObject);
begin
  // Enable/disable start+end controls
  dateStarts.Enabled := chkStarts.Checked and chkStarts.Enabled;
  timeStarts.Enabled := chkStarts.Checked and chkStarts.Enabled;
  dateEnds.Enabled := chkEnds.Checked and chkEnds.Enabled;
  timeEnds.Enabled := chkEnds.Checked and chkEnds.Enabled;
  Modification(Sender);
end;


procedure TfrmEventEditor.comboDefinerDropDown(Sender: TObject);
begin
  // Populate definers from mysql.user
  (Sender as TComboBox).Items.Assign(DBObject.Connection.AllUserHostCombinations);
end;


procedure TfrmEventEditor.comboEveryIntervalChange(Sender: TObject);
begin
  // To enter DAY_HOUR values editor has to accept "1-2"
  if Pos('_', comboEveryInterval.Text) > 0 then begin
    udEveryQuantity.Associate := nil;
    udEveryQuantity.Enabled := False;
  end else begin
    udEveryQuantity.Associate := editEveryQuantity;
    udEveryQuantity.Enabled := True;
  end;
  Modification(Sender);
end;

procedure TfrmEventEditor.btnDiscardClick(Sender: TObject);
begin
  // Reinit editor, discarding changes
  Modified := False;
  Init(DBObject);
end;


procedure TfrmEventEditor.btnHelpClick(Sender: TObject);
begin
  Help(Self, 'createevent');
end;


end.
