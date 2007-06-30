unit heidicomp;

interface

uses
  Windows, Classes, Controls, Forms, Dialogs, SysUtils,
  ComCtrls, CommCtrl, StdCtrls, ExtCtrls, Graphics,
  ZDataset;

{$I ../../../source/const.inc}

type
  TSortListView = class(TListView)
	private
    FImageIndexSortDesc,
    FImageIndexSortAsc : Integer;

  protected
    procedure SetImageIndexSortAsc( NewValue: Integer );
    procedure SetImageIndexSortDesc( NewValue: Integer );

  published
    procedure ColClick(Column: TListColumn); override;
    procedure ClearSortColumnImages;
    property ImageIndexSortAsc: Integer read FImageIndexSortAsc write SetImageIndexSortAsc default -1;
    property ImageIndexSortDesc: Integer read FImageIndexSortDesc write SetImageIndexSortDesc default -1;
  end;


  TVisibleOptions = (voCSV, voHTML);


  TExportSaveDialog = class(TSaveDialog)
  private
    FExtraPanel       : TPanel;

    FVisibleOptions   : TVisibleOptions;

    // Controls and vars for use with voCSV
    FFieldSepLabel,
		FLineSepLabel,
		FFieldEnclLabel   : TLabel;

    FFieldSepEdit,
    FLineSepEdit,
    FFieldEnclEdit    : TEdit;

    FFieldSep,
    FLineSep,
    FFieldEncl        : string;

    // Controls and vars for use with voHTML
    FConvertHTMLSpecialCharsCheckbox  : TCheckBox;
    FConvertHTMLSpecialChars          : Boolean;


  protected
    procedure DoClose; override;
    procedure DoShow; override;

    procedure SetFieldSep(NewValue:string);
    procedure SetLineSep(NewValue:string);
    procedure SetFieldEncl(NewValue:string);
    procedure SetVisibleOptions(NewValue: TVisibleOptions);

    procedure SetConvertHTMLSpecialChars(NewValue: Boolean);
    procedure ConvertHTMLSpecialCharsCheckboxOnclick(sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;

  published
    property FieldSep: string read FFieldSep write SetFieldSep;
    property LineSep: string read FLineSep write SetLineSep;
    property FieldEncl: string read FFieldEncl write SetFieldEncl;
    property VisibleOptions: TVisibleOptions read FVisibleOptions write SetVisibleOptions;
    property ConvertHTMLSpecialChars: Boolean read FConvertHTMLSpecialChars write SetConvertHTMLSpecialChars;

  end;

type
  TDeferDataSet = class;
  TAsyncPostRunner = procedure(ds: TDeferDataSet) of object;

  TDeferDataSet = class(TZQuery)
  private
    callback: TAsyncPostRunner;
    kind: Integer;
 protected
    procedure InternalPost; override;
 public
    constructor Create(AOwner: TComponent; PostCallback: TAsyncPostRunner); reintroduce;
    procedure ExecSQL; override;
    procedure DoAsync;
    procedure DoAsyncExecSql;
 end;

procedure Register;


implementation

procedure TDeferDataSet.InternalPost;
begin
  kind := 1;
  if @callback = nil then DoAsync
  else callback(self);
end;

procedure TDeferDataSet.ExecSql;
begin
  kind := 2;
  if @callback = nil then DoAsync
  else callback(self);
end;

constructor TDeferDataSet.Create(AOwner: TComponent; PostCallback: TAsyncPostRunner);
begin
  callback := PostCallback;
  inherited Create(AOwner);
end;

procedure TDeferDataSet.DoAsync;
begin
  case kind of
    1: inherited InternalPost;
    2: inherited ExecSQL;
  end;
end;

procedure TDeferDataSet.DoAsyncExecSql;
begin
  inherited ExecSql;
end;

procedure Register;
begin
  RegisterComponents(APPNAME, [TSortListView, TExportSaveDialog]);
end;


function CustomSortProc(Item1, Item2: TListItem; Column: integer): integer;
stdcall;
  {***
    Convert a string-number to an integer-number
    @note
      Copied from helpers.pas
      didn't want to add helpers.pas to uses-clause, as we
      should keep this file without dependencies

    @param string String-number
    @return int64
  }
  function MakeInt( Str: String ) : Int64;
  var
    i : Integer;
    StrWithInts : String;
  begin
    StrWithInts := '';
    for i:=1 to Length(str) do
    begin
      if StrToIntDef( str[i], -1 ) <> -1 then
      begin
        StrWithInts := StrWithInts + str[i];
      end;
    end;
    result := StrToInt64Def( StrWithInts, 0 );
  end;

var
  str1, str2 : String;
  int1, int2 : Int64;
  ValAIsBigger, ValBIsBigger : Shortint;
begin
  result:=0;

  if Column = 0 then
  begin
    str1 := Item1.Caption;
    str2 := Item2.Caption;
  end
  else
  begin // Take care of lines with less than needed subitems
    if Item1.SubItems.Count >= Column then
      str1 := Item1.SubItems[Column-1]
    else
      str1 := '';
    if Item2.SubItems.Count >= Column then
      str2 := Item2.SubItems[Column-1]
    else
      str2 := '';
  end;

  // prepare result-values for comparision, taking the current sort-direction into account
  ValAIsBigger := -1 * Item1.ListView.Column[Column].Tag;
  ValBIsBigger := 1 * Item1.ListView.Column[Column].Tag;

  if StrToIntDef( copy(str1,0,1), -1 ) <> -1 then
  begin
    // Assuming we have numeric values
    int1 := MakeInt( str1 );
    int2 := MakeInt( str2 );
    if int1 > int2 then
      result := ValAIsBigger
    else if int1 = int2 then
      result := 0
    else if int1 < int2 then
      result := ValBIsBigger;
  end
  else
  begin
    // Compare Strings
    if str1 > str2 then
      result := ValAIsBigger
    else if str1 = str2 then
      result := 0
    else if str1 < str2 then
      result := ValBIsBigger;
  end;

end;


// ***************************************************************
// TSortListView


procedure TSortListView.ColClick(Column: TListColumn);
var
  i : Integer;
const
  isDesc = -1;
  isAsc = 1;
begin
  // Remove all icons from columnheaders
  ClearSortColumnImages;
  // Clear Sort-Tags
  for i := 0 to Columns.Count-1 do
  begin
    if Columns[i] <> Column then
      Columns[i].Tag := 0;
  end;
  // Reverse or initiate sort-direction
  if Column.Tag in [0, isAsc] then
    Column.Tag := isDesc
  else
    Column.Tag := isAsc;
  // Assign icon-index if valid
  if (SmallImages <> nil) and (SmallImages.Count > ImageIndexSortAsc) and (SmallImages.Count > ImageIndexSortDesc) then
  begin
    if Column.Tag = isAsc then
      Column.ImageIndex := ImageIndexSortAsc
    else
      Column.ImageIndex := ImageIndexSortDesc;
  end;
  inherited colclick(Column);
  // Run sort-procedure
  CustomSort(@CustomSortProc, Column.index);
end;


procedure TSortListView.SetImageIndexSortAsc( NewValue: Integer );
begin
  if NewValue <> ImageIndexSortAsc then
  begin
    FImageIndexSortAsc := NewValue;
  end;
end;


procedure TSortListView.SetImageIndexSortDesc( NewValue: Integer );
begin
  if NewValue <> ImageIndexSortDesc then
  begin
    FImageIndexSortDesc := NewValue;
  end;
end;


procedure TSortListView.ClearSortColumnImages;
var
  i : Integer;
begin
  for i := 0 to Columns.Count-1 do
  begin
    Columns[i].ImageIndex := -1;
  end;
end;


// ***************************************************************
// TExportSaveDialog

constructor TExportSaveDialog.Create(AOwner: TComponent);
var
  xpos,
  spacing,
  edits_width,
  labels_top    : Integer;
begin
  inherited Create(AOwner);

  // Create the panel on which we put all other controls
  FExtraPanel := TPanel.Create(self);
  FExtraPanel.BorderWidth := 0;
  FExtraPanel.BevelOuter := bvNone;
  FExtraPanel.BevelInner := bvNone;
  FExtraPanel.Name := 'ExtraPanel';
  FExtraPanel.Caption := '';
  FExtraPanel.TabOrder := 1;

  xpos := 0;              // used for Left-properties, increased with each component
  spacing := 5;           // spacing between Labels and Edits.
  edits_width := 35;      // standard-width for Edits
  labels_top := 4;        // Labels need a bit more distance to top, so they get centered horizontally with the Edits


  // **** Add controls for use with voCSV

  // Field-separator
  FFieldSepLabel := TLabel.Create(FExtraPanel);
  with FFieldSepLabel do
  begin
    Name := 'FieldSepLabel';
    Caption := 'Field-separator:';
    Left := spacing;
    Top := labels_top;
    Align := alNone;
    AutoSize := True;
    Parent := FExtraPanel;
    Tag := Byte(voCSV);
    Visible := (Tag = Integer(FVisibleOptions));
    inc(xpos, Width+spacing);
  end;
  FFieldSepEdit := TEdit.Create(FExtraPanel);
  with FFieldSepEdit do
  begin
    Name := 'FieldSepEdit';
    Left := xpos+spacing;
    Width := edits_width;
    Text := FieldSep;
    Enabled := True;
    Ctl3D:=true;
    MaxLength := 10;
    TabOrder := 1;
    Parent := FExtraPanel;
    Tag := Byte(voCSV);
    Visible := (Tag = Integer(FVisibleOptions));
    inc(xpos, Width+spacing);
  end;

  // Line-terminator
  FLineSepLabel := TLabel.Create(FExtraPanel);
  with FLineSepLabel do
  begin
    Name := 'LineSepLabel';
    Caption := 'Line-terminator:';
    Left := xpos+spacing*2;
    Top := labels_top;
    Align := alNone;
    AutoSize := True;
    Parent := FExtraPanel;
    Tag := Byte(voCSV);
    Visible := (Tag = Integer(FVisibleOptions));
    inc(xpos, Width+spacing*2);
  end;
  FLineSepEdit := TEdit.Create(FExtraPanel);
  with FLineSepEdit do
  begin
    Name := 'LineSepEdit';
    Left := xpos+spacing;
    Width := edits_width;
    Text := LineSep;
    Enabled := True;
    Ctl3D:=true;
    MaxLength := 10;
    TabOrder := 2;
    Parent := FExtraPanel;
    Tag := Byte(voCSV);
    Visible := (Tag = Integer(FVisibleOptions));
    inc(xpos, Width+spacing);
  end;

  // Field-encloser
  FFieldEnclLabel := TLabel.Create(FExtraPanel);
  with FFieldEnclLabel do
  begin
    Name := 'FieldEnclLabel';
    Caption := 'Field-encloser:';
    Left := xpos+spacing*2;
    Top := labels_top;
    Align := alNone;
    AutoSize := True;
    Parent := FExtraPanel;
    Tag := byte(voCSV);
    Visible := (Tag = Integer(FVisibleOptions));
    inc(xpos, Width+spacing*2);
  end;
  FFieldEnclEdit := TEdit.Create(FExtraPanel);
  with FFieldEnclEdit do
  begin
    Name := 'FieldEnclEdit';
    Left := xpos+spacing;
    Width := edits_width;
    Text := FieldEncl;
    Enabled := True;
    Ctl3D:=true;
    MaxLength := 10;
    TabOrder := 3;
    Tag := Byte(voCSV);
    Visible := (Tag = Integer(FVisibleOptions));
    Parent := FExtraPanel;
  end;

  // **** Add controls for use with voHTML

  xpos := 0;

  // Convert HTMLSpecialChars?
  FConvertHTMLSpecialCharsCheckbox := TCheckbox.Create(FExtraPanel);
  with FConvertHTMLSpecialCharsCheckbox do
  begin
    Name := 'ConvertHTMLSpecialChars';
    Left := xpos+spacing;
    Top := labels_top;
    Caption := 'Convert special HTML-characters';
    Enabled := True;
    Ctl3D:=true;
    Width := 300;
    TabOrder := 1;
    Parent := FExtraPanel;
    OnClick := ConvertHTMLSpecialCharsCheckboxOnclick;
    Checked := ConvertHTMLSpecialChars;
    Tag := Byte(voHTML);
    Visible := (Tag = Integer(FVisibleOptions));
  end;

end;

destructor TExportSaveDialog.Destroy;
begin
  FFieldSepLabel.Free;
  FFieldSepEdit.Free;

  FLineSepLabel.Free;
  FLineSepEdit.Free;

  FFieldEnclLabel.Free;
  FFieldEnclEdit.Free;

  FExtraPanel.Free;
  inherited Destroy;
end;

procedure TExportSaveDialog.SetFieldSep(NewValue: String);
begin
  if NewValue <> FieldSep then
  begin
    FFieldSep := NewValue;
    FFieldSepEdit.Text := FFieldSep;
  end;
end;

procedure TExportSaveDialog.SetLineSep(NewValue: String);
begin
  if NewValue <> LineSep then
  begin
    FLineSep := NewValue;
    FLineSepEdit.Text := FLineSep;
  end;
end;

procedure TExportSaveDialog.SetFieldEncl(NewValue: String);
begin
  if NewValue <> FieldEncl then
  begin
    FFieldEncl := NewValue;
    FFieldEnclEdit.Text := FFieldEncl;
  end;
end;


procedure TExportSaveDialog.SetConvertHTMLSpecialChars(NewValue: Boolean);
begin
  if NewValue <> ConvertHTMLSpecialChars then
  begin
    FConvertHTMLSpecialChars := NewValue;
    FConvertHTMLSpecialCharsCheckBox.Checked := FConvertHTMLSpecialChars;
  end;
end;

procedure TExportSaveDialog.ConvertHTMLSpecialCharsCheckboxOnclick(sender: TObject);
begin
  SetConvertHTMLSpecialChars( FConvertHTMLSpecialCharsCheckbox.Checked );
end;


procedure TExportSaveDialog.DoClose;
begin
  FFieldSep := FFieldSepEdit.Text;
  FLineSep := FLineSepEdit.Text;
  FFieldEncl := FFieldEnclEdit.Text;
  FConvertHTMLSpecialChars := FConvertHTMLSpecialCharsCheckbox.Checked;
  inherited DoClose;
  { Hide any hint windows left behind }
  Application.HideHint;
end;

procedure TExportSaveDialog.DoShow;
var
  hSaveDlg : Cardinal;
  Rect1 : TRect;
begin
  // the save Dialog window is the parent of the SaveDialog1.Handle
  hSaveDlg := GetParent(Handle);
  // for reasons of system message transfer you must place all your funtional
  // delphi controls on some Delphi WinControl, like a TPanel
  GetWindowRect(hSaveDlg, Rect1);
  // I increase the height of the SaveDialog1 with the MoveWindow function,
  // the Left and Top are meanigless, since Delphi will center the window
  MoveWindow(hSaveDlg, 3,3, Rect1.Right-Rect1.Left, Rect1.Bottom-Rect1.Top+30, True);

  FExtraPanel.ParentWindow := hSaveDlg;
  FExtraPanel.SetBounds(0, Rect1.Bottom-Rect1.Top-30, Rect1.Right-Rect1.Left, 30);

  inherited DoShow;
end;



function TExportSaveDialog.Execute: Boolean;
begin
{ if NewStyleControls and not (ofOldStyleDialog in Options) then
    Template := 'TEXTFILEDLG'
    //Template := 'DLGTEMPLATE'
  else
    Template := nil;}
  Result := inherited Execute;
end;

procedure TExportSaveDialog.SetVisibleOptions( NewValue: TVisibleOptions );
var
  i,
  comptag : Byte;
begin
  // Hide/unhide option-controls
  if NewValue <> VisibleOptions then
  begin
    FVisibleOptions := NewValue;
    comptag := Byte(FVisibleOptions);

    for i := 0 to FExtraPanel.ComponentCount - 1 do
    begin
      TWinControl(FExtraPanel.Components[i]).Visible := (FExtraPanel.Components[i].Tag = comptag)
    end;
  end;
end;


end.
