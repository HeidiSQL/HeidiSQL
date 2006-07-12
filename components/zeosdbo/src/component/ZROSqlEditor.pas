{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 SQL Monitor component                   }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{            Written by Sergey Seroukhov                  }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZROSqlEditor;

{$I ZComponent.inc}

interface

uses
{$IFDEF WIN32}
  Windows, Messages,
{$ENDIF}
  {$IFDEF FPC}
  LCLIntf,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Graphtype,
  StdCtrls, ExtCtrls, ComCtrls, Buttons, ZDataset, Menus, ZMessages;
  //{$IFDEF USE_SYNEDIT}
  //, SynEdit, SynEditHighlighter, SynHighlighterSQL
  //{$IFDEF FPC}
  //, SynCompletion
  //{$ELSE}
  //, SynCompletionProposal
  //{$ENDIF FPC}
  //{$ENDIF USE_SYNEDIT};

type

  { TZROSQLEditorForm }

  TZROSQLEditorForm = class(TForm)
  private
    mnuSave: TMenuItem;
    mnuLoad: TMenuItem;
    dlgOpen: TOpenDialog;
    PopupMenu1: TPopupMenu;
    dlgSave: TSaveDialog;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Panel2: TPanel;
    tbsSelect: TTabSheet;
    Panel1: TPanel;
    Splitter2: TSplitter;
    ListBoxRelations: TListBox;
    ListBoxFields: TListBox;
    Splitter1: TSplitter;
    Panel3: TPanel;
    lblAlias: TLabel;
    edtAlias: TEdit;
    btnGenerate: TButton;
    btnCheck: TButton;
    chkReplace: TCheckBox;
    btnTest: TButton;
    edtSelect: TMemo;
    //{$IFDEF USE_SYNEDIT}
    //edtSelect: TSynEdit;
    //SynSQLSyn1: TSynSQLSyn;
    //{$IFDEF FPC}
    //SynCompletion:TSynCompletion;
    //{$ELSE}
    //SynCompletion:TSynCompletionProposal;
    //{$ENDIF FPC}
    //{$ELSE}
    //edtSelect: TMemo;
    //{$ENDIF USE_SYNEDIT}
    FDS: TZReadOnlyQuery;
    FSaveConnected: Boolean;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBoxRelationsDblClick(Sender: TObject);
    procedure ListBoxRelationsSelectionChange(Sender: TObject; User: boolean);
    procedure PageControl1Change(Sender: TObject);
    procedure ZeosROSQLEditorDestroy(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
    procedure mnuLoadClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure LoadTableList;
    procedure FillIdentifier;
    function ActiveEditor:TMemo;
    //function ActiveEditor:{$IFDEF USE_SYNEDIT} TSynEdit {$ELSE} TMemo {$ENDIF};
    //{$IFDEF USE_SYNEDIT}
    //{$IFDEF FPC}
    //procedure ccComplete(var Value: ansistring; Shift: TShiftState);
    //procedure ccExecute(Sender: TObject);
    //{$ELSE}
    //procedure ccComplete(Sender: TObject; var Value: string;
      //Shift: TShiftState; Index: Integer; EndToken: Char);
    //procedure ccExecute(Kind: SynCompletionType; Sender: TObject;
      //var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    //{$ENDIF FPC}
    //{$ENDIF USE_SYNEDIT}
  public
    PageControl1: TPageControl;
    constructor CreateEditor(ADS:TZReadOnlyQuery);
  end;

var
  ZeosROSQLEditorForm: TZROSQLEditorForm;

implementation
uses
  ZSqlTestForm, ZDbcIntfs, ZCompatibility;

const
  SQLDefaultFilter = 'SQL files (*.sql)|*.sql';
  
{ TZeosROSQLEditor }

constructor TZROSQLEditorForm.CreateEditor(ADS: TZReadOnlyQuery);
begin
  inherited CreateNew(Application,0);

  Height := 613;
  Left := 315;
  Top := 142;
  TextHeight := 13;
  Width := 768;
  Position := poDesktopCenter;
  Caption := sFormEditor;
  OnClose := FormClose;
  OnDestroy := ZeosROSQLEditorDestroy;
  btnOk := TBitBtn.Create(self);
  with btnOk do
  begin
    Parent := self;
    Anchors := [akRight, akBottom];
    Default := True;
    Height := 25;
    Kind := bkOk;
    Left := 593;
    ModalResult := mrOk;
    TabOrder := 3;
    TabStop := True;
    Top := 552;
    Width := 75;
    Caption := SButtonOk;
  end;
  btnCancel := TBitBtn.Create(self);
  with btnCancel do
  begin
    Parent := self;
    Anchors := [akRight, akBottom];
    Default := True;
    Height := 25;
    Kind := bkCancel;
    Left := 677;
    ModalResult := mrCancel;
    TabOrder := 4;
    TabStop := True;
    Top := 552;
    Width := 75;
    Caption := SButtonCancel;
  end;
  btnGenerate := TButton.Create(self);
  with btnGenerate do
  begin
    Parent := self;
    Anchors := [akLeft,akBottom];
    Caption := SButtonGenerate;
    Height := 25;
    Left := 8;
    OnClick := btnGenerateClick;
    TabOrder := 0;
    TabStop := True;
    Top := 552;
    Width := 75;
  end;
  btnCheck := TButton.Create(self);
  with btnCheck do
  begin
    Parent := self;
    Anchors := [akLeft,akBottom];
    Caption := SButtonCheck;
    Enabled := False;
    Height := 25;
    Left := 88;
    TabOrder := 1;
    TabStop := True;
    Top := 552;
    Visible := False;
    Width := 75;
  end;
  btnTest := TButton.Create(self);
  with btnTest do
  begin
    Parent := self;
    Anchors := [akLeft,akBottom];
    Caption := SButtonTest;
    Height := 25;
    Left := 168;
    OnClick := btnTestClick;
    TabOrder := 2;
    TabStop := True;
    Top := 552;
    Width := 75;
  end;
  dlgOpen := TOpenDialog.Create(self);
  with dlgOpen do
  begin
    FilterIndex :=0;
    Options := [ofEnableSizing,ofViewDetail];
    Title := SDialogOpenTitle;
    Filter := SQLDefaultFilter;
  end;
  dlgSave := TSaveDialog.Create(self);
  with dlgSave do
  begin
    FilterIndex :=0;
    Options := [ofEnableSizing,ofViewDetail];
    Title := SDialogSaveTitle;
    Filter := SQLDefaultFilter;
  end;
  mnuLoad := TMenuItem.Create(self);
  with mnuLoad do
  begin
    Caption := SMenuLoad;
    OnClick := mnuLoadClick;
  end;
  mnuSave := TMenuItem.Create(self);
  with mnuSave do
  begin
    Caption := SMenuSave;
    OnClick := mnuSaveClick;
  end;
  PopupMenu1 := TPopupMenu.Create(self);
  with PopupMenu1 do
  begin
    AutoPopup := true;
    Items.Add(mnuLoad);
    Items.Add(mnuSave);
  end;
  Panel2 := TPanel.Create(self);
  with Panel2 do
  begin
    Parent := self;
    Align := alTop;
    Anchors := [akTop,akLeft,akRight,akBottom];
    FullRepaint := False;
    Height := 545;
    Left := 0;
    TabOrder := 5;
    TabStop := False;
    Top := 0;
    Width := 768;
  end;
  Splitter1 := TSplitter.Create(self);
  with Splitter1 do
  begin
    Parent := Panel2;
    Align := alRight;
    Anchors := [akTop,akRight,akBottom];
    Height := 543;
    Left := 518;
    Top := 1;
    Width := 5;
  end;
  PageControl1 := TPageControl.Create(self);
  with PageControl1 do
  begin
    Parent := Panel2;
    Align := alClient;
    Anchors := [akTop,akLeft,akRight,akBottom];
    Height := 543;
    Left := 1;
    OnChange := PageControl1Change;
    TabIndex := 0;
    TabOrder := 0;
    TabPosition := tpTop;
    TabStop := True;
    Top := 1;
    Width := 517;
  end;
  tbsSelect := TTabSheet.Create(self);
  with tbsSelect do
  begin
    Parent := PageControl1;
    Caption := STabSheetSelect;
    Height := 513;
    Left := 2;
    Top := 28;
    Width := 513;
  end;
  Panel1 := TPanel.Create(self);
  with Panel1 do
  begin
    Parent := Panel2;
    Align := alRight;
    Anchors := [akTop,akRight,akBottom];
    FullRepaint := false;
    Height := 543;
    Left := 523;
    TabOrder := 1;
    TabStop := False;
    Top := 1;
    Width := 244;
  end;
  Splitter2 := TSplitter.Create(self);
  with Splitter2 do
  begin
    Parent := Panel1;
    Align := alBottom;
    Anchors := [akLeft,akRight,akBottom];
    Cursor := crVSplit;
    Height := 9;
    Left := 1;
    Top := 271;
    Width := 242;
  end;
  ListBoxRelations := TListBox.Create(self);
  with ListBoxRelations do
  begin
    Parent := Panel1;
    Align := alClient;
    Anchors := [akTop,akLeft,akRight,akBottom];
    Height := 198;
    ItemHeight := 13;
    Left := 1;
    OnDblClick := ListBoxRelationsDblClick;
    OnSelectionChange := ListBoxRelationsSelectionChange;
    TabOrder := 0;
    Top := 73;
    Width := 242
  end;
  ListBoxFields := TListBox.Create(self);
  with ListBoxFields do
  begin
    Parent := Panel1;
    Align := alBottom;
    Anchors := [akLeft,akRight,akBottom];
    Height := 262;
    ItemHeight := 13;
    Left := 1;
    MultiSelect := True;
    TabOrder := 1;
    Top := 280;
    Width := 242;
  end;
  Panel3 := TPanel.Create(self);
  with Panel3 do
  begin
    Parent := Panel1;
    Align := alTop;
    Anchors := [akTop,akLeft,akRight,akBottom];
    BevelOuter := bvNone;
    FullRepaint := False;
    Height := 72;
    Left := 1;
    TabOrder := 2;
    TabStop := False;
    Top := 1;
    Width := 242;
  end;
  edtAlias := TEdit.Create(self);
  with edtAlias do
  begin
    Parent := Panel3;
    Anchors := [akTop,akLeft];
    Height := 21;
    Left := 80;
    TabOrder := 0;
    Top := 8;
    Width := 153;
  end;
  lblAlias := TLabel.Create(self);
  with lblAlias do
  begin
    Parent := Panel3;
    Anchors := [akTop, akLeft];
    Caption := STableAlias;
    FocusControl := edtAlias;
    Height := 13;
    Left := 8;
    Top := 16;
    Width := 68;
  end;
  chkReplace := TCheckBox.Create(self);
  with chkReplace do
  begin
    Parent := Panel3;
    Anchors := [akTop,akLeft];
    Caption := SReplaceSQL;
    Checked := True;
    Height := 17;
    Left := 8;
    TabOrder := 1;
    TabStop := True;
    Top := 40;
    Width := 97;
  end;
  edtSelect := TMemo.Create(self);
  with edtSelect do
  begin
    Parent := tbsSelect;
    Align := alClient;
    Anchors := [akTop,akLeft,akRight,akBottom];
    Font.Height := -14;
    {$IFDEF WIN32}
    Font.Name := 'Courier New';
    {$ENDIF}
    Font.Pitch := fpFixed;
    Height := 513;
    Left := 0;
    PopupMenu := PopupMenu1;
    TabOrder := 0;
    TabStop := True;
    Top := 0;
    Width := 513;
  end;
  //{$IFDEF USE_SYNEDIT}
  //with SynSQLSyn1 do
  //begin
    //TableNameAttri.Background:=clWindow;
    //TableNameAttri.Foreground:=clGreen;
    //TableNameAttri.Style:=[fsUnderline];
    //with ADS.Connection do
      //if (strpos(PChar(Protocol),'firebird') <> nil) or
        //(strpos(PChar(Protocol),'interbase') <> nil) then
      //begin
        //SQLDialect:=sqlInterbase6;
      //end
      //else if Protocol = 'mssql' then
      //begin
        //SQLDialect:=sqlMSSQL2K;
      //end
      //else if (strpos(PChar(Protocol),'mysql') <> nil) then
      //begin
        //SQLDialect:=sqlMySQL;
      //end
      //else if (strpos(PChar(Protocol),'postgresql') <> nil) then
      //begin
        //SQLDialect:=sqlStandard;
      //end
      //else if Protocol = 'sybase' then
      //begin
        //SQLDialect:=sqlSybase;
      //end;
    //DefaultFilter:=SQLDefaultFilter;
  //end;
  //{$IFDEF FPC}
  //SynCompletion:=TSynCompletion.Create(Self);
  //{$ELSE}
  //SynCompletion:=TSynCompletionProposal.Create(Self);
  //{$ENDIF FPC}
  //with SynCompletion do begin
    //AddEditor(edtSelect);
    //ItemList.Clear;
    //OnCodeCompletion:=ccComplete;
    //OnExecute:=ccExecute;
  //end;
  //{$ENDIF USE_SYNEDIT}
  FSaveConnected := True;
  FDS:=ADS;
  if Assigned(FDS) then
  begin
    if not FDS.Connection.Connected then
    begin
      FSaveConnected := False;
      FDS.Connection.Connected := True;
    end;
    edtSelect.Lines.Text:=FDS.SQL.Text;
    LoadTableList;
    FillIdentifier;
  end;

end;

procedure TZROSQLEditorForm.FillIdentifier;
var
  i:integer;
begin
  //{$IFDEF USE_SYNEDIT}
  //SynSQLSyn1.TableNames.Clear;
  //for i:=0 to ListBoxRelations.Items.Count-1 do
  //begin
    //SynSQLSyn1.TableNames.Add(ListBoxRelations.Items[i]);
  //end;
  //{$ENDIF}
end;

procedure TZROSQLEditorForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if (ModalResult=mrOk) and Assigned(FDS) then
  begin
    FDS.SQL.Text:=edtSelect.Lines.Text;
  end;
end;

procedure TZROSQLEditorForm.LoadTableList;
var
  Metadata: IZDatabaseMetadata;
  TableTypes: TStringDynArray;
begin
  if Assigned(FDS.Connection) and FDS.Connection.Connected then
  begin
    Metadata := FDS.Connection.DbcConnection.GetMetadata;
    SetLength(TableTypes, 2);
    TableTypes[0] := 'TABLE';
    TableTypes[1] := 'VIEW';
    FDS.Connection.ShowSQLHourGlass;
    ListBoxRelations.Items.Clear;
    with Metadata.GetTables(FDS.Connection.Catalog, '', '', TableTypes) do
    try
      while Next do
        if ListBoxRelations.Items.IndexOf(trim(GetString(3))) = -1 then
          ListBoxRelations.Items.Add(trim(GetString(3)));
    finally
      Close;
    end;
    FDS.Connection.HideSQLHourGlass;
  end;
end;

function TZROSQLEditorForm.ActiveEditor:TMemo;
//function TZROSQLEditorForm.ActiveEditor:
  //{$IFDEF USE_SYNEDIT}TSynEdit{$ELSE}TMemo{$ENDIF};
begin
  case PageControl1.ActivePageIndex of
    0:Result:=edtSelect;
  end;
end;

procedure TZROSQLEditorForm.ListBoxRelationsDblClick(
  Sender: TObject);
var
  i:integer;
  Als, FieldsStr:string;
begin
  if ActiveEditor<>nil then
  with ActiveEditor do
  begin
    if chkReplace.Checked then Lines.Clear;
    if ListBoxFields.SelCount=0 then
    begin
      Lines.Add('select');
      Lines.Add('  *');
      Lines.Add('from');
      Lines.Add('  '+ListBoxRelations.Items[ListBoxRelations.ItemIndex]+' '+edtAlias.Text);
    end
    else
    begin
      Lines.Add('select');
      if edtAlias.Text<>'' then Als:=edtAlias.Text
      else Als:=ListBoxRelations.Items[ListBoxRelations.ItemIndex];
      FieldsStr:='';
      for i:=0 to ListBoxFields.Items.Count-1 do
      begin
        if ListBoxFields.Selected[i] then
        begin
         if FieldsStr<>'' then
           Lines.Add(FieldsStr+',');
         FieldsStr:='  '+Als+'.'+ListBoxFields.Items[i];
        end;
      end;
      if FieldsStr<>'' then Lines.Add(FieldsStr);
      Lines.Add('from');
      Lines.Add('  '+ListBoxRelations.Items[ListBoxRelations.ItemIndex]+' '+edtAlias.Text);
    end;
  end;
end;

procedure TZROSQLEditorForm.ListBoxRelationsSelectionChange(Sender: TObject;
  User: boolean);
var
  Metadata: IZDatabaseMetadata;
  TPName: string;
  StrEsc: string;
begin
  if Assigned(FDS.Connection) and FDS.Connection.Connected then
  begin
    FDS.Connection.ShowSQLHourGlass;
    Metadata := FDS.Connection.DbcConnection.GetMetadata;
    with ListBoxFields do begin
      ItemIndex := -1;
      Items.BeginUpdate;
      Items.Clear;
      Items.EndUpdate;
    end;
    if ListBoxRelations.ItemIndex >= 0 then
    begin
      TPName := ListBoxRelations.Items[ListBoxRelations.ItemIndex];
      with Metadata.GetColumns(FDS.Connection.Catalog,'', TPName,'') do
      try
        while Next do
          if ListBoxFields.Items.IndexOf(GetStringByName('COLUMN_NAME')) = -1 then
            ListBoxFields.Items.Add(GetStringByName('COLUMN_NAME'));
      finally
        Close;
      end;
    end;
    FDS.Connection.HideSQLHourGlass;
  end;
end;

procedure TZROSQLEditorForm.PageControl1Change(Sender: TObject);
begin
  case PageControl1.ActivePageIndex of
    0:edtSelect.SetFocus;
  end;
end;

procedure TZROSQLEditorForm.ZeosROSQLEditorDestroy(Sender: TObject);
begin
  if not FSaveConnected then
    FDS.Connection.Connected := False;
end;

procedure TZROSQLEditorForm.btnGenerateClick(Sender: TObject);
begin
  ListBoxRelationsDblClick(nil);
end;

procedure TZROSQLEditorForm.btnTestClick(Sender: TObject);
begin
  if Assigned(FDS.Connection) and FDS.Connection.Connected then
  begin
    ZeosSQLEditorTestForm:=TZeosSQLEditorTestForm.Create(Application);
    with ZeosSQLEditorTestForm do
    begin
      try
        ZeosSQL.Connection:=FDS.Connection;
        ZeosSQL.SQL.Text:=ActiveEditor.Lines.Text;
        ZeosSQL.Active := true;
        ShowModal
      except
        on E:Exception do
          ShowMessage(E.Message);
      end;
      Free;
    end;
  end;
end;

procedure TZROSQLEditorForm.mnuLoadClick(Sender: TObject);
begin
  if (dlgOpen.Execute) then
    ActiveEditor.Lines.LoadFromFile(dlgOpen.FileName);
end;

procedure TZROSQLEditorForm.mnuSaveClick(Sender: TObject);
begin
  if dlgSave.Execute then
    ActiveEditor.Lines.SaveToFile(dlgSave.FileName);
end;

//{$IFDEF USE_SYNEDIT}
//procedure TZROSQLEditorForm.ccComplete
//{$IFDEF FPC}
//(var Value: ansistring;
  //Shift: TShiftState)
//{$ELSE}
//(Sender: TObject; var Value: string;
      //Shift: TShiftState; Index: Integer; EndToken: Char)
//{$ENDIF}
//;
//begin
//end;

//procedure TZROSQLEditorForm.ccExecute
//{$IFDEF FPC}
//(Sender: TObject)
//{$ELSE}
//(Kind: SynCompletionType; Sender: TObject;
      //var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean)
//{$ENDIF}
//;
//function GetCurWord:string;
//var
  //S:string;
  //i,j:integer;
//begin
  //Result:='';
  //with ActiveEditor do
  //begin
    //S:=Trim(Copy(LineText, 1, CaretX));
    //I:=Length(S);
    //while (i>0) and (S[i]<>'.') do Dec(I);
    //if (I>0) then
    //begin
      //J:=i-1;
      ////Get table name
      //while (j>0) and (S[j] in ['A'..'z','"']) do Dec(j);
      //Result:=trim(Copy(S, j+1, i-j-1));
    //end;
  //end;
//end;
//var
  //S:string;
  //Metadata: IZDatabaseMetadata;
  //StrEsc: string;
//begin
  //S:=AnsiUpperCase(GetCurWord);
  //if S<>'' then
  //begin
    //if Assigned(FDS.Connection) and FDS.Connection.Connected then
    //begin
      //FDS.Connection.ShowSQLHourGlass;
      //Metadata := FDS.Connection.DbcConnection.GetMetadata;
      //SynCompletion.ItemList.Clear;
      //{$IFDEF FPC}
      //SynCompletion.OnPaintItem;
      //{$ELSE}
      //{$ENDIF}
      //with Metadata.GetColumns(FDS.Connection.Catalog,'',S,'') do
      //try
        //while Next do
        //if SynCompletion.ItemList.IndexOf(GetStringByName('COLUMN_NAME')) = -1 then
          //SynCompletion.ItemList.Add(GetStringByName('COLUMN_NAME'));
      //finally
        //Close;
      //end;
      //FDS.Connection.HideSQLHourGlass;
    //end;
  //end;
//end;
//{$ENDIF}

end.

