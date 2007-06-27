{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{         Standart Dbgrid with extra capabilities        }
{                                                        }
{       Copyright (c) 1999-2000 Sergey Seroukhov         }
{                                                        }
{********************************************************}

unit ZGrid;

interface

{$R *.DCR}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, ZFilterDlg, ZFindDlg, IniFiles, Menus, ZQuery, Db
  {$IFNDEF VER100}, ImgList {$ENDIF};

{$INCLUDE ..\Zeos.inc}

type
  { Zeos DbGrid }
  TZDBGrid = class(TDBGrid)
  private
    FMenu: TPopupMenu;
    FFilter: TZFilterDialog;
    FFind: TZFindDialog;
    FStandartDlg: Boolean;

    procedure OnFindClick(Sender: TObject);
    procedure OnIncludeClick(Sender: TObject);
    procedure OnExcludeClick(Sender: TObject);
    procedure OnSetFilterClick(Sender: TObject);
    procedure OnClearFilterClick(Sender: TObject);
    procedure OnSortAscClick(Sender: TObject);
    procedure OnSortDescClick(Sender: TObject);
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
   procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveColumns(IniFile: TIniFile; Section, Option: String);
    procedure LoadColumns(IniFile: TIniFile; Section, Option: String);
  published
    property StandartDlg: Boolean read FStandartDlg write FStandartDlg default true;
    property ParentCtl3D;
    property Ctl3D;
  end;

implementation

uses ZDbCtrlsConst;

{*************** TZDBGrid implementation *****************}

var
  AImageList: TImageList;

{ Initiate local imageList }
procedure InitiateImages; forward;

{ Class constructor }
constructor TZDBGrid.Create(AOwner: TComponent);
var
  MenuItem: TMenuItem;
begin
  inherited Create(AOwner);

  FStandartDlg := True;
  FFilter := TZFilterDialog.Create(Parent);
  FFind := TZFindDialog.Create(Parent);

  FMenu := TPopupMenu.Create(Parent);
  if not Assigned(AImageList) then
    InitiateImages;
{$IFNDEF VER100}
  FMenu.Images := AImageList;
{$ENDIF}

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SOperations;
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := '-';
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SFindField;
  MenuItem.OnClick := OnFindClick;
{$IFNDEF VER100}
  MenuItem.ImageIndex := 0;
{$ENDIF}
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := '-';
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SIncludeField;
  MenuItem.OnClick := OnIncludeClick;
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SExcludeField;
  MenuItem.OnClick := OnExcludeClick;
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SSetFilter;
  MenuItem.OnClick := OnSetFilterClick;
{$IFNDEF VER100}
  MenuItem.ImageIndex := 1;
{$ENDIF}
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SDropFilter;
  MenuItem.OnClick := OnClearFilterClick;
{$IFNDEF VER100}
  MenuItem.ImageIndex := 2;
{$ENDIF}
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := '-';
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SSortAsc;
  MenuItem.OnClick := OnSortAscClick;
{$IFNDEF VER100}
  MenuItem.ImageIndex := 3;
{$ENDIF}
  FMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(FMenu);
  MenuItem.Caption := SSortDesc;
  MenuItem.OnClick := OnSortDescClick;
{$IFNDEF VER100}
  MenuItem.ImageIndex := 4;
{$ENDIF}
  FMenu.Items.Add(MenuItem);
end;

{ Class destructor }
destructor TZDBGrid.Destroy;
begin
  FMenu.Free;
  FFilter.Free;
  FFind.Free;
  inherited Destroy;
end;

{ Find field }
procedure TZDBGrid.OnFindClick(Sender: TObject);
begin
  FFind.Dataset   := DataSource.Dataset as TZDataset;
  FFind.DataField := SelectedField.FieldName;
  FFind.Execute;
end;

{ Include field }
procedure TZDBGrid.OnIncludeClick(Sender: TObject);
begin
  FFilter.Dataset := DataSource.Dataset as TZDataset;
  FFilter.SetIncludeFilter(SelectedField.FieldName);
end;

{ Exclude field }
procedure TZDBGrid.OnExcludeClick(Sender: TObject);
begin
  FFilter.Dataset := DataSource.Dataset as TZDataset;
  FFilter.SetExcludeFilter(SelectedField.FieldName);
end;

{ Set filter }
procedure TZDBGrid.OnSetFilterClick(Sender: TObject);
begin
  FFilter.Dataset := DataSource.Dataset as TZDataset;
  FFilter.Execute;
end;

{ Drop filter }
procedure TZDBGrid.OnClearFilterClick(Sender: TObject);
begin
  FFilter.Dataset := DataSource.Dataset as TZDataset;
  FFilter.ClearFilter;
end;

{ Ascending sort }
procedure TZDBGrid.OnSortAscClick(Sender: TObject);
begin
  FFilter.Dataset := DataSource.Dataset as TZDataset;
  FFilter.Sort(SelectedField.FieldName);
end;

{ Descending sort }
procedure TZDBGrid.OnSortDescClick(Sender: TObject);
begin
  FFilter.Dataset := DataSource.Dataset as TZDataset;
  FFilter.SortDesc(SelectedField.FieldName);
end;

{ On key up event }
procedure TZDBGrid.KeyUp(var Key: Word; Shift: TShiftState);
begin
{ Ctrl-F }
  if (Shift = [ssCtrl]) and (Key = 70) and
    Assigned(SelectedField) and Assigned(DataSource) then
  begin
    FFind.Dataset   := DataSource.Dataset as TZDataset;
    FFind.DataField := SelectedField.FieldName;
    FFind.Execute;
  end;

{ Ctrl-S }
  if (Shift = [ssCtrl]) and (Key = 83) and
    Assigned(DataSource) then
  begin
    FFilter.Dataset := DataSource.Dataset as TZDataset;
    FFilter.Execute;
  end;

  inherited KeyUp(Key, Shift);
end;

{ On exit grid event }
procedure TZDBGrid.DoExit;
begin
//  if DataSource.Dataset.State in [dsInsert, dsEdit] then
//    DataSource.Dataset.Post;
  inherited DoExit;
end;

{ On mouse release button }
procedure TZDBGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  Pnt: TPoint;
  MenuItem: TMenuItem;
begin
  if {not Assigned(PopupMenu) and} (Button=mbRight) and FStandartDlg then begin
    if Assigned(PopupMenu) then
    begin
      FMenu.Items[0].Visible := True;
      FMenu.Items[1].Visible := True;
      for I := FMenu.Items[0].Count-1 downto 0 do
        FMenu.Items[0].Delete(I);
      for I := 0 to PopupMenu.Items.Count-1 do
      begin
        MenuItem := PopupMenu.Items[0];
        PopupMenu.Items.Delete(0);
        FMenu.Items[0].Add(MenuItem);
      end;
    end
    else
    begin
      FMenu.Items[0].Visible := False;
      FMenu.Items[1].Visible := False;
    end;
    Pnt := ClientToScreen(Point(X,Y));
    FMenu.Popup(Pnt.X, Pnt.Y);
    if Assigned(PopupMenu) then
      for I := 0 to FMenu.Items[0].Count-1 do
      begin
        MenuItem := FMenu.Items[0].Items[0];
        FMenu.Items[0].Delete(0);
        PopupMenu.Items.Add(MenuItem);
      end;
    Abort;
  end else
    inherited MouseUp(Button, Shift, X, Y);
end;

{ Save grid column sizes }
procedure TZDBGrid.SaveColumns(IniFile: TIniFile; Section, Option: String);
var
  I: Integer;
  Parent: TComponent;
begin
  if not Assigned(IniFile) then Exit;
  if Section = '' then
  begin
    Parent := Owner;
    while Assigned(Parent.Owner) and (not (Parent is TForm)) do
      Parent := Parent.Owner;
    Section := Parent.Name;
  end;
  if Option = '' then Option := Name;

  for I := 0 to Columns.Count-1 do
    IniFile.WriteInteger(Section,Option+'ColumnWidth'+IntToStr(I), Columns.Items[I].Width);
end;

{ Load grid column sizes }
procedure TZDBGrid.LoadColumns(IniFile: TIniFile; Section, Option: String);
var
  I, W: Integer;
  Parent: TComponent;
begin
  if not Assigned(IniFile) then Exit;
  if Section = '' then
  begin
    Parent := Owner;
    while Assigned(Parent.Owner) and (not (Parent is TForm)) do
      Parent := Parent.Owner;
    Section := Parent.Name;
  end;
  if Option = '' then Option := Name;

  for I := 0 to Columns.Count-1 do
  begin
    W := IniFile.ReadInteger(Section,Option+'ColumnWidth'+IntToStr(I),0);
    if W <> 0 then
      Columns.Items[I].Width := W;
  end;
end;

{*********************************************************}

{ Initiate local imageList }
procedure InitiateImages;
var
  Bitmap: TBitmap;
begin
  AImageList := TImageList.Create(NIL);

  Bitmap := TBitmap.Create;
  Bitmap.LoadFromResourceName(HInstance,'IDB_FIND');
  AImageList.ResourceLoad(rtBitmap,'IDB_FIND',Bitmap.Canvas.Pixels[15,15]);
  Bitmap.LoadFromResourceName(HInstance,'IDB_FILTERSET');
  AImageList.ResourceLoad(rtBitmap,'IDB_FILTERSET',Bitmap.Canvas.Pixels[15,15]);
  Bitmap.LoadFromResourceName(HInstance,'IDB_FILTERCLEAR');
  AImageList.ResourceLoad(rtBitmap,'IDB_FILTERCLEAR',Bitmap.Canvas.Pixels[15,15]);
  Bitmap.LoadFromResourceName(HInstance,'IDB_SORTASC');
  AImageList.ResourceLoad(rtBitmap,'IDB_SORTASC',Bitmap.Canvas.Pixels[15,15]);
  Bitmap.LoadFromResourceName(HInstance,'IDB_SORTDESC');
  AImageList.ResourceLoad(rtBitmap,'IDB_SORTDESC',Bitmap.Canvas.Pixels[15,15]);
  Bitmap.Free;
end;

initialization
finalization
  if Assigned(AImageList) then
    AImageList.Free;
end.
