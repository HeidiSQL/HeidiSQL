
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntComCtrls_Design;

{$INCLUDE ..\Source\TntCompilers.inc}

interface

uses
  DesignIntf, DesignMenus, DesignEditors, Classes, ComCtrls;

type
  IPrepareMenuItem = IMenuItem;

  TTntListViewEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string{TNT-ALLOW string}; override;
    function GetVerbCount: Integer; override;
  end;

  TTntPageControlEditor = class(TDefaultEditor)
  private
    function PageControl: TPageControl{TNT-ALLOW TPageControl};
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string{TNT-ALLOW string}; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AItem: IPrepareMenuItem); override;
  end;

  TTntStatusBarEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string{TNT-ALLOW string}; override;
    function GetVerbCount: Integer; override;
  end;

  TTntToolBarEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string{TNT-ALLOW string}; override;
    function GetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  SysUtils, DsnConst, TntComCtrls, TntDesignEditors_Design;

procedure Register;
begin
  RegisterComponentEditor(TTntListView, TTntListViewEditor);
  RegisterComponentEditor(TTntPageControl, TTntPageControlEditor);
  RegisterComponentEditor(TTntTabSheet, TTntPageControlEditor);
  RegisterComponentEditor(TTntStatusBar, TTntStatusBarEditor);
  RegisterComponentEditor(TTntToolBar, TTntToolBarEditor);
  RegisterComponentEditor(TTntToolButton, TTntToolBarEditor);
end;

{ TTntListViewEditor }

function TTntListViewEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

function TTntListViewEditor.GetVerb(Index: Integer): string{TNT-ALLOW string};
begin
  case Index of
    0: Result := SListColumnsEditor;
    1: Result := SListItemsEditor;
  end;
end;

procedure TTntListViewEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: EditPropertyWithDialog(Component, 'Columns', Designer);
    1: EditPropertyWithDialog(Component, 'Items', Designer);
  end;
end;

{ TTntPageControlEditor }

function TTntPageControlEditor.PageControl: TPageControl{TNT-ALLOW TPageControl};
begin
  if Component is TTabSheet{TNT-ALLOW TTabSheet} then
    Result := TTabSheet{TNT-ALLOW TTabSheet}(Component).PageControl
  else
    Result := Component as TPageControl{TNT-ALLOW TPageControl};
end;

function TTntPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

function TTntPageControlEditor.GetVerb(Index: Integer): string{TNT-ALLOW string};
begin
  case Index of
    0: Result := SNewPage;
    1: Result := SNextPage;
    2: Result := SPrevPage;
    3: Result := SDeletePage;
  end;
end;

procedure TTntPageControlEditor.PrepareItem(Index: Integer; const AItem: IPrepareMenuItem);
begin
  AItem.Enabled := (Index <> 3) or (PageControl.PageCount > 0);
end;

type TAccessPageControl = class(TPageControl{TNT-ALLOW TPageControl});

procedure TTntPageControlEditor.ExecuteVerb(Index: Integer);

  procedure CreateNewTabSheet;
  var
    NewTabsheet: TTntTabSheet;
  begin
    NewTabSheet := TTntTabSheet.Create(PageControl.Owner);
      NewTabSheet.PageControl := Self.PageControl;
    with NewTabSheet do begin
      Name := Designer.UniqueName(ClassName);
      Caption := Name;
      Visible := True;
    end;
    PageControl.ActivePage := NewTabSheet;
  end;

begin
  case Index of
    0: CreateNewTabSheet;
    1: PageControl.SelectNextPage(True, False);
    2: PageControl.SelectNextPage(False, False);
    3: if PageControl.ActivePage <> nil then
         PageControl.ActivePage.Free;
  end;
end;

{ TTntStatusBarEditor }

function TTntStatusBarEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TTntStatusBarEditor.GetVerb(Index: Integer): string{TNT-ALLOW string};
begin
  case Index of
    0: Result := SStatusBarPanelEdit;
  end;
end;

procedure TTntStatusBarEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: EditPropertyWithDialog(Component, 'Panels', Designer);
  end;
end;

{ TTntToolBarEditor }

procedure TTntToolBarEditor.ExecuteVerb(Index: Integer);
var
  ToolBar: TTntToolBar;
  ToolButton: TTntToolButton;
  I, J: Integer;
  NewName: WideString;
begin
  Assert(Index in [0, 1]);

  if Component is TTntToolBar then
    ToolBar := TTntToolBar(Component)
  else if (Component is TTntToolButton) and (TTntToolButton(Component).Parent is TTntToolBar) then
    ToolBar := TTntToolBar(TTntToolButton(Component).Parent)
  else
    Exit;

  ToolButton := TTntToolButton.Create(Component.Owner);

  I := 1;
  repeat
    NewName := 'TntToolButton' + IntToStr(I);
    for J := 0 to ToolBar.ControlCount - 1 do
      if WideSameText(ToolBar.Controls[J].Name, NewName) then
        NewName := '';
    Inc(I);
  until NewName <> '';
  ToolButton.Name := NewName;

  if Index = 1 then begin
    ToolButton.Style := tbsSeparator;
    ToolButton.Width := 8;
  end;

  for I := 0 to ToolBar.ControlCount - 1 do
    ToolButton.Left := ToolButton.Left + ToolBar.Controls[I].Width;

  ToolButton.Parent := ToolBar;
end;

function TTntToolBarEditor.GetVerb(Index: Integer): string{TNT-ALLOW string};
begin
  case Index of
    0: Result := SNewToolButton;
    1: Result := SNewToolSeparator;
  end;
end;

function TTntToolBarEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

end.
