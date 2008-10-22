
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntStdActns;

{$INCLUDE compilers.inc}

interface

uses
  Classes, ActnList, TntActnList, StdActns, TntDialogs;

type
{TNT-WARN THintAction}
  TTntHintAction = class(THintAction{TNT-ALLOW THintAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  public
    property Caption: WideString read GetCaption write SetCaption;
  published
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TEditAction}
  TTntEditAction = class(TEditAction{TNT-ALLOW TEditAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TEditCut}
  TTntEditCut = class(TEditCut{TNT-ALLOW TEditCut}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TEditCopy}
  TTntEditCopy = class(TEditCopy{TNT-ALLOW TEditCopy}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TEditPaste}
  TTntEditPaste = class(TEditPaste{TNT-ALLOW TEditPaste}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TEditSelectAll}
  TTntEditSelectAll = class(TEditSelectAll{TNT-ALLOW TEditSelectAll}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TEditUndo}
  TTntEditUndo = class(TEditUndo{TNT-ALLOW TEditUndo}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TEditDelete}
  TTntEditDelete = class(TEditDelete{TNT-ALLOW TEditDelete}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TWindowAction}
  TTntWindowAction = class(TWindowAction{TNT-ALLOW TWindowAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TWindowClose}
  TTntWindowClose = class(TWindowClose{TNT-ALLOW TWindowClose}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TWindowCascade}
  TTntWindowCascade = class(TWindowCascade{TNT-ALLOW TWindowCascade}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TWindowTileHorizontal}
  TTntWindowTileHorizontal = class(TWindowTileHorizontal{TNT-ALLOW TWindowTileHorizontal}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TWindowTileVertical}
  TTntWindowTileVertical = class(TWindowTileVertical{TNT-ALLOW TWindowTileVertical}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TWindowMinimizeAll}
  TTntWindowMinimizeAll = class(TWindowMinimizeAll{TNT-ALLOW TWindowMinimizeAll}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TWindowArrange}
  TTntWindowArrange = class(TWindowArrange{TNT-ALLOW TWindowArrange}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN THelpAction}
  TTntHelpAction = class(THelpAction{TNT-ALLOW THelpAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN THelpContents}
  TTntHelpContents = class(THelpContents{TNT-ALLOW THelpContents}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN THelpTopicSearch}
  TTntHelpTopicSearch = class(THelpTopicSearch{TNT-ALLOW THelpTopicSearch}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN THelpOnHelp}
  TTntHelpOnHelp = class(THelpOnHelp{TNT-ALLOW THelpOnHelp}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN THelpContextAction}
  TTntHelpContextAction = class(THelpContextAction{TNT-ALLOW THelpContextAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TCommonDialogAction}
  TTntCommonDialogAction = class(TCommonDialogAction{TNT-ALLOW TCommonDialogAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  public
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TFileAction}
  TTntFileAction = class(TFileAction{TNT-ALLOW TFileAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  public
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TFileOpen}
  TTntFileOpen = class(TFileOpen{TNT-ALLOW TFileOpen}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function GetDialog: TTntOpenDialog;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDialogClass: TCommonDialogClass; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Dialog: TTntOpenDialog read GetDialog;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TFileOpenWith}
  TTntFileOpenWith = class(TFileOpenWith{TNT-ALLOW TFileOpenWith}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function GetDialog: TTntOpenDialog;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDialogClass: TCommonDialogClass; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Dialog: TTntOpenDialog read GetDialog;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TFileSaveAs}
  TTntFileSaveAs = class(TFileSaveAs{TNT-ALLOW TFileSaveAs}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
    function GetDialog: TTntSaveDialog;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDialogClass: TCommonDialogClass; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Dialog: TTntSaveDialog read GetDialog;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TFilePrintSetup}
  TTntFilePrintSetup = class(TFilePrintSetup{TNT-ALLOW TFilePrintSetup}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

  {$IFDEF COMPILER_7_UP}
{TNT-WARN TFilePageSetup}
  TTntFilePageSetup = class(TFilePageSetup{TNT-ALLOW TFilePageSetup}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;
  {$ENDIF}

{TNT-WARN TFileExit}
  TTntFileExit = class(TFileExit{TNT-ALLOW TFileExit}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TSearchAction}
  TTntSearchAction = class(TSearchAction{TNT-ALLOW TSearchAction}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  public
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TSearchFind}
  TTntSearchFind = class(TSearchFind{TNT-ALLOW TSearchFind}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TSearchReplace}
  TTntSearchReplace = class(TSearchReplace{TNT-ALLOW TSearchReplace}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TSearchFindFirst}
  TTntSearchFindFirst = class(TSearchFindFirst{TNT-ALLOW TSearchFindFirst}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TSearchFindNext}
  TTntSearchFindNext = class(TSearchFindNext{TNT-ALLOW TSearchFindNext}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TFontEdit}
  TTntFontEdit = class(TFontEdit{TNT-ALLOW TFontEdit}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TColorSelect}
  TTntColorSelect = class(TColorSelect{TNT-ALLOW TColorSelect}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

{TNT-WARN TPrintDlg}
  TTntPrintDlg = class(TPrintDlg{TNT-ALLOW TPrintDlg}, ITntAction)
  private
    function GetCaption: WideString;
    procedure SetCaption(const Value: WideString);
    function GetHint: WideString;
    procedure SetHint(const Value: WideString);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: WideString read GetCaption write SetCaption;
    property Hint: WideString read GetHint write SetHint;
  end;

procedure TntStdActn_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);

implementation

uses
  Dialogs, TntClasses;

{TNT-IGNORE-UNIT}

procedure TntStdActn_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);
begin
  TntAction_AfterInherited_Assign(Action, Source);
  // TCommonDialogAction
  if (Action is TCommonDialogAction) and (Source is TCommonDialogAction) then begin
    TCommonDialogAction(Action).BeforeExecute := TCommonDialogAction(Source).BeforeExecute;
    TCommonDialogAction(Action).OnAccept      := TCommonDialogAction(Source).OnAccept;
    TCommonDialogAction(Action).OnCancel      := TCommonDialogAction(Source).OnCancel;
  end;
  // TFileOpen
  if (Action is TFileOpen) and (Source is TFileOpen) then begin
    {$IFDEF COMPILER_7_UP}
    TFileOpen(Action).UseDefaultApp := TFileOpen(Source).UseDefaultApp;
    {$ENDIF}
  end;
  // TFileOpenWith
  if (Action is TFileOpenWith) and (Source is TFileOpenWith) then begin
    TFileOpenWith(Action).FileName  := TFileOpenWith(Source).FileName;
    {$IFDEF COMPILER_7_UP}
    TFileOpenWith(Action).AfterOpen := TFileOpenWith(Source).AfterOpen;
    {$ENDIF}
  end;
  // TSearchFindNext
  if (Action is TSearchFindNext) and (Source is TSearchFindNext) then begin
    TSearchFindNext(Action).SearchFind := TSearchFindNext(Source).SearchFind;
  end;
end;

//-------------------------
//    TNT STD ACTNS
//-------------------------

{ TTntHintAction }

procedure TTntHintAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntHintAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntHintAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntHintAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntHintAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntHintAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntEditAction }

procedure TTntEditAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntEditAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntEditAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntEditAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntEditAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntEditAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntEditCut }

procedure TTntEditCut.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntEditCut.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntEditCut.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntEditCut.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntEditCut.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntEditCut.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntEditCopy }

procedure TTntEditCopy.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntEditCopy.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntEditCopy.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntEditCopy.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntEditCopy.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntEditCopy.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntEditPaste }

procedure TTntEditPaste.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntEditPaste.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntEditPaste.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntEditPaste.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntEditPaste.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntEditPaste.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntEditSelectAll }

procedure TTntEditSelectAll.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntEditSelectAll.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntEditSelectAll.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntEditSelectAll.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntEditSelectAll.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntEditSelectAll.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntEditUndo }

procedure TTntEditUndo.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntEditUndo.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntEditUndo.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntEditUndo.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntEditUndo.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntEditUndo.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntEditDelete }

procedure TTntEditDelete.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntEditDelete.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntEditDelete.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntEditDelete.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntEditDelete.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntEditDelete.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

procedure TTntEditDelete.UpdateTarget(Target: TObject);
begin
  Enabled := True;
end;

procedure TTntEditDelete.ExecuteTarget(Target: TObject);
begin
  if GetControl(Target).SelLength = 0 then
    GetControl(Target).SelLength := 1;
  GetControl(Target).ClearSelection
end;

{ TTntWindowAction }

procedure TTntWindowAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntWindowAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntWindowAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntWindowAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntWindowAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntWindowAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntWindowClose }

procedure TTntWindowClose.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntWindowClose.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntWindowClose.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntWindowClose.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntWindowClose.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntWindowClose.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntWindowCascade }

procedure TTntWindowCascade.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntWindowCascade.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntWindowCascade.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntWindowCascade.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntWindowCascade.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntWindowCascade.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntWindowTileHorizontal }

procedure TTntWindowTileHorizontal.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntWindowTileHorizontal.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntWindowTileHorizontal.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntWindowTileHorizontal.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntWindowTileHorizontal.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntWindowTileHorizontal.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntWindowTileVertical }

procedure TTntWindowTileVertical.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntWindowTileVertical.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntWindowTileVertical.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntWindowTileVertical.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntWindowTileVertical.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntWindowTileVertical.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntWindowMinimizeAll }

procedure TTntWindowMinimizeAll.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntWindowMinimizeAll.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntWindowMinimizeAll.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntWindowMinimizeAll.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntWindowMinimizeAll.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntWindowMinimizeAll.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntWindowArrange }

procedure TTntWindowArrange.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntWindowArrange.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntWindowArrange.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntWindowArrange.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntWindowArrange.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntWindowArrange.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntHelpAction }

procedure TTntHelpAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntHelpAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntHelpAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntHelpAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntHelpAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntHelpAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntHelpContents }

procedure TTntHelpContents.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntHelpContents.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntHelpContents.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntHelpContents.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntHelpContents.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntHelpContents.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntHelpTopicSearch }

procedure TTntHelpTopicSearch.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntHelpTopicSearch.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntHelpTopicSearch.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntHelpTopicSearch.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntHelpTopicSearch.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntHelpTopicSearch.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntHelpOnHelp }

procedure TTntHelpOnHelp.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntHelpOnHelp.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntHelpOnHelp.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntHelpOnHelp.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntHelpOnHelp.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntHelpOnHelp.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntHelpContextAction }

procedure TTntHelpContextAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntHelpContextAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntHelpContextAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntHelpContextAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntHelpContextAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntHelpContextAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntCommonDialogAction }

procedure TTntCommonDialogAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntCommonDialogAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCommonDialogAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntCommonDialogAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntCommonDialogAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntCommonDialogAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntFileAction }

procedure TTntFileAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFileAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFileAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFileAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFileAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFileAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntFileOpen }

procedure TTntFileOpen.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFileOpen.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFileOpen.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFileOpen.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFileOpen.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFileOpen.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

function TTntFileOpen.GetDialog: TTntOpenDialog;
begin
  Result := inherited Dialog as TTntOpenDialog;
end;

function TTntFileOpen.GetDialogClass: TCommonDialogClass;
begin
  Result := TTntOpenDialog;
end;

{ TTntFileOpenWith }

procedure TTntFileOpenWith.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFileOpenWith.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFileOpenWith.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFileOpenWith.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFileOpenWith.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFileOpenWith.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

function TTntFileOpenWith.GetDialog: TTntOpenDialog;
begin
  Result := inherited Dialog as TTntOpenDialog;
end;

function TTntFileOpenWith.GetDialogClass: TCommonDialogClass;
begin
  Result := TTntOpenDialog;
end;

{ TTntFileSaveAs }

procedure TTntFileSaveAs.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFileSaveAs.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFileSaveAs.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFileSaveAs.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFileSaveAs.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFileSaveAs.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

function TTntFileSaveAs.GetDialog: TTntSaveDialog;
begin
  Result := TOpenDialog(inherited Dialog) as TTntSaveDialog;
end;

function TTntFileSaveAs.GetDialogClass: TCommonDialogClass;
begin
  Result := TTntSaveDialog;
end;

{ TTntFilePrintSetup }

procedure TTntFilePrintSetup.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFilePrintSetup.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFilePrintSetup.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFilePrintSetup.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFilePrintSetup.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFilePrintSetup.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

 {$IFDEF COMPILER_7_UP}

{ TTntFilePageSetup }

procedure TTntFilePageSetup.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFilePageSetup.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFilePageSetup.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFilePageSetup.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFilePageSetup.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFilePageSetup.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;
 {$ENDIF}

{ TTntFileExit }

procedure TTntFileExit.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFileExit.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFileExit.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFileExit.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFileExit.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFileExit.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntSearchAction }

procedure TTntSearchAction.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntSearchAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSearchAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntSearchAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntSearchAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntSearchAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntSearchFind }

procedure TTntSearchFind.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntSearchFind.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSearchFind.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntSearchFind.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntSearchFind.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntSearchFind.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntSearchReplace }

procedure TTntSearchReplace.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntSearchReplace.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSearchReplace.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntSearchReplace.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntSearchReplace.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntSearchReplace.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntSearchFindFirst }

procedure TTntSearchFindFirst.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntSearchFindFirst.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSearchFindFirst.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntSearchFindFirst.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntSearchFindFirst.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntSearchFindFirst.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntSearchFindNext }

procedure TTntSearchFindNext.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntSearchFindNext.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSearchFindNext.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntSearchFindNext.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntSearchFindNext.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntSearchFindNext.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntFontEdit }

procedure TTntFontEdit.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFontEdit.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFontEdit.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFontEdit.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFontEdit.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFontEdit.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntColorSelect }

procedure TTntColorSelect.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntColorSelect.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntColorSelect.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntColorSelect.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntColorSelect.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntColorSelect.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntPrintDlg }

procedure TTntPrintDlg.Assign(Source: TPersistent);
begin
  inherited;
  TntStdActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntPrintDlg.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntPrintDlg.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntPrintDlg.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntPrintDlg.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntPrintDlg.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

end.
