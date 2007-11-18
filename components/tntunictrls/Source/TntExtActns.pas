
{*****************************************************************************}
{                                                                             }
{    Tnt Delphi Unicode Controls                                              }
{      http://www.tntware.com/delphicontrols/unicode/                         }
{        Version: 2.3.0                                                       }
{                                                                             }
{    Copyright (c) 2002-2007, Troy Wolbrink (troy.wolbrink@tntware.com)       }
{                                                                             }
{*****************************************************************************}

unit TntExtActns;

{$INCLUDE TntCompilers.inc}

interface

uses
  Classes, TntActnList, ExtActns;

type
{TNT-WARN TCustomFileRun}
  TTntCustomFileRun = class(TCustomFileRun{TNT-ALLOW TCustomFileRun}, ITntAction)
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

{TNT-WARN TFileRun}
  TTntFileRun = class(TFileRun{TNT-ALLOW TFileRun}, ITntAction)
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

{TNT-WARN TRichEditAction}
  TTntRichEditAction = class(TRichEditAction{TNT-ALLOW TRichEditAction}, ITntAction)
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

{TNT-WARN TRichEditBold}
  TTntRichEditBold = class(TRichEditBold{TNT-ALLOW TRichEditBold}, ITntAction)
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

{TNT-WARN TRichEditItalic}
  TTntRichEditItalic = class(TRichEditItalic{TNT-ALLOW TRichEditItalic}, ITntAction)
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

{TNT-WARN TRichEditUnderline}
  TTntRichEditUnderline = class(TRichEditUnderline{TNT-ALLOW TRichEditUnderline}, ITntAction)
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

{TNT-WARN TRichEditStrikeOut}
  TTntRichEditStrikeOut = class(TRichEditStrikeOut{TNT-ALLOW TRichEditStrikeOut}, ITntAction)
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

{TNT-WARN TRichEditBullets}
  TTntRichEditBullets = class(TRichEditBullets{TNT-ALLOW TRichEditBullets}, ITntAction)
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

{TNT-WARN TRichEditAlignLeft}
  TTntRichEditAlignLeft = class(TRichEditAlignLeft{TNT-ALLOW TRichEditAlignLeft}, ITntAction)
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

{TNT-WARN TRichEditAlignRight}
  TTntRichEditAlignRight = class(TRichEditAlignRight{TNT-ALLOW TRichEditAlignRight}, ITntAction)
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

{TNT-WARN TRichEditAlignCenter}
  TTntRichEditAlignCenter = class(TRichEditAlignCenter{TNT-ALLOW TRichEditAlignCenter}, ITntAction)
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

{TNT-WARN TTabAction}
  TTntTabAction = class(TTabAction{TNT-ALLOW TTabAction}, ITntAction)
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

{TNT-WARN TPreviousTab}
  TTntPreviousTab = class(TPreviousTab{TNT-ALLOW TPreviousTab}, ITntAction)
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

{TNT-WARN TNextTab}
  TTntNextTab = class(TNextTab{TNT-ALLOW TNextTab}, ITntAction)
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

{TNT-WARN TOpenPicture}
  TTntOpenPicture = class(TOpenPicture{TNT-ALLOW TOpenPicture}, ITntAction)
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

{TNT-WARN TSavePicture}
  TTntSavePicture = class(TSavePicture{TNT-ALLOW TSavePicture}, ITntAction)
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

{TNT-WARN TURLAction}
  TTntURLAction = class(TURLAction{TNT-ALLOW TURLAction}, ITntAction)
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

{TNT-WARN TBrowseURL}
  TTntBrowseURL = class(TBrowseURL{TNT-ALLOW TBrowseURL}, ITntAction)
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

{TNT-WARN TDownLoadURL}
  TTntDownLoadURL = class(TDownLoadURL{TNT-ALLOW TDownLoadURL}, ITntAction)
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

{TNT-WARN TSendMail}
  TTntSendMail = class(TSendMail{TNT-ALLOW TSendMail}, ITntAction)
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

{TNT-WARN TListControlAction}
  TTntListControlAction = class(TListControlAction{TNT-ALLOW TListControlAction}, ITntAction)
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

{TNT-WARN TListControlCopySelection}
  TTntListControlCopySelection = class(TListControlCopySelection{TNT-ALLOW TListControlCopySelection}, ITntAction)
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

{TNT-WARN TListControlDeleteSelection}
  TTntListControlDeleteSelection = class(TListControlDeleteSelection{TNT-ALLOW TListControlDeleteSelection}, ITntAction)
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

{TNT-WARN TListControlSelectAll}
  TTntListControlSelectAll = class(TListControlSelectAll{TNT-ALLOW TListControlSelectAll}, ITntAction)
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

{TNT-WARN TListControlClearSelection}
  TTntListControlClearSelection = class(TListControlClearSelection{TNT-ALLOW TListControlClearSelection}, ITntAction)
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

{TNT-WARN TListControlMoveSelection}
  TTntListControlMoveSelection = class(TListControlMoveSelection{TNT-ALLOW TListControlMoveSelection}, ITntAction)
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

implementation

uses
  ActnList, TntStdActns, TntClasses;

{TNT-IGNORE-UNIT}

procedure TntExtActn_AfterInherited_Assign(Action: TCustomAction{TNT-ALLOW TCustomAction}; Source: TPersistent);
begin
  TntStdActn_AfterInherited_Assign(Action, Source);
  // TCustomFileRun
  if (Action is TCustomFileRun) and (Source is TCustomFileRun) then begin
    TCustomFileRun(Action).Browse        := TCustomFileRun(Source).Browse;
    if TCustomFileRun(Source).BrowseDlg.Owner <> Source then
      TCustomFileRun(Action).BrowseDlg := TCustomFileRun(Source).BrowseDlg
    else begin
      { Carry over dialog properties.  Currently TOpenDialog doesn't support Assign. }
      { TCustomFileRun(Action).BrowseDlg.Assign(TCustomFileRun(Source).BrowseDlg); }
    end;
    TCustomFileRun(Action).Directory     := TCustomFileRun(Source).Directory;
    TCustomFileRun(Action).FileName      := TCustomFileRun(Source).FileName;
    TCustomFileRun(Action).Operation     := TCustomFileRun(Source).Operation;
    TCustomFileRun(Action).ParentControl := TCustomFileRun(Source).ParentControl;
    TCustomFileRun(Action).Parameters    := TCustomFileRun(Source).Parameters;
    TCustomFileRun(Action).ShowCmd       := TCustomFileRun(Source).ShowCmd;
  end;
  // TTabAction
  if (Action is TTabAction) and (Source is TTabAction) then begin
    TTabAction(Action).SkipHiddenTabs  := TTabAction(Source).SkipHiddenTabs;
    TTabAction(Action).TabControl      := TTabAction(Source).TabControl;
    TTabAction(Action).Wrap            := TTabAction(Source).Wrap;
    TTabAction(Action).BeforeTabChange := TTabAction(Source).BeforeTabChange;
    TTabAction(Action).AfterTabChange  := TTabAction(Source).AfterTabChange;
    TTabAction(Action).OnValidateTab   := TTabAction(Source).OnValidateTab;
  end;
  // TNextTab
  if (Action is TNextTab) and (Source is TNextTab) then begin
    TNextTab(Action).LastTabCaption := TNextTab(Source).LastTabCaption;
    TNextTab(Action).OnFinish       := TNextTab(Source).OnFinish;
  end;
  // TURLAction
  if (Action is TURLAction) and (Source is TURLAction) then begin
    TURLAction(Action).URL := TURLAction(Source).URL;
  end;
  // TBrowseURL
  if (Action is TBrowseURL) and (Source is TBrowseURL) then begin
    {$IFDEF COMPILER_7_UP}
    TBrowseURL(Action).BeforeBrowse := TBrowseURL(Source).BeforeBrowse;
    TBrowseURL(Action).AfterBrowse  := TBrowseURL(Source).AfterBrowse;
    {$ENDIF}
  end;
  // TDownloadURL
  if (Action is TDownloadURL) and (Source is TDownloadURL) then begin
    TDownloadURL(Action).FileName           := TDownloadURL(Source).FileName;
    {$IFDEF COMPILER_7_UP}
    TDownloadURL(Action).BeforeDownload     := TDownloadURL(Source).BeforeDownload;
    TDownloadURL(Action).AfterDownload      := TDownloadURL(Source).AfterDownload;
    {$ENDIF}
    TDownloadURL(Action).OnDownloadProgress := TDownloadURL(Source).OnDownloadProgress;
  end;
  // TSendMail
  if (Action is TSendMail) and (Source is TSendMail) then begin
    TSendMail(Action).Text := TSendMail(Source).Text;
  end;
  // TListControlAction
  if (Action is TListControlAction) and (Source is TListControlAction) then begin
    TListControlAction(Action).ListControl := TListControlAction(Source).ListControl;
  end;
  // TListControlCopySelection
  if (Action is TListControlCopySelection) and (Source is TListControlCopySelection) then begin
    TListControlCopySelection(Action).Destination := TListControlCopySelection(Source).Destination;
  end;
end;

//-------------------------
//    TNT EXT ACTNS
//-------------------------

{ TTntCustomFileRun }

procedure TTntCustomFileRun.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntCustomFileRun.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntCustomFileRun.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntCustomFileRun.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntCustomFileRun.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntCustomFileRun.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntFileRun }

procedure TTntFileRun.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntFileRun.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntFileRun.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntFileRun.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntFileRun.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntFileRun.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditAction }

procedure TTntRichEditAction.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditBold }

procedure TTntRichEditBold.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditBold.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditBold.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditBold.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditBold.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditBold.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditItalic }

procedure TTntRichEditItalic.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditItalic.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditItalic.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditItalic.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditItalic.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditItalic.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditUnderline }

procedure TTntRichEditUnderline.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditUnderline.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditUnderline.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditUnderline.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditUnderline.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditUnderline.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditStrikeOut }

procedure TTntRichEditStrikeOut.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditStrikeOut.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditStrikeOut.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditStrikeOut.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditStrikeOut.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditStrikeOut.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditBullets }

procedure TTntRichEditBullets.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditBullets.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditBullets.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditBullets.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditBullets.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditBullets.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditAlignLeft }

procedure TTntRichEditAlignLeft.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditAlignLeft.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditAlignLeft.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditAlignLeft.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditAlignLeft.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditAlignLeft.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditAlignRight }

procedure TTntRichEditAlignRight.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditAlignRight.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditAlignRight.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditAlignRight.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditAlignRight.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditAlignRight.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntRichEditAlignCenter }

procedure TTntRichEditAlignCenter.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntRichEditAlignCenter.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntRichEditAlignCenter.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntRichEditAlignCenter.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntRichEditAlignCenter.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntRichEditAlignCenter.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntTabAction }

procedure TTntTabAction.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntTabAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntTabAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntTabAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntTabAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntTabAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntPreviousTab }

procedure TTntPreviousTab.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntPreviousTab.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntPreviousTab.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntPreviousTab.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntPreviousTab.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntPreviousTab.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntNextTab }

procedure TTntNextTab.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntNextTab.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntNextTab.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntNextTab.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntNextTab.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntNextTab.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntOpenPicture }

procedure TTntOpenPicture.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntOpenPicture.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntOpenPicture.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntOpenPicture.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntOpenPicture.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntOpenPicture.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntSavePicture }

procedure TTntSavePicture.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntSavePicture.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSavePicture.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntSavePicture.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntSavePicture.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntSavePicture.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntURLAction }

procedure TTntURLAction.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntURLAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntURLAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntURLAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntURLAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntURLAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntBrowseURL }

procedure TTntBrowseURL.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntBrowseURL.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntBrowseURL.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntBrowseURL.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntBrowseURL.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntBrowseURL.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntDownLoadURL }

procedure TTntDownLoadURL.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntDownLoadURL.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntDownLoadURL.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntDownLoadURL.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntDownLoadURL.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntDownLoadURL.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntSendMail }

procedure TTntSendMail.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntSendMail.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntSendMail.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntSendMail.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntSendMail.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntSendMail.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntListControlAction }

procedure TTntListControlAction.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntListControlAction.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntListControlAction.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntListControlAction.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntListControlAction.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntListControlAction.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntListControlCopySelection }

procedure TTntListControlCopySelection.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntListControlCopySelection.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntListControlCopySelection.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntListControlCopySelection.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntListControlCopySelection.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntListControlCopySelection.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntListControlDeleteSelection }

procedure TTntListControlDeleteSelection.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntListControlDeleteSelection.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntListControlDeleteSelection.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntListControlDeleteSelection.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntListControlDeleteSelection.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntListControlDeleteSelection.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntListControlSelectAll }

procedure TTntListControlSelectAll.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntListControlSelectAll.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntListControlSelectAll.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntListControlSelectAll.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntListControlSelectAll.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntListControlSelectAll.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntListControlClearSelection }

procedure TTntListControlClearSelection.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntListControlClearSelection.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntListControlClearSelection.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntListControlClearSelection.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntListControlClearSelection.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntListControlClearSelection.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

{ TTntListControlMoveSelection }

procedure TTntListControlMoveSelection.Assign(Source: TPersistent);
begin
  inherited;
  TntExtActn_AfterInherited_Assign(Self, Source);
end;

procedure TTntListControlMoveSelection.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;

function TTntListControlMoveSelection.GetCaption: WideString;
begin
  Result := TntAction_GetCaption(Self);
end;

procedure TTntListControlMoveSelection.SetCaption(const Value: WideString);
begin
  TntAction_SetCaption(Self, Value);
end;

function TTntListControlMoveSelection.GetHint: WideString;
begin
  Result := TntAction_GetHint(Self);
end;

procedure TTntListControlMoveSelection.SetHint(const Value: WideString);
begin
  TntAction_SetHint(Self, Value);
end;

end.
