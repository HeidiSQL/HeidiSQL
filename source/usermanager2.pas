unit usermanager2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Vcl.Menus, Vcl.ImgList, Vcl.Controls,
  Vcl.StdCtrls, VirtualTrees, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ToolWin,
  Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.XPMan,
  System.Actions, Vcl.ActnList, dbconnection,
  userprivileges, types_helpers;

type

  TUserManager2Form = class(TForm)
    ImageListMain: TImageList;
    CoolBar1: TCoolBar;
    Splitter1: TSplitter;
    pnlRight: TPanel;
    tvPrivilegeObjects: TVirtualStringTree;
    Panel2: TPanel;
    tvUsers: TVirtualStringTree;
    PopupPrivileges: TPopupMenu;
    mniGrantAll: TMenuItem;
    mniRevokeAll: TMenuItem;
    pnlRightTop: TPanel;
    grpCredentials: TGroupBox;
    editUsername: TEdit;
    editFromHost: TButtonedEdit;
    editPassword: TButtonedEdit;
    editRepeatPassword: TEdit;
    lblUsername: TLabel;
    lblFromHost: TLabel;
    lblPassword: TLabel;
    lblRepeatPassword: TLabel;
    grpLimitations: TGroupBox;
    lblMaxQueries: TLabel;
    lblMaxUpdates: TLabel;
    lblMaxConnections: TLabel;
    lblMaxUserConnections: TLabel;
    editMaxQueries: TEdit;
    editMaxUpdates: TEdit;
    editMaxConnections: TEdit;
    editMaxUserConnections: TEdit;
    udMaxUserConnections: TUpDown;
    udMaxConnections: TUpDown;
    udMaxUpdates: TUpDown;
    udMaxQueries: TUpDown;
    grpSSLOptions: TGroupBox;
    lblSSL: TLabel;
    lblCipher: TLabel;
    lblIssuer: TLabel;
    lblSubject: TLabel;
    comboSSL: TComboBox;
    editCipher: TEdit;
    editIssuer: TEdit;
    editSubject: TEdit;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    ilPrivilegesHeader: TImageList;
    Statusbar: TStatusBar;
    tlbUser: TToolBar;
    btnNewUser: TToolButton;
    btnCloneUser: TToolButton;
    btnRemoveUser: TToolButton;
    tlbFile: TToolBar;
    btnSave: TToolButton;
    btnDiscard: TToolButton;
    btnCancel: TToolButton;
    btn1: TToolButton;
    btnShowProperties: TToolButton;
    pmUsersTree: TPopupMenu;
    mniAddNewUser: TMenuItem;
    mniAddNewHost: TMenuItem;
    aclMain: TActionList;
    actDeleteUserObject: TAction;
    actToggleUserProperties: TAction;
    procedure tvPrivilegeObjectsAdvancedHeaderDraw(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
    procedure tvPrivilegeObjectsHeaderDrawQueryElements(Sender: TVTHeader;
      var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure FormShow(Sender: TObject);
    procedure tvPrivilegeObjectsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure tvPrivilegeObjectsGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure tvPrivilegeObjectsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure tvPrivilegeObjectsAfterCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure tvPrivilegeObjectsNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure tvUsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure tvPrivilegeObjectsMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure tvPrivilegeObjectsMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure tvPrivilegeObjectsChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure GrantRevokeAllClick(Sender: TObject);
    procedure tvPrivilegeObjectsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PopupPrivilegesPopup(Sender: TObject);
    procedure tvUsersGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure tvPrivilegeObjectsHeaderMouseMove(Sender: TVTHeader;
      Shift: TShiftState; X, Y: Integer);
    procedure tvPrivilegeObjectsNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvUsersEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure tvUsersNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure tvUsersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mniAddNewUserClick(Sender: TObject);
    procedure tvUsersEditCancelled(Sender: TBaseVirtualTree;
      Column: TColumnIndex);
    procedure tvUsersEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure mniAddNewHostClick(Sender: TObject);
    procedure tvUsersCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure actDeleteUserObjectExecute(Sender: TObject);
    procedure tvUsersChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvUsersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvUsersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvPrivilegeObjectsFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure tvPrivilegeObjectsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure tvUsersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure tvPrivilegeObjectsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure NotImplementedYetClick(Sender: TObject);
    procedure actToggleUserPropertiesExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    FConnection: TDBConnection;
    FCurrentNode: PVirtualNode;
    FPrivilegesColumns: TPrivilegesTreeColumns;
    FServerVersion: Integer;
    FChangedList: TObjectDataList;
    FPrivilegesList: TObjectPrivilegeDataList;
    FViewsGroupNode: PVirtualNode;
    FCurrentGrantee: string;

    procedure NotImplementedYet;

    function AddChanging(AObjectData: TObjectData): Integer;
    function AddNewHostNode(AParentNode: PVirtualNode): PVirtualNode;
    function AddNewUserNode(AParentNode: PVirtualNode): PVirtualNode;
    function IsView(const AObjectName: string): Boolean;
    function PrivilegeFromColumn(AColumn: TColumnIndex): TPrivilege;

    procedure BequeathPrivilege(ANode: PVirtualNode; APrivilege: TPrivilege);
    procedure BequeathPrivileges(ANode: PVirtualNode; APrivileges: TPrivileges);
    procedure HotTrack(X, Y: Integer; Relative: Boolean); virtual;
    procedure InitFromDB;
    procedure InitPrivilegeTreeColumns;
    procedure PaintCheckImage(AColumn: TColumnIndex; ACanvas: TCanvas; ARect: TRect; AState: TCheckState; AEnabled: Boolean);
    procedure LoadPrivileges(const AGrantee: string);
    procedure ApplyPrivilegesToTree;

  public
    { Public-Deklarationen }
    function ShowModal(AConnection: TDBConnection): TModalResult; reintroduce; virtual;
  end;

var
  UserManager2Form: TUserManager2Form;
  FHotTrackInfo: THitInfo;

implementation

{$R *.dfm}

uses
  System.Types, System.UITypes,
  Winapi.CommCtrl, Winapi.UxTheme, Main, VirtualCheckTree;

procedure TUserManager2Form.FormCreate(Sender: TObject);
begin
  FViewsGroupNode:= NIL;
  FChangedList:= TObjectDataList.Create(FALSE);
  FPrivilegesList:= TObjectPrivilegeDataList.Create;
  FServerVersion:= 0;
end;

function TUserManager2Form.ShowModal(AConnection: TDBConnection): TModalResult;
begin
  FConnection:= AConnection;
  FPrivilegesColumns:= TPrivilegesTreeColumns.Create(
    FConnection.Parameters.NetTypeGroup, FConnection.ServerVersionInt
  );
  try
    Result:= inherited ShowModal;
  finally
    FreeAndNil(FPrivilegesColumns);
  end;
end;

procedure TUserManager2Form.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FChangedList);
  FreeAndNil(FPrivilegesList);
end;

procedure TUserManager2Form.FormShow(Sender: TObject);
var
  I, J: Integer;
  RN, SN, TN, TGN, UN: PVirtualNode;
  Users: TUsersList;
  sUserName, sDBName, sTableName, sColumnName, sViewName, sProcedureName,
  sFunctionName: string;
  dbqDatabases, dbqTables, dbqColumns, dbqViews, dbqProcedures,
  dbqFunctions: TDBQuery;
  bInternalSchema: Boolean;

  function AddUserChild(AParent: PVirtualNode; AKind: TUserKind;
    ACaption: string): PVirtualNode;
  var
    N: PVirtualNode;
    D, PD: PUserData;
    TV: TVirtualStringTree;
  begin
    TV:= tvUsers;
    N:= TV.AddChild(AParent);
    PD:= TV.GetNodeData(AParent);
    D:= TV.GetNodeData(N);
    if D <> NIL then begin
      D^.Kind:= AKind;
      D^.ParentData:= PD;
      D^.SetCaption(ACaption, FALSE);
      if PD <> NIL then begin
        PD^.AddChild(D);
      end;
    end;
    Result:= N;
  end;

  function AddPrivilegeChild(AParent: PVirtualNode; AKind: TNodeObjectKind;
    ACaption: string; const AInternalSchema: Boolean = FALSE): PVirtualNode;
  var
    N: PVirtualNode;
    D: PObjectData;
    TV: TVirtualStringTree;
  begin
    TV:= tvPrivilegeObjects;
    N:= TV.AddChild(AParent);
    D:= TV.GetNodeData(N);
    if D <> NIL then begin
      D^.Kind:= AKind;
      D^.Caption:= ACaption;
      D^.Inherites:= TRUE;
      D^.InternalSchema:= AInternalSchema;
      if AKind = okGlobal then begin
          D^.Inherites:= FALSE;
        end;
      Result:= N;
    end else begin
      Result:= NIL;
    end;
  end;
begin
  Screen.Cursor:= crHourGlass;
  try
    InitFromDB;
    Users := FConnection.GetUsersList;

    // Building the users and roles tree

    RN:= AddUserChild(NIL, ukGroupUsers, 'Users');
    for I := 0 to Users.UserNames.Count - 1 do begin
      sUserName:= Users.UserNames[I];
      UN:= AddUserChild(RN, ukUser, sUserName);
      for J := 0 to Users.HostNames[sUserName].Count - 1 do begin
        AddUserChild(UN, ukHost, Users.HostNames[sUserName][J]);
      end;
      if I =  1 then begin
        tvUsers.Expanded[UN]:= TRUE;
      end;
    end;
    tvUsers.Expanded[RN]:= TRUE;

    { !! TODO: Implement user roles for MySQL 8.0+ and MariaDB 10.0.5+)
    RN:= AddUserChild(NIL, ukGroupRoles, 'Roles');
    for I := 1 to 5 do begin
      UN:= AddUserChild(RN, ukRole, 'My Role ' + I.ToString);
    end;
    tvUsers.Expanded[RN]:= TRUE;
    }

    // Building the privileges and objects tree

    InitPrivilegeTreeColumns;

   // Exit;

    RN:= AddPrivilegeChild(NIL, okGlobal, FConnection.Parameters.SessionName);

    dbqDatabases:= FConnection.GetResults('SHOW DATABASES');
    while not dbqDatabases.Eof do begin
      sDBName:= dbqDatabases.Col(0);

      dbqTables:= FConnection.GetResults(
        'SHOW FULL TABLES IN ' + FConnection.QuoteIdent(sDBName) + ' ' +
        'WHERE TABLE_TYPE LIKE ' + FConnection.EscapeString('%TABLE') + ';'
      );
      bInternalSchema:= (CompareText(sDBName, 'information_schema') = 0) or
                        (CompareText(sDBName, 'performance_schema') = 0) or
                        (CompareText(sDBName, 'mysql') = 0);
      SN:= AddPrivilegeChild(RN, okSchema, sDBName, bInternalSchema);
      TGN:= AddPrivilegeChild(SN, okGroupTables, Format('Tables (%d)', [dbqTables.RecordCount]), bInternalSchema);
      while not dbqTables.Eof do begin
        sTableName:= dbqTables.Col('Tables_in_' + sDBName);

        dbqColumns:= FConnection.GetResults(
          'SHOW COLUMNS FROM ' + FConnection.QuoteIdent(sDBName) + '.' +
            FConnection.QuoteIdent(sTableName) + ';'
        );

        TN:= AddPrivilegeChild(TGN, okTable, Format(sTableName + ' (%d)', [dbqColumns.RecordCount]), bInternalSchema);
        while not dbqColumns.Eof do begin
          sColumnName:= dbqColumns.Col('Field');
          {CN:= }AddPrivilegeChild(TN, okColumn, sColumnName, bInternalSchema);
          dbqColumns.Next;
        end;
        dbqTables.Next;
      end;

      dbqViews:= FConnection.GetResults(
        'SHOW FULL TABLES IN ' + FConnection.QuoteIdent(sDBName) + ' ' +
        'WHERE TABLE_TYPE LIKE ' + FConnection.EscapeString('%VIEW') + ';'
      );
      TGN:= AddPrivilegeChild(SN, okGroupViews, Format('Views (%d)', [dbqViews.RecordCount]), bInternalSchema);
      while not dbqViews.Eof do begin
        sViewName:= dbqViews.Col('Tables_in_' + sDBName);
        {TN:= }AddPrivilegeChild(TGN, okView, sViewName, bInternalSchema);
        dbqViews.Next;
      end;
      FViewsGroupNode:= TGN;

      dbqProcedures:= FConnection.GetResults(
        'SHOW PROCEDURE STATUS WHERE `Db`= ' + FConnection.EscapeString(sDBName)
      );
      TGN:= AddPrivilegeChild(SN, okGroupProcedures, Format('Procedures (%d)', [dbqProcedures.RecordCount]), bInternalSchema);
      while not dbqViews.Eof do begin
        sProcedureName:= dbqProcedures.Col('Name');
        {TN:= }AddPrivilegeChild(TGN, okProcedure, sProcedureName, bInternalSchema);
        dbqProcedures.Next;
      end;

      dbqFunctions:= FConnection.GetResults(
        'SHOW FUNCTION STATUS WHERE `Db`= ' + FConnection.EscapeString(sDBName)
      );
      TGN:= AddPrivilegeChild(SN, okGroupFunctions, Format('Functions (%d)', [dbqFunctions.RecordCount]), bInternalSchema);
      while not dbqFunctions.Eof do begin
        sFunctionName:= dbqFunctions.Col('Name');
        {TN:= }AddPrivilegeChild(TGN, okFunction, sFunctionName, bInternalSchema);
        dbqFunctions.Next;
      end;

      { !! Triggers and Events currently not part of the privilege system !!
      dbqTriggers:= FConnection.GetResults(
        'SHOW TRIGGERS FROM ' + FConnection.QuoteIdent(sDBName)
      );
      TGN:= AddPrivilegeChild(SN, okGroupTriggers, Format('Triggers (%d)', [dbqTriggers.RecordCount]));
      while not dbqTriggers.Eof do begin
        sTriggerName:= dbqTriggers.Col('Trigger');
        TN:= AddPrivilegeChild(TGN, okTrigger, sTriggerName);
        dbqTriggers.Next;
      end;

      dbqEvents:= FConnection.GetResults(
        'SELECT EVENT_NAME AS ' + FConnection.QuoteIdent('Name') + ' ' +
        'FROM ' + FConnection.QuoteIdent('information_schema') + '.' +
        FConnection.QuoteIdent('EVENTS') + ' WHERE ' +
        FConnection.QuoteIdent('EVENT_SCHEMA') + '=' +
        FConnection.EscapeString(sDBName) + ';'
      );
      TGN:= AddPrivilegeChild(SN, okGroupEvents, Format('Events (%d)', [dbqEvents.RecordCount]));
      while not dbqEvents.Eof do begin
        sEventName:= dbqEvents.Col('Name');
        TN:= AddPrivilegeChild(TGN, okEvent, sEventName);
        dbqEvents.Next;
      end; }

      dbqDatabases.Next;
    end;
    tvPrivilegeObjects.Expanded[RN]:= TRUE;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TUserManager2Form.actDeleteUserObjectExecute(Sender: TObject);
begin
  ShowMessage('Delete');
end;

procedure TUserManager2Form.actToggleUserPropertiesExecute(Sender: TObject);
begin
  pnlRightTop.Visible:= not pnlRightTop.Visible;
  if pnlRightTop.Visible then begin
    actToggleUserProperties.Caption:= 'Hide user properties';
  end else begin
    actToggleUserProperties.Caption:= 'Show user properties';
  end;
end;

function TUserManager2Form.AddChanging(AObjectData: TObjectData): Integer;
begin
  if FChangedList.IndexOf(AObjectData) < 0 then begin
    Result:= FChangedList.Add(AObjectData);
  end else begin
    Result:= -1;
  end;
end;

function TUserManager2Form.AddNewHostNode(AParentNode: PVirtualNode): PVirtualNode;
var
  D: PUserData;
  N: PVirtualNode;
begin
  Result:= NIL;
  N:= tvUsers.AddChild(AParentNode);
  D:= tvUsers.GetNodeData(N);
  if D <> NIL then begin
    D^.Kind:= ukHost;
    D^.Caption:= 'localhost';
    D^.New:= TRUE;
    D^.SetChanged(ucpCaption);
    D^.SetChanged(ucpProperties);
    D^.SetChanged(ucpPrivileges);
    tvUsers.MoveTo(N, AParentNode, amAddChildFirst, FALSE);
    tvUsers.EditNode(N, -1);
    Result:= N;
  end;
end;

function TUserManager2Form.AddNewUserNode(AParentNode: PVirtualNode): PVirtualNode;
var
  D, PD: PUserData;
  N, HN: PVirtualNode;
begin
  Result:= NIL;
  N:= tvUsers.AddChild(AParentNode);
  D:= tvUsers.GetNodeData(N);
  if D <> NIL then begin
    PD:= D;
    D^.Kind:= ukUser;
    D^.SetCaption('New User');
    D^.New:= TRUE;
    HN:= tvUsers.AddChild(N);
    D:= tvUsers.GetNodeData(HN);
    if D <> NIL then begin
      D^.Kind:= ukHost;
      D^.New:= TRUE;
      D^.ParentData:= PD;
      D^.SetCaption('localhost');
      D^.SetChanged(ucpCaption);
      D^.SetChanged(ucpProperties);
      D^.SetChanged(ucpPrivileges);
      PD^.AddChild(D);
    end;
    tvUsers.MoveTo(N, AParentNode, amAddChildFirst, FALSE);
    tvUsers.Expanded[N]:= TRUE;
    tvUsers.EditNode(N, -1);
    Result:= N;
  end;
end;

procedure TUserManager2Form.ApplyPrivilegesToTree;
var
  SchemaName, ObjectName: string;
  N: PVirtualNode;
  D: PObjectData;
  T: TBaseVirtualTree;

  procedure ApplyPrivilegesToChildNodes(AParentNode: PVirtualNode;
    APrivileges: TPrivileges);
  var
    N: PVirtualNode;
    D: PObjectData;
    Ps: TPrivileges;
  begin
    N:= T.GetFirstChild(AParentNode);
    while N <> NIL do begin
      D:= T.GetNodeData(N);
      if D <> NIL then begin
        case D^.Kind of
          okGlobal:
          begin
          end;

          okSchema:
          begin
            SchemaName:= D^.Caption;
            FPrivilegesList.Filter(
              FCurrentGrantee, okSchema, SchemaName
            );
          end;

          okTable:
          begin
            ObjectName:= D^.Caption;
            FPrivilegesList.Filter(
              FCurrentGrantee, okTable, SchemaName, ObjectName
            );
          end;

          okView:
          begin
            ObjectName:= D^.Caption;
            FPrivilegesList.Filter(
              FCurrentGrantee, okView, SchemaName, ObjectName
            );
          end;

          okColumn:
          begin
            FPrivilegesList.Filter(
              FCurrentGrantee, okColumn, SchemaName, ObjectName, D^.Caption
            );
          end;

          okFunction:
          begin
            FPrivilegesList.Filter(
              FCurrentGrantee, okFunction, SchemaName, ObjectName
            );
          end;

          okProcedure:
          begin
            FPrivilegesList.Filter(
              FCurrentGrantee, okProcedure, SchemaName, ObjectName
            );
          end;
        end;
        if FPrivilegesList.FilteredList.Count > 0 then begin
          Ps:= FPrivilegesList.FilteredPrivileges;
          D^.Inherites:= FALSE;
        end else begin
          Ps:= APrivileges;
          D^.Inherites:= TRUE;
        end;
        D^.Privileges:= Ps;
        if N^.ChildCount > 0 then begin
          ApplyPrivilegesToChildNodes(N, Ps);
        end;
      end;
      N:= T.GetNextSibling(N);
    end;
  end;
begin
  T:= tvPrivilegeObjects;
  N:= T.GetFirst;
  D:= T.GetNodeData(N);
  if (D <> NIL) and (D^.Kind = okGlobal) then begin
    FPrivilegesList.Filter(FCurrentGrantee, okGlobal);
    D^.Privileges:= FPrivilegesList.FilteredPrivileges;
    ApplyPrivilegesToChildNodes(N, D^.Privileges);
  end;
  T.Invalidate;
  Application.ProcessMessages;
end;

procedure TUserManager2Form.BequeathPrivilege(ANode: PVirtualNode; APrivilege: TPrivilege);
var
  Grant: Boolean;
  PD, CD: PObjectData;
  PN, CN: PVirtualNode;
  TV: TVirtualStringTree;
begin
  PN:= ANode;
  PD:= tvPrivilegeObjects.GetNodeData(PN);
  TV:= tvPrivilegeObjects;
  if PD <> NIL then begin
    Grant:= PD^.Granted(APrivilege);
    CN:= TV.GetFirstChild(PN);
    CD:= TV.GetNodeData(CN);
    while CD <> NIL do begin
      if CD^.Inherites and (CD^.Granted(APrivilege) <> Grant) then begin
        CD^.TogglePrivilege(APrivilege);
        TV.InvalidateNode(CN);
      end;
      if TV.HasChildren[CN] and CD^.Inherites then begin
        BequeathPrivilege(CN, APrivilege);
      end;
      CN:= TV.GetNextSibling(CN);
      CD:= TV.GetNodeData(CN);
    end;
  end;
end;

procedure TUserManager2Form.BequeathPrivileges(ANode: PVirtualNode;
  APrivileges: TPrivileges);
var
  PD, CD: PObjectData;
  PN, CN: PVirtualNode;
  TV: TVirtualStringTree;
begin
  PN:= ANode;
  PD:= tvPrivilegeObjects.GetNodeData(PN);
  TV:= tvPrivilegeObjects;
  if PD <> NIL then begin
    CN:= TV.GetFirstChild(PN);
    CD:= TV.GetNodeData(CN);
    while CD <> NIL do begin
      if CD^.Inherites then begin
        CD^.Privileges:= APrivileges;
        TV.InvalidateNode(CN);
      end;
      if TV.HasChildren[CN] and CD^.Inherites then begin
        BequeathPrivileges(CN, APrivileges);
      end;
      CN:= TV.GetNextSibling(CN);
      CD:= TV.GetNodeData(CN);
    end;
  end;
end;

procedure TUserManager2Form.GrantRevokeAllClick(Sender: TObject);
var
  D: PObjectData;
  P: TPrivilege;
begin
  D:= tvPrivilegeObjects.GetNodeData(FCurrentNode);
  if D <> NIL then begin
    case TComponent(Sender).Tag of
      1:
      begin
        for P := Low(TPrivilege) to High(TPrivilege) do begin
          D^.Privileges:= D^.Privileges + [P];
        end;
      end;
      2: D^.Privileges:= [];
    end;
  end;
  try
    LockWindowUpdate(tvPrivilegeObjects.Handle);
    BequeathPrivileges(FCurrentNode, D^.Privileges);
    tvPrivilegeObjects.InvalidateNode(FCurrentNode);
    Application.ProcessMessages;
  finally
    LockWindowUpdate(0);
  end;
end;

procedure TUserManager2Form.HotTrack(X, Y: Integer; Relative: Boolean);
var
  CI1, CI2: TColumnIndex;
  D: PObjectData;
  HC1, HC2: TVirtualTreeColumn;
begin
  CI1:= FHotTrackInfo.HitColumn;
  tvPrivilegeObjects.GetHitTestInfoAt(X+1, Y+1, Relative, FHotTrackInfo);
  CI2:= FHotTrackInfo.HitColumn;
  if CI1 > -1 then begin
    HC1:= tvPrivilegeObjects.Header.Columns[CI1];
    tvPrivilegeObjects.Header.Invalidate(HC1, TRUE);
    tvPrivilegeObjects.InvalidateColumn(CI1);
  end;
  if CI2 > -1 then begin
    HC2:= tvPrivilegeObjects.Header.Columns[CI2];
    tvPrivilegeObjects.Header.Invalidate(HC2, TRUE);
    tvPrivilegeObjects.InvalidateColumn(CI2);

    D:= tvPrivilegeObjects.GetNodeData(FHotTrackInfo.HitNode);
    if D <> NIL then begin
      Statusbar.Panels[1].Text:= D^.Caption;
    end else begin
      Statusbar.Panels[1].Text:= '';
    end;
    Statusbar.Panels[2].Text:= PrivilegeFromColumn(CI2).ToString;

  end else begin
    Statusbar.Panels[1].Text:= '';
    Statusbar.Panels[2].Text:= '';
  end;
  Application.ProcessMessages;
end;

procedure TUserManager2Form.InitFromDB;
var
  tmp: string;
begin
  FConnection:= MainForm.ActiveConnection;
  FServerVersion:= FConnection.ServerVersionInt;
  InitPrivilegeTreeColumns;
  tmp := FConnection.GetVar('SHOW VARIABLES LIKE '+FConnection.EscapeString('skip_name_resolve'), 1);
end;

procedure TUserManager2Form.InitPrivilegeTreeColumns;
var
  C: TVirtualTreeColumn;
  VTC: TVirtualTreeColumns;
  P: TPrivilege;
begin
  VTC:= tvPrivilegeObjects.Header.Columns;
  VTC.Clear;

  // Object tree column
  C:= VTC.Add;
  C.Text:= '';
  C.MinWidth:= 150;
  C.CaptionAlignment:= taCenter;
  C.Width:= 200;
  C.Options:= [coEnabled, coParentBidiMode, coParentColor,
               coResizable, coVisible, coAllowFocus, coUseCaptionAlignment];

  // Inherit from parent column
  C:= VTC.Add;
  C.Text:= 'Inherit from Parent';
  C.Width:= 21;
  C.Options:= [coEnabled, coParentBidiMode, coParentColor, coVisible, coAllowFocus];

  // Privilege columns
  for P in FPrivilegesColumns do begin
    C:= VTC.Add;
    C.Options:= [coEnabled, coParentBidiMode, coParentColor, coVisible, coAllowFocus];
    C.Text:= P.ToString;
    C.Tag:= Integer(P);
    C.Width:= 21;
  end;
end;

function TUserManager2Form.IsView(const AObjectName: string): Boolean;
var
  N: PVirtualNode;
  D: PObjectData;
begin
  Result:= FALSE;
  if FViewsGroupNode <> NIL then begin
    N:= tvPrivilegeObjects.GetFirstChild(FViewsGroupNode);
    while N <> NIL do begin
      D:= tvPrivilegeObjects.GetNodeData(N);
      if (D <> NIL) and (D^.Kind = okView) and
         (CompareText(D^.Caption, AObjectName) = 0) then
      begin
        Result:= TRUE;
        Break;
      end;
      N:= tvPrivilegeObjects.GetNextSibling(N);
    end;
  end;
end;

procedure TUserManager2Form.LoadPrivileges(const AGrantee: string);
var
  C: TDBConnection;
  Q: TDBQuery;
  OPD: TObjectPrivilegeData;
  OK: TNodeObjectKind;
begin
  C:= FConnection;

  // Loading global privileges
  Q:= C.GetResults(
    'SELECT * FROM ' +
    C.QuoteIdent('information_schema') + '.' +
    C.QuoteIdent('USER_PRIVILEGES') +
    ' WHERE ' + C.QuoteIdent('GRANTEE') + '=' +
    C.EscapeString(AGrantee)
  );
  while not Q.Eof do begin
    OPD:= FPrivilegesList.Append;
    OPD.Kind:= okGlobal;
    OPD.Grantee:= Q.Col('GRANTEE');
    OPD.Privilege.FromString(Q.Col('PRIVILEGE_TYPE'));
    if (CompareText(Q.Col('IS_GRANTABLE'), 'YES') = 0) then begin
      OPD:= FPrivilegesList.Append;
      OPD.Kind:= okGlobal;
      OPD.Grantee:= Q.Col('GRANTEE');
      OPD.Privilege:= prGrant;
    end;
    Q.Next;
  end;

  // Loading schema privileges
  Q:= C.GetResults(
    'SELECT * FROM ' +
    C.QuoteIdent('information_schema') + '.' +
    C.QuoteIdent('SCHEMA_PRIVILEGES') +
    ' WHERE ' + C.QuoteIdent('GRANTEE') + '=' +
    C.EscapeString(AGrantee)
  );
  while not Q.Eof do begin
    OPD:= FPrivilegesList.Append;
    OPD.Kind:= okSchema;
    OPD.Grantee:= Q.Col('GRANTEE');
    OPD.SchemaName:= Q.Col('TABLE_SCHEMA');
    OPD.Privilege.FromString(Q.Col('PRIVILEGE_TYPE'));
    if (CompareText(Q.Col('IS_GRANTABLE'), 'YES') = 0) then begin
      OPD:= FPrivilegesList.Append;
      OPD.Kind:= okSchema;
      OPD.Grantee:= Q.Col('GRANTEE');
      OPD.Privilege:= prGrant;
      OPD.SchemaName:= Q.Col('TABLE_SCHEMA');
    end;
    Q.Next;
  end;

  // Loading table and view privileges
  Q:= C.GetResults(
    'SELECT * FROM ' +
    C.QuoteIdent('information_schema') + '.' +
    C.QuoteIdent('TABLE_PRIVILEGES') +
    ' WHERE ' + C.QuoteIdent('GRANTEE') + '=' +
    C.EscapeString(AGrantee)
  );
  while not Q.Eof do begin
    OPD:= FPrivilegesList.Append;
    OPD.ObjectName:= Q.Col('TABLE_NAME');
    if IsView(OPD.ObjectName) then begin
      OK:= okView;
    end else begin
      OK:= okTable;
    end;
    OPD.Kind:= OK;
    OPD.Grantee:= Q.Col('GRANTEE');
    OPD.SchemaName:= Q.Col('TABLE_SCHEMA');
    OPD.Privilege.FromString(Q.Col('PRIVILEGE_TYPE'));
    if (CompareText(Q.Col('IS_GRANTABLE'), 'YES') = 0) then begin
      OPD:= FPrivilegesList.Append;
      OPD.Kind:= OK;
      OPD.Grantee:= Q.Col('GRANTEE');
      OPD.Privilege:= prGrant;
      OPD.SchemaName:= Q.Col('TABLE_SCHEMA');
      OPD.ObjectName:= Q.Col('TABLE_NAME');
    end;
    Q.Next;
  end;

  // Loading column privileges
  Q:= C.GetResults(
    'SELECT * FROM ' +
    C.QuoteIdent('information_schema') + '.' +
    C.QuoteIdent('COLUMN_PRIVILEGES') +
    ' WHERE ' + C.QuoteIdent('GRANTEE') + '=' +
    C.EscapeString(AGrantee)
  );
  while not Q.Eof do begin
    OPD:= FPrivilegesList.Append;
    OPD.Kind:= okColumn;
    OPD.Grantee:= Q.Col('GRANTEE');
    OPD.SchemaName:= Q.Col('TABLE_SCHEMA');
    OPD.ObjectName:= Q.Col('TABLE_NAME');
    OPD.ColumnName:= Q.Col('COLUMN_NAME');
    OPD.Privilege.FromString(Q.Col('PRIVILEGE_TYPE'));
    if (CompareText(Q.Col('IS_GRANTABLE'), 'YES') = 0) then begin
      OPD:= FPrivilegesList.Append;
      OPD.Kind:= okColumn;
      OPD.Grantee:= Q.Col('GRANTEE');
      OPD.Privilege:= prGrant;
      OPD.SchemaName:= Q.Col('TABLE_SCHEMA');
      OPD.ObjectName:= Q.Col('TABLE_NAME');
      OPD.ColumnName:= Q.Col('COLUMN_NAME');
    end;
    Q.Next;
  end;
end;

procedure TUserManager2Form.mniAddNewHostClick(Sender: TObject);
begin
  AddNewHostNode(tvUsers.FocusedNode);
end;

procedure TUserManager2Form.mniAddNewUserClick(Sender: TObject);
begin
  AddNewUserNode(tvUsers.FocusedNode);
end;

procedure TUserManager2Form.NotImplementedYet;
begin
  MessageDlg('This function is not implemented yet. I''m so sorry.',
    mtInformation, [mbOk], 0);
end;

procedure TUserManager2Form.NotImplementedYetClick(Sender: TObject);
begin
  NotImplementedYet;
end;

procedure TUserManager2Form.PaintCheckImage(AColumn: TColumnIndex;
  ACanvas: TCanvas; ARect: TRect; AState: TCheckState; AEnabled: Boolean);
var
  IL: TCustomImageList;
  TV: TVirtualCheckTree;
  ColImageInfo: TVTImageInfo;
  XOffs, YOffs: Integer;
begin
  TV:= TVirtualCheckTree(tvPrivilegeObjects);
  IL:= TV.GetCheckImageListFor(ckSystemDefault);
  ColImageInfo.Images := IL;
  if not tvPrivilegeObjects.Enabled then begin
    AEnabled:= FALSE;
  end;
  if AColumn = FHotTrackInfo.HitColumn then begin
    XOffs:= 2;
    YOffs:= 1;
  end else begin
    XOffs:= 2;
    YOffs:= 1;
  end;
  ColImageInfo.Index := TV.GetCheckImage(nil, ctTriStateCheckBox, AState, AEnabled);
  ColImageInfo.XPos := ARect.Left + XOffs + (ARect.Width div 2) - (IL.Width div 2);
  ColImageInfo.YPos := ARect.Top + YOffs + (ARect.Height div 2) - (IL.Height div 2);
  TV.PaintCheckImage(
    ACanvas, ColImageInfo, FALSE
  );
end;

procedure TUserManager2Form.PopupPrivilegesPopup(Sender: TObject);
var
  D: PObjectData;
begin
  D:= tvPrivilegeObjects.GetNodeData(FCurrentNode);
  if D <> NIL then begin
    mniGrantAll.Enabled:= not D^.Inherites;
    mniRevokeAll.Enabled:= not D^.Inherites;
  end;
end;

function TUserManager2Form.PrivilegeFromColumn(AColumn: TColumnIndex): TPrivilege;
begin
  Result:= TPrivilege(tvPrivilegeObjects.Header.Columns[AColumn].Tag);
end;

procedure TUserManager2Form.tvPrivilegeObjectsAdvancedHeaderDraw(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
var
  CI: Integer;
  CT: string;
  PR, CR: TRect;
  LF: TLogFont;
  TF: TFont;
  TC: TCanvas;
  Theme: HTHEME;
  P: TPrivilege;

  procedure DrawImage(ImageList: TCustomImageList; Index: Integer; Canvas: TCanvas; X, Y: Integer; Style: Cardinal; Enabled: Boolean);
    // This is compied from VirtualTrees.pas because its do not draw properly
    // Header glyph images if the Tree is disabled
    procedure DrawDisabledImage(ImageList: TCustomImageList; Canvas: TCanvas; X, Y, Index: Integer);
    var
      Params: TImageListDrawParams;
    begin
      FillChar(Params, SizeOf(Params), 0);
      Params.cbSize := SizeOf(Params);
      Params.himl := ImageList.Handle;
      Params.i := Index;
      Params.hdcDst := Canvas.Handle;
      Params.x := X;
      Params.y := Y;
      Params.fState := ILS_SATURATE;
      ImageList_DrawIndirect(@Params);
    end;

  begin
    if Enabled then
      TImageList(ImageList).Draw(Canvas, X, Y, Index, Enabled)
    else
      DrawDisabledImage(ImageList, Canvas, X, Y, Index);
  end;

begin
  CI:= PaintInfo.Column.Index;
  TC:= PaintInfo.TargetCanvas;
  P:=  TPrivilege(Sender.Columns[CI].Tag);
  PR:= PaintInfo.PaintRectangle;
  CT:= PaintInfo.Column.Text;

  if hpeBackground in Elements then begin
    if FHotTrackInfo.HitColumn = CI then begin
      // Draw the vertical hottrack background
      CR:= Sender.Columns[CI].GetRect;
      CR.Bottom:= CR.Bottom + 2; // Overflow the Vista/W7 hottrack border at bottom
      Theme := OpenThemeData(Application.{$if CompilerVersion >= 20}ActiveFormHandle{$else}Handle{$ifend}, 'Explorer::TreeView');
      DrawThemeBackground(Theme, TC.Handle, TVP_TREEITEM, TREIS_HOT, CR, nil);
      CloseThemeData(Theme);
    end;
  end;

  if hpeText in Elements then begin
    TF := TFont.Create;
    TF.Assign(TC.Font);
    GetObject(TF.Handle, SizeOf(LF), @LF);
    TF.Name := tvPrivilegeObjects.Font.Name;
    if CI > 0 then begin
      if Sender.Treeview.Enabled then begin
        if CI = 1 then begin
          TF.Color := TPrivilege.CL_VIOLET_FG;
        end else begin
          TF.Color:= P.ColorFg;
        end;
      end else begin
        TF.Color:= clGray;
      end;

      LF.lfEscapement := 900;
      LF.lfOrientation := 900;
      TF.Handle := CreateFontIndirect(LF);
      TC.Font.Assign(TF);
      if CI = 1 then begin
        TC.Font.Style:= [fsBold];
      end else begin
        TC.Font.Style:= [];
      end;
      //TC.Brush.Style:= bsClear;
      SetBkMode(TC.Handle, TRANSPARENT);
      TC.TextOut(PR.Left, PR.Bottom - 5, CT);
      TF.free;
    end;
  end;

  if CI = 0 then begin
    DrawImage(Sender.Images, Sender.Columns[CI].ImageIndex, TC,
             PaintInfo.GlyphPos.X, PaintInfo.GlyphPos.Y, 0, Sender.Treeview.Enabled);
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsAfterCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
var
  D: PObjectData;
  P: TPrivilege;
begin
  if Column > 0 then begin
    D:= Sender.GetNodeData(Node);
    if (D <> NIL) and not D^.InternalSchema then begin
      P:= PrivilegeFromColumn(Column);
      if (Column > 1) and P.AvailableForObjectKind(D^.Kind) then begin
        if D^.Granted(PrivilegeFromColumn(Column)) then begin
          PaintCheckImage(Column, TargetCanvas, CellRect, csCheckedNormal, not D^.Inherites);
        end else begin
          PaintCheckImage(Column, TargetCanvas, CellRect, csUncheckedNormal, not D^.Inherites);
        end;
      end else if (Column = 1) then begin
        // Define which object types shows the inheritation checker
        if (D^.Kind <> okGlobal) and
           (D^.Kind <> okGroupTables) and
           (D^.Kind <> okGroupViews) and
           (D^.Kind <> okGroupProcedures) and
           (D^.Kind <> okGroupFunctions) and
           (D^.Kind <> okGroupEvents) and
           (D^.Kind <> okGroupTriggers) then
        begin
          if D^.Inherites then begin
            PaintCheckImage(Column, TargetCanvas, CellRect, csCheckedNormal, TRUE)
          end else begin
            PaintCheckImage(Column, TargetCanvas, CellRect, csUncheckedNormal, TRUE)
          end;
        end;
      end;
    end;
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  CI: Integer;
  CL: TColor;
  CR: TRect;
  P: TPrivilege;
  TC: TCanvas;
  Theme: HTHEME;
begin
  CI:= Column;
  P:=  TPrivilege(tvPrivilegeObjects.Header.Columns[CI].Tag);
  TC:= TargetCanvas;
  if CI > 0 then begin
    if Sender.Enabled then begin
      if CI = 1 then begin
        CL := TPrivilege.CL_VIOLET_BG;
      end else begin
        CL:= P.ColorBg;
      end;
      TC.Brush.Color:= CL;
      TC.FillRect(CellRect);
    end;

    if CI = FHotTrackInfo.HitColumn then begin
      // Draw the vertical hottrack background
      CR:= CellRect;
      CR.Inflate(0, 2); // Hide top and bottom edge on Vista/W7
      //CR.Right:= CR.Right + 1;
      if Node = Sender.GetLastVisible() then begin
        CR.Bottom:= CR.Bottom - 2; // Show the bottom edge on last visible node
      end;
      Theme := OpenThemeData(Application.{$if CompilerVersion >= 20}ActiveFormHandle{$else}Handle{$ifend}, 'Explorer::TreeView');
      DrawThemeBackground(Theme, TC.Handle, TVP_TREEITEM, TREIS_HOT, CR, NIL);
      CloseThemeData(Theme);
    end;
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  Sender.ClearSelection;
  Sender.FocusedNode:= NIL;
end;

procedure TUserManager2Form.tvPrivilegeObjectsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  D: PObjectData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    FreeAndNil(D^);
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsGetImageIndex(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  D: PObjectData;
begin
  D:= Sender.GetNodeData(Node);
  if (D <> NIL) and (Column = 0) then begin
    if ((Kind = ikNormal) or (Kind = ikSelected)) then begin
      case D^.Kind of
        okGlobal:         ImageIndex:= 166;
        okSchema:         ImageIndex:= 5;

        okGroupTables:    ImageIndex:= 14;
        okGroupViews:     ImageIndex:= 81;
        okGroupProcedures:ImageIndex:= 119;
        okGroupFunctions: ImageIndex:= 35;
        okGroupTriggers:  ImageIndex:= 137;
        okGroupEvents:    ImageIndex:= 80;

        okTable:          ImageIndex:= 14;
        okColumn:         ImageIndex:= 42;
        okView:           ImageIndex:= 81;
        okProcedure:      ImageIndex:= 119;
        okFunction:       ImageIndex:= 35;
        //okTrigger:        ImageIndex:= 137;
        //okEvent:          ImageIndex:= 80;
      end;
    end else if (Kind = ikOverlay) then begin
      if D^.Changed then begin
        ImageIndex:= 178;
      end;
    end;
  end;
end;


procedure TUserManager2Form.tvPrivilegeObjectsGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize:= SizeOf(TObjectData);
end;

procedure TUserManager2Form.tvPrivilegeObjectsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  D: PObjectData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    CellText:= D^.Caption;
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsHeaderDrawQueryElements(Sender: TVTHeader;
  var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if (PaintInfo.Column <> NIL) and (PaintInfo.Column.Index > 0) then begin
    Elements:= [hpeText, hpeBackground];
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsHeaderMouseMove(Sender: TVTHeader;
  Shift: TShiftState; X, Y: Integer);
begin
  HotTrack(X, Y, TRUE);
end;

procedure TUserManager2Form.tvPrivilegeObjectsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  D: PObjectData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    D^:= TObjectData.Create;
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  HotTrack(X, Y, TRUE);
end;

procedure TUserManager2Form.tvPrivilegeObjectsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  iDummy: Integer;
  P: TPoint;
begin
  if Button = mbRight then begin
    P:= tvPrivilegeObjects.ClientToScreen(Point(X, Y));
    FCurrentNode:= tvPrivilegeObjects.GetNodeAt(X, Y, FALSE, iDummy);
    PopupPrivileges.Popup(P.X, P.Y);
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
  HotTrack(MousePos.X, MousePos.Y, FALSE);
end;

procedure TUserManager2Form.tvPrivilegeObjectsNodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  bInherites: Boolean;
  D1, D2: PObjectData;
  CI: Integer;
  N1, N2: PVirtualNode;
  P: TPrivilege;
begin
  N1:= HitInfo.HitNode;
  D1:= Sender.GetNodeData(N1);
  CI:= HitInfo.HitColumn;
  if D1 <> NIL then begin
    LockWindowUpdate(Sender.Handle);
    try
      if CI = 1 then begin
        // Hit cell is the inheritation checker
        case D1^.Kind of
          okTable, okView, okSchema, okColumn:
          begin
            N2:= N1^.Parent;
            D2:= Sender.GetNodeData(N2);
            while (D2 <> NIL) and (D2^.GroupNode = TRUE) do begin
              // Find the next upper object with inheritable privileges
              N2:= N2^.Parent;
              D2:= Sender.GetNodeData(N2);
            end;
            bInherites:= D1^.ToggleInherites;
            if (D2 <> NIL) and bInherites then begin
              // Upward inheritance when re-enable inheritation on a child object
              D1^.Privileges:= D2^.Privileges;
              BequeathPrivileges(N1, D2^.Privileges);
            end;
            Sender.InvalidateNode(HitInfo.HitNode);
            Application.ProcessMessages;
          end;
        end;
      end else if (CI > 1) and (not D1^.Inherites) then begin
        P:= PrivilegeFromColumn(CI);
        D1^.TogglePrivilege(P);
        Sender.InvalidateNode(HitInfo.HitNode);
        Application.ProcessMessages;
        BequeathPrivilege(N1, P);
      end;
    finally
      if (D1 <> NIL) and D1^.Changed then begin
        AddChanging(D1^);
      end;
      LockWindowUpdate(0);
    end;
  end;
end;

procedure TUserManager2Form.tvPrivilegeObjectsNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  if HitInfo.HitColumn = 0 then begin
   Sender.ToggleNode(HitInfo.HitNode);
  end;
end;

procedure TUserManager2Form.tvUsersChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  D1, D2: PUserData;
  PN: PVirtualNode;
begin
  if Node = NIL then begin
    Exit;
  end;
  PN:= Node.Parent;
  D1:= Sender.GetNodeData(Node);
  D2:= Sender.GetNodeData(PN);
  if D1 <> NIL then begin
    actDeleteUserObject.Enabled:= (D1^.Kind = ukUser) or (D1^.Kind = ukHost) or
                                  (D1^.Kind = ukRole);
    case D1^.Kind of
      ukHost:
      begin
        Statusbar.Panels[0].Text:= D1^.InternalName;
        tvPrivilegeObjects.Enabled:= TRUE;
        if D2 <> NIL then begin
          FCurrentGrantee:= FConnection.EscapeString(D2^.Caption) + '@' +
                            FConnection.EscapeString(D1^.Caption);
          LoadPrivileges(FCurrentGrantee);
          ApplyPrivilegesToTree;
        end;
      end;

      ukRole:
      begin
        Statusbar.Panels[0].Text:= D1^.Caption;
        tvPrivilegeObjects.Enabled:= TRUE;
      end;

      else begin
        Statusbar.Panels[0].Text:= '';
        tvPrivilegeObjects.Enabled:= FALSE;
      end;
    end;

  end;
end;

procedure TUserManager2Form.tvUsersCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  D1, D2: PUserData;
begin
  D1:= Sender.GetNodeData(Node1);
  D2:= Sender.GetNodeData(Node2);
  if (D1 <> NIL) and (D2 <> NIL) then begin
    if (D1^.Kind = ukGroupUsers) then begin
      Result:= -1;
    end else if (D2^.Kind = ukGroupUsers) then begin
      Result:= 1;
    end else if (D1^.Kind = ukHost) and (D1^.Caption.ToLower = 'localhost') then begin
      Result:= -1;
    end else if (D2^.Kind = ukHost) and (D2^.Caption.ToLower = 'localhost') then begin
      Result:= 1;
    end else begin
      Result:= CompareStr(D1^.Caption, D2^.Caption);
    end;
  end;
end;

procedure TUserManager2Form.tvUsersEditCancelled(Sender: TBaseVirtualTree;
  Column: TColumnIndex);
var
  D: PUserData;
  N: PVirtualNode;
begin
  N:= Sender.FocusedNode;
  D:= Sender.GetNodeData(N);
  if D <> NIL then begin
    if D^.New then begin
      Sender.DeleteChildren(N, TRUE);
      Sender.DeleteNode(N);
    end;
  end;
end;

procedure TUserManager2Form.tvUsersEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var
  D: PUserData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    D^.New:= FALSE;
  end;
  Sender.Selected[Node]:= TRUE;
  Sender.Sort(Node^.Parent, -1, sdAscending);
end;

procedure TUserManager2Form.tvUsersEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var
  D: PUserData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    Allowed:= (D^.Kind = ukUser) or (D^.Kind = ukHost) or (D^.Kind = ukRole);
  end;
end;

procedure TUserManager2Form.tvUsersFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  D: PUserData;
begin
  D:= Sender.GetNodeData(Node);
  FreeAndNil(D^);
end;

procedure TUserManager2Form.tvUsersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  D: PUserData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    if (Kind = ikNormal) or (Kind = ikSelected) then begin
      case D^.Kind of
        ukGroupUsers: ImageIndex:= 11;
        ukUser:       ImageIndex:= 43;
        ukGroupRoles: ImageIndex:= 191;
        ukRole:       ImageIndex:= 191;
        ukHost:       ImageIndex:= 1;
      end;
    end else if (Kind = ikOverlay) then begin
      if D^.Changed then begin
        ImageIndex:= 178;
      end;
    end;
  end;
  if (Column = 0) and ((Kind = ikNormal) or (Kind = ikSelected)) then begin
    ImageIndex:= 12;
  end;
end;

procedure TUserManager2Form.tvUsersGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize:= SizeOf(TUserData);
end;

procedure TUserManager2Form.tvUsersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  D: PUserData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    CellText:= D^.Caption;
  end;
end;

procedure TUserManager2Form.tvUsersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  D: PUserData;
begin
  D:= Sender.GetNodeData(Node);
  D^:= TUserData.Create;
end;

procedure TUserManager2Form.tvUsersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  D: PUserData;
  N: PVirtualNode;
  P: TPoint;
begin
  if Button = mbRight then begin
    N:= tvUsers.GetNodeAt(X, Y);
    D:= tvUsers.GetNodeData(N);
    if D <> NIL then begin
      tvUsers.FocusedNode:= N;
      tvUsers.InvalidateNode(N);
      Application.ProcessMessages;
      P:= Point(X, Y);
      P:= tvUsers.ClientToScreen(P);
      mniAddNewUser.Visible:= (D^.Kind = ukGroupUsers);
      mniAddNewHost.Visible:= (D^.Kind = ukUser);
      pmUsersTree.Popup(P.X, P.Y);
    end;
  end;
end;

procedure TUserManager2Form.tvUsersNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  D: PUserData;
begin
  D:= Sender.GetNodeData(Node);
  if D <> NIL then begin
    if D^.Caption <> NewText then begin
      D^.SetCaption(NewText, TRUE);
    end;
  end;
end;


end.
