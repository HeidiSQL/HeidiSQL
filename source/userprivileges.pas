unit userprivileges;

interface

uses
  System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Vcl.Graphics, types_helpers;

type
  TPrivilegeColumnComparer = class(TComparer<TPrivilege>)
    function Compare(const Left, Right: TPrivilege): Integer; override;
  end;

  TPrivilegesTreeColumns = class(TList<TPrivilege>)
  private
    FComparer: TPrivilegeColumnComparer;
  public
    constructor Create(const ANetTypeGroup: TNetTypeGroup; const AServerVersion: Integer); virtual;
    destructor Destroy; override;

    procedure Sort;
  end;


type
  PObjectData = ^TObjectData;
  TObjectData = class(TObject)
    Kind: TNodeObjectKind;
    Caption: string;
    Privileges: TPrivileges;
    Inherites: Boolean;
    InternalSchema: Boolean;
    Changed: Boolean;

    function Granted(APrivilege: TPrivilege): Boolean; virtual;
    function GroupNode: Boolean; virtual;
    function ToggleInherites: Boolean; virtual;
    function TogglePrivilege(APrivilege: TPrivilege): Boolean; virtual;

    procedure SaveToDB(const ASchemeName, AGranteeName: string;
      const ANetTypeGroup: TNetTypeGroup;
      const AServerVersion: Integer); virtual;
  end;

  TObjectDataList = TObjectList<TObjectData>;

  PUserData = ^TUserData;
  TUserDataList = TList<PUserData>;
  TUserData = class
    Kind: TUserKind;
    Caption: string;
    Changed: Boolean;
    ChangedParts: TUserChangedParts;
    InternalName: string;
    New: Boolean;
    ParentData: PUserData;
    ChildData: TUserDataList;

    constructor Create;
    destructor Destroy; override;

    function AddChild(AChild: PUserData): Integer;
    procedure SetCaption(const ACaption: string; const ASetChanged: Boolean = TRUE);
    procedure SetChanged(const APart: TUserChangedPart);
  end;

  TUser = class(TObject)
    Username: string;
    Host: string;
    Password: string;
    Cipher: string;
    Issuer: string;
    Subject: String;
    MaxQueries: Integer;
    MaxUpdates: Integer;
    MaxConnections: Integer;
    MaxUserConnections: Integer;
    SSL: Integer;
    Problem: TUserProblem;
    function HostRequiresNameResolve: Boolean;
  end;
  PUser = ^TUser;

  TUsersList = class(TObjectList<TUser>)
  private
    FUserNames: TStringList;
    FHostNames: TStringList;
  protected
    function GetHostNames(const AUserName: string): TStringList; virtual;
    function GetUserNames: TStringList; virtual;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    destructor Destroy; override;

    function Add(const Value: TUser): Integer; virtual;
    procedure Clear; virtual;

    property HostNames[const UserName: string]: TStringList read GetHostNames;
    property UserNames: TStringList read GetUserNames;
  end;

  TObjectPrivilegeDataList = class;

  TObjectPrivilegeData = class(TObject)
  private
    FParent: TObjectPrivilegeDataList;
  public
    Kind: TNodeObjectKind;
    Grantee: string;
    Privilege: TPrivilege;
    SchemaName: string;
    ObjectName: string;
    ColumnName: string;
    Index: Integer;

    constructor Create(AParent: TObjectPrivilegeDataList); virtual;
  end;

  TObjectPrivilegeDataList = class(TObjectList<TObjectPrivilegeData>)
  private
    FFilterable: Boolean;
    FFilteredList: TObjectPrivilegeDataList;
  public
    constructor Create(AOwnsObjects: Boolean = TRUE;
      AFilterable: Boolean = TRUE); overload;
    destructor Destroy; override;

    function Append: TObjectPrivilegeData;
    function Filter(const AGrantee: string;
      const AKind: TNodeObjectKind;
      const ASchemaName: string = '';
      const AObjectName: string = '';
      const AColumnName: string = ''): TObjectPrivilegeDataList;

    function FilteredPrivileges: TPrivileges;
    function Privileges: TPrivileges;

    property FilteredList: TObjectPrivilegeDataList read FFilteredList;
  end;


implementation

uses
  SynRegExpr, System.SysUtils, VirtualTrees, dbconnection;

{ TObjectData }

function TObjectData.Granted(APrivilege: TPrivilege): Boolean;
begin
  Result:= APrivilege in Self.Privileges;
end;

function TObjectData.GroupNode: Boolean;
begin
  Result := (Self.Kind = okGroupTables) or
            (Self.Kind = okGroupViews) or
            (Self.Kind = okGroupProcedures) or
            (Self.Kind = okGroupFunctions) or
            (Self.Kind = okGroupTriggers) or
            (Self.Kind = okGroupEvents);
end;

procedure TObjectData.SaveToDB(const ASchemeName, AGranteeName: string;
      const ANetTypeGroup: TNetTypeGroup;
      const AServerVersion: Integer);
begin
  case ANetTypeGroup of
    ngMySQL:
    begin

    end;

    ngMariaDB:
    begin

    end;
  end;
end;

function TObjectData.ToggleInherites: Boolean;
begin
  Self.Inherites:= not Self.Inherites;
  Self.Changed:= TRUE;
  Result:= Self.Inherites;
end;

function TObjectData.TogglePrivilege(APrivilege: TPrivilege): Boolean;
begin
  if not Self.Inherites then begin
    Changed:= TRUE;
  end;
  if Self.Granted(APrivilege) then begin
    Self.Privileges:= Self.Privileges - [APrivilege];
    Result:= FALSE;
  end else begin
    Self.Privileges:= Self.Privileges + [APrivilege];
    Result:= TRUE;
  end;
end;


{ TPrivilegesInfo }

constructor TPrivilegesTreeColumns.Create(const ANetTypeGroup: TNetTypeGroup;
  const AServerVersion: Integer);
var
  I: Integer;
  P: TPrivilege;
begin
  FComparer:= TPrivilegeColumnComparer.Create;
  inherited Create;
  for P:= Low(TPrivilege) to High(TPrivilege) do begin
    if P.AvailableForServer(ANetTypeGroup, AServerVersion) then begin
      Self.Add(P);
    end;
  end;
  Sort;
end;

destructor TPrivilegesTreeColumns.Destroy;
begin
  FreeAndNil(FComparer);
  inherited;
end;


procedure TPrivilegesTreeColumns.Sort;
begin
  inherited Sort(FComparer);
end;

{ TPrivilegeInfoComparer }

function TPrivilegeColumnComparer.Compare(const Left,
  Right: TPrivilege): Integer;
begin
  Result:= 0;
  if Left.ColorFg = Right.ColorFg then begin
    Result:= CompareStr(Left.ToString, Right.ToString);
  end else begin
    if (Left.ColorFg = TPrivilege.CL_GREEN_FG) and (Right.ColorFg <> TPrivilege.CL_GREEN_FG) then begin
      Result:= -1;
    end else begin
      if (Left.ColorFg = TPrivilege.CL_RED_FG) and (Right.ColorFg = TPrivilege.CL_BLUE_FG) then begin
        Result:= -1;
      end else begin
        Result:= 1;
      end;
    end;
  end;
end;

{ TUserData }

constructor TUserData.Create;
begin
  ChildData:= TUserDataList.Create;
end;

destructor TUserData.Destroy;
begin
  FreeAndNil(ChildData);
  inherited;
end;

function TUserData.AddChild(AChild: PUserData): Integer;
begin
  Result:= -1;
  if not ChildData.Contains(AChild) then begin
    Result:= ChildData.Add(AChild);
  end;
end;

procedure TUserData.SetCaption(const ACaption: string;
  const ASetChanged: Boolean = TRUE);
var
  D, PD: PUserData;
  iPos: Integer;
begin
  if Caption <> ACaption then begin
    Caption:= ACaption;
    if ASetChanged then begin
      SetChanged(ucpCaption);
    end;
    case Kind of
      ukUser:
      begin
        InternalName:= ACaption;
        for D in ChildData do begin
          D^.InternalName:= '''' + ACaption + '''@''' + D^.Caption + '''';
          if ASetChanged then begin
            D^.SetChanged(ucpCaption);
          end;
        end;
      end;

      ukHost:
      begin
        PD:= ParentData;
        if PD <> NIL then begin
          InternalName:= '''' + PD^.Caption + '''@''' + ACaption + '''';
        end;
      end

      else begin
        InternalName:= ACaption;
      end;

      if ASetChanged then begin
        SetChanged(ucpCaption);
      end;
    end;
  end;
end;

procedure TUserData.SetChanged(const APart: TUserChangedPart);
var
  N: PVirtualNode;
  D: PUserData;
begin
  if not (APart in Self.ChangedParts) then begin
    Self.Changed:= TRUE;
    Self.ChangedParts:= Self.ChangedParts + [APart];
    case Self.Kind of
      ukGroupUsers: ;
      ukGroupRoles: ;
      ukUser:
      begin
//        N:= tvUsers
      end;
      ukRole: ;
      ukHost: ;
    end;
  end;
end;


{ TUser }

function TUser.HostRequiresNameResolve: Boolean;
var
  rx: TRegExpr;
begin
  rx := TRegExpr.Create;
  rx.Expression := '^(localhost|[\d\.\/\:]+|.*%.*|[\w\d]{4}\:.*)$';
  Result := not rx.Exec(Host);
  rx.Free;
end;

{ TUsersList }

constructor TUsersList.Create(AOwnsObjects: Boolean);
begin
  inherited Create(AOwnsObjects);
  FHostNames:= TStringList.Create;
  FUserNames:= TStringList.Create;
end;

destructor TUsersList.Destroy;
begin
  FreeAndNil(FHostNames);
  FreeAndNil(FUserNames);
  inherited;
end;

function TUsersList.GetHostNames(const AUserName: string): TStringList;
var
  User: TUser;
  UserName: string;
begin
  if not Assigned(FHostNames) then
    FHostNames:= TStringList.Create
  else
    FHostNames.Clear;
  for User in Self do begin
    if CompareText(User.Username, AUserName) = 0 then begin
      FHostNames.Add(User.Host);
    end;
  end;
  Result:= FHostNames;
end;

function TUsersList.GetUserNames: TStringList;
begin
  if Assigned(FUserNames) then
    Result:= FUserNames
  else begin
    FUserNames:= TStringList.Create;
    Result:= FUserNames;
  end;
end;

function TUsersList.Add(const Value: TUser): Integer;
begin
  Result:= inherited Add(Value);
  if FUserNames.IndexOf(Value.Username) = -1 then begin
    FUserNames.Add(Value.UserName);
  end;
end;

procedure TUsersList.Clear;
begin
  inherited Clear;
  FHostNames.Clear;
  FUserNames.Clear;
end;

{ TObjectPrivilegeData }

constructor TObjectPrivilegeData.Create(AParent: TObjectPrivilegeDataList);
begin
  inherited Create;
  FParent:= AParent;
end;

{ TObjectPrivilegeDataList }

constructor TObjectPrivilegeDataList.Create(AOwnsObjects: Boolean = TRUE;
  AFilterable: Boolean = TRUE);
begin
  inherited Create(AOwnsObjects);
  FFilterable:= AFilterable;
  if FFilterable then begin
    FFilteredList:= TObjectPrivilegeDataList.Create(FALSE, FALSE);
  end else begin
    FFilteredList:= NIL;
  end;
end;

destructor TObjectPrivilegeDataList.Destroy;
begin
  FreeAndNil(FFilteredList);
  inherited;
end;

function TObjectPrivilegeDataList.Append: TObjectPrivilegeData;
var
  OPD: TObjectPrivilegeData;
  I: Integer;
begin
  OPD:= TObjectPrivilegeData.Create(Self);
  I:= Add(OPD);
  OPD.Index:= I;
  Result:= OPD;
end;


function TObjectPrivilegeDataList.Filter(const AGrantee: string;
  const AKind: TNodeObjectKind; const ASchemaName, AObjectName,
  AColumnName: string): TObjectPrivilegeDataList;
var
  OPD: TObjectPrivilegeData;
begin
  Result:= NIL;
  if not FFilterable then begin
    Exit;
  end;
  try
    FFilteredList.Clear;
    case AKind of
      okGlobal:
      begin
        for OPD in Self do begin
          if (OPD.Kind = AKind) and
             (CompareText(OPD.Grantee, AGrantee) = 0) then
          begin
            FFilteredList.Add(OPD);
          end;
        end;
      end;

      okSchema:
      begin
        for OPD in Self do begin
          if (OPD.Kind = AKind) and
             (CompareText(OPD.Grantee, AGrantee) = 0) and
             (CompareText(OPD.SchemaName, ASchemaName) = 0) then
          begin
            FFilteredList.Add(OPD);
          end;
        end;
      end;

      okTable, okView, okProcedure, okFunction:
      begin
        for OPD in Self do begin
          if (OPD.Kind = AKind) and
             (CompareText(OPD.Grantee, AGrantee) = 0) and
             (CompareText(OPD.SchemaName, ASchemaName) = 0) and
             (CompareText(OPD.ObjectName, AObjectName) = 0) then
          begin
            FFilteredList.Add(OPD);
          end;
        end;
      end;

      okColumn:
      begin
        for OPD in Self do begin
          if (OPD.Kind = AKind) and
             (CompareText(OPD.Grantee, AGrantee) = 0) and
             (CompareText(OPD.SchemaName, ASchemaName) = 0) and
             (CompareText(OPD.ObjectName, AObjectName) = 0) and
             (CompareText(OPD.ColumnName, AColumnName) = 0) then
          begin
            FFilteredList.Add(OPD);
          end;
        end;
      end;
    end;
  finally
    Result:= FFilteredList;
  end;
end;

function TObjectPrivilegeDataList.FilteredPrivileges: TPrivileges;
var
  OPD: TObjectPrivilegeData;
begin
  Result:= [];
  for OPD in FFilteredList do begin
    Result:= Result + [OPD.Privilege];
  end;
end;

function TObjectPrivilegeDataList.Privileges: TPrivileges;
var
  OPD: TObjectPrivilegeData;
begin
  Result:= [];
  for OPD in Self do begin
    Result:= Result + [OPD.Privilege];
  end;
end;

end.
