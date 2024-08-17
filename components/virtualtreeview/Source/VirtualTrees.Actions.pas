unit VirtualTrees.Actions;

interface

uses
  System.Classes,
  System.Actions,
  Vcl.Controls,
  Vcl.ActnList,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  TVirtualTreeAction = class(TCustomAction)
  strict private
    fTree: TBaseVirtualTree; // Member variable for the property "Control"
    fTreeAutoDetect: Boolean; // True if a potential Virtual TreeView should be detected automatically, false if a specific Tree was assigned to the property "Tree"
    fOnAfterExecute: TNotifyEvent; // Member variable for the OnAfterExecute event
    function GetSelectedOnly: Boolean; // Setter for the property "SelectedOnly"
    procedure SetSelectedOnly(const Value: Boolean); // Getter for the property "SelectedOnly"
  strict protected
    fFilter: TVirtualNodeStates; // Apply only of nodes which match these states
    procedure SetControl(Value: TBaseVirtualTree); // Setter for the property "Control"
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoAfterExecute; virtual;// Fires the event "OnAfterExecute"
    property SelectedOnly: Boolean read GetSelectedOnly write SetSelectedOnly default False;
  public
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    constructor Create(AOwner: TComponent); override;
    function Update: Boolean; override;
    property Control: TBaseVirtualTree read fTree write SetControl;
    property OnAfterExecute: TNotifyEvent read fOnAfterExecute write fOnAfterExecute; // Executed after the action was performed
    property Caption;
    property Enabled;
    property HelpContext;
    property HelpType;
    property Hint;
    property ImageIndex;
    property ShortCut;
    property SecondaryShortCuts;
    property Visible;
    property OnHint;
  end;

  TVirtualTreePerItemAction = class(TVirtualTreeAction)
  strict private
    fOnBeforeExecute: TNotifyEvent;
    fOldCursor: TCursor;
  strict protected
    fToExecute: TVTGetNodeProc; // method which is executed per item to perform this action
    procedure DoBeforeExecute();
    procedure DoAfterExecute(); override;// Fires the event "OnAfterExecute"
  public
    constructor Create(AOwner: TComponent); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    property OnBeforeExecute: TNotifyEvent read fOnBeforeExecute write fOnBeforeExecute;
  end;

  // A standard action which checkmarks nodes in a virtual treeview
  TVirtualTreeCheckAll = class(TVirtualTreePerItemAction)
  protected
    fDesiredCheckState: TCheckState;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SelectedOnly;
    property OnUpdate;
  end;

  // A standard action which unchecks nodes in a virtual treeview
  TVirtualTreeUncheckAll = class(TVirtualTreeCheckAll)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVirtualTreeSelectAll = class(TVirtualTreeAction)
  public
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  end;

  // Base class for actions that are applied to selected nodes only
  TVirtualTreeForSelectedAction = class(TVirtualTreeAction)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TVirtualTreeCopy = class(TVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TVirtualTreeCut = class(TVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TVirtualTreePaste = class(TVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

  TVirtualTreeDelete = class(TVirtualTreeForSelectedAction)
  public
    procedure ExecuteTarget(Target: TObject); override;
  end;

procedure Register;


implementation

uses
  WinApi.Windows,
  Vcl.Forms;

procedure Register;
begin
  RegisterActions('VirtualTree', [TVirtualTreeCheckAll, TVirtualTreeUncheckAll, TVirtualTreeSelectAll, TVirtualTreeCopy, TVirtualTreeCut, TVirtualTreePaste, TVirtualTreeDelete], nil);
end;

{ TVirtualTreeAction }

constructor TVirtualTreeAction.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTree := nil;
  fFilter := [];
  fOnAfterExecute := nil;
  fTreeAutoDetect := True;
end;

function TVirtualTreeAction.GetSelectedOnly: Boolean;
begin
  exit(TVirtualNodeState.vsSelected in fFilter);
end;

procedure TVirtualTreeAction.SetSelectedOnly(const Value: Boolean);
begin
  if Value then
    Include(fFilter, TVirtualNodeState.vsSelected)
  else
    Exclude(fFilter, TVirtualNodeState.vsSelected);
end;

procedure TVirtualTreeAction.DoAfterExecute;
begin
  if Assigned(fOnAfterExecute) then
    fOnAfterExecute(Self);
end;

function TVirtualTreeAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result := (Target is TBaseVirtualTree);
end;

function TVirtualTreeAction.Update(): Boolean;
begin
  Result := inherited;
  // If an OnUpdate event handler is assigned, TBasicAction.Update() will return True and so TBasicAction.UpdateTarget() will not be called.
  // We would then end up with Control == nil. So trigger update of action manually.
  if Result and Assigned(OnUpdate) then
    SendAppMessage(CM_ACTIONUPDATE, 0, LPARAM(Self))
end;

procedure TVirtualTreeAction.UpdateTarget(Target: TObject);
begin
  if fTreeAutoDetect and (Target is TBaseVirtualTree) then
    fTree := (Target as TBaseVirtualTree);
  Enabled := Assigned(Control) and not Control.IsEmpty and (not SelectedOnly or (Control.SelectedCount > 0))
end;

procedure TVirtualTreeAction.ExecuteTarget(Target: TObject);
begin
  DoAfterExecute();
end;

procedure TVirtualTreeAction.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTree) then
    FTree := nil;
end;

procedure TVirtualTreeAction.SetControl(Value: TBaseVirtualTree);
begin
  if Value <> fTree then begin
    fTree := Value;
    if Assigned(fTree) then begin
      fTree.FreeNotification(Self);// register Self as a component that should be notified when fTree is about to be destroyed.
    end;//if
    // Do not update the target of this action if it wa set explicitely by the developer
    fTreeAutoDetect := not Assigned(fTree);
  end;//if
end;


{ TVirtualTreePerItemAction }

constructor TVirtualTreePerItemAction.Create(AOwner: TComponent);
begin
  inherited;
  fToExecute := nil;
  fOnBeforeExecute := nil;
  fOldCursor := crNone;
end;

procedure TVirtualTreePerItemAction.DoAfterExecute;
begin
  inherited;
  if fOldCursor <> crNone then
    Screen.Cursor := fOldCursor;
end;

procedure TVirtualTreePerItemAction.DoBeforeExecute;
begin
  if Screen.Cursor <> crHourGlass then begin
    fOldCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;//if
  if Assigned(fOnBeforeExecute) then
    fOnBeforeExecute(Self);
end;

procedure TVirtualTreePerItemAction.ExecuteTarget(Target: TObject);
begin
  DoBeforeExecute();
  Control.BeginUpdate();
  try
    Control.IterateSubtree(nil, Self.fToExecute, nil, fFilter, true);
  finally
    Control.EndUpdate();
    DoAfterExecute();
  end;
end;

{ TVirtualTreeCheckAll }

constructor TVirtualTreeCheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Check all items in the list';
  Caption := 'Check &All';
  fDesiredCheckState := csCheckedNormal;
  fToExecute := procedure(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean)
                begin
                  if not Control.CheckState[Node].IsDisabled then
                    Control.CheckState[Node] := fDesiredCheckState;
                end;
end;


{ TVirtualTreeUncheckAll }

constructor TVirtualTreeUncheckAll.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hint := 'Uncheck all items in the list';
  Caption := '&Uncheck All';
  fDesiredCheckState := csUncheckedNormal;
end;


{ TVirtualStringSelectAll }

procedure TVirtualTreeSelectAll.UpdateTarget(Target: TObject);
begin
  Inherited;
  //Enabled := Enabled and (toMultiSelect in Control.TreeOptions.SelectionOptions)  // TreeOptions is protected  :-(
end;

procedure TVirtualTreeSelectAll.ExecuteTarget(Target: TObject);
begin
  Control.SelectAll(False);
  inherited;
end;


{ TVirtualTreeForSelectedAction }

constructor TVirtualTreeForSelectedAction.Create(AOwner: TComponent);
begin
  inherited;
  SelectedOnly := True;
end;


{ TVirtualTreeCopy }

procedure TVirtualTreeCopy.ExecuteTarget(Target: TObject);
begin
  Control.CopyToClipboard();
  Inherited;
end;


{ TVirtualTreeCut }

procedure TVirtualTreeCut.ExecuteTarget(Target: TObject);
begin
  Control.CutToClipboard();
  Inherited;
end;


{ TVirtualTreePaste }

procedure TVirtualTreePaste.ExecuteTarget(Target: TObject);
begin
  Control.PasteFromClipboard();
  Inherited;
end;


{ TVirtualTreeDelete }

procedure TVirtualTreeDelete.ExecuteTarget(Target: TObject);
begin
  Control.DeleteSelectedNodes();
  Inherited;
end;


end.
