unit VirtualTrees.EditLink;

// Base class for inplace node editors implementing IVTEditLink interface
// and default node editor.

interface

uses
  WinApi.Messages,
  System.Types,
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  VirtualTrees,
  VirtualTrees.Types,
  VirtualTrees.BaseTree;

type
  //Edit support Classes.
  TStringEditLink = class;

  TVTEdit = class(TCustomEdit)
  private
    procedure CMAutoAdjust(var Message : TMessage); message CM_AUTOADJUST;
    procedure CMExit(var Message : TMessage); message CM_EXIT;
    procedure CMRelease(var Message : TMessage); message CM_RELEASE;
    procedure CNCommand(var Message : TWMCommand); message CN_COMMAND;
    procedure WMChar(var Message : TWMChar); message WM_CHAR;
    procedure WMDestroy(var Message : TWMDestroy); message WM_DESTROY;
    procedure WMGetDlgCode(var Message : TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKeyDown(var Message : TWMKeyDown); message WM_KEYDOWN;
  protected
    FRefLink : IVTEditLink;
    FLink : TStringEditLink;
    procedure AutoAdjustSize; virtual;
    function CalcMinHeight : Integer; virtual;
    procedure CreateParams(var Params : TCreateParams); override;
    function GetTextSize : TSize; virtual;
    procedure KeyPress(var Key : Char); override;
  public
    constructor Create(Link : TStringEditLink); reintroduce;
    procedure ClearLink;
    procedure ClearRefLink;
    procedure Release; virtual;

    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property PasswordChar;
  end;

  TBaseEditLink = class;

  TEditLinkEditEvent = procedure (Sender: TBaseEditLink; var Result: Boolean) of object;
  TEditLinkPrepareEditEvent = procedure (Sender: TBaseEditLink; var Edit: TControl; var Result: Boolean) of object;

  // Most abstract base class for implementing IVTEditLink.
  // Knows almost nothing about associated Edit control and doesn't perform any
  // actions on it. Contains some properties that are not used directly but could
  // be useful in descendant classes. Follows general extension approach - all
  // IVTEditLink methods are virtual and most of them call DoXXX virtual methods
  // which in turn call event handlers so these extension options possible:
  //   - overriding main API methods to run additional actions before, after or
  //     instead of basic class code.
  //     (+) Lesser modification of existing classes
  //     (-) Event handlers are already launched after calling parent method
  //     (-) It's critical to check Result of parent method and exit immediately
  //       on False - this value means no action is done.
  //     (-) Returning Result is necessary
  //   - overriding DoXXX methods to run additional actions inside basic class code
  //     (+) No need in returning - lesser boilerplate code
  //     (-) Should call inherited to launch event handlers (OK if not using them)
  //   - assign event handlers in end-user code
  //     (+) Access to external classes with data to copy to EditLink editor.
  //     (-) Lesser encapsulation
  TBaseEditLink = class(TInterfacedObject, IVTEditLink)
  strict protected
    FEdit: TControl;                          // One of the property editor classes.
    FTree : TCustomVirtualStringTree;         //A back reference to the tree calling.
    FNode : PVirtualNode;                     //The node to be edited.
    FColumn : TColumnIndex;                   //The column of the node.
    FStopping : Boolean;                      //Set to True when the edit link requests stopping the edit action.
    FAlignment : TAlignment;
    FBiDiMode: TBiDiMode;

    // custom event handlers
    FOnPrepareEdit: TEditLinkPrepareEditEvent;
    FOnBeginEdit,
    FOnEndEdit,
    FOnCancelEdit: TEditLinkEditEvent;

    procedure SetEdit(const Value : TControl); //Setter for the FEdit member;
  public
    // IVTEditLink API
    function BeginEdit : Boolean; virtual; stdcall;
    function CancelEdit : Boolean; virtual; stdcall;
    function EndEdit : Boolean; virtual; stdcall;
    function GetBounds : TRect; virtual; stdcall; abstract;
    function PrepareEdit(Tree : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex) : Boolean; virtual; stdcall;
    procedure ProcessMessage(var Message : TMessage); virtual; stdcall; abstract;
    procedure SetBounds(R : TRect); virtual; stdcall; abstract;

    // Methods to plug custom actions into main ones. In base class only call event handlers.
    // Descendants may modify Result to cancel further flow.
    procedure DoBeginEdit(var Result: Boolean); virtual;
    procedure DoCancelEdit(var Result: Boolean); virtual;
    procedure DoEndEdit(var Result: Boolean); virtual;
    procedure DoPrepareEdit(var Result: Boolean); virtual;

    property Alignment : TAlignment read FAlignment;
    property BiDiMode: TBiDiMode read FBiDiMode;
    property Column : TColumnIndex read FColumn; //[IPK] Make Column(Index) accessible
    property Node : PVirtualNode read FNode;     //[IPK] Make FNode accessible
    property Tree : TCustomVirtualStringTree read FTree;
    property Stopping : Boolean read FStopping;

    property OnBeginEdit: TEditLinkEditEvent read FOnBeginEdit write FOnBeginEdit;
    property OnCancelEdit: TEditLinkEditEvent read FOnCancelEdit write FOnCancelEdit;
    property OnEndEdit: TEditLinkEditEvent read FOnEndEdit write FOnEndEdit;
    property OnPrepareEdit: TEditLinkPrepareEditEvent read FOnPrepareEdit write FOnPrepareEdit;
  end;

  // Edit link that has TWinControl-based Edit. Performs visibility and focus actions,
  // transfers window messages to Edit control.
  TWinControlEditLink = class(TBaseEditLink)
  protected
    function GetEdit: TWinControl;                //Getter for the FEdit member;
    procedure SetEdit(const Value : TWinControl); //Setter for the FEdit member;
  public
    destructor Destroy; override;

    function BeginEdit : Boolean; override; stdcall;
    function CancelEdit : Boolean; override; stdcall;
    function EndEdit : Boolean; override; stdcall;
    function GetBounds : TRect; override; stdcall;
    procedure ProcessMessage(var Message : TMessage); override; stdcall;

    property Edit : TWinControl read GetEdit write SetEdit;
  end;

  // Edit link that implements default node text editor.
  TStringEditLink = class(TWinControlEditLink)
  protected
    FTextBounds : TRect;                      //Smallest rectangle around the text.
    function GetEdit: TVTEdit;                //Getter for the FEdit member;
    procedure SetEdit(const Value : TVTEdit); //Setter for the FEdit member;

    procedure InitializeSelection; virtual;
  public
    constructor Create;

    function BeginEdit : Boolean; override; stdcall;
    function CancelEdit : Boolean; override; stdcall;
    function EndEdit : Boolean; override; stdcall;
    function PrepareEdit(Tree : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex) : Boolean; override; stdcall;
    procedure SetBounds(R : TRect); override; stdcall;

    property Edit : TVTEdit read GetEdit write SetEdit;
  end;

implementation

uses
  WinApi.Windows,
  System.SysUtils,
  System.Math,
  Vcl.Graphics,
  Vcl.Forms;

type
  TCustomVirtualStringTreeCracker = class(TCustomVirtualStringTree);

//----------------- TVTEdit --------------------------------------------------------------------------------------------

//Implementation of a generic node caption editor.

constructor TVTEdit.Create(Link : TStringEditLink);
begin
  inherited Create(nil);
  if not Assigned(Link) then
    raise EArgumentException.Create('Parameter Link must not be nil.');
  ShowHint := False;
  ParentShowHint := False;
  //This assignment increases the reference count for the interface.
  FRefLink := Link;
  //This reference is used to access the link.
  FLink := Link;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.ClearLink;
begin
  FLink := nil
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.ClearRefLink;
begin
  FRefLink := nil
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTEdit.CalcMinHeight : Integer;
var
  textHeight : Integer;
begin
  //Get the actual text height.
  textHeight := GetTextSize.cy;
  //The minimal height is the actual text height in pixels plus the the non client area.
  Result := textHeight + (Height - ClientHeight);
  //Also, proportionally to the text size, additional pixel(s) needs to be added for the caret.
  Result := Result + Trunc(textHeight * 0.05);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CMAutoAdjust(var Message : TMessage);
begin
  AutoAdjustSize;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CMExit(var Message : TMessage);
begin
  if Assigned(FLink) and not FLink.Stopping then
    with TCustomVirtualStringTreeCracker(FLink.Tree) do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) then
        DoEndEdit
      else
        DoCancelEdit;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CMRelease(var Message : TMessage);
begin
  Free;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CNCommand(var Message : TWMCommand);
begin
  if Assigned(FLink) and Assigned(FLink.Tree) and (Message.NotifyCode = EN_UPDATE) and not (vsMultiline in FLink.Node.States) then
    //Instead directly calling AutoAdjustSize it is necessary on Win9x/Me to decouple this notification message
    //and eventual resizing. Hence we use a message to accomplish that.
    AutoAdjustSize()
  else
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMChar(var Message : TWMChar);
begin
  if not (Message.CharCode in [VK_ESCAPE, VK_TAB]) then
    inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMDestroy(var Message : TWMDestroy);
begin
  //If editing stopped by other means than accept or cancel then we have to do default processing for
  //pending changes.
  if Assigned(FLink) and not FLink.Stopping and not (csRecreating in Self.ControlState) then
  begin
    with TCustomVirtualStringTreeCracker(FLink.Tree) do
    begin
      if (toAutoAcceptEditChange in TreeOptions.StringOptions) and Modified then
        Text[FLink.Node, FLink.Column] := FLink.Edit.Text;
    end;
    FLink := nil;
    FRefLink := nil;
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMGetDlgCode(var Message : TWMGetDlgCode);
begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTALLKEYS or DLGC_WANTTAB or DLGC_WANTARROWS;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.WMKeyDown(var Message : TWMKeyDown);
//Handles some control keys.

var
  Shift : TShiftState;
  EndEdit : Boolean;
  Tree : TBaseVirtualTree;
  NextNode : PVirtualNode;
  ColumnCandidate : Integer;
  EditOptions : TVTEditOptions;
  Column : TVirtualTreeColumn;
begin
  Tree := FLink.Tree;
  case Message.CharCode of
    VK_ESCAPE :
      begin
        TCustomVirtualStringTreeCracker(Tree).DoCancelEdit;
      end;
    VK_RETURN :
      begin
        EndEdit := not (vsMultiline in FLink.Node.States);
        if not EndEdit then
        begin
          //If a multiline node is being edited the finish editing only if Ctrl+Enter was pressed,
          //otherwise allow to insert line breaks into the text.
          Shift := KeyDataToShiftState(Message.KeyData);
          EndEdit := ssCtrl in Shift;
        end;
        if EndEdit then
        begin
          Tree := FLink.Tree;
          FLink.Tree.InvalidateNode(FLink.Node);
          NextNode := Tree.GetNextVisible(FLink.Node, True);
          TCustomVirtualStringTreeCracker(FLink.Tree).DoEndEdit;

          //get edit options for column as priority. If column has toDefaultEdit
          //use global edit options for tree
          EditOptions := TCustomVirtualStringTreeCracker(Tree).TreeOptions.EditOptions; //default
          ColumnCandidate := - 1;
          if Tree.Header.Columns.Count > 0 then //are there any columns?
          begin
            Column := Tree.Header.Columns[Tree.FocusedColumn];
            if Column.EditOptions <> toDefaultEdit then
              EditOptions := Column.EditOptions;

            //next column candidate for toVerticalEdit and toHorizontalEdit
            if Column.EditNextColumn <> - 1 then
              ColumnCandidate := Column.EditNextColumn;
          end;

          case EditOptions of
            toDefaultEdit :
              TCustomVirtualStringTreeCracker(Tree).TrySetFocus;
            toVerticalEdit :
              if NextNode <> nil then
              begin
                Tree.FocusedNode := NextNode;

                //for toVerticalEdit ColumnCandidate is also proper,
                //select ColumnCandidate column in row below
                if ColumnCandidate <> - 1 then
                begin
                  Tree.FocusedColumn := ColumnCandidate;
                  TCustomVirtualStringTreeCracker(Tree).EditColumn := ColumnCandidate;
                end;

                if Tree.CanEdit(Tree.FocusedNode, Tree.FocusedColumn) then
                  TCustomVirtualStringTreeCracker(Tree).DoEdit;
              end;
            toHorizontalEdit :
              begin
                if ColumnCandidate = - 1 then
                begin
                  //for toHorizontalEdit if property EditNextColumn is not used
                  //try to use just next column
                  ColumnCandidate := Tree.FocusedColumn + 1;
                  while (ColumnCandidate < Tree.Header.Columns.Count) and not Tree.CanEdit(Tree.FocusedNode, ColumnCandidate) do
                    Inc(ColumnCandidate);
                end
                else if not Tree.CanEdit(Tree.FocusedNode, ColumnCandidate) then
                  ColumnCandidate := Tree.Header.Columns.Count; //omit "focus/edit column" (see below)

                if ColumnCandidate < Tree.Header.Columns.Count then
                begin
                  Tree.FocusedColumn := ColumnCandidate;
                  TCustomVirtualStringTreeCracker(Tree).EditColumn := ColumnCandidate;
                  TCustomVirtualStringTreeCracker(Tree).DoEdit;
                end;
              end;
          end;
        end;
      end;
    VK_UP :
      begin
        if not (vsMultiline in FLink.Node.States) then
          Message.CharCode := VK_LEFT;
        inherited;
      end;
    VK_DOWN :
      begin
        if not (vsMultiline in FLink.Node.States) then
          Message.CharCode := VK_RIGHT;
        inherited;
      end;
    VK_TAB :
      begin
        if Tree.IsEditing then
        begin
          Tree.InvalidateNode(FLink.Node);
          if ssShift in KeyDataToShiftState(Message.KeyData) then
            NextNode := Tree.GetPreviousVisible(FLink.Node, True)//Shift+Tab goes to previous mode
          else
            NextNode := Tree.GetNextVisible(FLink.Node, True);
          Tree.EndEditNode;
          //check NextNode, otherwise we got AV
          if NextNode <> nil then
          begin
            //Continue editing next node
            Tree.ClearSelection();
            Tree.Selected[NextNode] := True;
            if Tree.CanEdit(Tree.FocusedNode, Tree.FocusedColumn) then
              TCustomVirtualStringTreeCracker(Tree).DoEdit;
          end;
        end;
      end;
    Ord('A') :
      begin
        if Tree.IsEditing and ([ssCtrl] = KeyboardStateToShiftState) then
        begin
          Self.SelectAll();
          Message.CharCode := 0;
        end;
      end;
    else
      inherited;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.AutoAdjustSize;
//Changes the size of the edit to accomodate as much as possible of its text within its container window.
//NewChar describes the next character which will be added to the edit's text.

var
  Size : TSize;
begin
  if not (vsMultiline in FLink.Node.States) and not (toGridExtensions in TCustomVirtualStringTreeCracker(FLink.Tree).TreeOptions.MiscOptions { see issue #252 } ) then
  begin
    //avoid flicker
    SendMessage(Handle, WM_SETREDRAW, 0, 0);
    try
      Size := GetTextSize;
      Inc(Size.cx, 2 * TCustomVirtualStringTreeCracker(FLink.Tree).TextMargin);
      //Repaint associated node if the edit becomes smaller.
      if Size.cx < Width then
        FLink.Tree.Invalidate();

      if FLink.Alignment = taRightJustify then
        FLink.SetBounds(Rect(Left + Width - Size.cx, Top, Left + Width, Top + Max(Size.cy, Height)))
      else
        FLink.SetBounds(Rect(Left, Top, Left + Size.cx, Top + Max(Size.cy, Height)));
    finally
      SendMessage(Handle, WM_SETREDRAW, 1, 0);
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.CreateParams(var Params : TCreateParams);
begin
  inherited;
  if not Assigned(FLink.Node) then
    exit; //Prevent AV exceptions occasionally seen in code below

  //Only with multiline style we can use the text formatting rectangle.
  //This does not harm formatting as single line control, if we don't use word wrapping.
  with Params do
  begin
    Style := Style or ES_MULTILINE;
    if vsMultiline in FLink.Node.States then
      Style := Style and not (ES_AUTOHSCROLL or WS_HSCROLL) or WS_VSCROLL or ES_AUTOVSCROLL;
    if tsUseThemes in FLink.Tree.TreeStates then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end
    else
    begin
      Style := Style or WS_BORDER;
      ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVTEdit.GetTextSize : TSize;
var
  DC : HDC;
  LastFont : THandle;
begin
  DC := GetDC(Handle);
  LastFont := SelectObject(DC, Font.Handle);
  try
    //Read needed space for the current text.
    GetTextExtentPoint32(DC, PChar(Text + 'yG'), Length(Text) + 2, Result);
  finally
    SelectObject(DC, LastFont);
    ReleaseDC(Handle, DC);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TVTEdit.KeyPress(var Key : Char);
begin
  if (Key = #13) and Assigned(FLink) and not (vsMultiline in FLink.Node.States) then
    Key := #0; //Filter out return keys as they will be added to the text, avoids #895
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTEdit.Release;
begin
  if HandleAllocated then
    PostMessage(Handle, CM_RELEASE, 0, 0);
end;

//----------------- TBaseEditLink --------------------------------------------------------------------------------------

procedure TBaseEditLink.SetEdit(const Value : TControl);
begin
  if Assigned(FEdit) then
    FEdit.Free;
  FEdit := Value;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseEditLink.BeginEdit : Boolean;
//Notifies the edit link that editing can start now. descendants may cancel node edit
//by returning False.

begin
  Result := not FStopping;
  if Result then
    DoBeginEdit(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseEditLink.CancelEdit : Boolean;

// Performs edit cancelling.

begin
  Result := not FStopping;
  if Result then
  begin
    // Let descendants cancel the cancel
    DoCancelEdit(Result);
    if not Result then
      Exit;
    FStopping := True;
    FTree.CancelEditNode;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseEditLink.EndEdit : Boolean;

// Performs edit ending.

begin
  Result := not FStopping;
  if Result then
  begin
    // Let descendants cancel the end
    DoEndEdit(Result);
    if not Result then
      Exit;
    FStopping := True;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TBaseEditLink.PrepareEdit(Tree : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex) : Boolean;

// Performs general init: assign Tree, Node, Column, other properties; destroys previous
// edit instance.

begin
  Result := Tree is TCustomVirtualStringTree;
  if not Result then Exit; // should not happen

  FTree := Tree as TCustomVirtualStringTree;
  FNode := Node;
  FColumn := Column;
  if Column <= NoColumn then
  begin
    FBidiMode := FTree.BidiMode;
    FAlignment := TCustomVirtualStringTreeCracker(FTree).Alignment;
  end
  else
  begin
    FBidiMode := FTree.Header.Columns[Column].BidiMode;
    FAlignment := FTree.Header.Columns[Column].Alignment;
  end;
  SetEdit(nil); // always dispose edit

  DoPrepareEdit(Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseEditLink.DoBeginEdit(var Result: Boolean);
begin
  if Assigned(OnBeginEdit) then
    OnBeginEdit(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseEditLink.DoCancelEdit(var Result: Boolean);
begin
  if Assigned(OnCancelEdit) then
    OnCancelEdit(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseEditLink.DoEndEdit(var Result: Boolean);
begin
  if Assigned(OnEndEdit) then
    OnEndEdit(Self, Result);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TBaseEditLink.DoPrepareEdit(var Result: Boolean);
begin
  if Assigned(OnPrepareEdit) then
    OnPrepareEdit(Self, FEdit, Result);
end;

//----------------- TWinControlEditLink ------------------------------------------------------------------------------------

destructor TWinControlEditLink.Destroy;
begin
  //FEdit.Free; casues issue #357. Fix:
  if Assigned(FEdit) and Edit.HandleAllocated then
    PostMessage(Edit.Handle, CM_RELEASE, 0, 0);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWinControlEditLink.GetEdit: TWinControl;
begin
  Result := TWinControl(FEdit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWinControlEditLink.SetEdit(const Value: TWinControl);
begin
  inherited SetEdit(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWinControlEditLink.BeginEdit: Boolean;
begin
  Result := inherited;
  if Result then
  begin
    Edit.Show;
    Edit.SetFocus;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWinControlEditLink.CancelEdit: Boolean;
begin
  Result := inherited;
  if Result then
  begin
    Edit.Hide;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TWinControlEditLink.GetBounds : TRect;
begin
  Result := FEdit.BoundsRect;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TWinControlEditLink.ProcessMessage(var Message : TMessage);
begin
  FEdit.WindowProc(Message);
end;

//----------------------------------------------------------------------------------------------------------------------

function TWinControlEditLink.EndEdit: Boolean;
begin
  Result := inherited;
  if Result then
  begin
    Edit.Hide;
  end;
end;

//----------------- TStringEditLink ------------------------------------------------------------------------------------

constructor TStringEditLink.Create;
begin
  inherited;
  FEdit := TVTEdit.Create(Self);
  with Edit do
  begin
    Visible := False;
    BorderStyle := bsSingle;
    AutoSize := False;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.GetEdit: TVTEdit;
begin
  Result := TVTEdit(FEdit);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringEditLink.InitializeSelection;
begin
  Edit.SelectAll;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringEditLink.SetEdit(const Value : TVTEdit);
begin
  inherited SetEdit(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.BeginEdit : Boolean;
begin
  Result := inherited;
  if Result then
  begin
    InitializeSelection;
    Edit.AutoAdjustSize;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.CancelEdit : Boolean;
begin
  Result := inherited;
  if Result then
  begin
    Edit.ClearLink;
    Edit.ClearRefLink;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.EndEdit : Boolean;
begin
  Result := inherited;
  if Result then
  try
    if Edit.Modified then
      FTree.Text[FNode, FColumn] := Edit.Text;
    Edit.ClearLink;
    Edit.ClearRefLink;
  except
    FStopping := False;
    raise;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TStringEditLink.PrepareEdit(Tree : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex) : Boolean;
var
  Text : string;
begin
  Result := inherited;
  if Result then
  begin
    Edit := TVTEdit.Create(Self);
    Edit.Visible := False;
    Edit.BorderStyle := bsSingle;
    Edit.AutoSize := True;
    Edit.Parent := Tree;
    //Initial size, font and text of the node.
    FTree.GetTextInfo(Node, Column, Edit.Font, FTextBounds, Text);
    Edit.Font.Color := clWindowText;
    Edit.RecreateWnd;
    Edit.AutoSize := False;
    Edit.Text := Text;
    Edit.BidiMode := FBidiMode;
    if Edit.BidiMode <> bdLeftToRight then
      ChangeBidiModeAlignment(FAlignment);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStringEditLink.SetBounds(R : TRect);
//Sets the outer bounds of the edit control and the actual edit area in the control.

var
  lOffset, tOffset, Height : TDimension;
  offsets : TVTOffsets;
begin
  if not FStopping then
  begin
    //Check if the provided rect height is smaller than the edit control height.
    Height := R.Bottom - R.Top;
    if Height < Edit.ClientHeight then
    begin
      //If the height is smaller than the minimal height we must correct it, otherwise the caret will be invisible.
      tOffset := Edit.CalcMinHeight - Height;
      if tOffset > 0 then
        Inc(R.Bottom, tOffset);
    end;

    //Set the edit's bounds but make sure there's a minimum width and the right border does not
    //extend beyond the parent's left/right border.
    if R.Left < 0 then
      R.Left := 0;
    if R.Right - R.Left < 30 then
    begin
      if FAlignment = taRightJustify then
        R.Left := R.Right - 30
      else
        R.Right := R.Left + 30;
    end;
    if R.Right > FTree.ClientWidth then
      R.Right := FTree.ClientWidth;
    Edit.BoundsRect := R;

    //The selected text shall exclude the text margins and be centered vertically.
    //We have to take out the two pixel border of the edit control as well as a one pixel "edit border" the
    //control leaves around the (selected) text.
    R := Edit.ClientRect;

    //If toGridExtensions are turned on, we can fine tune the left margin (or the right margin if RTL is on)
    //of the text to exactly match the text in the tree cell.
    if (toGridExtensions in TCustomVirtualStringTreeCracker(FTree).TreeOptions.MiscOptions) and
      ((FAlignment = taLeftJustify) and (Edit.BidiMode = bdLeftToRight) or (FAlignment = taRightJustify) and (Edit.BidiMode <> bdLeftToRight)) then
    begin
      //Calculate needed text area offset.
      FTree.GetOffsets(FNode, offsets, ofsText, FColumn);
      if FColumn = FTree.Header.MainColumn then
      begin
        if offsets[ofsToggleButton] < 0 then
          lOffset := - (offsets[ofsToggleButton] + 2)
        else
          lOffset := 0;
      end
      else
        lOffset := offsets[ofsText] - offsets[ofsMargin] + 1;
      //Apply the offset.
      if Edit.BidiMode = bdLeftToRight then
        Inc(R.Left, lOffset)
      else
        Dec(R.Right, lOffset);
    end;

    lOffset := IfThen(vsMultiline in FNode.States, 0, 2);
    if tsUseThemes in FTree.TreeStates then
      Inc(lOffset);
    InflateRect(R, - TCustomVirtualStringTreeCracker(FTree).TextMargin + lOffset, lOffset);
    if not (vsMultiline in FNode.States) then
    begin
      tOffset := FTextBounds.Top - Edit.Top;
      //Do not apply a negative offset, the cursor will disappear.
      if tOffset > 0 then
        OffsetRect(R, 0, tOffset);
    end;
    R.Top := Max( - 1, R.Top); //A value smaller than -1 will prevent the edit cursor from being shown by Windows, see issue #159
    R.Left := Max( - 1, R.Left);
    SendMessage(Edit.Handle, EM_SETRECTNP, 0, LPARAM(@R));
  end;
end;

end.
