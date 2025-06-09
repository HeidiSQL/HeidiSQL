unit VirtualTrees.DragImage;

interface

uses
  WinApi.Windows,
  WinApi.ActiveX,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics;

{$MINENUMSIZE 1, make enumerations as small as possible}


type
  // Drag image support for the tree.
  TVTTransparency = 0 .. 255;

  // Simple move limitation for the drag image.
  TVTDragMoveRestriction = (
    dmrNone,
    dmrHorizontalOnly,
    dmrVerticalOnly
  );

  TVTDragImageStates = set of (
    disHidden,                            // Internal drag image is currently hidden (always hidden if drag image helper interfaces are used).																															
    disInDrag,                            // Drag image class is currently being used.
    disPrepared                           // Drag image class is prepared.
    );

  // Class to manage header and tree drag image during a drag'n drop operation.
  TVTDragImage = class
  private
    FOwner         : TCustomControl;
    FBackImage,                              // backup of overwritten screen area
    FAlphaImage,                             // target for alpha blending
    FDragImage     : TBitmap;                // the actual drag image to blend to screen
    FRestriction   : TVTDragMoveRestriction; // determines in which directions the drag image can be moved
    FColorKey      : TColor;                 // color to make fully transparent regardless of any other setting
    FStates        : TVTDragImageStates;     // Determines the states of the drag image class.
  public
    constructor Create(AOwner : TCustomControl);
    destructor Destroy; override;

    procedure EndDrag;
    procedure PrepareDrag(DragImage : TBitmap; HotSpot : TPoint; const DataObject: IDataObject; pColorKey: TColor = clWindow);
    property MoveRestriction : TVTDragMoveRestriction read FRestriction write FRestriction default dmrNone;
  end;

implementation

uses
  WinApi.ShlObj,
  WinApi.Messages,
  System.SysUtils,
  System.Math,
  VirtualTrees.DragnDrop,
  VirtualTrees.Types,
  VirtualTrees.Utils,
  VirtualTrees.BaseTree;

//----------------- TVTDragImage ---------------------------------------------------------------------------------------

constructor TVTDragImage.Create(AOwner : TCustomControl);
begin
  FOwner := AOwner;
  FRestriction := dmrNone;
  FColorKey := clNone;
end;

//----------------------------------------------------------------------------------------------------------------------

destructor TVTDragImage.Destroy;
begin
  EndDrag;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTDragImage.EndDrag;
begin
  FStates := FStates - [disInDrag, disPrepared];
  FBackImage.Free;
  FBackImage := nil;
  FDragImage.Free;
  FDragImage := nil;
  FAlphaImage.Free;
  FAlphaImage := nil;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTDragImage.PrepareDrag(DragImage : TBitmap; HotSpot : TPoint; const DataObject : IDataObject; pColorKey: TColor = clWindow);
// Creates all necessary structures to do alpha blended dragging using the given image.
// ImagePostion and HotSpot are given in screen coordinates. The first determines where to place the drag image while
// the second is the initial mouse position.
// This method also determines whether the system supports drag images natively. If so then only minimal structures
// are created.

var
  Width, Height      : Integer;
  DragSourceHelper   : IDragSourceHelper;
  DragInfo           : TSHDragImage;
  lDragSourceHelper2 : IDragSourceHelper2; // Needed to get Windows Vista+ style drag hints.
  lNullPoint         : TPoint;
begin
  Width := DragImage.Width;
  Height := DragImage.Height;
  FColorKey := pColorKey;

  // Determine whether the system supports the drag helper interfaces.
  if Assigned(DataObject) and Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER, IDragSourceHelper, DragSourceHelper)) then
  begin
    lNullPoint := Point(0, 0);
    if Supports(DragSourceHelper, IDragSourceHelper2, lDragSourceHelper2) then
      lDragSourceHelper2.SetFlags(DSH_ALLOWDROPDESCRIPTIONTEXT); // Show description texts
    // First let the system try to initialze the DragSourceHelper, this works fine for file system objects (CF_HDROP)
    StandardOLEFormat.cfFormat := CF_HDROP;
    if not Succeeded(DataObject.QueryGetData(StandardOLEFormat)) or not Succeeded(DragSourceHelper.InitializeFromWindow(0, lNullPoint, DataObject)) then
    begin
      // Supply the drag source helper with our drag image.
      DragInfo.sizeDragImage.cx := Width;
      DragInfo.sizeDragImage.cy := Height;
      DragInfo.ptOffset := HotSpot;
      DragInfo.hbmpDragImage := CopyImage(DragImage.Handle, IMAGE_BITMAP, Width, Height, LR_COPYRETURNORG);
      DragInfo.crColorKey := ColorToRGB(FColorKey);
      if not Succeeded(DragSourceHelper.InitializeFromBitmap(@DragInfo, DataObject)) then
      begin
        DeleteObject(DragInfo.hbmpDragImage);
      end;
    end;
  end;
end;


end.
