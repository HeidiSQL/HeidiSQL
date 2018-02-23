unit VirtualCheckTree;

interface

uses
  VirtualTrees, Vcl.Graphics, Vcl.ImgList;

type

  TVirtualCheckTree = class(TVirtualStringTree)
    public
      class function GetCheckImageListFor(Kind: TCheckImageKind): TCustomImageList; override;
      function GetCheckImage(Node: PVirtualNode; ImgCheckType: TCheckType = ctNone; ImgCheckState: TCheckState = csUncheckedNormal; ImgEnabled: Boolean = True): Integer; override;
      procedure GetHitTestInfoAt(X, Y: Integer; Relative: Boolean; var HitInfo: THitInfo); override;
      procedure PaintCheckImage(Canvas: TCanvas; const ImageInfo: TVTImageInfo; Selected: Boolean); override;
  end;


implementation

uses
  userprivileges;

{ TVirtualCheckTree }

function TVirtualCheckTree.GetCheckImage(Node: PVirtualNode;
  ImgCheckType: TCheckType; ImgCheckState: TCheckState;
  ImgEnabled: Boolean): Integer;
begin
  Result:= inherited GetCheckImage(Node, ImgCheckType, ImgCheckState, ImgEnabled);
end;

class function TVirtualCheckTree.GetCheckImageListFor(
  Kind: TCheckImageKind): TCustomImageList;
begin
  Result:= inherited GetCheckImageListFor(Kind);
end;

procedure TVirtualCheckTree.GetHitTestInfoAt(X, Y: Integer; Relative: Boolean;
  var HitInfo: THitInfo);
begin
  inherited;
end;

procedure TVirtualCheckTree.PaintCheckImage(Canvas: TCanvas;
  const ImageInfo: TVTImageInfo; Selected: Boolean);
begin
  inherited;
end;



end.
