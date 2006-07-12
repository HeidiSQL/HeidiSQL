{ TPassEdit

  Delphi's TEdit will not theme correctly when the password character is set.

  This occurs because TEdit only enables extended style ES_PASSWORD when
  PasswordChar is explicitly set to a value other than #0.  TEdit sends EM_SETPASSWORDCHAR
  to set the password character, then enables ES_PASSWORD.

  The problem occurs because Windows doesn't expect the password character to be
  explicitly set, simply setting the extended style is enough, Windows will handle the rest.

  This very simple descendent of TCustomEdit removes the problematic code that
  sets the password character to "*" and instead lets Windows set the character to
  the default ('*').  This means that internally FPasswordChar always remains as #0, and
  Windows handles the drawing of the correct password character.

  Rik Barker. 16/03/2004
}


unit PassEdit;

interface
uses Classes, Controls, StdCtrls;

type
   TPassEdit=class(TCustomEdit)
   private
      FDummy: byte;
   protected
      procedure CreateParams(var Params: TCreateParams); override;
   public
      constructor Create(AOwner: TComponent); override;
   published
      property Anchors;
      property AutoSelect;
      property AutoSize;
      property BiDiMode;
      property BorderStyle;
      property CharCase;
      property Color;
      property Constraints;
      property Ctl3D;
      property DragCursor;
      property DragKind;
      property DragMode;
      property Enabled;
      property Font: byte read FDummy;
      property HideSelection;
      property ImeMode;
      property ImeName;
      property MaxLength;
      property OEMConvert;
      property ParentBiDiMode;
      property ParentColor;
      property ParentCtl3D;
      property ParentFont: byte read FDummy;
      property ParentShowHint;
      property PopupMenu;
      property ReadOnly;
      property ShowHint;
      property TabOrder;
      property TabStop;
      property Text;
      property Visible;
      property OnChange;
      property OnClick;
      property OnContextPopup;
      property OnDblClick;
      property OnDragDrop;
      property OnDragOver;
      property OnEndDock;
      property OnEndDrag;
      property OnEnter;
      property OnExit;
      property OnKeyDown;
      property OnKeyPress;
      property OnKeyUp;
      property OnMouseDown;
      property OnMouseMove;
      property OnMouseUp;
      property OnStartDock;
      property OnStartDrag;
   end;

procedure Register;

implementation
uses Windows;

constructor TPassEdit.Create(AOwner: TComponent);
begin
   inherited;
   inherited ParentFont:=False;
   inherited Font.Name:='MS Shell Dlg';
end;

procedure TPassEdit.CreateParams(var Params: TCreateParams);
begin
   inherited;
   Params.Style := Params.Style or ES_PASSWORD;
end;

procedure Register;
begin
   RegisterComponents('XP',[TPassEdit]);
end;

end.
